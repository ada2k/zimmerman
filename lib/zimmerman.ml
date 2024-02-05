open Cohttp_eio
open Eio
open Printf

module Route = Route

type response_and_body = Http.Response.t * (Buf_read.t -> Buf_write.t -> unit)
type params = Route.matches list
type queries = (string * Route.matches option) list
type handler = Http.Request.t -> params -> queries -> response_and_body
type middleware = Http.Request.t -> params -> queries -> handler -> response_and_body option
type methods = {
  get: handler option;
  post: handler option;
}
type app = {
  middleware: middleware list;
  routes: (Route.t, methods) Hashtbl.t;
  not_found: handler;
  method_not_allowed: handler;
  bad_request: handler;
}

let empty_methods = {
  get = None;
  post = None;
}

let logger (req: Http.Request.t) (params: params) (queries: queries) (handler: handler) =
  let open Cohttp.Code in
  let start = Unix.gettimeofday () in
  let (res, body) = handler req params queries in
  let code = code_of_status res.status in
  Logs.info (fun m -> m "%s  GET %s - HTTP %d in %fs"
    (if is_success code then "🟢" else (if is_client_error code then "🟡" else "🔴"))
    req.resource
    code
    (Unix.gettimeofday () -. start));
  Logs.debug (fun m ->
    m "Params: %s" (List.map Route.print_match params |> String.concat "; ");
    m "Queries: %s" (List.map (
      function
      | key, Some m -> sprintf "%s = %s" key (Route.print_match m)
      | key, None -> sprintf "%s = None" key
    ) queries |> String.concat "; ")
  );
  Some (res, body)  

let response_of_string status string =
  Http.Response.make ~status ~headers:(Http.Header.init_with "Content-Length" (String.length string |> string_of_int)) (),
  fun _ write -> Buf_write.string write string; Buf_write.close write

let response_of_stream status stream =
  Http.Response.make ~status ~headers:(Http.Header.init_with "Transfer-Encoding" "chunked") (),
  fun _ write ->
    let rec next_chunk () =
      match Stream.take stream with
      | Some chunk -> Buf_write.string write (sprintf "%X\r\n%s\r\n" (String.length chunk) chunk); next_chunk ()
      | None -> Buf_write.string write "0\r\n\r\n"; Buf_write.close write
    in next_chunk ()

let app (): app = {
  middleware = [];
  routes = Hashtbl.create 2;
  not_found = (fun _ _ _ -> response_of_string `Not_found "Not Found");
  method_not_allowed = (fun _ _ _ -> response_of_string `Method_not_allowed "Method Not Allowed");
  bad_request = (fun _ _ _ -> response_of_string `Bad_request "Bad Request");
}
let default () = { (app ()) with middleware = [logger] }

let use middleware app = { app with middleware = app.middleware @ [middleware] }
let not_found handler app = { app with not_found = handler }
let method_not_allowed handler app = { app with method_not_allowed = handler }

let resolve_route routes resource = Option.value ~default:empty_methods (
  Hashtbl.find_opt
  routes
  resource)

let get route handler app =
  Hashtbl.replace app.routes route { (resolve_route app.routes route) with get = Some handler };
  app

let post route handler app =
  Hashtbl.replace app.routes route { (resolve_route app.routes route) with post = Some handler };
  app

let any route handler app =
  Hashtbl.replace app.routes route { post = Some handler; get = Some handler };
  app

let serve ?callback socket app =
  let routes = Hashtbl.to_seq app.routes in

  let server = Server.make_expert ~callback:(fun _ req _ ->
    let handler, params, queries =
      Option.value ~default:(app.not_found, [], []) @@ Seq.find_map (fun (route, methods) ->
        match Route.match_string route req.resource with
        | Some (params, Some queries) -> Some (Option.value ~default:app.method_not_allowed (match req.meth with `GET -> methods.get | `POST -> methods.get | _ -> None) , params, queries)
        | Some (params, None) -> Some (app.bad_request, params, [])
        | None -> None
      ) routes
    in
    
    let rec process middlewares =
      if List.is_empty middlewares then
        handler req params queries
      else
        match ((List.hd middlewares) req params queries handler) with
        | Some res -> res
        | None -> process (List.tl middlewares)
    in process app.middleware
  ) () in

  match callback with
  | Some f -> f ()
  | None -> ();
  ;
  Server.run ~on_error:raise socket server