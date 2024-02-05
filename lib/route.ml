module String = Containers.String
open Printf

type optional =
| Optional (* option *)
| Mandatory (* Some <val> or query None *)
| Toggle (* for bool -> Some true if no value or set to true *)
type conversions = StringC of optional | IntC of optional | BoolC of optional

type segment =
| Literal of string
| Param of conversions
| Wildcard

type t = segment list * (string * conversions) list option

let of_string url: t =
  let segments_of_path path =
    let path = if path.[0] = '/' then String.drop 1 path else path in
    let segments = String.split_on_char '/' path in
    List.map (function
      | ":string" -> Param (StringC Mandatory)
      | ":int" -> Param (IntC Mandatory)
      | "*" -> Wildcard
      | literal -> Literal literal
    ) segments
  in

  let queries_of_string query =
    String.split_on_char '&' query |> List.map (fun el ->
      match String.Split.left ~by:"=" el with
      | Some (key, "string") -> key, StringC Optional
      | Some (key, "string!") -> key, StringC Mandatory
      | Some (key, "int") -> key, IntC Optional
      | Some (key, "int!") -> key, IntC Mandatory
      | Some (key, "bool") -> key, BoolC Optional
      | Some (key, "bool!") -> key, BoolC Mandatory
      | None -> el, BoolC Toggle
      | _ -> raise (Invalid_argument (sprintf "Invalid type specified for query at '%s'" el))
    )
  in

  match String.Split.left ~by:"?" url with
  | Some (path, query) -> segments_of_path path, Some (queries_of_string query)
  | None -> segments_of_path url, None

type matches = String of string | Int of int | Bool of bool | Unit

exception Bad_request

(* None indicates no match. Some (params, None) indicates bad request. *)
let match_string (path_segs, queries) url: (matches list * (string * matches option) list option) option =
  let int_of_string s = match int_of_string_opt s with
  | Some i -> i
  | None -> raise (Not_found)
  in
  let bool_of_string s = match bool_of_string_opt s with
  | Some i -> i
  | None -> raise (Not_found)
  in

  let match_path path =
    let path = if path.[0] = '/' then String.drop 1 path else path in
    let segments = String.split_on_char '/' path in
    List.map2 (fun segment ->
      function
      | Literal string -> if segment = string then Unit else raise Not_found
      | Param (StringC Mandatory) -> String segment
      | Param (IntC Mandatory) -> Int (int_of_string segment)
      | Param (BoolC Mandatory) -> Bool (bool_of_string segment)
      | Wildcard -> Unit
      | _ -> failwith "matching error"
    ) segments path_segs |> List.filter (fun m -> not (m = Unit))
  in

  let match_queries querystring =
    let queries = Option.get queries in
    let elements = String.split_on_char '&' querystring |> List.map (fun el ->
      match String.Split.left ~by:"=" el with
      | Some (key, value) -> key, Some value
      | None -> el, None
    ) in
    List.map (fun (key, typ) ->
      key, match typ with
      | StringC opt -> (
        match List.assoc_opt key elements with
        | Some (Some v) -> Some (String v)
        | Some (None) | None -> if opt = Mandatory then raise Bad_request else None
      )
      | IntC opt -> (
        match List.assoc_opt key elements with
        | Some (Some v) -> (
          match int_of_string_opt v with
          | Some i -> Some (Int i)
          | None -> raise Bad_request
        )
        | Some (None) | None -> if opt = Mandatory then raise Bad_request else None
      )
      | BoolC opt -> (
        let present = List.assoc_opt key elements in
        match present with
        | Some (Some v) -> (
          match bool_of_string_opt v with
          | Some i -> Some (Bool i)
          | None -> raise Bad_request
        )
        | Some (None) | None -> (
          match opt with
          | Toggle -> Some (Bool (not (present = None)))
          | Optional -> None
          | Mandatory -> raise Bad_request
        )
      )
    ) queries
  in

  try
    match String.Split.left ~by:"?" url with
    | Some (path, query) -> Some (match_path path,
      try Some (match_queries query) with Bad_request -> None)
    | None ->
      match queries with
      | None -> Some (match_path url, Some []) 
      | Some queries -> (
        let optional = List.filter_map (fun (key, conv) ->
          match conv with
          | StringC (Optional|Toggle) | IntC (Optional|Toggle) | BoolC (Optional|Toggle) -> Some (key, None)
          | _ -> None
        ) queries in
        if List.is_empty optional then
          Some (match_path url, None)
        else
          Some (match_path url, Some optional)
      )
  with
  | Not_found -> None

let print_match =
  function
  | String s -> sprintf "String '%s'" s
  | Int i -> sprintf "Int %d" i
  | Bool b -> sprintf "Bool %b" b
  | Unit -> sprintf "Unit (Internal)"