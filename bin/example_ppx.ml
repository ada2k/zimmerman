open Eio
open Printf
open Zimmerman

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug)

let () = Eio_main.run @@ fun env -> Switch.run @@ fun sw ->
  Zimmerman.default ()
  |> [%get "/hello/:string?greeting=string", fun _req ?(greeting = "hello") name -> response_of_string `OK (sprintf "%s %s" greeting name)]
  |> Zimmerman.serve ~callback:(fun () -> Logs.info (fun m -> m "Listening at :%d" 8081)) (Net.listen env#net ~backlog:1 ~sw:sw (`Tcp (Net.Ipaddr.V4.loopback, 8081)))