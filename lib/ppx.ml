open Ppxlib
open Zimmerman
open Ast_builder

let expand meth ~ctxt route_str handler =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let params, queries = Route.of_string route_str in
  let params = List.fold_left (fun (acc: (expression * pattern) list) ->
    let open Route in
    let label = "_"^string_of_int (List.length acc) in
    let var = Default.ppat_var ~loc {loc; txt = label} in
    let ident_e = Default.evar ~loc label in
    function
    | Param conv -> (ident_e, (
      match conv with
      | StringC _ -> [%pat? String [%p var]]
      | IntC _ -> [%pat? Int [%p var]]
      | BoolC _ -> [%pat? Bool [%p var]]
    )) :: acc
    | _ -> acc
  ) [] params in

  let queries = List.map (fun (key, conv) ->
    let label = "_"^key in
    let key_pat = Default.pstring ~loc key in
    let var = Default.ppat_var ~loc {loc; txt = label} in
    let ident_e = key, Default.evar ~loc label in
    ident_e, [%pat? [%p key_pat], [%p var]], conv
  ) (Option.value ~default:[] queries) in

  let f = Default.evar ~loc ("Zimmerman."^meth) in
  let params_pat = List.map (fun (_, pat) -> pat) params |> Default.plist ~loc in
  let queries_pat = List.map (fun (_, pat, _) -> pat) queries |> Default.plist ~loc in
  let handler =
    Default.evar ~loc "_req" :: List.map (fun (ident, _) -> ident) params
    |> Default.eapply handler ~loc
  in
  let handler =
    let open Route in
    List.map (fun ((key, ident), _, (conv: Route.conversions)) ->
      let label, pat, exp = match conv with
      | StringC Mandatory -> (
        Labelled key, [%pat? Some (String _v)], [%expr _v]
      )
      | StringC Optional -> (
        Optional key, [%pat? _opt], [%expr (
          match _opt with
          | Some (String _v) -> Some _v
          | _ -> None
        )]
      )
      | IntC Mandatory -> (
        Labelled key, [%pat? Some (Int _v)], [%expr _v]
      )
      | IntC Optional -> (
        Optional key, [%pat? _opt], [%expr (
          match _opt with
          | Some (Int _v) -> Some _v
          | _ -> None
        )]
      )
      | BoolC Mandatory -> (
        Labelled key, [%pat? Some (Bool _v)], [%expr _v]
      )
      | BoolC (Optional|Toggle) -> (
        Optional key, [%pat? _opt], [%expr (
          match _opt with
          | Some (Bool _v) -> Some _v
          | _ -> None
        )]
      )
      | _ -> failwith "type mismatch"
      in
      label, [%expr (
        match [%e ident] with
        | [%p pat] -> [%e exp]
      )]
    ) queries
    |> Default.pexp_apply ~loc handler
  in

  let route_str = Default.estring ~loc route_str in
  [%expr (
    [%e f] (Zimmerman.Route.of_string [%e route_str]) (fun _req (matches: Zimmerman.Route.matches list) queries ->
      match matches with
      | [%p params_pat] -> (
        match queries with
        | [%p queries_pat] -> [%e handler]
        | _ -> failwith "type mismatch"
      )
      | _ -> failwith "type mismatch in ppx!"
    )
  )]

let () = List.iter (fun meth ->
  let ext =
    let open Extension in
    V3.declare meth Context.expression
      Ast_pattern.(single_expr_payload (pexp_tuple ((estring __)^::(__)^::nil)))
      (expand meth)
  in
  let rule = Ppxlib.Context_free.Rule.extension ext in
  Driver.register_transformation ~rules:[ rule ] meth
) ["get"; "post"]