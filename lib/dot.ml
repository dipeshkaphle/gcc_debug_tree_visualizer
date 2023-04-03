open! Base
open Dot_intf

let attr_to_string ( a : attr ) : string =
  Printf.sprintf "%slabel=\"{%s\n}\""  (match a.shape with | Record -> "shape=record;" | _ -> "") a.lbl


let node_to_string ( n: node_stmt ) : string = Printf.sprintf "%s [%s];"
    n.name (Option.value ~default:""
              Option.( n.attr  >>= fun x-> Some (attr_to_string x) ) )


let make_direction is_directed =
  if is_directed then " -> " else " -- "

let rec get_top_node_name ?cluster (n: stmt)  =
  let cluster' = Option.(cluster >>= fun x -> Some (Printf.sprintf "lhead=%s" x)) in
  match n with
  | Node_stmt n' ->  ( n'.name, Option.value ~default:"" cluster')
  | Edge_stmt e -> let (n', _) = e.edge in ( n'.name, Option.value ~default:"" cluster')
  | Subgraph s ->
    let leader =  List.hd_exn s.stmt_list in
    get_top_node_name ~cluster:s.cluster leader


let rec edge_to_string ( e: edge_stmt ) =
  let direction = make_direction e.is_directed in
  let (l, r ) = e.edge in
  let ( rhs, lbl  )= get_top_node_name r in
  Printf.sprintf "%s\n%s %s %s [%s][label=\"%s\"]" (stmt_to_string r) l.name direction rhs lbl e.lbl

and
  stmt_to_string (s: stmt) : string = match s with
  | Node_stmt n -> node_to_string n
  | Edge_stmt e -> edge_to_string e
  | Subgraph g ->  begin
      graph_to_string ~gtype:"subgraph" g
    end
and
  graph_to_string ?gtype ( g: graph ) =
  let top = Printf.sprintf "%s %s{\n" (Option.value ~default:"digraph" gtype) g.cluster  in
  let bottom = "\n}" in
  let all_stmts_as_string = List.map ~f:stmt_to_string g.stmt_list in
  let all_stmts_combined = String.concat ~sep:"\n\n" all_stmts_as_string in
  (* let () = Stdio.print_endline (Printf.sprintf "CLUSTER:%s\n%s\n----------------" g.cluster all_stmts_combined) in *)
  Printf.sprintf "%s\n%s\n%s" top all_stmts_combined bottom
