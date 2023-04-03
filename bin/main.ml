open Gcc_debug_tree_visualizer
open Base
open Gcc_debug_tree_visualizer.Node


module G = Gcc_debug_tree_visualizer

let get_new_name s i =
  let x = Printf.sprintf "%s__%d" s !i in
  i := !i + 1;
  x


let make_label (node: Node.node_t) =
  match node with
    Node (Stmt _) -> ""
  | Node (Expr e) ->
    Printf.sprintf
      "expr_type:%s\\l\\\n|result_type:%s\\l\\\n|symbol_name:%s\\l\\"
      e.expression_type
      (Option.value ~default:"NA" e.result_type)
      (Option.value  ~default:"NA" e.var_name)
  | Node (Stmtlist sl) ->
    Printf.sprintf
      "result_type:%s"
      (Option.value ~default:"NA" sl.result_type)

let to_dot ( node: Node.node_t ) : Dot.graph =
  let i = ref 0 in
  let get_name = fun x -> get_new_name x i in
  let rec f : Node.node_t -> Dot.graph = (fun node ->
      match node with
      | Node.Node (Node.Expr e) -> begin
          let top_level_node = Dot.({
              name = get_name "expr";
              attr = Some ({shape=Record; lbl=(make_label node)})
            }) in
          let all_subexprs = List.map ~f:(fun (lbl, x)-> (lbl,f (Node.Node (Node.Expr x)))) (Option.value ~default:[] e.args ) in
          let all_edges = List.map
              ~f:(fun (lbl, x ) -> Dot.Edge_stmt Dot.{is_directed = true; edge= (top_level_node, Subgraph x); lbl=lbl}) all_subexprs in
          Dot.( { stmt_list =  ( Node_stmt top_level_node )::all_edges; cluster= (get_name "cluster") })
        end
      | Node.Node (Node.Stmt s) -> begin
          f (Node.Node (Node.Expr s.exp))
        end
      | Node.Node (Node.Stmtlist sl) -> begin
          let top_level_node = Dot.({
              name = get_name "stmt";
              attr = Some ({shape=Record; lbl=(make_label node)})
            }
            )
          in
          let all_stmts_graph = List.map
              ~f:(fun x ->  f (Node.Node (Node.Stmt x)))
              sl.all_stmts in
          let all_edges = List.mapi
              ~f:(fun i x-> Dot.Edge_stmt Dot.{is_directed = true;
                                               edge = (top_level_node , Subgraph x );
                                               lbl= (Int.to_string i) }  ) all_stmts_graph in
          Dot.({stmt_list=(Node_stmt top_level_node)::all_edges; cluster = get_name "cluster" } )
        end
    )
  in
  (f node)

let argv = Sys.get_argv ()
let s =
  let filename = argv.(1) in
  In_channel.with_open_text filename (fun c -> In_channel.input_all c)
(* In_channel.input_all in_channel *)

let dot_out = argv.(2)

let t =  s |> G.Parser.replace_parens
         (* |> (fun x-> Stdio.print_endline x ; x) *)
         |> G.Parser.to_sexp
         |> G.Parser.parse
         |> to_dot
         |> G.Dot.graph_to_string
         |> (fun x -> Out_channel.with_open_text dot_out (fun c -> Out_channel.output_string c x ))
