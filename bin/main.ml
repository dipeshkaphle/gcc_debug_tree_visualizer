open Gcc_debug_tree_visualizer
open Sexplib

module G = Gcc_debug_tree_visualizer

let () =  "(statement_list type (void ) stmt (modify_expr) stmt (something_expr ) )"
          |> G.Parser.to_sexp
          |> G.Parser.parse
          |> Node.sexp_of_node_t |> Sexp.to_string_hum ~indent:4 |> print_endline
