open Gcc_debug_tree_visualizer
open Sexplib

module G = Gcc_debug_tree_visualizer

let s = "<statement_list 0x7f18ac7c6600
    type <void_type 0x7f18ac62ff18 void VOID
        align:8 warn_if_not_align:0 symtab:0 alias-set -1 canonical-type 0x7f18ac62ff18
        pointer_to_this <pointer_type 0x7f18ac637000>>
    side-effects head 0x7f18ac7f81b0 tail 0x7f18ac7f81c8 stmts 0x7f18ac7f73e8 0x7f18ac7f7410

    stmt <modify_expr 0x7f18ac7f73e8
        type <real_type 0x7f18ac637348 double DF
            size <integer_cst 0x7f18ac613d50 constant 64>
            unit-size <integer_cst 0x7f18ac613d68 constant 8>
            align:64 warn_if_not_align:0 symtab:0 alias-set -1 canonical-type 0x7f18ac637348 precision:64
            pointer_to_this <pointer_type 0x7f18ac637930>>
        side-effects
        arg:0 <array_ref 0x7f18ac7d3ee0 type <real_type 0x7f18ac637348 double>
            arg:0 <var_decl 0x7f18ac7db3f0 x> arg:1 <var_decl 0x7f18ac7db510 i>
            testing/test.c:38:6 start: testing/test.c:38:5 finish: testing/test.c:38:8>
        arg:1 <float_expr 0x7f18ac7c6a80 type <real_type 0x7f18ac637348 double>
            side-effects
            arg:0 <call_expr 0x7f18ac7f35d0 type <integer_type 0x7f18ac62f5e8 int>
                side-effects
                fn <addr_expr 0x7f18ac7c6a60 type <pointer_type 0x7f18ac7f6d20>
                    constant arg:0 <function_decl 0x7f18ac7b4a00 rand>
                    testing/test.c:38:12 start: testing/test.c:38:12 finish: testing/test.c:38:15>
                testing/test.c:38:12 start: testing/test.c:38:12 finish: testing/test.c:38:17>>
        testing/test.c:38:10 start: testing/test.c:38:5 finish: testing/test.c:38:17>
    stmt <modify_expr 0x7f18ac7f7410 type <real_type 0x7f18ac637348 double>
        side-effects
        arg:0 <array_ref 0x7f18ac7d3f18 type <real_type 0x7f18ac637348 double>
            arg:0 <var_decl 0x7f18ac7db480 y> arg:1 <var_decl 0x7f18ac7db510 i>
            testing/test.c:39:6 start: testing/test.c:39:5 finish: testing/test.c:39:8>
        arg:1 <float_expr 0x7f18ac7c6ac0 type <real_type 0x7f18ac637348 double>
            side-effects
            arg:0 <call_expr 0x7f18ac7f3600 type <integer_type 0x7f18ac62f5e8 int>
                side-effects
                fn <addr_expr 0x7f18ac7c6aa0 type <pointer_type 0x7f18ac7f6d20>
                    constant arg:0 <function_decl 0x7f18ac7b4a00 rand>
                    testing/test.c:39:12 start: testing/test.c:39:12 finish: testing/test.c:39:15>
                testing/test.c:39:12 start: testing/test.c:39:12 finish: testing/test.c:39:17>>
        testing/test.c:39:10 start: testing/test.c:39:5 finish: testing/test.c:39:17>>"

let () =  s |> G.Parser.replace_parens
          |> (fun x-> print_endline x ; x)
          |> G.Parser.to_sexp
          |> G.Parser.parse
          |> Node.sexp_of_node_t |> Sexp.to_string_hum ~indent:4 |> print_endline
