open Base
open Sexplib.Std
open Stdio

exception Parsing_error of string

let replace_parens s =
  let le_replaced_str = String.substr_replace_all s ~pattern:"<" ~with_:"(" in
  String.substr_replace_all le_replaced_str ~pattern:">" ~with_:")"

let to_sexp s =
  Parsexp.Single.parse_string_exn s

let rec split_after sexp l  =
  Option.(List.hd l >>=
          fun x -> if Sexp.equal x sexp then List.tl l else split_after sexp (List.tl_exn l) )

let split_after_and_get_all ?change_fn sexp l  =
  let res = ref [] in
  let rec f sexp' l = match split_after sexp' l with
    | Some tl ->
      res := tl::( !res );
      let sexp''= Option.fold change_fn ~init:sexp' ~f:(fun acc g -> g acc) in
      f sexp'' tl
    | None -> List.rev !res
  in f sexp l

let rec get_all_that_satisfy_f ~f l =
  match l with
  | hd::tail ->
    let tail' = get_all_that_satisfy_f ~f tail in
    if (f l) then (hd, tail) :: tail' else tail'
  | [] -> []

let get_if_atom s = match s with
  | Sexp.Atom x -> x
  | Sexp.List l -> raise (Parsing_error "[get_if_atom] Expected Atom but got List")

let get_if_list s = match s with
  | Sexp.Atom x -> raise (Parsing_error "[get_if_list] expected List but got Atom")
  | Sexp.List l -> l

(** TYPE is in the following format
    type (void_type 0x7f18ac62ff18 void VOID
           align:8 warn_if_not_align:0 symtab:0 alias-set -1 canonical-type 0x7f18ac62ff18
           pointer_to_this (pointer_type 0x7f18ac637000))
 **)
let get_type_if_there (l: Sexp.t list) =
  let after_type = split_after (Sexp.Atom "type" ) l in
  Option.(
    after_type >>=
    (fun lsexp -> List.hd lsexp) >>=
    (fun lsexp ->
       let lst = get_if_list lsexp in
       Option.(List.hd lst >>=
               (fun general_type ->
                  Some (
                    Printf.sprintf "general:%s|exact:%s"
                      (Sexp.to_string general_type)
                      (Option.fold ~init:"None"
                         ~f:(fun _ e -> Sexp.to_string e) (List.nth lst 2)  ) ) )
              )
    )
  )

let rec parse_expr_args (l: Sexp.t list) : Node.expr list option =
  let all_args =  get_all_that_satisfy_f
      ~f:(fun l ->
          match l with
          | hd::tail -> (match (hd,List.hd tail) with
              | (Sexp.Atom _, Some (Sexp.List (x::_))) -> true
              | _ -> false)
          | [] -> false
        ) l in
  let first_sexp_of_all_args = (List.map ~f:(fun (hd,tl) -> (hd, List.hd_exn tl) )all_args ) in
  let expr_of_all_args = List.map ~f:(fun (hd,tl) ->
      parse_expr ( get_if_list tl ) )  first_sexp_of_all_args in
  Some expr_of_all_args

and
  parse_expr : Sexp.t list -> Node.expr = fun l ->
  let hd = List.hd_exn l in
  let tl = List.tl_exn l in
  match hd with
  |  Sexp.Atom t  ->
    let expression_type = t in
    let result_type= get_type_if_there tl in
    let after_type = split_after (Sexp.Atom "type") l in
    Node.{
      expression_type;
      result_type;
      (*A Node will either have type at 1 index or name, so only one of them will be Some at a time*)
      Node.var_name = if Option.is_some result_type then None
        else Option.(( List.nth tl 1 ) >>= (fun x-> Some ( Sexp.to_string  x) )) ;
      args = parse_expr_args (match after_type with
          | Some rem ->  rem
          | None -> tl)
    }
  | Sexp.List l' -> raise ( Parsing_error ("Unexpected list" ^ Sexp.to_string hd) )

let parse_stmt : Sexp.t -> Node.stmt = fun t ->
  match t with
  | Sexp.Atom x -> raise (Parsing_error (Printf.sprintf "Expected a list but got an Atom %s" (Sexp.to_string t) ))
  | Sexp.List l -> { Node.exp = parse_expr l }

let rec parse_stmtlist :  Sexp.t list -> Node.stmt_list  =
  (fun l ->
     let stmts = List.map ~f:List.hd_exn ( split_after_and_get_all (Sexp.Atom "stmt") l  ) in
     Node.({
         result_type= get_type_if_there l;
         all_stmts =  List.map ~f:(parse_stmt) stmts ;
       })
  )

let parse:  Sexp.t -> Node.node_t =
  fun  s   ->
  match s with
  | Sexp.Atom x -> raise (Parsing_error (Printf.sprintf "Invalid atom: %s" x))
  | Sexp.List lst -> (*raise Parsing_error*)
    let hd =  List.hd_exn lst  in
    let tail =   List.tl_exn lst   in
    match hd with
    | Sexp.Atom t  -> ( match t with
        | "statement_list" -> Node.Node (Node.Stmtlist (parse_stmtlist tail))
        | "stmt" -> let stmt =  ((fun x -> Sexp.List x |> parse_stmt) tail) in
          Node.Node (Node.Stmt stmt  )
        | _ -> Node.Node (Node.Expr ( parse_expr lst ))
      )
    | Sexp.List l ->  raise (Parsing_error ("Unexpected list" ^ Sexp.to_string hd))
