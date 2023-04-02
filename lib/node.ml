open Base
type expr = {
  expr_name: string;
  typename: string option;
  args: expr list option;
} [@@deriving sexp]

type stmt = {
  exp: expr;
} [@@deriving sexp]

type stmt_list = {
  all_stmts : stmt list;
  typename : string option;
} [@@deriving sexp]


type _ node  =
  | Stmt : stmt -> stmt node
  | Expr : expr -> expr node
  | Stmtlist : stmt_list -> stmt_list node
[@@deriving sexp_of]


type node_t = Node : 'any node -> node_t [@@deriving sexp_of]
