open Base
type expr = {
  expression_type: string;
  result_type: string option;
  var_name: string option;
  args: ( string * expr ) list option;
} [@@deriving sexp]

type stmt = {
  exp: expr;
} [@@deriving sexp]

type stmt_list = {
  result_type: string option;
  all_stmts : stmt list;
} [@@deriving sexp]


type _ node  =
  | Stmt : stmt -> stmt node
  | Expr : expr -> expr node
  | Stmtlist : stmt_list -> stmt_list node
[@@deriving sexp_of]


type node_t = Node : 'any node -> node_t [@@deriving sexp_of]
