open! Base
(* SOURCE: https://graphviz.org/doc/info/lang.html *)
type label = string [@@deriving sexp]

type shape_type =
  | Record
  | Normal [@@deriving sexp]

type attr = {
  shape: shape_type;
  lbl: label
} [@@deriving sexp]

type node_stmt = {
  name: string;
  attr: attr option
}[@@deriving sexp]

type edge_stmt = {
  edge: node_stmt * stmt;
  is_directed : bool;
  lbl: label
} [@@deriving sexp]
and
  stmt =
  | Node_stmt of node_stmt
  | Edge_stmt of edge_stmt
  | Subgraph of graph
  (* | Attr_stmt  (*Not going to allow a lot of customization, only labels will be used,so no need for this*)*)
(* | Equal_stmt (*Do not know it's use case*) *)
[@@deriving sexp]
and
  stmt_list = stmt list [@@deriving sexp]
and
  graph = {
  stmt_list : stmt list;
  cluster: string;
} [@@deriving_sexp]
