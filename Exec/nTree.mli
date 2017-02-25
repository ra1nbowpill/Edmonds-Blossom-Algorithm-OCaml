(* Arbitrary tree *)

type t = |Node of (Graph.vertex * t list)

(* Getters *)

val value : t -> Graph.vertex

val childs : t -> t list

(* Tree management *)

val depth_of : Graph.vertex -> t -> int

val depth : t -> int

val size : t -> int

val is_leaf : t -> bool

val find : Graph.vertex -> t -> t option

val unsafe_find : Graph.vertex -> t -> t

val mem : Graph.vertex -> t -> bool

val add : Graph.vertex * Graph.vertex -> t -> t

val contract : Graph.vertex -> t -> t

val remove : Graph.vertex -> t -> t

val anti_arc : 'a * 'b -> 'b * 'a
val anti_arcs : Graph.ESet.elt list -> (Graph.vertex * Graph.vertex) list
val anti_arcs_eset : Graph.ESet.t -> Graph.ESet.t

val unoriented_arcs_eset : Graph.ESet.t -> Graph.ESet.t
val unoriented_arcs : Graph.ESet.elt list -> Graph.ESet.elt list

val find_vertices : Graph.vertex list -> t -> t list

val remove_vertices : Graph.vertex list -> t -> t

val contract_path : Graph.vertex -> Graph.vertex list -> t -> t

(* Functions *)

val vertices : t -> int list

val arcs : t -> (Graph.vertex * Graph.vertex) list

val path_from_to : Graph.vertex -> Graph.vertex -> t -> Graph.vertex list
val path_to : Graph.vertex -> t -> Graph.vertex list
val old_path_to : Graph.vertex -> t -> Graph.vertex list

val old_path_from_to : Graph.vertex -> Graph.vertex -> t -> Graph.vertex list

(* Getters with parity *)

val p_vertices : bool -> t -> Graph.vertex list

val p_arcs : bool -> t -> (Graph.vertex * Graph.vertex) list

val p_path_to : bool -> Graph.vertex -> t -> Graph.vertex list

val even_vertices : t -> Graph.vertex list
val uneven_vertices : t -> Graph.vertex list

val even_arcs : t -> (Graph.vertex * Graph.vertex) list
val uneven_arcs : t -> (Graph.vertex * Graph.vertex) list

val even_path_to : Graph.vertex -> t -> Graph.vertex list
val uneven_path_to : Graph.vertex -> t -> Graph.vertex list

val p_arcs_of_path : bool -> 'a list -> ('a * 'a) list

val even_arcs_to : Graph.vertex -> t -> (Graph.vertex * Graph.vertex) list
val uneven_arcs_to : Graph.vertex -> t -> (Graph.vertex * Graph.vertex) list

(* Conversions *)

val eset : t -> Graph.ESet.t

val vset : t -> Graph.VSet.t

val iter : (t -> 'a) -> t -> unit

(* Constructors *)

val empty : t
val create_node : Graph.vertex -> t list -> t
val of_arcs : (Graph.vertex * Graph.vertex) list -> t
val of_eset : Graph.ESet.t -> t
