val print_tree : NTree.t -> unit
val print_list_base :
  ('a -> unit) -> ?print_last:('a -> unit) -> 'a list -> unit
val print_couple : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> unit
val print_int : Graph.vertex -> unit
val print_list_beauty : ('a -> unit) -> 'a list -> unit
val print_list : ('a -> unit) -> 'a list -> unit
val print_int_list : Graph.vertex list -> unit
val print_vertex : Graph.vertex -> unit
val print_vertex_list : Graph.vertex list -> unit
val print_arc : Graph.ESet.elt -> unit
val print_arc_list : Graph.ESet.elt list -> unit
val print_eset : Graph.ESet.t -> unit
val print_vset : Graph.VSet.t -> unit
val print_delta :
  string -> (Graph.VSet.elt -> Graph.t -> Graph.ESet.t) -> Graph.t -> unit
val print_delta_in : Graph.t -> unit
val print_delta_out : Graph.t -> unit
