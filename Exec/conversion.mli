val vset_of_eset : Graph.ESet.t -> Graph.VSet.t
val eset_of_vset : Graph.vertex -> Graph.VSet.t -> Graph.ESet.t
val couple_of_list : 'a -> 'b list -> ('a * 'b) list
val vset_of_list : Graph.VSet.elt list -> Graph.VSet.t
val eset_of_list : Graph.ESet.elt list -> Graph.ESet.t
val graph_of_list : (Graph.vertex * Graph.vertex) list -> Graph.t
val tree_of_list : (Graph.vertex * Graph.vertex) list -> NTree.t
