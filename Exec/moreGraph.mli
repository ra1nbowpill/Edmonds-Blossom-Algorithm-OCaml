module VSet = Graph.VSet
module ESet = Graph.ESet
val remove_vertices_from_graph : Graph.t -> Graph.vertex list -> Graph.t
val remove_vset_from_graph : Graph.t -> VSet.t -> Graph.t
val remove_tree_from_graph : Graph.t -> NTree.t -> Graph.t
val saturated_vertices : ESet.t -> VSet.t
val unsaturated_vertices : Graph.t -> ESet.t -> VSet.t
val matching_neighboors_of : Graph.vertex -> ESet.t -> VSet.t
