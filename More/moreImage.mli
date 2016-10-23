val circle_node :
  ?radius:float
  -> ?center:Gg.p2
  -> text:string
  -> Vg.image 


val null_image : Vg.image * Gg.box2


type 'element property

module Properties : sig
  

  val drawing_color : Gg.color -> [< `Arc | `Vertex] property

  val filling_color : Gg.color -> [< `Arc | `Vertex] property

  val linewidth : float -> [< `Arc | `Vertex] property

  val radius : float -> [`Vertex] property

  val offset : float -> [`Arc] property

  val draw_triangle : bool -> [`Arc] property

  val remove_drawing_color :
    ([< `Arc | `Vertex] property list as 'prop_list) -> 'prop_list 
  val remove_filling_color :
    ([< `Arc | `Vertex] property list as 'prop_list) -> 'prop_list 
  val remove_linewidth :
    ([< `Arc | `Vertex] property list as 'prop_list) -> 'prop_list 
  val remove_radius :
    ([`Vertex] property list as 'prop_list) -> 'prop_list 
  val remove_offset :
    ([`Arc] property list as 'prop_list) -> 'prop_list
  val remove_draw_triangle :
    ([`Arc] property list as 'prop_list) -> 'prop_list


  val get_drawing_color :
    ?default:Gg.color -> [< `Arc | `Vertex] property list -> Gg.color 
  val get_filling_color :
    ?default:Gg.color option
    -> [< `Arc | `Vertex] property list -> Gg.color option
  val get_linewidth :
    ?default:float -> [< `Arc | `Vertex] property list -> float
  val get_radius :
    ?default:float -> [`Vertex] property list -> float
  val get_offset :
    ?default:float -> [ `Arc] property list -> float
  val get_draw_triangle :
    ?default:bool -> [`Arc] property list -> bool

end

    
val draw_arc :
  properties:[`Arc] property list -> Gg.p2 -> Gg.p2 -> Vg.image -> Vg.image

val draw_vertex :
  properties:[`Vertex] property list -> Gg.p2 -> Vg.image -> Vg.image


val draw_graph :
  ?vertex_properties:(Graph.vertex -> [`Vertex] property list)
  -> ?arc_properties:(Graph.arc -> [`Arc] property list)
  -> layout:(Graph.vertex -> Gg.p2)
  -> Graph.t
  -> Vg.image * Gg.box2 
