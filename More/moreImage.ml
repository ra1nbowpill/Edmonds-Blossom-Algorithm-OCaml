open Vg
open Gg
       
let common_font =
  let open Font in
  { name = "sans-serif";
    size = 1.0;
    weight = `W400;
    slant = `Normal
  }

let estimated_width_of_text text =
  let average_char_length = 0.55 in
  (String.length text |> max 1 |> float) *. average_char_length

let circle_node ?(radius=1.) ?(center=P2.o) ~text =
  let radius = 0.3 in
  let circle = P.empty |> P.circle P2.o radius in
  let width = estimated_width_of_text text in
  let size = 0.6 *. radius /. width in
  let font = { common_font with Font.size = size } in
  ( I.const Color.white
    |> I.cut ~area:(`Anz) circle
  )
  |> I.blend
       ( I.const Color.black
	 |> I.cut ~area:(`O { P.o with P.width = 0.02 }) circle
       )
  |> I.blend
       (I.const Color.black
	|> I.cut_glyphs ~text font []
	|> I.move (V2.smul radius (V2.v (-. width /. 2.) (-0.3)))
       )
  |> I.move center


let null_image =
  let path =
    P.empty
    |> P.sub (P2.v (-1.) (0.)) |> P.line (P2.v 1. 0.)
    |> P.sub P2.o |> P.line (P2.v 0. 1.5)
  in
  let area = `O { P.o with P.width = 0.1 } in
  ( I.const Color.black |> I.cut ~area path,
    Box2.v (P2.v (-3.) (-1.)) (V2.v 6. 4.)
  )




type any = [ `Arc | `Vertex ]
type vertex = [ `Vertex ]
type arc = [`Arc]

type 'element property =
  | DrawColor : Gg.color -> [< `Arc | `Vertex] property
  | FillColor : Gg.color option -> [< `Arc | `Vertex] property
  | LineWidth : float -> [<`Arc | `Vertex] property
  | Radius : float -> vertex property
  | Offset : float -> arc property
  | DrawTriangle : bool -> arc property (* TODO *)



module Properties = struct
    

    let drawing_color color = DrawColor color
    let filling_color color = FillColor (Some color)
    let linewidth w = LineWidth w
    let radius r = Radius r
    let offset off = Offset off
    let draw_triangle b = DrawTriangle b (* TODO *)

    let get_property getter default properties =
      properties
      |> List.map getter 
      |> MoreList.find ~predicate:(fun elt -> elt <> None)
      |> MoreOption.flatten
      |> MoreOption.default default

			    
    let get_drawing_color ?(default=Gg.Color.black) =
      get_property
	(function DrawColor c -> Some c | _ -> None)
	default
    let get_filling_color ?(default=Some Gg.Color.black) =
      get_property
	(function FillColor c -> Some c | _ -> None)
	default
    let get_linewidth ?(default=0.01) =
      get_property
	(function LineWidth lw -> Some lw | _ -> None)
	default
    let get_radius ?(default=0.1) =
      get_property
	(function Radius r -> Some r | _ -> None)
	default
    let get_offset ?(default=0.2) =
      get_property
	(function Offset off -> Some off | _ -> None)
	default
    let get_draw_triangle ?(default=true) =
      get_property (* TODO *)
	(function DrawTriangle b -> Some b | _ -> None)
	default

    let remove_drawing_color =
      MoreList.remove
	~predicate:(function DrawColor _ -> true | _ -> false)
    let remove_filling_color =
      MoreList.remove
	~predicate:(function FillColor _ -> true | _ -> false)
    let remove_linewidth =
      MoreList.remove
	~predicate:(function LineWidth lw -> true | _ -> false)
    let remove_radius =
      MoreList.remove
	~predicate:(function Radius r -> true | _ -> false)
    let remove_offset =
      MoreList.remove
	~predicate:(function Offset off -> true | _ -> false)
    let remove_draw_triangle =
      MoreList.remove (* TODO *)
	~predicate:(function DrawTriangle b -> true | _ -> false)

	
  end

open Properties

let draw_arc ~properties from_point to_point image =
  let draw_color = get_drawing_color properties in
  let fill_color = get_filling_color properties in 
  let linewidth = get_linewidth properties in
  let offset = get_offset properties in
  let draw_triangle = get_draw_triangle properties in
  let vec = Gg.(V2.sub to_point from_point |> V2.unit) in
  let normal = Gg.V2.ortho vec in
  let tip_length = 10. *. linewidth in
  let tip_short_length = 7. *. linewidth in 
  let tip_width = 3. *. linewidth in 
  let start_point = Gg.V2.(from_point + offset * vec) in 
  let tip_triangle = Gg.V2.(to_point - offset * vec) in
  let base_triangle = Gg.V2.(tip_triangle - tip_short_length * vec) in
  let left_triangle =
    Gg.V2.(tip_triangle + tip_width * normal - tip_length * vec)
  in
  let right_triangle =
    Gg.V2.(tip_triangle - tip_width * normal - tip_length * vec)
  in
  let path = (* TODO *)
    if draw_triangle then
      Vg.(
      P.empty
      |> P.sub start_point
      |> P.line base_triangle
      |> P.sub base_triangle
      |> P.line left_triangle
      |> P.line tip_triangle
      |> P.line right_triangle
      |> P.close
      )
    else
      Vg.(
      P.empty
      |> P.sub start_point
      |> P.line base_triangle
      |> P.close
      )
  in
  let outline =
    Vg.P.(
      { o with
        width = linewidth;  
      })
  in
  let filling =
    match fill_color with
    | None -> Vg.I.void
    | Some color ->
       Vg.I.const color 
       |> Vg.I.cut ~area:`Aeo path
  in
  let drawing =
    Vg.I.const draw_color
    |> Vg.I.cut ~area:(`O outline) path
  in
  image
  |> Vg.I.blend filling 
  |> Vg.I.blend drawing 



let draw_vertex ~properties position image =
  let draw_color = get_drawing_color properties in
  let fill_color = get_filling_color properties in
  let line_width = get_linewidth properties in
  let radius = get_radius properties in
  let path = Vg.P.circle position radius Vg.P.empty in
  let outline =
    Vg.P.(
      { o with
        width = line_width
      })
  in
  let filling = match fill_color with
    | None -> Vg.I.void
    | Some color ->
       Vg.I.const color
       |> Vg.I.cut ~area:`Aeo path
  in
  let drawing =
    Vg.I.const draw_color 
    |> Vg.I.cut ~area:(`O outline) path
  in
  image
  |> Vg.I.blend filling
  |> Vg.I.blend drawing




let get_best_box graph layout =
  let get_extreme proj optimum =
    let f vertex = vertex |> layout |> proj |> optimum in
    Graph.fold_vertices ~f graph 0.
  in
  let open Gg in
  let x_min = get_extreme V2.x min in 
  let y_min = get_extreme V2.y min in 
  let x_max = get_extreme V2.x max in 
  let y_max = get_extreme V2.y max in
  Box2.v
    (P2.v (x_min -. 1.) (y_min -. 1.))
    (V2.v (x_max -. x_min +. 2.) (y_max -. y_min +. 2.))




let draw_graph
      ?(vertex_properties=fun v ->[])
      ?(arc_properties=fun arc -> [])
      ~layout
      graph =
  let draw_vertex vertex =
    draw_vertex
      ~properties:(vertex_properties vertex)
      (layout vertex)
  in
  let draw_arc (src, dst) =
    draw_arc
      ~properties:(arc_properties (src,dst))
      (layout src)
      (layout dst)
  in
  let image =
    Vg.I.const Gg.Color.white
    |> Graph.fold_vertices ~f:draw_vertex graph
    |> Graph.fold_arcs ~f:draw_arc graph
  in
  let box = get_best_box graph layout in
  (image,box)

