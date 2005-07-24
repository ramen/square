open Name
open Names
open Value
open Eval
open Prelude

let color_to_record color =
  let (r, g, b) =
    (color lsr 16 land 0xff,
     color lsr 8 land 0xff,
     color lsr 0 land 0xff) in
  (Value.record Names.record [
     Name.of_string "r", Value.Int r;
     Name.of_string "g", Value.Int g;
     Name.of_string "b", Value.Int b;
   ])

let def = add_binding global_env in

def "Graphics"
  (Value.record Names.module_ [
     Name.of_string "open_graph",
     (sqfun
        | Value.String s ->
            Graphics.open_graph s;
            Graphics.set_window_title "[s]quare graphics";
            Value.None
        | _ ->
            Value.fail Names.e_type "argument must be a string");

     Name.of_string "close_graph",
     (sqfun
        | Value.None ->
            Graphics.close_graph ();
            Value.None
        | _ ->
            Value.fail Names.e_type "function takes no arguments");

     Name.of_string "set_window_title",
     (sqfun
        | Value.String s ->
            Graphics.set_window_title s;
            Value.None
        | _ ->
            Value.fail Names.e_type "argument must be a string");

     Name.of_string "clear_graph",
     (sqfun
        | Value.None ->
            Graphics.clear_graph ();
            Value.None
        | _ ->
            Value.fail Names.e_type "function takes no arguments");

     Name.of_string "size",
     (sqfun
        | Value.None ->
            Value.record Names.record [Name.of_string "width",
                                       Value.Int (Graphics.size_x ());
                                       Name.of_string "height",
                                       Value.Int (Graphics.size_y ())]
        | _ ->
            Value.fail Names.e_type "function takes no arguments");     

     Name.of_string "set_color_rgb",
     (sqfun
        | Value.List [Value.Int r; Value.Int g; Value.Int b] ->
            Graphics.set_color (Graphics.rgb r g b);
            Value.None
        | Value.List _ ->
            Value.fail Names.e_type "function requres 3 ints"
        | _ ->
            Value.fail Names.e_type "argument must be a list");
       
     Name.of_string "foreground", color_to_record (Graphics.foreground);
     Name.of_string "background", color_to_record (Graphics.background);
     Name.of_string "black", color_to_record (Graphics.black);
     Name.of_string "white", color_to_record (Graphics.white);
     Name.of_string "red", color_to_record (Graphics.red);
     Name.of_string "green", color_to_record (Graphics.green);
     Name.of_string "blue", color_to_record (Graphics.blue);
     Name.of_string "yellow", color_to_record (Graphics.yellow);
     Name.of_string "cyan", color_to_record (Graphics.cyan);
     Name.of_string "magenta", color_to_record (Graphics.magenta);

     Name.of_string "plot_xy",
     (sqfun
        | Value.List [Value.Int x; Value.Int y] ->
            Graphics.plot x y;
            Value.None
        | Value.List _ ->
            Value.fail Names.e_type "function requires 2 ints"
        | _ ->
            Value.fail Names.e_type "argument must be a list");

     Name.of_string "point_color_xy",
     (sqfun
        | Value.List [Value.Int x; Value.Int y] ->
            color_to_record (Graphics.point_color x y)
        | Value.List _ ->
            Value.fail Names.e_type "function requires 2 ints"
        | _ ->
            Value.fail Names.e_type "argument must be a list");

     Name.of_string "moveto_xy",
     (sqfun
        | Value.List [Value.Int x; Value.Int y] ->
            Graphics.moveto x y;
            Value.None
        | Value.List _ ->
            Value.fail Names.e_type "function requires 2 ints"
        | _ ->
            Value.fail Names.e_type "argument must be a list");

     Name.of_string "rmoveto_xy",
     (sqfun
        | Value.List [Value.Int x; Value.Int y] ->
            Graphics.rmoveto x y;
            Value.None
        | Value.List _ ->
            Value.fail Names.e_type "function requires 2 ints"
        | _ ->
            Value.fail Names.e_type "argument must be a list");

     Name.of_string "current",
     (sqfun
        | Value.None ->
            Value.record Names.record [Name.of_string "x",
                                       Value.Int (Graphics.current_x ());
                                       Name.of_string "y",
                                       Value.Int (Graphics.current_y ())]
        | _ ->
            Value.fail Names.e_type "function takes no arguments");

     Name.of_string "lineto_xy",
     (sqfun
        | Value.List [Value.Int x; Value.Int y] ->
            Graphics.lineto x y;
            Value.None
        | Value.List _ ->
            Value.fail Names.e_type "function requires 2 ints"
        | _ ->
            Value.fail Names.e_type "argument must be a list");

     Name.of_string "rlineto_xy",
     (sqfun
        | Value.List [Value.Int x; Value.Int y] ->
            Graphics.rlineto x y;
            Value.None
        | Value.List _ ->
            Value.fail Names.e_type "function requires 2 ints"
        | _ ->
            Value.fail Names.e_type "argument must be a list");

   ]);
()

let _ =
  try ignore (eval_string global_env "

!update Graphics {
  set_color: fun {r, g, b} -> Graphics.set_color_rgb (r, g, b),
  plot: fun {x, y} -> Graphics.plot_xy (x, y),
  plots: fun points -> each points [Graphics.plot],
  point_color: fun {x, y} -> Graphics.point_color_xy (x, y),
  moveto: fun {x, y} -> Graphics.moveto_xy (x, y),
  rmoveto: fun {x, y} -> Graphics.rmoveto_xy (x, y),
  lineto: fun {x, y} -> Graphics.lineto_xy (x, y),
  rlineto: fun {x, y} -> Graphics.rlineto_xy (x, y),
};

") with Value.Error e ->
    print_endline ("Graphics Error: " ^ (Value.to_string e))
