(* main.ml - square language main script *)

(* todo:
     - change comment characters (lexer)
     - allow ? and ! in names (lexer)
     - raw strings? (lexer)
     - try/catch DONE! finally?
     - slice
     - polymorphic add/remove (in progress)
     - "in" for strings and lists (index-based)
     - better parser error reporting
     - Graphics
     - tutorial
     - sourceforge
*)

open Ast
open Value
open Eval
open Name
open Names
open Prelude
open Parse
open Printf

let version = "0.2.0"
let prompt = ":: "
let dump_ast = false

let rec toploop () =
  let rec astloop stream =
    match stream with parser
      | [< 'ast >] ->
          if dump_ast then print_endline (AST.to_string ast);
          (try
             print_endline (Value.to_string (eval global_env ast))
           with
             | Value.Error e ->
                 print_endline ("Error: " ^ (Value.to_string e)));
          print_string prompt;
          flush stdout;
          astloop stream
      | [< >] -> () in
  try
    print_string prompt;
    flush stdout;
    astloop (parse_stream (lexer (Stream.of_channel stdin)))
  with
    | Stream.Error "" -> (print_endline "Syntax error"; toploop ())
    | Stream.Error e -> (print_endline ("Syntax error: " ^ e); toploop ())
    | Stream.Failure -> (print_endline "Syntax error"; toploop ())
    | _ as e ->
        (print_endline ("Uncaught exception: " ^ (Printexc.to_string e));
         toploop ())

let load_file filename =
  let in_channel =
    if filename = "-"
    then stdin
    else open_in filename in
  let line_num = ref 1 in
  let in_stream = Stream.of_channel in_channel in
  let drop_shebang = parser
    | [< ''#'; ''!'; xs >] ->
        (let rec passthru = parser
           | [< 'x; xs >] -> [< 'x; passthru xs >]
           | [< >] -> [< >]
         and junk_line = parser
           | [< ''\n'; xs >] -> [< passthru xs >]
           | [< 'x; xs >] -> [< junk_line xs >]
           | [< >] -> [< >] in
           junk_line xs)
    | [< xs >] -> [< xs >]
    | [< >] -> [< >] in
  let rec count_lines = parser
    | [< 'x; xs >] ->
        if x = '\n' then line_num := !line_num + 1;
        [< 'x; count_lines xs >]
    | [< >] -> [< >] in
  let rec astloop stream =
    match stream with parser
      | [< 'ast >] ->
          (try
             ignore (eval global_env ast)
           with
             | Value.Error e ->
                 Value.fail Names.e_eval (sprintf "Error in line %d of %s: %s" !line_num filename (Value.to_string e)));
          astloop stream
      | [< >] -> () in
    try
      astloop (parse_stream (lexer (count_lines (drop_shebang in_stream))))
    with
      | Stream.Error "" -> Value.fail Names.e_eval (sprintf "Syntax error in line %d of %s" !line_num filename); toploop ()
      | Stream.Error e -> Value.fail Names.e_eval (sprintf "Syntax error in line %d of %s: %s" !line_num filename e); toploop ()
      | Stream.Failure -> Value.fail Names.e_eval (sprintf "Syntax error in line %d of %s" !line_num filename); toploop ()
      | Value.Error e as e' -> raise e'
      | _ as e ->
          Value.fail Names.e_eval (sprintf "Uncaught exception in line %d of %s: %s" !line_num filename (Printexc.to_string e));
          toploop ()

let def = add_binding global_env in
  def "load"
    (Value.Function
       (function
          | Value.String filename -> load_file filename; Value.None
          | _ -> Value.fail Names.e_type "argument must be a string"))

let _ =
  match Array.length Sys.argv with
    | 1 ->
        printf "Welcome to [s]quare version %s!\n" version;
        toploop ();
        print_newline ()
    | _ ->
        (try
           load_file Sys.argv.(1)
         with
           | Value.Error (Value.Record (n, r))
               when n == Names.record
                 && NameMap.mem Names.e_eval r ->
               print_endline (Value.to_string (NameMap.find Names.e_eval r))
           | Value.Error v ->
               print_endline (Value.to_string v))
