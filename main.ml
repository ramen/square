(* main.ml - square language main script *)

(* todo:
     - change comment characters (lexer)
     - allow ? and ! in names (lexer)
     - raw strings? (lexer)
     - try/catch DONE! finally?
     - load/import
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
open Prelude
open Parse
open Printf

let version = "0.1.8"
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
      | [< _ = Stream.empty >] -> () in
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

let _ =
  printf "Welcome to [s]quare version %s!\n" version;
  toploop ();
  print_newline ()
