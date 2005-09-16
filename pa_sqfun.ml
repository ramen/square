(*pp camlp4o pa_extend.cmo q_MLast.cmo -loc loc *)

let match_case = Grammar.Entry.create Pcaml.gram "match_case";;

EXTEND
  GLOBAL: match_case;

  Pcaml.expr: LEVEL "expr1" [
    [ "sqfun"; OPT "|"; l = LIST1 match_case SEP "|" ->
        <:expr< Value.Function (fun [ $list:l$ ]) >> ]
  ];

  match_case: [
    [ x1 = Pcaml.patt;
      w = OPT [ "when"; e = Pcaml.expr -> e ]; "->";
      x2 = Pcaml.expr -> (x1, w, x2) ]
  ];
END;;
