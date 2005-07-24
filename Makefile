all: square

square: name.cmo names.cmo ast.cmo parse.cmo value.cmo eval.cmo prelude.cmo sqGraphics.cmo main.ml
	ocamlc -o square -pp camlp4o unix.cma str.cma graphics.cma \
		name.cmo names.cmo ast.cmo parse.cmo value.cmo eval.cmo prelude.cmo sqGraphics.cmo main.ml

name.cmo: name.ml
	ocamlc -c name.ml

names.cmo: names.ml name.cmo
	ocamlc -c names.ml name.cmo

ast.cmo: ast.ml name.cmo
	ocamlc -c ast.ml name.cmo

parse.cmo: parse.ml
	ocamlc -c -pp camlp4o parse.ml

value.cmo: value.ml
	ocamlc -c value.ml

eval.cmo: eval.ml
	ocamlc -c eval.ml

prelude.cmo: prelude.ml pa_sqfun.cmo
	camlp4o -I . pr_o.cmo pa_sqfun.cmo prelude.ml -o prelude.ppo
	ocamlc -c -pp 'camlp4o -I . pa_sqfun.cmo' prelude.ml

sqGraphics.cmo: sqGraphics.ml pa_sqfun.cmo
	camlp4o -I . pr_o.cmo pa_sqfun.cmo sqGraphics.ml -o sqGraphics.ppo
	ocamlc -c -pp 'camlp4o -I . pa_sqfun.cmo' sqGraphics.ml

pa_sqfun.cmo: pa_sqfun.ml
	camlp4o pa_extend.cmo q_MLast.cmo pr_o.cmo pa_sqfun.ml \
		-o pa_sqfun.ppo -loc loc
	camlp4o pa_extend.cmo q_MLast.cmo pa_sqfun.ml \
		-o pa_sqfun.ast -loc loc
	ocamlc -c -I +camlp4 -pp 'camlp4o pa_extend.cmo q_MLast.cmo -loc loc' \
		pa_sqfun.ml

clean:
	rm -f square *.ppo *.ppr *.cmo *.cmi *.o *.cmx *.ast *~
