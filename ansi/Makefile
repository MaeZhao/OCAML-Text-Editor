default:
	utop -init ansi.ml
	
build:
	ocamlbuild -use-ocamlfind ansi.cmo

test:
	ocamlbuild -use-ocamlfind -tag 'debug' ansi.byte

check:
	bash checkenv.sh && bash checktypes.sh

finalcheck: check
	bash finalcheck.sh

docs:
	mkdir -p doc
	ocamldoc -d doc -html ansi.ml

clean:
	ocamlbuild -clean
	rm -rf doc