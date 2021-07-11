
MODULES= editor test # put things you want compiled here
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
# Temporary make execution for ex files
NOTTY_EX = unix_term_ex.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind 

PKGS=unix oUnit str notty graphics
ANSI=ansi.byte
GRAPH = graph_ex.byte
NOTTY_TXT = notty_text.byte
EDIT = editor.byte
CURSOR = cursor_ex.byte
CURSOR = cursor_ex.byte
TEST = test.byte

default: build
	utop

build: # when you run make build this happens
	$(OCAMLBUILD) $(OBJECTS)

edit:
	$(OCAMLBUILD) $(EDIT) && ./$(EDIT)
# test:
# 	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential
ansii:
	$(OCAMLBUILD) $(ANSI) && ./$(ANSI)

graph:
	$(OCAMLBUILD)  $(GRAPH) && ./$(GRAPH)

text:
	$(OCAMLBUILD)  $(NOTTY_TXT) && ./$(NOTTY_TXT)

cursor:
	$(OCAMLBUILD)  $(CURSOR) && ./$(CURSOR)

test:
	$(OCAMLBUILD)  $(TEST) && ./$(TEST)
# check:
# 	bash checkenv.sh && bash checktypes.sh

# finalcheck: check
# 	bash checkzip.sh
# 	bash finalcheck.sh

# bisect: clean test
# 	bisect-ppx-report -I _build -html report bisect0001.out

play:
	$(OCAMLBUILD) $(NOTTY_EX) && ./$(NOTTY_EX)

zip:
	zip search_src.zip *.ml* _tags Makefile  

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report search_src.zip bisect*.out
