PACKAGES=-package oUnit -package yojson -package str
BUILD=ocamlbuild -use-ocamlfind \
	-cflags "-g -w +A"\
	$(PACKAGES)

.PHONY: default test doc clean mrproper
default:
	$(BUILD) src/scrabble.native

test:
	$(BUILD) src/test.native
	./test.native

doc:
	$(BUILD) -ocamldoc "ocamldoc -charset utf8 -stars" \
		src/scrabble.docdir/index.html
	rm -rf doc
	mv _build/src/scrabble.docdir doc
	rm -rf scrabble.docdir

clean:
	rm -rf _build

mrproper: clean
	rm -rf test.native scrabble.native
