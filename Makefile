test:
	ocamlbuild tests/test.native
	./test.native

clean:
	rm -Rf _build *.native *.npy
