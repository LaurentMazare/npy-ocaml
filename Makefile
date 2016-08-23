npy.lib: .FORCE
	ocamlbuild npy.cmxa npy.cma npy.cmxs npy.cmx

test:
	ocamlbuild tests/test.native
	./test.native

clean:
	rm -Rf _build *.native *.npy

.FORCE:
