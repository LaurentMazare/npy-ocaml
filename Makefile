npy.lib: .FORCE
	jbuilder build src/npy.cmi src/npy.cmxa

test:
	jbuilder runtest

clean:
	rm -Rf _build

.FORCE:
