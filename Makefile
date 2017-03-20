all:
	jbuilder build @install

test:
	jbuilder runtest

clean:
	rm -Rf _build

.FORCE:
