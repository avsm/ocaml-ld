
TARGETS = ld.cmx memo.cmx switchables.cmx froc_lwt.cmx

all: $(TARGETS)

%.cmx: %.ml
	ocamlbuild -pkg froc -pkg lwt.unix -pkg react -pkg unix $@

clean:
	rm -rf $(TARGETS) _build
