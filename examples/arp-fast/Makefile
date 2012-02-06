
TARGET = arp.native
SOURCE = froc_lwt.ml memo.ml nettypes.ml arp.ml

all: $(TARGET)

$(TARGET): $(SOURCE)
	ocamlbuild -pkg froc -pkg lwt $@

clean:
	rm -rf $(TARGET) _build
