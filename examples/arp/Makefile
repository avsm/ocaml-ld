
TARGET = arp.native
SOURCE = nettypes.ml arp.ml

all: $(TARGET)

$(TARGET): $(SOURCE)
	ocamlbuild -pkg froc $@

clean:
	rm -rf $(TARGET) _build
