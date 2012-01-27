
TARGET = ld.cmx
SOURCE = ld.ml

all: $(TARGET)

$(TARGET): $(SOURCE)
	ocamlbuild -pkg lwt.unix -pkg react -pkg unix $@

clean:
	rm -rf $(TARGET) _build
