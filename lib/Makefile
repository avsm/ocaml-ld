
TARGET = memo.cmx
SOURCE = memo.ml

all: $(TARGET)

$(TARGET): $(SOURCE)
	ocamlbuild -pkg lwt.unix -pkg react -pkg unix $@

clean:
	rm -rf $(TARGET) _build
