
TARGET = server.native
SOURCE = conf.ml fn.ml debug.ml server.ml

all: $(TARGET)

$(TARGET): $(SOURCE)
	ocamlbuild -pkg lwt.unix -pkg react -pkg unix $@

clean:
	rm -rf $(TARGET) _build
