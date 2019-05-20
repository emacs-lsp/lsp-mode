.DEFAULT_GOAL: all

doc-html:
	asciidoctor --safe -b html5 -B ./src/ -D output -o lsp-mode.html ./src/index.adoc
	cp ./src/output/* ./output/

doc-pdf:
	asciidoctor-pdf  --safe -b html5 -B ./src/ -D output -o lsp-mode.pdf ./src/index.adoc
	cp ./src/output/* ./output/

all: doc-html doc-pdf

clean:
	rm -rf ./src/output output
