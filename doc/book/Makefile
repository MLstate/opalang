OPA ?= opa
OPA_PLUGIN_BUILDER ?= opa-plugin-builder

export

HTML2PDF=wkhtmltopdf
ADOCDIR=/opt/local/etc/asciidoc/

EXAMPLES= \
  hello_chat/hello_chat.exe \
  hello_wiki/hello_wiki.exe \
  hello_web_services/hello_wiki_rest.exe \
  hello_web_services/hello_wiki_rest_client.exe \
  hello_web_services/hello_wiki_rest_client_customizable.exe \
  hello_recaptcha/hello_recaptcha_app.exe

all: book.pdf index.html

zip: ../book.zip

lang.map: more_lang.map opa.lang grammar.lang
	rm -f lang.map
	source-highlight --lang-list >> lang2.map
	cat lang2.map more_lang.map > lang.map
	rm -f lang2.map

book.pdf: lang.map *.adoc */*.adoc asciidoc.conf
	a2x --asciidoc-opts="--conf asciidoc.conf" -d book -v --dblatex-opts="--texstyle=style.sty -d" -a toc main.adoc;
	mv main.pdf book.pdf

book.xml: lang.map *.adoc */*.adoc opalang.css */*.opa
	asciidoc --conf asciidoc.conf -a toclevel=4 -a theme=slidy -a stylesheet=$(CURDIR)/opalang.css -a toc -a numbered -b docbook -v -d book main.adoc;
	mv main.xml book.xml

book_multipart/index.html: book.xml
	xsltproc --stringparam html.stylesheet opalang.css -o book_multipart/ --nonet chunked.xsl book.xml

book3.pdf: lang.map *.adoc */*.adoc asciidoc.conf mlstatebook.cls
	a2x --asciidoc-opts="--conf asciidoc.conf" -d book -v --dblatex-opts="--texstyle=style.sty -d -P latex.class.book=mlstatebook" -a toc main.adoc;
	mv main.pdf book3.pdf

index.html: book.xml
	asciidoc --conf asciidoc.conf -a toclevel=4 -a theme=slidy -a stylesheet=$(CURDIR)/opalang.css -a toc -a numbered -b xhtml11 -v main.adoc;
	mv main.html index.html


book2.pdf: index2.html Makefile toc.xsl print-cover.html print-footer.html print-header.html
	$(HTML2PDF)  --enable-toc-back-links  --print-media-type --margin-left 20mm --margin-right 20mm --margin-top 30mm --margin-bottom 30mm --page-width 210 --page-height 297 --header-left "[section]" --header-right "[subsection]" --header-font-name serif --header-font-size 8 --header-line --header-spacing 5 --footer-left "Simple, Secure, Scalable Web Development with Opa" --footer-right "[page]" --footer-font-name serif --footer-font-size 8 --footer-line --footer-spacing 2 --debug-javascript cover print-cover.html  toc --xsl-style-sheet toc.xsl --disable-dotted-lines index2.html book2.pdf

index2.html: index.html
	asciidoc --conf asciidoc.conf -a theme=slidy -a stylesheet=$(CURDIR)/opalang.css -b xhtml11 -v -d book -a numbered main.adoc;
	mv main.html index2.html

test-chunked.html: test.html
	asciidoc --conf asciidoc.conf -a theme=slidy -a stylesheet=$(CURDIR)/opalang.css -b xhtml11 -v -d book -a numbered --nonet $(ADOCDIR)docbook-xsl/chunked.xsl test.adoc


test.html: *.adoc */*.adoc opalang.css
	asciidoc --conf asciidoc.conf -a theme=slidy -a stylesheet=$(CURDIR)/opalang.css -b xhtml11 -v -d book -a numbered test.adoc

test.pdf: test.html Makefile toc.xsl print-cover.html print-footer.html print-header.html
	$(HTML2PDF)  --enable-toc-back-links  --print-media-type --margin-left 20mm --margin-right 20mm --margin-top 30mm --margin-bottom 30mm --page-width 210 --page-height 297 --header-left "[section]" --header-right "[subsection]" --header-font-name serif --header-font-size 8 --header-line --header-spacing 5 --footer-left "Simple, Secure, Scalable Web Development with Opa" --footer-right "[page]" --footer-font-name serif --footer-font-size 8 --footer-line --footer-spacing 2 --debug-javascript cover print-cover.html  toc --xsl-style-sheet toc.xsl --disable-dotted-lines test.html test.pdf

../book.zip: clean index.html
	zip ../book.zip *

examples: $(EXAMPLES)
	make -C also_bindings/sources
	make -C also_bindings/sources/c_binding
	make -C also_bindings/sources/external_types
	make -C also_bindings/sources/opa_types

hello_recaptcha/%.exe:
	make -C $(@D) $(@F)

%.exe: %.opa
	cd $(<D) && $(OPA) $(<F)

clean-examples:
	rm -rf */_build */_tracks */*.opp */*.opx
	rm -f */*.exe

clean: clean-examples
	rm -f index*.html test*.html *.pdf *.log book_multipart/*
