# ============================== #
# ===== BSL INTERN MANUAL ====== #
# ============================== #

MANUAL=bslmanual
STYLE=style.css

TEX=$(MANUAL).tex
PS = $(patsubst %.tex, %.ps, $(TEX))
PDF = $(patsubst %.tex, %.pdf, $(TEX))

TARGET=index.html 
# $(MANUAL).html

%.dvi : %.tex
	latex $<
	latex $<
%.ps : %.dvi
	dvips $<
%.pdf : %.ps
	ps2pdf $<

all : $(TARGET)

pdf : $(PDF)

index.html : $(MANUAL).html
	hacha $<
	cp $(STYLE) $(MANUAL).css
	cp mainalgo.png bslmanual001.png

$(MANUAL).html : $(MANUAL).tex
	hevea -fix $(MANUAL).tex
	cp $(STYLE) $(MANUAL).css

clean :
	rm -f *.aux *.log *.toc *.html *.dvi *.out *.pdf

tilde :
	rm -f *~
