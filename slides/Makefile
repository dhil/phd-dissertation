TEXC=pdflatex
CFLAGS=-interaction=nonstopmode -halt-on-error -file-line-error
BIBC=bibtex
PAPER=viva
BIBLIO=$(PAPER)
LATEST_COMMIT=$(shell git log --format="%h" -n 1)

all: $(PAPER).pdf
draft: $(PAPER).pdf-draft

$(PAPER).aux: $(PAPER).tex
	$(TEXC) $(CFLAGS) $(PAPER)

$(BIBLIO).bbl: $(PAPER).aux $(BIBLIO).bib
	$(BIBC) $(PAPER)

$(PAPER).pdf: $(PAPER).aux $(BIBLIO).bbl
	$(TEXC) $(CFLAGS) $(PAPER)
	$(TEXC) $(CFLAGS) $(PAPER)

$(PAPER).pdf-draft: CFLAGS:=$(CFLAGS) "\def\DRAFT{$(LATEST_COMMIT)}\input{$(PAPER)}"
$(PAPER).pdf-draft: all
	mv $(PAPER).pdf $(PAPER)-draft.pdf
	tar cf thesis-draft.tar.gz $(PAPER)-draft.pdf

clean:
	rm -f *.log *.aux *.toc *.out
	rm -f *.bbl *.blg *.fls *.xml
	rm -f *.nav *.snm
	rm -f *.fdb_latexmk *.vtc *.cut
	rm -f $(PAPER).pdf camera-ready.pdf submission.pdf
	rm -f *.o *.cmx *.cmo
