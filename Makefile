TEXC=pdflatex
CFLAGS=-interaction=nonstopmode -halt-on-error -file-line-error
BIBC=bibtex
PAPER=thesis
BIBLIO=$(PAPER)

all: $(PAPER).pdf

$(PAPER).aux: $(PAPER).tex
	$(TEXC) $(CFLAGS) $(PAPER)

$(BIBLIO).bbl: $(PAPER).aux $(BIBLIO).bib
	$(BIBC) $(PAPER)

$(PAPER).pdf: $(PAPER).aux $(BIBLIO).bbl
	$(TEXC) $(CFLAGS) $(PAPER)
	$(TEXC) $(CFLAGS) $(PAPER)

clean:
	rm -f *.log *.aux *.toc *.out
	rm -f *.bbl *.blg *.fls *.xml
	rm -f *.fdb_latexmk *.vtc *.cut
	rm -f $(PAPER).pdf camera-ready.pdf submission.pdf
	rm -f *.o *.cmx *.cmo
