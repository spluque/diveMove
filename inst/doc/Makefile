## $Id$

##_+ Prepare the vignettes ------------------------------------------------
# We don't need to do anything with the Sweave files, as R CMD build
# weaves them before calling make
LATEX=pdflatex
BIBTEX=bibtex
TEXTRASH=*.*ps $*.dvi $*.bbl $*.aux $*.blg *.log $*.toc $*.out $*.tex auto/
RMOTHERTRASH=find . -name '*.png' -a ! -name 'tcltk.png' -delete

all : diveMove.pdf

%.pdf :	%.tex
	R CMD Rd2dvi --pdf -o 'diveMove-rd.pdf' --title='diveMove' ../../man/
	$(LATEX) $*
	$(BIBTEX) $*
	$(LATEX) $*
	$(LATEX) $*
	$(LATEX) $*
	rm -rf $(TEXTRASH) diveMove-*
	$(RMOTHERTRASH)
