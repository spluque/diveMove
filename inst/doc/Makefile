##_+ Makefile for LaTeX files ---------------------------------------------
# We don't need to do anything with the Sweave files, as R CMD build
# weaves them before calling make

# LATEX	= latex
PDFLATEX = pdflatex
BIBTEX = bibtex
# DVIPS = dvips
# PSTOPDF = ps2pdf
TEXTRASH = *.*ps $*.dvi $*.bbl $*.aux $*.blg *.log $*.toc $*.out auto/

all : diveMove.pdf

%.pdf :	%.tex
	R CMD Rd2dvi --pdf -o 'diveMove-rd.pdf' --title='diveMove' ../../man/
	$(PDFLATEX) $*
	$(BIBTEX) $*
	$(PDFLATEX) $*
	$(PDFLATEX) $*
	$(PDFLATEX) $*
	rm -rf $(TEXTRASH) diveMove-*
