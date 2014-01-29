## $Id$

###_+ Custom VARS ---------------------------------------------------------

# We don't need to do anything with the Sweave files, as R CMD build
# weaves them before calling make
LATEX=pdflatex
BIBTEX=bibtex
TEXTRASH=*.*ps *.dvi *.bbl *.aux *.blg *.log *.toc *.out *.tex auto/
RMOTHERTRASH=-find . -name '*.png' -a ! -name 'tcltk.png' -delete


###_+ Phonies -------------------------------------------------------------

.PHONY : clean

###_+ Rules ---------------------------------------------------------------

all : diveMove.pdf

%.pdf : %.tex
	$(R_HOME)/bin/R CMD Rd2pdf --no-preview -o '$*-rd.pdf' \
	--title='$*' ../man/
	$(LATEX) $*
	$(BIBTEX) $*
	$(LATEX) $*
	$(LATEX) $*
	$(LATEX) $*
	rm -rf $(TEXTRASH) $*-*
	# gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dPDFSETTINGS=/ebook \
	# -dCompatibilityLevel=1.5 -dAutoRotatePages=/None \
	# -sOutputFile='$*_compressed.pdf' '$*.pdf'
	# mv -f '$*_compressed.pdf' '$*.pdf'
	$(RMOTHERTRASH)

clean :
	$(RMOTHERTRASH)


###_+ Emacs local variables
## Local variables:
## allout-layout: (1 + : 0)
## End:
