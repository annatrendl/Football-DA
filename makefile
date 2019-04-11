abstract.pdf: abstract.tex football_refs.bib
	pdflatex abstract
	bibtex abstract
	pdflatex abstract
	pdflatex abstract
	pdflatex abstract
	pdflatex abstract
	pdftk abstract.pdf output abstract_repaired.pdf
	mv abstract_repaired.pdf  abstract.pdf

diff.tex: original.tex abstract.tex
	latexdiff original.tex abstract.tex > diff.tex

diff.pdf: diff.tex refs.bib
	pdflatex diff
	bibtex diff
	pdflatex diff
	pdflatex diff
	pdflatex diff
	pdflatex diff
	pdftk diff.pdf output diff_repaired.pdf
	mv diff_repaired.pdf  diff.pdf	

#local_refs.bib: abstract.aux refs.bib
#	cat refs.bib | aux2bib abstract.aux > local_refs.bib

abstract.aux: abstract.tex football_refs.bib

make clean:
	rm *.aux *.bbl *.blg *.log *.dvi *.lof *.ttt *.fff *.out *.run.xml *-blx.bib

