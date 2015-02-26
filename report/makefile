default: pdf

pdf:
	lhs2TeX --agda relatorio.lagda -o relatorio.tex
	pdflatex relatorio.tex
	pdflatex relatorio.tex
	bibtex relatorio.aux
	pdflatex relatorio.tex
clean:
	rm *.agdai
	rm *.pdf
