BASE=anaDJIA

all: $(BASE).pdf

$(BASE).tex: $(BASE).Rnw
		Rscript -e "library('knitr'); knit('$(BASE).Rnw')"
		
$(BASE).pdf: $(BASE).tex
	xelatex $(BASE)
	bibtex $(BASE)
	xelatex $(BASE)
	xelatex $(BASE)
	
clean:
	rm -rf figure
	rm -rf *.a*
	rm -rf *.b*
	rm -rf *.l*
	rm -rf *.t*
	rm -rf *.out
	rm -rf *.gz
	
distclean: $(BASE).pdf clean
			