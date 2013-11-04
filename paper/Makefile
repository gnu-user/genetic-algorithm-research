
.PHONY: clean
TARGET = sig-alternate


pdf: clean
	latex -interaction=batchmode $(TARGET)
	bibtex $(TARGET)
	pdflatex -interaction=batchmode $(TARGET)
	$(clean)

clean: 
	@rm -f *.log
	@rm -f *.out
	@rm -f *.dvi
	@rm -f *.bbl
	@rm -f *.blg
	@find . -name "*-converted-to*" -exec rm {} \;
	@find . -name "*.aux" -exec rm {} \;

