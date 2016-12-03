name := report
all: $(name).pdf
$(name).pdf: $(name).tex
	mkdir -p build
	pdflatex -output-directory build/ $(name)
	pdflatex -output-directory build/ $(name)
	mv build/$(name).pdf .
clean:
	rm -rf build
distclean: clean
	rm -f $(name).pdf
.PHONY: all clean
