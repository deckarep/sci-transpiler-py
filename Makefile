.PHONY: grammar out parse

grammar:
	antlr sexp.g4 -Dlanguage=Python3 -o scicomp/ -visitor

parse:
	python main.py 'sci_files/buildup.sc'

out:
	python main.py 'sci_files/lsl1_dos_2_0_sidewalk.sc' > output/lsl1_dos_2_0_sidewalk.py