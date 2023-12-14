default:	example

all:	example docs tools

tools:	bin/skyline-tool

docs:	Dist/V.smile.homebrew.pdf

example:	Dist/V.smile.example.bin

bin/skyline-tool:	bin/buildapp bin/zx7mini \
	SkylineTool/skyline-tool.asd \
	$(shell ls SkylineTool/*.lisp SkylineTool/src/*.lisp)
	mkdir -p bin
	@echo "Note: This may take a while if you don't have some common Quicklisp \
libraries already compiled. On subsequent runs, though, it'll be much quicker." >&2
	bin/buildapp --output bin/skyline-tool \
		--load SkylineTool/setup.lisp \
		--load-system skyline-tool \
		--entry skyline-tool::command

bin/buildapp:
	sbcl --load SkylineTool/prepare-system.lisp --eval '(cl-user::quit)'

YEAR=$(shell date +%Y)
YEAR2=$(shell date +%y)
MONTH=$(shell date +%m)
DATE=$(shell date +%d)
JULIAN=$(shell date +%j)
BUILD=$(shell date +%y.%j)

Dist/V.smile.homebrew.pdf:	Manual/homebrew.tex
	mkdir -p Object/pdf
	cp $< Object/pdf/
	ln -sf ../Manual Object/
	-cd Object/pdf ; xelatex -interaction=batchmode homebrew
	-cd Object/pdf ; xelatex -interaction=batchmode homebrew
	-cd Object/pdf ; xelatex -interaction=batchmode homebrew
	mkdir -p Dist
	mv Object/pdf/homebrew.pdf Dist/V.smile.homebrew.pdf
