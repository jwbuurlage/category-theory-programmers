CHAPTERS = \
	00_header.md \
	01_introduction.md \
	02_categories.md \
	03_category_of_types.md \
	04_products.md \
	05_yoneda.md \
	06_ccc.md \
	07_adjunctions.md \
	08_monads.md \
	09_f_algebras.md \
	10_comonads.md \
	11_lenses.md \
	A0_ideas.md \
	A1_exercises.md \
	A2_haskell.md \
	R0_references.md

pdf: categories_for_programmers.pdf
html: categories_for_programmers.html
epub: categories_for_programmers.epub
all: pdf html epub

categories_for_programmers.pdf: $(CHAPTERS)
	pandoc $(CHAPTERS) \
	  --filter pandoc-citeproc \
	  --template template.texinfo \
	  --output=$@

categories_for_programmers.html: $(CHAPTERS)
	pandoc $(CHAPTERS) \
	  --standalone \
	  --mathjax \
	  --to=html5 \
	  --output=$@

categories_for_programmers.epub: $(CHAPTERS)
	pandoc $(CHAPTERS) \
	  --standalone \
	  --to=epub3 \
	  --output=$@

.PHONY: all pdf html epub
