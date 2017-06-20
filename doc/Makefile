all:
	pandoc \
	--filter pandoc-citeproc \
	--template template.texinfo \
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
	R0_references.md \
	-o categories_for_programmers.pdf

html:
	pandoc \
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
	A0_ideas.md \
	A1_exercises.md \
	A2_haskell.md \
	-s \
	--mathjax \
	-t html5 \
	-o categories_for_programmers.html
