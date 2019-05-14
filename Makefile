%.ml: %.ipynb
		jupyter nbconvert --to ocaml $^ 

notebook: while.ml
		jupyter notebook
