TEMPLATES_DIR = ./tests
all : main

main : main.ml
	ocamlbuild -use-menhir main.byte
	
tests: 
	$(foreach file, $(wildcard $(TEMPLATES_DIR)/*), ./main.byte $(file);)
    
clean:
	rm main.byte
	rm -r _build
	
