default: pdf

.PHONY: pdf

pdf:
	pandoc -f markdown+lhs -t pdf ./app/Main.lhs -o ./enunciado/prova1.pdf
