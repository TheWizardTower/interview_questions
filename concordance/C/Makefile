concordance: main.c
	gcc main.c -o concordance -g

gdb: concordance
	gdb ./concordance

valgrind: concordance
	valgrind ./concordance < input2.txt
