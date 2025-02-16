out: crt.o main.o
	ld crt.o main.o -o out

crt.o: crt.asm
	nasm crt.asm -f elf64

main.o: main
	as main -o main.o

main: main.c ../target/debug/crs
	../target/debug/crs main.c

clean:
	rm out main main.o crt.o
