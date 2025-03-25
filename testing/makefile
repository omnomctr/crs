out:  main.o testing.o
	gcc  main.o testing.o -o out

main.o: main
	as main -o main.o

main: main.c ../target/debug/crs
	../target/debug/crs main.c

testing.o: testing.c
	gcc -c testing.c -o testing.o

clean:
	rm -f out main main.o testing.o
