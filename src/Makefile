# DEFAULT: sha256.o Main.idr
# 	idris -p contrib Main.idr -o main

# clean:
# 	rm -f sha256.o main 

sha256.o: sha256.h sha256.c
	gcc -c -g sha256.c -lssl -lcrypto



# main: main.o util.o
#         gcc -o main main.o util.o
# main.o: main.c util.h
#         gcc -c -g main.c
# util.o: util.c util.h
#         gcc -c -g util.c




# CC = gcc

# cursesrun.o: cursesrun.c cursesrun.h

# clean: .PHONY
# 	rm cursesrun.o

# .PHONY:
