CC = gcc
CFLAGS = -Wall	-Wextra	-pedantic
ARCHIVO ?= 

all: $(ARCHIVO)
	@./$(ARCHIVO)

$(ARCHIVO): $(ARCHIVO).o
	$(CC) $(CFLAGS) $(ARCHIVO).o -o $(ARCHIVO)

$(ARCHIVO).o: $(ARCHIVO).c	
	$(CC) $(CFLAGS) -c $(ARCHIVO).c -o $(ARCHIVO).o

	

clean:
	rm *.o $(ARCHIVO)

.PHONY: all clean