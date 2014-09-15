all:  factorial

factorial: factorial.c cam.c cam.h
	$(CC) factorial.c cam.c -o factorial
