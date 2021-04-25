#-----------------------FILES-------------------------------------------
EXEC_FAST	:=vm
#-----------------------TOOLS-------------------------------------------
# Compiler
CC = gcc
# Compiler options
CFLAGS = -Wall -Ofast -I.
LFLAGS = -lpng

# ----------------------RULES-------------------------------------------
.PHONY: all clean

all: $(EXEC_FAST) 

$(EXEC_FAST): vm.c
	$(CC) $(CFLAGS) $< -o $@ $(LFLAGS)

clean:
	-rm -fv $(EXEC_FAST)


