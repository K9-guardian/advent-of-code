LIB := ./lib
SHARE := ./share
FLAGS := -Wno-unused-result -shared

all: $(SHARE)/hash_helper.so

$(SHARE)/hash_helper.so: $(LIB)/hash_helper.c
	swipl-ld $(FLAGS) -o $(SHARE)/hash_helper $(LIB)/hash_helper.c
