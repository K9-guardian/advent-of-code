LIB := ./lib
SHARE := ./share
FLAGS := -Wno-unused-result -shared

all: $(SHARE)/hash_helper

$(SHARE)/hash_helper: $(LIB)/hash_helper.c
	swipl-ld $(FLAGS) -o $(SHARE)/hash_helper $(LIB)/hash_helper.c
