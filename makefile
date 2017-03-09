CC=ghc
FLAGS=
TARGETS=moduletest test splay range
$(addsuffix .out, $(TARGETS)): %.out : %.hs
	$(CC) $(FLAGS) $< -o $@ && rm *.o *.hi
.PHONY:$(TARGETS) clean
$(TARGETS):%:%.out
	./$<
clean:
	rm *.out