CC=ghc
FLAGS=
TARGETS=moduletest test splay range tester
$(addsuffix .out, $(TARGETS)): %.out : %.hs
	$(CC) $(FLAGS) $< -o $@ && rm *.o *.hi
.PHONY:$(TARGETS) clean
$(TARGETS):%:%.out
	./$<
clean:
	rm *.out