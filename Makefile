CFLAGS += -std=c11

run: runtime/main.c runtime/jumptable.h
	$(LINK.c) -I runtime $< -o $@

runtime/jumptable.h: runtime/instruct.h
	sed -rn 's/([[:upper:]]+)/\&\&lbl_\1/;T;p' $< > $@
