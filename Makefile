CFLAGS += -std=gnu11 -m32 -g3

run: runtime/main.c runtime/io.c runtime/error.c runtime/instruct.c runtime/compare.c runtime/prim.c runtime/str.c runtime/jumptable.h runtime/value.h
	$(LINK.c) -I runtime $(filter %.c,$^) -o $@

runtime/jumptable.h: runtime/instruct.h
	sed -rn 's/([[:upper:]]+)/\&\&lbl_\1/;T;p' $< > $@

runtime/instruct.c: runtime/instruct.h
	{ echo 'const char *name_of_instructions[] = {'; sed -rn 's/([[:upper:]]+).*/"\1",/;T;p' $<; echo '};';} > $@

opcode.ml: runtime/instruct.h
	#awk '/[[:upper:]]/{sub(",","");print "let op"$$1"="i++}' $< > $@
	awk '/[[:upper:]]/{sub(",","");print "let op"$$1"="n++;s[n-1]=$$1}END{printf "let name_of_opcodes=[|";for(i=0;i<n;i++)printf "\""s[i]"\";";print"|]"}' $< > $@

cprim.ml: runtime/prim.c
	awk '/cprims/{a=1} /\w+,/&&a==1{sub(",","");s[n++]=$$1} END{printf "let name_of_prims=[|";for(i=0;i<n;i++)printf "\""s[i]"\";";print"|]"}' $< > $@
