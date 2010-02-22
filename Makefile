EBIN=./ebin
DOC=./doc

all:
	erl -make

run:
	erl -pa $(EBIN) $(EOPTS) -eval 'application:load(nbh)' -eval 'nbh:start()'

clean:
	rm -f $(EBIN)/*.beam

docs:
	erl -noshell -eval '{ok, Makefiles} = file:consult("Emakefile"), edoc:files(lists:foldl(fun(L, Acc) -> Acc ++ L end, [], lists:map(fun({F, _}) -> filelib:wildcard(atom_to_list(F)) end, Makefiles)), [{dir, "$(DOC)"}]), halt().'

cleandocs:
	rm -rf $(DOC)/*
	touch $(DOC)/.dummy

.PHONY: all run clean docs cleandocs
