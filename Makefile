all: pvc_utils.beam cities.beam path.beam

#OPTIMS=+hipe

%.beam: %.erl
	erlc $(OPTIMS) $<

clean:
	@rm -f *.beam *~ erl_crash.dump
