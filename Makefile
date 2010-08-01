all: pvc_utils.beam

#OPTIMS=+hipe

%.beam: %.erl
	erlc $(OPTIMS) $<

clean:
	@rm -f *.beam *~ erl_crash.dump
