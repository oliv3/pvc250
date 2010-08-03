all: utils.beam cities.beam path.beam tsm.beam

#OPTIMS=+hipe

%.beam: %.erl
	erlc $(OPTIMS) $<

clean:
	@rm -f *.beam *~ erl_crash.dump
