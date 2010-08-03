#OPTIMS=+hipe

all: utils.beam cities.beam path.beam tsm.beam

run:
	@./go.sh
%.beam: %.erl
	erlc $(OPTIMS) $<

clean:
	@rm -f *.beam *~ erl_crash.dump
