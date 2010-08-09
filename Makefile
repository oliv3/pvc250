#OPTIMS=+hipe

all: utils.beam cities.beam path.beam tsp.beam

run:
	@./go.sh

%.beam: %.erl tsp.hrl
	erlc $(OPTIMS) $<

clean:
	@rm -f *.beam *~ erl_crash.dump
