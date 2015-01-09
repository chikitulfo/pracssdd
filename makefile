
SRC = src
EBIN = ebin
ERL = erl
GEN = beam
ERLC_EMULATOR = erl -boot start_clean

SOURCE = $(wildcard $(SRC)/*.erl)

TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

CODE = $(SOURCE:%.erl=$(EBIN)/%.beam) 

$(EBIN)/%.beam: %.erl
	erlc  -W -b beam -o $(EBIN) $(EFLAGS) $(WAIT) $<

all: $(TARGETS)
   
clean:
	rm -f $(EBIN)/*.beam erl_crash.dump 

