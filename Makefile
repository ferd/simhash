.PHONY: all test clean
	PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

all:
	@$(REBAR) compile

test:
	@$(REBAR) compile
	@mkdir -p logs/
	@ct_run -name ct_node -spec ct.spec -pa `pwd`/ebin/ `pwd`/include/ -env ERL_LIBS `pwd`/deps

clean:
	@rm -rf ebin/ logs/
