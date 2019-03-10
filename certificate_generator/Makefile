TEZOS_CLIENT=~/tezos/tezos-client
LIQUIDITY=liquidity

SRC=certificate_generator.liq
OUT=$(SRC).tz

$(OUT):
	$(LIQUIDITY) $(SRC)

typecheck: $(OUT)
	$(TEZOS_CLIENT) typecheck script $(OUT)

all: $(OUT) typecheck

clean:
	rm -fr *.tz
	rm -fr *~ \#*
