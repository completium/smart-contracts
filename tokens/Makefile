TEZOS_CLIENT=~/liquidity/tezos/tezos-client
LIQUIDITY=liquidity

SRC=erc20.liq
OUT=$(SRC).tz

$(OUT):
	$(LIQUIDITY) $(SRC)

typecheck: $(OUT)
	$(TEZOS_CLIENT) typecheck script $(OUT)

all: $(OUT) typecheck

erc721: erc721.liq
	$(LIQUIDITY) erc721.liq
	$(TEZOS_CLIENT) typecheck script erc721.liq.tz

clean:
	rm -fr *.tz
	rm -fr *~ \#*
