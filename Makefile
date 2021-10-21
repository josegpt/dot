IGNOREFOLDERS='(var|etc)'

all: home var etc

clean: uhome uvar uetc

var:
	doas stow -t / var

etc:
	doas stow -t / etc

home:
	stow --ignore=$(IGNOREFOLDERS) .

uvar:
	doas stow -t / -D var

uetc:
	doas stow -t / -D etc

uhome:
	stow --ignore=$(IGNOREFOLDERS) -D .

.PHONY: home uhome var uvar etc uetc all clean
