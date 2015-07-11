TARGET=babel-repl
ifdef LIB
	OPT=-L $(LIB)
else
	OPT=-L $(HOME)/.emacs.d/site-lisp
endif

all: $(TARGET).elc

clean:
	@rm -f $(TARGET).elc $(TARGET).el~

.el.elc:
	emacs $(OPT) -batch -f batch-byte-compile $<
