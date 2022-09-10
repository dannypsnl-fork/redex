SCRBL := raco scribble --htmls +m --redirect-main http://docs.racket-lang.org/

build:
	@$(SCRBL) scribblings/redex.scrbl
