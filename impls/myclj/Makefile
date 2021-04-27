
#SOURCES_UTIL = src/mal/readline.clj
#SOURCES_BASE = $(SOURCES_UTIL) src/mal/reader.cljc src/mal/printer.cljc
#SOURCES_LISP = src/mal/env.cljc src/mal/core.cljc src/mal/stepA_mal.cljc
#SRCS = $(SOURCES_BASE) src/mal/env.cljc src/mal/core.cljc
#SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

DEPS = deps

dist: mal.jar

deps:
	lein deps

mal.jar: $(SOURCES)
	lein with-profile stepA uberjar
	cp target/stepA_mal.jar $@

SHELL := bash
mal: mal.jar
	cat <(echo -e '#!/bin/sh\nexec java -jar "$$0" "$$@"') mal.jar > $@
	chmod +x mal

src/mal/%.clj: $(DEPS)
	@true

target/%.jar: src/mal/%.clj $(SRCS)
	lein with-profile $(word 1,$(subst _, ,$*)) uberjar

clean:
	rm -rf target/ mal.jar mal
