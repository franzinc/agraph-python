# $Id: Makefile,v 1.2 2008/10/11 21:34:15 layer Exp $

localmakefile := $(shell test -f Makefile.local && echo yes)

ifeq ($(localmakefile),yes)
include Makefile.local
endif
include Makefile.defs

default: build

all: clean build

clean: FORCE
	rm -fr aghttpd build.out autoloads.out
	find . -name '*.fasl' -print0 | xargs -0 rm

build: FORCE
	rm -fr aghttpd build.tmp
### not ready for this:
#	echo '(setq excl::*break-on-warnings* t)' >> build.tmp
	echo '(load "load")' >> build.tmp
	echo '(generate-executable "aghttpd"' >> build.tmp
	echo '  (quote ("load.lisp")) ' >> build.tmp
	echo '  :restart-init-function (quote python-http-server-main) ' >> build.tmp
	echo '  :include-compiler nil ' >> build.tmp
	echo '  :runtime :standard)' >> build.tmp
	$(lisp) -d build.out -L build.tmp -kill
	rm -f build.tmp

FORCE:
