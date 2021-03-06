# -*-makefile-*-

PY_MODULES= afm.py tfm.py
FILES= mftrace.py GNUmakefile.in \
	gf2pbm.c mftrace.1 \
	COPYING \
	README.texi README.txt ChangeLog \
	mftrace.spec.in mftrace.spec \
	$(PY_MODULES) configure configure.in config.h.in
CFLAGS = -Wall -O2

## retain during override.
CFLAGS += -I.
srcdir=.

NAME=mftrace
MANPAGES=mftrace.1
VERSION=1.2.18
distdir=$(NAME)-$(VERSION)
prefix=/usr/local
datadir=${prefix}/share/mftrace/
exec_prefix=${prefix}
bindir=${exec_prefix}/bin
mandir=${prefix}/share/man
PYTHON=/usr/bin/python
EXEEXT=


## up/download.
FTP_DIR=hanwen@lilypond.org:www/download/sources/mftrace/
WWW_DIR=hanwen@lilypond.org:www/mftrace/
GIT_MASTER_SITE=git+ssh://git@github.com/hanwen/mftrace.git

all: compile

compile: mftrace gf2pbm

version.texi: GNUmakefile
	echo -e '@macro mftversion\n$(VERSION)\n@end macro\n' > $@

README.txt: README.texi version.texi
	makeinfo --no-split --no-headers --output $@  $<

README.html: README.texi version.texi
	makeinfo --html --no-split --no-headers --output $@  $<

dist: $(FILES)
	mkdir $(distdir)
	ln $(FILES) $(distdir)
	tar --owner=0 --group=0  -zcf $(distdir).tar.gz $(distdir)
	rm -rf $(distdir)

mftrace.spec: mftrace.spec.in configure
	./config.status

mftrace: mftrace.py configure
	./config.status

configure: configure.in
	autoconf
	./configure

GNUmakefile: GNUmakefile.in configure
	./config.status
	chmod a-w GNUmakefile

install: mftrace gf2pbm install-man install-py
	install -d $(DESTDIR)$(bindir) 
	install mftrace $(DESTDIR)$(bindir)
	install gf2pbm$(EXEEXT) $(DESTDIR)$(bindir)/gf2pbm$(EXEEXT)


install-py: $(PY_MODULES)
	install -d $(DESTDIR)$(datadir)
	install -m644 $^ $(DESTDIR)$(datadir) 
	$(PYTHON) -m compileall $(DESTDIR)$(datadir)

install-man: $(MANPAGES)
	install -d $(DESTDIR)$(mandir)/man1
	install -m644 $^ $(DESTDIR)$(mandir)/man1/

uninstall:
	rm $(DESTDIR)$(bindir)/mftrace $(DESTDIR)$(bindir)/gf2pbm$(EXEEXT)
	cd $(DESTDIR)$(datadir) && rm $(PY_MODULES)
	cd $(DESTDIR)$(mandir)/man1 && rm $(MANPAGES)
	-rmdir $(DESTDIR)$(bindir)
	-rmdir $(DESTDIR)$(datadir)
	-rmdir $(DESTDIR)$(mandir)/man1 
	-rmdir $(DESTDIR)$(mandir)/ 
	-rmdir $(DESTDIR)$(prefix)
	-rmdir $(DESTDIR)$(exec_prefix)

clean:
	rm -f config.cache config.h config.log config.status GNUmakefile
	rm -f mftrace gf2pbm
	rm -f *'~' '#'*


TEST_FONTS= ecbi0900 logo10
test:
	$(foreach a, $(TEST_FONTS), $(PYTHON) mftrace.py --glyphs 65 -V $(a) &&)true


udist:  README.html
	autoconf
	./configure --prefix=$(HOME)/usr/pkg/mftrace
	make dist
	@$(if $(shell git diff), echo "commit before disting" && false,true)
	if test -d .git ; then \
		git tag -m ''  release/$(VERSION) ; \
		git push $(GIT_MASTER_SITE) tag release/$(VERSION) ; \
	fi
	scp $(distdir).tar.gz $(FTP_DIR)
	scp $< $(WWW_DIR)/index.html

rpm: dist
	rpmbuild -tb mftrace-$(VERSION).tar.gz
