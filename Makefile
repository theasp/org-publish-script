NAME=org-publish
VERSION=0.1
DESTDIR=

# Common prefix for installation directories.
# NOTE: This directory must exist when you start the install.
prefix = /usr/local
datarootdir = $(prefix)/share
datadir = $(datarootdir)/$(NAME)
exec_prefix = $(prefix)
# Where to put the executable for the command `gcc'.
bindir = $(exec_prefix)/bin
# Where to put the directories used by the compiler.
libexecdir = $(exec_prefix)/libexec
# Where to put the Info files.
infodir = $(datarootdir)/info

INSTALL_BIN=install
INSTALL_DATA=install -m 0644

ORG_PUBLISH=org-publish-script-head.el org-publish.el org-publish-script.el

DESTS=org-publish

.PHONY: all
all: $(DESTS)

.PHONY: install
install: $(DESTS)
	install -d $(DESTDIR)$(bindir)
	$(INSTALL_BIN) org-publish $(DESTDIR)$(bindir)

org-publish: $(ORG_PUBLISH)
	cat $^ > $@
	chmod +x $@

.PHONY: test
test: all
	./tests.sh

.PHONY: clean
clean:
	$(RM) -f $(DESTS)
	$(RM) -f *.tar.gz

.PHONY: tar
tar: $(NAME)-$(VERSION).tar.gz

$(NAME)-$(VERSION).tar.gz: clean
	tar cvfz $@ --transform "s!^!$(NAME)-$(VERSION)/!" --show-transformed --exclude-vcs --exclude-backup -- *
