# This is simple Makefile to be used in all our OCaml projects
# which are actually built with OCamlBuild.
#
# Started by Jean-Vincent Loddo and Luca Saiu in 2008. 
# We hereby place this makefile into the public domain
#   --Jean-Vincent Loddo, Luca Saiu.
#
# This is the revision of 2008-03-13.


######################################################################
# Implementation of targets. Note that the user is *not* supposed to
# override these, but only to define the project-dependant '-local'
# versions:

# The main target. Its implementation is entirely project-dependant:
main: main-local

# 'all' is just an alias for 'main':
all: main

# In some projects we may need to build something more than 'main',
# but we do nothing more by default:
world: world-local main

# Generate ocamldoc documentation:
ocamldoc: main ocamldoc-local
	#ocamlbuild -I +lablgtk2 ocamlbricks.docdir/index.html
	eval `grep '^name=' META`; \
	if test -z "$$prefix"; then \
		echo 'prefix is undefined in CONFIGME.'; \
		exit 1; \
	fi; \
	@echo 'I have found absolutely no way to make ocamlbuild work with ocamldoc. --L.'
	exit 1

# Install programs and libraries:
install: install-programs install-libraries install-local

# The user is free to override this to add custom targets to install into the
# $prefix/bin installation directory:
OTHER_INSTALL_TARGETS =

# Install the programs from this package into $prefix/bin:
install-programs: main install-programs-local
	@(if ! source CONFIGME &> /dev/null; then \
		echo 'Evaluating CONFIGME failed.'; \
		exit -1; \
	fi; \
	eval `grep '^name=' META`; \
	if test -z "$$prefix"; then \
		echo 'prefix is undefined in CONFIGME.'; \
		exit 1; \
	fi; \
	if test -z "$$name"; then \
		echo 'name is undefined in CONFIGME.'; \
		exit 1; \
	fi; \
	echo "Creating $$prefix/bin/..."; \
	(mkdir -p $$prefix/bin &> /dev/null || true); \
	echo "Installing programs from $$name into $$prefix/bin/..."; \
	shopt -s nullglob; \
	for file in $(OTHER_INSTALL_TARGETS) _build/*.byte _build/*.native; do \
	    echo 'Installing '`basename $$file`'...'; \
	    cp -a $$file $$prefix/bin; \
	done) && \
	echo 'Program installation was successful.'

# Remove the programs from this package from $prefix/bin:
uninstall-programs: main uninstall-programs-local
	@(if ! source CONFIGME &> /dev/null; then \
		echo 'Evaluating CONFIGME failed.'; \
		exit -1; \
	fi; \
	eval `grep '^name=' META`; \
	if test -z "$$prefix"; then \
		echo 'prefix is undefined in CONFIGME.'; \
		exit 1; \
	fi; \
	if test -z "$$name"; then \
		echo 'name is undefined in CONFIGME.'; \
		exit 1; \
	fi; \
	echo "Removing $$name programs from $$prefix/bin/..."; \
	shopt -s nullglob; \
	for file in $(OTHER_INSTALL_TARGETS) _build/*.byte _build/*.native; do \
	    export pathname=$$prefix/bin/`basename $$file`; \
	    echo "Removing $$pathname..."; \
	    rm -f $$pathname; \
	done) && \
	echo 'Program uninstallation was successful.'

# The user is free to override this to add custom targets to install into the
# library installation directory:
OTHER_LIBRARY_INSTALL_TARGETS =

# Install the library in this package into the path chosen at configuration time:
install-libraries: main install-libraries-local
	@(if ! source CONFIGME &> /dev/null; then \
		echo 'Evaluating CONFIGME failed.'; \
		exit -1; \
	fi; \
	eval `grep '^name=' META`; \
	if test -z "$$libraryprefix"; then \
		echo 'libraryprefix is undefined in CONFIGME.'; \
		exit 1; \
	fi; \
	if test -z "$$name"; then \
		echo 'name is undefined in CONFIGME.'; \
		exit 1; \
	fi; \
	echo "Installing libraries from $$name into $$libraryprefix/$$name/..."; \
	(mkdir $$libraryprefix/$$name &> /dev/null || true); \
	cp -f META $(OTHER_LIBRARY_INSTALL_TARGETS) \
	      _build/*.cma _build/*.cmxa _build/*.a \
	    $$libraryprefix/$$name/) && \
	echo 'Library installation was successful.'

# Uninstall programs and libraries:
uninstall: uninstall-programs uninstall-libraries uninstall-local

# Remove the library from the installation path chosen at configuration time:
uninstall-libraries: main uninstall-libraries-local
	@(if ! source CONFIGME &> /dev/null; then \
		echo 'Evaluating CONFIGME failed.'; \
		exit -1; \
	fi; \
	eval `grep '^name=' META`; \
	if test -z "$$libraryprefix"; then \
		echo 'libraryprefix is undefined in CONFIGME.'; \
		exit 1; \
	fi; \
	if test -z "$$name"; then echo 'name is undefined in META.'; exit 1; fi; \
	echo "Uninstalling $$name from $$libraryprefix/..."; \
	echo rm -rf $$libraryprefix/$$name/) && \
	echo 'Uninstallation was successful.'

# Make a source tarball:
dist: clean dist-local
	@(eval `grep '^name=' META`; \
	eval `grep '^version=' META`; \
	if test -z "$$name"; then echo 'name is undefined in META.'; exit 1; fi; \
	if test -z "$$version"; then echo 'version is undefined in META.'; exit 1; fi; \
	echo "Making the source tarball _build/$$name-$$version.tar.gz..."; \
	mkdir -p _build/$$name-$$version; \
	cp -af * _build/$$name-$$version/ &> /dev/null; \
	(tar --exclude=_build -C _build -czf \
	     _build/$$name-$$version.tar.gz $$name-$$version/ && \
	rm -rf _build/$$name-$$version)) && \
	echo "Success."

# Make a binary tarball:
dist-binary: dist-binary-local
	echo 'To do: implement the dist-binary target.'

# Remove generated stuff:
clean: clean-local
	rm -rf _build
	find -type f -name \*~ -exec rm -f {} \; 
	find -type f -name \#\*\# -exec rm -f {} \; 
	find -type f -name core -exec rm -f {} \; 

# Meta-help about the targets defined in this make file:
targets:
	@cat Makefile Makefile.local | grep -B 1 "^[a-z0-9_-]*[:]" | \
	  awk '/BEGIN/ {r=""} /^[#]/ { r=substr($$0,2); next; } /^[a-z0-9_-]*[-]local[:]/ {r=""; next} /^[a-z0-9_-]*[:]/{split($$0,a,/:/); printf("%s\r\t\t\t--- %s\n",a[1],r); r=""; next} {r=""}' | sort


######################################################################
# Default implementation for '-local' targets:

# All the user-definable '-local' targets have an empty implementation
# by default:
main-local:
world-local:
ocamldoc-local:
install-local:
uninstall-local:
install-programs-local:
uninstall-programs-local:
install-libraries-local:
uninstall-libraries-local:
dist-local:
dist-binary-local:
clean-local:

# Let's avoid confusion between all and main: they're the same thing
# for us, and we only support main-local:
all-local:
	echo 'all-local does not exist. Use main-local instead'
	exit 1


#####################################################################
# Default compilation flags. The user *is* expected to override or
# extend these:
OCAMLBUILD_BYTECODE_CFLAGS =
OCAMLBUILD_NATIVE_CFLAGS =


#####################################################################
# Default rules:

# Bytecode libraries:
%.cma:
	ocamlbuild $(OCAMLBUILD_BYTECODE_CFLAGS) $@

# Native libraries:
%.cmxa:
	ocamlbuild $(OCAMLBUILD_NATIVE_CFLAGS) $@

#####################################################################
# Include the project-dependant file which implements the '-local'
# targets:
include Makefile.local
