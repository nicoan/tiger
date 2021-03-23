# Unix makefile for main example
MOSMLHOME=/usr
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools

MOSMLC=mosmlc -c -liberal
MOSMLL=mosmlc

GCC=gcc -m32 -c

# Unix
REMOVE=rm -f
MOVE=mv

# Make
all: tigerc

tigerc:
	$(MAKE) -C ./lexer/
	$(MAKE) -C ./parser/
	$(MAKE) -C ./semantic/
	$(MAKE) -C ./translate/
	$(MAKE) -C ./instruction_selection/
	$(MAKE) -C ./flow/
	$(MAKE) -C ./allocation/
	$(MAKE) -C ./utils/
	$(MOSMLL) -o tigerc \
		-I ./lexer \
		-I ./parser \
		-I ./semantic \
		-I ./translate \
		-I ./instruction_selection \
		-I ./flow \
		-I ./allocation \
		-I ./lexer \
		-I ./utils \
		main.sml
	$(GCC) runtime.c

# Cleaning
SUBDIRS=$(shell ls -d */)

clean_subdirs:
	for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir clean; \
	done

clean: clean_subdirs
	$(REMOVE) Makefile.bak
	$(REMOVE) tigerc
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) *.o
	$(REMOVE) tigerc
