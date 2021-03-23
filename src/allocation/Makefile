MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c -liberal -I ../flow -I ../translate -I ../utils

# Unix
REMOVE=rm -f
MOVE=mv

.SUFFIXES : .sig .sml .ui .uo

all: flow translate utils coloring.uo regalloc.uo

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) parser.output
	$(REMOVE) parser.sig
	$(REMOVE) parser.sml
	$(REMOVE) errlist
	$(REMOVE) *.ui *.uo

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

coloring.uo: coloring.ui
regalloc.uo: regalloc.ui
flow:
	$(MAKE) -C ../flow
translate:
	$(MAKE) -C ../translate
utils:
	$(MAKE) -C ../utils
