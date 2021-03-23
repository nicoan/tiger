MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLLEX=mosmllex
MOSMLC=mosmlc -c -liberal

# Unix
REMOVE=rm -f
MOVE=mv

.SUFFIXES : .sig .sml .ui .uo

all: stack.ui stack.uo table.ui table.uo topsort.uo aux.uo

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) errlist
	$(REMOVE) *.ui *.uo *.o

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

aux.uo: aux.ui