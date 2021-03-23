MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c -liberal -I ../translate

# Unix
REMOVE=rm -f
MOVE=mv

.SUFFIXES : .sig .sml .ui .uo

all: translate codegen.uo

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) errlist
	$(REMOVE) *.ui *.uo *.o

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

codegen.uo: codegen.ui
translate:
	$(MAKE) -C ../translate