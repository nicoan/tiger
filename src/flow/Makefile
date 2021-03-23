MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c -liberal -I ../translate -I ../utils

# Unix
REMOVE=rm -f
MOVE=mv

.SUFFIXES : .sig .sml .ui .uo

all: translate utils flow.uo liveness.uo

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) errlist
	$(REMOVE) *.ui *.uo *.o

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

flow.ui: graph.ui
flow.uo: flow.ui graph.uo
liveness.uo: liveness.ui
translate:
	$(MAKE) -C ../translate
utils:
	$(MAKE) -C ../translate