MOSMLHOME=/usr
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c -liberal -I ../utils -I ../parser

# Unix
REMOVE=rm -f
MOVE=mv

.SUFFIXES : .sig .sml .ui .uo

all: utils parser translate.uo canon.uo

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) errlist
	$(REMOVE) *.ui *.uo *.o

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

translate.uo: tigerit.uo frame.ui frame.uo translate.ui
translate.ui: frame.ui
frame.ui: temp.ui tree.uo assem.uo
assem.uo: temp.ui temp.uo
tigerit.uo: tree.uo
tree.uo: temp.ui
canon.uo: canon.ui
utils:
	$(MAKE) -C ../utils
parser:
	$(MAKE) -C ../parser