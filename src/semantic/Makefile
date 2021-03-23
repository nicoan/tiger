MOSMLHOME=/usr
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c -liberal -I ../utils -I ../parser -I ../translate

# Unix
REMOVE=rm -f
MOVE=mv

.SUFFIXES : .sig .sml .ui .uo

all: utils parser translate seman.uo

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) errlist
	$(REMOVE) *.ui *.uo *.o

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

seman.uo: seman.ui env.uo
env.uo: types.uo
utils:
	$(MAKE) -C ../utils
parser:
	$(MAKE) -C ../parser
translate:
	$(MAKE) -C ../translate