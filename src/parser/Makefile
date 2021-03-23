MOSMLHOME=/usr
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLYACC=mosmlyac -v
MOSMLC=mosmlc -c -liberal -I ../utils

# Unix
REMOVE=rm -f
MOVE=mv

.SUFFIXES : .sig .sml .ui .uo

all: utils parser.uo escape.uo prettyprinter.uo

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

linenumber.sml:
	$(MOSMLC) linenumber.sml
parser.sml parser.sig: parser.y
	$(MOSMLYACC) parser.y
parser.ui: ast.uo
parser.uo: parser.ui linenumber.uo ast.uo
escape.uo: escape.ui
utils:
	$(MAKE) -C ../utils
