MOSMLHOME=/usr
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools

MOSMLC=mosmlc -c -liberal -I ../parser
MOSMLLEX=mosmllex

# Unix
REMOVE=rm -f
MOVE=mv

.SUFFIXES : .sig .sml .ui .uo

all: parser lexer.uo

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) lexer.sml
	$(REMOVE) errlist
	$(REMOVE) *.ui *.uo
	$(REMOVE) *.o

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

lexer.sml:
	$(MOSMLLEX) lexer.lex
parser:
	$(MAKE) -C ../parser