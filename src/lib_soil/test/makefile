#$Header: /weru/cvs/weps/weps.src/soil/test/makefile,v 1.1.1.1 1999-03-12 17:05:31 wagner Exp $
#
# Makefile for soil test routines

# basename of library routines tested
LIB = soil

WEPS_DIR = ../..			# relative path to WEPS project dir
.INCLUDE : $(WEPS_DIR)/prologue.mk	# include project master prologue file
#-------------------------------------------------------------------------

# additional libs required to link in
.IF $(OS) == DOS
LDLIBS += main$(MDL) date$(MDL) math$(MDL)
.ELSE
LDLIBS += -lmain$(MDL) -ldate$(MDL) -lmath$(MDL)
.END

TST = $(LIB)
TST_SRC = tst$(TST).for
TST_OBJ = ${TST_SRC:.for=$(O)}

all .PHONY : modules tst

modules .PHONY :
	+cd ..; $(MAKE)
	+cd ../../util/math; $(MAKE)
	+cd ../../util/date; $(MAKE)
#	+cd ../../soil; $(MAKE)
	+cd ../../main; $(MAKE) lib

tst .PHONY : ${TST_SRC:.for=$(E)}

tst$(TST)$E:

# SRC file include dependencies (can't deal with imbedded include files)

tstsoil$O: $(INC_DIR)/{p1werm.inc p1const.inc p1unconv.inc } \
	$(INC_DIR)/{m1sim.inc m1flag.inc m1subr.inc m1dbug.inc} \
	$(INC_DIR)/{w1clig.inc}

#-------------------------------------------------------------------------
.INCLUDE : $(WEPS_DIR)/epilogue.mk	# include project master epilogue file
