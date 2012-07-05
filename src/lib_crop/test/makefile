#$Header: /weru/cvs/weps/weps.src/crop/test/makefile,v 1.1.1.1 1999-03-12 17:05:19 wagner Exp $
#
# Makefile for crop test routines

# basename of library routines tested
LIB = crop

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
#	+cd ../../crop; $(MAKE)
	+cd ../../main; $(MAKE) lib

tst .PHONY : ${TST_SRC:.for=$(E)}

tst$(TST)$E:

# SRC file include dependencies (can't deal with imbedded include files)

tstcrop$O: $(INC_DIR)/{p1werm.inc p1const.inc p1unconv.inc } \
	$(INC_DIR)/{m1sim.inc m1flag.inc m1subr.inc m1dbug.inc} \
	$(INC_DIR)/{c1info.inc c1db1.inc c1db2.inc c1db3.inc w1clig.inc} \
	$(INC_DIR)/crop/{csoil.inc chumus.inc cfert.inc cgrow.inc cenvr.inc} \
	$(INC_DIR)/crop/{cparm.inc p1crop.inc}

#-------------------------------------------------------------------------
.INCLUDE : $(WEPS_DIR)/epilogue.mk	# include project master epilogue file
