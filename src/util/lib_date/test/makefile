#$Header: /weru/cvs/weps/weps.src/util/date/test/makefile,v 1.1.1.1 1999-03-12 17:05:31 wagner Exp $
#
# Makefile for date test routines

# basename of library routines tested
LIB = date

WEPS_DIR = ../../..					# relative path to WEPS project dir
.INCLUDE : $(WEPS_DIR)/prologue.mk	# include project master prologue file
#-------------------------------------------------------------------------

TST = $(LIB)
TST_SRC = tst$(TST).for tst$(TST)2.for tst$(TST)3.for
TST_OBJ = ${TST_SRC:.for=$(O)}

all .PHONY : $(LIB_NAME) tst

$(LIB_NAME) .PHONY :
	+cd ..; $(MAKE)

tst .PHONY : ${TST_SRC:.for=$(E)}

tst$(TST)$E:

tst$(TST)2$E:

tst$(TST)3$E:

# SRC file include dependencies (can't deal with imbedded include files)

#-------------------------------------------------------------------------
.INCLUDE : $(WEPS_DIR)/epilogue.mk	# include project master epilogue file
