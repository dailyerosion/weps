#$Header: /weru/cvs/weps/weps.src/util/math/test/makefile,v 1.1.1.1 1999-03-12 17:05:32 wagner Exp $
#
# Makefile for math test routines

# basename of library routines tested
LIB = math

WEPS_DIR = ../../..					# relative path to WEPS project dir
.INCLUDE : $(WEPS_DIR)/prologue.mk	# include project master prologue file
#-------------------------------------------------------------------------

TST = $(LIB)
TST_SRC = tst$(TST).for
TST_OBJ = ${TST_SRC:.for=$(O)}

all .PHONY : $(LIB_NAME) tst

$(LIB_NAME) .PHONY :
	+cd ..; $(MAKE)

tst .PHONY : ${TST_SRC:.for=$(E)}

tst$(TST)$E:

# SRC file include dependencies (can't deal with imbedded include files)

#-------------------------------------------------------------------------
.INCLUDE : $(WEPS_DIR)/epilogue.mk	# include project master epilogue file
