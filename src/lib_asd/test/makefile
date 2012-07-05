#$Header: /weru/cvs/weps/weps.src/asd/test/makefile,v 1.1.1.1 1999-03-12 17:05:17 wagner Exp $
#
# Makefile for asd test routines

# basename of library routines tested
LIB = asd

WEPS_DIR = ../..					# relative path to WEPS project dir
.INCLUDE : $(WEPS_DIR)/prologue.mk	# include project master prologue file
#-------------------------------------------------------------------------

.IF $(OS) == DOS
LDLIBS += math$(MODEL)			# additional libs required to link in
.ELSE
LDLIBS += -lmath$(MODEL)			# additional libs required to link in
.END

TST = $(LIB)
TST_SRC = tst$(TST).for tst$(TST)2.for
TST_OBJ = ${TST_SRC:.for=$(O)}

all .PHONY : $(LIB_NAME) tst

$(LIB_NAME) .PHONY :
	+cd ..; $(MAKE)
	+cd ../../util/math; $(MAKE)

tst .PHONY : ${TST_SRC:.for=$(E)}

tst$(TST)$E:

tst$(TST)2$E:

# SRC file include dependencies (can't deal with imbedded include files)

tstasd$O: $(INC_DIR)/{p1werm.inc m1subr.inc s1layr.inc s1agg.inc} \
			$(INC_DIR)/manage/{asd.inc}

tstasd2$O: $(INC_DIR)/{p1werm.inc m1subr.inc s1layr.inc s1agg.inc} \
			$(INC_DIR)/manage/{asd.inc}


#-------------------------------------------------------------------------
.INCLUDE : $(WEPS_DIR)/epilogue.mk	# include project master epilogue file
