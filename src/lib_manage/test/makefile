#$Header: /weru/cvs/weps/weps.src/manage/test/makefile,v 1.1.1.1 1999-03-12 17:05:28 wagner Exp $
#
# Makefile for manage test routines

# basename of library routines tested
LIB = wman

WEPS_DIR = ../..					# relative path to WEPS project dir
.INCLUDE : $(WEPS_DIR)/prologue.mk	# include project master prologue file
#-------------------------------------------------------------------------

# additional libs required to link in
.IF $(OS) == DOS
LDLIBS += asd$(MODEL) date$(MODEL) math$(MODEL)
		   -decomp$(MODEL) -crop$(MODEL)
.ELSE
LDLIBS += -lasd$(MODEL) -ldate$(MODEL) -lmath$(MODEL) \
		   -ldecomp$(MODEL) -lcrop$(MODEL)
.END

TST = $(LIB)
TST_SRC = tst$(TST).for
TST_OBJ = ${TST_SRC:.for=$(O)}

all .PHONY : $(LIB_NAME) tst

$(LIB_NAME) .PHONY :
	+cd ..; $(MAKE)
	+cd ../../util/math; $(MAKE)
	+cd ../../util/date; $(MAKE)
	+cd ../../asd; $(MAKE)
	+cd ../../main; $(MAKE)

tst .PHONY : ${TST_SRC:.for=$(E)}

tst$(TST)$E:

# SRC file include dependencies (can't deal with imbedded include files)

tstwman$O: $(INC_DIR)/{p1werm.inc m1subr.inc s1layr.inc s1agg.inc} \
			$(INC_DIR)/manage/{man.inc asd.inc}

#-------------------------------------------------------------------------
.INCLUDE : $(WEPS_DIR)/epilogue.mk	# include project master epilogue file
