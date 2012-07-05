#$Header: /weru/cvs/weps/weps.src/crop/makefile,v 1.2 1999-04-13 21:58:46 wjr Exp $
#
# Makefile for crop routines

# basename of library
LIB = crop

WEPS_DIR = ..						# relative path to WEPS project dir
.INCLUDE : $(WEPS_DIR)/prologue.mk	# include project master prologue file
#-------------------------------------------------------------------------

LIB_SRC = crop.for cinit.for npcy.for npmin.for \
		growth.for huc1.for sdst.for cinput.for \
		nmnim.for nuse.for temps.for \
		scrv1.for nuts.for nconc.for cpout.for cprnl.for

all .PHONY : $(LIB_NAME)

# SRC file include dependencies (can't deal with imbedded include files)

cinit$O: $(INC_DIR)/crop/{cgrow.inc cenvr.inc cparm.inc csoil.inc} \
		$(INC_DIR)/crop/{chumus.inc cfert.inc p1crop.inc} \
		$(INC_DIR)/{p1werm.inc m1dbug.inc m1flag.inc m1sim.inc c1info.inc c1gen.inc m1subr.inc}

cinput$O:	$(INC_DIR)/{p1werm.inc m1subr.inc m1dbug.inc m1flag.inc} \
		$(INC_DIR)/{c1info.inc c1db1.inc c1db2.inc c1db3.inc}

crop$O:	$(INC_DIR)/crop/{cgrow.inc cenvr.inc cparm.inc csoil.inc} \
		$(INC_DIR)/crop/{chumus.inc cfert.inc p1crop.inc} \
		$(INC_DIR)/{p1werm.inc m1dbug.inc m1flag.inc m1sim.inc w1clig.inc} \
		$(INC_DIR)/{c1gen.inc c1glob.inc c1geom.inc h1hydro.inc h1et.inc} \
		$(INC_DIR)/manage/{harv.inc oper.inc}

growth$O:	$(INC_DIR)/crop/{cgrow.inc cenvr.inc cparm.inc csoil.inc} \
			$(INC_DIR)/crop/{chumus.inc cfert.inc p1crop.inc} \
			$(INC_DIR)/{p1werm.inc m1dbug.inc m1flag.inc m1sim.inc m1subr.inc c1db1.inc} \
			$(INC_DIR)/{w1clig.inc c1glob.inc c1geom.inc c1mass.inc h1hydro.inc}

huc1$O:	$(INC_DIR)/crop/{cgrow.inc cenvr.inc} $(INC_DIR)/{p1werm.inc p1const.inc}

npcy$O: $(INC_DIR)/crop/{cgrow.inc cenvr.inc cparm.inc csoil.inc chumus.inc cfert.inc}

nuse$O: $(INC_DIR)/crop/{cgrow.inc cenvr.inc cparm.inc csoil.inc chumus.inc} \
		$(INC_DIR)/crop/{cfert.inc p1crop.inc}

npmin$O: $(INC_DIR)/crop/{cenvr.inc csoil.inc chumus.inc cfert.inc}

nmmin$O: $(INC_DIR)/crop/{cenvr.inc csoil.inc chumus.inc cfert.inc}

temps$O: $(INC_DIR)/crop/{cenvr.inc cparm.inc}

nuts$O: $(INC_DIR)/crop/{cparm.inc}

cprnl$O: $(INC_DIR)/crop/{cgrow.inc cenvr.inc} \
         $(INC_DIR)/manage/{harv.inc oper.inc} \
         $(INC_DIR)/{m1dbug.inc}

cpout$O: $(INC_DIR)/{m1dbug.inc m1flag.inc} \

#-------------------------------------------------------------------------
.INCLUDE : $(WEPS_DIR)/epilogue.mk	# include project master epilogue file
