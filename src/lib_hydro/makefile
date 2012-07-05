#$Header: /weru/cvs/weps/weps.src/hydro/makefile,v 1.1.1.1 1999-03-12 17:05:22 wagner Exp $
#
# Makefile for hydro routines

# basename of library
LIB = hydro

WEPS_DIR = ..                       # relative path to WEPS project dir
.INCLUDE : $(WEPS_DIR)/prologue.mk  # include project master prologue file
#-------------------------------------------------------------------------

LIB_SRC = hydro.for hinit.for darcy.for et.for \
          extra.for psd.for radnet.for scsq.for \
          snomlt.for store.for transp.for heat.for \
          dawn.for waterk.for avrage.for sgmoid.for


all .PHONY : $(LIB_NAME)

# SRC file include dependencies (can't deal with imbedded include files)

darcy$O: $(INC_DIR)/{p1werm.inc p1const.inc p1unconv.inc m1sim.inc m1subr.inc} \
         $(INC_DIR)/{m1flag.inc h1et.inc} \
         $(INC_DIR)/hydro/{htheta.inc hh2o.inc hlayrs.inc}

dawn$O: $(INC_DIR)/{p1const.inc}

et$O: $(INC_DIR)/{w1clig.inc h1et.inc}

extra$O: $(INC_DIR)/{p1werm.inc} $(INC_DIR)/hydro/{htheta.inc hlayrs.inc}

heat$O: $(INC_DIR)/{p1werm.inc p1const.inc p1unconv.inc m1sim.inc m1subr.inc} \
        $(INC_DIR)/{m1flag.inc w1clig.inc} $(INC_DIR)/hydro/{hlayrs.inc}

hinit$O: $(INC_DIR)/{p1werm.inc p1const.inc} \
         $(INC_DIR)/hydro/{htheta.inc hlayrs.inc hpsd.inc htemp.inc}

hydro$O: $(INC_DIR)/{p1werm.inc p1unconv.inc m1sim.inc m1subr.inc} \
         $(INC_DIR)/{m1flag.inc w1clig.inc h1et.inc h1hydro.inc s1layr.inc} \
         $(INC_DIR)/hydro/{hfdecl.inc htheta.inc hh2o.inc hlayrs.inc htemp.inc}

radnet$O: $(INC_DIR)/{p1const.inc}

store$O: $(INC_DIR)/{p1werm.inc p1unconv.inc} \
         $(INC_DIR)/hydro/{htheta.inc hh2o.inc hlayrs.inc}

transp$O: $(INC_DIR)/{p1werm.inc p1unconv.inc h1et.inc} \
          $(INC_DIR)/hydro/{htheta.inc hlayrs.inc}

#-------------------------------------------------------------------------
.INCLUDE : $(WEPS_DIR)/epilogue.mk	# include project master epilogue file
