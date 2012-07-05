!$Author$
!$Date$
!$Revision$
!$HeadURL$

PROGRAM test_crop

use f2kcli            !Use F2k commandline parser
use flib_sax          !Use F95 XML parser
use m_cropxml         !Defines begin_element, end_element and pcdata_chunk

IMPLICIT NONE

! WEPS include files which define global variables used in standalone crop submodel
include 'p1werm.inc'
include 'precision.inc'
include 'command.inc' !Contains "cook_yield" global variable (Yield computation method flag)
include 'file.inc'     !Includes the lui's for various WEPS files opened
include 'w1clig.inc'  !Cligen db variables
include 's1layr.inc'  !nslay(1), aszlyt(1,1), aszlyd(1,1)
include 's1dbc.inc'   !asfcce(1,1),asfcec(1,1),asfom(1,1),asftan(1,1),asmno3(1)
include 's1dbh.inc'   !asfcla(1,1),asfsan(1,1),asfsil(1,1)
include 's1phys.inc'  !asdblk(1,1)
include 'd1glob.inc'  !admbgz(1,1,1)
include 'c1gen.inc'   !acxrow(1)
include 'm1flag.inc'  !am0cif
include 'm1sim.inc'   !amzele,am0jd,amalat
include 'm1dbug.inc'  !am0cdb
include 'h1hydro.inc' !ahfwsf(1) water stress factor, ahrsk(1,1),ahrwc(1,1),ah0cb(1,1),aheaep(1,1)
include 'h1temp.inc'  !ahtsav(1,1),ahtsmx(1,1),ahtsmn(1,1),ahfice(1,1)
include 'c1info.inc'
include 'c1db1.inc'
include 'c1db2.inc'
include 'w1wind.inc'  !awudmx
include 'c1glob.inc'  !acthucum(1),acmst(1),acmrt(1),acxstmrep, 
include 'h1et.inc'    !ahzeta,ahzetp,ahzpta,ahzea,ahzep,ahzptp
include 's1sgeo.inc'  !as0rrk(1),as0rrc(1),aslrr(1)
include 'h1db1.inc'   !ahrwcs(1,1)
include 's1agg.inc'   !aslagm(1,1),as0ags(1,1),aslagn(1,1),aslagx(1,1),aseags(1,1)
include 'crop/prevstate.inc'


! Local variables used for extracting commandline args
CHARACTER(LEN=1024) :: lline
CHARACTER(LEN=1024) :: exe
CHARACTER(LEN=1024) :: argv
INTEGER             :: narg,iarg

INTEGER             :: julday       !Date conversion function

INTEGER             :: id, im, iy, i_jday
INTEGER             :: sd, sm, sy, start_jday
INTEGER             :: ed, em, ey, end_jday
INTEGER             :: Pd, Pm, Py, plant_jday
INTEGER             :: Hd, Hm, Hy, harvest_jday
INTEGER             :: mature_warn_flg

CHARACTER(LEN=1024) :: crop_file
TYPE(xml_t)         :: fxml
INTEGER             :: iostat

CHARACTER(LEN=1024) :: cligen_file
REAL                :: cligen_version

INTEGER             :: idx
LOGICAL             :: growcrop_flg

INTEGER             :: sr

! Initialize variables
! ************************************************************************************************
cligen_file = 'cli_gen.cli'    !Default cligen filename
crop_file = 'carrot.crop'      !Default crop record filename
cook_yield = 1                 !Default to using functional Yield/Residue ratio info

sr = 1

! Set default start and end dates (made it 2 calendar years)
sd = 1; sm = 1; sy = 1
ed = 31; em = 12; ey = 2

! Set default planting and harvest dates (arbitrarily specified)
Pd = 15; Pm = 5; Py = 1
Hd = 20; Hm = 10; Hy = 1

growcrop_flg = .false.
am0cif = .false.         !flag to initialize crop initialization routines (set to true on planting date)
am0cfl = 1               !flag to specify if detailed (submodel) output file should be generated
am0cgf = .false.         !supposed to indicate a growing crop

acxrow(1) = 0.76         !row spacing (m)
ahfwsf(1) = 1.0          !water stress factor

! Number of soil layers
nslay(1) = 1
!Set thickness and depth of first layer (defaulting to a value deeper than roots will reach)
aszlyt(1,1) = 1000.0; aszlyd(1,1) = 1000.0
! Assign values to some soil layer properties
asfcce(1,1) = 0.0; asfcec(1,1) = 0.0; asfom(1,1) = 3.0; asftan(1,1) = 0.0; asftap(1,1) = 0.0; asmno3(1) = 0.0;
asfcla(1,1) = 20.0; admbgz(1,1,1) = 0.0; asdblk(1,1) = 1.0;
asfsan(1,1) = 40.0; asfsil(1,1) = 40.0


ahtsav(1,1) = 20.0; ahtsmx(1,1) = 24.0; ahtsmn(1,1) = 22.0; ahfice(1,1) = 0.0;

amzele = 100.0               !default simulation site elevation (m)
amalat = 38.0
am0cdb = 1                   !set crop debug output flag (default to no output)

awudmx = 10.0                !set max daily wind speed
awudmn = 1.0                 !set min daily wind speed
awadir = 0.0                 !set wind dir
awhrmx = 12.0                !set wind dir

awrrh = 0.0                  !Relative Humidity?

acthucum(1) = 0.0            !initialize accumulated heat units
acmst(1) = 0.0               !initialize total standing crop mass
acmrt(1) = 0.0               !initialize total root crop mass
acxstmrep = 0.0              !initialize repesentative stem dia.

ahzeta = 0.0                 !initialize actual evapotranspiration
ahzetp = 0.0                 !initialize potential evapotranspiration
ahzpta = 0.0                 !initialize actual plant transpiration
ahzea = 0.0                  !initialize bare soil evaporation
ahzep = 0.0                  !initialize potential bare soil evaporation
ahzptp = 0.0                 !initialize potential plant transpiration

as0rrk(1) = 0.0; aslrrc(1) = 0.0; aslrr(1) = 0.0  !initialize Random Roughness parms
ahrsk(1,1) = 0.0             !saturated soil hydraulic conductivity
ahrwc(1,1) = 0.0             !soil water content
ah0cb(1,1) = 0.0             !
aheaep(1,1) = 0.0            !

!soil layer water content stuff
ahrwcs(1,1) = 0.0; ahrwca(1,1) = 0.0; ahrwcf(1,1) = 0.0; ahrwcw(1,1) = 0.0
!soil layer aggregate size distribution stuff
aslagm(1,1) = 0.0; as0ags(1,1) = 0.0; aslagn(1,1) = 0.0; aslagx(1,1) = 0.0; aseags(1,1) = 0.0


! initialize math precision global variables
! the factor here is due to the implementation of the EXP function
! apparently, the limit is not the real number limit, but something else
! this works in Lahey, but I cannot attest to it's portability
max_real = huge(1.0) * 0.999150
max_arg_exp = log(max_real)



narg = COMMAND_ARGUMENT_COUNT(); call GET_COMMAND(lline); call GET_COMMAND_ARGUMENT(0,exe)
!print *, "Arg count=", narg
!print *, "Line=",trim(lline)
!print *, "Program=",trim(exe)

do iarg = 1,narg
  call GET_COMMAND_ARGUMENT(iarg,argv)
  !print *, "Argument ",iarg,"=",trim(argv)

  if ( (argv(2:2).eq.'?') .or. (argv(2:2).eq.'h') ) then
     write(*,*) 'Valid command line options:'
     write(*,*)
     write(*,*) '-?  Display this help screen'
     write(*,*) '-h  Display this help screen'
     write(*,*)
     write(*,*) '-Pddmmyy   Plant date DD/MM/YY'
     write(*,*) '           Specify -P020501 for planting on day 2 month 5 year 1'
     write(*,*) '           Day and month must be 2 digits, Year can be 1 to 4 digits'
     write(*,*)
     write(*,*) '-Hddmmyy   Harvest date DD/MM/YY'
     write(*,*) '           Specify -H020901 to harvest on day 2 month 9 year 1'
     write(*,*) '           Day and month must be 2 digits, Year can be 1 to 4 digits'
     write(*,*)
     write(*,*) '-Y#        Method to determine yield'
     write(*,*) '           0 - Use functional Yield/Residue ratio'
     write(*,*) '           1 - Use full mass partitioning relationships (default)'
     write(*,*)
     write(*,*) '-R#        Set row spacing (m)'
     write(*,*) '           0.0 - broadcast seeding (no row spacing)'
     write(*,*) '           0.0 < # - plant or transplant row spacing (default is 0.76m)'
     write(*,*)
     write(*,*) '-W#        Set daily water stress factor (same stress value every day)'
     write(*,*) '           0.0 - full stress'
     write(*,*) '           0.0 < # < 1.0 - partial stress'
     write(*,*) '           1.0 - no stress (default)'
     write(*,*)
     write(*,*) '-O#        Set output level'
     write(*,*) '           0 - no output files'
     write(*,*) '           1 - crop.out, inpt.out & season.out (default)'
     write(*,*) '           2 - allcrop.prn (not currently used)'
     write(*,*)
     write(*,*) '-D#        Set debug output level'
     write(*,*) '           0 - no debug output'
     write(*,*) '           1 - cdbug.out (default)'
     write(*,*)
     write(*,*) '-M#        Set Maturity Warning output level'
     write(*,*) '           0 - no Maturity Warning output'
     write(*,*) '           1 - Warns Harvest before Maturity (default)'
     write(*,*)
     write(*,*) '-ccrop_file     Specify XML format crop record file'
     write(*,*) '-wcligen_file   Specify cligen output format weather file'
     write(*,*)
     call exit(1)

   else if(argv(2:2) .eq. 'P') then !Determine Julian planting date from DD/MM/YY
     read(argv(3:4),*) Pd
     read(argv(5:6),*) Pm
     read(argv(7:),*) Py

   else if(argv(2:2) .eq. 'H') then !Determine Julian harvest date from DD/MM/YY
     read(argv(3:4),*) Hd
     read(argv(5:6),*) Hm
     read(argv(7:),*) Hy

   else if(argv(2:2) .eq. 'Y') then !Specify the method to compute Yield
     read(argv(3:),*) cook_yield

   else if(argv(2:2) .eq. 'R') then !Specify row spacing (m)
     read(argv(3:),*) acxrow(1)

   else if(argv(2:2) .eq. 'W') then !daily water stress factor (one should modify and read daily values from a file)
     read(argv(3:),*) ahfwsf(1)

   else if(argv(2:2) .eq. 'O') then !Specify crop submodel output flag
     read(argv(3:),*) am0cfl

   else if(argv(2:2) .eq. 'D') then !Specify crop submodel debug output flag
     read(argv(3:),*) am0cdb

   else if(argv(2:2) .eq. 'M') then !Specify crop submodel debug output flag
     read(argv(3:),*) mature_warn_flg

   else if(argv(2:2) .eq. 'c') then !Obtain crop record path/filename
     crop_file = argv(3:)
     !print *, 'Crop record path/filename: ',trim(crop_file)

   else if(argv(2:2) .eq. 'w') then !Obtain cligen weather path/filename
     cligen_file = argv(3:)
     !print *, 'Cligen weather path/filename: ',trim(cligen_file)

   else
     write(*,*) 'Unknown option: ', trim(argv)
     call exit(1)
  end if

end do

start_jday = julday(sd,sm,sy); end_jday = julday(ed,em,ey)

plant_jday = julday(Pd,Pm,Py); harvest_jday = julday(Hd,Hm,Hy)
  !print *, 'Plant date is: ',Pd,'/',Pm,'/',Py,'  plant_jday: ',plant_jday
  !print *, 'Harvest date is: ',Hd,'/',Hm,'/',Hy,'  harvest_jday: ',harvest_jday


! Should check that we have a crop record file and cligen file present

call open_xmlfile(trim(crop_file),fxml,iostat)
if (iostat /= 0) stop "Cannot open XML crop record file"

call xml_parse(fxml, &
             begin_element_handler = begin_element_handler, &
             end_element_handler = end_element_handler, &
             pcdata_chunk_handler = pcdata_chunk_handler, &
             verbose = .false.)


call fopenk (luicli, cligen_file, 'old')    !Open cligen file and read in header info

! read(luicli,fmt="(a)",err=90) lline        !1st line: Get version number
! read(lline,*) cligen_version; write(*,*) 'Cligen Version: ', cligen_version

call test_crop_cliginit()                   !Reads header info and sets "monthly" and "yearly" variables
! write(*,*) 'Monthly average daily max temp'
! write(*,*) (awtmxav(idx), idx = 1,12)     !Monthly average daily maximum temperature (deg C)
! write(*,*) 'Monthly average daily min temp'
! write(*,*) (awtmnav(idx), idx = 1,12)     !Monthly average daily minimum temperature (deg C)
! write(*,*) 'Monthly average daily temp'
! write(*,*) (awtmav(idx), idx = 1,12)      !Monthly average daily avearge temperature (deg C)
! write(*,*) 'Yearly average daily temp'
! write(*,*) awtyav                         !Yearly average temperature (deg C)
! write(*,*) 'Monthly average precip'
! write(*,*) (awzmpt(idx), idx = 1,12)      !Monthly average precipitation (mm)


  if (am0cfl .gt. 0) then 
     call fopenk (luocrop, 'crop.out', 'unknown')    !daily crop output of most state variables
     call fopenk (luoseason, 'season.out', 'unknown')  !seasonal summaries of yield and biomass
     call fopenk (luoinpt, 'inpt.out', 'unknown')    !echo crop input data - AR
     call fopenk (luoshoot, 'shoot.out', 'unknown')   !shoot growth data
     call cpout                                 !print headings for crop output files
  endif

  if (am0cfl .gt. 1) then
     call fopenk (luoallcrop, 'allcrop.prn', 'unknown') !main crop debug file (Doesn't appear to be used right now)
  endif
  if (am0cdb .gt. 0) then
     open (unit = 27, file = 'cdbug.out')       !crop submodel debug output file
  endif

do am0jd = start_jday, end_jday                 !Currently must start on Jan. 1 and end on Dec. 31

   
   call caldat(am0jd,id,im,iy)
   call test_crop_getcli(id,im,iy)              !This reads in one year of data into an array on Jan 1 of each year

   if (am0jd == plant_jday) then
      write(*,*) 'This is our planting date'
      write(*,*) 'Date: ',Pd,'/',Pm,'/',Py
      growcrop_flg = .true.
      am0cif = .true.
      am0cgf = .true.
   end if

   if (am0jd == harvest_jday) then
      write(*,*) 'This is our harvest date'
      write(*,*) 'Date: ',Hd,'/',Hm,'/',Hy
      am0cgf = .false.
      call crop_endseason( ac0nam(sr), am0cfl,&
          nslay(sr), ac0idc(sr), acdayam(sr), &
          acthum(sr), acxstmrep(sr),&
          prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr),&
          prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),&
          prevbgstemz(1,sr), &
          prevrootstorez(1,sr), prevrootfiberz(1,sr),&
          prevht(sr), prevstm(sr), prevrtd(sr),&
          prevdayap(sr), prevhucum(sr), prevrthucum(sr),&
          prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),&
          acdayspring(sr), mature_warn_flg )
   end if

   if (growcrop_flg .eqv. .true.) then
      !write(*,*) 'Today is: ',id,'/',im,'/',iy
      call callcrop(am0jd-plant_jday+1,1)       ! (am0jd-plant_jday+1) = current day of crop growth
      if (am0jd == harvest_jday) then
         growcrop_flg = .false.
      end if
!      call callcrop(am0jd-plant_jday+1,1)
   end if


!write(*,*) 'After: ', i_jday

end do


goto 9999

90 write(*,*) 'cligen_file: ', cligen_file, 'line: ', lline

9999 STOP

END PROGRAM test_crop


! copy of WEPS cropupdate() routine containing only required
! code for standalone test_crop program.

subroutine cropupdate(                                         &
   bcmstandstem, bcmstandleaf, bcmstandstore,                  &
   bcmflatstem, bcmflatleaf, bcmflatstore,                     &
   bcmshoot, bcmbgstemz,                                       &
   bcmrootstorez, bcmrootfiberz,                               &
   bczht, bcdstm, bczrtd,                                      &
   bcthucum, bczgrowpt, bcmbgstem,                             &
   bcmrootstore, bcmrootfiber, bcxstmrep,                      &
   bcm, bcmst, bcmf, bcmrt, bcmrtz,                            &
   bcrcd, bszrgh, bsxrgs, bsargo,                              &
   bcrsai, bcrlai, bcrsaz, bcrlaz,                             &
   bcffcv, bcfscv, bcftcv, bcfcancov,                          &
   bc0rg, bcxrow,                                              &
   bnslay, bc0ssa, bc0ssb, bc0sla,                             &
   bcovfact, bc0ck, bcxstm, bcdpop )

   include 'p1const.inc'
   include 'p1werm.inc'

   real :: bcmstandstem, bcmstandleaf, bcmstandstore
   real :: bcmflatstem, bcmflatleaf, bcmflatstore
   real :: bcmshoot, bcmbgstemz(mnsz)
   real :: bcmrootstorez(mnsz), bcmrootfiberz(mnsz)
   real :: bczht, bcdstm, bczrtd
   real :: bcthucum, bczgrowpt
   real :: bszrgh, bsxrgs, bsargo
   integer :: bc0rg
   real :: bcxrow
   real :: bcmbgstem, bcmrootstore, bcmrootfiber, bcxstmrep
   real :: bcm, bcmst, bcmf, bcmrt, bcmrtz(mnsz)
   real :: bcrcd
   real :: bcrsai, bcrlai, bcrsaz(mncz), bcrlaz(mncz)
   real :: bcffcv, bcfscv, bcftcv, bcfcancov
   integer :: bnslay
   real :: bc0ssa, bc0ssb, bc0sla
   real :: bcovfact, bc0ck, bcxstm, bcdpop

      ! calculate crop stem area index
      ! when exponent is not 1, must use mass for single plant stem to get stem area
      ! bcmstandstem, convert (kg/m^2) / (plants/m^2) = kg/plant
      ! result of ((m^2 of stem)/plant) * (# plants/m^2 ground area) = (m^2 of stem)/(m^2 ground area)
      if( bcdpop .gt. 0.0 ) then
          bcrsai = bcdpop * bc0ssa * (bcmstandstem/bcdpop)**bc0ssb
      else
          bcrsai = 0.0
      end if

      ! (m^2 stem / m^2 ground) / ((stems/m^2 ground) * m) = m/stem
      ! this value not reset unless it is meaningful
      if( (bcdstm * bczht) .gt. 0.0 ) then
          bcxstmrep = bcrsai / (bcdstm * bczht)
      end if

return
end
