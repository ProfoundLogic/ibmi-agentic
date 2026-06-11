     h*--------------------------------------------------------------------*
     hcopyright('(c) Copyright Fiserv Insurance Solutions, Inc., 2006-2008')
     h*--------------------------------------------------------------------*
     h/title EDTWMWHP:  Edit Work History File - WMWHP
     h**********************************************************************
     h*  EDTWMWHP:  Edit Work History File - WMWHP.         C:58   P:SY058
     h**********************************************************************
     h option(*nodebugio : *srcstmt)                                                               *
     h debug(*yes)                                                                                 *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 10/29/08  C:33  Project: 114WPD                                                            *
      *                -Convert results to PDF                                                     *
      *=============================================================================================
     fWMWHL     ipe  e           k disk                                          Input file
     fWMCML1    if   e           k disk                                          Input file

     fEDTERR    o    e             disk
     fEDTCLMS   o    e             printer oflind(*IN89) usrOpn

     D/copy sPrRunCmd
     D/copy sprdate
     D/copy sprstring
     D/copy wpremplr
     D/copy wprar
     D/copy wprgroup
     D/copy wpragent
     D/copy wprtable
     D/copy wprcdeval
     D/copy wprusrprms

     D*---------------------------------------------------------------------
     D* Stand Alone Fields - TOP
     D*---------------------------------------------------------------------
     D comapi          c                   const('WA001R')
     D ##co#           s              3  0
     D pmCO#           s              3  0
     D ##fnam          s             40
     D ##conm          s             40
     D ##fnd           s              3  0
     D CoDesc          s             30
     D xxUSER          s             10                                            Record counter
     D xxCODE          s             10                                            Record counter
     D XXCO#           s              3  0
     D EL_rrn          s              9  0
     D EM_rrn          s              9  0
     D*                                                                -----
     D kkfnd           s              3P 0
     D*                                                                -----
     D xxFLD6          s              6                                                :
     D F               s              3  0
     D S               s              3  0
     D R               s              2  0
     D D               s              3  0
     D endEdit         s                   like(*in60)
     D doDetPrint      s                   like(*in60)
     D DTinvalid       s                   like(*in60)
     D #@co#           s              3
     D ##emp#          s              9

     D errText         s             35    dim(25) ctdata perrcd(1)
     d errCT           s              7p 0 dim(25)                                 Index
     D co#             s              3  0
     D cderrn          s              9  0
     D cdedsc          s             30
     D us_rrn          s              9  0
     D cdename         s             40
     D savCO#          s                   like(whco#)
     D savfnd          s                   like(whfnd)
     D savfyr          s                   like(whfyr)
     D savcase         s                   like(whcase)

     D savkey          s             20

     d*---------| Initialization work area |-------------------------------
     D sys_date        s               d   datfmt(*usa)
     D sys_time        s               t   timfmt(*hms:)
     D WKdate8         s              8  0
     D SYSnum7         s              7  0
     D xxDATEX         s              7s 0
     d dirPDF          s           1024    inz

     d*---------| Constants |----------------------------------------------
     dcnTITLE          c                   const('PDS Claims - File Edit')       Title
     dcnTYPE           c                   const('Work History Master- WMWHP')   File (new/old)
     dgenERR01         c                   const('date prior to accident date')
     dgenERR02         c                   const('date after claimant death')
     dgenERR03         c                   const('future date invalid')

     D KCON01          c                   const('EDIT CLAIM WORK HISTORY +
     D                                            - WMWHP')

     D*---------------------------------------------------------------------
     D* Stand Alone Fields - BOTTOM
     D*---------------------------------------------------------------------

     d*-------| Time |------------------------------------------------------
     d                 ds                  inz                                   Work
     d  xxtime                 1      6  0
     d  xxtmx                  1      6
     d  tmhrs                  1      2  0
     d  tmmin                  3      4  0
     d  tmsec                  5      6  0


     d*-------| x Error Code Data Structure |----------------------------
     d                 ds                  inz                                   Work
     d  xxx                    1      4a
     d  xz                     1      1
     d  x0                     2      2
     d  x                      2      4  0

     d*-------| Claim Key Data Structure |---------------------------------
     d                 ds                  inz                                   Work
     d  CLMkey                 1     20
     d  whco#                  1      3  0
     d  whfnd                  4      6  0
     d  whfyr                  7      9  0
     d  whcase                10     16  0
     d  whdate                17     20p 0

     d*---------| Working storage |--------------------------------------
     dWork_Stor        ds                  inz                                   Work
     d  IX                            5p 0                                         Index
     d  xxCOUNT                       9p 0                                         Record counter
     d  xxERRS                        9p 0                                         Record counter
     d  xxACCTS                       9p 0                                         Print counter

     d*-------| Time to char |---------------------------------------------
     d                 ds                  inz                                   Work
     d  savetime               1      6  0
     d  savetimx               1      6

     d*-------| Claim employer # to char |--------------------------------
     d                 ds                  inz                                   Work
     d  CLMemp#                1     14
     d  cmemp#                 1      9  0
     d  cmdiv                 10     14  0
     d  xxemp#                 1      9
     d  xxdiv                 10     14


     d*-------| Input Dates Data Structure |----------- 8 @ 7 0 ------------
     d                 ds                  inz                                   Work
     derrDATES                        7  0 dim(4)                                  Index
     d  whdt                   1      7  0
     d  trDATE                 8     14  0
     d  whidt                 15     21  0
     d  whldwk                22     28  0

     d*-------| Input User Codes Data Structure |------ 2 @ 10A ------------
     d                 ds                  inz                                   Work
     derrSTAF                        10A   dim(2)                                  Index
     d  whuser                 1     10
     d  whiusr                11     20

     D psds          esds                  extname(szq1p)

     i*_____________________________________________________________________
     i*---------| Define Record ID indicator |------------------------------
     ipwh01         50                                                           Work
     i*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

     c*=====================================================================
     c*---===========         Mainline processing             ============--
     c*=====================================================================
     c*    -- Got an input record.
     c                   if        *in50=*on                                     >-+
     c                   eval      trDATE = whDATE                                 : :

     c*    -- Count bad CO#                     (no CO# break done)
     c                   if        whCO#<>pmCO#                                  >-+ :
     c                   add       1             errCT(25)                         : :
     c                   else                                                    <-+ :

     c                   eval      xxCOUNT=xxCOUNT+1                                 :
     c                   if        xxCOUNT=1                                         :
     c                   eval      savetime = whTIME                               : :
     c                   endif
     c                   eval      doDetPrint=*off                                 : :
     c                   eval      endEdit=*off                                    : :
     c                   movel     ' '           xz                                    :
     c                   CLEAR                   EER01                                   :

     c*---- Case number is required ---------------------------------------
     c                   if        whcase = *zero
     c                   eval      x=18                                            : :
     c                   exsr      $Error                                          : :
     c                   eval      endEdit=*on                                     : :
     c                   endif

     c*---- Get claim data ------------------------------------------------
     c     cmky          chain     pcm01                              61
     c                   if        *in61 = *on                                     : :
     c                   movel(p)  CLMkey        EEDATA                            : :
     c                   eval      x = 19                                          : :
     c                   exsr      $Error                                          : :
     c                   eval      endEdit=*on                                     : :
     c                   endif                                                   <-+ :

     c                   if        savkey = CLMkey                                 : :
     c                   movel(p)  savkey        EEDATA                            : :
     c     EEDATA        cat       savetimx:1    EEDATA                            : :
     c                   eval      x = 23                                          : :
     c                   exsr      $Error                                          : :
     c                   endif                                                   <-+ :

     c*---- ValidEmp - validate employer/division--------------------------
     c                   if        endEdit = *off                                  : :
     c                   if        ValidEmp(cmemp#:cmdiv:EM_rrn:whfnd:whco#)
     c                              = *off
     c                   eval      x=21                                            : :
     c                   eval      EEDATA = xxemp# + ' / ' + xxdiv
     c                   exsr      $Error                                          : :
     c                   endif

     c*    -- Break level 1
     c                   if        savfnd <> whFND                               >-+ :
     c                   exsr      $NewGrp                                         : :
     c                   exsr      $NewFYR                                         : :
     c                   eval      savfnd = whFND                                  : :
     c                   endif                                                   <-+ :

     c*    -- Break level 2
     c                   if        savfyr <> whFYR                               >-+ :
     c                   exsr      $NewFYR                                         : :
     c                   eval      savfyr = whFYR                                  : :
     c                   endif                                                   <-+ :
     c                   endif                                                   <-+ :


     c*================================================================---
     c*---------| Process record field edits |----------------------------
     c                   exsr      $Process                                        :

     c*================================================================---
     c*---------| End of record process - print detail |------------------
     c                   exsr      $Write_D1                                       :


     c                   endif                                                   <-+ :
     c                   eval      savkey = CLMkey                                 : :
     c                   endif                                                   <-+ :
     c*-------------------------------------------------------------------

     c*================================================================---
     c*---------| End of primary file |-----------------------------------
     clr                 exsr      $Finish                                         :
     c*================================================================---

     c*===================================================================*
     c*------------| Process a record subroutine |-------------------------
     c     $Process      begsr                                                   >---+


     c*---- Record status ------------------------------------------------
     c                   if        whSTAT <> *blank and whSTAT <> 'A'
     c                             and whSTAT <> 'I'
     c                   eval      x=14                                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*---------------------------------------------------------------------
     c*---| ValiDates - all dates in Demographics array: errDATES |---------
     c                   exsr      $ValiDates                                      :
     c*---------------------------------------------------------------------

     c*---------------------------------------------------------------------
     c*---| ValiStaff- all staff codes in Claim Master array: errSTAF |-----
     c                   exsr      $ValiStaff                                      :
     c*---------------------------------------------------------------------


     c*---- Maintenance Time - (required) ---------------------------------
     c                   eval      xxtime = whTIME                                 : :
     c                   eval      D = 16                                          : :
     c                   exsr      $ValiTime                                       : :

     c*---- Initial Time - (not required)----------------------------------
     c                   eval      xxtime = whITIM                                 : :
     c                   eval      D = 17                                          : :
     c                   exsr      $ValiTime                                       : :

     c                   endsr                                                   <-+

     c*============================================= =======================
     c*------------| Time Validation |-------------------------------- -----
     c*=====================================================================
     c*-- Validation for hhmmss (6.0) times

     c     $ValiTime     begsr                                                   >-----+
     c                   if        D = 16 and
     c                             xxtime = *zero
     c                   eval      x = 15
     c                   exsr      $Error                                          : :

     c                   else                                                      : :

     c*           Test numeric                                         ----
     c                   eval      *in41 = *off
     c                   testn                   xxtmx                41
     c                   if        *IN41 = *off and
     c                             xxtmx <> *blank or

     c                             *IN41 = *on and
     c                             xxtime <> *zero

     c*        o  Must be numeric                                      ----
     c*        o  Hours must be 0-24                                   ----
     c*        o  Minutes must be 00 - 60.                              ----
     c*        o  Seconds must be 00 - 60                             ----

     c                   if        *in41 = *off or
     c                             tmhrs > 24 or
     c                             tmmin > 60 or
     c                             tmsec > 60

     c                   movel(p)  xxtime        EEDATA                            : :
     c                   eval      x = D
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif
     c                   endif
     c                   endsr                                                   <-+


     c*============================================= =======================
     c*------------| Dates Validation |-------------------------------------
     c*=====================================================================
     c*-- Validation for 4 dates in WMWHP in array errDATES;
     c*     1st 2 dates are required.

     c     $ValiDates    begsr                                                   >-----+
     c                   do        4             D
     c                   eval      DTinvalid = *off                                : :
     c                   movel(p)  ' '           EETYPE                            : :

     c                   if        D < 3 and                                     >-----+
     c                             errDATES(D) = *zero                           >-----+
     c                   eval      x = D + 4                                       : :
     c                   exsr      $Error                                          : :
     c                   eval      DTinvalid = *on                                 : :
     c                   endif                                                   <---+

     c*      Otherwise, validate the date
     c                   if        errDATES(D) <> *zero                          >-----+
     c                   eval      WKdate8 = dtMDYY(errDATES(D))

     c*---------------------------------------------------------------------
     c*      Invalid date.
     c     WKdate8       ifeq      *zero                                         >-----+
     c                   eval      xxDATEX = errDATES(D)                           : :
     c                   movel(p)  xxDATEX       EEDATA                            : :
     c                   eval      x = D                                           : :
     c                   exsr      $Error                                          : :
     c                   eval      DTinvalid = *on                                 : :
     c                   else                                                    <---+

     c*---------------------------------------------------------------------
     c*    Date is valid: check relationship error.
     c*           a. Future date.
     c                   if        errDATES(D) > SYSnum7                         >-----+
     c                   eval      xxDATEX = errDATES(D)                           : :
     c                   movel(p)  xxDATEX       EEDATA                            : :
     c                   movel(p)  'F'           EETYPE                            : :
     c                   eval      x = D + 6                                       : :
     c                   exsr      $Error                                          : :
     c                   eval      DTinvalid = *on                                 : :
     c                   endif                                                   <---+
     c                   endif                                                   <---+
     c                   endif                                                   <---+

     c                   enddo                                                   <---+

     c                   endsr                                                   <---+

     c*=====================================================================
     c*  Change Fund Year
     c*=====================================================================
     c     $NewFYR       begsr                                                   >-----+
     c*
     c*---- ValidEmpYr - validate employer/division/fyr--------------------
     c                   if        ValidEmpYr(whfyr:cmemp#:cmdiv:EL_rrn:
     c                                        whfnd:whco#) = *off
     c                   eval      x=22                                            : :
     c                   movel(p)  whfyr         EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endsr                                                   <---+

     c*=====================================================================
     c*  Change of Group number
     c*=====================================================================
     c     $NewGrp       begsr                                                   >-----+
     c                   if        ValidGrp(whfnd) = *off
     c                   eval      x=20                                            : :
     c                   movel(p)  whfnd         EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endsr                                                   <---+


     c*=====================================================================
     c*  Validation for staff codes in WMWHP in array errSTAF.
     c*=====================================================================
     c     $ValiStaff    begsr                                                   >-----+

     c                   do        2             S
     c                   if        errSTAF(S) = *blank                           >-----+
     c                   if        S =1                                          >-----+
     c                   eval      x = 11                                          : :
     c                   exsr      $Error                                          : :
     c                   endif
     c*
     c*   Not blank, validate user ID
     c                   else
     c                   if        GetUsrRRN(errSTAF(S))                         >-----+
     c                                 = *zero                                   >-----+
     c                   movel(p)  errSTAF(S)    EEDATA                            : :
     c                   eval      x = S + 11                                      : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif
     c                   enddo                                                   <---+

     c                   endsr                                                   <---+


     c*=====================================================================
     c*------------| Error subroutine |-------------------------------------
     c*=====================================================================
     c     $Error        begsr                                                   >-+
     c                   ADD       1             errCT(x)                              :

      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++========================
     c*------------ Set up print fields ------------------------------------
     c                   if        x < 99
     c                   move      ' '           x0                                    :
     c                   endif                                                   <-----+
     c     PeList        cat       xxx:0         Pelist                                :
     c                   movel     ','           xz                                    :

     c                   z-add     whFND         FND                               : :
     c                   z-add     whFYR         FYR                               : :
     c                   z-add     whCASE        CASE                              : :
     c                   movel     whSTAT        STAT                              : :
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++========================

     c*------------ File out -----------------------------------------------
     c                   exsr      $Write_Err                                      :
     C                   movel(p)  *BLANKS       EEDATA

     c                   eval      doDetPrint=*on                                  : :
     c                   endsr                                                   <-----+


     c*=====================================================================
     c*------------| Write Error File subroutine |-------------------------
     c*=====================================================================
     c     $Write_Err    begsr                                                   >-+
     c*        First get key
     c                   movel(p)  CLMkey        EEKEY                                 :

     c                   movel(p)  'WMWHP'       EEFILE                                :
     c                   z-add     x             EECODE                            : :
     c                   move(p)   errText(x)    xxFLD6                                :
     c                   movel(p)  xxFLD6        EEFLD                                 :

     c*     Offending application record: ID fields
     c                   movel(p)  whPGM         ERPGM                                 :
     c                   movel(p)  whUSER        ERUSER                                :
     c                   z-add     whDT          ERDT                              : :
     c                   z-add     whTIME        ERTIME                            : :

     c*     Error record ID fields
     c                   eval      EEPGM  = q1pgm
     c                   eval      EEUSER = q1user
     c     *hms          move      q1time        sys_time
     c                   move      sys_time      EETIME
     c     *cymd         move      sys_date      EEDT
     c                   eval      EEIPGM  = q1pgm
     c                   eval      EEIUSR = q1user
     c     *cymd         move      sys_date      EEIDT
     c                   move      sys_time      EEITIM

     c                   write     EER01                                               :

     c                   eval      xxERRS=xxERRS+1                                     :
     c                   endsr                                                   <-+

     c*------------< Print detail line subroutine >-----------------------
     c     $Write_D1     begsr                                                   >-+
     c                   if        PeList <> *blank or
     c                             doDetPrint = *on                              >-+ :
     c                   if        *in89 = *on
     c                   write     HEAD                                            : :
     c                   write     HED0                                            : :
     c                   eval      *in89=*off                                      : :
     c                   endif                                                   <-----+
     c                   write     PDET1                                           :
     c                   eval      xxACCTS=xxACCTS+1                                   :
     c                   endif                                                   <-----+
     c                   eval      PeList=*blank                                     :
     c                   eval      doDetPrint=*off                                 : :
     c                   endsr                                                   <-+

     c*------------| Print total line subroutine |------------------------
     c     $Write_Tl     begsr                                                   >-+
     c                   if        *in89 = *on
     c                   write     HEAD                                            : :
     c                   write     HED0                                            : :
     c                   eval      *in89=*off                                      : :
     c                   endif                                                   <-----+
     c                   endsr                                                   <-+

     c*---------< Initialization Subroutine >-----------------------------
     c     *inzsr        begsr                                                   >-+

     c*---------< Program entry >-----------------------------------------
     c     *entry        plist
     c                   parm                    pmRTCD            1
     c                   parm                    pmCO              3
     c                   parm                    pmLIBRARY        10

     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam

      *=============================================================================================
      *  Klists
      *=============================================================================================
     c     cmky          klist
     c                   kfld                    whCO#
     c                   kfld                    whfnd
     c                   kfld                    whfyr
     c                   kfld                    whcase

     c                   open      EDTCLMS

     C                   move      pmCO          pmCO#
     c                   eval      co#=pmCO#                                     >-+ :

     C     *usa          move      *date         sys_date
     C     *hms          move      q1time        sys_time

     c     *usa          move      sys_date      ptRDAT                            :
     C     *hms          move      sys_time      ptRTIM

     C     *cymd         move      sys_date      SYSnum7
     c                   eval      xxCOUNT=*zero                                   :

     c*--------< Call API for getting descriptions (use 91 on error)
     c                   call      comapi        compl                  91
     c                   eval      xxCO#=##co#                                     : :
     c                   eval      CONAME = ##conm
     c                   eval      CONAME = centerFld(CONAME)

     c                   eval      PTNAME = (KCON01)
     c                   eval      PTNAME = centerFld(PTNAME)

     c                   write     HEAD                                            : :
     c                   write     HED0                                            : :
     c                   eval      *in89=*off                                      :

     c                   endsr                                                   <-+
     c*-------------------------------------------------------------------

     c*------------< End of job  subroutine >-----------------------------
     c     $Finish       begsr                                                   >-+
     c                   exsr      $Write_Tl                                       :

     c*---  Print Error Summary
     c                   write     HEAD                                            : :
     c                   write     HEDerr                                          : :

     c                   Do        25            x
     c                   if        errCT(x) <> 0                                   : :

     c                   movel(p)  errtext(x)    PERRTXT                           :
     c                   move(p)   errtext(x)    PERRFIELD                         :

     c                   z-add     errCT(x)      PERRCT                            : :
     c                   write     errLIN                                          : :

     c                   endif                                                   <-+
     c                   enddo                                                   <-+
     c*---  End  line ------------------------------------------------------

     c                   write     HEAD                                            : :
     c                   write     PRTTOT                                          :

     c                   close     EDTCLMS
     C                   exsr      CvtToPDF

     c                   endsr                                                   <-+
     c/copy wCpyEdtClm

** errText                      25 @ 35  (29 caption +6 fieldname)
Date maintained invalid      WHDT          001
Work history date invalid    WHDATE        002
Initial date invalid         WHIDT         003
Last date worked invalid     WHLDWK        004
Date maintained missing      WHDT          005
Work history date missing    WHDATE        006
Date maintained in future    WHDT          007
Work history date in future  WHDATE        008
Initial date in future       WHIDT         009
Last date worked in future   WHLDWK        010
Maintenance user missing     WHUSER        011
Maintenance user invalid     WHUSER        012
Initial user invalid         WHIUSR        013
Status is invalid            WHSTAT        014
Time maintained missing      WHTIME        015
Time maintained invalid      WHTIME        016
Initial time invalid         WHITIM        017
Case number missing          WHCASE        018
Claim record not found       CLMKEY        019
Group not valid              WHFND         020
Employer not found           WHEMP#        021
Fund year is invalid         WHFYR         022
Duplicate key fields         CLMKEY        023
xxxxx                        XXXXX         024
Company number skipped       WHCO#         025
