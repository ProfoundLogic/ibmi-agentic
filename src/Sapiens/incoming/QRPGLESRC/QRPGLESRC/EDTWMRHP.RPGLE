     h*--------------------------------------------------------------------*
     hcopyright('(c) Copyright Fiserv Insurance Solutions, Inc., 2006-2008')
     h*--------------------------------------------------------------------*
     h/title EDTWMRHP:  Edit Claim Reserve History - WMRHP
     h**********************************************************************
     h*  EDTWMRHP:  Edit Claim Reserve History - WMRHP       C:58   P:SY058
     h**********************************************************************
     h option(*nodebugio : *srcstmt)                                                               *
     h debug(*yes)                                                                                 *
     H*****************************************************************
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 08/18/13  C:280 Project: 147wac                                                            *
      *                 change so that first time record does not error                            *
      *                                                                                            *
      *                                                                                            *
      * 10/29/08  C:33  Project: 114WPD                                                            *
      *                -Convert results to PDF                                                     *
      *=============================================================================================
     fWMRHL     ipe  e           k disk                                          Input file
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


     d*-------| Input Reserves Data Structures |------ 10 @ 1A, 9.2 --------
     d*---------| Code/amount Data Arrays |---------------------------------
     D                 DS
     D  RHT                    1     20
     D                                     DIM(10)                              OUTSTAND RSV TYPE
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS
     D  RHI                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
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

     D errText         s             35    dim(50) ctdata perrcd(1)
     d errCT           s              7p 0 dim(50)                                 Index
     D co#             s              3  0
     D cderrn          s              9  0
     D cdedsc          s             30
     D us_rrn          s              9  0
     D cdename         s             40
     D savCO#          s                   like(rhco#)
     D savfnd          s                   like(rhfnd)
     D savfyr          s                   like(rhfyr)
     D savcase         s                   like(rhcase)

     D savkey          s             16

     d*---------| Initialization work area |-------------------------------
     D sys_date        s               d   datfmt(*usa)
     D sys_time        s               t   timfmt(*hms:)
     D WKdate8         s              8  0
     D SYSnum7         s              7  0
     D xxDATEX         s              7s 0
     d dirPDF          s           1024    inz

     d*---------| Constants |----------------------------------------------
     dcnTITLE          c                   const('PDS Claims - File Edit')
     dcnTYPE           c                   const('Claim Reseve History - WMRHP')
     dgenERR01         c                   const('date prior to accident date')
     dgenERR02         c                   const('date after claimant death')
     dgenERR03         c                   const('future date invalid')

     D KCON01          c                   const('EDIT CLAIM RESERVE HIS+
     D                                            TORY - WMRHP')
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

     d*-------| Time to char |---------------------------------------------
     d                 ds                  inz                                   Work
     d  savetime               1      6  0
     d  savetimx               1      6


     d*-------| x Error Code Data Structure |----------------------------
     d                 ds                  inz                                   Work
     d  xxx                    1      4a
     d  xz                     1      1
     d  x0                     2      2
     d  x                      2      4  0

     d*-------| Claim Key Data Structure |---------------------------------
     d                 ds                  inz                                   Work
     d  CLMkey                 1     16
     d  rhco#                  1      3  0
     d  rhfnd                  4      6  0
     d  rhfyr                  7      9  0
     d  rhcase                10     16  0

     d*---------| Working storage |--------------------------------------
     dWork_Stor        ds                  inz                                   Work
     d  IX                            5p 0                                         Index
     d  xxCOUNT                       9p 0                                         Record counter
     d  xxERRS                        9p 0                                         Record counter
     d  xxACCTS                       9p 0                                         Print counter

     d*-------| Claim employer # to char |--------------------------------
     d                 ds                  inz                                   Work
     d  CLMemp#                1     14
     d  cmemp#                 1      9  0
     d  cmdiv                 10     14  0
     d  xxemp#                 1      9
     d  xxdiv                 10     14


     d*-------| Input Dates Data Structure |----------- 3 @ 7 0 ------------
     d                 ds                  inz                                   Work
     derrDATES                        7  0 dim(3)                                  Index
     d  rhefdt                 1      7  0
     d  rhdt                   8     14  0
     d  rhidt                 15     21  0

     d*-------| Input Users Data Structure |----------- 2 @ 10A ------------
     d                 ds                  inz                                   Work
     derrSTAF                        10A   dim(2)                                  Index
     d  rhuser                 1     10
     d  rhiusr                11     20

     D psds          esds                  extname(szq1p)

     i*_____________________________________________________________________
     i*---------| Define Record ID indicator |------------------------------
     iprh01         50                                                           Work
     i*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

     c*=====================================================================
     c*---===========         Mainline processing             ============--
     c*=====================================================================
     c*    -- Got an input record.
     c                   if        *in50=*on                                     >-+

     c*    -- Count bad CO#                     (no CO# break done)
     c                   if        rhCO#<>pmCO#                                  >-+ :
     c                   add       1             errCT(30)                         : :
     c                   else                                                    <-+ :

     c                   eval      xxCOUNT=xxCOUNT+1                                 :
     c                   if        xxCOUNT=1                                         :
     c                   eval      savetime = rhTIME                               : :
     c                   eval      savkey = CLMkey                                 : :
     c                   endif
     c                   eval      doDetPrint=*off                                 : :
     c                   eval      endEdit=*off                                    : :
     c                   movel     ' '           xz                                    :
     c                   CLEAR                   EER01                                   :

     c*---- Case number is required ---------------------------------------
     c                   if        rhcase = *zero
     c                   eval      x=23                                            : :
     c                   exsr      $Error                                          : :
     c                   eval      endEdit=*on                                     : :
     c                   endif

     c*---- Get claim data ------------------------------------------------
     c     cmky          chain     pcm01                              61
     c                   if        *in61 = *on                                     : :
     c                   movel(p)  CLMkey        EEDATA                            : :
     c                   eval      x = 2                                           : :
     c                   exsr      $Error                                          : :
     c                   eval      endEdit=*on                                     : :
     c                   endif                                                   <-+ :

     C*
     C*** BEGIN CHG *** 08/17/13 **************************************
     C*
     c                   if        savkey = CLMkey and savetime = rhTIME           : :
     c                             and xxCOUNT <> 1
     C*
     C*** END   CHG *** 08/17/13 **************************************
     C*
     c                   movel(p)  savkey        EEDATA                            : :
     c     EEDATA        cat       savetimx:1    EEDATA                            : :
     c                   eval      x = 1                                           : :
     c                   exsr      $Error                                          : :
     c                   endif                                                   <-+ :

     c*---- ValidEmp - validate employer/division--------------------------
     c                   if        endEdit = *off                                  : :
     c                   if        ValidEmp(cmemp#:cmdiv:EM_rrn:rhfnd:rhco#)
     c                              = *off
     c                   eval      x=26                                            : :
     c                   eval      EEDATA = xxemp# + ' / ' + xxdiv
     c                   exsr      $Error                                          : :
     c                   endif

     c*    -- Break level 1
     c                   if        savfnd<>rhFND                                 >-+ :
     c                   exsr      $NewGrp                                         : :
     c                   exsr      $NewFYR                                         : :
     c                   eval      savfnd = rhFND                                  : :
     c                   endif                                                   <-+ :

     c*    -- Break level 2
     c                   if        savfyr<>rhFYR                                 >-+ :
     c                   exsr      $NewFYR                                         : :
     c                   eval      savfyr = rhFYR                                  : :
     c                   endif                                                   <-+ :
     c                   endif                                                   <-+ :


     c*================================================================---
     c*---------| Process record field edits |----------------------------
     c                   exsr      $Process                                        :

     c*================================================================---
     c*---------| End of record process - print detail |------------------
     c                   exsr      $Write_D1                                       :


     c                   endif                                                   <-+ :
     c                   endif                                                   <-+ :
     c                   eval      savkey = CLMkey                                 : :
     c                   eval      savetime = rhTIME                               : :
     c*-------------------------------------------------------------------

     c*================================================================---
     c*---------| End of primary file |-----------------------------------
     clr                 exsr      $Finish                                         :
     c*================================================================---

     c*===================================================================*
     c*------------| Process a record subroutine |-------------------------
     c     $Process      begsr                                                   >---+

     c*---- Claim status --------------------------------------------------
     c                   if        rhCLST <> *blank
     c                   if        rhCLST <> 'O' and rhCLST <> 'C'
     c                   eval      x=20                                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif

     c*---- Record status ------------------------------------------------
     c                   if        rhSTAT <> *blank and rhSTAT <> 'A'
     c                             and rhSTAT <> 'I'
     c                   eval      x=22                                            : :
     c                   exsr      $Error                                          : :
     c                   endif


     c*---- Explanation is required ---------------------------------------
     c                   if        rhEXPL = *blank
     c                   eval      x=21                                            : :
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

     c*------------| Individual field edits |------------------------------

     c*---- Time field - (required) --------------------------------- ----

     c*          Note: used as sequence #: No 'time' edits
     c                   if        rhTIME = *zero                                  : :
     c                   eval      x = 16                                          : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*---- Initial Time - (required) -------------------------------------
     c                   eval      xxtime = RHITIM                                 : :
     c                   eval      D = 17                                          : :
     c                   exsr      $ValiTime                                       : :

     c*---- Reserve Codes/amounts -----------------------------------------
     c                   exsr      $ValiReserv                                     : :

     c                   endsr                                                   <-+

     c*============================================= =======================
     c*------------| Reserve Validation |----------------------------- -----
     c*=====================================================================
     c     $ValiReserv   begsr                                                   >-----+

     c*-- Validation for Reserve Codes/amounts 1-10
     c                   do        10            R
     c                   if        RHT(R) <> *blank and                          >-----+
     c                             ValidResType(co#:RHT(R):cderrn:cdedsc)        >-----+
     c                             = *off                                        >-----+
     c                   eval      x = R + 30                                      : :
     c                   movel(p)  RHT(R)        EEDATA                            : :
     c                   move      R             EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif                                                   <---+
     c                   if        RHI(R) <> *zero and RHT(R) = *blank           >-----+
     c                   eval      x = R + 40                                      : :
     c                   movel(p)  RHT(R)        EEDATA                            : :
     c                   move      R             EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif                                                   <---+
     c                   enddo                                                   <-+

     c                   endsr                                                   <-+

     c*============================================= =======================
     c*------------| Time Validation |-------------------------------- -----
     c*=====================================================================
     c*-- Validation for hhmmss (6.0) times

     c     $ValiTime     begsr                                                   >-----+
     c                   if        xxtime = *zero
     c                   eval      x = D
     c                   exsr      $Error                                          : :

     c                   else                                                      : :

     c*           Test numeric                                         ----
     c                   eval      *in41 = *off
     c                   testn                   xxtmx                41
     c                   if        *IN41 = *off and
     c                             xxtmx <> *blank or

     c                             *IN41 = *on and
     c                             xxtime <> *zero

     c*           Must be numeric                                      ----
     c                   if        *in41 = *off or

     c*           Hours must be 0-24                                   ----
     c                             tmhrs > 24 or

     c*           Minutes must be 00 - 60.                              ----
     c                             tmmin > 60 or

     c*           Seconds must be 00 - 60                             ----
     c                             tmsec > 60

     c                   movel(p)  xxtime        EEDATA                            : :
     c                   eval      x = D + 1
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif
     c                   endif
     c                   endsr                                                   <-+


     c*============================================= =======================
     c*------------| Dates Validation |-------------------------------------
     c*=====================================================================
     c*-- Validation for 3 dates in WMRHP in array errDATES;
     c*     all 3 dates are required.

     c     $ValiDates    begsr                                                   >-----+
     c                   do        3             D
     c                   eval      DTinvalid = *off                                : :
     c                   movel(p)  ' '           EETYPE                            : :

     c                   if        errDATES(D) = *zero                           >-----+
     c                   eval      x = D + 2                                       : :
     c                   exsr      $Error                                          : :
     c                   eval      DTinvalid = *on                                 : :
     c                   else                                                    <---+

     c*      Otherwise, validate the date
     c                   eval      WKdate8 = dtMDYY(errDATES(D))

     c*---------------------------------------------------------------------
     c*      Invalid date.
     c     WKdate8       ifeq      *zero                                         >-----+
     c                   eval      xxDATEX = errDATES(D)                           : :
     c                   movel(p)  xxDATEX       EEDATA                            : :
     c                   eval      x = D + 4                                       : :
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
     c                   if        ValidEmpYr(rhfyr:cmemp#:cmdiv:EL_rrn:
     c                                        rhfnd:rhco#) = *off
     c                   eval      x=25                                            : :
     c                   movel(p)  rhFYR         EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endsr                                                   <---+

     c*=====================================================================
     c*  Change of Group number
     c*=====================================================================
     c     $NewGrp       begsr                                                   >-----+
     c                   if        ValidGrp(rhfnd) = *off
     c                   eval      x=24                                            : :
     c                   movel(p)  rhfnd         EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endsr                                                   <---+


     c*=====================================================================
     c*  Validation for staff codes in WMRHP in array errSTAF.
     c*=====================================================================
     c     $ValiStaff    begsr                                                   >-----+

     c                   do        2             S
     c                   if        errSTAF(S) = *blank                           >-----+
     c                   eval      x = S + 11                                      : :
     c                   exsr      $Error                                          : :
     c                   else
     c                   if        GetUsrRRN(errSTAF(S))                         >-----+
     c                                 = *zero                                   >-----+
     c                   movel(p)  errSTAF(S)    EEDATA                            : :
     c                   eval      x = S + 13                                      : :
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

     c                   z-add     rhFND         FND                               : :
     c                   z-add     rhFYR         FYR                               : :
     c                   z-add     rhCASE        CASE                              : :
     c                   movel     rhSTAT        STAT                              : :
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

     c                   movel(p)  'WMRHP'       EEFILE                                :
     c                   z-add     x             EECODE                            : :
     c                   move(p)   errText(x)    xxFLD6                                :
     c                   movel(p)  xxFLD6        EEFLD                                 :

     c*     Offending application record: ID fields
     c                   movel(p)  RHPGM         ERPGM                                 :
     c                   movel(p)  RHUSER        ERUSER                                :
     c                   z-add     RHDT          ERDT                              : :
     c                   z-add     RHTIME        ERTIME                            : :

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
     c                   kfld                    rhCO#
     c                   kfld                    rhfnd
     c                   kfld                    rhfyr
     c                   kfld                    rhcase


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

     c                   Do        50            x
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

** errText                      50 @ 35  (29 caption +6 fieldname)
Duplicate key fields         CLMKEY        001
Claim record not found       CLMKEY        002
Effective date missing       RHEFDT        003
Date maintained missing      RHDT          004
Initial date missing         RHIDT         005
Effective date invalid       RHEFDT        006
Date maintained invalid      RHDT          007
Initial date invalid         RHIDT         008
Effective date in future     RHEFDT        009
Date maintained in future    RHDT          010
Initial date in future       RHIDT         011
Maintenance user missing     RHUSER        012
Initial user missing         RHIUSR        013
Maintenance user invalid     RHUSER        014
Initial user invalid         RHIUSR        015
Time/seq# missing            RHTIME        016
Initial time missing         RHITIM        017
Initial time invalid         RHITIM        018
xxxxxxx                      XXXXXX        019
Claim status is invalid      RHCLST        020  x
Explanation is missing       RHEXP         021
Record status is invalid     RHSTAT        022  x
Case number missing          RHCASE        023  x
Group not valid              RHFND         024
Fund year is invalid         RHFYR         025
Employer not found           CMEMP#        026
xxxxxxxxxx                   XXXXX         027
xxxxxxxxxx                   XXXXX         028
xxxxxxxxxx                   XXXXX         029
Company number skipped       RHCO#         030
Reserve code 01 invalid      RHSU01        031
Reserve code 02 invalid      RHSU02        032
Reserve code 03 invalid      RHSU03        033
Reserve code 04 invalid      RHSU04        034
Reserve code 05 invalid      RHSU05        035
Reserve code 06 invalid      RHSU06        036
Reserve code 07 invalid      RHSU07        037
Reserve code 08 invalid      RHSU08        038
Reserve code 09 invalid      RHSU09        039
Reserve code 10 invalid      RHSU10        040
Reserve code 01 missing      RHSU01        041
Reserve code 02 missing      RHSU02        042
Reserve code 03 missing      RHSU03        043
Reserve code 04 missing      RHSU04        044
Reserve code 05 missing      RHSU05        045
Reserve code 06 missing      RHSU06        046
Reserve code 07 missing      RHSU07        047
Reserve code 08 missing      RHSU08        048
Reserve code 09 missing      RHSU09        049
Reserve code 10 missing      RHSU10        050
