     h*--------------------------------------------------------------------*
     hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*--------------------------------------------------------------------*
     h/title EDTWMPDP:  Edit Claim Provider Master - WMPDP
     h**********************************************************************
     h*  EDTWMPDP:  Edit Claim Provider Master - WMPDP       C:58   P:SY058
     h**********************************************************************
     h option(*nodebugio : *srcstmt)                                                               *
     h debug(*yes)                                                                                 *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 11/02/09  C:232 Project: 139WMR                                                            *
      *                 OFAC Reporting-Add Pending Status Indicator.                               *
      *                                                                                            *
      * 10/29/08  C:33  Project: 114WPD                                                            *
      *                -Convert results to PDF                                                     *
      *=============================================================================================
     fWMPDL     ipe  e           k disk                                          Input file

     fEDTERR    o    e             disk
     fEDTPROV   o    e             printer oflind(*IN89) usrOpn

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

     D errText         s             35    dim(35) ctdata perrcd(1)
     d errCT           s              7p 0 dim(35)                                 Index
     D co#             s              3  0
     D cderrn          s              9  0
     D cdedsc          s             30
     D us_rrn          s              9  0
     D cdename         s             40
     D savFEIN         s                   like(pdFEIN)
     D savMOD          s                   like(pdMOD)

     D savkey          s             13

     d*---------| Initialization work area |-------------------------------
     D sys_date        s               d   datfmt(*usa)
     D sys_time        s               t   timfmt(*hms:)
     D WKdate8         s              8  0
     D SYSnum7         s              7  0
     D xxDATEX         s              7s 0
     d dirPDF          s           1024    inz

     d*---------| Constants |----------------------------------------------
     dcnTITLE          c                   const('PDS Claims - File Edit')
     dcnTYPE           c                   const('Claim Providers - WMPDL')
     dgenERR03         c                   const('future date invalid')

     D KCON01          c                   const('EDIT CLAIM PROVIDER MA+
     D                                            STER - WMPDP')
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
     d  PRVkey                 1     13
     d  pdFEIN                 1      9  0
     d  spac1                 10     10a
     d  pdMOD                 11     13  0

     d*---------| Working storage |--------------------------------------
     dWork_Stor        ds                  inz                                   Work
     d  IX                            5p 0                                         Index
     d  xxCOUNT                       9p 0                                         Record counter
     d  xxERRS                        9p 0                                         Record counter
     d  xxACCTS                       9p 0                                         Print counter


     d*-------| Input Dates Data Structure |----------- 5 @ 7 0 ------------
     d                 ds                  inz                                   Work
     derrDATES                        7  0 dim(5)                                  Index
     d  pddt                   1      7  0
     d  pdidt                  8     14  0
     d  pdw9se                15     21  0
     d  pdw9rc                22     28  0
     d  pdwcdt                29     35  0

     d*-------| Input Users Data Structure |----------- 2 @ 10A ------------
     d                 ds                  inz                                   Work
     derrSTAF                        10A   dim(2)                                  Index
     d  pduser                 1     10
     d  pdiusr                11     20

     D psds          esds                  extname(szq1p)

     i*_____________________________________________________________________
     i*---------| Define Record ID indicator |------------------------------
     ippd01         50                                                           Work
     i*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

     c*=====================================================================
     c*---===========         Mainline processing             ============--
     c*=====================================================================
     c*    -- Got an input record.
     c                   if        *in50=*on                                     >-+
     c                   eval      xxCOUNT=xxCOUNT+1                                 :

     c                   if        xxCOUNT=1                                         :
     c                   eval      savkey = PRVkey                                 : :
     c                   endif

     c                   eval      doDetPrint=*off                                 : :
     c                   eval      endEdit=*off                                    : :
     c                   movel     ' '           xz                                    :
     c                   CLEAR                   EER01                                   :
     c*
     c*---- FEIN is required ----------------------------------------------
     c                   if        pdFEIN = *zero
     c                   eval      x=13                                            : :
     c                   exsr      $Error                                          : :
     c                   else
     c*

     c                   if        savkey = PRVkey                                   :
     c                   movel(p)  savkey        EEDATA                            : :
     c                   eval      x = 14                                          : :
     c                   exsr      $Error                                          : :
     c                   endif                                                   <-+ :


     c*================================================================---
     c*---------| Process record field edits |----------------------------
     c                   exsr      $Process                                        :

     c*================================================================---
     c*---------| End of record process - print detail |------------------
     c                   exsr      $Write_D1                                       :


     c                   eval      savkey = PRVkey                                 : :
     c                   endif                                                   <-+ :
     c                   endif
     c*-------------------------------------------------------------------

     c*================================================================---
     c*---------| End of primary file |-----------------------------------
     clr                 exsr      $Finish                                         :
     c*================================================================---

     c*===================================================================*
     c*------------| Process a record subroutine |-------------------------
     c     $Process      begsr                                                   >---+

     c*--- Record status --------------------------------------------------
     c                   if        pdSTAT <> 'A' and pdSTAT <> 'I'
     c*** Beg Add *** 11/02/09 ****************************************
     c                   if        ( clientID = '139' and pdSTAT <> 'P')
     c                             or ( clientID <> '139' )
     c*** End Add *** 11/02/09 ****************************************
     c                   eval      x=15                                            : :
     c                   exsr      $Error                                          : :
     c*** Beg Add *** 11/02/09 ****************************************
     c                   endif
     c*** End Add *** 11/02/09 ****************************************
     c                   endif

     c*---- Name is required ----------------------------------------------
     c                   if        pdNAME = *blank
     c                   eval      x=16                                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*---- Address is required -------------------------------------------
     c                   if        pdADR1 = *blank
     c                   eval      x=17                                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        pdCTY = *blank
     c                   eval      x=18                                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        pdST = *blank
     c                   eval      x=19                                            : :
     c                   exsr      $Error                                          : :
     c                   else
     c                   if        ValidState(pdst:us_rrn:cdename) = *off
     c                   movel(p)  pdst          EEDATA                            : :
     c                   eval      x = 20                                          : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif

     c                   if        pdZIP = *zero
     c                   eval      x=21                                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*--- 1099 Flag ------------------------------------------------  ----
     c                   if        pd1099 = *blank
     c                   eval      x=22                                            : :
     c                   exsr      $Error                                          : :

     c                   else
     c                   if        pd1099 <> 'Y' and pd1099 <> 'N'
     c                   eval      x=23                                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif

     c*--- Payee Type -----------------------------------------------  ----
     c                   if        pdTYPE = *blank
     c                   eval      x=32                                            : :
     c                   exsr      $Error                                          : :
     c                   else
     c                   if        ValidPayeeTyp(co#:pdtype:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=33                                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif

     c*--- Specialty Code -------------------------------------------  ----
     c                   if        pdSPCD <> *blank and
     c                             ValidSPCode(co#:pdSPCD:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=34                                            : :
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
     c                   eval      xxtime = PDTIME                                 : :
     c                   eval      D = 24                                          : :
     c                   exsr      $ValiTime                                       : :

     c*---- Initial Time - (required) -------------------------------------
     c                   eval      xxtime = PDITIM                                 : :
     c                   eval      D = 26                                          : :
     c                   exsr      $ValiTime                                       : :

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
     c*-- Validation for 5 dates in WMPDP in array errDATES;
     c*   first 2 dates are required.

     c     $ValiDates    begsr                                                   >-----+
     c                   do        5             D
     c                   eval      DTinvalid = *off                                : :
     c                   movel(p)  ' '           EETYPE                            : :

     c                   if        D < 3                                         >-----+
     c                   if        errDATES(D) = *zero                           >-----+
     c                   eval      x = D + 10                                      : :
     c                   exsr      $Error                                          : :
     c                   eval      DTinvalid = *on                                 : :
     c                   endif                                                   <---+
     c                   endif                                                   <---+

     c*      Validate if not zero
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
     c                   eval      x = D + 5                                       : :
     c                   exsr      $Error                                          : :
     c                   eval      DTinvalid = *on                                 : :
     c                   endif                                                   <---+
     c                   endif                                                   <---+
     c                   endif                                                   <---+

     c                   enddo                                                   <---+

     c                   endsr                                                   <---+


     c*=====================================================================
     c*  Validation for staff codes in WMRHP in array errSTAF.
     c*=====================================================================
     c     $ValiStaff    begsr                                                   >-----+

     c                   do        2             S
     c                   if        errSTAF(S) = *blank                           >-----+
     c                   eval      x = S + 27                                      : :
     c                   exsr      $Error                                          : :
     c                   else
     c                   if        GetUsrRRN(errSTAF(S))                         >-----+
     c                                 = *zero                                   >-----+
     c                   movel(p)  errSTAF(S)    EEDATA                            : :
     c                   eval      x = S + 29                                      : :
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

     c                   z-add     pdFEIN        FEIN                              : :
     c                   z-add     pdMOD         MOD                               : :
     c                   movel     pdSTAT        STAT                                  :

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
     c                   movel(p)  PRVkey        EEKEY                                 :

     c                   movel(p)  'WMPDP'       EEFILE                                :
     c                   z-add     x             EECODE                            : :
     c                   move(p)   errText(x)    xxFLD6                                :
     c                   movel(p)  xxFLD6        EEFLD                                 :

     c*     Offending application record: ID fields
     c                   movel(p)  PDPGM         ERPGM                                 :
     c                   movel(p)  PDUSER        ERUSER                                :
     c                   z-add     PDDT          ERDT                              : :
     c                   z-add     PDTIME        ERTIME                            : :

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

     c*** Beg Add *** 11/02/09 ****************************************
     c                   call      'RTVCLTID'
     C                   PARM                    clientID          3
     c*** End Add *** 11/02/09 ****************************************
     c                   open      EDTPROV

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

     c                   Do        35            x
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

     c                   close     EDTPROV
     C                   exsr      CvtToPDF

     c                   endsr                                                   <-+
     c/copy wCpyEdtClm

** errText                      35 @ 35  (29 caption +6 fieldname)
Date maintained invalid      PDDT          001
Initial date invalid         PDIDT         002
W9 sent date invalid         PDW9SE        003
W9 received date invalid     PDW9RC        004
WC Cert date invalid         PDWCDT        005
Date maintained in future    PDDT          006
Initial date in future       PDIDT         007
W9 sent date in future       PDW9SE        008
W9 received date in future   PDW9RC        009
WC Cert date in future       PDWCDT        010
Date maintained missing      PDDT          011
Initial date missing         PDIDT         012
FEIN number is missing       PDFEIN        013
Duplicate FEIN/MOD s         PRVKEY        014
Record status is invalid     PDSTAT        015
Provider name is missing     PDNAME        016
Provider address is missing  PDADR1        017
City is missing              PDCTY         018
State is missing             PDST          019
State is invalid             PDST          020
Zip code is missing          PDZIP         021
1099 flag is missing         PD1099        022
1099 flag is invalid         PD1099        023
Maintenance time missing     PDTIME        024
Maintenance time invalid     PDTIME        025
Initial time missing         PDITIM        026
Initial time invalid         PDITIM        027
Maintenance user missing     PDUSER        028
Initial user missing         PDIUSR        029
Maintenance user invalid     PDUSER        030
Initial user invalid         PDIUSR        031
Payee type missing           PDTYPE        032
Payee type invalid           PDTYPE        033
Specialty code invalid       PDSPCD        034
xxxxxxxxxxxx                 XXXXXC        035
