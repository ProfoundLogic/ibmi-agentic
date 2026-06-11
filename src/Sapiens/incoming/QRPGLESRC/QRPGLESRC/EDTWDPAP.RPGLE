     h*--------------------------------------------------------------------*
     hcopyright('(c) Copyright Fiserv Insurance Solutions, Inc., 2006-2008')
     h*--------------------------------------------------------------------*
     h/title EDTWDPAP:  Edit Claim Payment History - WDPAP
     h**********************************************************************
     h*  EDTWDPAP:  Edit Claim Payment History - WDPAP.      C:58   P:SY058
     h**********************************************************************
     h option(*nodebugio : *srcstmt)                                                               *
     h debug(*yes)                                                                                 *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 10/29/08  C:33  Project: 114WPD                                                            *
      *                -Convert results to PDF                                                     *
      *=============================================================================================
     h
     fWDPAL     ipe  e           k disk                                          Input file
     fWMPDL     if   e           k disk                                          Input file
     fWMCML1    if   e           k disk                                          Input file

     fEDTERR    o    e             disk
     fEDTPMTS   o    e             printer oflind(*IN89) usrOpn

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

     D errText         s             35    dim(61) ctdata perrcd(1)
     d errCT           s              7p 0 dim(61)                                 Index
     D co#             s              3  0
     D cderrn          s              9  0
     D cdedsc          s             30
     D us_rrn          s              9  0
     D cdename         s             40
     D savCO#          s                   like(paco#)
     D savfnd          s                   like(pafnd)
     D savfyr          s                   like(pafyr)
     D savcase         s                   like(pacase)

     D savkey          s             16

     d*---------| Initialization work area |-------------------------------
     D sys_date        s               d   datfmt(*usa)
     D sys_time        s               t   timfmt(*hms:)
     D WKdate8         s              8  0
     D SYSnum7         s              7  0
     D xxDATEX         s              7s 0
     d dirPDF          s           1024    inz

     d*---------| Constants |----------------------------------------------
     dcnTITLE          c                   const('PDS Claims - File Edit')       Title
     dcnTYPE           c                   const('Claim Payment History WDPAP')     e (new/old)
     dgenERR01         c                   const('date prior to accident date')
     dgenERR02         c                   const('date after claimant death')
     dgenERR03         c                   const('future date invalid')

     D KCON01          c                   const('EDIT CLAIM PAYMENT HISTORY +
     D                                            - WDPAP')

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
     d  paFEIN                 1      9  0
     d  xxFEIN                 1      9

     d*-------| Claim Key Data Structure |---------------------------------
     d                 ds                  inz                                   Work
     d  paSEQ#                 1      5  0
     d  xxSEQ#                 1      5

     d*-------| Claim Key Data Structure |---------------------------------
     d                 ds                  inz                                   Work
     d  CLMkey                 1     16
     d  paco#                  1      3  0
     d  pafnd                  4      6  0
     d  pafyr                  7      9  0
     d  pacase                10     16  0

     d*---------| Working storage |--------------------------------------
     dWork_Stor        ds                  inz                                   Work
     d  IX                            5p 0                                         Index
     d  xxCOUNT                       9p 0                                         Record counter
     d  xxERRS                        9p 0                                         Record counter
     d  xxACCTS                       9p 0                                         Print counter

     d*-------| Time to char |---------------------------------------------
     d                 ds                  inz                                   Work
     d  saveseq                1      5  0
     d  saveseqx               1      5

     d*-------| Claim employer # to char |--------------------------------
     d                 ds                  inz                                   Work
     d  CLMemp#                1     14
     d  cmemp#                 1      9  0
     d  cmdiv                 10     14  0
     d  xxemp#                 1      9
     d  xxdiv                 10     14


     d*-------| Input Dates Data Structure |---------- 10 @ 7 0 ------------
     d                 ds                  inz                                   Work
     derrDATES                        7  0 dim(10)                                 Index
     d  patrdt                 1      7  0
     d  papodt                 8     14  0
     d  pafrdt                15     21  0
     d  patodt                22     28  0
     d  padt                  29     35  0
     d  parcvd                36     42  0
     d  padue                 43     49  0
     d  padpcd                50     56  0
     d  parcdt                57     63  0
     d  paidt                 64     70  0

     d*-------| Input User Codes Data Structure |------ 4 @ 10A ------------
     d                 ds                  inz                                   Work
     derrSTAF                        10A   dim(4)                                  Index
     d  pauser                 1     10
     d  paiusr                11     20
     d  parqst                21     30
     d  padpcu                31     40

     D psds          esds                  extname(szq1p)

     i*_____________________________________________________________________
     i*---------| Define Record ID indicator |------------------------------
     ippa01         50                                                           Work
     i*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

     c*=====================================================================
     c*---===========         Mainline processing             ============--
     c*=====================================================================
     c*    -- Got an input record.
     c                   if        *in50=*on                                     >-+

     c*    -- Count bad CO#                     (no CO# break done)
     c                   if        paCO#<>pmCO#                                  >-+ :
     c                   add       1             errCT(61)                         : :
     c                   else                                                    <-+ :

     c                   eval      xxCOUNT=xxCOUNT+1                                 :
     c                   if        xxCOUNT=1                                         :
     c                   eval      saveseq = paSEQ#                                : :
     c                   endif
     c                   eval      doDetPrint=*off                                 : :
     c                   eval      endEdit=*off                                    : :
     c                   movel     ' '           xz                                    :
     c                   CLEAR                   EER01                                   :

     c*---- Case number is required ---------------------------------------
     c                   if        pacase = *zero
     c                   eval      x=17                                            : :
     c                   exsr      $Error                                          : :
     c                   eval      endEdit=*on                                     : :
     c                   endif

     c*---- Sequence number is required -----------------------------------
     c                   if        paSEQ# = *zero
     c                   eval      x=48                                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*---- Get claim data ------------------------------------------------
     c     cmky          chain     pcm01                              61
     c                   if        *in61 = *on                                     : :
     c                   movel(p)  CLMkey        EEDATA                            : :
     c                   eval      x = 19                                          : :
     c                   exsr      $Error                                          : :
     c                   eval      endEdit=*on                                     : :
     c                   endif                                                   <-+ :

     c                   if        savkey = CLMkey and saveseq = paSEQ#            : :
     c                   movel(p)  savkey        EEDATA                            : :
     c     EEDATA        cat       saveseqx:1    EEDATA                            : :
     c                   eval      x = 18                                          : :
     c                   exsr      $Error                                          : :
     c                   endif                                                   <-+ :

     c*---- ValidEmp - validate employer/division--------------------------
     c                   if        endEdit = *off                                  : :
     c                   if        ValidEmp(cmemp#:cmdiv:EM_rrn:pafnd:paco#)
     c                              = *off
     c                   eval      EEDATA = xxemp# + '/' + xxdiv
     c                   eval      x=31                                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*    -- Break level 1
     c                   if        savfnd<>paFND                                 >-+ :
     c                   exsr      $NewGrp                                         : :
     c                   exsr      $NewFYR                                         : :
     c                   eval      savfnd = paFND                                  : :
     c                   endif                                                   <-+ :

     c*    -- Break level 2
     c                   if        savfyr<>paFYR                                 >-+ :
     c                   exsr      $NewFYR                                         : :
     c                   eval      savfyr = paFYR                                  : :
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

     c*---- Amount is required --------------------------------------------
     c                   if        paAMT = *zero
     c                   eval      x=21                                            : :
     c                   exsr      $Error                                          : :
     c                   else

     c*---- FEIN is required if not compensation --------------- ----
     c                   if        paFEIN = *zero and paRETP <> 'C'
     c                   eval      x=22                                            : :
     c                   exsr      $Error                                          : :
     c                   else
     c                   if        paFEIN <> *zero
     c     pdky          chain     ppd01                              62
     c                   if        *in62 = *on or pdSTAT <> 'A'
     c                   if        xxFEIN <> CMSS#                                 : :
     c                   movel(p)  xxFEIN        EEDATA                            : :
     c                   eval      x=23                                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif
     c                   endif
     c                   endif
     c                   endif

     c*---- Record status ------------------------------------------------
     c                   if        paSTAT <> *blank and paSTAT <> 'A'
     c                             and paSTAT <> 'I'
     c                   movel(p)  paSTAT        EEDATA                            : :
     c                   eval      x=24                                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*---- Payment value 1  (BR2) ---------------------------------------
     c                   if        paBR2  = *blank
     c                   eval      x=32                                            : :
     c                   exsr      $Error                                          : :
     c                   else
     c                   if        ValidBR2Code(co#:paBR2:cderrn:cdedsc) = *off
     c                   movel(p)  paBR2         EEDATA                            : :
     c                   eval      x=33                                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif

     c*---- Payment value 2  (BR7) ---------------------------------------
     c                   if        paBR7  = *blank
     c                   eval      x=34                                            : :
     c                   exsr      $Error                                          : :
     c                   else
     c                   if        ValidBR7Code(co#:paBR7:cderrn:cdedsc) = *off
     c                   movel(p)  paBR7         EEDATA                            : :
     c                   eval      x=35                                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif

     c*---- Reserve type -------------------------------------------------
     c                   if        paRETP  = *blank
     c                   eval      x=36                                            : :
     c                   exsr      $Error                                          : :
     c                   else
     c                   if        paRETP  <> 'C' and paRETP <> 'M' and
     c                             paRETP  <> 'E' and paRETP <> 'L'
     c                   movel(p)  paRETP        EEDATA                            : :
     c                   eval      x=37                                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif

     c*---- Payment type -------------------------------------------------
     c                   if        paTYPE  = *blank
     c                   eval      x=38                                            : :
     c                   exsr      $Error                                          : :
     c                   else
     c                   if        paTYPE  <> 'P' and paTYPE <> 'M' and
     c                             paTYPE  <> 'V' and paTYPE <> 'R' and
     c                             paTYPE  <> 'A' and paTYPE <> 'S' and
     c                             paTYPE  <> 'Z'
     c                   movel(p)  paTYPE        EEDATA                            : :
     c                   eval      x=39                                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif

     c*---- Transaction code ---------------------------------------------
     c                   if        paTRCD  = *blank
     c                   eval      x=53                                            : :
     c                   exsr      $Error                                          : :
     c                   else
     c                   if        ValidTRCode(co#:paTRCD:cderrn:cdedsc) = *off
     c                   movel(p)  paTRCD        EEDATA                            : :
     c                   eval      x=54                                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif

     c*---- Reserve type (code)--(PARECD) --------------------------------
     c                   if        paRECD = *blank                               >-----+
     c                   eval      x=49                                            : :
     c                   exsr      $Error                                          : :
     c                   else                                                    <---+

     c                   if        paRECD <> *blank and                          >-----+
     c                             ValidResType(co#:paRECD:cderrn:cdedsc)        >-----+
     c                             = *off                                        >-----+
     c                   movel(p)  paRECD        EEDATA                            : :
     c                   eval      x=50                                            : :
     c                   exsr      $Error                                          : :
     c                   endif                                                   <---+
     c                   endif                                                   <---+

     c*---- Final comp payment flag ---------------------------------- ---
     c                   if        paFNAL <> 'Y' and paFNAL <> 'N'               >-----+
     c                   movel(p)  paFNAL        EEDATA                            : :
     c                   eval      x=51                                            : :
     c                   exsr      $Error                                          : :
     c                   endif                                                   <---+

     c*---- Office number -------------------------------------------- ---
     c                   if        paOFF# <> *blank and                          >-----+
     c                             ValidOffice(co#:pafnd:paoff#) = *off
     c                   movel(p)  paOFF#        EEDATA                            : :
     c                   eval      x=55                                            : :
     c                   exsr      $Error                                          : :
     c                   endif                                                   <---+

     c*---- Group payment flag --------------------------------------- ---
     c                   if        paPGTP <> ' ' and paPGTP <> 'G'               >-----+
     c                   movel(p)  paPGTP        EEDATA                            : :
     c                   eval      x=52                                            : :
     c                   exsr      $Error                                          : :
     c                   endif                                                   <---+

     c*---------------------------------------------------------------------
     c*---| ValiDates - all dates in Demographics array: errDATES |---------
     c                   exsr      $ValiDates                                      :
     c*---------------------------------------------------------------------

     c*---------------------------------------------------------------------
     c*---| ValiStaff- all staff codes in Claim Master array: errSTAF |-----
     c                   exsr      $ValiStaff                                      :
     c*---------------------------------------------------------------------

     c*------------| Individual field edits |------------------------------

     c*---- Time fields - (required) --------------------------------- ----
     c                   eval      xxtime = paTIME                                 : :
     c                   eval      D = 25                                          : :
     c                   exsr      $ValiTime                                       : :

     c                   eval      xxtime = paITIM                                 : :
     c                   eval      D = 26                                          : :
     c                   exsr      $ValiTime                                       : :

     c                   endsr                                                   <-+


     c*============================================= =======================
     c*------------| Dates Validation |-------------------------------------
     c*=====================================================================
     c*-- Validation for 10 dates in WMCDP in array errDATES;

     c     $ValiDates    begsr                                                   >-----+
     c                   do        10            D
     c                   eval      DTinvalid = *off                                : :
     c                   movel(p)  ' '           EETYPE                            : :

     c*         6 dates are required + 1 if compensation
     c                   if        D < 6 or D = 10 or                            >-----+
     c                             D = 6 and paRETP <> 'C'                       >-----+
     c                   if        errDATES(D) = *zero                           >-----+
     c                   eval      x = D + 10                                      : :
     c                   exsr      $Error                                          : :
     c                   eval      DTinvalid = *on                                 : :
     c                   endif                                                   <---+
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
     c********           exsr      $Error                                          : :
     c                   eval      DTinvalid = *on                                 : :
     c                   endif                                                   <---+
     c                   endif                                                   <---+
     c                   endif                                                   <---+

     c                   enddo                                                   <---+
     c                   endsr                                                   <---+

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
     c                   eval      x = D + 2
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif
     c                   endif
     c                   endsr                                                   <-+

     c*============================================= =======================

     c*=====================================================================
     c*  Change Fund Year
     c*=====================================================================
     c     $NewFYR       begsr                                                   >-----+
     c*
     c*---- ValidEmpYr - validate employer/division/fyr--------------------
     c                   if        ValidEmpYr(pafyr:cmemp#:cmdiv:EL_rrn:
     c                                        pafnd:paco#) = *off
     c                   eval      x=30                                            : :
     c                   movel(p)  pafyr         EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endsr                                                   <---+

     c*=====================================================================
     c*  Change of Group number
     c*=====================================================================
     c     $NewGrp       begsr                                                   >-----+
     c                   if        ValidGrp(pafnd) = *off
     c                   eval      x=18                                            : :
     c                   movel(p)  pafnd         EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endsr                                                   <---+


     c*=====================================================================
     c*  Validation for staff codes in WDPAP in array errSTAF.
     c*=====================================================================
     c     $ValiStaff    begsr                                                   >-----+
     c                   do        4             S
     c                   if        errSTAF(S) = *blank and S < 4                 >-----+
     c                   eval      x = S + 40                                      : :
     c                   exsr      $Error                                          : :
     c                   else
     c                   if        errSTAF(S) <> *blank and                      >-----+
     c                             GetUsrRRN(errSTAF(S))                         >-----+
     c                                 = *zero                                   >-----+
     c                   movel(p)  errSTAF(S)    EEDATA                            : :
     c                   eval      x = S + 43                                      : :
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

     c                   z-add     paFND         FND                               : :
     c                   z-add     paFYR         FYR                               : :
     c                   z-add     paCASE        CASE                              : :
     c                   z-add     paSEQ#        A50                               : :
     c                   movel(p)  paTRCD        TRCD                              : :
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
     c     EEKEY         cat       xxSEQ#:1      EEKEY                             : :

     c                   movel(p)  'WDPAP'       EEFILE                                :
     c                   z-add     x             EECODE                            : :
     c                   move(p)   errText(x)    xxFLD6                                :
     c                   movel(p)  xxFLD6        EEFLD                                 :

     c*     Offending application record: ID fields
     c                   movel(p)  PAPGM         ERPGM                                 :
     c                   movel(p)  PAUSER        ERUSER                                :
     c                   z-add     PADT          ERDT                              : :
     c                   z-add     PATIME        ERTIME                            : :

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
     c                   kfld                    paCO#
     c                   kfld                    pafnd
     c                   kfld                    pafyr
     c                   kfld                    pacase

     c     pdky          klist
     c                   kfld                    paFEIN
     c                   kfld                    paMOD

     c                   open      EDTPMTS

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

     c                   Do        61            x
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

     c                   close     EDTPMTS
     C                   exsr      CvtToPDF

     c                   endsr                                                   <-+
     c/copy wCpyEdtClm

** errText                      10 @ 35  (29 caption +6 fieldname)
Transaction date invalid     PATRDT        001
Posting date invalid         PAPODT        002
Service from date invalid    PAFRDT        003
Service to date invalid      PATODT        004
Date maintained invalid      PADT          005
Date received invalid        PARCVD        006
Date due invalid             PADUE         007
Dup pmt clear invalid        PADPCD        008
Reconciliation date invalid  PARCDT        009
Initial date invalid         PAIDT         010
Transaction date missing     PATRDT        011
Posting date missing         PAPODT        012
Service from date missing    PAFRDT        013
Service to date missing      PATODT        014
Date maintained missing      PADT          015
Date received missing        PARCVD        016
Case number missing          PADUE         017
Duplicate payment found      PASEQ#        018
Claim data not found         CLMKEY        019
Initial date missing         PAIDT         020
Amount missing               PAAMT         021
FEIN is missing              PAFEIN        022
FEIN/MOD invalid             PAFEIN        023
Record status invalid        PASTAT        024
Time maintained missing      PATIME        025
Initial time missing         PAITIM        026
Time maintained invalid      PATIME        027
Initial time invalid         PAITM         028
Group not valid              PAFND         029
Fund year is invalid         PAFYR         030
Employer not found           CMEMP#        031
Payment value 1 missing      PABR2         032
Payment value 1 invalid      PABR2         033
Payment value 2 missing      PABR7         034
Payment value 2 invalid      PABR7         035
Reserve type missing         PARETP        036
Reserve type invalid         PARETP        037
Payment type missing         PATYPE        038
Payment type invalid         PATYPE        039
xxxxxxxxtype invalid         XXXXXE        040
Maintenance user missing     PAUSER        041
Initial user missing         PAIUSR        042
Initial requestor missing    PARQST        043
Maintenance user invalid     PAUSER        044
Initial user invalid         PAIUSR        045
Initial requestor invalid    PARQST        046
Duplic clear user invalid    PADPCU        047
Sequence number missing      PASEQ#        048
Reserve code missing         PARECD        049
Reserve code invalid         PARECD        050
Final comp flag invalid      PAFNAL        051
Group flag invalid           PAPGTP        052
Transaction code missing     PATRCD        053
Transaction code invalid     PATRCD        054
Office number invalid        PAOFF#        055
xxxxxxxxxxxxxr               XXXXX         056
xxxxxxxxxxxxxr               XXXXX         057
xxxxxxxxxxxxxr               XXXXX         058
xxxxxxxxxxxxxr               XXXXX         059
xxxxxxxxxxxxxr               XXXXX         060
Company number skipped       PACO#         061
