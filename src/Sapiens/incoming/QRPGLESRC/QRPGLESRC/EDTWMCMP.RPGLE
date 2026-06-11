     h*--------------------------------------------------------------------*
     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     h*--------------------------------------------------------------------*
     h/title EDTWMCMP:  Edit Claim Master File - WMCMP
     h**********************************************************************
     h*  EDTWMCMP:  Edit Claim Master File - WMCMP.          C:58   P:SY058
     h**********************************************************************
     h option(*nodebugio : *srcstmt)                                                               *
     h debug(*yes)                                                                                 *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 07/13/10 C:235 Project: 114WWK                                                             *
      *                Add Recovery Option to Subro Drop Down                                      *
      *                CMSUBR flag='R' as a "Y" value                                              *
     *                                                                                            *
      * 10/29/08  C:33  Project: 114WPD                                                            *
      *                -Convert results to PDF                                                     *
      *=============================================================================================
     fWMCML1    ipe  e           k disk                                          Input file
     fWMCXL     if   e           k disk                                          Input file
     fWDF1L     if   e           k disk                                          Input file
     fWMCDL     if   e           k disk                                          Input file
     fWMPDL     if   e           k disk                                          Input file

     fEDTERR    o    e             disk
     fEDTWMCMPO o    e             printer oflind(*in71) usrOpn

     D/copy sPrRunCmd
     D/copy sprdate
     D/copy sprstring
     D/copy wpremplr
     D/copy wprgroup
     D/copy wprtable
     D/copy wprcdeval
     D/copy wprusrprms

     d*--------------------------------------------------------------------*
     D co#             s              3  0
     D cderrn          s              9  0
     D cdedsc          s             30
     D errText         s             35    dim(160) ctdata perrcd(1)
     d errCT           s              7p 0 dim(160)                                Index

     d*-------| Count Reserve Errors by number |----------------------------
     d rsvBadCode      s              5p 0 dim(10)                                 Index
     d rsvRemCls       s              5p 0 dim(10)                                 Index
     d rsvRemNeg       s              5p 0 dim(10)                                 Index
     d rsvPaidNeg      s              5p 0 dim(10)                                 Index
     d rsvRemNoC       s              5p 0 dim(10)                                 Index
     d rsvPaidNoC      s              5p 0 dim(10)                                 Index

     d*-------| Input Reserves Data Structures |------ 10 @ 1A, 9.2 --------
     d*---------| Code/amount Data Arrays |---------------------------------
     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)                              OUTSTAND RSV TYPE
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2

     d*---------| Constants |----------------------------------------------
     dcnTITLE          c                   const('PDS Claims - File Edit')       Title
     dcnTYPE           c                   const('Claim Master - WMCMP')         File (new/old)
     dgenERR01         c                   const('date prior to accident date')
     dgenERR02         c                   const('date after claimant death')
     dgenERR03         c                   const('future date invalid')

     d*---------| Initialization work area |-------------------------------
     D sys_date        s               d   datfmt(*usa)
     D sys_time        s               t   timfmt(*hms:)
     d dirPDF          s           1024    inz

     d*---------| Working storage |--------------------------------------
     dWork_Stor        ds                  inz                                   Work
     d  IX                            5p 0                                         Index
     d  xxCOUNT                       9p 0                                         Record counter
     d  xxERRS                        9p 0                                         Record counter
     d  xxACCTS                       9p 0                                         Print counter

     d*-------| Claim Key Data Structure |---------------------------------
     d                 ds                  inz                                   Work
     d  CLMkey                 1     40
     d  cmco#                  1      3  0
     d  cmfnd                  4      6  0
     d  cmfyr                  7      9  0
     d  cmcase                10     16  0

     d*-------| Accident time |-------------------------------------------
     d                 ds                  inz                                   Work
     d  cmactm                 1      6  0
     d  actmx                  1      6
     d  tmhrs                  1      2  0
     d  tmmin                  3      4  0
     d  tmsec                  5      6  0

     d*-------| Claim SS# to numeric |------------------------------------
     d                 ds                  inz                                   Work
     d  cmSS#                  1      9
     d  numSS#                 1      9  0

     d*-------| Claim employer # to char |--------------------------------
     d                 ds                  inz                                   Work
     d  CLMemp#                1     14
     d  cmemp#                 1      9  0
     d  cmdiv                 10     14  0
     d  xxemp#                 1      9
     d  xxdiv                 10     14

     d*-------| Input Staff Codes Data Structure |------ 9 @ 3A ------------
     d                 ds                  inz                                   Work
     derrSTAF                         3A   dim(9)                                  Index
     d  cmsupr                 1      3
     d  cmclex                 4      6
     d  cmfead                 7      9
     d  cmaman                10     12
     d  cmcec                 13     15
     d  cmnrse                16     18
     d  cmesup                19     21
     d  cmuman                22     24
     d  cmsdfa                25     27

     d*-------| Input Flags Data Structure |----------- 25 @ 1A ------------
      *---------- fields w/binary (Y/N) values  (plus blank for convenience)

     d                 ds                  inz                                   Work
     derrFLAGS                        1A   dim(25)                                 Index
     d  cmplfl                 1      1
     d  cmeadr                 2      2
     d  cmprex                 3      3
     d  cmgt1                  4      4
     d  cmosha                 5      5
     d  cmdny                  6      6
     d  cmcvfl                 7      7
     d  cmsubr                 8      8
     d  cmacqr                 9      9
     d  cmsdf                 10     10
     d  cmmco                 11     11
     d  cmodis                12     12
     d  cmhcap                13     13
     d  cmdth                 14     14
     d  cmpoi                 15     15
     d  cmfsal                16     16
     d  cmagcp                17     17
     d  cmvorc                18     18
     d  cmpeff                19     19
     d  cmueff                20     20
     d  cmssff                21     21
     d  cmotff                22     22
     d  cmsfff                23     23
     d  cmexcs                24     24
     d  cmeagr                25     25

     d*-------| Input Dates Data Structure |----------- 26 @ 7 0 -----------
     d                 ds                  inz                                   Work
     derrDATES                        7  0 dim(26)                                 Index
     d  cmacdt                 1      7  0
     d  cmdtrp                 8     14  0
     d  cmdt                  15     21  0
     d  cmentd                22     28  0
     d  cmrcvd                29     35  0
     d  cmidt                 36     42  0
     d  cmdbdt                43     49  0
     d  cmdtlw                50     56  0
     d  cmdtic                57     63  0
     d  cmdtre                64     70  0

     d  cmhidt                71     77  0
     d  cmsgdt                78     84  0
     d  cmrlw                 85     91  0
     d  cmmmi                 92     98  0
     d  cmdod                 99    105  0
     d  cmfsdt               106    112  0
     d  cmertw               113    119  0
     d  cmrtw                120    126  0
     d  cmlsdt               127    133  0
     d  cmddat               134    140  0

     d  cmtpad               141    147  0
     d  cmcmdt               148    154  0
     d  cmgref               155    161  0
     d  cmntdt               162    168  0
     d  cmdyef               169    175  0
     d  cmdyrc               176    182  0

     d*-------| x Error Code Data Structure |----------------------------
     d                 ds                  inz                                   Work
     d  xxx                    1      4a
     d  xz                     1      1
     d  x0                     2      2
     d  x                      2      4  0

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
     D xxFM            s             12S 0                                         Record counter
     D*                                                                -----
     D kkfnd           s              3P 0
     D kkFEIN          s                   like(PDFEIN)
     D kkMOD           s                   like(PDMOD)
     D*                                                                -----
     D xxFLD6          s              6                                                :
     D F               s              3  0
     D S               s              3  0
     D R               s              2  0
     D D               s              3  0
     D doDetPrint      s                   like(*in60)
     D DTinvalid       s                   like(*in60)
     D #@co#           s              3
     D ##emp#          s              9
     D rrTEST          s              9  0 inz(0)
     D CD_rrn          s              9  0
     D EM_rrn          s              9  0
     D EL_rrn          s              9  0
     D WKdate8         s              8  0
     D SYSnum7         s              7  0

     D XXCO#           s              3  0
     D XXFND           s              3  0
     D XXFYR           s              3  0
     D XXSTAT          s              1
     D KCON01          c                   const('EDIT CLAIM MASTER FILE - +
     D                                            WMCMP')
     D*---------------------------------------------------------------------
     D* Stand Alone Fields - BOTTOM
     D*---------------------------------------------------------------------
     D psds          esds                  extname(szq1p)

     i*_____________________________________________________________________
     i*---------| Define Record ID indicator |------------------------------
     ipcm01         50                                                           Work
     i*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


     c*------------------ ==  Mainline processing == ----------------------
     c*    -- Got an input record.
     c                   if        *in50=*on                                     >-+

     c*    -- Count bad CO#                     (no CO# break done)
     c                   if        cmCO#<>pmCO#                                  >-+ :
     c                   add       1             errCT(41)                         : :
     c                   else                                                    <-+ :

     c                   eval      xxCOUNT=xxCOUNT+1                                 :

     c                   eval      doDetPrint=*off                                 : :
     c                   movel     ' '           xz                                    :
     c                   CLEAR                   EER01                                   :

     c                   if        xxCOUNT=1                                     >-+ :
     c                   eval      xxCO#=cmCO#                                     : :
     c*****              eval      xxFND=cmFND                                     : :
     c                   endif                                                   <-+ :


     c*    -- Break level 1
     c                   if        xxFND<>cmFND                                  >-+ :
     c                   exsr      $NewGroup                                       : :

     c                   eval      xxCO#=cmCO#                                     : :
     c                   eval      xxFND=cmFND                                     : :

     c                   endif                                                   <-+

     c*================================================================---
     c*---------| Process record field edits |----------------------------
     c                   if        CMCO# = pmCO#                                 >-+ :
     c                   exsr      $Process                                        :
     c                   endif                                                   <-+

     c*================================================================---
     c*---------| End of record process - print detail |------------------
     c                   exsr      $Write_D1                                       :

     c                   endif                                                   <-+ :
     c                   endif                                                   <-+

     c*================================================================---
     c*---------| End of primary file |-----------------------------------
     clr                 exsr      $Finish                                         :
     c*================================================================---

     c*===================================================================*
     c*------------| Process a record subroutine |-------------------------
     c     $Process      begsr                                                   >---+

     c*------------| Individual field edits |------------------------------

     c*---- ValidEmp - validate employer/division--------------------------
     c                   if        ValidEmp(cmemp#:cmdiv:EM_rrn:cmfnd:cmco#)
     c                              = *off
     c                   eval      x=44                                            : :
     c                   eval      EEDATA = xxemp# + ' / ' + xxdiv
     c                   exsr      $Error                                          : :
     c                   endif
     c*
     c*---- ValidEmpYr - validate employer/division/fyr--------------------
     c                   if        ValidEmpYr(cmfyr:cmemp#:cmdiv:EL_rrn:
     c                                        cmfnd:cmco#) = *off
     c                   eval      x=45                                            : :
     c                   movel(p)  cmfyr         EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c*
     c*---- Case number is required ---------------------------------------
     c                   if        cmcase = *zero
     c                   eval      x=46                                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c*
     c*---- Location number if required, if present --------------------
     c                   if        *in61 = *off and CXLOCT = 'Y' and
     c                             cmctg1 = *blank
     c                   eval      x=83                                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmctg1 <> *blank and
     c                             ValidLoc(co#:cmfnd:cmemp#:cmdiv:cmctg1)
     c                             = *off                                          : :
     c                   eval      x=84                                            : :
     c                   movel(p)  cmctg1        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*---- Social security number required -------------------------------
     C                   eval      *in44 = *off
     c                   testn                   cmss#                44
     c                   if        *IN44 = *on and
     c                             numSS# = *zero or
     c                             cmSS# = *blank
     c                   eval      x=47                                            : :
     c                   exsr      $Error                                          : :

     c                   else
     c     cmSS#         chain     pcd01                              64
     c     *IN64         ifeq      *on
     c                   eval      x=92                                            : :
     c                   movel(p)  CMSS#         EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif
     c                   if        *IN44 = *on
     c                   eval      kkFEIN = numSS#                                 : :
     c                   eval      kkMOD = *zero                                   : :
     c     pdkey         chain     ppd01                              64
     c     *IN64         ifeq      *off
     c     PD1099        andeq     'Y'
     c                   eval      x=93                                            : :
     c                   movel(p)  CMSS#         EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif


     c*---- Class code if required, or if present ------------------------
     c                   if        *in62 = *off and F1RLXC <> 'Y' and
     c                             cmclas = *zero
     c                   eval      x=86                                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmclas <> *zero and
     c                             ValidClas(co#:cmfnd:cmfyr:cmemp#:cmdiv: +
     c                             cmclas:cmspec:cmacdt:cderrn) = *off
     c                   eval      x=87                                            : :
     c                   movel(p)  CLMkey        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c*
     c*---- Status must be O or C ----------------------------------------
     c                   if        cmstat <> 'O' and cmstat <> 'C'
     c                   eval      x=88                                            : :
     c                   movel(p)  cmstat        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c*
     c*---- Verify Hospital FEIN if present |-----------------------------
     c                   if        cmhosf <> *zero
     c                   eval      kkFEIN = cmhosf                                 : :
     c                   eval      kkMOD = cmhosm                                  : :
     c     pdkey         chain     ppd01                              63
     c                   if        *in63 = *on
     c                   eval      x=89                                            : :
     c                   movel(p)  cmhosf        xxFM                              : :
     c                   move      cmhosm        xxFM                              : :
     c                   movel(p)  xxFM          EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif

     c*---- Jurisdiction State --------------------------------------------
     c                   if        ValidJurSt(co#:cmJRST:cderrn:cdedsc) = *off
     c                   eval      x=68                                            : :
     c                   movel(p)  cmJRST        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif


     c*---- Office number -------------------------------------------- ----
     c                   if        cmOFF# = *blank
     c                   eval      x=94                                            : :
     c                   exsr      $Error                                          : :
     c                   else

     c                   if        ValidOffice(co#:cmfnd:cmoff#) = *off
     c                   movel(p)  cmOFF#        EEDATA                            : :
     c                   eval      x=95                                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif

     c*---- Occupation code ------------------------------------------ ----
     c                   if        cmOCCD <> *blank and
     c                             ValidOccup(co#:cmOCCD) = *off
     c                   movel(p)  cmOCCD        EEDATA                            : :
     c                   eval      x=96                                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*---- Department ----------------------------------------------- ----
     c                   if        cmDEPT <> *blank and
     c                             ValidDept(co#:cmfnd:cmemp#:cmdiv:cmdept)
     c                             = *off                                          : :
     c                   movel(p)  cmDEPT        EEDATA                            : :
     c                   eval      x=97                                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*---- Days must be 0-7 ----------------------------------------------
     c                   if        cmDAYS > 7
     c                   eval      x=50                                            : :
     c                   movel(p)  cmDAYS        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*---- Accident time - (if present) ----------------------------------
     c                   if        cmACTM <> *zero
     c*           Test numeric                                         ----
     C                   eval      *in41 = *off
     c                   testn                   actmx                41
     c                   if        *IN41 = *off and
     c                             actmx <> *blank or

     c                             *IN41 = *on and
     c                             CMACTM <> *zero

     c*           Must be numeric                                      ----
     c                   if        *in41 = *off or

     c*           Hours must be 0-24                                   ----
     c                             tmhrs > 24 or

     c*           Minutes must be 00 - 60.                              ----
     c                             tmmin > 60 or

     c*           Seconds must be 00 - 60                             ----
     c                             tmsec > 60

     c                   eval      x=49                                            : :
     c                   movel(p)  cmtime        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif
     c                   endif

     c*---- Class code  >>> Where get RTCD for key ???? to WMPRP-----------
     c*---------------------------------------------------------------------

     c*---------------------------------------------------------------------
     c*----| Accident location data |--------------------------------- ----
     c     cmacnm        ifeq      *blank                                          : :
     c                   eval      x=71                                            : :
     c                   movel(p)  cmacnm        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif                                                     : :
     c     cmacdr        ifeq      *blank                                          : :
     c                   eval      x=72                                            : :
     c                   movel(p)  cmacdr        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif                                                     : :
     c     cmacty        ifeq      *blank                                          : :
     c                   eval      x=73                                            : :
     c                   movel(p)  cmacty        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif                                                     : :
     c     cmast         ifeq      *blank                                          : :
     c                   eval      x=74                                            : :
     c                   exsr      $Error                                          : :
     c                   else                                                      : :
     c                   if        ValidState(cmast) = *off
     c                   eval      x=91                                            : :
     c                   movel(p)  cmast         EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif                                                     : :
     c     cmacz1        ifeq      *zero                                           : :
     c                   eval      x=75                                            : :
     c                   movel(p)  cmacz1        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif                                                     : :

     c*---------------------------------------------------------------------
     c*---| ValiDates - all dates in Claim Master array: errDATES |---------
     c                   exsr      $ValiDates                                      :
     c*---------------------------------------------------------------------

     c*---------------------------------------------------------------------
     c*---| ValiCodes - codes in Claim Master |-------------------------
     c                   exsr      $ValiCodes                                      :
     c*---------------------------------------------------------------------

     c*---------------------------------------------------------------------
     c*---| ValiFlags - all flags in Claim Master array: errFLAGS |---------
     c                   exsr      $ValiFlags                                      :
     c*---------------------------------------------------------------------

     c*---------------------------------------------------------------------
     c*---| ValiStaff- all staff codes in Claim Master array: errSTAF |-----
     c                   exsr      $ValiStaff                                      :
     c*---------------------------------------------------------------------

     c*---------------------------------------------------------------------
     c*---| ValiResv - all reserve fields in Claim Master |-----------------
     c*---|   Arrays: CMT, CMA, CMR |---------------------------------------
     c                   exsr      $ValiResv                                       :
     c*---------------------------------------------------------------------

     c                   endsr                                                   <---+

     c*============================================= =======================
     c*------------| Dates Validation |-------------------------------------
     c*=====================================================================
     c*-- Validation for 26 dates in WMCMP in array errDATES
     c*     First 6 dates are required; 7th required if LTFL = 'I'
     c*       o use error codes 31-37 if required date is missing/0. (+30)

     c*     Remaining dates validated only if not zero.
     c*       o use error codes 1-26 if date is invalid.
     c*       o use codes 01-26 also if relationship error, BUT in addition
     c*           use type(EETYPE) F, A, or D. (Future, Accident, Death).
     c*               (codes 27-30 and 38-40 not used)

     c     $ValiDates    begsr                                                   >-----+
     c                   do        26            D
     c                   eval      DTinvalid = *off                                : :
     c                   movel(p)  ' '           EETYPE                            : :

     c                   if        D<7 and errDATES(D)=0 or                      >-----+
     c                             D=7 and cmltfl='I' and                        >-----+
     c                             errDATES(D)=0                                 >-----+
     c                   eval      x= D+30                                         : :
     c                   exsr      $Error                                          : :
     c                   eval      DTinvalid = *on                                 : :
     c                   eval      x= x-30                                         : :
     c                   else                                                    <---+

     c*      Otherwise, validate only if not zero.
     c                   if        errDATES(D) <> 0                              >-----+
     c                   eval      WKdate8 = dtMDYY(errDATES(D))

     c*---------------------------------------------------------------------
     c*      Invalid date.
     c     WKdate8       ifeq      *zero                                         >-----+
     c                   eval      x=D                                             : :
     c                   movel(p)  errDATES(D)   EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   eval      DTinvalid = *on                                 : :
     c                   else                                                    <---+

     c*---------------------------------------------------------------------
     c*    Date is valid: check relationship errors.
     c*           a. Future date.
     c                   if        D<9 and errDATES(D) > SYSnum7 or              >-----+
     c                             D=11 and errDATES(D) > SYSnum7 or             >-----+
     c                             D=15 and errDATES(D) > SYSnum7 or             >-----+
     c                             D=19 and errDATES(D) > SYSnum7 or             >-----+
     c                             D>20 and errDATES(D) > SYSnum7                >-----+
     c                   movel(p)  errDATES(D)   EEDATA                            : :
     c                   movel(p)  'F'           EETYPE                            : :
     c                   eval      x=D                                             : :
     c                   exsr      $Error                                          : :
     c                   eval      DTinvalid = *on                                 : :
     c                   endif                                                   <---+

     c*           b. Before accident date.
     c                   if        D>1 and D <> 11 and D <> 19 and               >-----+
     c                             errDATES(D) < cmacdt                          >-----+
     c                   movel(p)  errDATES(D)   EEDATA                            : :
     c                   movel(p)  'A'           EETYPE                            : :
     c                   eval      x=D                                             : :
     c                   exsr      $Error                                          : :
     c                   eval      DTinvalid = *on                                 : :
     c                   endif                                                   <---+

     c*           c. After claimant death.
     c                   if        D<>1 and D<>2 and D<>3 and D<>4 and D<>5 and
     c                             D<>6 and D<>11 and D<>15 and D<>19 and
     c                             D<>20 and CMDOD <> 0 and errDATES(D) > CMDOD
     c                   movel(p)  'D'           EETYPE                            : :
     c                   eval      x=D                                             : :
     c                   exsr      $Error                                          : :
     c                   eval      DTinvalid = *on                                 : :
     c                   endif                                                   <---+

     c                   endif                                                   <---+
     c                   endif                                                   <---+
     c                   endif                                                   <---+
     c                   enddo                                                   <---+

     c                   endsr                                                   <---+

     c*=====================================================================
     c*--------------| Validate Table codes |---WTABL-----------------------
     c*=====================================================================
     c     $ValiCodes    begsr                                                   >-----+
     c                   if        ValidNatr(co#:cmnatr:cderrn:cdedsc) = *off
     c                   eval      x=51                                            : :
     c                   movel(p)  cmnatr        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        ValidPart(co#:cmpart:cderrn:cdedsc) = *off
     c                   eval      x=52                                            : :
     c                   movel(p)  cmpart        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        ValidCause(co#:cmcaus:cderrn:cdedsc) = *off
     c                   eval      x=53                                            : :
     c                   movel(p)  cmcaus        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*=====================================================================
     c*--------------| Generic Table codes  |---WTGTL-----------------------

     c*---| Lost time flag must be M, I or R |-----------------------------
     c                   if        ValidLostT(co#:cmltfl:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=48                                            : :
     c                   movel(p)  cmltfl        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmctry <> *blank and
     c                             ValidCntry(co#:cmctry:cderrn:cdedsc) = *off
     c                   eval      x=54                                            : :
     c                   movel(p)  cmctry        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        ValidNCCI(co#:cmncci:cmjrst:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=55                                            : :
     c                   movel(p)  cmNCCI        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmSIC <> *blank and
     c                             ValidSICCde(co#:cmsic:cderrn:cdedsc)
     c                             = *off
     c                   movel(p)  cmSIC         EEDATA                            : :
     c                   eval      x=98                                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmmcon <> *blank and
     c                             ValidMethC(co#:cmmcon:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=56                                            : :
     c                   movel(p)  cmmcon        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmests <> *blank and
     c                             ValidEmpSt(co#:cmests:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=57                                            : :
     c                   movel(p)  cmests        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmshft <> *blank and
     c                             ValidShift(co#:cmshft:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=58                                            : :
     c                   movel(p)  cmshft        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmacst <> *blank and
     c                             ValidActSt(co#:cmacst:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=59                                            : :
     c                   movel(p)  cmacst        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        ValidLossC(co#:cmlscv:cmjrst:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=60                                            : :
     c                   movel(p)  cmlscv        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmlsrv <> *blank and
     c                             ValidLossR(co#:cmlsrv:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=61                                            : :
     c                   movel(p)  cmlsrv        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmmco = 'Y' and
     c                             ValidMCOtyp(co#:cmmctp:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=62                                            : :
     c                   movel(p)  cmmctp        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmitrt <> *blank and
     c                             ValidInitTr(co#:cmitrt:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=63                                            : :
     c                   movel(p)  cmitrt        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmwmth <> *blank and
     c                             ValidMethPIW(co#:cmwmth:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=64                                            : :
     c                   movel(p)  cmwmth        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmwgpd <> *blank and
     c                             ValidFWageP(co#:cmwgpd:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=65                                            : :
     c                   movel(p)  cmwgpd        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmagcd <> *blank and
     c                             ValidAgrComp(co#:cmagcd:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=66                                            : :
     c                   movel(p)  cmagcd        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmcovt <> *blank and
     c                             ValidCovT(co#:cmcovt:cmjrst:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=67                                            : :
     c                   movel(p)  cmcovt        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cmrtwq <> *blank and
     c                             ValidRTWQual(co#:cmrtwq:cderrn:cdedsc)
     c                             = *off
     c                   eval      x=69                                            : :
     c                   movel(p)  cmrtwq        EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   endsr                                                   <---+

     c*============================================= =======================
     c*------------| Flags Validation |-------------------------------------
     c*=====================================================================
     c*-- Validation for 24 flags in WMCMP in array errFLAGS.
     c*    o Flags must be 'N' or blank; or 'Y': otherwise, error
     c*    o Process elements 1-24 using index 'F'
     c*    o Add F + 100 = error numbers 101-125 (index x)

     c     $ValiFlags    begsr                                                   >-----+
     c                   do        25            F
     c                   if        errFLAGS(F) <> *blank and                     >-----+
     c                             errFLAGS(F) <> 'Y' and                        >-----+
     c******BEGIN ADD***07/13/10********************************************
     c                             errFLAGS(F) <> 'R' and                        >-----+
     c******END   ADD***07/13/10********************************************
     c                             errFLAGS(F) <> 'N'                            >-----+
     c                   eval      x = F + 100                                     : :
     c                   movel(p)  errFLAGS(F)   EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   eval      x= x-100                                        : :
     c                   endif
     c                   enddo

     c                   endsr                                                   <---+

     c*=====================================================================
     c*  Validation for 9 staff codes in WMCMP in array errSTAF.
     c*=====================================================================
     c     $ValiStaff    begsr                                                   >-----+

     c                   do        9             S
     c                   if        errSTAF(S) = *blank and S = 1 or              >-----+
     c                             errSTAF(S) = *blank and S = 2 or              >-----+
     c                             errSTAF(S) = *blank and S = 5                 >-----+
     c                   movel(p)  errSTAF(S)    EEDATA                            : :
     c                   eval      x = S + 144                                     : :
     c                   exsr      $Error                                          : :
     c                   eval      x= x-144                                        : :
     c                   else
     c                   if        errSTAF(S) <> *blank                          >-----+
     c                   if        GetUserID(errSTAF(S)) = *blank                >-----+
     c                   movel(p)  errSTAF(S)    EEDATA                            : :
     c                   eval      x = S + 130                                     : :
     c                   exsr      $Error                                          : :
     c                   eval      x= x-130                                        : :
     c                   endif
     c                   endif
     c                   endif
     c                   enddo                                                   <---+
     c                   endsr                                                   <---+

     c*=====================================================================
     c*  Validation for Reserve data in WMCMP arrays.
     c*=====================================================================
     c     $ValiResv     begsr                                                   >-----+

     c                   do        10            R
     c                   if        CMT(R) <> *blank and                          >-----+
     c                             ValidResType(co#:CMT(R):cderrn:cdedsc)        >-----+
     c                             = *off                                        >-----+
     c                   eval      x=76                                            : :
     c                   movel(p)  cmt(R)        EEDATA                            : :
     c                   move      R             EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   add       1             rsvBadCode(R)
     c                   endif                                                   <---+
     c                   if        CMSTAT = 'C' and CMR(R) <> *zero              >-----+
     c                   eval      x=77                                            : :
     c                   movel(p)  cmr(R)        EEDATA                            : :
     c                   move      R             EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   add       1             rsvRemCls(R)
     c                   endif                                                   <---+
     c                   if        CMR(R) < *zero                                >-----+
     c                   eval      x=78                                            : :
     c                   movel(p)  cmr(R)        EEDATA                            : :
     c                   move      R             EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   add       1             rsvRemNeg(R)
     c                   endif                                                   <---+
     c                   if        CMA(R) < *zero                                >-----+
     c                   eval      x=79                                            : :
     c                   movel(p)  cma(R)        EEDATA                            : :
     c                   move      R             EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   add       1             rsvPaidNeg(R)
     c                   endif                                                   <---+
     c                   if        CMR(R) <> *zero and CMT(R) = *blank           >-----+
     c                   eval      x=80                                            : :
     c                   movel(p)  cmr(R)        EEDATA                            : :
     c                   move      R             EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   add       1             rsvRemNoC(R)
     c                   endif                                                   <---+
     c                   if        CMA(R) <> *zero and CMT(R) = *blank           >-----+
     c                   eval      x=81                                            : :
     c                   movel(p)  cma(R)        EEDATA                            : :
     c                   move      R             EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   add       1             rsvPaidNoC(R)
     c                   endif                                                   <---+

     c                   enddo                                                   <---+

     c                   endsr                                                   <---+

     c*=====================================================================
     c*------------| New Group Processing |--------------------------------
     c*=====================================================================
     c     $NewGroup     begsr                                                   >-----+

     c*---- ValidGrp - validate group--------------------------------------
     c                   if        cmfnd = *zero
     c                   eval      x=42                                            : :
     c                   exsr      $Error                                          : :

     c                   else
     c                   if        ValidGrp(cmfnd) = *off
     c                   eval      x=43                                            : :
     c                   movel(p)  cmfnd         EEDATA                            : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif                                                   <-+

     c*------------| Get DART/claim control file for group |---------------
     c                   eval      kkfnd = cmfnd                                   : :
     c     cxky          chain     pcx01                              61
     c     *in61         ifeq      *on
     c                   eval      CXLOCT = 'N'                                    : :
     c                   eval      x=82                                            : :
     c                   exsr      $Error                                          : :
     c                   endif                                                   <-+

     c*------------| Get Fund Detail file for group/year |------------------
     c                   eval      kkfnd = cmfnd                                   : :
     c     f1ky          chain     pf101                              62
     c     *in62         ifeq      *on
     c                   eval      F1RLXC = 'Y'                                    : :
     c                   eval      x=85                                            : :
     c                   exsr      $Error                                          : :
     c                   endif                                                   <-+

     c                   endsr                                                   <---+

     c*=====================================================================
     c*------------| Error subroutine |-------------------------------------
     c*=====================================================================
     c     $Error        begsr                                                   >-+
     c                   ADD       1             errCT(x)                              :
     c                   if        x < 99
     c                   move      ' '           x0                                    :
     c                   endif                                                   <-----+
     c     PeList        cat       xxx:0         Pelist                                :
     c                   movel     ','           xz                                    :

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

     c                   movel(p)  'WMCMP'       EEFILE                                :
     c                   z-add     x             EECODE                            : :
     c                   move(p)   errText(x)    xxFLD6                                :
     c                   movel(p)  xxFLD6        EEFLD                                 :

     c*     Offending application record: ID fields
     c                   movel(p)  CMPGM         ERPGM                                 :
     c                   movel(p)  CMUSER        ERUSER                                :
     c                   z-add     CMDT          ERDT                              : :
     c                   z-add     CMTIME        ERTIME                            : :
     c                   movel(p)  CMIPGM        ERIPGM                                :
     c                   movel(p)  CMIUSR        ERIUSR                                :
     c                   z-add     CMIDT         ERIDT                             : :
     c                   z-add     CMITM         ERITIM                            : :

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
     c                   if        *in71 = *on
     c                   write     HEAD                                            : :
     c                   write     HED0                                            : :
     c                   eval      *in71=*off                                      : :
     c                   endif                                                   <-----+
     c                   write     PDET1                                           :
     c                   endif                                                   <-----+
     c                   eval      PeList=*blank                                     :
     c                   eval      xxACCTS=xxACCTS+1                                   :
     c                   eval      doDetPrint=*off                                 : :
     c                   endsr                                                   <-+

     c*------------| Print total line subroutine |------------------------
     c     $Write_Tl     begsr                                                   >-+
     c                   if        *in71 = *on
     c                   write     HEAD                                            : :
     c                   write     HED0                                            : :
     c                   eval      *in71=*off                                      : :
     c                   endif                                                   <-----+
     c                   endsr                                                   <-+

     c*---------< Initialization Subroutine >-----------------------------
     c     *inzsr        begsr                                                   >-+

     c*---------< Program entry >-----------------------------------------
     c     *entry        plist
     c                   parm                    pmRTCD            1
     c                   parm                    pmCO              3
     c                   parm                    pmLIBRARY        10

     C*  Company file/fund (group) retrieve program parameter list
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam

      *=============================================================================================
      *  Klists
      *=============================================================================================
     c     cxky          klist
     c                   kfld                    ##CO#
     c                   kfld                    kkfnd

     c     pdkey         klist
     c                   kfld                    kkFEIN
     c                   kfld                    kkMOD

     c     f1ky          klist
     c                   kfld                    ##CO#
     c                   kfld                    cmfnd
     c                   kfld                    cmfyr

     c                   open      EDTWMCMPO

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
     c***                eval      xxCO#=##co#                                     : :
     c                   eval      xxFND=##fnd                                     : :
     c                   eval      CONAME = ##conm

     c                   eval      PTNAME = (KCON01)
     c                   eval      PTNAME = centerFld(PTNAME)

     c                   write     HEAD                                            : :
     c                   write     HED0                                            : :
     c                   eval      *in71=*off                                      :

     c                   endsr                                                   <-+
     c*-------------------------------------------------------------------

     c*------------< End of job  subroutine >-----------------------------
     c     $Finish       begsr                                                   >-+
     c                   eval      cmCO#=*loval                                    :
     c                   eval      cmFND=*loval                                    :
     c                   eval      cmFYR=*loval                                    :
     c                   exsr      $Write_Tl                                       :

     c*---  Print Error Summary
     c                   write     HEAD                                            : :
     c                   write     HEDerr                                          : :

     c                   Do        160           x
     c                   if        errCT(x) <> 0                                   : :

     c                   movel(p)  errtext(x)    PERRTXT                           :
     c                   move(p)   errtext(x)    PERRFIELD                         :

     c*---  Print summary line ------------------------------------------
     c*       If reserve error, print detail totals    #76-81
     c                   if        X >= 76 and X <= 81                           <-+
     c                   z-add     *zero         PERRCT                            : :
     c                   Do        10            R

     c                   select                                                  <-+
     c     x             wheneq    76                                            <-+
     c                   add       rsvBadCode(R) PERRCT                            : :
     c                   movel     CMT(R)        PERRTXT                               :
     c     x             wheneq    77                                            <-+
     c                   add       rsvRemCls(R)  PERRCT                            : :
     c     x             wheneq    78                                            <-+
     c                   add       rsvRemNeg(R)  PERRCT                            : :
     c                   movel     CMT(R)        PERRTXT                               :
     c     x             wheneq    79                                            <-+
     c                   add       rsvPaidNeg(R) PERRCT                            : :
     c                   movel     CMT(R)        PERRTXT                               :
     c     x             wheneq    80                                            <-+
     c                   add       rsvRemNoC(R)  PERRCT                            : :
     c     x             wheneq    81                                            <-+
     c                   add       rsvPaidNoC(R) PERRCT                            : :
     c                   endsl                                                   <-+

     c                   if        PERRCT <> *zero                               <-+
     C                   if        *in71 = *on
     c                   write     HEAD                                            : :
     c                   write     HEDerr                                          : :
     c                   eval      *in71=*off                                      : :
     c                   endif                                                   <-+
     c                   move      R             PERRFIELD                             :
     c                   move      R             PERRTXT                               :
     c                   write     errLIN                                          : :
     c                   move      '00'          PERRTXT                               :
     c                   move      '00'          PERRFIELD                             :
     c                   endif                                                   <-+

     c                   enddo                                                   <-+

     C                   if        *in71 = *on
     c                   write     HEAD                                            : :
     c                   write     HEDerr                                          : :
     c                   eval      *in71=*off                                      : :
     c                   endif                                                   <-+
     c                   z-add     errCT(x)      PERRCT                            : :
     c                   write     errLIN                                          : :

     c                   else                                                    <-+
     C                   if        *in71 = *on
     c                   write     HEAD                                            : :
     c                   write     HEDerr                                          : :
     c                   eval      *in71=*off                                      : :
     c                   endif                                                   <-----+
     c                   z-add     errCT(x)      PERRCT                            : :
     c                   write     errLIN                                          : :
     c                   endif                                                   <-+

     c                   endif                                                   <-+
     c                   enddo                                                   <-+
     c*---  End  line ------------------------------------------------------

     c                   write     HEAD                                            : :
     c                   write     PRTTOT                                          :

     c                   close     EDTWMCMPO
     C                   exsr      CvtToPDF

     c                   endsr                                                   <-+
     c/copy wCpyEdtClm
     c*-------------------------------------------------------------------
** errText                     160 @ 35  (29 caption +6 fieldname)
Accident date invalid        CMACDT        001
Date reported invalid        CMDTRP        002
Date maintained invalid      CMDT          003
Entry date invalid           CMENTD        004
Date received invalid        CMRCVD        005
Initial date invalid         CMIDT         006
Disability date invalid      CMDBDT        007
Date last worked invalid     CMDTLW        008
Initial contact date invalid CMDTIC        009
Rehab assigned date invalid  CMDTRE        010
Hire date invalid            CMHIDT        011
Surgery date invalid         CMSGDT        012
Released to work dt invalid  CMRLW         013
Max med improve dt invalid   CMMMI         014
Date of death invalid        CMDOD         015
Full salary paid dt invalid  CMFSDT        016
Est return work dt invalid   CMERTW        017
Return to work date invalid  CMRTW         018
Days lost update dt invalid  CMLSDT        019
Destruction date invalid     CMDDAT        020
TPA contact date invalid     CMTPAD        021
Commuted date invalid        CMCMDT        022
Gross wkly eff date invalid  CMGREF        023
Net weekly amt date invalid  CMNTDT        024
Denial eff date invalid      CMDYEF        025
Denial recission dt invalid  CMDYRC        026
xxxxxxxxx      027           XXXXXX        027
xxxxxxxxx      028           XXXXXX        028
xxxxxxxxx      029           XXXXXX        029
xxxxxxxxx      030           XXXXXX        030
Accident date missing        CMACDT        031
Date reported missing        CMDTRP        032
Date maintained missing      CMDT          033
Entry date missing           CMENTD        034
Date received missing        CMRCVD        035
Initial date missing         CMIDT         036
Disability date missing      CMDBDT        037
xxxxxxxxx      038           XXXXXX        038
xxxxxxxxx      039           XXXXXX        039
xxxxxxxxx      040           XXXXXX        040
Invalid company number       CMCO#         041
Group number missing         CMFND         042
Invalid group number         CMFND         043
Invalid employer/division    CMEMP#        044
Invalid employer year        CMFYR         045
Case number missing          CMCASE        046
Social Security missing      CMSS#         047
Lost time flag not M,I, or R CMLTFL        048
Invalid time                 CMACTM        049
Days must be 1 - 7           CMDAYS        050
Nature code is invalid       CMNATR        051
Part code is invalid         CMPART        052
Cause code is invalid        CMCAUS        053
Country code invalid         CMCTRY        054
NCCI code is invalid         CMNCCI        055
Method of contact invalid    CMMCON        056
Employment status invalid    CMESTS        057
Shift code is invalid        CMSHFT        058
Activity status code invalid CMACST        059
Loss coverage invalid        CMLSCV        060
Loss recovery type invalid   CMLSRV        061
MC organization type invalid CMMCTP        062
Initial treatment invalid    CMITRT        063
Determn pre-inj wage invalid CMWMTH        064
Wage period invalid          CMWGPD        065
Agree/compensate code invalidCMAGCD        066
Coverage type is invalid     CMCOVT        067
Jurisdiction state invalid   CMJRST        068
Return/work qualifier invalidCMRTWQ        069
Determn pre-inj wage invalid CMWMTH        070
Accident location missing    CMACNM        071
Accident address is missing  CMACDR        072
Accident city is missing     CMACTY        073
Accident state missing       CMAST         074
Accident zip code missing    CMACZ1        075
   reserve type invalid      CMRT00        076
Closed with remaining resv   CMRT00        077
XX negative paid amount      CMRT00        078
XX negative remaining amt    CMRT00        079
Paid amt but type missing    CMRT00        080
Remaining amt/type missing   CMRT00        081
DART/Clm control not found   WMCXP         082
Location code missing        CMCTG1        083
Invalid location code        CMCTG1        084
Fund detail not found        WDF1L         085
Required class code missing  CMCLAS        086
Class code is invalid        CMCLAS        087
Status not O or C            CMSTAT        088
Hospital FEIN/MOD not found  CMHOSF        089
Hospital state invalid       CMHOSS        090
Accident state invalid       CMAST         091
Demographics not found       CMSS#         092
Claimant SS# is provider #   CMSS#         093
Office number missing        CMOFF#        094
Office number invalid        CMOFF#        095
Occupation code invalid      CMOCCD        096
Department invalid           CMDEPT        097
SIC Code invalid             CMSIC         098
xxxxxxxxx      099           XXXXXX        099
xxxxxxxxx      100           XXXXXX        100
Potential lost time invalid  CMPLFL        101
Employer premises invalid    CMEADR        102
Pre-existing flag invalid    CMPREX        103
More than 1 day lost invalid CMGT1         104
OSHA reportable flag invalid CMOSHA        105
Claim denied flag invalid    CMDNY         106
Controvert flag invalid      CMCVFL        107
Subrogate flag invalid       CMSUBR        108
Acquired flag invalid        CMACQR        109
SDF flag invalid             CMSDF         110
Managed Care flag invalid    CMMCO         111
Occupational disease invalid CMODIS        112
Handicap claim flag invalid  CMHCAP        113
Death claim flag invalid     CMDTH         114
Full pay on inj date invalid CMPOI         115
Full salary continued invalidCMFSAL        116
Agree to compensate invalid  CMAGCP        117
Vocational rehab flag invalidCMVORC        118
Pension offset invalid       CMPEFF        119
Unemployment offset invalid  CMUEFF        120
Soc Security offset invalid  CMSSFF        121
Benefit other offset invalid CMOTFF        122
Special fund offset invalid  CMSFFF        123
Excess reinsurance invalid   CMEXCS        124
Employer agree flag invalid  CMEAGR        125
Xxxxxx xxxxxxx 126 x invalid CMXXXX        126
Xxxxxx xxxxxxx 127 x invalid CMXXXX        127
Xxxxxx xxxxxxx 128 x invalid CMXXXX        128
Xxxxxx xxxxxxx 129 x invalid CMXXXX        129
Xxxxxx xxxxxxx 130 x invalid CMXXXX        130
Supervisor invalid           CMSUPR        131
Claims examiner invalid      CMCLEX        132
Field adjustor invalid       CMFEAD        133
Account manager invalid      CMAMAN        134
Claim entry clerk invalid    CMCEC         135
Case nurse invalid           CMNRSE        136
Excess adjustor invalid      CMEXCS        137
Unit manager invalid         CMUMAN        138
SDF adjustor invalid         CMSDFA        139
xxxxxxxxx      140           XXXXXX        140
xxxxxxxxx      141           XXXXXX        141
xxxxxxxxx      142           XXXXXX        142
xxxxxxxxx      143           XXXXXX        143
xxxxxxxxx      144           XXXXXX        144
Supervisor missing           CMSUPR        145
Claims examiner missing      CMCLEX        146
xxxxxxxxx      147           XXXXXX        147
xxxxxxxxx      148           XXXXXX        148
Claim entry clerk missing    CMCEC         149
xxxxxxxxx      150           XXXXXX        150
xxxxxxxxx      151           XXXXXX        151
xxxxxxxxx      152           XXXXXX        152
xxxxxxxxx      153           XXXXXX        153
xxxxxxxxx      154           XXXXXX        154
xxxxxxxxx      155           XXXXXX        155
xxxxxxxxx      156           XXXXXX        156
xxxxxxxxx      157           XXXXXX        157
xxxxxxxxx      158           XXXXXX        158
xxxxxxxxx      159           XXXXXX        159
xxxxxxxxx      160           XXXXXX        160
