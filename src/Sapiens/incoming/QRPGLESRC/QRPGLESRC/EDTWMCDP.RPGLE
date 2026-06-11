     h*--------------------------------------------------------------------*
     hcopyright('(c) Copyright Fiserv Insurance Solutions, Inc., 2005-2008')
     h*--------------------------------------------------------------------*
     h/title EDTWMCMP:  Edit Claim Demographics - WMCDP
     h**********************************************************************
     h*  EDTWMCMP:  Edit Claim Demographics - WMCDP.         C:58   P:SY058
     h**********************************************************************
     h option(*nodebugio : *srcstmt)                                                               *
     h debug(*yes)                                                                                 *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 10/29/08  C:33  Project: 114WPD                                                            *
      *                -Convert results to PDF                                                     *
      *=============================================================================================
     fWMCDL     ipe  e           k disk                                          Input file

     fEDTERR    o    e             disk
     fEDTWMCDPO o    e             printer oflind(*IN89) usrOpn

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
     D*                                                                -----
     D kkfnd           s              3P 0
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

     D errText         s             35    dim(40) ctdata perrcd(1)
     d errCT           s              7p 0 dim(40)                                 Index
     D saveSS#         s                   like(CDSS#)
     D co#             s              3  0
     D cderrn          s              9  0
     D cdedsc          s             30
     D us_rrn          s              9  0
     D cdename         s             40



     d*---------| Initialization work area |-------------------------------
     D sys_date        s               d   datfmt(*usa)
     D sys_time        s               t   timfmt(*hms:)
     D WKdate8         s              8  0
     D SYSnum7         s              7  0
     D xxDATEX         s              7s 0
     d dirPDF          s           1024    inz

     d*---------| Constants |----------------------------------------------
     dcnTITLE          c                   const('PDS Claims - File Edit')       Title
     dcnTYPE           c                   const('Claim Demographics- WMCDP')    File (new/old)
     dgenERR01         c                   const('date prior to accident date')
     dgenERR02         c                   const('date after claimant death')
     dgenERR03         c                   const('future date invalid')

     D KCON01          c                   const('EDIT CLAIM DEMOGRAPHICS +
     D                                            - WMCDP')

     D*---------------------------------------------------------------------
     D* Stand Alone Fields - BOTTOM
     D*---------------------------------------------------------------------

     d*-------| x Error Code Data Structure |----------------------------
     d                 ds                  inz                                   Work
     d  xxx                    1      4a
     d  xz                     1      1
     d  x0                     2      2
     d  x                      2      4  0

     d*-------| Time |------------------------------------------------------
     d                 ds                  inz                                   Work
     d  xxtime                 1      6  0
     d  xxtmx                  1      6
     d  tmhrs                  1      2  0
     d  tmmin                  3      4  0
     d  tmsec                  5      6  0

     d*-------| Social security redefined |--------------------------------
     d                 ds                  inz                                   Work
     d  CDSS#                  1      9
     d*
     d  ##SSA                  1      3
     d  ##SSB                  4      5
     d  ##SSC                  6      9

     d*---------| Working storage |--------------------------------------
     dWork_Stor        ds                  inz                                   Work
     d  IX                            5p 0                                         Index
     d  xxCOUNT                       9p 0                                         Record counter
     d  xxERRS                        9p 0                                         Record counter
     d  xxACCTS                       9p 0                                         Print counter

     d*-------| Input Dates Data Structure |----------- 3 @ 7 0 ------------
     d                 ds                  inz                                   Work
     derrDATES                        7  0 dim(3)                                  Index
     d  cddtob                 1      7  0
     d  cddt                   8     14  0
     d  cdidt                 15     21  0

     d*-------| Input User Codes Data Structure |------ 2 @ 10A ------------
     d                 ds                  inz                                   Work
     derrSTAF                        10A   dim(2)                                  Index
     d  cduser                 1     10
     d  cdiusr                11     20

     D psds          esds                  extname(szq1p)

     i*_____________________________________________________________________
     i*---------| Define Record ID indicator |------------------------------
     ipcd01         50                                                           Work
     i*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

     c*=====================================================================
     c*---===========         Mainline processing             ============--
     c*=====================================================================
     c*    -- Got an input record.
     c                   if        *in50=*on                                     >-+

     c                   eval      xxCOUNT=xxCOUNT+1                                 :

     c                   eval      doDetPrint=*off                                 : :
     c                   movel     ' '           xz                                    :
     c                   CLEAR                   EER01                                   :

     c                   if        saveSS# = CDSS#                                 : :
     c                   movel(p)  CDSS#         EEDATA                            : :
     c                   eval      x = 1                                           : :
     c                   exsr      $Error                                          : :
     c                   endif                                                   <-+ :

     c                   if        xxCOUNT=1                                     >-+ :
     c                   eval      saveSS# = CDSS#                                 : :
     c                   endif                                                   <-+ :
     c*
     c*---- Social secy is required ---------------------------------------
     c                   if        cdss# = *blank
     c                   eval      x = 2                                           : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*================================================================---
     c*---------| Process record field edits |----------------------------
     c                   exsr      $Process                                        :

     c*================================================================---
     c*---------| End of record process - print detail |------------------
     c                   exsr      $Write_D1                                       :

     c                   eval      saveSS# = CDSS#                                 : :

     c                   endif                                                   <-+ :
     c*-------------------------------------------------------------------

     c*================================================================---
     c*---------| End of primary file |-----------------------------------
     clr                 exsr      $Finish                                         :
     c*================================================================---

     c*===================================================================*
     c*------------| Process a record subroutine |-------------------------
     c     $Process      begsr                                                   >---+

     c*---------------------------------------------------------------------
     c*---| ValiDates - all dates in Demographics array: errDATES |---------
     c                   exsr      $ValiDates                                      :
     c*---------------------------------------------------------------------

     c*---------------------------------------------------------------------
     c*---| ValiStaff- all staff codes in Claim Master array: errSTAF |-----
     c                   exsr      $ValiStaff                                      :
     c*---------------------------------------------------------------------

     c*------------| Individual field edits |------------------------------

     c*--------------REQUIRED-----------------------------------------------
     c*----| Name |----------------------                              -----
     c                   if        cdlsn = *blank
     c                   eval      x = 22                                          : :
     c                   exsr      $Error                                          : :
     c                   endif

     c                   if        cdfsn = *blank
     c                   eval      x = 23                                          : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*----| Address |----------------------                           -----
     c                   if        cdadr1 = *blank
     c                   eval      x = 24                                          : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   if        cdcty = *blank
     c                   eval      x = 25                                          : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   if        cdst = *blank
     c                   eval      x = 26                                          : :
     c                   exsr      $Error                                          : :
     c                   else
     c                   if        ValidState(cdst:us_rrn:cdename) = *off
     c                   movel(p)  cdst          EEDATA                            : :
     c                   eval      x = 27                                          : :
     c                   exsr      $Error                                          : :
     c                   endif
     c                   endif
     c                   if        cdzip = *zero
     c                   eval      x = 28                                          : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*----| Country code if not blank |------------------------------------
     c                   if        cdCTRY <> *blank and
     c                             ValidCntry(co#:cdctry:cderrn:cdedsc) = *off
     c                   movel(p)  cdctry        EEDATA                            : :
     c                   eval      x = 29                                          : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*---------------------------------------------------------------------
     c*----| ValidGender -- claimant sex |----------------------------------
     c                   if        cdSEX <> *blank and
     c                             ValidGender(co#:cdSEX) = *off
     c                   movel(p)  cdSEX         EEDATA                            : :
     c                   eval      x = 17                                          : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*----| ValiMariSt -- claimant marital status |------------------------
     c                   if        cdMSTS <> *blank and
     c                             ValidMariSt(co#:cdMSTS) = *off
     c                   movel(p)  cdmsts        EEDATA                            : :
     c                   eval      x = 18                                          : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*----| ValiCounty -- county code |------------------------------ -----
     c                   if        cdCNTY <> *blank and
     c                             ValidCounty(co#:cdCNTY) = *off
     c                   movel(p)  cdCNTY        EEDATA                            : :
     c                   eval      x = 30                                          : :
     c                   exsr      $Error                                          : :
     c                   endif

     c*---- Time fields - (required) --------------------------------- ----
     c                   eval      xxtime = CDTIME                                 : :
     c                   eval      D = 9                                           : :
     c                   exsr      $ValiTime                                       : :

     c                   eval      xxtime = CDITM                                  : :
     c                   eval      D = 10                                          : :
     c                   exsr      $ValiTime                                       : :

     c                   endsr                                                   <-+
     c*---------------------------------------------------------------------

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
     c*------------| Dates Validation |-------------------------------------
     c*=====================================================================
     c*-- Validation for 3 dates in WMCDP in array errDATES;
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

     c*      Otherwise, validate
     c                   eval      WKdate8 = dtMDYY(errDATES(D))

     c*---------------------------------------------------------------------
     c*      Invalid date.
     c     WKdate8       ifeq      *zero                                         >-----+
     c                   eval      xxDATEX = errDATES(D)                           : :
     c                   movel(p)  xxDATEX       EEDATA                            : :
     c                   eval      x = D + 5                                       : :
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
     c                   eval      x = D + 18                                      : :
     c                   exsr      $Error                                          : :
     c                   eval      DTinvalid = *on                                 : :
     c                   endif                                                   <---+
     c                   endif                                                   <---+
     c                   endif                                                   <---+

     c                   enddo                                                   <---+

     c                   endsr                                                   <---+


     c*=====================================================================
     c*  Validation for staff codes in WMCDP in array errSTAF.
     c*=====================================================================
     c     $ValiStaff    begsr                                                   >-----+

     c                   do        2             S
     c                   if        errSTAF(S) = *blank                           >-----+
     c                   eval      x = S + 12                                      : :
     c                   exsr      $Error                                          : :
     c                   else
     c                   if        GetUsrRRN(errSTAF(S))                         >-----+
     c                                 = *zero                                   >-----+
     c                   movel(p)  errSTAF(S)    EEDATA                            : :
     c                   eval      x = S + 14                                      : :
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
     c                   movel(p)  CDSS#         EEKEY                                 :

     c                   movel(p)  'WMCDP'       EEFILE                                :
     c                   z-add     x             EECODE                            : :
     c                   move(p)   errText(x)    xxFLD6                                :
     c                   movel(p)  xxFLD6        EEFLD                                 :

     c*     Offending application record: ID fields
     c                   movel(p)  CDPGM         ERPGM                                 :
     c                   movel(p)  CDUSER        ERUSER                                :
     c                   z-add     CDDT          ERDT                              : :
     c                   z-add     CDTIME        ERTIME                            : :
     c                   movel(p)  CDIPGM        ERIPGM                                :
     c                   movel(p)  CDIUSR        ERIUSR                                :
     c                   z-add     CDIDT         ERIDT                             : :
     c                   z-add     CDITM         ERITIM                            : :

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
     c                   endif                                                   <-----+
     c                   eval      PeList=*blank                                     :
     c                   eval      xxACCTS=xxACCTS+1                                   :
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

     c                   open      EDTWMCDPO

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

     c                   Do        40            x
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

     c                   close     EDTWMCDPO
     C                   exsr      CvtToPDF

     c                   endsr                                                   <-+
     c/copy wCpyEdtClm

** errText                      40 @ 35  (29 caption +6 fieldname)
Duplicate SS#                CDSS#         001
Missing  social security #   CDSS#         002
Date of birth missing        CMDTOB        003
Date maintained missing      CDDT          004
Initial date missing         CDIDT         005
Date of birth invalid        CMDTOB        006
Date maintained invalid      CDDT          007
Initial date invalid         CMIDT         008
Time maintained missing      CDTIME        009
Initial time missing         CDITM         010
Time maintained invalid      CDTIME        011
Initial time invalid         CDITM         012
Maintenance user missing     CDUSER        013
Initial user missing         CDIUSR        014
Maintenance user invalid     CDUSER        015
Initial user invalid         CDIUSR        016
Claimant gender code invalid CDSEX         017
Marital status invalid       CDMSTS        018
Date of birth in future      CDDTOB        019
Maintenance date in future   CDDT          020
Initial date in future       CDIDT         021
Last name missing            CDLSN         022
First name missing           CDFSN         023
Address missing              CDADR1        024
City missing                 CDCTY         025
State missing                CDST          026
State invalid                CDST          027
Zip code missing             CDZIP         028
Country code invalid         CDCTRY        029
County code invalid          CDCNTY        030
xxxxxxxxx      031           XXXXXX        031
xxxxxxxxx      032           XXXXXX        032
xxxxxxxxx      033           XXXXXX        033
xxxxxxxxx      034           XXXXXX        034
xxxxxxxxx      035           XXXXXX        035
xxxxxxxxx      036           XXXXXX        036
xxxxxxxxx      037           XXXXXX        037
xxxxxxxxx      038           XXXXXX        038
xxxxxxxxx      039           XXXXXX        039
xxxxxxxxx      040           XXXXXX        040
