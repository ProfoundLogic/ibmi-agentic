     Hcopyright('(c) 2023 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE W7320R - INVOICES - PROMPT
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*            W0051R                                             *
     H*                                                               *
     H* CALLS TO:  W7320C - SUBMIT                                    *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/14/23 C:344 Support: 109043                                *
     H*                Add a missing default Invoice# parameter for   *
     H*                calling the W7320C program.                    *
     H*                                                               *
     H* 10/06/14 C:279 Project: 102WGI (Back to Base:102CB)           *
     H*                Add prompt for generating all premium notices  *
     H*                at the same time according to billing schedule.*
     H*                                                               *
     H* 11/22/13 C:190 Project: 129WSB                                *
     H*                Modify program to process new "Audit" option.  *
     H*                Add checkbox for suppression of prior year bal *
     H*                                                               *
     H* 10/31/12 C:65  Project: WC2116                                *
     H*                Default checkbox to N to avoid error message.  *
     H*                                                               *
     H* 10/23/09 C:234 SUPPORT: SYS184 PRISS# 22                      *
     H*                Remove hard coded report library name.         *
     H*                                                               *
     H* 09/07/09 C:234 SUPPORT: SYS184 PRISS# 22                      *
     H*                Changes for project 132WLB (prm notice portion)*
     H*                sent back to base                              *
     H*                                                               *
     H* 10/01/08 C:190 PROJECT: 132WLB                                *
     H*                Added field "Type of output(P,F,B)             *
     H*                                                               *
     H*  07/02/08  C:65  PROJECT: SUPP#64170                          *
     H*                  FIX AGENT RATE PLAN CHECK TO GET ACTIVE AGENT*
     H*                                                               *
     H*  02/07/08  C:191 PROJECT: 114WPZ                              *
     H*                  Default Premium Notice to yes                *
     H*                                                               *
     H*  10/11/06  C:33  Seq#: WA099/41801                            *
     H*                  Use SC305R instead of SC580R.                *
     H*                                                               *
     H* 05/24/06 C:53  PROJECT: WC1410                                *
     H*                ADD NEW FIELDS AND CREATE 2ND SCREEN           *
     H*                                                               *
     H* 01/06/06 C:168 PROJECT: 132WDT WC1347                         *
     H*                ADDED FIELD "RESOLVE TO DARTS" TO SAVE SPLF    *
     H*                                                               *
     H* 03/07/05 C:136 PROJECT: WC1199 (REVAMP DUNNING II)            *
     H*                ADD MIN BALANACE DUE AMT TO PASS THROUGH       *
     H*                TO W7321R.                                     *
     H*                                                               *
     H* 10/23/01 C:136 SUPP:29442  WA099                              *
     H*                Program executes an endless loop if certain    *
     H*                conditions are encountered.  Changed do loop.  *
     H*                                                               *
     H* 04/06/01 C:90  PROJECT: WC577                                 *
     H*                Do not allow premium notice to be run if for   *
     H*                a single agency bill type employer.            *
     H*                                                               *
     H* 05/30/00 C:61  PROJECT WC577                                  *
     H*              - Use correct fund when called from              *
     H*                WW emp pol years.                              *
     H*                                                               *
     H* 12/02/99 C:61  PROJECT WC577                                  *
     H*              - Screen file change to enable F12=Cancel if     *
     H*                called for single employer.  (@@RRN NE 0)      *
     H*              - Change to return *INKC and *INKL               *
     H*                                                               *
     H* 11/18/99 C:61  PROJECT WC577                                  *
     H*                Enable to be called for single employer from   *
     H*                opt81 of employer detail (W0051R).             *
     H*                                                               *
     H* 04/26/99  C:87   PROJECT: CM042                               *
     H*                  Change 'installment date' to 'installment    *
     H*                  date range'.  Add prompt for 'Generate -     *
     H*                  monthly, renewal or all'.  Chg 'Due Date' to *
     H*                  'Add'l days from installment due'.  Add      *
     H*                  appropriate processing for these changes.    *
     H*                  Remove 'Policy eff date range' & processing. *
     H*                                                               *
     H* 12/22/98  C:87   PROJECT: CM041                               *
     H*                  Add prompt for 'Interim days...'.
     H*                                                               *
     H* 11/30/98  C:35   PROJECT: CM041                               *
     H*                  ADD OPTIONAL POLICY EFFECTIVE DATE RANGE.    *
     H*                                                               *
     H*  09/03/98 C:87 PROJECT 72WBY                                  *
     H*                ADD CODE TO DISPLAY ERROR MSG IF THE FIELD     *
     H*                BEING SEARCHED ON IS NOT A PROPER SEARCH FIELD *
     H*                CLEAR FUND NAME BEFORE LOADING IT              *
     H*                SET INDICATOR FOR POSITION TO AFTER SEARCH     *
     H*                                                               *
     H*  12/01/97 C:87 PROJECT CM032                                  *
     H*                REMOVED SCFRDT AND SCTODT, NOW PASSING SCDATE  *
     H*                 AS FROM AND TO DATE IN PARMS FOR MULTIPLE     *
     H*                 EMPLOYERS OR PASSING *BLANKS AS FROM AND TO   *
     H*                 DATE FOR SINGLE EMPLOYERS.                    *
     H*                                                               *
     H*  10/29/97 C:79 PROJECT WC712                                  *
     H*                Program was bombing if Fund or Company = zero  *
     H*                Added an error message to notify the user to   *
     H*                change their user parms.                       *
     H*                                                               *
     H*  07/03/97 C:79 PROJECT WC712                                  *
     H*               -PREMIUM NOTICE, ADDL V3 CHANGES.               *
     H*                                                               *
     H*  11/11/96 C:58 PROJECT CM017                                  *
     H*                GENERAL CLEANUP                                *
     H*                CONVERTED TO COMMERCIAL BASIS                  *
     H*                REDID ERROR INDICATORS TO BE TOP DOWN WITH NEW *
     H*                  SCREEN                                       *
     H*                ALLOW FOR ADDTL SORTS/RANGES                   *
     H*                                                               *
     H*  01/10/96 C:57 PROJECT CM017                                  *
     H*               -CHG NOTICE DATE TO INSTALLMENT DATE, REMOVE    *
     H*                EDIT TO REQUIRE IT TO BE DAY 01 OF PERIOD.     *
     H*               -ADD SCDUDT TO SCREEN AND AS A PARM             *
     H*                                                               *
     H*  01/03/95 C:32 PROJECT WC081                                  *
     H*                Added choice of Employer Name Sort to screen   *
     H*                and Employer Name Range; also added employer   *
     H*                number to screen.                              *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*                                                               *
     H*   73  CALLED FROM W0051R                                      *
     H*                                                               *
     H*   SCR01:                                                      *
     H*   20  SCDATE INVALID                                          *
     H*   21  SCEXDF <> Y/N                                           *
     H*   22  SCCOM1 INVALID                                          *
     H*   23  SCCOM2 INVALID                                          *
     H*   24  SCCOM3 INVALID                                          *
     H*   25  SCPYR INVALID FOR FUND                                  *
     H*   26  SCEMP#/SCDIV NOT FOUND                                  *
     H*   27  SCSRT1 = BLANK                                          *
     H*   28  SCSRT1 <> EA/EN/AA/AN                                   *
     H*   29  SCFEMP/SCTEMP <> BLANK WHEN SCSRT1 <> EN                *
     H*   30  SCFEMP > SCTEMP                                         *
     H*   31  SCENFR/SCENTO <> BLANK WHEN SCSRT1 <> EA                *
     H*   32  SCENFR > SCENTO                                         *
     H*   33  SCFAGT/SCTAGT <> BLANK WHEN SCSRT1 <> AN                *
     H*   34  SCFAGT > SCTAGT                                         *
     H*   35  SCANFR/SCANTO <> BLANK WHEN SCSRT1 <> AA                *
     H*   36  SCANFR > SCANTO                                         *
     H*   37  SCSRT2 <> BLANK  (SCSRT1 = EA/EN)                       *
     H*   38  SCSRT2 <> EA/EN/BLANK (WHEN SCSRT1 = AA/AN)             *
     H*   39  BOTH INDIVIDUAL EMPLR & BATCH PARMS ENTERED             *
     H*   40  NEITHER INDIVIDUAL EMPLR & BATCH PARMS ENTERED          *
     H*   41  POL EFF FROM DATE INVALID                               *
     H*   42  POL EFF TO DATE INVALID                                 *
     H*   43  POL EFF FROM DATE GREATER THAN POL EFF TO DATE          *
     H*   44  SCADDU INVALID                                          *
     H*   49  POSITION CURSOR SCCOM2                                  *
     H*   50  POSITION CURSOR SCCOM3                                  *
     H*   51  POSITION CURSOR SCEMP#                                  *
     H*   52  POSITION CURSOR SCDIV                                   *
     H*   53  POSITION CURSOR SCPYR                                   *
     H*   54  POSITION CURSOR SCFREA                                  *
     H*   55  POSITION CURSOR SCTOEA                                  *
     H*   56  POSITION CURSOR SCFREN                                  *
     H*   57  POSITION CURSOR SCTOEN                                  *
     H*   58  POSITION CURSOR SCFRAA                                  *
     H*   59  POSITION CURSOR SCTOAA                                  *
     H*   61  POSITION CURSOR SCFRAN                                  *
     H*   62  POSITION CURSOR SCTOAN                                  *
     H*   63  POSITION CURSOR SCSRT2                                  *
     H*   64  POSITION CURSOR SCDRMK                                  *
     H*   65  POSITION CURSOR SCSRT1                                  *
     H*   66  POSITION CURSOR SCDATE                                  *
     H*   67  CANNOT RUN PREM NOTICE IF AGENCY BILLED (WC31370)       *
     H*   99  INVALID CO# AND/OR FUND                                 *
     H*                                                               *
     H*****************************************************************
     F*
     FW7320F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSE)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PFRRNE)
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PFRRND)
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDSD)
     FWDELL1    IF   E           K DISK
     FWMEXL     IF   E           K DISK
     FWTABL     IF   E           K DISK
      *** BEGIN ADD *** 12/01/97 *************************************
     FWMAAP     IF   E             DISK
      *** END   ADD *** 12/01/97 *************************************
     F**** BEGIN ADD **** 04/06/01 ************************************
     FWMAHL     IF   E           K DISK
     FWMARL     IF   E           K DISK
     F**** END   ADD **** 04/06/01 ************************************

     D*
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D DBFDSD          DS
     D  DBRRND               397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D***  BEGIN ADD  ***  04/26/99  **********************************
     D                 DS
     D  SCDATE                 1      8  0
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D  XXCCYY                 5      8  0
     D*
     D                 DS
     D  SCDAT2                 1      8  0
     D  X2MM                   1      2  0
     D  X2DD                   3      4  0
     D  X2CCYY                 5      8  0
     D***  END   ADD  ***  04/26/99  **********************************
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      ******* beg add ****** 10/06/14 *********************************
     d intFrDt         s              7  0
     d intToDt         s              7  0
     d xxFrDt          s              8
     d xxToDt          s              8
     d qqAll           s              1
      ******* end add ****** 10/06/14 *********************************
      ******* Beg Add ****** 04/14/23 *********************************
     d @defInv#        s             10    inz('0')
      ******* End Add ****** 04/14/23 *********************************
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D***  BEG   ADD  ***  10/23/09  **********************************
     D XXRPM1          C                   CONST('This report will pr-
     D                                     oduce output file W-
     D                                     FPNP in library PDS-
     D                                     RPT')
     D XXRPM2          C                   CONST(', if selected')

     I***  END   ADD  ***  10/23/09  **********************************

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR REMARK TYPE TABLE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    SCDRMK
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C*
     C*  FUND YEAR KEY LIST
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPYR
     C*
     C* KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    UPCO#             3 0
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C* KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EM1KY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      ***** BEG ADD ******* 11/18/99 **********************************
     C*
     C     EMKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      ***** END ADD ******* 11/18/99 **********************************
     C*
     C* KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  CLIENT CONTROL FILE
     C*
     C     EXKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C*
     C* KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY
     C**** BEGIN ADD **** 04/06/01 ************************************
     C*
     C* KEY LIST FOR THE AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     AHKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C* KEY LIST FOR THE AGENT RATE FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHRT#
     C*
     C     ARKY1         KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHRT#
     C                   KFLD                    XXEFDT
     C*
     C**** END   ADD **** 04/06/01 ************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
      ***** BEG ADD ******* 12/02/99 **********************************
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
      ***** END ADD ******* 12/02/99 **********************************
      ***** BEG ADD ******* 11/18/99 **********************************
     C                   PARM                    @@RRN             9 0          WDELP
      *
      *   When @@RRN is not *ZERO, WDELP record is used to determine
      *   FND, EMP, DIV, FYR of single employer and irrelevent prompt
      *   fields are not displayed.
      *
      *
      ***** END ADD ******* 11/18/99 **********************************
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     **
     **  LIKE DEFINES
     **
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELCO#         @@CO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFND         @@FND
     C     *LIKE         DEFINE    ELFYR         XXPYR
     C     *LIKE         DEFINE    ELFYR         @@FYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELEMP#        @@EMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELDIV         @@DIV
     C     *LIKE         DEFINE    EXPNRM        XXDRMK
     C     *LIKE         DEFINE    EXPNRM        QQDRMK
      ***** BEG ADD ******* 11/18/99 **********************************
     C     *LIKE         DEFINE    EMEMP#        ##EMP#
     C     *LIKE         DEFINE    EMDIV         ##DIV
     C     *LIKE         DEFINE    EMNAME        ##NAME
      ***** END ADD ******* 11/18/99 **********************************
     C*** BEG ADD ****** 02/07/08 ************************************
     C* Retrieve Client ID
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** END ADD ****** 02/07/08 ************************************
     **
     **  INITIALIZE FIELDS
     **
     C*** BEG ADD ****** 10/23/09 *************************************
     C                   CALL      'SA030R'
     C                   PARM                    XXSUFX            3
     C     XXRPM1        CAT(P)    XXSUFX:0      SCRPMG
     C                   CAT(P)    XXRPM2:0      SCRPMG
     C*** END ADD ****** 10/23/09 *************************************
      ***** BEG ADD ******* 11/18/99 **********************************
     C                   SETON                                        73
      *
      *  Call for single employer/div/fyr, if employer detail passed.
      *
      ***** BEG DEL ******* 12/02/99 **********************************
     C*********  Q1PRMS    IFGT 2
      ***** END DEL ******* 12/02/99 **********************************
      ***** BEG ADD ******* 12/02/99 **********************************
     C                   SELECT
      *
     C     Q1PRMS        WHENGT    4
      ***** END ADD ******* 12/02/99 **********************************
     C     @@RRN         CHAIN     WDELP                              73
      *
     C     *IN73         IFEQ      *OFF
     C                   MOVE      ELFND         SCFND
     C                   MOVE      ELEMP#        SCEMP#
     C                   MOVE      ELDIV         SCDIV
     C                   MOVE      ELFYR         SCPYR
      *
     C     EMKY2         CHAIN     PEM01                              60
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C     *IN60         IFEQ      *OFF
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        ##EMP#
     C                   PARM      EMDIV         ##DIV
     C                   PARM      EMNAME        ##NAME
     C                   PARM                    ##EDN            80
      *
     C                   MOVEL     ##EDN         SCEDN
     C                   ENDIF                                                  *IN60
      *
     C                   Z-ADD     ELFYR         SCFYR
     C                   ENDIF                                                  *IN60
      *
      ***** BEG DEL ******* 12/02/99 **********************************
     C***********          ENDIF                           Q1PRMS
      ***** END DEL ******* 12/02/99 **********************************
      ***** BEG ADD ******* 12/02/99 **********************************
      ***** END ADD ******* 12/02/99 **********************************
     C                   ENDSL                                                  Q1PRMS
      *
      ***** END ADD ******* 11/18/99 **********************************
     C                   Z-ADD     *ZERO         SCDATE
     C***  BEGIN CHG  ***  04/26/99  **********************************
     C                   Z-ADD     *ZERO         SCDAT2
     C*******************  Z-ADD*ZERO     SCDUDT
     C***  END   CHG  ***  04/26/99  **********************************
     C                   MOVE      *OFF          *IN46
      *** BEGIN ADD *** 12/01/97 ************************************
     C                   Z-ADD     0             QQVIEW            1 0
      *** END   ADD *** 12/01/97 ************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C                   MOVE      UPCO#         XXCO#
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      ***** BEG CHG ******* 05/30/00 **********************************
     C***************      MOVE UPFND     SCFND
     C   73              MOVE      UPFND         SCFND
      ***** END CHG ******* 05/30/00 **********************************
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C***  BEGIN ADD  ***  09/03/98  **********************************
     C                   MOVE      *BLANKS       SCNAME
     C***  END   ADD  ***  09/03/98  **********************************
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C***  BEGIN CHG  ***  09/03/98  **********************************
     C*******************  MOVELFNNAME    SCNAME
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCNAME
     C                   ENDIF
     C***  END   CHG  ***  09/03/98  **********************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   Get Default DARTS Remark Code From Admin/DARTS control maint.
     C*
     C     EXKY          CHAIN     PEX01                              60
     **
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANK        SCDRMK
      *** beg add *** 11/22/13 **********************************
     C                   MOVEL     *BLANK        scSupBalP
      *** end add *** 11/22/13 **********************************
     C                   ELSE
     C                   MOVEL     EXPNRM        SCDRMK
      *** beg add *** 11/22/13 **********************************
     C                   MOVEL     EXspyb        scSupBalP
      *** end add *** 11/22/13 **********************************
     C                   ENDIF
      ***** BEG ADD ******* 11/18/99 **********************************
      *
      ***** BEG ADD ******* 01/06/06 **********************************
      *
      * Defaults the "Resolve to DARTS" flag
      *
     C*** BEG ADD ****** 02/07/08 ************************************
     C                   SELECT
     C     CLTID         WHENEQ    '114'
     C                   MOVEL     'Y'           SCDRES
     C                   OTHER
     C*** END ADD ****** 02/07/08 ************************************
     C                   MOVEL     'N'           SCDRES
     C*** BEG ADD ****** 02/07/08 ************************************
     C                   ENDSL
     C*** END ADD ****** 02/07/08 ************************************
      *** BEG ADD *** 05/24/06 **********************************
      * Defaults flags
     C                   MOVE      'Y'           SCDEP
     C                   MOVE      'Y'           SCEXP
     c*** begin add ***  10/31/12  ***********************************
     C                   MOVE      'N'           SCEXDF
     c*** end add *****  10/31/12  ***********************************
      *** END ADD *** 05/24/06 **********************************
      *** BEG DEL ****** 09/07/09 ************************************
      *** BEG ADD ****** 10/01/08 ************************************
     C*********  CLTID     IFEQ '132'
     C*********            MOVE *ON       *IN70
     C*********            ENDIF
      *** END ADD ****** 10/01/08 ************************************
      *** END DEL ****** 09/07/09 ************************************
      *
      ***** BEG ADD ******* 01/06/06 **********************************
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      ***** END ADD ******* 11/18/99 **********************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C*   CONVERT THE INSTALLMENT (NOTICE) DATE ENTERED
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C***  BEGIN ADD  ***  04/26/99  **********************************
     C*   Convert the installment 'to' date
     C                   Z-ADD     SCDAT2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDAT2
     C***  END   ADD  ***  04/26/99  **********************************
     C*
     C***  BEGIN DEL  ***  04/26/99  **********************************
     C*** CONVERT THE DUE DATE DATE ENTERED
     C*******************
     C*******************  Z-ADDSCDUDT    SRDTI
     C*******************  CALL 'SRCVTDAT'DATCHK
     C*******************  MOVELSRDTO     SCDUDT
     C***  END   DEL  ***  04/26/99  **********************************
     C*
      *** BEGIN DEL *** 12/01/97 ************************************
     C** CONVERT THE POLICY EFFECTIVE FROM DATE
     C*******************
     C******************** Z-ADDSCFRDT    SRDTI
     C******************** CALL 'SRCVTDAT'DATCHK
     C******************** MOVELSRDTO     SCFRDT
     C******************
     C** CONVERT THE POLICY EFFECTIVE TO DATE
     C******************
     C******************** Z-ADDSCTODT    SRDTI
     C******************** CALL 'SRCVTDAT'DATCHK
     C******************** MOVELSRDTO     SCTODT
      *** END   DEL *** 12/01/97 ************************************
      /free
       //**** beg add ***** 10/06/14 **********************************
       srDtI = scFDt;
       Exsr cvtDate;
       If srDtO <> ' ';
          scFDt = %Int(%trim(srDtO));
       Endif;

       srDtI = scTDt;
       Exsr cvtDate;
       If srDtO <> ' ';
          scTDt = %Int(%trim(srDtO));
       Endif;

       isFMIC = *off;
       If cltId = '102';
          isFMIC = *on;
       Endif;
       //**** end add ***** 10/06/14 **********************************
      /end-free
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
      *** BEG ADD *** 05/24/06 **********************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   ENDIF
      *** END ADD *** 05/24/06 **********************************
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      *ON
      ***** BEG DEL ******* 12/02/99 **********************************
     C*************        MOVE *ON       *INLR
      ***** END DEL ******* 12/02/99 **********************************
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*  TABLE SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
      *** BEGIN CHG *** 12/01/97 *************************************
     C***COMMENT CODE 1
     C**********
     C********** HFLD      IFEQ 'SCCOM1'
     C**********           CALL 'W2191R'
     C**********           PARM '2'       QQCALL  1
     C********** *INKC     PARM           QQKC    1
     C********** *INKL     PARM           QQKL    1
     C**********           PARM *BLANKS   SCKEY  10
     C**********
     C********** *INKC     IFEQ *ON
     C**********           GOTO TERM
     C**********           ENDIF
     C**********
     C********** *INKL     IFEQ *ON
     C**********           MOVE *ON       *IN48
     C**********           ENDIF
     ***********
     C********** SCKEY     IFNE *BLANKS
     C**********           MOVE *ON       *IN49
     C**********           MOVELSCKEY     SCCOM1
     ***********
     C**********           ELSE
     C**********           MOVE *ON       *IN48
     ***********
     C**********           ENDIF                           SCKEY IFNE
     ***********
     C**********           MOVEL'SCR01'   Q2RECF    P
     C**********           GOTO STRT
     ***********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C***COMMENT CODE 2
     C**********
     C********** HFLD      IFEQ 'SCCOM2'
     C**********           CALL 'W2191R'
     C**********           PARM '2'       QQCALL  1
     C********** *INKC     PARM           QQKC    1
     C********** *INKL     PARM           QQKL    1
     C**********           PARM *BLANKS   SCKEY
     C**********
     C********** *INKC     IFEQ *ON
     C**********           GOTO TERM
     C**********           ENDIF
     C**********
     C********** *INKL     IFEQ *ON
     C**********           MOVE *ON       *IN49
     C**********           ENDIF
     ***********
     C********** SCKEY     IFNE *BLANKS
     C**********           MOVE *ON       *IN50
     C**********           MOVELSCKEY     SCCOM2
     ***********
     C**********           ELSE
     C**********           MOVE *ON       *IN49
     ***********
     C**********           ENDIF                           SCKEY IFNE
     ***********
     C**********           MOVEL'SCR01'   Q2RECF    P
     C**********           GOTO STRT
     ***********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C***COMMENT CODE 3
     C**********
     C********** HFLD      IFEQ 'SCCOM3'
     C**********           CALL 'W2191R'
     C**********           PARM '2'       QQCALL  1
     C********** *INKC     PARM           QQKC    1
     C********** *INKL     PARM           QQKL    1
     C**********           PARM *BLANKS   SCKEY
     C**********
     C********** *INKC     IFEQ *ON
     C**********           GOTO TERM
     C**********           ENDIF
     C**********
     C********** *INKL     IFEQ *ON
     C**********           MOVE *ON       *IN50
     C**********           ENDIF
     ***********
     C********** SCKEY     IFNE *BLANKS
     C**********           MOVE *ON       *IN64
     C**********           MOVELSCKEY     SCCOM3
     ***********
     C**********           ELSE
     C**********           MOVE *ON       *IN50
     ***********
     C**********           ENDIF                           SCKEY IFNE
     ***********
     C**********           MOVEL'SCR01'   Q2RECF    P
     C**********           GOTO STRT
     ***********
     C**********           ENDIF                           HFLD IFEQ
      *
      *  COMMENT CODES SEARCH
      *
     C     HFLD          IFEQ      'SCCOM1'
     C     HFLD          OREQ      'SCCOM2'
     C     HFLD          OREQ      'SCCOM3'
      *
     C                   CALL      'W2191R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      *BLANKS       SCKEY            10
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCCOM1'
     C                   MOVE      *ON           *IN48
      *
     C     HFLD          WHENEQ    'SCCOM2'
     C                   MOVE      *ON           *IN49
      *
     C     HFLD          WHENEQ    'SCCOM3'
     C                   MOVE      *ON           *IN50
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  *INKL=*ON
      *
     C     SCKEY         IFNE      *BLANKS
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCCOM1'
     C                   MOVE      *ON           *IN49
     C                   MOVEL     SCKEY         SCCOM1
      *
     C     HFLD          WHENEQ    'SCCOM2'
     C                   MOVE      *ON           *IN50
     C                   MOVEL     SCKEY         SCCOM2
      *
     C     HFLD          WHENEQ    'SCCOM3'
     C                   MOVE      *ON           *IN64
     C                   MOVEL     SCKEY         SCCOM3
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  SCKEY IFNE
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD IFEQ
      *
      *** END   CHG *** 12/01/97 *************************************
     C*
     C*  EMPLOYER NUMBER/DIVISION SEARCH
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C                   EXSR      SRESRC
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*  POLICY YEAR SEARCH
     C*
     C     HFLD          IFEQ      'SCPYR'
     **
     C     SCEMP#        IFEQ      *ZERO
     C                   EXSR      SRESRC
     **
     C                   ELSE
     **
     **  CALL THE POLICY SEARCH PROGRAM
     **
     C                   EXSR      SRPSRC
     **
      *** BEG DEL *** 05/24/06 **********************************
     C***********          MOVEL'SCR01'   Q2RECF    P
      *** END DEL *** 05/24/06 **********************************
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  SCEMP# IFEQ
     **
     C                   ENDIF                                                  HFLD IFEQ
     **
      *** BEGIN CHG *** 12/01/97 *************************************
     C***TABLE*SEARCH - FROM EMPLOYER NAME
     C**********
     C********** HFLD      IFEQ 'SCFREA'
      **********
     C**********           CALL 'W0050R'
     C********** *INKC     PARM           QQKC    1
     C********** *INKL     PARM           QQKL    1
     C**********           PARM '2'       QQCALL  1
     C**********           PARM 0         QQRRN   90
     C**********           PARM DBRRNE    QQRRN2  90
     C**********           PARM           QQQTE   1
     ***********
     C********** *INKC     IFEQ *ON
     C**********           GOTO TERM
     C**********           ENDIF
     ***********
     ****POSITION CURSOR TO FROM EMPLOYER ALPHA FIELD
     ***********
     C********** *INKL     IFEQ *ON
     C**********           MOVE *ON       *IN54
     C**********           GOTO STRT
     C**********           ENDIF
     ***********
     ***LOAD*EMPLOYER NAME
     ***********
     C********** QQRRN     IFNE *ZERO
     ***********
     C********** QQRRN     CHAINPFRRNE               60
     ***********
     ****POSITION CURSOR TO NEXT INPUT FIELD
     ***********
     C********** *IN60     IFEQ *ZERO
     C**********           MOVELEMNAME    SCFREA
     C**********           MOVE *ON       *IN55
     C**********           ENDIF                           *IN60 IFEQ
     ***********
     ****POSITION CURSOR TO FROM EMPLOYER ALPHA FIELD
     ***********
     C**********           ELSE
     C**********           MOVE *ON       *IN54
     ***********
     C**********           ENDIF                           QQRRN IFNE
     ***********
     C**********           GOTO STRT
     ***********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C***TABLE*SEARCH - TO EMPLOYER NAME
     C**********
     C********** HFLD      IFEQ 'SCTOEA'
     ***********
     C**********           CALL 'W0050R'
     C********** *INKC     PARM           QQKC    1
     C********** *INKL     PARM           QQKL    1
     C**********           PARM '2'       QQCALL  1
     C**********           PARM 0         QQRRN   90
     C**********           PARM DBRRNE    QQRRN2  90
     C**********           PARM           QQQTE   1
     ***********
     ****POSITION CURSOR TO TO EMPLOYER ALPHA FIELD
     ***********
     C********** *INKC     IFEQ *ON
     C**********           GOTO TERM
     C**********           ENDIF
     ***********
     C********** *INKL     IFEQ *ON
     C**********           MOVE *ON       *IN55
     C**********           GOTO STRT
     C**********           ENDIF
     ***********
     ***LOAD*EMPLOYER NAME
     ***********
     C********** QQRRN     IFNE *ZERO
     ***********
     C********** QQRRN     CHAINPFRRNE               60
     ***********
     ****POSITION CURSOR TO NEXT INPUT FIELD
     ***********
     C********** *IN60     IFEQ *ZERO
     C**********           MOVELEMNAME    SCTOEA
     C**********           MOVE *ON       *IN56
     C**********           ENDIF                           *IN60 IFEQ
     ***********
     ****POSITION CURSOR TO TO EMPLOYER ALPHA FIELD
     ***********
     C**********           ELSE
     C**********           MOVE *ON       *IN55
     ***********
     C**********           ENDIF                           QQRRN IFNE
     ***********
     C**********           GOTO STRT
     ***********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C***TABLE*SEARCH - FROM EMPLOYER NUMBER
     C**********
     C********** HFLD      IFEQ 'SCFREN'
     ***********
     C**********           CALL 'W0050R'
     C********** *INKC     PARM           QQKC    1
     C********** *INKL     PARM           QQKL    1
     C**********           PARM '2'       QQCALL  1
     C**********           PARM 0         QQRRN   90
     C**********           PARM DBRRNE    QQRRN2  90
     C**********           PARM           QQQTE   1
     C**********           PARM 2         QQVIEW  10
     C**********           PARM           @@RRN3  90
     ***********
     C********** *INKC     IFEQ *ON
     C**********           GOTO TERM
     C**********           ENDIF
     ***********
     ****POSITION CURSOR TO FROM EMPLOYER NUMBER FIELD
     ***********
     C********** *INKL     IFEQ *ON
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           MOVE *ON       *IN56
     C**********           GOTO STRT
     C**********           ENDIF
     ***********
     ****LOAD*EMPLOYER NUMBER
     ***********
     C********** QQRRN     IFNE *ZERO
     ***********
     C********** QQRRN     CHAINPFRRNE               60
     ***********
     ****POSITION CURSOR TO NEXT INPUT FIELD
     ***********
     C********** *IN60     IFEQ *ZERO
     C**********           MOVELEMEMP#    SCFREN
     C**********           MOVE *ON       *IN57
     C**********           ENDIF                           *IN60 IFEQ
     ***********
     ****POSITION CURSOR TO FROM EMPLOYER NUMBER FIELD
     ***********
     C**********           ELSE
     C**********           MOVE *ON       *IN56
     ***********
     C**********           ENDIF                           QQRRN IFNE
     ***********
     C**********           GOTO STRT
     ***********
     ***********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C***TABLE*SEARCH - TO EMPLOYER NUMBER
     C**********
     C********** HFLD      IFEQ 'SCTOEN'
     ***********
     C**********           CALL 'W0050R'
     C********** *INKC     PARM           QQKC    1
     C********** *INKL     PARM           QQKL    1
     C**********           PARM '2'       QQCALL  1
     C**********           PARM 0         QQRRN   90
     C**********           PARM DBRRNE    QQRRN2  90
     C**********           PARM           QQQTE   1
     C**********           PARM 2         QQVIEW  10
     C**********           PARM           @@RRN3  90
     ***********
     C********** *INKC     IFEQ *ON
     C**********           GOTO TERM
     C**********           ENDIF
     ***********
     ****POSITION CURSOR TO TO EMPLOYER NUMBER FIELD
     ***********
     C********** *INKL     IFEQ *ON
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           MOVE *ON       *IN57
     C**********           GOTO STRT
     C**********           ENDIF
     ***********
     ****LOAD*EMPLOYER NUMBER
     ***********
     C********** QQRRN     IFNE *ZERO
     ***********
     C********** QQRRN     CHAINPFRRNE               60
     ***********
     ****POSITION CURSOR TO NEXT INPUT FIELD
     ***********
     C********** *IN60     IFEQ *ZERO
     C**********           MOVELEMEMP#    SCTOEN
     C**********           MOVE *ON       *IN58
     C**********           ENDIF                           *IN60 IFEQ
     ***********
     ****POSITION CURSOR TO TO EMPLOYER NUMBER FIELD
     ***********
     C**********           ELSE
     C**********           MOVE *ON       *IN57
     ***********
     C**********           ENDIF                           QQRRN IFNE
     ***********
     C**********           GOTO STRT
     ***********
     ***********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C***TABLE*SEARCH - FROM AGENT
     C**********
     C********** HFLD      IFEQ 'SCFRAN'
     ***********
     C**********           CALL 'W0545R'
     C**********           PARM           QQPRDS 40
     C**********           PARM           QQCALL  1
     C**********           PARM XXFND     QQFND   30
     C**********           PARM           QQAGT#  50
     C**********           PARM           QQRTCD  1
     ***********
     C********** QQRTCD    IFEQ 'A'
     C**********           GOTO TERM
     C**********           ENDIF
     ***********
     ****POSITION CURSOR TO FROM AGENT NUMBER FIELD
     ***********
     C********** QQRTCD    IFEQ 'B'
     C**********           MOVE *ON       *IN61
     C**********           GOTO STRT
     C**********           ENDIF
     ***********
     ****POSITION CURSOR TO NEXT INPUT FIELD
     ***********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVELQQAGT#    SCFRAN
     C**********           MOVE *ON       *IN62
     C**********           ENDIF
     ***********
     C**********           GOTO STRT
     ***********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C***TABLE*SEARCH - TO AGENT
     C**********
     C********** HFLD      IFEQ 'SCTOAN'
     ***********
     C**********           CALL 'W0545R'
     C**********           PARM           QQPRDS 40
     C**********           PARM           QQCALL  1
     C**********           PARM XXFND     QQFND
     C**********           PARM           QQAGT#  50
     C**********           PARM           QQRTCD  1
     ***********
     C********** QQRTCD    IFEQ 'A'
     C**********           GOTO TERM
     C**********           ENDIF
     ***********
     ****POSITION CURSOR TO TO AGENT NUMBER FIELD
     ***********
     C********** QQRTCD    IFEQ 'B'
     C**********           MOVE *ON       *IN62
     C**********           GOTO STRT
     C**********           ENDIF
     ***********
     ****POSITION CURSOR TO FIRST INPUT FIELD ON THE SCREEN
     ***********
     C********** QQRTCD    IFEQ '*'
     C**********           MOVELQQAGT#    SCTOAN
     C**********           ENDIF
     ***********
     C**********           GOTO STRT
     ***********
     C**********           ENDIF                           HFLD IFEQ  '
     C**********
     C***TABLE*SEARCH - FROM AGENT NAME
     C**********
     C********** HFLD      IFEQ 'SCFRAA'
     ***********
     C**********           CALL 'W0545R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM XXFND     QQFND
     C**********           PARM           QQAGT#
     C**********           PARM           QQRTCD
     ***********
     C********** QQRTCD    IFEQ 'A'
     C**********           GOTO TERM
     C**********           ENDIF
     ***********
     ****POSITION CURSOR TO FROM AGENT NAME FIELD
     ***********
     C********** QQRTCD    IFEQ 'B'
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           MOVE *ON       *IN58
     C**********           GOTO STRT
     C**********           ENDIF
     ***********
     C********** QQRTCD    IFEQ '*'
     C**********           CALL 'WA002R'
     C**********           PARM XXCO#     @@CO#
     C**********           PARM XXFND     @@FND
     C**********           PARM *ZERO     @@EMP#
     C**********           PARM *ZERO     @@DIV
     C**********           PARM *ZERO     @@FYR
     C**********           PARM           QQAGT#
     C**********           PARM *BLANKS   QQANAM 40
     ***********
     ***********
     ****IF*AGNAME IS FOUND POSITION CURSOR TO NEXT FIELD ELSE POSITION
     ****TO*FROM AGENT ALPHA
     ***********
     C********** QQANAM    IFNE *BLANKS
     C**********           MOVELQQANAM    SCFRAA
     C**********           MOVE *ON       *IN59
     C**********           ELSE
     C**********           MOVE *ON       *IN73
     C**********           MOVE *ON       *IN58
     C**********           ENDIF                           *IN60 IFEQ
     ***********
     C**********           ENDIF
     ***********
     C**********           GOTO STRT
     ***********
     C**********           ENDIF                           HFLD IFEQ
     C**********
     C***TABLE*SEARCH - TO AGENT NAME
     C**********
     C********** HFLD      IFEQ 'SCTOAA'
     ***********
     C**********           CALL 'W0545R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM XXFND     QQFND
     C**********           PARM           QQAGT#
     C**********           PARM           QQRTCD
     ***********
     C********** QQRTCD    IFEQ 'A'
     C**********           GOTO TERM
     C**********           ENDIF
     ***********
     ****POSITION CURSOR TO TO AGENT ALPHA FIELD
     ***********
     C********** QQRTCD    IFEQ 'B'
     C**********           MOVE *ON       *IN59
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           ENDIF
     ***********
     C********** QQRTCD    IFEQ '*'
     C**********           CALL 'WA002R'
     C**********           PARM XXCO#     @@CO#
     C**********           PARM XXFND     @@FND
     C**********           PARM *ZERO     @@EMP#
     C**********           PARM *ZERO     @@DIV
     C**********           PARM *ZERO     @@FYR
     C**********           PARM           QQAGT#
     C**********           PARM *BLANKS   QQANAM 40
     ***********
     ***********
     ****IF*AGNAME IS FOUND POSITION CURSOR TO NEXT FIELD ELSE POSITION
     ****TO*FROM AGENT ALPHA
     ***********
     C********** QQANAM    IFNE *BLANKS
     C**********           MOVELQQANAM    SCTOAA
     C**********           MOVE *ON       *IN61
     C**********           ELSE
     C**********           MOVE *ON       *IN59
     C**********           MOVE *ON       *IN73
     C**********           ENDIF                           *IN60 IFEQ
     ***********
     C**********           ENDIF
     ***********
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     ***********
     C**********           ENDIF                           HFLD IFEQ  '
      *
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  TABLE SEARCH - FROM/TO EMPLOYER NAME/NUMBER
      * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
     C     HFLD          IFEQ      'SCFREA'
     C     HFLD          OREQ      'SCTOEA'
     C     HFLD          OREQ      'SCFREN'
     C     HFLD          OREQ      'SCTOEN'
      *
     C     HFLD          IFEQ      'SCFREA'
     C     HFLD          OREQ      'SCTOEA'
     C                   Z-ADD     1             QQVIEW
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCFREN'
     C     HFLD          OREQ      'SCTOEN'
     C                   Z-ADD     2             QQVIEW
     C                   ENDIF
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRNE        QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C                   PARM                    QQVIEW
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
      *  POSITION CURSOR TO 'HFLD'
      *
     C     *INKL         IFEQ      *ON
     C     QQRRN         OREQ      *ZERO
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCFREA'
     C                   MOVE      *ON           *IN54
      *
     C     HFLD          WHENEQ    'SCTOEA'
     C                   MOVE      *ON           *IN55
      *
     C     HFLD          WHENEQ    'SCFREN'
     C                   MOVE      *ON           *IN56
      *
     C     HFLD          WHENEQ    'SCTOEN'
     C                   MOVE      *ON           *IN57
      *
     C                   ENDSL
      *
     C     *INKL         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL = *ON
      *
     C                   ENDIF                                                  *INKL=*ON OR QQRRN
      *
      * * * * * * * * * * * * * * * * * * * * * * *
      * LOAD EMPLOYER NAME/NUMBER
      * * * * * * * * * * * * * * * * * * * * * * *
      *
     C     QQRRN         IFNE      *ZERO
      *
     C     QQRRN         CHAIN     PFRRNE                             60
      *
      *  POSITION CURSOR TO NEXT INPUT FIELD
      *
     C     *IN60         IFEQ      *ZERO
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCFREA'
     C                   MOVEL     EMNAME        SCFREA
     C                   MOVE      *ON           *IN55
      *
     C     HFLD          WHENEQ    'SCTOEA'
     C                   MOVEL     EMNAME        SCTOEA
     C                   MOVE      *ON           *IN56
      *
     C     HFLD          WHENEQ    'SCFREN'
     C                   MOVEL     EMEMP#        SCFREN
     C                   MOVE      *ON           *IN57
      *
     C     HFLD          WHENEQ    'SCTOEN'
     C                   MOVEL     EMEMP#        SCTOEN
     C                   MOVE      *ON           *IN58
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  *IN60=*ZERO
      *
     C                   ENDIF                                                  QQRRN NE *ZERO
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD=SCFREA
      *
      * * * * * * * * * * * * * * * * * * * * * * * * * *
      *  TABLE SEARCH - FROM/TO AGENT NAME/NUMBER
      * * * * * * * * * * * * * * * * * * * * * * * * * *
     C     HFLD          IFEQ      'SCFRAN'
     C     HFLD          OREQ      'SCTOAN'
     C     HFLD          OREQ      'SCFRAA'
     C     HFLD          OREQ      'SCTOAA'
     **
     C     HFLD          IFEQ      'SCFRAN'
     C     HFLD          OREQ      'SCTOAN'
     C                   Z-ADD     2             QQVIEW
     C                   ENDIF
      *
     C     HFLD          IFEQ      'SCFRAA'
     C     HFLD          OREQ      'SCTOAA'
     C                   Z-ADD     1             QQVIEW
     C                   ENDIF
      *
     C                   CALL      'W0040R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      DBRRNE        QQRRN             9 0
     C                   PARM                    QQVIEW
     **
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=*ON
     **
     C     *INKL         IFEQ      *ON
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCFRAN'
     C                   MOVE      *ON           *IN61
      *
     C     HFLD          WHENEQ    'SCTOAN'
     C                   MOVE      *ON           *IN62
      *
     C     HFLD          WHENEQ    'SCFRAA'
     C                   MOVE      *ON           *IN58
      *
     C     HFLD          WHENEQ    'SCTOAA'
     C                   MOVE      *ON           *IN59
      *
     C                   ENDSL
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *INKL=*ON
      *
      * MOVE VALUES AND POSITION CURSOR TO NEXT INPUT FIELD
      *
     C     QQRRN         IFNE      *ZERO
      *
     C     QQRRN         CHAIN     WMAAP                              60
      *
     C     *IN60         IFEQ      *ZERO
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCFRAN'
     C                   MOVEL(P)  AAAGT#        SCFRAN
     C                   MOVE      *ON           *IN62
      *
     C     HFLD          WHENEQ    'SCTOAN'
     C                   MOVEL(P)  AAAGT#        SCTOAN
     C***  BEGIN CHG  ***  09/03/98  **********************************
     C*******************  MOVE *ON       *IN58
     C                   MOVE      *ON           *IN66
     C***  END   CHG  ***  09/03/98  **********************************
      *
     C     HFLD          WHENEQ    'SCFRAA'
     C                   MOVEL     AANAME        SCFRAA
     C                   MOVE      *ON           *IN59
      *
     C     HFLD          WHENEQ    'SCTOAA'
     C                   MOVEL     AANAME        SCTOAA
     C                   MOVE      *ON           *IN61
      *
     C                   ENDSL
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  *IN60 EQ *ZERO
      *
     C                   ENDIF                                                  QQRRN NE *ZERO
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD IFEQ SCFAGT..
      *
      *** END   CHG *** 12/01/97 *************************************
      *
     C*
     C*  TABLE SEARCH - DARTS REMARK CODE
     C*
     C     HFLD          IFEQ      'SCDRMK'
     **
     C***  Begin Chg  ***  10/11/06  **************************************
     C**********           CALL 'SC580R'
     C**********           PARM *BLANKS   XXDRMK
     C**********           PARM           QQRTCD  1
     C**********
     C********** QQRTCD    IFEQ 'A'
     C**********           GOTO TERM
     C**********           ENDIF
     C**********
     ****POSITION CURSOR TO TO DARTS REMARK FIELD
     ***********
     C********** QQRTCD    IFEQ 'B'
     C**********           MOVE *ON       *IN64
     C**********           ENDIF
     ***********
     ****POSITION CURSOR TO NEXT INPUT FIELD ON SCREEN
     ***********
     C********** XXDRMK    IFNE *BLANK
     C**********           MOVE XXDRMK    SCDRMK
     C**********           MOVE *ON       *IN51
     ***********
     C**********           ELSE
     C**********           MOVE *ON       *IN64
     ***********
     C**********           ENDIF                           XXDRMK IFNE
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN64
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL(P)  ##RMCD        SCDRMK
     C                   MOVE      *ON           *IN51
     C                   OTHER
     C                   MOVE      *ON           *IN64
     C                   ENDSL
     C***  End   Chg  ***  10/11/06  **************************************
     **
      *** BEG DEL *** 05/24/06 **********************************
     C***********          MOVEL'SCR01'   Q2RECF
      *** END DEL *** 05/24/06 **********************************
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C***  BEGIN ADD  ***  09/03/98  **********************************
     C*  DISPLAY ERROR IF NOT A PROPER SEARCH FIELD
     C*
     C     HFLD          IFNE      'SCCOM1'
     C     HFLD          ANDNE     'SCCOM2'
     C     HFLD          ANDNE     'SCCOM3'
     C     HFLD          ANDNE     'SCEMP#'
     C     HFLD          ANDNE     'SCDIV'
     C     HFLD          ANDNE     'SCPYR'
     C     HFLD          ANDNE     'SCFREA'
     C     HFLD          ANDNE     'SCFREN'
     C     HFLD          ANDNE     'SCTOEA'
     C     HFLD          ANDNE     'SCTOEN'
     C     HFLD          ANDNE     'SCFRAA'
     C     HFLD          ANDNE     'SCFRAN'
     C     HFLD          ANDNE     'SCTOAA'
     C     HFLD          ANDNE     'SCTOAN'
     C     HFLD          ANDNE     'SCDRMK'
     C                   MOVE      *ON           *IN71
     C                   ENDIF
     C***  END   ADD  ***  09/03/98  **********************************
     **
      *** BEG DEL *** 05/24/06 **********************************
     C***********          MOVEL'SCR01'   Q2RECF
      *** END DEL *** 05/24/06 **********************************
     **
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  INKJ = *ON
     C*
     C*   COMMAND 12 = RETURN
     C*
     C     *INKL         IFEQ      *ON
      ***** BEG DEL ******* 12/02/99 **********************************
     C*****************    MOVE *ON       *INLR
      ***** END DEL ******* 12/02/99 **********************************
      *** BEG ADD *** 05/24/06 **********************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     Q2RECF        IFEQ      'SCR01'
      *** END ADD *** 05/24/06 **********************************
     C                   GOTO      TERM
      *** BEG ADD *** 05/24/06 **********************************
     C                   ENDIF
      *** END ADD *** 05/24/06 **********************************
     C                   ENDIF
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      *ON
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C                   MOVE      UPCO#         XXCO#
     C                   GOTO      STRT01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*   PROCESS REQUEST SCREEN
      *** BEG ADD *** 05/24/06 **********************************
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
      *** END ADD *** 05/24/06 **********************************
     C*
     C     SCR01T        TAG
     **
     **  LOAD WORK FIELDS FOR KEY LISTS
     **
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     *** BEG ADD *** 10/29/97 *****************************************
     C*  VALIDATE COMPANY AND FUND
     C*
     C     SCFND         IFEQ      *ZERO
     C     UPCO#         OREQ      *ZERO
     C*
     C                   MOVE      *ON           *IN99
     C                   GOTO      STRT
     C*
     C                   ENDIF
     *** END ADD *** 10/29/97 *****************************************
      /free
       //***** beg add ***** 10/06/14 *********************************
       // both installment and policy eff dates cannot be entered.
       If isFMIC = *on and scDate <> *Zero or scDat2 <> *zero;
          If scFDt <> *Zero or scTDt <> *Zero;
             *in82 = *on;
             Exsr procStrt;
          Endif;
       Endif;

       If scDate <> *Zero or isFMIC = *off;
       //****** end add ****** 10/06/14 ******************************
      /end-free
     C*
     C*   VALIDATE THE INSTALLMENT (NOTICE) DATE ENTERED
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN20
      *** BEG DEL *** 05/24/06 **********************************
     C***********          MOVEL'SCR01'   Q2RECF    P
      *** END DEL *** 05/24/06 **********************************
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  04/26/99  **********************************
     C*   Validate the installment 'to' date entered
     C                   Z-ADD     SCDAT2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDAT2
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN19
      *** BEG DEL *** 05/24/06 **********************************
     C***********          MOVEL'SCR01'   Q2RECF    P
      *** END DEL *** 05/24/06 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*   Installment "from date" cannot be greater than "to date"
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            8 0
     C                   Z-ADD     SCDAT2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2            8 0
     C     XXDATE        IFGT      XXDAT2
     C                   MOVE      *ON           *IN43
     C                   GOTO      STRT
     C                   ENDIF                                                  XXDATE>XXDAT2
     C*
     C*   Validate installment "from" and "to" dates are in same month
     C     XXMM          IFNE      X2MM
     C     XXCCYY        ORNE      X2CCYY
     C                   MOVE      *ON           *IN16
     C                   GOTO      STRT
     C                   ENDIF                                                  XXMM<>X2MM
      /free
       //***** beg add ***** 10/06/14 *********************************
       Else;

          // Check From and To date for valid values
          srDtI = scFDt;
          Exsr cvtDate;
          If srDtO <> ' ';
             scFDt = %Int(%trim(srDtO));
          Endif;

          If srErr = *on;
             *in83 = *on;
             Exsr procStrt;
          Endif;

          srDtI = scTDt;
          Exsr cvtDate;
          If srDtO <> ' ';
             scTDt = %Int(%trim(srDtO));
          Endif;

          If srErr = *on;
             *in84 = *on;
             Exsr procStrt;
          Endif;

          // "from date" cannot be greater than "to date"
          intFrDt = 0;
          intToDt = 0;

          srDtI = scFDt;
          Exsr cvtDtCYMD;
          If srDtO <> ' ';
             intFrDt = %Int(%trim(srDtO));
          Endif;

          srDtI = scTDt;
          Exsr cvtDtCYMD;
          If srDtO <> ' ';
             intToDt = %Int(%trim(srDtO));
          Endif;

          If intFrDt > intToDt;
             *in85 = *on;
             Exsr procStrt;
          Endif;
       Endif;
       //****** end add ***** 10/06/14 **************************************
      /end-free
     C*
     C*   Validate entry for 'Generate renewal, monthly or all'
     C     SCGRMA        IFNE      'R'
     C     SCGRMA        ANDNE     'M'
     C     SCGRMA        ANDNE     'A'
      *** beg add *** 11/22/13 **********************************
     C     SCGRMA        ANDNE     'B'
      *** end add *** 11/22/13 **********************************
     C                   MOVE      *ON           *IN18
      *** BEG DEL *** 05/24/06 **********************************
     C***********          MOVEL'SCR01'   Q2RECF    P
      *** END DEL *** 05/24/06 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCGRMA<>'R'
     C*
      *** beg chg *** 11/22/13 **********************************
     C*********   If SCGRMA = 'A' then installment from must equal installment to date
     C*********     SCGRMA        IFEQ      'A'
     C*   If SCGRMA = 'B' then installment from must equal installment to date
     C     SCGRMA        IFEQ      'B'
      *** end chg *** 11/22/13 **********************************
     C*
     C     SCDATE        IFNE      SCDAT2
     C                   MOVE      *ON           *IN17
      *** BEG DEL *** 05/24/06 **********************************
     C***********          MOVEL'SCR01'   Q2RECF    P
      *** END DEL *** 05/24/06 **********************************
     C                   GOTO      STRT
     C                   ENDIF                                                  SCDATE<>SCDAT2
     C*
     C                   ENDIF                                                  SCGRMA='A'
     C***  END   ADD  ***  04/26/99  **********************************
     C*
     C*
     C***  BEGIN CHG  ***  04/26/99  **********************************
     C*** VALIDATE THE DUE DATE DATE ENTERED
     C**********
     C**********           Z-ADDSCDUDT    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCDUDT
     C********** SRERR     IFEQ *ON
     C**********           MOVE *ON       *IN44
     C**********           MOVEL'SCR01'   Q2RECF    P
     C**********           GOTO STRT
     C**********           ENDIF
     C*
     C*   Validate the Add'l Days From Installment Due
     C     SCADDU        IFLT      *ZERO
     C                   MOVE      *ON           *IN44
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C***  END   CHG  ***  04/26/99  **********************************
     C*
     C*   EXCLUDE DEDUCTIBLE FEES MUST BE "Y" OR "N"
     C*
     C     SCEXDF        IFNE      'Y'
     C     SCEXDF        ANDNE     'N'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  12/22/98  **********************************
     C*  Validate Interim days
     C     SCIDYS        IFLT      *ZERO
     C                   MOVE      *ON           *IN72
     C                   GOTO      STRT
     C                   ENDIF
     C***  END   ADD  ***  12/22/98  **********************************
     C*
     C*   VALIDATE COMMENT FIELD 1
     C*
     C     SCCOM1        IFNE      *BLANK
     C                   MOVEL(P)  SCCOM1        XXKEY            10
     C     TBKEY         CHAIN     PBC01                              22
     C     *IN22         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*   VALIDATE COMMENT FIELD 2
     C*
     C     SCCOM2        IFNE      *BLANK
     C                   MOVEL(P)  SCCOM2        XXKEY
     C     TBKEY         CHAIN     PBC01                              23
     C     *IN23         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*   VALIDATE COMMENT FIELD 3
     C*
     C     SCCOM3        IFNE      *BLANK
     C                   MOVEL(P)  SCCOM3        XXKEY
     C     TBKEY         CHAIN     PBC01                              24
     C     *IN24         IFEQ      *ON
     C     BCSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     **
     ** VALIDATE THE DARTS REMARK CODE
     **
     C     SCDRMK        IFNE      *BLANKS
     C     SCDRMK        CHAIN     PCT01                              47
     C     *IN47         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      *** BEG ADD ***** 01/06/06 **********************************
      *
      * Validate the "Resolve to DARTS" flag
      *
     C     SCDRES        IFNE      'Y'
     C     SCDRES        ANDNE     'N'
     C                   SETON                                        6869
     C                   GOTO      STRT
     C                   ENDIF
      *
      * If "Resolve to DARTS" flag = 'Y', then user most select a valid
      * DARTS remark.
      *
     C     SCDRES        IFEQ      'Y'
     C     SCDRMK        CHAIN     PCT01                              47
     C     *IN47         IFEQ      *ON
     C                   SETON                                        64
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      *** END ADD ***** 01/06/06 **********************************
     C*
      *** BEG ADD ***** 03/07/05 **********************************
     C*
     C* MINIMUM BALANCE !< 0
     C*
     C     SCMINB        IFLT      0
     C                   MOVE      *ON           *IN74
      *** BEG ADD *** 05/24/06 **********************************
     C                   GOTO      STRT
      *** END ADD *** 05/24/06 **********************************
     C                   ENDIF
      *** END ADD ***** 03/07/05 **********************************
     C*
      *** BEG ADD *** 05/24/06 **********************************
     C     SCEXP         IFNE      'Y'
     C     SCEXP         ANDNE     'N'
     C                   MOVE      *ON           *IN75
     C                   GOTO      STRT
     C                   ENDIF
     C     SCDEP         IFNE      'Y'
     C     SCDEP         ANDNE     'N'
     C                   MOVE      *ON           *IN76
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCR02T        TAG
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
      *** END ADD *** 05/24/06 **********************************
     C*
     C*  VALIDATE ONLY ONE SET OF FIELDS ENTERED
     C*
     C     SCPYR         IFNE      *ZERO
     C     SCEMP#        ORNE      *ZERO
     C     SCDIV         ORNE      *ZERO
      *** BEGIN CHG *** 12/01/97 *************************************
     C********** SCFRDT    IFNE *ZERO
     C********** SCTODT    ORNE *ZERO
     C********** SCSRT1    ORNE *BLANK
     C     SCSRT1        IFNE      *BLANK
      *** END   CHG *** 12/01/97 *************************************
     C                   MOVE      *ON           *IN39
     C                   GOTO      STRT
     C                   ENDIF                                                  SCSRT1 NE *BLNK
     C                   ENDIF                                                  SCPYR NE *ZERO
     C*
     C*  VALIDATE EITHER INDIVIDUAL EMPLOYER ENTERED OR LARGE RUN
     C*
     C     SCPYR         IFEQ      *ZERO
     C     SCEMP#        ANDEQ     *ZERO
     C     SCDIV         ANDEQ     *ZERO
      *** BEGIN DEL *** 12/01/97 ************************************
     C********** SCFRDT    ANDEQ*ZERO
     C********** SCTODT    ANDEQ*ZERO
      *** END   DEL *** 12/01/97 ************************************
     C     SCSRT1        ANDEQ     *BLANK
     C                   MOVE      *ON           *IN40
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  VALIDATIONS FOR SINGLE EMPLOYER RUN
     C*
     C     SCPYR         IFNE      *ZERO
     C     SCEMP#        ORNE      *ZERO
     C     SCDIV         ORNE      *ZERO
     C*
     C*  VALIDATE THE EMPLOYER/DIV ENTERED
     C*
     C                   MOVE      SCEMP#        XXEMP#
     C     EMKY          CHAIN     PEM01                              26
     C     *IN26         CABEQ     *ON           STRT
     C*
     C                   MOVE      SCDIV         XXDIV
     C     EM1KY         CHAIN     PEM01                              45
     C     *IN45         CABEQ     *ON           STRT
     C*
     C*  VALIDATE THE POLICY YEAR ENTERED
     C*
     C                   Z-ADD     SCPYR         XXPYR
     C     ELKY          CHAIN     PEL01                              25
     C     *IN25         CABEQ     *ON           STRT
     C*
     C**** BEGIN ADD **** 04/06/01 ************************************
     C*
     C* PREMIUM NOTICE CANNOT BE RUN FOR SINGLE EMPLOYER IF AGENCY BILL
     C*
     C                   MOVE      *HIVAL        XXFYR             3 0
     C     AHKY          SETGT     PAH01
     C     AHKY1         READPE    PAH01                                  60
     C*** BEGIN ADD ***  07/02/08  *********************************************
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        ANDNE     'A'
     C     AHKY1         READPE    PAH01                                  60
     C                   ENDDO
     C*** END ADD *****  07/02/08  *********************************************
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      *HIVAL        XXEFDT            7 0
     C     ARKY1         SETGT     PAR01
     C     ARKY          READPE    PAR01                                  61
     C*
      *** BEG CHG **** 10/23/01 ************************************************
     C*********  *IN61     DOUEQ*OFF
     C*********  ARSTAT    ANDNE'I'
     C     *IN61         DOWEQ     *OFF
     C     ARSTAT        ANDNE     'A'
      *** END CHG **** 10/23/01 ************************************************
     C     ARKY          READPE    PAR01                                  61
     C                   ENDDO
     C*
      *** BEG CHG **** 10/23/01 ************************************************
     C*********  ARPLAN    IFEQ 'A'
     C     *IN61         IFEQ      *OFF
     C     ARPLAN        ANDEQ     'A'
      *** END CHG **** 10/23/01 ************************************************
     C                   SETON                                            67
     C                   GOTO      STRT
     C                   ENDIF                                                  ARPLAN = A
     C*
     C                   ENDIF                                                  *IN60 = OFF
     C**** END   ADD **** 04/06/01 ************************************
      *** BEGIN ADD *** 12/01/97 *************************************
     C*  SET FROM / TO DATES TO *BLANK
      *
     C                   MOVE      *BLANKS       XXFRDT            8
     C                   MOVE      *BLANKS       XXTODT            8
      *** END   ADD *** 12/01/97 *************************************
     C                   GOTO      CALLIT
     **
     C                   ENDIF                                                  SCPYR IFNE 0
     C*
      *** BEGIN DEL *** 12/01/97 *************************************
     C** VALIDATE THE POLICY EFFECTIVE FROM DATE
     C********************
     C*******************  Z-ADDSCFRDT    SRDTI
     C*******************  CALL 'SRCVTDAT'DATCHK
     C******************** MOVELSRDTO     SCFRDT
     C********** SRERR     IFEQ *ON
     C******************** MOVE *ON       *IN41
     C******************** GOTO STRT
     C******************** ENDIF
     C******************
     C** VALIDATE THE POLICY EFFECTIVE TO DATE
     C********************
     C******************** Z-ADDSCTODT    SRDTI
     C******************** CALL 'SRCVTDAT'DATCHK
     C******************** MOVELSRDTO     SCTODT
     C********** SRERR     IFEQ *ON
     C******************** MOVE *ON       *IN42
     C******************** GOTO STRT
     C******************** ENDIF
     C*******************
     C** POL EFF DATE CANNOT BE GREATER THAN POL EFF TO DATE
     C*******************
     C*******************  Z-ADDSCFRDT    SRDTI
     C******************** CALL 'SRCVTDAT'DTCYMD
     C*******************  MOVELSRDTO     XXFRDT  80
     C******************** Z-ADDSCTODT    SRDTI
     C******************** CALL 'SRCVTDAT'DTCYMD
     C******************** MOVELSRDTO     XXTODT  80
     C********** XXFRDT    IFGT XXTODT
     C******************** MOVE *ON       *IN43
     C******************** GOTO STRT
     C******************** ENDIF
      *** END   DEL *** 12/01/97 *************************************
     C*
     C*  VALIDATE SORT SEQUENCE ONE
     C*
     C     SCSRT1        IFEQ      *BLANK
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCSRT1        IFNE      'EA'
     C     SCSRT1        ANDNE     'EN'
     C     SCSRT1        ANDNE     'AA'
     C     SCSRT1        ANDNE     'AN'
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  VALIDATE THE SECONDARY SORT FIELD
     C*
     C*  SORT2 MUST BE BLANK IF EMPLR #/NAME AS PRIMARY
     C*
     C     SCSRT1        IFEQ      'EA'
     C     SCSRT1        OREQ      'EN'
     C     SCSRT2        IFNE      *BLANK
     C                   MOVE      *ON           *IN37
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*  SORT2 MUST BE EMPLR #/NAME IF AGENT #/NAME AS PRIMARY
     C*
     C     SCSRT1        IFEQ      'AA'
     C     SCSRT1        OREQ      'AN'
     C     SCSRT2        IFNE      'EA'
     C     SCSRT2        ANDNE     'EN'
     C                   MOVE      *ON           *IN38
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C***  BEGIN DEL  ***  04/26/99  **********************************
      **** BEGIN ADD **** 11/30/98 *********************************************
     C********** SCPEFD    IFNE 0
     C********** SCPETD    ORNE 0
      **********
      **VALIDATE THE POLICY EFFECTIVE FROM DATE
      **********
     C**********           Z-ADDSCPEFD    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCPEFD
     C********** SRERR     IFEQ *ON
     C**********           MOVE *ON       *IN41
     C**********           GOTO STRT
     C**********           ENDIF
      **********
      **VALIDATE THE POLICY EFFECTIVE TO DATE
      **********
     C**********           Z-ADDSCPETD    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCPETD
     C********** SRERR     IFEQ *ON
     C**********           MOVE *ON       *IN42
     C**********           GOTO STRT
     C**********           ENDIF
      **********
      **POL EFF DATE CANNOT BE GREATER THAN POL EFF TO DATE
      **********
     C**********           Z-ADDSCPEFD    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXPEFD  70
     C**********           Z-ADDSCPETD    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXPETD  70
     C********** XXPEFD    IFGT XXPETD
     C**********           MOVE *ON       *IN43
     C**********           GOTO STRT
     C**********           ENDIF
      **********
     C**********           ENDIF                           SCPEFD IFNE 0
      **** END   ADD **** 11/30/98 *********************************************
     C***  END   DEL  ***  04/26/99  **********************************
     C*
     C*  VALIDATE THE EMPLOYER SORT RANGES
     C*
     C     SCTOEA        IFNE      *BLANK
     **
     **    CONVERT TO UPPER CASE
     **
     C     LO:UP         XLATE(P)  SCFREA        SCFREA
     C     LO:UP         XLATE(P)  SCTOEA        SCTOEA
     **
     C     SCTOEN        IFNE      *ZERO
     C                   MOVE      *ON           *IN31
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*  VALIDATE THE AGENT SORT RANGES
     C*
     C     SCTOAA        IFNE      *BLANK
     **
     **    CONVERT TO UPPER CASE
     **
     C     LO:UP         XLATE(P)  SCFRAA        SCFRAA
     C     LO:UP         XLATE(P)  SCTOAA        SCTOAA
     **
     C     SCTOAN        IFNE      *ZERO
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*  VALIDATE THE LOW RANGE IS NOT GREATER THAN THE HIGH RANGE
     C*
     C     SCFREA        IFGT      SCTOEA
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF
     C     SCFREN        IFGT      SCTOEN
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C                   ENDIF
     C     SCFRAA        IFGT      SCTOAA
     C                   MOVE      *ON           *IN36
     C                   GOTO      STRT
     C                   ENDIF
     C     SCFRAN        IFGT      SCTOAN
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF
     C*
      *** BEGIN ADD *** 12/01/97 *************************************
      *  SET FROM / TO DATES TO SCDATE
      *
      /free
       //****** beg add ****** 10/06/14 *******************************
       If scDate <> *Zero;
       //****** end add ****** 10/06/14 *******************************
      /end-free
     C                   MOVE      SCDATE        XXFRDT            8
     C***  BEGIN CHG  ***  04/26/99  **********************************
     C*******************  MOVE SCDATE    XXTODT  8
     C                   MOVE      SCDAT2        XXTODT            8
     C***  END   CHG  ***  04/26/99  **********************************
      *** END   ADD *** 12/01/97 *************************************
      /free
       //****** beg add ****** 10/06/14 *******************************
       Else;
          xxFrDt = %EditC(scFDt:'X');
          xxToDt = %EditC(scTDt:'X');
       Endif;
       //****** end add ****** 10/06/14 *******************************
      /end-free
      *
     C     CALLIT        TAG
      *** BEGIN ADD *** 10/01/08 *************************************
     C*
     C*  VALIDATE TYPE OF OUTPUT(P, F & B')
     C*
     C     SCPFB         IFEQ      *BLANK
      *** BEG CHG ****** 09/07/09 ************************************
     C*********  CLTID     ANDEQ'132'
      *** END CHG ****** 09/07/09 ************************************
     C                   MOVE      *ON           *IN77
     C                   GOTO      STRT
     C                   ENDIF
     C*
      *** BEG CHG ****** 09/07/09 ************************************
     C*********  CLTID     IFEQ '132'
     C*********  SCPFB     ANDNE'P'
     C     SCPFB         IFNE      'P'
      *** END CHG ****** 09/07/09 ************************************
     C     SCPFB         ANDNE     'F'
     C     SCPFB         ANDNE     'B'
     C                   MOVE      *ON           *IN78
     C                   GOTO      STRT
     C                   ENDIF
     C***  END   ADD  ***  10/01/08  **********************************
      *
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQ1FND            3
      /free
       //***** beg add ***** 10/06/14 *********************************
       If scDate <> *Zero;
       //***** end add ***** 10/06/14 *********************************
      /end-free
     C                   MOVE      SCDATE        QQDATE            8
     C***  BEGIN CHG  ***  04/26/99  **********************************
     C                   MOVE      SCDAT2        QQDAT2            8
      /free
       //***** beg add ***** 10/06/14 *********************************
          qqAll = ' ';
       Else;
          qqDate = %EditC(scFDt:'X');
          qqDat2 = %EditC(scTDt:'X');
          qqAll = 'Y';
       Endif;
       //***** beg add ***** 10/06/14 *********************************
      /end-free
     C*******************  MOVE SCDUDT    QQDUDT  8
     C                   MOVE      SCADDU        QQADDU            3
     C***  END   CHG  ***  04/26/99  **********************************
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      SCFREA        QQFREA            2
     C                   MOVE      SCTOEA        QQTOEA            2
     C                   MOVE      SCFREN        QQFREN            9
     C                   MOVE      SCTOEN        QQTOEN            9
     C                   MOVE      SCFRAA        QQFRAA            2
     C                   MOVE      SCTOAA        QQTOAA            2
     C                   MOVE      SCFRAN        QQFRAN            5
     C                   MOVE      SCTOAN        QQTOAN            5
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVEL     SCEXDF        QQEXDF            1
     C                   MOVEL     SCCOM1        QQCOM1            2
     C                   MOVEL     SCCOM2        QQCOM2            2
     C                   MOVEL     SCCOM3        QQCOM3            2
      *** BEGIN CHG *** 12/01/97 *************************************
     C******************** MOVE SCFRDT    QQFRDT  8
     C******************** MOVE SCTODT    QQTODT  8
     C                   MOVE      XXFRDT        QQFRDT            8
     C                   MOVE      XXTODT        QQTODT            8
      *** END   CHG *** 12/01/97 *************************************
     C                   MOVE      XXPYR         QQPYR             3
     C                   MOVE      SCSRT1        QQSRT1            2
     C                   MOVE      SCSRT2        QQSRT2            2
     C                   MOVE      SCDRMK        QQDRMK
     C***  BEGIN DEL  ***  04/26/99  **********************************
      *****BEGIN*ADD **** 11/30/98 *********************************************
     C**************       MOVE SCPEFD    QQPEFD  8
     C**************       MOVE SCPETD    QQPETD  8
      *****END***ADD **** 11/30/98 *********************************************
     C***  END   DEL  ***  04/26/99  **********************************
     C*
     C***  BEGIN ADD  ***  12/22/98  **********************************
     C                   MOVE      SCIDYS        QQIDYS            3
     C***  END   ADD  ***  12/22/98  **********************************
     C***  BEGIN ADD  ***  04/26/99  **********************************
     C                   MOVE      SCGRMA        QQGRMA            1
     C***  END   ADD  ***  04/26/99  **********************************
     C***  BEG ADD ******  03/07/05  **********************************
     C                   MOVE      SCMINB        QQMINB            7
     C***  END ADD ******  03/07/05  **********************************
     C***  BEG ADD ******  10/01/08  **********************************
     C                   MOVE      SCPFB         QQPFB             1
     C***  END ADD ******  10/01/08  **********************************
     C*
     C                   CALL      'W7320C'
     C                   PARM                    QQCO#
     C                   PARM                    QQ1FND
     C                   PARM                    QQDATE
     C                   PARM                    QQDAT2
     C                   PARM                    QQADDU
     C                   PARM                    QQEXDF
     C                   PARM                    QQCOM1
     C                   PARM                    QQCOM2
     C                   PARM                    QQCOM3
     C                   PARM                    QQPYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQSRT1
     C                   PARM                    QQSRT2
     C                   PARM                    QQFREA
     C                   PARM                    QQTOEA
     C                   PARM                    QQFRAA
     C                   PARM                    QQTOAA
     C                   PARM                    QQFREN
     C                   PARM                    QQTOEN
     C                   PARM                    QQFRAN
     C                   PARM                    QQTOAN
     C                   PARM                    QQOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   PARM                    QQDRMK
     C                   PARM                    QQIDYS
     C                   PARM                    QQGRMA
     C                   PARM                    QQMINB
     C                   PARM      SCDRES        QQDRES            1
     C                   PARM      SCDEP         QQDEP             1
     C                   PARM      SCEXP         QQEXP             1
     C                   PARM      SCPFB         QQPFB             1
      *** beg add *** 11/22/13 **********************************
     C                   PARM                    qqMass            1
     C                   PARM      scSupBalP     qqSupBalP         1
      *** end add *** 11/22/13 **********************************
      ***** beg add ***** 10/06/14 ************************************
     C                   Parm                    qqAll
      ***** end add ***** 10/06/14 ************************************
      ***** Beg Add ***** 04/14/23 ************************************
     C                   Parm                    @defInv#
      ***** End Add ***** 04/14/23 ************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
      *
     C     Q1PRMS        IFGT      2
     C                   MOVE      *INKC         @@KC
     C                   ENDIF
      *
     C     Q1PRMS        IFGT      3
     C                   MOVE      *INKL         @@KL
     C                   ENDIF
      *
     C                   MOVE      *ON           *INLR
      *
      *
      *
     ******************************************************************
     **  SUBROUTINE TO SEARCH FOR AND RETURN AN EMPLOYER NUMBER       *
     ******************************************************************
     **
     C     SRESRC        BEGSR
     **
     C                   CALL      'W0050R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRNE        QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
      *** BEG DEL *** 05/24/06 **********************************
     C***********          MOVEL'SCR01'   Q2RECF    P
      *** END DEL *** 05/24/06 **********************************
     **
     **  POSITION CURSOR TO CORRECT FIELD
     **
     C                   SELECT
     C     HFLD          WHENEQ    'SCEMP#'
     C                   MOVE      *ON           *IN51
     **
     C     HFLD          WHENEQ    'SCDIV'
     C                   MOVE      *ON           *IN52
     **
     C     HFLD          WHENEQ    'SCPYR'
     C                   MOVE      *ON           *IN53
     **
     C                   ENDSL
     **
     C                   GOTO      STRT
     **
     C                   ENDIF                                                  *INKL IFEQ
     **
     ** LOAD EMPLOYER NUMBER AND DIVISION
     **
     C     QQRRN         IFNE      *ZERO
     **
     C     QQRRN         CHAIN     PFRRNE                             60
     **
     C     *IN60         IFEQ      *ZERO
     C                   MOVEL     EMEMP#        SCEMP#
     C                   MOVEL     EMDIV         SCDIV
     **
     **  CALL POLICY YEAR SEARCH
     **
     C                   EXSR      SRPSRC
     **
     **  POSITION CURSOR TO CORRECT FIELD
     **
     C     SCPYR         IFEQ      *ZERO
     C                   MOVE      *ON           *IN54
     C                   ENDIF                                                  *IN60 IFEQ
     **
     C                   ENDIF                                                  *IN60 IFEQ
     **
     C                   ELSE
     **
     **  POSITION CURSOR TO CORRECT FIELD
     **
     C                   SELECT
     C     HFLD          WHENEQ    'SCEMP#'
     C                   MOVE      *ON           *IN51
     **
     C     HFLD          WHENEQ    'SCDIV'
     C                   MOVE      *ON           *IN52
     **
     C     HFLD          WHENEQ    'SCPYR'
     C                   MOVE      *ON           *IN53
     **
     C                   ENDSL
     **
     C                   ENDIF                                                  QQRRN IFNE
     **
      *** BEG DEL *** 05/24/06 **********************************
     C***********          MOVEL'SCR01'   Q2RECF    P
      *** END DEL *** 05/24/06 **********************************
     C                   GOTO      STRT
     **
     C                   ENDSR
     ******************************************************************
     **  POLICY SEARCH SUBROUTINE                                    **
     ******************************************************************
     C     SRPSRC        BEGSR
     C*
     C*  VALIDATE THE EMPLOYER/DIV ENTERED
     C*
     C                   MOVE      SCEMP#        XXEMP#
     C     EMKY          CHAIN     PEM01                              26
     C     *IN26         CABEQ     *ON           STRT
     C*
     C                   MOVE      SCDIV         XXDIV
     C     EM1KY         CHAIN     PEM01                              45
     C     *IN45         CABEQ     *ON           STRT
     **
     C                   CALL      'W0051R'
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '2'           QQCALL            1
     C                   PARM      0             QQRRN             9 0
     C                   PARM      DBRRND        QQRRN2            9 0
     C                   PARM                    QQQTE             1
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
      *** BEG DEL *** 05/24/06 **********************************
     C***********          MOVEL'SCR01'   Q2RECF    P
      *** END DEL *** 05/24/06 **********************************
     C                   MOVE      *ON           *IN53
     C                   GOTO      STRT
     C                   ENDIF
     **
     ** LOAD POLICY YEAR
     **
     C     QQRRN         IFNE      *ZERO
     **
     C     QQRRN         CHAIN     PFRRND                             60
     **
     **  MOVE VALUE TO SCREEN FIELD AND POSITION TO NEXT ENTRY FIELD
     **
     C     *IN60         IFEQ      *ZERO
     C                   MOVEL     ELFYR         SCPYR
     C                   MOVE      *ON           *IN65
     C                   ENDIF                                                  *IN60 IFEQ
     **
     **  POSITION CURSOR TO POLICY YEAR FIELD ON SCREEN
     **
     C                   ELSE
     C                   MOVE      *ON           *IN53
     **
     C                   ENDIF                                                  QQRRN IFNE
     **
     C                   ENDSR
      ****** beg add ***** 10/06/14 ****************************
      **********************************************************
      * CvtDate - Convert Date and Check Date validity
      **********************************************************
     c     cvtDate       Begsr
     c                   Call      'SRCVTDAT'    datChk
     c                   Endsr
      **********************************************************
      * CvtDtCYMD -  Convert Date into CYMD
      **********************************************************
     c     cvtDtCYMD     Begsr
     c                   Call      'SRCVTDAT'    dtCYMD
     c                   Endsr
      **********************************************************
      * ProcStrt - Process from start
      **********************************************************
     c     procStrt      Begsr
     C                   GoTo      Strt
     c                   Endsr
      ****** end add ***** 10/06/14 ****************************
