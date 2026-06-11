     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H*****************************************************************
     H option(*nodebugio:*srcstmt)
      *** Begin Change *** 11/12/20 *******************************
     H***dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) ACTGRP(*caller)
      *** End   Change *** 11/12/20 *******************************
     H/define CONVERT
     H****************************************************************
     H/TITLE W0069R - CLAIM/DARTS CONTROL MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*  THIS PROGRAM WILL ALLOW THE USER TO SETUP VARIOUS SECURITY   *
     H*  LEVELS AND OTHER CLAIMS AND DARTS CRITERIA.                  *
     H*                                                               *
     H*  PROGRAMMER: C:42                                             *
     H*  CREATED:    12/27/95          PROJECT: WC451                 *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/29/22 C:340 Project WC2823                                 *
     H*                FROI Data Field Updates                        *
     H*                                                               *
     H* 09/09/21 C:079 Support 140/97343                              *
     H*                Condition call to FA700R                       *
     H*                                                               *
     H* 11/12/20 C:180 Project 133WRV                                 *
     H*                Add IDC default values                         *
     H*                                                               *
     H* 07/16/18 C:062 Project 133WQO                                 *
     H*                Add auto-diary on add of additional payee      *
     H*                                                               *
     H* 03/27/18 C:180 Support 69834                                  *
     H*                Remove Conversion Claim Edit.                  *
     H*                                                               *
     H* 09/01/17 C:180 Project: 114MHV                                *
     H*                Add CVT Claim Number displays on EOB flag.     *
     H*                                                               *
     H* 05/10/17 C:212 Project: 90MHY                                 *
     H*                Make PBM fields visibility client flagged      *
     H*                                                               *
     H* 02/14/17 C:212 Project: 90MHY                                 *
     H*                PBM interface                                  *
     H*                Add PBM Level/Type/Eligibility Flag etc fields *
     H*                and related processing                         *
     H*                                                               *
     H* 06/22/16 C:307 Support: 42119                                 *
     H*                Initialized checkbox  to 'N' on screen 02 to  *
     H*                handle validation.                             *
     H*                                                               *
     H* 09/04/14 C:279 Project: 114WOE                                *
     H*                Field added to 4th screen for Claim Handling   *
     H*                Instructions.                                  *
     H*                                                               *
     H* 07/28/14 C:295 Project: 114WMD                                *
     H*                 Store the DART setup for Claim Action plan at *
     H*                 Group Level.                                  *
     H*                                                               *
     H* 11/25/13  C:282  Project: 129WSC                              *
     H*                  Add the field for the Heading for Legal      *
     H*                  Reserve on SCR02.                            *
     H*                  Add the field for the Apportionment remark   *
     H*                  code on SCR03.                               *
     H*                  For Client '129'.                            *
     H*                                                               *
     H* 08/29/13  C:279  Project: WC2213                              *
     H*                  New field added to 7th screen for AWW        *
     H*                  required.                                    *
     H*                                                               *
     H* 04/13/12 C:277 SUPPORT: 79699                                 *
     H*                Don't show field "NCCI Medical Data Call" if   *
     H*                System flag is not set to "Y".                 *
     H*                                                               *
     H* 03/16/12  C:282  Project: 133WML                              *
     H*                  New fields added to 5th screen for Removal   *
     H*                  of CMS submission flag.                      *
     H*                                                               *
     H* 02/12/12  C:189  SEQ79127                                     *
     H*                  5th screen should be displayed only if       *
     H*                  CMS module flag WCFL46 is set                *
     H*                                                               *
     H* 07/27/11 C:190 Project: 114WZT                                *
     H*                Add new fields so that it can determine if     *
     H*                the fields should be required/ not required    *
     H*                during Claim entry and maintenance mode.       *
     H*                                                               *
     H* 06/01/11  C:189  Project: 25WPW                               *
     H*                  Add wage calc DARTS field.                   *
     H*                  Move 114MZL field to new screen              *
     H*                                                               *
     H* 06/23/11  C:189  SY255-PRISS 27                               *
     H*                  7th screen should be displayed always irresp-*
     H*                  ective of the Open/Close setup in general    *
     H*                  table file                                   *
     H*                                                               *
     H* 05/05/11 C:235 Project: 114MZL                                *
     H*                Exclude Groups from NCCI Medical Data Call     *
     H*                                                               *
     H*  3/28/11  C: 58  Project: 133WKZ                              *
     H*                  Add Flags: require DOH, DOB, Gender at claim *
     H*                  entry.                                       *
     H*                                                               *
     H*                                                               *
     H* 12/13/10 C:198 Project: WC1963                                *
     H*                Added processing for the new field SCMBQF      *
     H*                on SCR05.                                      *
     H*                                                               *
     H* 12/02/10 C:235 Project: 133WKI                                *
     H*                Added a new screen that will be the 2nd screen *
     H*                of Miscellaneous information.                  *
     H*                                                               *
     H* 06/15/10 C:198 Project: 25WPT                                 *
     H*                fixed the issue where changes till SCR05 are   *
     H*                 not being retained.                           *
     H*                                                               *
     H* 05/18/10 C:198 Project: WC1879                                *
     H*                Added edits for re-submission authority.       *
     H*                                                               *
     H* 04/16/10 C:198 Project: WC1819 Phase 3 Changes                *
     H*                Corrected display of claim input fields on     *
     H*                SCR05 on WW Groups-Inquiry - RECOMPILED.       *
     H*                                                               *
     H* 02/16/10 C:198 Project: WC1819 Phase 5 change                 *
     H*                Fixed cursor movement issue.                   *
     H*                Changed error check on Query specific/diary    *
     H*                user.                                          *
     H*                                                               *
     H* 01/18/10 C:247 Project: 25WPT - Litigation Scrn tracking      *
     H*                                 Attorney Fees                 *
     H*                Added SCR06 for Attorney PV1 Codes             *
     H*
     H* 08/14/09 C:232 Project: WC1819 Phase 5.                       *
     H*                Added SCR05 for Medicare Query Response        *
     H*                                                               *
     H* 07/16/09 C:198 Project: WC1819                                *
     H*                Added SCR05 for CMS Medicare Diaries.          *
     H*                                                               *
     H* 07/15/09 C:189 Project: WC1770                                *
     H*                Medicare Secondary Payer                       *
     H*                                                               *
     H* 07/21/09 C:62  Project: 114WTT                                *
     H*                Add Devereux Group Processing.                 *
     H*                                                               *
     H* 07/14/09 C:189 Project: 114WOJ                                *
     H*                  Change of Address Tracking/Force new Darts   *
     H*                  Remark                                       *
     H*                                                               *
     H* 05/20/09 C:33  Project: WC1770                                *
     H*                Medicare Secondary Payer                       *
     H*                                                               *
     H* 08/28/08 C:136 SUPPORT: DR139 64774                           *
     H*                MAKE DECISION DATE ON SCR03 OPTIONAL BYY       *
     H*                CHANGING BLNK,10 TO 'N'                        *
     H*                                                               *
     H* 10/19/07 C:191 Project: WC1584                                *
     H*              - Add Default EDI mailing date increment         *
     H*                                                              *
     H* 09/20/07 C:062 SEQ#10 : MC300                                 *
     H*                ADD FLAG TO CONTROL WHICH GROUPS TO INCLUDE    *
     H*                IN THE EXTRACT                                 *
     H*                                                               *
     H* 08/01/07 C:183 SEQ#10 : 114WLY                                *
     H*                Changes to make "RELEASE EXACT DUPLICATE       *
     H*                PAYMENT" and 'POSSIBLE DUPLICATE PAYMENT"      *
     H*                security fields optional.                      *
     H*                                                              *
     H* 05/07/07 C:191 Project: WC1438                                *
     H*                Add flag to Claims DARTS control screen for    *
     H*                "Claim Diaries access summary screen(Y/)"      *
     H*                                                              *
     H* 04/26/07 C:132 Project: 133WEB                                *
     H*              - Add Possible Duplicate Payment fields in the   *
     H*                Security Section.                              *
     H*              - Change Add Duplicate payment fields from       *
     H*                114WLY to have Exact Duplicate veribage        *
     H*                                                              *
     H* 01/25/07 C:183 Project: 114WLY                                *
     H*                Add Duplicate Payment fields in the Security   *
     H*                Section.                                       *
     H*                                                               *
     H*  10/11/06  C:33  Seq#: WA099/41801                            *
     H*                  Use SC305R instead of SC580R.                *
     H*                                                               *
     H* 01/24/06 C:168 Project: 132WBU                                *
     H*                Add security lvl/type fields to override edit  *
     H*                for Deision date                               *
     H*                                                               *
     H* 10/14/03 C:136 Project: WC1159                                *
     H*                Add security lvl/type fields to override edit  *
     H*                if payroll has a zero amount with a claim.     *
     H*                                                               *
     H* 08/12/99 C:112 Project: WC861                                 *
     H*                Add screen field SCRLBL to display.  Allows    *
     H*                designating claim print labels for 'Record     *
     H*                Only' (CMLTFL='R') to be printed along with    *
     H*                Medical (CMLTFL='M') and Indemnity (CMLTFL='I')*
     H*                Flag to be set to 'Y' to print or otherwise    *
     H*                left blank.                                    *
     H*                                                               *
     H*  04/09/98  C:90  PROJECT: WC577                               *
     H*                  CHANGE F10 KEY TO DISPLAY ERROR MESSAGE ON   *
     H*                  NON-SEARCHABLE FIELDS                        *
     H*                                                               *
     H*  04/06/98  C:90  PROJECT: WC577                               *
     H*                  REMOVE FIELD "DEFAULT CLAIM CHECK TYPE"      *
     H*                  CHANGE F10 KEY TO DISPLAY ERROR MESSAGE ON   *
     H*                  NON-SEARCHABLE FIELDS                        *
     H*                                                               *
     H*  02/05/98  C:47  91XXX                                        *
     H*            ADDED 4 MORE SECURITY TYPES TO EACH EXISTING TYPE. *
     H*                                                               *
     H*  05/13/97  C:47  60ANC                                        *
     H*            MODIFIED TO USE WITH VERSION 3.                    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*   09      INDICATES MAINT/INQUIRY MODE                        *
     H*   40      INDICATES WHETHER TO WRITE OR UPDATE RECORD         *
     H*   60      GENERAL USAGE (CHAIN, READ, ETC..)                  *
     H*                                                               *
     H*  SCR02:                                                       *
     H*   20      CLM RITY LEVEL CANNOT BE BLANK - CLAIM DELETION     *
     H*   21      VALUE CANNOT BE BLANK                               *
     H*   22      VALUE MUST BE Y OR N.                               *
     H*   23      OVERRIDE CX DATE <= ACCIDENT DATE ERR MSG-SEC LEVEL *
     H*                                                               *
     H*  SCR07:                                                       *
     H*   21    DOH Required VALUE MUST BE Y OR ' '.
     H*   22    DOB Required VALUE MUST BE Y OR ' '.
     H*   23    GNDR Required VALUE MUST BE Y OR ' '.
     H*****************************************************************
     FW0069F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E             DISK
     FWMCXL     UF A E           K DISK
      *** Begin Add *** 11/12/20 **************************************
     FWMCXP2    UF A E           K DISK
      *** End   Add *** 11/12/20 **************************************
     FSTCTL1    IF   E           K DISK
     FWTPSL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     F*** Add System Control file
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     FSMUPP     IF   E             DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     F*** BEG ADD ****** 04/13/12 *********************************************
     FSFBPF     IF   E             DISK
     F                                     RENAME(SFB01:SFB02)
     F*** END ADD ****** 04/13/12 *********************************************
     F*** BEG ADD ****** 09/01/17 *********************************************
     FFMCXL     UF   E           K DISK    USROPN RENAME(PCX01:FX01) PREFIX (F_)
     F*** END ADD ****** 09/01/17 *********************************************
     D UA              S             10    DIM(3) CTDATA PERRCD(1)
      * ----------------------------------------------------------------
      * SECURITY TYPE VALIDATION WAS REMOVED.
      * TO INSTALL, REMOVE ASTERISKS AND MOVE SECOND & THIRD LINE
      *   TO BOTTOM OF PROGRAM.
      *
      * SECURITY VALIDATION ARRAY
      ****  **  ARRAY SCD - Valid Security Type Codes
      ****  ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
      * ----------------------------------------------------------------
      *
      * ENTERED SECURITY TYPES
     D BLNK            S              1    DIM(12) CTDATA PERRCD(1)
     D DIRY            S              1    DIM(12) CTDATA PERRCD(1)
      *
      * SCR01 SECURITY TYPES
     D                 DS
     D  STY                    1      7
     D                                     DIM(7)
     D  SCCDT                  1      1
     D  SCOCAT                 2      2
     D  SCCKDT                 3      3
     D  SCDPPT                 4      4
     D  SCPDCT                 5      5
     D  SCVSZT                 6      6
     D  SCFSVT                 7      7
      *
      * WMCXP REMARK CODES
     D                 DS
     D  XRK1                   1     60
     D                                     DIM(12)
     D  CXRFFA                 1      5
     D  CXRTFA                 6     10
     D  CXRCNC                11     15
     D  CXROPN                16     20
     D  CXRADS                21     25
     D  CXRSSD                26     30
     D  CXRUMD                31     35
     D  CXRCAD                36     40
     D  CXRSRD                41     45
     D  CXDSRC                46     50
     D  CXNMAD                51     55
     D  CXAADP                56     60
      *
      * SCR03 REMARK CODES
     D                 DS
     D  RK1                    1     60
     D                                     DIM(12)
     D  SCRFFA                 1      5
     D  SCRTFA                 6     10
     D  SCRCNC                11     15
     D  SCROPN                16     20
     D  SCRADS                21     25
     D  SCRSSD                26     30
     D  SCRUMD                31     35
     D  SCRCAD                36     40
     D  SCRSRD                41     45
     D  SCDSRC                46     50
     D  SCNMAD                51     55
     D  SCAADP                56     60
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** Beg Add *** 4/13/12 *********************************************
     D  XXNCIN         S              1
     D*** End Add *** 4/13/12 *********************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D OPTNM           C                   CONST('Claim/DARTS Control-
     D                                      Information')
      *** BEG ADD *** 03/16/12 *******************************************
     d Ptr             S               *   Inz(%Addr(*In))
     d strInd1         S              1    Dim(99)
     d                                     Based(Ptr)
     d strInd2         S              1    Dim(99)
     d                                     Based(Ptr)
      *** END ADD *** 03/16/12 *******************************************
      ***** Beg Add ****** 07/28/14 ************************************
     d isClmAct        s              1n
      ***** End Add ****** 07/28/14 ************************************
      ***** Beg Add ****** 09/01/17 ************************************
     D xxSub#          s                   inz like(F_CXSUB#)
     d ##rtcd          s              1
     d ##co#           s              3
     d ##wcf#          s              3
     d ##sub#          s              5
     d ##fnd           s              3
      ***** End Add ****** 09/01/17 ************************************
      *---------------------------------------------------------------------
      * Prototype Declaration
      *---------------------------------------------------------------------
     d GetCMSSubDC     pr                  extPgm('SC305R')
     d ##Call                         1
     d ##KC                           1
     d ##KL                           1
     d ##Rmcd                         5
     d ##Desc                        40
     d ##Rtcd                         1
      *
     d GetCMSSubDU     pr                  extPgm('W2493R')
     d qqPrds                        40
     d qqCall                         1
     d xxTbid                         4
     d xxKey                         10
     d qqRtcd                         1

     d GetCMSSubSU     pr                  extPgm('S0005R')
     d ##CAll                         1
     d ##RRN                          9  0
     d ##VIEW                         1  0
      *** END ADD *** 03/16/12 *******************************************
      *** BEG ADD *** 09/01/17 *******************************************
     d FA700R          pr                  extPgm('FA700R')
     d ##rtcd                         1
     d ##co#                          3
     d ##wcf#                         3
     d ##sub#                         5
     d ##fnd                          3
      *** END ADD *** 09/01/17 *******************************************
       //*** Beg Add *** 09/09/21 *********************************************
     d isWcMc          s              1n
      /Copy sprModule
       //*** End Add *** 09/09/21 *********************************************
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE DARTS TABLE CODE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXDART            5
     C*
     C*  KEY LIST FOR THE CLAIMS/DARTS CONTROL FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C     GTKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C*
     C*  TABLE FILE
     C*
     C     PSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C     *LIKE         DEFINE    FNCO#         XXCO#
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C                   MOVEL     *BLANKS       QQRTC             1
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
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
     C*
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      1             ##VIEW            1 0
     C*
     C                   MOVE      *BLANK        XX                5
     C                   Z-ADD     *ZERO         X                 3 0
     C                   Z-ADD     *ZERO         Y                 3 0
     C*
     C*  GET OPTION NAME
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*** Retrieve flag from system control file to display
     C*** Claim diary access summary screen option
     C                   MOVE      *BLANKS       XCKEY             2
     C                   MOVEL     'WC'          XCKEY
     C     XCKEY         CHAIN     SWC01                              60
     C     *IN60         IFEQ      '0'
     C     WCFL33        ANDEQ     'Y'
     C                   MOVE      '1'           *IN75
     C                   ELSE
     C                   MOVE      '0'           *IN75
     C                   ENDIF                                                  *IN60 = '0'
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C                   SETOFF                                       16
     C     ##CLT#        IFEQ      '114'
     C                   SETON                                        16
     C                   ENDIF
     C                   SETOFF                                       01
     C     ##CLT#        IFEQ      '025'
     C                   SETON                                        01
     C                   ENDIF
      *** BEG ADD ****** 11/25/13 *********************************************
      /Free
       If ##CLT# = '129';
          isFCC = *On;
       Else;
          isFCC = *Off;
       EndIf;

       //***** Beg Add *** 05/10/17 *******************************************
       // PBM fields are visible for selected clients only
       If ##CLT# = '090';
          vsbPBM = *On;
       Else;
          vsbPBM = *Off;
       EndIf;
       //***** End Add *** 05/10/17 *******************************************
      /End-Free
      *** END ADD ****** 11/25/13 *********************************************
       //*** Beg Add *** 09/09/21 **********************************************
       isWcMc = isModEnabled('Managed Care');
       //*** End Add *** 09/09/21 **********************************************
     C*
     C     TOP           TAG
     C*
     C*  GET THE FUND RECORD
     C*
     C     @@RRN         CHAIN     PFN01                              60
     C                   MOVE      FNFND         SCFND
     C                   MOVE      FNCO#         XXCO#
     C                   MOVE      FNFND         XXFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*  GET THE COMPANY RECORD
     C*
     C                   MOVE      *BLANKS       SCCONM
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C     @@MODE        COMP      'D'                                    09
      *
      * SCREEN 1
     C                   MOVE      *BLANK        SCCDL
     C                   MOVE      *BLANK        SCCDT
     C                   MOVE      *BLANK        SCOCAL
     C                   MOVE      *BLANK        SCOCAT
     C                   MOVE      *BLANK        SCCKDL
     C                   MOVE      *BLANK        SCCKDT
     C                   MOVE      *BLANK        SCDPPL
     C                   MOVE      *BLANK        SCDPPT
     C                   MOVE      *BLANK        SCPDCL
     C                   MOVE      *BLANK        SCPDCT
     C                   MOVE      *BLANK        SCVSZL
     C                   MOVE      *BLANK        SCVSZT
     C                   MOVE      *BLANK        SCFSVL
     C                   MOVE      *BLANK        SCFSVT
      *
     C                   MOVE      *BLANK        SCCDT2
     C                   MOVE      *BLANK        SCCDT3
     C                   MOVE      *BLANK        SCCDT4
     C                   MOVE      *BLANK        SCCDT5
     C                   MOVE      *BLANK        SCOCA2
     C                   MOVE      *BLANK        SCOCA3
     C                   MOVE      *BLANK        SCOCA4
     C                   MOVE      *BLANK        SCOCA5
     C                   MOVE      *BLANK        SCCKD2
     C                   MOVE      *BLANK        SCCKD3
     C                   MOVE      *BLANK        SCCKD4
     C                   MOVE      *BLANK        SCCKD5
     C                   MOVE      *BLANK        SCDPP2
     C                   MOVE      *BLANK        SCDPP3
     C                   MOVE      *BLANK        SCDPP4
     C                   MOVE      *BLANK        SCDPP5
     C                   MOVE      *BLANK        SCPDC2
     C                   MOVE      *BLANK        SCPDC3
     C                   MOVE      *BLANK        SCPDC4
     C                   MOVE      *BLANK        SCPDC5
     C                   MOVE      *BLANK        SCVSZ2
     C                   MOVE      *BLANK        SCVSZ3
     C                   MOVE      *BLANK        SCVSZ4
     C                   MOVE      *BLANK        SCVSZ5
     C                   MOVE      *BLANK        SCFSV2
     C                   MOVE      *BLANK        SCFSV3
     C                   MOVE      *BLANK        SCFSV4
     C                   MOVE      *BLANK        SCFSV5
     C                   CLEAR                   SCMBS
     C                   CLEAR                   SCMBS1
     C                   CLEAR                   SCMBS2
     C                   CLEAR                   SCMBS3
     C                   CLEAR                   SCMBS4
     C                   CLEAR                   SCMBS5
     C                   CLEAR                   SCRBS
     C                   CLEAR                   SCRBS1
     C                   CLEAR                   SCRBS2
     C                   CLEAR                   SCRBS3
     C                   CLEAR                   SCRBS4
     C                   CLEAR                   SCRBS5
     C***** Beg Add *** 02/14/17 ***************************************
     C                   CLEAR                   SCPBMLvl
     C                   CLEAR                   SCPBMTyp1
     C                   CLEAR                   SCPBMTyp2
     C                   CLEAR                   SCPBMTyp3
     C                   CLEAR                   SCPBMTyp4
     C                   CLEAR                   SCPBMTyp5
     C***** End Add *** 02/14/17 ***************************************
      *
      * SCREEN 2
      ***** Beg Add ****** 06/22/16 ************************************
     C*****              MOVE      *BLANK        SCLOCT
      ***** End Add ****** 06/22/16 ************************************
     C                   MOVE      *BLANK        SCPMTS
     C                   MOVE      *BLANK        SCPSC2
     C                   MOVE      *BLANK        SCACCP
     C                   MOVE      *BLANK        SCRT1
     C                   MOVE      *BLANK        SCRT2
     C                   MOVE      *BLANK        SCRT3
     C                   MOVE      *BLANK        SCRT4
     C                   MOVE      *BLANK        SCMPCD
     C                   MOVE      *BLANK        SCMPD
     C                   MOVE      *BLANK        SCCAT2
     C                   MOVE      *BLANK        SCADJ
     C                   MOVE      *BLANK        SCISC
     C                   MOVE      *BLANK        SCRCHG
     C                   MOVE      *BLANK        SCRLBL
     C                   MOVE      *BLANKS       SCDPPC
     C                   Z-ADD     *ZEROS        SCEDIM
      ***** Beg Add ****** 11/25/13 ************************************
      /free
       If isFCC = *On;
          scLGRH = *Blanks;
       EndIf;
      /end-free
      ***** End Add ****** 11/25/13 ************************************
      * SCREEN 7
     C                   MOVE      *BLANK        SCOPCL
     C                   MOVE      *BLANK        SCDOHR
     C                   MOVE      *BLANK        SCDOBR
     C                   MOVE      *BLANK        SCGDRR
     C                   MOVE      *BLANKS       SCEXNM
     C                   MOVE      *BLANK        SCLSRV
     C                   MOVE      *BLANK        SCWMTH
      ***** Beg Add ****** 08/29/13 ************************************
      /free
       scAwwr = ' ';
       //*** Beg Add *** 09/01/17 **************************************
       scCVT# = *Blanks;
       visCvt# = *on;
       //*** End Add *** 09/01/17 **************************************
      /end-free
      ***** End Add ****** 08/29/13 ************************************
      ***** Beg Add *** 02/14/17 *******************************************
      /free
        scPBMElgb = *blanks;
      /end-free
      ***** End Add *** 02/14/17 *******************************************
      ***** Beg Add ****** 06/22/16 ************************************
      /free
       scloct = 'N';
      /end-free
      ***** End Add ****** 06/22/16 ************************************
      *
      * SCREEN 3
     C                   MOVE      *BLANK        SCRFFA
     C                   MOVE      *BLANK        SCRTFA
     C                   MOVE      *BLANK        SCRCNC
     C                   MOVE      *BLANK        SCROPN
     C                   MOVE      *BLANK        SCDSRC
     C                   MOVE      *BLANK        SCNMAD
     C                   MOVE      *BLANK        SCAADP
     C*** Begin Add *** 07/16/18 *********************************
     C                   MOVE      *BLANK        SCAADR
     C*** End   Add *** 07/16/18 *********************************
     C                   MOVE      *BLANK        SCRODY
     C                   MOVE      *BLANK        SCRADS
     C                   MOVE      *BLANK        SCBADJ
     C                   MOVE      *BLANK        SCRSSD
     C                   MOVE      *BLANK        SCBSPV
     C                   MOVE      *BLANK        SCRUMD
     C                   MOVE      *BLANK        SCBMGR
     C                   MOVE      *BLANK        SCRCAD
     C                   MOVE      *BLANK        SCRSRD
     C                   MOVE      *BLANK        SCCDSM
     C                   MOVE      *BLANK        SCFL01
     C                   MOVE      *BLANKS       SCDEVF
     C                   MOVE      *BLANKS       SCWCR
      ***** Beg Add ****** 11/25/13 ************************************
      /free
       If isFCC = *On;
          scAPRM = *Blanks;
       EndIf;
      /end-free
      ***** End Add ****** 11/25/13 ************************************
      ***** Beg Add ****** 07/28/14 ************************************
      /free
       scClmAct = *Blanks;
      /end-free
      ***** End Add ****** 07/28/14 ************************************
      * SCREEN 4
     C                   MOVE      *BLANK        SCMEDI
     C                   MOVE      *BLANK        SCBENI
     C                   MOVE      *BLANK        SCRPS
     C                   MOVE      *BLANK        SCSTSP
     C                   MOVE      *BLANK        SCDBEN
     C                   MOVE      *BLANK        SCDART
     C                   MOVE      *BLANK        SCMOC
     C                   MOVE      *BLANK        SCACCM
     C                   MOVE      *BLANK        SCADRM
     C                   MOVE      *BLANK        SCNFAM
     C                   MOVE      *BLANK        SCINC
     C                   MOVE      *BLANK        SCACCI
     C                   MOVE      *BLANK        SCADRI
     C                   MOVE      *BLANK        SCNFAI
     C*** Beg Add ****** 08/29/22 *********************************************
     C                   MOVE      *BLANK        SCPFRO
     C*** End Add ****** 08/29/22 *********************************************
      * SCREEN 5
     C                   MOVE      *BLANK        SCQRSD
     C                   MOVE      *BLANK        SCQRDU
     C                   MOVE      *BLANK        SCQRSU
     C                   MOVE      *BLANK        SCRDUD
     C                   MOVE      *BLANK        SCRSUD
     C                   MOVE      *BLANK        SCCERD
     C                   MOVE      *BLANK        SCCEDU
     C                   MOVE      *BLANK        SCCESU
     C                   MOVE      *BLANK        SCNAM1
     C                   MOVE      *BLANK        SCNAM2
     C                   MOVEL     *BLANK        SCMBQF
     C*
     C                   MOVE      *BLANK        SCAPV1
     C                   MOVE      *BLANK        SCAPV2
     C                   MOVE      *BLANK        SCAPV3
     C                   MOVE      *BLANK        SCAPV4
     C                   MOVE      *BLANK        SCAPV5
     C*** BEG ADD ****** 03/16/12 ****************************************
      /free
        scrcsc = *blanks;
        scrcdu = *blanks;
        scrcsu = *blanks;
        scnam3 = *blanks;
        scnam4 = *blanks;
      /end-free
     C*** END ADD ****** 03/16/12 ****************************************
      *** Begin Add *** 11/12/20 *****************************************
       scLossCd  = *Blank;
       scLsReCd  = *Blank;
       scCovt    = *Blank;
       scLsCv    = *Blank;
       scLoadAww = *Blank;
       scLossCdsl = 'GTco# = ' + %char(xxCo#) + ' and GTTBID = ''  AC''' +
       ' and GTstat <> ''I''';

       scLsReCdSl = 'GTco# = ' + %char(xxCo#) + ' and GTTBID = ''LSRV''' +
       ' and GTstat <> ''I''';

       scCovtSel = 'GTco# = ' + %char(xxCo#) + ' and GTTBID = ''  CT''' +
       ' and GTstat <> ''I''';

       scLsCvSel = 'GTco# = ' + %char(xxCo#) + ' and GTTBID = ''  LC''' +
       ' and GTstat <> ''I''';
       Chain(N) CXKY PCX02;
      *** End   Add *** 11/12/20 *****************************************

     C     CXKY          CHAIN(N)  PCX01                              60
     C     *IN60         IFEQ      *OFF
      * SCREEN 1
     C                   Z-ADD     CXSECL        SCCDL
     C                   Z-ADD     CXSECC        SCOCAL
     C                   Z-ADD     CXCKDL        SCCKDL
     C                   Z-ADD     CXDLTD        SCDPPL
     C                   Z-ADD     CXPYOD        SCPDCL
     C                   Z-ADD     CXVSZL        SCVSZL
     C                   Z-ADD     CXFSVL        SCFSVL
     C                   MOVEL     CXCDT         SCCDT
     C                   MOVEL     CXOCAT        SCOCAT
     C                   MOVEL     CXCKDT        SCCKDT
     C                   MOVEL     CXDPPT        SCDPPT
     C                   MOVEL     CXPDCT        SCPDCT
     C                   MOVEL     CXVSZT        SCVSZT
     C                   MOVEL     CXFSVT        SCFSVT
     C                   MOVEL     CXCDT2        SCCDT2
     C                   MOVEL     CXCDT3        SCCDT3
     C                   MOVEL     CXCDT4        SCCDT4
     C                   MOVEL     CXCDT5        SCCDT5
     C                   MOVEL     CXOCA2        SCOCA2
     C                   MOVEL     CXOCA3        SCOCA3
     C                   MOVEL     CXOCA4        SCOCA4
     C                   MOVEL     CXOCA5        SCOCA5
     C                   MOVEL     CXCKD2        SCCKD2
     C                   MOVEL     CXCKD3        SCCKD3
     C                   MOVEL     CXCKD4        SCCKD4
     C                   MOVEL     CXCKD5        SCCKD5
     C                   MOVEL     CXDPP2        SCDPP2
     C                   MOVEL     CXDPP3        SCDPP3
     C                   MOVEL     CXDPP4        SCDPP4
     C                   MOVEL     CXDPP5        SCDPP5
     C                   MOVEL     CXPDC2        SCPDC2
     C                   MOVEL     CXPDC3        SCPDC3
     C                   MOVEL     CXPDC4        SCPDC4
     C                   MOVEL     CXPDC5        SCPDC5
     C                   MOVEL     CXVSZ2        SCVSZ2
     C                   MOVEL     CXVSZ3        SCVSZ3
     C                   MOVEL     CXVSZ4        SCVSZ4
     C                   MOVEL     CXVSZ5        SCVSZ5
     C                   MOVEL     CXFSV2        SCFSV2
     C                   MOVEL     CXFSV3        SCFSV3
     C                   MOVEL     CXFSV4        SCFSV4
     C                   MOVEL     CXFSV5        SCFSV5
     C                   MOVEL     CXAUT1        SCAUT1
     C                   MOVEL     CXAUT2        SCAUT2
     C                   MOVEL     CXAUT3        SCAUT3
     C                   MOVEL     CXAUT4        SCAUT4
     C                   MOVEL     CXAUT5        SCAUT5
     C                   MOVEL     CXAUSL        SCAUSL
     C                   MOVEL     CXDS01        SCDS01
     C                   MOVEL     CXDS02        SCDS02
     C                   MOVEL     CXDS03        SCDS03
     C                   MOVEL     CXDS04        SCDS04
     C                   MOVEL     CXDS05        SCDS05
     C                   MOVEL     CXDSRC        SCDSRC
     C                   MOVEL     CXDSLV        SCDSLV
     C                   MOVEL     CXDPT1        SCDPT1
     C                   MOVEL     CXDPT2        SCDPT2
     C                   MOVEL     CXDPT3        SCDPT3
     C                   MOVEL     CXDPT4        SCDPT4
     C                   MOVEL     CXDPT5        SCDPT5
     C                   MOVEL     CXDPPC        SCDPPC
     C                   MOVEL     CXDPLV        SCDPLV
     C                   Z-ADD     CXEDIM        SCEDIM
      *
     C                   MOVEL     CXDPT6        SCDPT6
     C                   MOVEL     CXDPT7        SCDPT7
     C                   MOVEL     CXDPT8        SCDPT8
     C                   MOVEL     CXDPT9        SCDPT9
     C                   MOVEL     CXDPT0        SCDPT0
     C                   MOVEL     CXDPL2        SCDPL2
     C                   Z-ADD     CXMBS         SCMBS
     C                   MOVEL     CXMBS1        SCMBS1
     C                   MOVEL     CXMBS2        SCMBS2
     C                   MOVEL     CXMBS3        SCMBS3
     C                   MOVEL     CXMBS4        SCMBS4
     C                   MOVEL     CXMBS5        SCMBS5
     C                   Z-ADD     CXRBS         SCRBS
     C                   MOVEL     CXRBS1        SCRBS1
     C                   MOVEL     CXRBS2        SCRBS2
     C                   MOVEL     CXRBS3        SCRBS3
     C                   MOVEL     CXRBS4        SCRBS4
     C                   MOVEL     CXRBS5        SCRBS5
     C***** Beg Add *** 02/14/17 ***************************************
      /free

        scPBMLvl = CXPBMEL;
        scPBMTyp1 = CXPBME1;
        scPBMTyp2 = CXPBME2;
        scPBMTyp3 = CXPBME3;
        scPBMTyp4 = CXPBME4;
        scPBMTyp5 = CXPBME5;

      /end-free
     C***** End Add *** 02/14/17 ***************************************
      *
      * SCREEN 2
     C                   MOVEL     CXLOCT        SCLOCT
     C*
     C                   MOVEL     CXPMTS        SCPMTS
     C                   MOVE      *BLANK        SCPMTD
     C                   MOVEL(P)  CXPMTS        XXKEY
     C     PSKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PSDESC        SCPMTD
     C                   END
     C*
     C                   MOVEL     CXPSC2        SCPSC2
     C                   MOVE      *BLANK        SCPSCD
     C                   MOVEL(P)  CXPSC2        XXKEY
     C     PSKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PSDESC        SCPSCD
     C                   END
     C*
     C*
     C                   MOVEL     CXDPPC        SCDPPC
     C                   MOVE      *BLANK        SCDPPD
     C                   MOVEL(P)  CXDPPC        XXKEY
     C     PSKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PSDESC        SCDPPD
     C                   ENDIF
     C*
     C                   MOVEL     CXACCP        SCACCP
     C                   MOVEL     CXRT01        SCRT1
     C                   MOVEL     CXRT02        SCRT2
     C                   MOVEL     CXRT03        SCRT3
     C                   MOVEL     CXRT04        SCRT4
     C*
     C                   MOVEL     CXMJR         SCMPCD
     C                   MOVE      *BLANK        SCMPD
     C                   MOVEL(P)  'MAJP'        XXTBID
     C                   MOVEL(P)  CXMJR         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCMPD
     C                   END
     C*
     C                   MOVEL     CXCAT2        SCCAT2
     C                   MOVEL     CXADJ         SCADJ
     C                   MOVEL     CXCSDF        SCISC
     C                   MOVEL     CXRCHG        SCRCHG
     C                   MOVE      CXRLBL        SCRLBL
      ***** Beg Add ****** 11/25/13 ************************************
      /free
       If isFCC = *On;
          scLGRH = CXlgrh;
       EndIf;
      /end-free
      ***** End Add ****** 11/25/13 ************************************
      * SCREEN 7
     C                   MOVEL     CXOPCL        SCOPCL
     C                   MOVEL     CXDOHR        SCDOHR
     C                   MOVEL     CXDOBR        SCDOBR
     C                   MOVEL     CXGDRR        SCGDRR
     C                   MOVE      CXEXNM        SCEXNM
     C                   MOVEL     CXLSRV        SCLSRV
     C                   MOVEL     CXWMTH        SCWMTH
      ***** Beg Add ****** 08/29/13 ************************************
      /free
       If CXawwr = ' ';
          scAwwr = 'Y';
       Else;
          scAwwr = CXawwr;
       Endif;

       //***** Beg Add *** 02/14/17 ************************************
       If CXPBME = *blanks;
          scPBMElgb = 'Y';
       Else;
          scPBMElgb = CXPBME;
       Endif;
       //***** End Add *** 02/14/17 ************************************
       //***** Beg Add *** 09/01/17 ************************************
       SCcvt# = CXcvt#;
       VisCvt# = *on;
       //***** End Add *** 09/01/17 ************************************
      /end-free
      ***** End Add ****** 08/29/13 ************************************
      * SCREEN 3
     C                   MOVEL     CXRFFA        SCRFFA
     C                   MOVEL     CXRTFA        SCRTFA
     C                   MOVEL     CXRCNC        SCRCNC
     C                   MOVEL     CXROPN        SCROPN
     C                   MOVEL     CXDSRC        SCDSRC
     C                   Z-ADD     CXRODY        SCRODY
     C                   MOVEL     CXRADS        SCRADS
     C                   Z-ADD     CXBADJ        SCBADJ
     C                   MOVEL     CXRSSD        SCRSSD
     C                   Z-ADD     CXBSPV        SCBSPV
     C                   MOVEL     CXRUMD        SCRUMD
     C                   Z-ADD     CXBMGR        SCBMGR
     C                   MOVEL     CXRCAD        SCRCAD
     C                   MOVEL     CXRSRD        SCRSRD
     C                   MOVE      CXCDSM        SCCDSM
     C                   MOVE      CXFL01        SCFL01
     C                   MOVE      CXDEVF        SCDEVF
     C                   MOVEL     CXNMAD        SCNMAD
     C                   MOVEL     CXAADP        SCAADP
     C*** Begin Add *** 07/16/18 *********************************
     C                   MOVEL     CXAADR        SCAADR
     C*** End   Add *** 07/16/18 *********************************
     C                   MOVE      CXWCR         SCWCR
      ***** Beg Add ****** 11/25/13 ************************************
      /free
       If isFCC = *On;
          scAPRM = CXaprm;
       EndIf;
      /end-free
      ***** End Add ****** 11/25/13 ************************************
      ***** Beg Add ****** 07/28/14 ************************************
      /free
       scClmAct = CXcapd;
      /end-free
      ***** End Add ****** 07/28/14 ************************************
      * SCREEN 4
     C                   MOVEL     CXMEDI        SCMEDI
     C                   MOVEL     CXBENI        SCBENI
     C                   MOVEL     CXRPS         SCRPS
     C                   MOVEL     CXSTSP        SCSTSP
     C                   MOVEL     CXDBEN        SCDBEN
     C                   MOVEL     CXDART        SCDART
     C                   MOVEL     CXMOC         SCMOC
     C                   MOVEL     CXACCM        SCACCM
     C                   MOVEL     CXADRM        SCADRM
     C                   Z-ADD     CXNFAM        SCNFAM
     C                   MOVEL     CXINC         SCINC
     C                   MOVEL     CXACCI        SCACCI
     C                   MOVEL     CXADRI        SCADRI
     C                   Z-ADD     CXNFAI        SCNFAI
     C                   MOVEL     UA(2)         SCMODE
     C*** Beg Add ****** 08/29/22 *********************************************
     C                   MOVEL     CXPRTFROI     SCPFRO
     C*** End Add ****** 08/29/22 *********************************************
     C                   ELSE
     C                   MOVE      UA(1)         SCMODE
     C                   END
     C   09              MOVEL(P)  UA(3)         SCMODE
      /free
       //********** beg add ********* 09/04/14 *******************************************
       scClmInst = CXclmInst;
       If scClmInst = ' ';
          scClmInst = 'N';
       Endif;
       //********** end add ********* 09/04/14 *******************************************
      /end-free
      *SCREEN 5
     C                   MOVEL     CXCERD        SCCERD
     C                   MOVEL     CXCEDU        SCCEDU
     C                   MOVEL     CXCESU        SCCESU
     C*
     C                   MOVEL(P)  'AXDU'        XXTBID
     C                   MOVEL(P)  SCCEDU        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCNAM1
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCCESU        XXSTAF
     C     XXSTAF        CHAIN     PUP02                              60
     C     *IN60         IFEQ      *OFF
     C     UPSTAT        ANDEQ     'A'
     C                   MOVEL(P)  UPUSNM        SCNAM2
     C                   ENDIF
     C                   MOVEL     CXQRSD        SCQRSD
      *
     C                   MOVEL     CXQRDU        SCQRDU
     C     CXQRDU        IFNE      *BLANKS
     C                   MOVEL(P)  'AXDU'        XXTBID
     C                   MOVEL(P)  CXQRDU        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL     GTDESC        SCRDUD
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVEL     CXQRSU        SCQRSU
     C     CXQRSU        IFNE      *BLANKS
     C     CXQRSU        CHAIN     PUP02                              60
     C     *IN60         IFEQ      *OFF
     C     UPSTAT        ANDEQ     'A'
     C                   MOVEL     UPUSNM        SCRSUD
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL     CXMBQF        SCMBQF
     C*** BEG ADD ****** 03/16/12 ****************************************
      /free

        scRcsc = CXrcsc;

        scRcdu = CXrcdu;

        If (CXrcdu <> *blanks);

           xxTbid = 'AXDU';
           xxKey  = CXrcdu;

           Chain GTky pgt01;
              If %found;
                 If (GTstat = 'A');
                  scnam3 = GTdesc;
                 Endif;                                                   // (GTstat = 'A')
              Endif;                                                      // %found

        Endif;                                                            // (CXrcdu <> *blanks)

        scRcsu = CXrcsu;

        If (CXrcsu <> *blanks);
           Chain CXrcsu pup02;
           If %found;
              If (UPstat = 'A');
                 scnam4 = UPusnm;
              Endif;                                                      // %found;
           Endif;                                                         // (UPstat = 'A')

        Endif;                                                            // (CXrcsu <> *blanks)

      /end-free
     C*** END ADD ****** 03/16/12 ****************************************
     C* SCREEN 6
     C                   MOVEL     CXAPV1        SCAPV1
     C                   MOVEL     CXAPV2        SCAPV2
     C                   MOVEL     CXAPV3        SCAPV3
     C                   MOVEL     CXAPV4        SCAPV4
     C                   MOVEL     CXAPV5        SCAPV5
      *
      //*** Begin Add *** 11/12/20 ***************************************
      // Screen 8
       scLossCd  = CXlossCd;
       scLsReCd  = CXlsReCd;
       scCovt    = CXcovt;
       scLsCv    = CXlsCv;
       scLoadAww = CXloadAww;
      //*** End   Add *** 11/12/20 ***************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*  ----------------------------------------------------------------
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*
     C     Q2RECF        IFEQ      'SCR07'
     C                   EXFMT     SCR07
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C                   EXFMT     SCR04
     C                   END
     C*
     C                   MOVE      *BLANKS       SYKEY             2
     C                   MOVEL     'SY'          SYKEY
     C     SYKEY         CHAIN     SSY01                              60
     C     *IN60         IFEQ      '0'
     C     1             SUBST     SYFL12:2      XXFL12            1
     C     XXFL12        IFEQ      'Y'
     C     WCFL46        ANDEQ     'Y'
     C     Q2RECF        IFEQ      'SCR05'
     C                   EXFMT     SCR05
     C                   END
     C                   ENDIF                                                  SYFL12(POS 2) = 'Y'
     C                   ENDIF                                                  *IN60 = '0'
     C     Q2RECF        IFEQ      'SCR06'
     C                   EXFMT     SCR06
     C                   END

      //*** Begin Add *** 11/12/20 ***************************************
        If Q2recf = 'SCR08';
           ExFmt SCR08;
        Endif;
      //*** End   Add *** 11/12/20 ***************************************
     C*
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3=EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*  --------------------------------------------------------------
     C*   F10=SEARCH
     C*
     C     *INKJ         IFEQ      *ON
      *  -------------------------------------------------------------------
      *  TABLE SEARCH FOR RLS PMT STATUS - SCREEN 2
      *
     C     Q2RECF        IFEQ      'SCR02'
      *
      * RESET ERROR INDICATORS
      *
     C                   MOVEA     '00'          *IN(20)                        *IN20 - 21
     C                   MOVEA     '00000000'    *IN(22)                        *IN22 - 29
     C                   MOVEA     '0000'        *IN(30)                        *IN30 - 33
     C                   MOVEA     '0000'        *IN(42)                        *IN42 - 45
     C                   MOVEA     '00000'       *IN(52)                        *IN52 - 56
     C                   MOVEA     '000000'      *IN(77)                        *IN77 - 82
     C                   MOVEA     '0000000'     *IN(83)                        *IN83 - 89
     C                   MOVE      '0'           *IN74
      *
     C     HFLD          IFEQ      'SCPMTS'
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'W2142R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    XXKEY
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   MOVE      *ON           *IN80
      *
     C     *INKL         IFEQ      *ON
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN99
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C                   MOVEL     XXKEY         SCPMTS
     C                   MOVE      *BLANK        SCPMTD
     C     PSKY          CHAIN     PPS01                              79
     C     *IN79         IFEQ      *OFF
     C                   MOVE      *ON           *IN83                          POSITION CURSOR
     C                   MOVE      *OFF          *IN80                          POS CUSR
     C                   MOVEL(P)  PSDESC        SCPMTD
     C                   END
      *
     C     *IN79         IFEQ      *ON
     C                   GOTO      STRT
     C                   END
      *
     C                   END                                                    QQRTC = B
     C*
     C                   GOTO      STRT
     C                   END                                                    HFLD = SCPMTS
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     HFLD          IFEQ      'SCPSC2'
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'W2142R'
     C                   PARM      '2'           @@CALL
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    XXKEY
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   MOVE      *ON           *IN83
      *
     C     *INKL         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN99
     C                   GOTO      STRT
      *
     C                   ELSE
     C                   MOVEL     XXKEY         SCPSC2
     C                   MOVE      *BLANK        SCPSCD
      *
     C     PSKY          CHAIN     PPS01                              79
     C     *IN79         IFEQ      *OFF
     C                   MOVE      *ON           *IN84                          POSITION CURSOR
     C                   MOVE      *OFF          *IN83                          POS CUSR
     C                   MOVEL(P)  PSDESC        SCPSCD
     C                   END
      *
     C     *IN79         IFEQ      *ON
     C                   GOTO      STRT
     C                   END
      *
     C                   END                                                    QQRTC = B
     C*
     C                   GOTO      STRT
     C                   END                                                    HFLD = SCPSC2
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     HFLD          IFEQ      'SCDPPC'
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'W2142R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    XXKEY
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   MOVE      *ON           *IN89
      *
     C     *INKL         IFEQ      *ON
     C     XXKEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN99
     C                   GOTO      STRT
      *
     C                   ELSE
      *
     C                   MOVEL     XXKEY         SCDPPC
     C                   MOVE      *BLANK        SCDPPD
     C     PSKY          CHAIN     PPS01                              79
     C     *IN79         IFEQ      *OFF
     C                   MOVE      *ON           *IN89                          POSITION CURSOR
     C                   MOVEL(P)  PSDESC        SCDPPD
     C                   ENDIF
      *
     C     *IN79         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF                                                  QQRTC = B
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD = SCDPPC
     C     HFLD          IFEQ      'SCMPCD'
     C                   MOVEL(P)  'MAJP'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC
      *
     C     QQRTC         CABEQ     'A'           TERM
      *
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   MOVE      *ON           *IN85                          POS CUSR
      *
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANK
     C                   GOTO      STRT
     C                   END
      *
     C     QQRTC         IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCMPCD
     C                   MOVE      *BLANK        SCMPD
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCMPD
     C                   MOVE      *ON           *IN86                          POS CUSR
     C                   MOVE      *OFF          *IN85
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   ENDIF                                                  HFLD = SCMPCD
     C                   MOVE      *ON           *IN92
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  Q2RECF = SCR02
      *
      *
      * TABLE SEARCH FOR DARTS REMARK CODE  --  SCREEN 3
      *
      *  -------------------------------------------------------------------
     C     Q2RECF        IFEQ      'SCR03'
      *
      * RESET ERROR INDICATORS
      *
     C                   MOVEA     '0000'        *IN(12)                        *IN12 - 15
     C                   MOVEA     '0'           *IN(20)                        *IN20
     C                   MOVEA     '00000000'    *IN(21)                        *IN21 - 28
     C                   MOVEA     '0'           *IN(29)                        *IN29
     C                   MOVEA     '000'         *IN(30)                        *IN30 - 32
     C                   MOVEA     '00000000'    *IN(33)                        *IN33 - 40
     C                   MOVEA     '0'           *IN(41)                        *IN41
     C                   MOVEA     '0000'        *IN(42)                        *IN42 - 45
     C                   MOVEA     '0000000'     *IN(47)                        *IN47 - 53
     C                   MOVEA     '0'           *IN(57)                        *IN57
     C                   MOVEA     '00000000'    *IN(58)                        *IN58 - 65
     C                   MOVEA     '00'          *IN(66)                        *IN66 - 67
     C*** Indicator *IN75 will not be setoff
     C                   MOVEA     '00000'       *IN(70)                        *IN70 - 74
     C                   MOVEA     '00'          *IN(76)                        *IN76 - 77
     C                   MOVEA     '000'         *IN(78)                        *IN78 - 80
     C                   MOVEA     '0000'        *IN(85)                        *IN85 - 88
     C                   MOVE      '0'           *IN99
      *
     C     HFLD          IFEQ      'SCRFFA'
     C     HFLD          OREQ      'SCRTFA'
     C     HFLD          OREQ      'SCRCNC'
     C     HFLD          OREQ      'SCROPN'
     C     HFLD          OREQ      'SCDSRC'
     C     HFLD          OREQ      'SCNMAD'
     C     HFLD          OREQ      'SCAADP'
     C     HFLD          OREQ      'SCRADS'
     C     HFLD          OREQ      'SCRCAD'
     C     HFLD          OREQ      'SCRSRD'
     C     HFLD          OREQ      'SCRSSD'
     C     HFLD          OREQ      'SCRUMD'
     C     HFLD          OREQ      'SCWCR '
      ***** Beg Add ****** 07/28/14 ************************************
     C     HFLD          OREQ      'SCCLMACT'
      ***** Beg Add ****** 07/28/14 ************************************
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     *LIKE         DEFINE    ##RMCD        QQRMCD
      *
     C     *INKC         CABEQ     'A'           TERM
     C                   MOVEL(P)  'SCR03'       Q2RECF
      *
     C                   SELECT
      *
      * CHANGE FROM ADJUSTER
     C     HFLD          WHENEQ    'SCRFFA'
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN46
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCRFFA
     C                   MOVE      *ON           *IN47
     C                   OTHER
     C                   MOVE      *ON           *IN46
     C                   ENDSL
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CHANGE TO ADJUSTER
     C     HFLD          WHENEQ    'SCRTFA'
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN47
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCRTFA
     C                   MOVE      *ON           *IN48
     C                   OTHER
     C                   MOVE      *ON           *IN47
     C                   ENDSL
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NAME CHANGE
     C     HFLD          WHENEQ    'SCRCNC'
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN48
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCRCNC
     C                   MOVE      *ON           *IN54
     C                   OTHER
     C                   MOVE      *ON           *IN48
     C                   ENDSL
      * WAGE CALC
     C     HFLD          WHENEQ    'SCWCR '
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN94
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCWCR
     C                   MOVE      *ON           *IN49
     C                   OTHER
     C                   MOVE      *ON           *IN94
     C                   ENDSL
      ***** Beg Add ****** 07/28/14 ************************************
      * Claim Action Plan
      /free
       When hfld = 'SCCLMACT';
          Select;
             When *inkl = *on;
                *in49 = *on;
             When ##Rmcd <> ' ';
                scClmAct = ##Rmcd;
                indMedadj = *on;
          EndSl;
      /End-Free
      ***** End Add ****** 07/28/14 ************************************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * REOPEN
     C     HFLD          WHENEQ    'SCROPN'
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN54
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCROPN
     C                   MOVE      *ON           *IN12
     C                   OTHER
     C                   MOVE      *ON           *IN54
     C                   ENDSL
      * DECISION DATE
     C     HFLD          WHENEQ    'SCDSRC'
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN57
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCDSRC
     C                   MOVE      *ON           *IN49
     C                   OTHER
     C                   MOVE      *ON           *IN57
     C                   ENDSL
      * -----------------------------------------------------------------
      *
     C     HFLD          WHENEQ    'SCNMAD'
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN83
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCNMAD
     C                   MOVE      *ON           *IN93
     C                   ENDSL
     C     HFLD          WHENEQ    'SCAADP'
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN94
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCAADP
     C                   MOVE      *ON           *IN49
     C                   ENDSL
      * -----------------------------------------------------------------
      * ADJ MED
     C     HFLD          WHENEQ    'SCRADS'
     C                   SELECT
     C     *INKL         WHENEQ    *ON
      ******* Beg Add ********** 07/28/14 *****************************
     C********           MOVE      *ON           *IN49
      /Free
       indMedadj = *on;
      /End-Free
      ******* End Add ********** 07/28/14 *****************************
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCRADS
     C                   MOVE      *ON           *IN13
     C                   OTHER
      ******* Beg Add ********** 07/28/14 *****************************
     C********           MOVE      *ON           *IN49
      /Free
       indMedadj = *on;
      /End-Free
      ******* End Add ********** 07/28/14 *****************************
     C                   ENDSL
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * ADJ COMP
     C     HFLD          WHENEQ    'SCRCAD'
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN52
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCRCAD
     C                   MOVE      *ON           *IN53
     C                   OTHER
     C                   MOVE      *ON           *IN52
     C                   ENDSL
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * STATE REPORT
     C     HFLD          WHENEQ    'SCRSRD'
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN53
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCRSRD
     C                   MOVE      *ON           *IN50
     C                   OTHER
     C                   MOVE      *ON           *IN53
     C                   ENDSL
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SUPERVISOR
     C     HFLD          WHENEQ    'SCRSSD'
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN50
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCRSSD
     C                   MOVE      *ON           *IN14
     C                   OTHER
     C                   MOVE      *ON           *IN50
     C                   ENDSL
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * UNIT/CLAIM MANAGER
     C     HFLD          WHENEQ    'SCRUMD'
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN51
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCRUMD
     C                   MOVE      *ON           *IN15
     C                   OTHER
     C                   MOVE      *ON           *IN51
     C                   ENDSL
      *
     C                   ENDSL
     C                   ELSE
     C                   MOVE      *ON           *IN92
     C                   ENDIF
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  Q2RECF=SCR03
      * ---------------------------------------------------------------
     C     Q2RECF        IFEQ      'SCR04'
      *
      * RESET ERROR INDICATORS
      *
     C                   MOVEA     '00000000'    *IN(22)                        *IN22 - 29
     C                   MOVEA     '000000'      *IN(30)                        *IN30 - 35
     C                   MOVEA     '0'           *IN(40)
     C                   MOVEA     '0000'        *IN(42)                        *IN42 - 45
     C                   MOVEA     '0000'        *IN(53)                        *IN53 - 56
     C                   MOVEA     '00'          *IN(65)                        *IN65 - 66
      *
     C     HFLD          IFEQ      'SCADRM'
     C                   MOVE      *BLANK        QQRMCD
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     *INKC         CABEQ     'A'           TERM
     C                   MOVEL(P)  'SCR04'       Q2RECF
      *
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN53
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCADRM
     C                   MOVE      *ON           *IN54
     C                   OTHER
     C                   MOVE      *ON           *IN53
     C                   ENDSL
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD=SCADRM
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     HFLD          IFEQ      'SCADRI'
     C                   MOVE      *BLANK        QQRMCD
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     *INKC         CABEQ     'A'           TERM
     C                   MOVEL(P)  'SCR04'       Q2RECF
      *
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN55
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCADRI
     C                   MOVE      *ON           *IN56
     C                   OTHER
     C                   MOVE      *ON           *IN55
     C                   ENDSL
      *
     C     QQRTC         IFEQ      '*'
     C                   MOVEL     QQRMCD        SCADRI
     C                   MOVE      *ON           *IN56
     C                   ENDIF
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD=SCADRI
      *
     C                   MOVE      *ON           *IN92
     C                   GOTO      STRT
     C                   ENDIF                                                  Q2RECF=SCR04
      * ---------------------------------------------------------------
     C     Q2RECF        IFEQ      'SCR05'
      *
      * RESET ERROR INDICATORS
      *
     C                   MOVEA     '00000000'    *IN(69)                        *IN69-76
     C                   MOVEA     '00'          *IN(77)                        *IN77-78
     C                   MOVEA     '0000000'     *IN(61)                        *IN61 - 67
     C                   MOVEA     '00'          *IN(78)                        *IN78 - 79
     C*** BEG ADD *** 03/16/12 ****************************************
      /free
       %subarr(strInd1:83:7) = '0000000';
       %subarr(strInd2:91:3) = '000';
      /end-free
     C*** END ADD *** 03/16/12 ****************************************
      *
     C     HFLD          IFEQ      'SCCERD'
     C                   MOVE      *BLANK        QQRMCD
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     *INKC         CABEQ     'A'           TERM
     C                   MOVEL(P)  'SCR05'       Q2RECF
      *
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN78
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCCERD
     C                   MOVE      *ON           *IN64
     C                   MOVE      *OFF          *IN78
     C                   OTHER
     C                   MOVE      *ON           *IN78
     C                   MOVE      *OFF          *IN64
     C                   ENDSL
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD=SCCERD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     HFLD          IFEQ      'SCCEDU'
      *
     C                   MOVEL(P)  'AXDU'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD            1
      *
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCCEDU
     C                   MOVEL(P)  'SCR05'       Q2RECF
     C*
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCNAM1
     C                   MOVE      *ON           *IN67                          POS CUR
     C                   MOVE      *OFF          *IN64                          POS CUR
     C                   ELSE
     C                   MOVE      *ON           *IN64                          POS CUR
     C                   MOVE      *OFF          *IN67                          POS CUR
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ON           *IN64                          POS CUR
     C                   MOVE      *OFF          *IN67                          POS CUR
     C                   ENDIF
     C*
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD=SCCEDU
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     HFLD          IFEQ      'SCCESU'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C                   MOVEL(P)  'SCR05'       Q2RECF
     C     ##RRN         CHAIN     PUP01                              99
     C*
     C                   MOVEL(P)  UPSTAF        SCCESU
     C                   MOVEL(P)  UPUSNM        SCNAM2
     C                   MOVE      *OFF          *IN67
     C                   MOVE      *ON           *IN69
     C                   ELSE
     C                   MOVE      *ON           *IN67
     C                   MOVE      *OFF          *IN69
     C                   ENDIF
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD=SCCESU
      *
     C     HFLD          IFEQ      'SCQRSD'
     C                   MOVE      *BLANK        QQRMCD
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     QQRMCD        PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
      *
     C     *INKC         CABEQ     'A'           TERM
     C                   MOVEL(P)  'SCR05'       Q2RECF
      *
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN69                          POS CUR
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCQRSD
     C                   MOVE      *ON           *IN68                          POS CUR
     C                   MOVE      *OFF          *IN69                          POS CUR
     C                   OTHER
     C                   MOVE      *ON           *IN69                          POS CUR
     C                   ENDSL
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD=SCQRSD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     HFLD          IFEQ      'SCQRDU'
      *
     C                   MOVEL(P)  'AXDU'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL            1
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD            1
      *
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCQRDU
     C                   MOVEL(P)  'SCR05'       Q2RECF
     C*
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCRDUD
     C                   MOVE      *ON           *IN74                          POS CUR
     C                   MOVE      *OFF          *IN68                          POS CUR
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ON           *IN68                          POS CUR
     C                   ENDIF
     C*
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD=SCQRDU
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     HFLD          IFEQ      'SCQRSU'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C                   MOVEL(P)  'SCR05'       Q2RECF
     C     ##RRN         CHAIN     PUP01                              99
     C*
     C                   MOVEL(P)  UPSTAF        SCQRSU
     C                   MOVEL(P)  UPUSNM        SCRSUD
     C                   MOVE      *OFF          *IN74                          POS CUR
     C                   MOVE      *ON           *IN81                          POS CUR
     C                   ELSE
     C                   MOVE      *ON           *IN74                          POS CUR
     C                   MOVE      *OFF          *IN69                          POS CUR
     C                   ENDIF
      *
     C                   ENDIF
      *
     C*** BEG ADD *** 03/16/12 *******************************************
      /free

        If (hfld = 'SCRCSC');

          qqRmcd = *blanks;
          ##Call = '2';

          GetCMSSubDC(##Call:##KC:##KL:##Rmcd:##Desc:##Rtcd);
          *INKC = ##KC;
          *INKL = ##KL;
          qqRmcd = ##Rmcd;

          If (*INKC = 'A');
             *inlr = *on;
          Endif;

          q2Recf = 'SCR05';

          Select;
          When (*inKL = *on);
             *in83 = *on;                                                 // POS CUR
          When (##Rmcd <> *blanks);
             scRcsc = ##Rmcd;
             *in85 = *on;                                                 // POS CUR
             *in83 = *off;                                                // POS CUR
          Other;
             *in83 = *on;                                                 // POS CUR
          Endsl;

          Exsr GotoStrt;

       endif;                                                             // (hfld = 'SCRCSC')

       If (hfld = 'SCRCDU');

          xxTbid = 'AXDU';
          xxkey  = *blanks;

          GetCMSSubDU(qqPrds:qqCall:xxTbid:xxKey:qqRtcd);

          If (qqRtcd = '*');
             scrcdu = xxKey;
             q2Recf = 'SCR05';

             Chain GTky pgt01;

             If %found;
                If (GTstat = 'A');
                   scnam3 = GTdesc;
                   *in89 = *on;                                           // POS CUR
                   *in85 = *off;                                          // POS CUR
                Endif;                                                    // (GTstat = 'A')
             Else;
                *in89 = *on;
             Endif;                                                       // %found;

          Endif;                                                          // (qqRtcd = '*')

          Exsr GotoStrt;

       Endif;                                                             // (hfld = 'SCRCDU')

       If (hfld = 'SCRCSU');

          ##CAll = '2';
          ##RRN  = *zeros;
          ##VIEW = 1;

          GetCMSSubSU(##CAll:##RRN:##VIEW);

          If (##RRN <> *zeros);
             q2Recf = 'SCR05';

             Chain ##RRN pup01;
               If %found;
                  scRcsu = UPstaf;
                  scNam4 = UPusnm;
                  *in89 = *off;
                  *in78 = *on;
               Endif;                                                     // %found
          Else;
            *in89 = *on;
            *in78 = *off;
          Endif;                                                          // (##RRN <> *zeros)

       Endif;                                                             // (hfld = 'SCRCSU')

      /end-free
     C*** END ADD *** 03/16/12 *******************************************
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  Q2RECF=SCR05
     C     Q2RECF        IFEQ      'SCR06'
     C     HFLD          IFEQ      'SCAPV1'
     C                   MOVE      *BLANK        QQRMCD
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C                   PARM      *BLANKS       ##RECD            2
      *
     C     *INKC         CABEQ     'A'           TERM
     C                   MOVEL(P)  'SCR06'       Q2RECF
      *
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN93
     C     ##KEY         WHENNE    *BLANKS
     C                   MOVEL     ##KEY         SCAPV1
     C                   MOVE      *ON           *IN94
     C                   MOVE      *OFF          *IN93
     C                   OTHER
     C                   MOVE      *ON           *IN93
     C                   MOVE      *OFF          *IN94
     C                   ENDSL
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD=SCAPV1
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     HFLD          IFEQ      'SCAPV2'
     C                   MOVE      *BLANK        QQRMCD
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C                   PARM      *BLANKS       ##RECD            2
      *
     C     *INKC         CABEQ     'A'           TERM
     C                   MOVEL(P)  'SCR06'       Q2RECF
      *
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN94
     C     ##KEY         WHENNE    *BLANKS
     C                   MOVEL     ##KEY         SCAPV2
     C                   MOVE      *ON           *IN95
     C                   MOVE      *OFF          *IN94
     C                   OTHER
     C                   MOVE      *ON           *IN94
     C                   MOVE      *OFF          *IN95
     C                   ENDSL
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD=SCAPV2
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     HFLD          IFEQ      'SCAPV3'
     C                   MOVE      *BLANK        QQRMCD
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C                   PARM      *BLANKS       ##RECD            2
      *
     C     *INKC         CABEQ     'A'           TERM
     C                   MOVEL(P)  'SCR06'       Q2RECF
      *
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN95
     C     ##KEY         WHENNE    *BLANKS
     C                   MOVEL     ##KEY         SCAPV3
     C                   MOVE      *ON           *IN96
     C                   MOVE      *OFF          *IN95
     C                   OTHER
     C                   MOVE      *ON           *IN95
     C                   MOVE      *OFF          *IN96
     C                   ENDSL
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD=SCAPV3
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     HFLD          IFEQ      'SCAPV4'
     C                   MOVE      *BLANK        QQRMCD
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C                   PARM      *BLANKS       ##RECD            2
      *
     C     *INKC         CABEQ     'A'           TERM
     C                   MOVEL(P)  'SCR06'       Q2RECF
      *
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN96
     C     ##KEY         WHENNE    *BLANKS
     C                   MOVEL     ##KEY         SCAPV4
     C                   MOVE      *ON           *IN97
     C                   MOVE      *OFF          *IN96
     C                   OTHER
     C                   MOVE      *ON           *IN96
     C                   MOVE      *OFF          *IN97
     C                   ENDSL
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD=SCAPV4
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     HFLD          IFEQ      'SCAPV5'
     C                   MOVE      *BLANK        QQRMCD
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C                   PARM      *BLANKS       ##RECD            2
      *
     C     *INKC         CABEQ     'A'           TERM
     C                   MOVEL(P)  'SCR06'       Q2RECF
      *
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *ON           *IN97
     C     ##KEY         WHENNE    *BLANKS
     C                   MOVEL     ##KEY         SCAPV5
     C                   MOVE      *ON           *IN97
     C                   MOVE      *OFF          *IN96
     C                   OTHER
     C                   MOVE      *ON           *IN97
     C                   MOVE      *OFF          *IN96
     C                   ENDSL
     C                   ENDIF                                                  HFLD=SCAPV5
      *
     C                   GOTO      STRT
     C                   ENDIF                                                  Q2RECF=SCR06
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     Q2RECF        IFEQ      'SCR07'
      *
      * RESET ERROR INDICATORS
     C                   MOVEA     '000'         *IN(21)                        *IN21-23
     C                   MOVEA     '00'          *IN(32)                        *IN31-32
      *
     C                   MOVEA     '0'           *IN(17)                        *IN17
     C                   GOTO      STRT
     C                   ENDIF
     C                   END                                                    *INKJ
      * -----------------------------------------------------------------
     C*
     C*   F12=CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C     Q2RECF        IFEQ      'SCR07'
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL(P)  'SCR07'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C     Q2RECF        IFEQ      'SCR04'
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C     Q2RECF        IFEQ      'SCR05'
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C     Q2RECF        IFEQ      'SCR06'
     C                   MOVEL(P)  'SCR05'       Q2RECF
     C                   GOTO      STRT
     C                   END
      *** Begin Add *** 11/12/20 ****************************************
       If Q2recf = 'SCR08';
          If ##Clt# = '025';
             Q2recf = 'SCR06';
          Else;
             Q2recf = 'SCR05';
          Endif;
          Exsr GotoStrt;
       Endif;
      *** End   Add *** 11/12/20 ****************************************
     C                   GOTO      TERM
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
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
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   F13=PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C                   CALL      'W0006R'
     C                   GOTO      TOP
     C                   END
     C*
     C*
     C                   END                                                    END OF FUNCTION KEYS
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C     Q2RECF        CABEQ     'SCR05'       SCR05T
     C     Q2RECF        CABEQ     'SCR06'       SCR06T
     C     Q2RECF        CABEQ     'SCR07'       SCR07T
      *** Begin Add *** 11/12/20 ****************************************
     C     Q2RECF        CABEQ     'SCR08'       SCR08T
      *** End   Add *** 11/12/20 ****************************************
     C* ====================================================================
     C*   PROCESS REQUEST SCREEN 1
     C*
     C     SCR01T        TAG
     C*
     C     *IN09         IFEQ      *ON
     C                   GOTO      INQ1
     C                   END
      *
      * RESET ERROR INDICATORS
      *
     C                   MOVEA     '00000'       *IN(20)                        *IN20 - 24
     C                   MOVEA     '000000'      *IN(25)                        *IN25 - 30
     C                   MOVEA     '0000000'     *IN(31)                        *IN31 - 37
     C                   MOVEA     '0'           *IN(40)                        *IN40
     C                   MOVEA     '0000'        *IN(42)                        *IN42 - 45
     C                   MOVEA     '0000000'     *IN(52)                        *IN52 - 58
     C                   MOVEA     '0'           *IN(65)                        *IN65
     C***** Beg Add *** 02/14/17 *******************************************
      /free
        erPBMLvl = *off;
        erPBMTyp = *off;
      /end-free
     C***** End Add *** 02/14/17 *******************************************
      *
      *  Edit Security Type, if Entered
      *
      *
      *
      *
     C*
     C*  THE CLAIM DELETION SECURITY LEVEL CANNOT BE < OR = 0
     C*
     C     SCCDL         IFLE      *ZERO
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE OVERRIDE CX DATE <= ACCIDENT DATE ERROR MSG SECURITY
     C*  LEVEL CANNOT BE < OR = 0
     C*
     C     SCOCAL        IFLE      *ZERO
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE EOMB CHECK RQST DELETION SEC LEVEL CANNOT BE < OR = 0
     C*
     C     SCCKDL        IFLE      *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE DELETE PENDED PAYMENT SEC LEVEL CANNOT BE < OR = 0
     C*
     C     SCDPPL        IFLE      *ZERO
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE PAY ON DENIAL CLAIMS SEC LEVEL CANNOT BE < OR = 0
     C*
     C     SCPDCL        IFLE      *ZERO
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE VOID/STOP/REVERSE PMT LEVEL CANNOT BE < OR = 0
     C*
     C     SCVSZL        IFLE      *ZERO
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE OVR FULL SALARY VOUCHER EDIT LEVEL CANNOT BE < OR = 0
     C*
     C     SCFSVL        IFLE      *ZERO
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  THE 0 PAYROLL SECURITY EDIT LEVEL CANNOT BE <= 0.
     C*
     C     SCAUSL        IFLE      *ZERO
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  DECISION DATE SECURITY EDIT LEVEL CANNOT BE <= 0.
     C*
     C                   SETOFF                                       39
     C     SCDSLV        IFLE      *ZERO
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C*  DUPLICATE PAYMENT SECURITY EDIT LEVEL CANNOT BE <= 0.
     C*
     C                   SETOFF                                       42
     C     SCDPLV        IFLT      *ZERO
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   ENDIF
      *
     C*
     C*  POSSIBLE DUPLICATE PAYMENT SECURITY EDIT LEVEL CANNOT BE <= 0.
     C*
     C                   SETOFF                                       74
     C     SCDPL2        IFLT      *ZERO
     C                   MOVE      *ON           *IN74
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  Medical Benificiary Flag security edit level cannot be <= 0.
     C*
     C     SCMBS         CABLT     *ZERO         STRT                   76
     C*
     C*  Medical Benificiary Flag security edit level cannot be <= 0.
     C*
     C     SCRBS         CABLT     *ZERO         STRT                   78
     C*
     C***** Beg Add *** 02/14/17 ***************************************
     C*
     C*  PBM Eligible level is < = 0
     C*
     C                   Eval      erPBMLvl = *off
     C     scPBMLvl      IFLT      *ZERO
     C                   Eval      erPBMLvl = *on
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C***** End Add *** 02/14/17 ***************************************
     C*
     C     INQ1          TAG
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C* ====================================================================
     C*   PROCESS REQUEST SCREEN 2
     C*
     C     SCR02T        TAG
     C*
     C     *IN09         IFEQ      *ON
     C                   GOTO      INQ2
     C                   END
      *
      * RESET ERROR INDICATORS
      *
     C                   MOVEA     '00'          *IN(20)                        *IN20 - 21
     C                   MOVEA     '00000000'    *IN(22)                        *IN22 - 29
     C                   MOVEA     '0000'        *IN(30)                        *IN30 - 33
     C                   MOVEA     '0000'        *IN(42)                        *IN42 - 45
     C                   MOVEA     '00000'       *IN(52)                        *IN52 - 56
     C                   MOVEA     '000000'      *IN(77)                        *IN77 - 82
     C                   MOVEA     '0000000'     *IN(83)                        *IN83 - 89
     C*
     C*  THE LOCATION ENTRY FLAG MUST BE 'Y' OR 'N'
     C*
     C     SCLOCT        IFNE      'Y'
     C     SCLOCT        ANDNE     'N'
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE PAYMENT STATUS CODE DEFAULT WHEN RELEASED PMT IS PENDED
      *
     C     SCPMTS        IFEQ      *BLANK
     C                   MOVE      *ON           *IN78                          BLANK
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   MOVEL(P)  SCPMTS        XXKEY
     C                   MOVE      *BLANK        SCPMTD
     C     PSKY          CHAIN     PPS01                              79
     C     *IN79         IFEQ      *ON                                          INVALID TAB VAL
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL(P)  PSDESC        SCPMTD
     C                   END
     C     PSTYPE        IFNE      'P'
     C                   MOVE      *ON           *IN77                          NOT PMT TYPE
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE PAYMENT STATUS CODE DEFAULT WHEN PENDED PMT IS RELEASED
      *
     C     SCPSC2        IFNE      *BLANK
     C                   MOVEL(P)  SCPSC2        XXKEY
     C                   MOVE      *BLANK        SCPSCD
     C     PSKY          CHAIN     PPS01                              81
     C     *IN81         IFEQ      *ON                                          INVALID TAB VAL
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL(P)  PSDESC        SCPSCD
     C                   END
      *
     C     PSTYPE        IFNE      'R'
     C                   MOVE      *ON           *IN82                          NOT PMT TYPE
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*  THE DUPLICATE PAYMENT STATUS CODE DEFAULT WHEN PENDED PMT IS RELEASED
      *
     C     SCDPPC        IFNE      *BLANK
     C                   MOVEL(P)  SCDPPC        XXKEY
     C                   MOVE      *BLANK        SCDPPD
     C     PSKY          CHAIN     PPS01                              35
     C     *IN35         IFEQ      *ON                                          INVALID TAB VAL
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL(P)  PSDESC        SCDPPD
     C                   ENDIF
     C                   ENDIF
      *
     C     SCEDIM        IFLT      0
     C                   SETON                                        36
     C                   GOTO      STRT
     C                   ENDIF                                                  SCEDIM<0
     C*
     C*  ALLOW CLAIM CLOSING IF PENDED PAYMENTS EXIST
     C*
     C     SCACCP        IFNE      'Y'
     C     SCACCP        ANDNE     'N'
     C     SCACCP        ANDNE     *BLANK
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK RESERVE CODES ON 'RESERVE ON CONTROVERT' FIELDS
     C*
     C     SCRT1         IFNE      *BLANK
     C     SCRT1         ANDNE     'C'
     C     SCRT1         ANDNE     'M'
     C     SCRT1         ANDNE     'L'
     C     SCRT1         ANDNE     'E'
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C     SCRT2         IFNE      *BLANK
     C     SCRT2         ANDNE     'C'
     C     SCRT2         ANDNE     'M'
     C     SCRT2         ANDNE     'L'
     C     SCRT2         ANDNE     'E'
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C     SCRT3         IFNE      *BLANK
     C     SCRT3         ANDNE     'C'
     C     SCRT3         ANDNE     'M'
     C     SCRT3         ANDNE     'L'
     C     SCRT3         ANDNE     'E'
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C     SCRT4         IFNE      *BLANK
     C     SCRT4         ANDNE     'C'
     C     SCRT4         ANDNE     'M'
     C     SCRT4         ANDNE     'L'
     C     SCRT4         ANDNE     'E'
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE MAJOR PERIL CODE DEFAULT AGAINST TABLE VALUE
     C*
     C                   MOVEL(P)  'MAJP'        XXTBID
     C                   MOVEL(P)  SCMPCD        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE HEADING FOR CATEGORY 2 IS REQUIRED  (W0070R07/SCR01)
     C*
     C     SCCAT2        IFEQ      *BLANK
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE HEADING FOR ADJUSTER IS REQUIRED (W0070R09/SCR01)
     C*
     C     SCADJ         IFEQ      *BLANK
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C*  INITIAL SORT VALUE FOR 'WORK WITH CLAIMS' SCREEN (W0070R)
     C*
     C     SCISC         IFNE      *BLANK
     C     SCISC         ANDNE     'CA'
     C     SCISC         ANDNE     'SS'
     C     SCISC         ANDNE     'CN'
     C     SCISC         ANDNE     'SC'
     C                   MOVE      *ON           *IN31
     C                   GOTO      STRT
     C                   END
     C*
     C*  ENTER RESERVE CHANGE (Y) OR TOTAL INCURRED (N/ )
     C*
     C     SCRCHG        IFNE      'Y'
     C     SCRCHG        ANDNE     'N'
     C     SCRCHG        ANDNE     *BLANK
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  Enter 'Y' to have 'Record Only' claims included when printing
     C*  claim labels otherwise leave blank
     C*
     C     SCRLBL        IFNE      'Y'
     C     SCRLBL        ANDNE     'N'
     C     SCRLBL        ANDNE     ' '
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     INQ2          TAG
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL(P)  'SCR07'       Q2RECF
     C*** BEG ADD ****** 04/13/12 *********************************************
      /free
         Read sfb02;

           If Not %EOF(SFBPF);
              xxncin = %Subst(FBST07:1:1);

                If xxncin <> 'Y';
                  *In33 = *On;
                Else;
                  *In33 = *Off;
                Endif;

           Else;
                *In33 = *Off;
           Endif;

      /end-free
     C*** END ADD ****** 04/13/12 *********************************************
     C                   GOTO      STRT
     C*
     C*
     C* ====================================================================
     C*   PROCESS REQUEST SCREEN 7
     C*
     C     SCR07T        TAG
     C*
     C     *IN09         IFEQ      *ON
     C                   GOTO      INQ7
     C                   END
      *
      * RESET ERROR INDICATORS
      *
     C                   MOVEA     '0'           *IN(17)                        *IN17
     C*
     C***Open/Close table description flag Y or N
     C*
     C     SCOPCL        IFNE      *BLANK
     C     SCOPCL        ANDNE     'Y'
     C     SCOPCL        ANDNE     'N'
     C                   MOVE      *ON           *IN17
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEA     '000'         *IN(21)                        *IN21-23
     C*
     C     SCDOHR        IFNE      *BLANK
     C     SCDOHR        ANDNE     'Y'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C     SCDOBR        IFNE      *BLANK
     C     SCDOBR        ANDNE     'Y'
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C     SCGDRR        IFNE      *BLANK
     C     SCGDRR        ANDNE     'Y'
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C     SCEXNM        IFNE      *BLANK
     C     SCEXNM        ANDNE     'Y'
     C     SCEXNM        ANDNE     'N'
     C                   MOVE      *ON           *IN19
     C                   GOTO      STRT
     C                   ENDIF
     C                   MOVEA     '00'          *IN(32)                        *IN32-33
     C*
     C     SCLSRV        IFNE      *BLANK
     C     SCLSRV        ANDNE     'Y'
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C     SCWMTH        IFNE      *BLANK
     C     SCWMTH        ANDNE     'Y'
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   END
      ****** Begin Add **** 09/01/17 ****************************************
      /Free
       If SCcvt# <> *blanks and SCcvt# <> 'Y';
          SCcvt# = *blanks;
       Endif;

       MsgCvt# = *blanks;
       ErrCvt# = *off;
       ##rtcd = *blanks;
       ##co#  = %editc(xxco#:'X');
       ##wcf# = %editc(xxfnd:'X');
       ##sub# = *zeros;
       ##fnd  = *zeros;
       //*** Beg Add *** 09/09/21 **************************************
       If isWcMc = '1';
       //*** End Add *** 09/09/21 **************************************
          FA700R(##rtcd:##co#:##wcf#:##sub#:##fnd);
          If ##rtcd = 'F';
          //** Begin Delete *** 03/27/18 *********************************
          //**  MsgCvt#   = 'Unable to retrieve subscriber number.';
          //**  ErrCvt#   = *on;
          //**  Exsr GotoStrt;
          //** End Delete *** 03/27/18 *********************************
          Endif;
       //*** Beg Add *** 09/09/21 **************************************
       Endif;
       //*** End Add *** 09/09/21 **************************************
      /End-Free
      ****** End   Add **** 09/01/17 ****************************************
     C*
     C     INQ7          TAG
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C* ====================================================================
     C*   PROCESS REQUEST SCREEN 3
     C*
     C     SCR03T        TAG
     C*
     C     *IN09         IFEQ      *ON
     C                   GOTO      INQ3
     C                   END
      *
      * RESET ERROR INDICATORS
      *
     C                   MOVEA     '0000'        *IN(12)                        *IN12 - 15
     C                   MOVEA     '0'           *IN(20)                        *IN20
     C                   MOVEA     '00000000'    *IN(21)                        *IN21 - 28
     C                   MOVEA     '0'           *IN(29)                        *IN29
     C                   MOVEA     '000'         *IN(30)                        *IN30 - 32
     C                   MOVEA     '00000000'    *IN(33)                        *IN33 - 40
     C                   MOVEA     '0'           *IN(41)                        *IN41
     C                   MOVEA     '0000'        *IN(42)                        *IN42 - 45
     C                   MOVEA     '0000000'     *IN(47)                        *IN47 - 53
     C                   MOVEA     '00000000'    *IN(58)                        *IN58 - 65
     C                   MOVEA     '00'          *IN(66)                        *IN66 - 67
     C*** Indicator *IN75 will not be setoff
     C                   MOVEA     '00000'       *IN(70)                        *IN70 - 74
     C                   MOVEA     '00'          *IN(76)                        *IN76 - 77
     C                   MOVEA     '000'         *IN(78)                        *IN78 - 80
     C                   MOVEA     '0000'        *IN(85)                        *IN85 - 88
     C                   MOVE      '0'           *IN99
     C                   MOVE      '0'           *IN89
      *
      * EDIT DARTS CODES
      *
     C                   DO        12            Y
      *
     C     BLNK(Y)       IFEQ      'Y'
     C     Y             OREQ      2
     C     RK1(1)        ANDNE     *BLANKS
      *
      * DARTS CODE CANNOT BE BLANK
      *
     C     RK1(Y)        IFEQ      *BLANKS
     C     Y             ADD       69            X
     C                   MOVE      '1'           *IN(X)
     C                   MOVE      '1'           *IN99
     C                   ENDIF                                                  RK1,Y
      *
     C*** Begin Add *** 07/16/18 ***********************************
     C                   else
     C     Y             IfEQ      12
     C     RK1(Y)        ANDEQ     *BLANKS
     C     scAadr        ANDEQ     'Y'
     C     Y             ADD       69            X
     C                   MOVE      '1'           *IN69
     C                   MOVE      '1'           *IN99
     C                   ENDIF                                                  RK1,Y
     C*** End   Add *** 07/16/18 ***********************************
     C                   ENDIF                                                  BLNK,Y
      *
      * VALIDATE DARTS CODE
      *
     C     *IN99         IFEQ      *OFF
      *
     C     RK1(Y)        IFNE      *BLANKS
      *
     C                   MOVE      RK1(Y)        XXDART
     C     CTKY          CHAIN     PCT01                              81
      *
     C     *IN81         IFEQ      *ON
     C                   MOVE      '0'           *IN81
     C     Y             ADD       57            X
     C                   MOVE      '1'           *IN(X)
     C                   MOVE      '1'           *IN99
     C                   ENDIF                                                  *IN81 IFEQ
      *
     C                   ENDIF                                                  RK1,Y IFNE
      *
     C                   ENDIF                                                  *IN80 IFEQ
      *
      * CHECK THAT DARTS CODE IS ACTIVE
      *
     C     *IN99         IFEQ      *OFF
      *
     C     RK1(Y)        IFNE      *BLANKS
      *
     C     CTSTAT        IFNE      'A'
     C     Y             ADD       20            X
     C                   MOVE      '1'           *IN(X)
     C                   MOVE      '1'           *IN99
     C                   ENDIF                                                  CTSTAT IFNE
      *
     C                   ENDIF                                                  RK1,Y IFNE
      *
     C                   ENDIF                                                  *IN99 IFEQ
      *
      * CHECK THAT DARTS CODE IS DIARY TYPE
      *
     C     *IN99         IFEQ      *OFF
      *
     C     RK1(Y)        IFNE      *BLANKS
      *
     C     DIRY(Y)       IFEQ      'Y'
      *
     C     CTFL01        IFNE      'Y'
     C     Y             ADD       32            X
     C                   MOVE      '1'           *IN(X)
     C                   MOVE      '1'           *IN99
     C                   ENDIF                                                  CTFL01 IFNE
      *
     C                   ENDIF                                                  DIRY,Y IFNE
      *
     C                   ENDIF                                                  RK1,Y IFNE
      *
     C                   ENDIF                                                  *IN99 IFEQ
      *
      *
     C  N99              ENDDO                                                  DO 9 Y
      *
     C     *IN99         IFEQ      *ON
     C                   MOVE      *OFF          *IN99
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN99 IFEQ
      *
      *  DAYS CANNOT BE LESS THAN ZERO
      *
     C     SCRODY        IFLT      *ZERO
     C                   MOVE      *ON           *IN85
     C                   GOTO      STRT
     C                   END
      *
     C     SCBADJ        IFLT      *ZERO
     C                   MOVE      *ON           *IN86
     C                   GOTO      STRT
     C                   END
     C*
     C     SCBSPV        IFLT      *ZERO
     C                   MOVE      *ON           *IN87
     C                   GOTO      STRT
     C                   END
     C*
     C     SCBMGR        IFLT      *ZERO
     C                   MOVE      *ON           *IN88
     C                   GOTO      STRT
     C                   END
     C***CLIENT 25 WAVE CALC
     C*
     C     *IN01         IFEQ      *ON
     C     SCWCR         ANDNE     *BLANKS
     C                   MOVE      SCWCR         XXDART
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C     CTSTAT        ORNE      'A'
     C                   MOVE      *ON           *IN08
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      ******** Beg Add ****** 07/28/14 ***************************************
      /free
       isClmAct = *off;
       If scClmAct  <> ' ';
          Chain (scClmAct) pct01;
          If not %found;
             errAcplRmk = *on;
             isClmAct  = *on;
          Endif;
          If CTstat  =  'I';
             errAcrDart = *on;
             isClmAct = *on;
          Endif;
       Endif;
      /end-free
      ******** End Add ****** 07/28/14 ***************************************
     C***DEVEREUX FLAG MUST BE Y OR BLANK IF CLT# IS 114
     C*
     C     *IN16         IFEQ      *ON
     C     SCDEVF        ANDNE     'Y'
     C     SCDEVF        ANDNE     ' '
     C                   MOVE      *ON           *IN80
     C                   GOTO      STRT
     C                   ENDIF
     C***Claim Diaries access summary screen flag can be Y N or ' '
     C*
     C     SCCDSM        IFNE      *BLANK
     C     SCCDSM        ANDNE     'Y'
     C     SCCDSM        ANDNE     'N'
     C                   MOVE      *ON           *IN74
     C                   GOTO      STRT
     C                   ENDIF
     C***EXCLUSION MUST BE Y OR BLANK IF CLT# IS 114
     C*
     C     *IN16         IFEQ      *ON
     C     SCFL01        ANDNE     'Y'
     C     SCFL01        ANDNE     ' '
     C                   MOVE      *ON           *IN89
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     INQ3          TAG
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
      ******** Beg Chg ****** 07/28/14 ***************************************
     C*******            MOVEL(P)  'SCR04'       Q2RECF
      /free
       If isClmAct = *off;
          Q2recf = 'SCR04';
       Endif;
      /end-free
      ******** End Chg ****** 07/28/14 ***************************************
     C                   GOTO      STRT
     C*
     C* ====================================================================
     C*   PROCESS REQUEST SCREEN 4
     C*
     C     SCR04T        TAG
     C*
     C     *IN09         IFEQ      *ON
     C                   GOTO      INQ4
     C                   END
      *
      * RESET ERROR INDICATORS
      *
     C                   MOVEA     '00000000'    *IN(22)                        *IN22 - 29
     C                   MOVEA     '000000'      *IN(30)                        *IN30 - 35
     C                   MOVEA     '0'           *IN(40)
     C                   MOVEA     '0000'        *IN(42)                        *IN42 - 45
     C                   MOVEA     '0000'        *IN(53)                        *IN53 - 54
     C                   MOVEA     '00'          *IN(65)                        *IN65 - 66
     C*
     C*  DISPLAY THE MEDICAL INFO SCREEN IN 'WORK WITH CLAIMS'?
     C*
     C     SCMEDI        IFNE      *BLANK
     C     SCMEDI        ANDNE     'Y'
     C     SCMEDI        ANDNE     'N'
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  DISPLAY THE BENEFIT INFO SCREEN IN 'WORK WITH CLAIMS'?
     C*
     C     SCBENI        IFNE      *BLANK
     C     SCBENI        ANDNE     'Y'
     C     SCBENI        ANDNE     'N'
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  DISPLAY THE RESERVE/PAYMENT SUMMARY SCREEN IN 'WORK WITH CLAIMS'?
     C*
     C     SCRPS         IFNE      *BLANK
     C     SCRPS         ANDNE     'Y'
     C     SCRPS         ANDNE     'N'
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  DISPLAY THE STATE SPECIFIC SCREEN IN 'WORK WITH CLAIMS'?
     C*
     C     SCSTSP        IFNE      *BLANK
     C     SCSTSP        ANDNE     'Y'
     C     SCSTSP        ANDNE     'N'
     C                   MOVE      *ON           *IN31
     C                   GOTO      STRT
     C                   END
     C*** Beg Add ****** 08/29/22 *********************************************
     C*
     C*  DISPLAY Print FROI Screen IN 'WORK WITH CLAIMS'?
     C*
     C     SCPFRO        IFNE      *BLANK
     C     SCPFRO        ANDNE     'Y'
     C     SCPFRO        ANDNE     'N'
     C                   MOVE      *ON           *IN36
     C                   GOTO      STRT
     C                   END
     C*
     C*** End Add ****** 08/29/22 *********************************************
     C*
     C*  DISPLAY THE DARTS BENEFIT DIARY SCREEN IN 'WORK WITH CLAIMS'?
     C*
     C     SCDBEN        IFNE      *BLANK
     C     SCDBEN        ANDNE     'Y'
     C     SCDBEN        ANDNE     'N'
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  DISPLAY THE DARTS SCREEN IN 'WORK WITH CLAIMS'?
     C*
     C     SCDART        IFNE      *BLANK
     C     SCDART        ANDNE     'Y'
     C     SCDART        ANDNE     'N'
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   END
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*  AUTO CLAIM CLOSING CRITERIA
     C*
     C*    MEDICAL ONLY CLAIMS
     C*
     C     SCMOC         IFNE      *BLANK
     C     SCMOC         ANDNE     'Y'
     C     SCMOC         ANDNE     'N'
     C                   MOVE      *ON           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C*    ACTUAL CLAIM CLOSING
     C*
     C     SCACCM        IFNE      *BLANK
     C     SCACCM        ANDNE     'Y'
     C     SCACCM        ANDNE     'N'
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   END
      *
      * NUMBER OF DAYS OF NO FINANCIAL ACTIVITY
      *
     C     SCNFAM        IFLT      *ZERO
     C                   MOVE      *ON           *IN66
     C                   GOTO      STRT
     C                   END
      ** *******
      * MEDICAL ONLY CLAIMS
      *
     C     SCMOC         IFEQ      'Y'
     C*
     C* ACTUAL CLAIM CLOSING MUST BE  Y  OR  N
     C*
     C     SCACCM        IFNE      'Y'
     C     SCACCM        ANDNE     'N'
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   END
      *
      * ACTUAL CLAIM CLOSING = Y
      *
     C     SCACCM        IFEQ      'Y'
      *
      * MUST HAVE # DAYS OF NO FINANCIAL ACTIVITY
      *
     C     SCNFAM        IFEQ      *ZERO
     C                   MOVE      *ON           *IN66
     C                   GOTO      STRT
     C                   END
      *
      * IF ACTUAL CLAIM CLOSING = Y, THERE MUST NOT BE A DIARY REMARK.
      *
     C     SCADRM        IFNE      *BLANK
     C                   MOVE      *ON           *IN43                          CANT HAVE RMK
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    SCACCM = Y
      *
      * ACTUAL CLAIM CLOSING = N
      *
     C     SCACCM        IFEQ      'N'
      *
      * MUST HAVE # DAYS OF NO FINANCIAL ACTIVITY
      *
     C     SCNFAM        IFEQ      *ZERO
     C                   MOVE      *ON           *IN66
     C                   GOTO      STRT
     C                   END
      *
      * THERE MUST BE A DIARY REMARK.
      *
     C     SCADRM        IFEQ      *BLANK
     C                   MOVE      *ON           *IN42                          CANT BE BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    SCACCM = N
     C*
     C                   END                                                    SCMOC  = Y
     C*
     C*    ADJUSTER DIARY REMARK
     C*
     C     SCADRM        IFNE      *BLANK
     C                   MOVEL(P)  SCADRM        XXDART
     C     CTKY          CHAIN     PCT01                              81
     C     *IN81         IFEQ      *ON
     C                   MOVE      *ON           *IN65
     C                   GOTO      STRT
     C                   END
     C     CTSTAT        IFNE      'A'
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   END
     C                   END
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C*    INDEMNITY CLAIMS
     C*
     C     SCINC         IFNE      *BLANK
     C     SCINC         ANDNE     'Y'
     C     SCINC         ANDNE     'N'
     C                   MOVE      *ON           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C*  AUTO CLAIM CLOSING CRITERIA
     C*    ACTUAL CLAIM CLOSING
     C*
     C     SCACCI        IFNE      *BLANK
     C     SCACCI        ANDNE     'Y'
     C     SCACCI        ANDNE     'N'
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   END
      **************
      * NUMBER OF DAYS OF NO FINANCIAL ACTIVITY
      *
     C     SCINC         IFEQ      'Y'
      *
     C* ACTUAL CLAIM CLOSING MUST BE  Y  OR  N
     C*
     C     SCACCI        IFNE      'Y'
     C     SCACCI        ANDNE     'N'
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   END
      *
      * ACTUAL CLAIM CLOSING = Y
      *
     C     SCACCI        IFEQ      'Y'
      *
      * MUST HAVE # DAYS OF NO FINANCIAL ACTIVITY
      *
     C     SCNFAI        IFEQ      *ZERO
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   END
      *
      * IF ACTUAL CLAIM CLOSING = Y, THERE MUST NOT BE A DIARY REMARK.
      *
     C     SCADRI        IFNE      *BLANK
     C                   MOVE      *ON           *IN45                          CANT HAVE RMK
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    SCACCI = Y
      *
      * ACTUAL CLAIM CLOSING = N
      *
     C     SCACCI        IFEQ      'N'
      *
      * MUST HAVE # DAYS OF NO FINANCIAL ACTIVITY
      *
     C     SCNFAI        IFEQ      *ZERO
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   END
      *
      * THERE MUST BE A DIARY REMARK.
      *
     C     SCADRI        IFEQ      *BLANK
     C                   MOVE      *ON           *IN44                          CANT BE BLANK
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    SCACCI = N
     C*
     C                   END                                                    SCINC  = Y
     C*
     C*    ADJUSTER DIARY REMARK
     C*
     C     SCADRI        IFNE      *BLANK
     C                   MOVEL(P)  SCADRI        XXDART
     C     CTKY          CHAIN     PCT01                              81
     C     *IN81         IFEQ      *ON
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   END
     C     CTSTAT        IFNE      'A'
     C                   MOVE      *ON           *IN35
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     INQ4          TAG
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL(P)  'SCR05'       Q2RECF
     C                   GOTO      STRT
     C* ====================================================================
     C*   PROCESS REQUEST SCREEN 5
     C*
     C     SCR05T        TAG
     C*
     C     *IN09         IFEQ      *ON
     C                   GOTO      INQ5
     C                   END
      *
     C                   MOVEA     '0000000'     *IN(61)                        *IN61 - 67
     C                   MOVEA     '00'          *IN(78)                        *IN78 - 79
     C                   MOVEA     '00'          *IN(81)                        *IN81-82
     C*** BEG ADD *** 03/16/12 *******************************************
      /free
       %subarr(strInd1:83:7) = '0000000';
       %subarr(strInd2:91:3) = '000';
      /end-free
     C*** END ADD *** 03/16/12 ****************************************
      *
     C     SCCERD        IFNE      *BLANKS
     C                   MOVE      SCCERD        XXDART
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C     CTSTAT        ORNE      'A'
     C                   SETON                                        61
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN81 = *ON
     C                   ENDIF                                                  SCCERD <> ' '
      *
     C     SCCEDU        IFNE      *BLANKS
     C                   MOVEL(P)  'AXDU'        XXTBID
     C                   MOVEL(P)  SCCEDU        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   MOVE      *ON           *IN62
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF                                                  SCCEDU <> ' '
      *
     C     SCCESU        IFNE      *BLANKS
     C                   MOVEL(P)  SCCESU        XXSTAF
     C     XXSTAF        CHAIN     PUP02                              60
     C     *IN60         IFEQ      *ON
     C     UPSTAT        ORNE      'A'
     C                   MOVE      *ON           *IN65
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF                                                  SCCEDU <> ' '
      *
     C     SCCERD        IFNE      *BLANKS
     C     SCCEDU        ANDEQ     *BLANKS
     C     SCCESU        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN66
     C                   MOVE      *ON           *IN79
     C                   GOTO      STRT
     C                   ENDIF                                                  SCCERD <> ' '
      *
     C     SCCERD        IFNE      *BLANKS
     C     SCCEDU        ANDNE     *BLANKS
     C     SCCESU        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN63
     C                   GOTO      STRT
     C                   ENDIF                                                  SCCERD <> ' '
      *
     C*    QUERY RESPONSE DIARY CODE VALIDATION
     C*
     C     SCQRSD        IFNE      *BLANK
     C                   MOVEL(P)  SCQRSD        XXDART
     C     CTKY          CHAIN     PCT01                              81
     C     *IN81         IFEQ      *ON
     C                   MOVE      *ON           *IN70
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
      *IF QUERY RESPONSE DIARY CODE IS ENTERED THEN DIARY USER OR
      *SPECIFIC USER MUST BE ENTERED.
     C*
     C     SCQRSD        IFNE      *BLANK
     C     SCQRSU        ANDEQ     *BLANK
      *
     C     SCQRDU        IFEQ      *BLANK
     C                   MOVE      *ON           *IN71
     C                   MOVE      *ON           *IN75
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      *IF DIARY USER IS ENTERED THEN SPECIFIC USER CAN NOT BE ENTERED.
     C*
     C     SCQRDU        IFNE      *BLANK
     C     SCQRSU        ANDNE     *BLANK
     C                   MOVE      *ON           *IN72
     C                   MOVE      *ON           *IN76
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  VALIDATE QUERY RESPONSE DIARY USER
     C*
     C     SCQRDU        IFNE      *BLANKS
     C                   MOVEL(P)  'AXDU'        XXTBID
     C                   MOVEL(P)  SCQRDU        XXKEY
     C     GTKY          CHAIN     PGT01                              81
     C     *IN81         IFEQ      *ON
     C                   MOVE      *ON           *IN73
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*
      *IF QUERY RESPONSE DIARY CODE IS ENTERED THEN DIARY USER OR
      *SPECIFIC USER MUST BE ENTERED.
     C     SCQRSD        IFNE      *BLANK
     C     SCQRDU        ANDEQ     *BLANK
      *
     C     SCQRSU        IFEQ      *BLANK
     C                   MOVE      *ON           *IN75
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
      *IF SPECIFIC USER IS ENTERED THEN DIARY USER CAN NOT BE ENTERED.
     C*
     C     SCQRSU        IFNE      *BLANK
     C     SCQRDU        ANDNE     *BLANK
     C                   MOVE      *ON           *IN76
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  VALIDATE QUERY RESPONSE SPECIFIC USER
     C*
     C     SCQRSU        IFNE      *BLANKS
     C     SCQRSU        CHAIN     PUP02                              81
     C     *IN81         IFEQ      *ON
     C                   MOVE      *ON           *IN77
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCMBQF        IFNE      'Y'
     C     SCMBQF        ANDNE     ' '
     C                   MOVE      *ON           *IN82
     C                   GOTO      STRT
     C                   ENDIF
     C*** BEG ADD *** 03/16/12 *********************************************
      *    CMS Submission Diary code validation
      /free

       If (scRcsc <> *blanks);
          xxdart = scRcsc;
          Chain CTky pct01;
          If not %Found;
             *in84 = *on;
             Exsr GotoStrt;
          Endif;                                                          // not %found
       Endif;                                                             // (scRcsc <> *blanks)

       // If CMS Submission Diary Code is entered then Diary user
       // or specific user must be enetred.

       If (scRcsc <> *blanks and scRcsu = *blanks);

          If (scRcdu = *blanks);
             *in86 = *on;
             *in91 = *on;
             Exsr GotoStrt;
          Endif;                                                          // (scRcdu = *blanks)
       Endif;                                         // (scRcsc <> *blanks and scRcsu = *blanks)

       // If Diary User is entered then specific user can not
       // be entered.

       If (scRcdu <> *blanks and scRcsu <> *blanks);

          *in87 = *on;
          *in92 = *on;
          Exsr GotoStrt;
       Endif;                                         // (scRcdu <> *blanks and scRcsu <> *blanks)

       // Validate CMS Submission flag diary User

       If (ScRcdu <> *blanks);
          xxTbid = 'AXDU';
          xxKey  = scRcdu;

          Chain GTky pgt01;
          If not %found;
             *in88 = *on;
             Exsr GotoStrt;
          Endif;                                                          // not %found
       Endif;                                                             // (ScRcdu <> *blanks)

       // If CMS submission flag Diary Code is enetered then
       // diary user or specific user must be entered.

       If (scRcsc <> *blanks and scRcdu = *blanks);

          If (scRcsu = *blanks);
             *in91 = *on;
             Exsr GotoStrt;
          Endif;                                                          // (scRcsu = *blanks)
       Endif;                                         // (scRcsc <> *blanks and scRcdu = *blanks)

       // If specific user is eneterd then diary user can not
       // be entered.

       If (scRcsu <> *blanks and scRcdu <> *blanks);
          *in92 = *on;
          Exsr GotoStrt;
       Endif;                                         // (scRcsu <> *blanks and scRcdu <> *blanks)

       // Validate CMS submission Flag Specific User

       If (scRcsu <> *blanks);

           Chain scRcsu pup02;
           If not %found;
              *in93 = *on;
              Exsr GotoStrt;
           Endif;                                                         // not %found
       Endif;                                                             // (scRcsu <> *blanks)

      /end-free
     C*** END ADD *** 03/16/12 *********************************************
     C* ----------------------------------------------------------------
     C     INQ5          TAG
     C* SCREEN 6 IS ONLY FOR CLIENT '025'
     C     ##CLT#        IFEQ      '025'
     C                   MOVEL(P)  'SCR06'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C                   ENDIF
     C* ====================================================================
     C*   PROCESS REQUEST SCREEN 6
     C*
     C     SCR06T        TAG
     C*
     C     *IN09         IFEQ      *ON
     C                   GOTO      INQ6
     C                   END
      *** Begin Add  *** 11/12/20 *************************************
     C     INQ6          TAG
       Q2recf = 'SCR08';
       Exsr GotoStrt;
     C* ====================================================================
     C*   PROCESS REQUEST SCREEN 8
     C*
     C     SCR08T        TAG
      * No validation needed for SCR08
     C     *IN09         IFEQ      *ON
     C                   GOTO      INQ8
     C                   END
      *** End  Add  *** 11/12/20 *************************************


     C     ALLSET        TAG
     C*  ALL EDIT HAVE BEEN PASSED, UPDATE/WRITE RECORD.
     C*
     C     CXKY          CHAIN     PCX01                              40
      *** Begin Add *** 11/12/20 **************************************
       Chain CXKY pcx02;
      *** End   Add *** 11/12/20 **************************************
     C                   Z-ADD     XXCO#         CXCO#
     C                   Z-ADD     XXFND         CXFND
      * SCREEN 1
     C                   Z-ADD     SCCDL         CXSECL
     C                   Z-ADD     SCOCAL        CXSECC
     C                   Z-ADD     SCCKDL        CXCKDL
     C                   Z-ADD     SCDPPL        CXDLTD
     C                   Z-ADD     SCPDCL        CXPYOD
     C                   Z-ADD     SCVSZL        CXVSZL
     C                   Z-ADD     SCFSVL        CXFSVL
     C                   MOVEL     SCCDT         CXCDT
     C                   MOVEL     SCOCAT        CXOCAT
     C                   MOVEL     SCCKDT        CXCKDT
     C                   MOVEL     SCDPPT        CXDPPT
     C                   MOVEL     SCPDCT        CXPDCT
     C                   MOVEL     SCVSZT        CXVSZT
     C                   MOVEL     SCFSVT        CXFSVT
     C                   MOVEL     SCCDT2        CXCDT2
     C                   MOVEL     SCCDT3        CXCDT3
     C                   MOVEL     SCCDT4        CXCDT4
     C                   MOVEL     SCCDT5        CXCDT5
     C                   MOVEL     SCOCA2        CXOCA2
     C                   MOVEL     SCOCA3        CXOCA3
     C                   MOVEL     SCOCA4        CXOCA4
     C                   MOVEL     SCOCA5        CXOCA5
     C                   MOVEL     SCCKD2        CXCKD2
     C                   MOVEL     SCCKD3        CXCKD3
     C                   MOVEL     SCCKD4        CXCKD4
     C                   MOVEL     SCCKD5        CXCKD5
     C                   MOVEL     SCDPP2        CXDPP2
     C                   MOVEL     SCDPP3        CXDPP3
     C                   MOVEL     SCDPP4        CXDPP4
     C                   MOVEL     SCDPP5        CXDPP5
     C                   MOVEL     SCPDC2        CXPDC2
     C                   MOVEL     SCPDC3        CXPDC3
     C                   MOVEL     SCPDC4        CXPDC4
     C                   MOVEL     SCPDC5        CXPDC5
     C                   MOVEL     SCVSZ2        CXVSZ2
     C                   MOVEL     SCVSZ3        CXVSZ3
     C                   MOVEL     SCVSZ4        CXVSZ4
     C                   MOVEL     SCVSZ5        CXVSZ5
     C                   MOVEL     SCFSV2        CXFSV2
     C                   MOVEL     SCFSV3        CXFSV3
     C                   MOVEL     SCFSV4        CXFSV4
     C                   MOVEL     SCFSV5        CXFSV5
     C                   MOVEL     SCAUT1        CXAUT1
     C                   MOVEL     SCAUT2        CXAUT2
     C                   MOVEL     SCAUT3        CXAUT3
     C                   MOVEL     SCAUT4        CXAUT4
     C                   MOVEL     SCAUT5        CXAUT5
     C                   MOVEL     SCAUSL        CXAUSL
     C                   MOVEL     SCDS01        CXDS01
     C                   MOVEL     SCDS02        CXDS02
     C                   MOVEL     SCDS03        CXDS03
     C                   MOVEL     SCDS04        CXDS04
     C                   MOVEL     SCDS05        CXDS05
     C                   MOVEL     SCDSLV        CXDSLV
     C                   MOVEL     SCDSRC        CXDSRC
     C                   MOVEL     SCDPT1        CXDPT1
     C                   MOVEL     SCDPT2        CXDPT2
     C                   MOVEL     SCDPT3        CXDPT3
     C                   MOVEL     SCDPT4        CXDPT4
     C                   MOVEL     SCDPT5        CXDPT5
     C                   MOVEL     SCDPLV        CXDPLV
     C                   MOVEL     SCDPPC        CXDPPC
     C                   Z-ADD     SCEDIM        CXEDIM
      *
     C                   MOVEL     SCDPT6        CXDPT6
     C                   MOVEL     SCDPT7        CXDPT7
     C                   MOVEL     SCDPT8        CXDPT8
     C                   MOVEL     SCDPT9        CXDPT9
     C                   MOVEL     SCDPT0        CXDPT0
     C                   MOVEL     SCDPL2        CXDPL2
     C                   Z-ADD     SCMBS         CXMBS
     C                   MOVEL     SCMBS1        CXMBS1
     C                   MOVEL     SCMBS2        CXMBS2
     C                   MOVEL     SCMBS3        CXMBS3
     C                   MOVEL     SCMBS4        CXMBS4
     C                   MOVEL     SCMBS5        CXMBS5
     C                   Z-ADD     SCRBS         CXRBS
     C                   MOVEL     SCRBS1        CXRBS1
     C                   MOVEL     SCRBS2        CXRBS2
     C                   MOVEL     SCRBS3        CXRBS3
     C                   MOVEL     SCRBS4        CXRBS4
     C                   MOVEL     SCRBS5        CXRBS5
     C***** Beg Add *** 02/14/17 ***************************************
      /free

        CXPBMEL = scPBMLvl;
        CXPBME1 = scPBMTyp1;
        CXPBME2 = scPBMTyp2;
        CXPBME3 = scPBMTyp3;
        CXPBME4 = scPBMTyp4;
        CXPBME5 = scPBMTyp5;

      /end-free
     C***** End Add *** 02/14/17 ***************************************
      *
      * SCREEN 2
     C                   MOVEL     SCLOCT        CXLOCT
     C                   MOVEL     SCPMTS        CXPMTS
     C                   MOVEL     SCPSC2        CXPSC2
     C                   MOVEL     SCACCP        CXACCP
     C                   MOVEL     SCRT1         CXRT01
     C                   MOVEL     SCRT2         CXRT02
     C                   MOVEL     SCRT3         CXRT03
     C                   MOVEL     SCRT4         CXRT04
     C                   MOVEL     SCMPCD        CXMJR
     C                   MOVEL     SCCAT2        CXCAT2
     C                   MOVEL     SCADJ         CXADJ
     C                   MOVEL     SCISC         CXCSDF
     C                   MOVEL     SCRCHG        CXRCHG
     C                   MOVE      SCRLBL        CXRLBL
      ***** Beg Add ****** 11/25/13 ************************************
      /free
       If isFCC = *On;
          CXlgrh = scLGRH;
       EndIf;
      /end-free
      ***** End Add ****** 11/25/13 ************************************
      * SCREEN 7
     C                   MOVE      SCOPCL        CXOPCL
     C                   MOVE      SCDOHR        CXDOHR
     C                   MOVE      SCDOBR        CXDOBR
     C                   MOVE      SCGDRR        CXGDRR
     C                   MOVE      SCEXNM        CXEXNM
     C                   MOVE      SCLSRV        CXLSRV
     C                   MOVE      SCWMTH        CXWMTH
      ***** Beg Add ****** 08/29/13 ************************************
      /free
       CXawwr = scAwwr;
       //***** Beg Add *** 02/14/17 *********************************** *
       CXPBME = scPBMElgb;
       //***** End Add *** 02/14/17 ************************************
       //***** Beg Add *** 09/01/17 ************************************
       CXcvt# = SCcvt#;

       ##rtcd = *blanks;
       ##co#  = %editc(xxco#:'X');
       ##wcf# = %editc(xxfnd:'X');
       ##sub# = *zeros;
       ##fnd  = *zeros;
       //*** Beg Add *** 09/09/21 **************************************
       If isWcMc = '1';
       //*** End Add *** 09/09/21 **************************************
          FA700R(##rtcd:##co#:##wcf#:##sub#:##fnd);

       //***** Beg Add *** 03/27/18 ************************************
       If ##rtcd <> 'F';
       //***** End Add *** 03/27/18 ************************************

          If not %Open(FMCXL);
             Open FMCXL;
          Endif;
          xxSub# = %int(##Sub#);
          Chain (xxCo#:xxSub#:xxFnd) FMCXL;

          If %found;
             F_CXcvt# = SCcvt#;
             Update FX01;
          Endif;
       //***** End Add *** 09/01/17 ************************************

       //***** Beg Add *** 03/27/18 ************************************
       Endif;
       //***** End Add *** 03/27/18 ************************************
      /end-free
      ***** End Add ****** 08/29/13 ************************************
       //*** Beg Add *** 09/09/21 **************************************
       Endif;
       //*** End Add *** 09/09/21 **************************************
      *
      * SCREEN 3
     C                   MOVEL     SCRFFA        CXRFFA
     C                   MOVEL     SCRTFA        CXRTFA
     C                   MOVEL     SCRCNC        CXRCNC
     C                   MOVEL     SCROPN        CXROPN
     C                   MOVEL     SCDSRC        CXDSRC
     C                   Z-ADD     SCRODY        CXRODY
     C                   MOVEL     SCRADS        CXRADS
     C                   Z-ADD     SCBADJ        CXBADJ
     C                   MOVEL     SCRSSD        CXRSSD
     C                   Z-ADD     SCBSPV        CXBSPV
     C                   MOVEL     SCRUMD        CXRUMD
     C                   Z-ADD     SCBMGR        CXBMGR
     C                   MOVEL     SCRCAD        CXRCAD
     C                   MOVEL     SCRSRD        CXRSRD
     C                   MOVE      SCCDSM        CXCDSM
     C                   MOVE      SCDEVF        CXDEVF
     C                   MOVE      SCFL01        CXFL01
     C                   MOVEL     SCNMAD        CXNMAD
     C                   MOVEL     SCAADP        CXAADP
     C*** Begin Add *** 07/16/18 *********************************
     C                   MOVEL     SCAADR        CXAADR
     C*** End   Add *** 07/16/18 *********************************
     C                   MOVE      SCWCR         CXWCR
      ***** Beg Add ****** 11/25/13 ************************************
      /free
       If isFCC = *On;
          CXaprm = scAPRM;
       EndIf;
      /end-free
      ***** End Add ****** 11/25/13 ************************************
      ***** Beg Add ****** 07/28/14 ************************************
      /free
       CXcapd = scClmAct;
      /end-free
      ***** End Add ****** 07/28/14 ************************************
      * SCREEN 4
     C                   MOVEL     SCMEDI        CXMEDI
     C                   MOVEL     SCBENI        CXBENI
     C                   MOVEL     SCRPS         CXRPS
     C                   MOVEL     SCSTSP        CXSTSP
     C                   MOVEL     SCDBEN        CXDBEN
     C                   MOVEL     SCDART        CXDART
     C                   MOVEL     SCMOC         CXMOC
     C                   MOVEL     SCACCM        CXACCM
     C                   Z-ADD     SCNFAM        CXNFAM
     C                   MOVEL     SCADRM        CXADRM
     C                   MOVEL     SCINC         CXINC
     C                   MOVEL     SCACCI        CXACCI
     C                   Z-ADD     SCNFAI        CXNFAI
     C                   MOVEL     SCADRI        CXADRI
      /free
     C*** Beg Add ****** 08/29/22 *********************************************
       CXprtFroi = scPFro;
     C*** End Add ****** 08/29/22 *********************************************
       //********** beg add ********* 09/04/14 *******************************************
       CXclmInst = scClmInst;
       //********** end add ********* 09/04/14 *******************************************
      /end-free
      * SCREEN 5
     C                   MOVEL     SCQRSD        CXQRSD
     C                   MOVEL     SCQRDU        CXQRDU
     C                   MOVEL     SCQRSU        CXQRSU
     C                   MOVEL     SCCERD        CXCERD
     C                   MOVEL     SCCEDU        CXCEDU
     C                   MOVEL     SCCESU        CXCESU
     C                   MOVEL     SCMBQF        CXMBQF
     C*** BEG ADD ***** 03/16/12 *************************************
      /free
       CXrcsc = scRcsc;
       CXrcdu = scRcdu;
       CXrcsu = scRcsu;
      /end-free
     C*** END ADD ***** 03/16/12 *************************************
      * SCREEN 6
     C                   MOVEL     SCAPV1        CXAPV1
     C                   MOVEL     SCAPV2        CXAPV2
     C                   MOVEL     SCAPV3        CXAPV3
     C                   MOVEL     SCAPV4        CXAPV4
     C                   MOVEL     SCAPV5        CXAPV5
      *
      **** BEG ADD **** 11/12/20 ***************************************
      * SCREEN 8
       CXlossCd  = scLossCd;
       CXlsReCd  = scLsReCd;
       CXcoVt    = scCoVt;
       CXlsCv    = scLsCv;
       CXloadAww = scLoadAww;
      **** END ADD **** 11/12/20 ***************************************
      * MAINT FIELDS
     C                   MOVE      *BLANK        CXSTAT
     C                   MOVEL     Q1USER        CXUSER
     C                   MOVEL     Q1PGM         CXPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXDT
     C*
     C                   Z-ADD     DSTIME        CXTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCX01
     C                   ELSE
     C                   MOVEL     Q1USER        CXIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXIDT
     C*
     C                   WRITE     PCX01
     C                   END
      *** Begin Add *** 11/12/20 *****************************************
       If scLossCd <> *blank or scLsReCd <> *Blank or scCovt <> *blank or
          scLscv <> *blank or scLoadAww = 'Y';
          If %found(WMCXP2);
             Update PCX02;
          Else;
             CXIusr = Q1User;
             CXidt  =  %dec(%date():*cymd);
             Write PCX02;
          Endif;
       Endif;
      *** End   Add *** 11/12/20 *****************************************
     C*
     C*
      *** Begin Change *** 11/12/20 *************************************
     C***     INQ6          TAG
     C     INQ8          TAG
      *** End   Change *** 11/12/20 *************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
      /Free
       //***** Beg Add *** 09/01/17 ************************************
        If %Open(FMCXL);
           Close FMCXL;
        Endif;
       //***** End Add *** 09/01/17 ************************************
      /End-Free
     C                   MOVE      '1'           *INLR
      *** BEG ADD *** 03/16/12 *******************************************
     C**************************************************************************
     C*  Subroutine to execute GOTO STRT
     C**************************************************************************
     C     GotoStrt      BegSr
     C                   Goto      STRT
     C                   EndSr
      *** END ADD *** 03/16/12 *******************************************
      * **  ARRAY DAYS
      * N
      * N
      * Y
      * Y
      * Y
      * Y
      * Y
      * N
      * N
**  ARRAY UA
-- Add --
- Change -
-Display-
**  ARRAY BLNK - MUST REMARK CODE BE ENTERED (Y/N) ?
N
N
Y
N
N
N
N
N
N
N
N
N
**  ARRAY DIRY - MUST REMARK CODE BE A DIARY TYPE (Y/N) ?
N
N
N
N
Y
Y
Y
Y
Y
Y
N
N
