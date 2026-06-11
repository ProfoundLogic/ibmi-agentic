     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H Dftactgrp(*no) ActGrp(*Caller)
     H/define CONVERT
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W0171R - Issue Cancellation Confirmations
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 09/21/98  PROGRAMMER: C:87  PROJECT: WC762          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/28/20 C:79  Project: WC2671                                *
     H*          C:33  Audit request changes                          *
     H*                                                               *
     H* 06/19/20 C:65  Support: 89453 (Sunz)                          *
     H*                Set the non-renew flag to yes value for any    *
     H*                cx codes overriden to report as non-renewal.   *
      *                                                               *
      * 02/10/20 C:298 Project: WC2704                                *
      *                Cancellation Output Configuration - Print      *
      *                for All Divisions or State Report Only.        *
      *                                                               *
     H* 09/16/19 C:300 Support: 83101                                 *
     H*                Cancellation Scriptura Form Change Needed      *
     H*                Adding week ending date into WXC7P/C7WEDT      *
      *                                                               *
      * 05/31/19 C:65  Support: 79861 (FMIC) (Replicate 64432)        *
      *                Save Output from OutputWindow in SPILETTER     *
      *                uses position 190 of LDA.  If set, it          *
      *                causes Scriptura to fail when processing a mass*
      *                run. Adjusted ldMassFile in LDA to use         *
      *                different positions.                           *
     H*                                                               *
     H* 04/22/19 C:291 Project: 102WHW/PHW78876                       *
     H*                Add Region parameter to call W7231C program.   *
     H*                                                               *
     H* 06/28/18 C:65  PROJECT: Phw#71890                             *
     H*                Pass spool file output flag to W0172C.         *
     H*                                                               *
     H* 03/28/17 C:65  PROJECT: SY371 (added 06/28/18 Phw#71890)      *
     H*                Populate fein into scriptura workfile.         *
     H*                                                               *
     H* 03/17/17 C:65  PROJECT: SY371 (added 06/28/18 Phw#71890)      *
     H*                Populate formatted name/address fields for     *
     H*                Scriptura output.                              *
     H*                                                               *
     H* 02/28/17 C:65  PROJECT: SY371                                 *
     H*                Update category on master record for mass      *
     H*                run to allow proper work file data program     *
     H*                to be called.                                  *
     H*                                                               *
     H* 01/13/17 C:136 Project: SY371                                 *
     H*                Don't abend if Scriptura fails on PDSDEV       *
     H*                                                               *
     H* 10/06/16 C:65  Project: SY371                                 *
     H*                Request XMLPrep record for Scriptura if needed *
     H*                and update into new work file.  Populate mass  *
     H*                request XML file name into lda to allow        *
     H*                different programs to link documents together. *
     H*                                                               *
     H* 06/16/15  C:306 SUPPORT: 35357                                *
     H*                 Adding new parameters to call W7231C program  *
     H*                                                               *
     H* 09/13/13  C:290 SUPPORT: WA114 83161                          *
     H*                 POPULATED AGENT/PRODUCER PHONE NUMBER SO      *
     H*                 THAT IT APPEARS ON CANCELLATION AND           *
     H*                 REINSTATEMENT NOTICES.                        *
     H*                                                               *
     H* 01/23/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     H* 04/06/11 C:235 PROJECT: 114WYM                                *
     H*                OCR for Cancellation/ Audit Bills              *
     H*                                                               *
     H* 11/02/10 C:220 SUPPORT: 74745                                 *
     H*                Add PARAM in KRMS Call (W7231C)                *
     H*                                                               *
     H* 10/14/10 C:136 SUPPORT: WA025 74528                           *
     H*                REMOVE CLIENT SPECIFIC CALL TO W7231C. TASB    *
     H*                WENT BACK TO BASE FOR THIS PROCESS.            *
     H*                                                               *
     H* 05/03/10 C:220 PROJECT: WC1881                                *
     H*                Add Ctrl flag to Final Audit Request Form      *
     H*                                                               *
     H* 11/25/09 C:232 Support: WA136/70791                           *
     H*                Issue related to second line of DBA name in the*
     H*                Cancellation Confirmations.                    *
     H*                                                               *
     H* 08/28/09 C:61  SUPPORT WA133 SEQ69698                        *
     H*                RECOMPILED FOR V9R1.                           *
     H*                                                               *
     H* 07/28/09  C:189  PROJECT: 93WIF                               *
     H*                  CHANGE SSN DISPLAY ON AUDIT REQUEST          *
     H*                                                               *
     H* 02/12/09 C:61  SUPPORT WA093 SEQ67197                         *
     H*                Call W7231C with correct parms for clt093      *
     H*                                                               *
     H* 03/03/08 C:65  Seq#: WA045/61808                              *
     H*                DO NOT CALL AUDIT REQUEST PROGRAM TO PRINT IF  *
     H*                A CX AUDIT REQUEST RECORD ALREADY EXISTS.      *
     H*                                                               *
     H*  01/25/08  C:180 PRISS 11 - Fix so DARTS can retrieve         *
     H*                             Canc confirmation PDF document.   *
     H*                                                               *
     H*  11/06/07  C:183 PROJECT: 71WIL                               *
     H*                  USING NEW PARAMETER, DETERMINED IF THE DOC   *
     H*                  NEEDS TO BE RESOLVED TO DARTS. IF SO RETREIVE*
     H*                  THE FOLDER AND DOCUMENT ID.                  *
     H*                                                               *
     H*  10/03/07  C:193 PROJECT: 71WIQ                               *
     H*                  INCL ECM PREPRTED FORM/BASE AUDIT FORM       *
     H*                                                               *
     H* 09/20/07 C:33  Project: 109WDZ  Seq#: WA045/60034             *
     H*                If process in being called from Auto Confirm-  *
     H*                 ations, use the CX code to determine if the   *
     H*                 Audit Tracking record should be written.      *
     H*                                                               *
     H* 07/16/07 C:183 Project 45WFO                                  *
     H*                Change PARM List for W7231C call.              *
     H*                                                               *
     H* 05/22/07 C:33  Seq#: WA114/51873                              *
     H*                Use different parm list for call to W7231C for *
     H*                 CLT048 & CLT091.                              *
     H*                                                               *
     H* 04/17/07 C:175 Seq#: WA114/51873                              *
     H*                Use different parm list for call to W7231C.    *
     H*                {Only for CLT025} Corrected parm list for      *
     H*                93 to follow standards.                        *
     H*                                                               *
     H* 01/03/07 C:33  Seq#: WA099/44610                              *
     H*                Release record lock                            *
     H*                                                               *
     H* 11/15/06 C:61  PROJECT 139WCI                                 *
     H*                If PEO group, print PEO name / address info.   *
     H*                                                               *
     H* 11/07/06 C:61  SUPPORT WA129 53118                            *
     H*                Write Quote Status 'C'ancellation type record  *
     H*                with current agent number.                     *
     H*                                                               *
     H* 11/03/06 C:136 SEQ#: WA071 55489                              *
     H*                CLIENT SPECIFIC - POPULATE GROUP SELF INSURED #*
     H*                                                               *
     H* 10/05/06 C:53  SUPPORT WA093 54989                            *
     H*          C:61 -Same change as 53091 for CLT093                *
     H*               -CLT093 calls W7231C with modified parm list.
     H*                                                               *
     H* 06/08/06 C:33  Seq#: WA045/53091                              *
     H*                Use different parm list for call to W7231C.    *
     H*                {Only for CLT045}                              *
     H*                                                               *
     H* 06/01/06 C:53  SUPPORT WA093 54989                            *
     H*                Same change as 53091 for CLT093                *
     H*                                                               *
     H* 01/09/06 C:168 PROJECT 129WNI                                 *
     H*              Added code to fill the following fields for      *
     H*              document printing                                *
     H*              - Agent mailing address                          *
     H*              - Agent mailing city/state/zip                   *
     H*              - Employer contact #                             *
     H*                                                               *
     H* 03/23/05 C:53  PROJECT: WC1196                                *
     H*                ADDITION TO PARM LIST.                         *
     H*                                                               *
     H* 11/10/04 C:09  SUPPORT WA033 44548                            *
     H*                Modify MODE parameter on call to W7231C to     *
     H*                ensure job is reflected to run on demand       *
     H*                and route output to user parm output queue     *
     H*                                                               *
     H* 05/14/04 C:65 PROJECT: WC1202                                 *
     H*               STORE EXTENDED EMPLOYER NAMES FOR REPORTING.    *
     H*                                                               *
     H* 01/17/04 C:61  PROJECT WC577                                  *
     H*                Clear WX@#P before use by query W7165Q.        *
     H*                                                               *
     H* 12/11/02 C:61  SUPPORT WA081 35144                            *
     H*                Get most recent ACTIVE CX reason code record.  *
     H*                                                               *
     H* 07/03/02 C:61  PROJECT WC1081                                 *
     H*                Print the Federal Name on CX notice.           *
     H*                                                               *
     H* 04/12/02 C:61  SUPPORT WA107 31830                            *
     H*                Update maint date fields correctly.            *
     H*                                                               *
     H* 06/21/01 C:135 PROJECT: WC883                                 *
     H*                ADD PARM CONSTANT ' ' TO THE CALL TO W7231C    *
     H*                TO PRINT AUDIT REQUEST WITH ESTIMATED DATA     *
     H*                                                               *
     H* 01/31/01  C:35   PROJECT: SUPPORT DR112 #26106                *
     H*                  INITIALIZE DOC ID IN DARTS TO BLANKS.        *
     H*                                                               *
     H* 01/26/01 C:61  PROJECT WC577                                  *
     H*                Save Rating state to work file.                *
     H*                                                               *
     H* 08/25/00  C:35   PROJECT: SUPPORT WA071 #24061                *
     H*                  PGM WAS PICKING UP FIRST QUOTE TRACKING      *
     H*                  RECORD INSTEAD OF MOST RECENT.               *
     H*                                                               *
     H*  06/05/00 C:61  SUPPORT DR109 23102
     H*                 DARTS created should have CMPNAM = W0050R.
     H*
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H*  04/17/00  C:87  PROJECT: SUPP WA091 22402                    *
     H*                  When processing non-state rptg divs in sub-  *
     H*                  routing CHKDIV, need to pass assign CXDIV    *
     H*                  to QQDIV before call audit request pgm.      *
     H*                                                               *
     H* 12/06/99 C:118 SUPPORT: WA048/20840                           *
     H*                Modify the test of the carrier policy number   *
     H*                to see if it is an old-format PDS policy       *
     H*                number.                                        *
     H*                                                               *
     H*  10/19/99  C:87  PROJECT: SUPP WC095 #20198                   *
     H*                  Decimal data error if DSDATE is not filled.  *
     H*                                                               *
     H*  09/28/99  C:87  PROJECT: SUPP DR081 #19842                   *
     H*                  Condition delete on record found.            *
     H*                                                               *
     H* 09/16/99 C:65  PROJECT: WC851                                 *
     H*                PULL THE SHORT-RATE/PRORATE FLAG FROM WDELP.   *
     H*                                                               *
     H*  08/19/99  C:87  PROJECT: SUPP WA091 #19271                   *
     H*                  Pgm was not processing confirmations if cx   *
     H*                  notice flag was off (CFNOTC<>'Y').  Added    *
     H*                  code to update quote trkg for non state rptg *
     H*                  divs.  Added renamed fields for PCM02.       *
     H*                  Add code to delete record from WX#@P after   *
     H*                  printing canx notice (to prevent multiples   *
     H*                  notices being printed).
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H*  06/13/99  C:87  PROJECT: SUPP WA081 #18416                   *
     H*                  Added code to chain to canx reason code file *
     H*                  using state and eff dt. This code replaces   *
     H*                  the table file code which was deleted.       *
     H*                  Assign today's date to confirmation post dt. *
     H*                  Modified code to put dates in mm/dd/ccyy for *
     H*                  cx confirmation DARTS comment.               *
     H*                                                               *
     H*****************************************************************
     H*
     FSMCOL     IF   E           K DISK
     FWDCXL1    IF   E           K DISK
     F                                     INFDS(CXFDS1)
     FWDCXL6    IF   E           K DISK
     F                                     RENAME(PCX01:PCX06)
     F                                     INFDS(CXFDS6)
     FWDCXP     UF   E             DISK
     F                                     RENAME(PCX01:PCX0P)
     F*
     FWMEML     IF   E           K DISK
     F*
     FWDELL     IF   E           K DISK
     F                                     infds(dbfds)
     FWXC7P     O    E             DISK    USROPN
     FXMLPREL1  UF   E           K DISK
     F*
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWMFNL     IF   E           K DISK
     F*
     FWDF2L     IF   E           K DISK
     F*
     FWMCFL     IF   E           K DISK
     F*
     FSTCTL1    IF   E           K DISK
     F*
     FWX#@P     UF A E             DISK    USROPN
     F*
     FWTGTL     IF   E           K DISK
     F*
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     F*
     F*
     FWMRPL     IF   E           K DISK
     F*
     FWAQTL7    IF A E           K DISK
     FWMRSL     IF   E           K DISK
     FWTCRL     IF   E           K DISK
      *
     FWMECL1    IF   E           K DISK
     FWMAUL     IF   E           K DISK
     FWDRRL     IF   E           K DISK    RENAME(PRR01:PRR00)

     D*
     D isMassReq       s              1n   inz
     d massFlag        s              1
     d xmlLink         s                   like(XPLink)
     d xmlFileName     s                   like(XPXML)
     d xmlFileBlank    s                   like(XPXML)
     d xmlFile         s                   like(XPXML)
     d outputFile      s                   like(XPFILE)
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     D isUseScript     S              1n   inz
     d strLang         s             60
     d strLangCode     s            250
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     D xxPre           s              2    inz('CC')
     d processType     c                   const('Cancellations')
     d QQRegion        s              3

     d StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     D*
     D dbfds           ds
     D  dbrrn                397    400b 0
     D*
     d x#RRN           s              9  0 inz(0)
     d*
     D/copy sPrXML
     d/copy sPrLetter
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     d
     D*
     D svlda           ds          1024    dtaara(*lda)
     D ldMassFile                    60    Overlay(svlda:129)
     D ldOutputFile                  60    Overlay(svlda:191)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
     D S4401C          pr                  ExtPgm('S4401C')
     D   xmlName                     61    const
     D ObjExists       pr             1n
     D  object                       10    const
     D  objType                       7    const
     D                 ds
     D  amn                    1    540    dim(9)
     D  ##amn1               001    060
     D  ##amn2               061    120
     D  ##amn3               121    180
     D  ##amn4               181    240
     D  ##amn5               241    300
     D  ##amn6               301    360
     D  ##amn7               361    420
     D  ##amn8               421    480
     D  ##amn9               481    540
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D*
     D                 DS
     D  XXPOL                  1     18
     D  XXOPOL                 1     11
     D  XXOFND                 1      3
     D  XXOEMP                 4      8
     D  XXOFYR                 9     11
     D  XOBLNK                12     18
     D*
     D*  Constants
     D YYCFDT          C                   CONST('Confirmation date: ')
     D YYCFPD          C                   CONST('Confirmation issue d-
     D                                     ate: ')
     D YYCXDT          C                   CONST('Cancellation effecti-
     D                                     ve date: ')
     D YYCDAT          C                   CONST('Cancellation issue d-
     D                                     ate: ')
     D CHGNUM          C                   CONST('SA020R')
     D*
     D CXFDS1          DS
     D  CXRRN1               397    400B 0
     D CXFDS6          DS
     D  CXRRN6               397    400B 0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     IPCR01         01
     I              CRCO#                       $CRCO#
     I              CRKEY                       $CRKEY
     I              CRDESC                      $CRDSC
     I              CRSTAT                      $CRSTA
     I              CRUSER                      $CRUSR                           =
     I              CRPGM                       $CRPGM
     I              CRTIME                      $CRTIM
     I              CRDT                        $CRDT
     I              CRNCCI                      $CRNCC
     I              CRATRK                      $CRATR
     I              CRPCHL                      $CRPCL
     I              CRAREI                      $CRAREI
     I              CRRDAY                      $CRRDAY
     I              CRPCNT                      $CRPCNT
     I              CRACNF                      $CRACNF
     I              CRCDAY                      $CRCDAY
     I              CRICE                       $CRICE
     I              CRUPSP                      $CRUPSP
     I              CRUPSC                      $CRUPSC
     I              CRNDNW                      $CRNDNW
     I              CRNTNP                      $CRNTNP
     I              CRMDNP                      $CRMDNP
     I              CRIDOV                      $CRIDOV

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@ADTK            1
     C                   PARM                    @@AURQ            5
     C                   PARM                    @@PCHL            1
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
     C                   PARM                    @@AFTY            1
     C                   PARM                    @@DDUE            8
     C                   PARM                    @@SSF             1
     C                   PARM                    @@SRFL            1
     C*** Beg Add *** 10/28/20 **************************************
     C                   PARM                    @@G301E           1
     C                   PARM                    @@G311E           1
     C                   PARM                    @@GASRD           1
     C*** End Add *** 10/28/20 **************************************
     C*
     C     AURQPL        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM      *BLANK        QQFRDT            8
     C                   PARM      *BLANK        QQTODT            8
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM      @@ADTK        QQRPFL            1
     C                   PARM      @@AURQ        QQDRMK            5
     C                   PARM      @@QUE         QQQUE            10
     C                   PARM      'D'           QQMODE           10
     C                   PARM      *BLANK        QQCOM1           30
     C                   PARM      *BLANK        QQCOM2           30
     C                   PARM      *BLANK        QQCOM3           30
     C                   PARM      *BLANK        QQCOM4           30
     C                   PARM      *BLANK        QQCOM5           30
     C                   PARM      *BLANK        QQCDE1            2
     C                   PARM      *BLANK        QQCDE2            2
     C                   PARM      *BLANK        QQCDE3            2
     C                   PARM      *BLANK        QQCDE4            2
     C                   PARM      *BLANK        QQCDE5            2
     C                   PARM      ' '           QQSRFL            1
     C                   PARM      'B'           QQNBRB            1
     C                   PARM      *BLANKS       QQAUTY            2
     C                   PARM      *BLANK        QQATIE            1
     C                   PARM      *BLANKS       QQAR             30
     C                   PARM      *BLANK        QQACTY            1
     C                   PARM      *BLANK        QQACST            1
     C                   PARM      *BLANK        QQG15C            1
     C                   PARM      @@AFTY        QQAFTY            1
     C                   PARM      @@DDUE        QQDDUE            8
     C                   PARM      *ZEROS        QQPRDT            8
     C                   PARM      @@SSF         QQSSF             1
     C                   PARM      @@SRFL        QQSRFL            1
     C                   PARM      'N'           AudRProc          1
     C                   PARM      ' '           UpdAudTR          1
     C                   PARM      ' '           QQRegion
     C*** Beg Add *** 10/28/20 **************************************
     C                   PARM                    ##allDiv          1
     C                   PARM      ' '           ##NOOUT           1
     C                   PARM                    ##G301E           1
     C                   PARM                    ##G311E           1
     C                   PARM      ' '           ##LCLMS           1
     C                   PARM      ' '           ##NMBR           10
     C*** End Add *** 10/28/20 **************************************
     C*
     C     AURQ45        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM      *BLANK        QQFRDT            8
     C                   PARM      *BLANK        QQTODT            8
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM      @@ADTK        QQRPFL            1
     C                   PARM      @@AURQ        QQDRMK            5
     C                   PARM      @@QUE         QQQUE            10
     C                   PARM      'D'           QQMODE           10
     C                   PARM      *BLANK        QQCOM1           30
     C                   PARM      *BLANK        QQCOM2           30
     C                   PARM      *BLANK        QQCOM3           30
     C                   PARM      *BLANK        QQCOM4           30
     C                   PARM      *BLANK        QQCOM5           30
     C                   PARM      *BLANK        QQCDE1            2
     C                   PARM      *BLANK        QQCDE2            2
     C                   PARM      *BLANK        QQCDE3            2
     C                   PARM      *BLANK        QQCDE4            2
     C                   PARM      *BLANK        QQCDE5            2
     C                   PARM      ' '           QQSRFL            1
     C                   PARM      *BLANKS       QQAUTY            2
     C                   PARM      *BLANK        QQATIE            1
     C                   PARM      *BLANKS       QQAR             30
     C                   PARM      *BLANK        QQACTY            1
     C                   PARM      *BLANK        QQACST            1
     C                   PARM      *BLANK        QQG15C            1
     C                   PARM      @@AFTY        QQAFTY            1
     C                   PARM      @@DDUE        QQDDUE            8
     C*
     C     AURQ48        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM      *BLANK        QQFRDT            8
     C                   PARM      *BLANK        QQTODT            8
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM      @@ADTK        QQRPFL            1
     C                   PARM      @@AURQ        QQDRMK            5
     C                   PARM      @@QUE         QQQUE            10
     C                   PARM      'D'           QQMODE           10
     C                   PARM      *BLANK        QQCOM1           30
     C                   PARM      *BLANK        QQCOM2           30
     C                   PARM      *BLANK        QQCOM3           30
     C                   PARM      *BLANK        QQCOM4           30
     C                   PARM      *BLANK        QQCOM5           30
     C                   PARM      *BLANK        QQCDE1            2
     C                   PARM      *BLANK        QQCDE2            2
     C                   PARM      *BLANK        QQCDE3            2
     C                   PARM      *BLANK        QQCDE4            2
     C                   PARM      *BLANK        QQCDE5            2
     C                   PARM      ' '           QQSRFL            1
     C                   PARM      *BLANKS       QQAUTY            2
     C                   PARM      *BLANK        QQATIE            1
     C                   PARM      *BLANKS       QQAR             30
     C                   PARM      *BLANK        QQACTY            1
     C                   PARM      *BLANK        QQACST            1
     C                   PARM      *BLANK        QQG15C            1
     C                   PARM      @@AFTY        QQAFTY            1
     C                   PARM      @@DDUE        QQDDUE            8
     C*
     C     AURQ25        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM      *BLANK        QQFRDT            8
     C                   PARM      *BLANK        QQTODT            8
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM      @@ADTK        QQRPFL            1
     C                   PARM      @@AURQ        QQDRMK            5
     C                   PARM      @@QUE         QQQUE            10
     C                   PARM      'D'           QQMODE           10
     C                   PARM      *BLANK        QQCOM1           30
     C                   PARM      *BLANK        QQCOM2           30
     C                   PARM      *BLANK        QQCOM3           30
     C                   PARM      *BLANK        QQCOM4           30
     C                   PARM      *BLANK        QQCOM5           30
     C                   PARM      *BLANK        QQCDE1            2
     C                   PARM      *BLANK        QQCDE2            2
     C                   PARM      *BLANK        QQCDE3            2
     C                   PARM      *BLANK        QQCDE4            2
     C                   PARM      *BLANK        QQCDE5            2
     C                   PARM      ' '           QQSRFL            1
     C                   PARM      *BLANKS       QQAUTY            2
     C                   PARM      *BLANK        QQATIE            1
     C                   PARM      *BLANKS       QQAR             30
     C                   PARM      *BLANK        QQACTY            1
     C                   PARM      *BLANK        QQACST            1
     C                   PARM      *BLANK        QQG15C            1
     C                   PARM      @@AFTY        QQAFTY            1
     C                   PARM      @@DDUE        QQDDUE            8
     C*
     C     AURQ91        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM      *BLANK        QQFRDT            8
     C                   PARM      *BLANK        QQTODT            8
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM      @@ADTK        QQRPFL            1
     C                   PARM      @@AURQ        QQDRMK            5
     C                   PARM      @@QUE         QQQUE            10
     C                   PARM      'D'           QQMODE           10
     C                   PARM      *BLANK        QQCOM1           30
     C                   PARM      *BLANK        QQCOM2           30
     C                   PARM      *BLANK        QQCOM3           30
     C                   PARM      *BLANK        QQCOM4           30
     C                   PARM      *BLANK        QQCOM5           30
     C                   PARM      *BLANK        QQCDE1            2
     C                   PARM      *BLANK        QQCDE2            2
     C                   PARM      *BLANK        QQCDE3            2
     C                   PARM      *BLANK        QQCDE4            2
     C                   PARM      *BLANK        QQCDE5            2
     C                   PARM      ' '           QQSRFL            1
     C                   PARM      *BLANKS       QQAUTY            2
     C                   PARM      *BLANK        QQATIE            1
     C                   PARM      *BLANKS       QQAR             30
     C                   PARM      *BLANK        QQACTY            1
     C                   PARM      *BLANK        QQACST            1
     C                   PARM      *BLANK        QQG15C            1
     C                   PARM      @@AFTY        QQAFTY            1
     C                   PARM      @@DDUE        QQDDUE            8
     C*
     C     AURQ93        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM      *BLANK        QQFRDT            8
     C                   PARM      *BLANK        QQTODT            8
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM      @@ADTK        QQRPFL            1
     C                   PARM      @@AURQ        QQDRMK            5
     C                   PARM      @@QUE         QQQUE            10
     C                   PARM      'D'           QQMODE           10
     C                   PARM      *BLANK        QQCOM1           30
     C                   PARM      *BLANK        QQCOM2           30
     C                   PARM      *BLANK        QQCOM3           30
     C                   PARM      *BLANK        QQCOM4           30
     C                   PARM      *BLANK        QQCOM5           30
     C                   PARM      *BLANK        QQCDE1            2
     C                   PARM      *BLANK        QQCDE2            2
     C                   PARM      *BLANK        QQCDE3            2
     C                   PARM      *BLANK        QQCDE4            2
     C                   PARM      *BLANK        QQCDE5            2
     C                   PARM      ' '           QQSRFL            1
     C                   PARM      'B'           QQNBRB            1
     C                   PARM      *BLANKS       QQAUTY            2
     C                   PARM      *BLANK        QQATIE            1
     C                   PARM      *BLANKS       QQAR             30
     C                   PARM      *BLANK        QQACTY            1
     C                   PARM      *BLANK        QQACST            1
     C                   PARM      *BLANK        QQG15C            1
     C                   PARM      @@AFTY        QQAFTY            1
     C                   PARM      @@DDUE        QQDDUE            8
     C                   PARM      *ZEROS        QQPRDT            8
     C                   PARM      @@SSF         QQSSF             1
     C                   PARM      @@SRFL        QQSRFL            1
     C                   PARM      'N'           AudRProc          1
     C                   PARM      ' '           UpdAudTR          1
     C*
     C     DOCPL         PLIST
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQREQ#            5
     C                   PARM      @@PCHL        QQPCHL            1
     C                   PARM      @@QUE         QQQUE            10
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQDCID           12
     C                   PARM                    QQSPOOL           1
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *  LINE FORMAT
      *
     C     LINEPL        PLIST
     C                   PARM                    ##LINE          256
     C                   PARM                    ##LEN             3 0
     C                   PARM                    ##REM           256
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*   Cancellation detail key lists
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCXDT
     C*
     C     CXKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     CXKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C*
     C*   Cancellation confirmation control key list
     C     CFKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  Key list for the rate set code file
     C     RSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKRTCD
     C*
     C*  Key list for cancellation reason file
     C     CRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKST
     C                   KFLD                    KKKEY
     C                   KFLD                    KKEFDT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKST
     C                   KFLD                    KKKEY
     C*
     C*  Company file
     C     COKY          KLIST
     C                   KFLD                    KKCO#
      *
      *   Employer master key list
      *
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
      *
     C     EMKY1         KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C*
     C*  Employer detail
     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C*
     C     ELKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C*
     C*  Fund master
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C***Darts remarks file
     C*
     C*  Generic table file key
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C*  Agent assignment file
     C     AAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    CXAGT#
     C*
     C*  Agent master file
     C     AGKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*
     C*  Carrier/state reporting file
     C     RPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C*  Quote tracking file
     C*
     C     QTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKSTAT
     C                   KFLD                    KKAGT#
     C*
     C     QTKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKSTAT
     C*
     C* Key list for Employer Contacts
     C*
     C     MEKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    ##TYP1            1
      *
     C     AUKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXFYR
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C*
     C     ELKY3         KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    CXEMP#
     C                   KFLD                    CXDIV
     C                   KFLD                    CXFYR
      *
     C     RRKY          KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT            7 0
      *
     C     RRKY2         KLIST
     C                   KFLD                    CXCO#
     C                   KFLD                    CXFND
     C                   KFLD                    XXST
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CXAGT#        KKAGT#
     C     *LIKE         DEFINE    CXCO#         KKCO#
     C     *LIKE         DEFINE    CXFND         KKFND
     C     *LIKE         DEFINE    CXCXDT        KKCXDT
     C     *LIKE         DEFINE    CXEMP#        KKEMP#
     C     *LIKE         DEFINE    CXDIV         KKDIV
     C     *LIKE         DEFINE    CXFYR         KKFYR
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    RSRTCD        KKRTCD
     C     *LIKE         DEFINE    CRST          KKST
     C     *LIKE         DEFINE    CREFDT        KKEFDT
     C     *LIKE         DEFINE    QTSTAT        KKSTAT
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C*
     C                   MOVEL     *OFF          AUTOCF            1
     C*
     C                   SELECT
     C     @@ADTK        WHENEQ    '*'
     C                   MOVEL     *BLANKS       @@ADTK
     C                   MOVEL     *ON           AUTOCF
     C                   ENDSL
      /free
       isUseSpoolOut = UseSpoolOut(processType);
       isUseScript = UseScriptura(processType);
       isUseXML = UseScriptura(processType)
        or Use3rdXml(processType);

       Clear xmlLink;
       Clear xmlFileName;

       isMassReq = *on;
       massFlag = 'Y';

      /end-free
      /Free
       If (isUseScript = *on) and (Autocf = *on);

          xxProcType = processType;
          xxMass = 'Y';

          In *Lock *dtaara;

          Clear svLDA;

          Chain(n) cfky pcf01;

          Select;
          When %found and CFDART = 'Y' and CFCFDR <> ' ';
             xxDart = 'Y';
             xxRMCD = CFCFDR;
          Other;
             xxDart = 'N';
             xxRMCD = ' ';
          Endsl;

          Out *dtaara;

          xxSubmit = OutputWindow(xxProcType:xxMass:'Y':'M');

       Endif;

      //**** Beg Add *** 10/28/20 ************************************************
       Clear ##G301E;
       Clear ##G311E;
       ##allDiv = 'Y';
       If %parms >= 12;
          ##G301E = @@G301E;
       Endif;
       If %parms >= 13;
          ##G311E = @@G311E;
       Endif;
       If %parms >= 14 and @@GASRD = 'Y';
          ##allDiv = ' ';
       Endif;
      //**** End Add *** 10/28/20 ************************************************
      /End-free
     C*
     C*================================================================
     C*  Main processing
     C*================================================================
     C                   CLEAR                   P#@01
     C*
     C     CFKY          CHAIN     PCF01                              60
     C*
     C                   Z-ADD     CFCFSD        KKCXDT
     C*
     C     CXKY          SETLL     PCX06
     C     CXKY1         READE(N)  PCX06                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     *LIKE         DEFINE    CXRRN6        CXRRN
     C                   Z-ADD     CXRRN6        CXRRN
     C*
     C*  Process record only if Confirmation date is greater than 0
     C     CXCFDT        IFGT      0
     C*
     C                   Z-ADD     CXEMP#        KKEMP#
     C                   Z-ADD     CXDIV         KKDIV
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   MOVE      *OFF          INFO              1
     C*
     C     EMSTRP        IFEQ      'Y'
     C     CFNOTC        IFEQ      'Y'
     C                   EXSR      SRNOTC
     C                   ENDIF                                                  CFNOTC='Y'
     C*
     C                   MOVE      *BLANK        XXAUCX            1
     C     AUKY          CHAIN     PAU01                              60
     C     *IN60         IFEQ      *OFF
     C     AUCXFL        ANDEQ     'Y'
     C                   MOVE      'Y'           XXAUCX
     C                   ENDIF
     C*
     C*    If the Generate Audit Request flag = 'Y' then call W7231C
     C     CFAURQ        IFEQ      'Y'
     C     XXAUCX        ANDNE     'Y'
     C*
     C     INFO          IFEQ      *OFF
     C                   EXSR      GTCXIN
     C                   ENDIF
     C*
     C                   SELECT
     C     AUTOCF        WHENEQ    *ON
     C                   MOVEL     $CRATR        @@ADTK
     C                   ENDSL
     C*
     C                   MOVE      CXCO#         QQCO#
     C                   MOVE      CXFND         QQFND
     C                   MOVE      CXFYR         QQFYR
     C                   MOVE      CXEMP#        QQEMP#
     C                   MOVE      CXDIV         QQDIV
     C                   SELECT
     C     CLTID         WHENEQ    '048'
     C                   CALL      'W7231C'      AURQ48
     C     CLTID         WHENEQ    '091'
     C                   CALL      'W7231C'      AURQ91
     C     CLTID         WHENEQ    '093'
     C                   CALL      'W7231C'      AURQ93
     C                   OTHER
     C                   CALL      'W7231C'      AURQPL
     C                   ENDSL
     C                   ENDIF                                                  CFAURQ='Y'
     C*
     C*    If the Update Quote Tracking flag = 'Y' then write a 'C' type
     C*    record to the Quote Tracking file
     C     CFQTRK        IFEQ      'Y'
      *
     C                   CALL      'WA002R'                             99
     C                   PARM                    CXCO#
     C                   PARM                    CXFND
     C                   PARM                    CXEMP#
     C                   PARM                    CXDIV
     C                   PARM      *ZERO         ##FYR             3 0
     C     KKAGT#        PARM      *ZERO         ##AGT#            5 0
     C                   PARM      *BLANK        ##ANAM           60
      *
     C*
     C                   Z-ADD     CXEMP#        KKEMP#
     C                   Z-ADD     CXDIV         KKDIV
      *
     C                   MOVE      'W'           KKSTAT
     C     QTKY          SETGT     PQT01
     C     QTKY          READPE(N) PQT01                                  60
     C   60QTKY1         CHAIN(N)  PQT01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     KKAGT#        QTAGT#
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C                   Z-ADD     XXDATE        QTWKDT
     C                   MOVE      'C'           QTTYPE
     C*
     C                   MOVE      Q1USER        QTUSER
     C                   MOVE      DSTIME        QTTIME
     C                   MOVE      Q1PGM         QTPGM
     C                   Z-ADD     XXDATE        QTDT
     C*
     C                   MOVE      QTUSER        QTIUSR
     C                   MOVE      QTTIME        QTITM
     C                   MOVE      QTPGM         QTIPGM
     C                   MOVE      QTDT          QTIDT
     C                   Z-ADD     XXDATE        QTIDT
     C*
     C                   WRITE     PQT01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  CFQTRK='Y'
     C*
     C     CXRRN         CHAIN     WDCXP                              60
     C*
     C*    Update Confirmation flag and Conf processed date
     C                   MOVE      'Y'           CXCFFL
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXCFPD
      *
     C                   MOVEL     Q1USER        CXUSER
     C                   MOVEL     Q1PGM         CXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXDT
     C                   MOVEL     DSTIME        CXTIME
      *
     C                   UPDATE    PCX0P
     C*
     C**** Call subroutine to check other divisions
     C*
     C*    Call subroutine to write DARTS record if remark is not blank
     C     CFCFDR        IFNE      *BLANK
     C     isUseSpoolOut ANDEQ     *on
     C                   EXSR      DARTS
     C                   ENDIF
     C*
     C*    Call subroutine to check other divisions
     C                   EXSR      CHKDIV
     C*
     C                   ENDIF                                                  EMSTRP='Y'
     C                   ENDIF                                                  CXCFDT>0
     C*
     C     CXKY1         READE(N)  PCX06                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C* Call process to generate Scriptura output/Darts if requested.
      /Free
       Select;
       When xmlFileName = *blank;
       When objExists('PDSDEV':'*LIB');
          Monitor;
          S4401C(xmlFileName + '$');
          On-error;
          Endmon;
       Other;
          S4401C(xmlFileName + '$');
       Endsl;
      /End-Free
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*================================================================
     C*  GTCXIN - Get the employers cancellation information
     C*================================================================
     C     GTCXIN        BEGSR
     C*
     C*  Get employer detail record
     C                   Z-ADD     CXEMP#        KKEMP#
     C                   Z-ADD     CXDIV         KKDIV
     C                   Z-ADD     CXFYR         KKFYR
     C     ELKY          CHAIN     PEL01                              60
     C                   eval      x#RRN = dbRRN
     C*
     C*      Get jurisdiction state
     C                   MOVE      ELRTCD        KKRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*      Chain to canx reason code file
     C                   MOVEL(P)  CXRSN         KKKEY
     C                   MOVE      RSJRST        KKST
     C                   MOVE      RSJRST        CRJRST
     C                   Z-ADD     CXCDAT        KKEFDT
     C*
     C                   CLEAR                   $CRATR
     C                   CLEAR                   $CRPCL
     C*
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     $CRSTA        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           INFO
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C*  SRNOTC - Writes to WX#@P and calls W0172C to print the Canx
     C*           Notice and Cert. Holders letter documents
     C*================================================================
     C     SRNOTC        BEGSR
     C*
     C     INFO          IFEQ      *OFF
     C                   EXSR      GTCXIN
     C                   ENDIF
     C*
     C*  Determine type of cancellation
     C                   SETOFF                                       717273
     C*
     C     ELPFDT        IFEQ      CXCXDT
     C                   SETON                                        71
     C                   ENDIF
     C*
     C     ELPFDT        IFNE      CXCXDT
     C     ELSRPR        ANDEQ     'P'
     C                   SETON                                        72
     C                   ENDIF
     C*
     C     ELPFDT        IFNE      CXCXDT
     C     ELSRPR        ANDEQ     'S'
     C                   SETON                                        73
     C                   ENDIF
     C*
     C*  Load fields for WX#@P
     C                   MOVE      CXCO#         CRCO#
     C                   MOVE      CXFND         CRFND
     C                   MOVE      CXEMP#        CREMP#
     C                   MOVE      CXFYR         CRFYR
     C                   TIME                    CRUNID
     C                   MOVEL     'C'           CRCORR
     C*
     C                   MOVE      *BLANKS       CRFLAT
     C                   MOVE      *BLANKS       CRPROR
     C                   MOVE      *BLANKS       CRSHRT
     C                   MOVE      *BLANK        CRNRNW
     C     $CRNCC        IFEQ      '00'
     C     $CRIDOV       OREQ      '3'
     C                   MOVE      'Y'           CRNRNW
     C                   ELSE
     C   71              MOVE      'Y'           CRFLAT
     C   72              MOVE      'Y'           CRPROR
     C   73              MOVE      'Y'           CRSHRT
     C                   ENDIF
     C*
     C*  Load employer address fields
     C                   MOVE      *BLANKS       XXEAD1           40
     C                   MOVE      *BLANKS       XXEAD2           40
     C                   MOVE      *BLANKS       XXECSZ           40
     C     EMMPFL        IFEQ      'P'
     C                   MOVEL     EMPAD1        XXEAD1
     C                   MOVEL     EMPAD2        XXEAD2
     C                   MOVEL     EMPCTY        XXCITY           25
     C                   MOVEL     EMPST         XXST              2
     C                   MOVEL     EMPZIP        XXZIP5            5
     C                   MOVE      EMPZIP        XXZIP4            4
     C                   ELSE
     C                   MOVEL     EMMAD1        XXEAD1
     C                   MOVEL     EMMAD2        XXEAD2
     C                   MOVEL     EMMCTY        XXCITY           25
     C                   MOVEL     EMMST         XXST              2
     C                   MOVEL     EMMZIP        XXZIP5            5
     C                   MOVE      EMMZIP        XXZIP4            4
     C                   ENDIF
     C*
     C     XXCITY        CAT(P)    ',':0         XXECSZ
     C                   CAT       XXST:1        XXECSZ
     C                   CAT       XXZIP5:1      XXECSZ
     C     XXZIP4        IFNE      *ZERO
     C                   CAT       '-':0         XXECSZ
     C                   CAT       XXZIP4:0      XXECSZ
     C                   ENDIF
     C*
     C                   MOVEL(P)  EMNAME        CRENAM
     C                   MOVEL     EMNAME        CRNAM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        CRNAM1
     C                   MOVE      EMNAM2        CRNAM2
     C                   MOVEL     EMFDNM        CRFDN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        CRFDN1
     C                   MOVE      EMFDN2        CRFDN2
      *
     C                   MOVEL(P)  CRNAM1        ##LINE
     C                   MOVEL(P)  ##LINE        CRNAM1
      *
     C                   MOVE(P)   CRNAM2        XXTMP            50
     C                   MOVEL(P)  XXTMP         CRNAM2
      *
     C                   MOVEL(P)  CRFDN1        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRFDN1
      *
     C                   MOVE(P)   CRFDN2        XXTMP            50
     C     ##REM         CAT(P)    XXTMP         ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRFDN2
      *
     C     XXEAD1        CAT(P)    ', ':0        CREADR
     C                   CAT(P)    XXECSZ:0      CREADR
     C*
     C                   MOVE      CXCXDT        CREFDT
     C                   MOVE      CXCDAT        CRMLDT
     C*
     C                   MOVEL     'A'           CRSTAT
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C                   Z-ADD     DSTIME        CRTIME
     C*
     C     FNKY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      F2CAR#        CRCARR
     C                   ELSE
     C                   MOVE      *BLANK        CRCARR
     C                   ENDIF
     C*
     C                   MOVE      ELPFDT        CRPFDT
     C                   MOVE      *ZERO         CRTORE
     C     CLTID         IFEQ      '071'
     C                   MOVEL     EMDBA         CRGSI#
     C                   ENDIF
     C*
     C                   MOVE      CXRSN         CRCRSN
     C                   MOVEL(P)  $CRNCC        KKKEY
     C                   MOVE      'CXCD'        KKTBID
     C     GTKY          CHAIN     PGT01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     GTDESC        CRRESN
     C                   ELSE
     C                   MOVE      *BLANKS       CRRESN
     C                   ENDIF
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      FNNAME        CRFNAM
     C                   ELSE
     C                   MOVE      *BLANKS       CRFNAM
     C                   ENDIF                                                  *IN60
     C*
     C                   MOVE      EMFEIN        CRFEIN
     C                   MOVEL     XXEAD1        CREAD1
     C     XXEAD2        IFNE      *BLANK
     C                   MOVEL     XXEAD2        CREAD2
     C                   MOVEL     XXECSZ        CRECSZ
     C                   ELSE
     C                   MOVEL     XXECSZ        CREAD2
     C                   MOVEL     *BLANK        CRECSZ
     C                   ENDIF
     C*
     C                   MOVE      CXFND         #@FND             3
     C                   MOVE      CXEMP#        #@EMP#            9
     C                   MOVE      CXFYR         #@FYR             3
     C*
     C     #@FND         CAT(P)    #@EMP#:0      CRPOL#
     C                   CAT       #@FYR:0       CRPOL#
     C*
     C                   MOVEL(P)  CRPOL#        CRCAP#
     C*
     C*  Check for a carrier/state reporting record
     C     RPKY          CHAIN     PRP01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     RPCAP#        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  RPCAP#        CRCAP#
     C                   MOVEL(P)  RPCAP#        XXPOL
     C*
     C*  If carrier policy number is numeric, check to see if it is a
     C*  PDS old-format policy number
     C                   TESTN                   XXOPOL               61
     C     *IN61         IFEQ      *ON
     C     XOBLNK        IFEQ      *BLANKS
     C                   Z-ADD     *ZEROS        KKEMP#
     C                   MOVE      XXOFND        KKFND
     C                   MOVE      XXOEMP        KKEMP#
     C                   MOVE      XXOFYR        KKFYR
     C*
     C     ELKY2         CHAIN     PEL02                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   MOVE      ELFND         KKFND
     C                   MOVE      ELEMP#        KKEMP#
     C                   MOVE      ELFYR         KKFYR
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       CRPOL#
     C*
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C*
     C                   ENDIF                                                  XOBLNK=BLKS
     C*
     C                   ENDIF                                                  *IN61 IFEQ *OFF
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       CRPOL#
     C*
     C                   ENDIF                                                  RPCAP# IFNE *BLANKS
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       CRPOL#
     C*
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C                   MOVE      ELPTDT        CRPTDT
     C*
     C                   EXSR      SRISUO
     C                   MOVEL     XXISUO        CRISUO
     C*
     C                   EXSR      SRAOUT
     C                   MOVEL     XXAOUT        CRAOUT
     C*
     C                   Z-ADD     CXCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRISUD
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRPFRD
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRPTOD
     C*
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CRCAND
      *
      *
      *  Get Employer Federal Name
      *
     C                   MOVEL(P)  *BLANKS       CREDBA
     C     EMKY1         CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  EMFDNM        CREDBA
     C     EMFDN2        IFNE      *BLANKS
     C     EMFDNM        CAT(P)    EMFDN2:0      #WRK10          100
     C                   MOVEL(P)  #WRK10        CREDBA
     C                   ENDIF                                                  EMFDN2 IFNE
     C                   ENDIF
      *
     C*
     C                   MOVE      CXAMTD        CRAMTD
     C                   MOVE      CXRFEE        CRRFEE
     C*
     C                   Z-ADD     CXRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         CRRPMY
     C                   MOVE      CXERR         CRERR
     C*
     C                   MOVEL     CXCOM1        CRCOM1
     C                   MOVEL     CXCOM2        CRCOM2
     C*
     C     FNMAD1        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  FNMAD1        CRGAD1
     C                   MOVEL(P)  FNMAD2        CRGAD2
     C                   MOVEL     FNMCTY        XXCITY
     C                   MOVEL     FNMST         XXST
     C                   MOVEL     FNMZIP        XXZIP5
     C                   MOVE      FNMZIP        XXZIP4
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  FNPAD1        CRGAD1
     C                   MOVEL(P)  FNPAD2        CRGAD2
     C                   MOVEL     FNPCTY        XXCITY
     C                   MOVEL     FNPST         XXST
     C                   MOVEL     FNPZIP        XXZIP5
     C                   MOVE      FNPZIP        XXZIP4
     C*
     C                   ENDIF                                                  FNMAD1 IFNE *BLANKS
     C*
     C     XXCITY        CAT(P)    ',':0         CRGCSZ
     C                   CAT(P)    XXST:1        CRGCSZ
     C                   CAT(P)    XXZIP5:1      CRGCSZ
     C*
     C     XXZIP4        IFNE      *ZEROS
     C                   CAT(P)    '-':0         CRGCSZ
     C                   CAT(P)    XXZIP4:0      CRGCSZ
     C                   ENDIF                                                  XXZIP4 IFNE *ZEROS
     C*
     C                   MOVE      *BLANKS       CRGPHN
     C                   MOVE      *BLANKS       XXAREA
     C*
     C     FNPH1A        IFNE      0
     C*
     C                   MOVE      FNPH1A        XXAREA            3
     C                   MOVEL     FNPH1         XXEXCH            3
     C                   MOVE      FNPH1         XXPHN#            4
     C*
     C                   ELSE
     C*
     C                   MOVE      FNAREA        XXAREA            3
     C                   MOVEL     FNPH2         XXEXCH            3
     C                   MOVE      FNPH2         XXPHN#            4
     C*
     C                   ENDIF                                                  FNPH1A IFNE 0
     C*
     C     XXAREA        IFNE      *BLANKS
     C*
     C     '('           CAT(P)    XXAREA:0      CRGPHN
     C                   CAT(P)    ')':0         CRGPHN
     C                   CAT(P)    XXEXCH:1      CRGPHN
     C                   CAT(P)    '-':0         CRGPHN
     C                   CAT(P)    XXPHN#:0      CRGPHN
     C*
     C                   ENDIF                                                  XXAREA IFNE *BLANKS
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRIDT
     C                   MOVEL     Q1USER        CRIUSR
     C*
     C                   MOVE      'Y'           CRCFFL
      *
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVEL     Q1PGM         CRPGM
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRDT
     C                   MOVEL     DSTIME        CRTIME
      *
      *
      * Get PEO Information if PEO Group
      *
     C     FNPEOF        IFNE      *BLANK
      *
     C                   CALL      'SRADD1'
     C                   PARM      CXCO#         #5CO#             3 0
     C                   PARM      CXFND         #5FND             3 0
     C                   PARM      CXEMP#        #5EMP#            9 0
     C                   PARM      CXDIV         #5DIV             5 0
     C                   PARM      CXFYR         #5FYR             3 0
     C                   PARM                    #5NAME          100
     C                   PARM                    #5FNAM          100
     C     CREAD1        PARM                    #5EAD1           40
     C     CREAD2        PARM                    #5EAD2           40
     C     CRECSZ        PARM                    #5ECSZ           40
     C                   PARM                    #5CTY            25
     C                   PARM                    #5ST              2
     C                   PARM                    #5ZIP             9 0
     C     CRFEIN        PARM                    #5FEIN            9 0
     C     CREADR        PARM                    #5EADR           90
      *
     C                   MOVEL(P)  #5NAME        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRNAM1
      *
     C                   MOVE(P)   #5NAME        XXTMP            50
     C     ##REM         CAT(P)    XXTMP         ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRNAM2
      *
     C                   MOVEL(P)  #5FNAM        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRFDN1
      *
     C                   MOVE(P)   #5FNAM        XXTMP            50
     C     ##REM         CAT(P)    XXTMP         ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CRFDN2
      *
     C                   MOVEL(P)  #5NAME        CRENAM
     C                   MOVEL(P)  #5FNAM        CREDBA
      *
     C     CREAD2        IFEQ      *BLANK
     C                   MOVEL     CRECSZ        CREAD2
     C                   MOVEL     *BLANKS       CRECSZ
     C                   ENDIF                                                  CREAD2
      *
     C                   ENDIF                                                  FNPEOF
      *
     C*
     C                   MOVE      *OFF          *IN36
     C     CLTID         IFEQ      '114'
     C                   MOVE      *ON           *IN36
      *
     C                   MOVE      CRAMTD        XXAMT            11 2
     C                   CALL      'WA082R'
     C                   PARM                    CXCO#
     C                   PARM                    CXFND
     C                   PARM                    CXEMP#
     C                   PARM                    CXFYR
     C                   PARM                    CXDIV
     C                   PARM      CRCAND        XXDUDT            8 0
     C                   PARM                    XXAMT
     C                   PARM                    XXSCAN           41
     C                   MOVEL     XXSCAN        CROCR
     C                   ENDIF
     C*
     C*
     C                   OPEN      WX#@P
     C                   WRITE     P#@01
     C                   CLOSE     WX#@P
     C*
     C*  Call CL to print documents
     C                   MOVE      CXCO#         QQCO#
     C                   MOVE      CXFND         QQFND
     C                   MOVE      CXFYR         QQFYR
     C                   MOVE      CXEMP#        QQEMP#
     C                   MOVE      CXDIV         QQDIV
     C                   MOVE      CXREQ#        QQREQ#
     C*
     C     INFO          IFEQ      *OFF
     C                   EXSR      GTCXIN
     C                   ENDIF
     C*
     C                   SELECT
     C     AUTOCF        WHENEQ    *ON
     C                   MOVE      $CRPCL        @@PCHL
     C                   ENDSL
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         saveDate          7 0
     C*
     C                   Z-ADD     CRMLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         xxMLDT            8 0
     C*
     C                   Z-ADD     CREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         x1EFDT            8 0
     C                   move      cxco#         #1co#
     C                   move      cxfnd         #1fnd
     C                   move      cxemp#        #1emp#
     C                   move      cxdiv         #1div
     C                   move      ' '           ##odr
     C                   move      'F'           ##nodr
     C                   move      'M'           ##aodr
     C                   clear                   ##emn1
     C                   clear                   ##emn2
     C                   clear                   ##emn3
     C                   clear                   ##emn4
     C                   clear                   ##emn5
     C                   clear                   ##emn6
     C                   clear                   ##emn7
     C                   clear                   ##amn1
     C                   clear                   ##amn2
     C                   clear                   ##amn3
     C                   clear                   ##amn4
     C                   clear                   ##amn5
     C                   clear                   ##amn6
     C                   clear                   ##amn7
     C                   clear                   ##amn8
     C                   clear                   ##amn9
     C                   Clear                   QQPEOExt
     C*
     C     ELKY3         Chain     PEL01
     C*
     C                   MOVE      ELRTCD        KKRTCD
     C     RSKY          CHAIN     PRS01                              60
     C*
     C     RRKY          Setgt     PRR00
     C     RRKY2         Readpe(n) PRR00                                  60
     C*
     C                   select
     C                   when      FNPEOF = *blanks
     C                   clear                   ##pgfl
     C                   when      rrst <> '  '
     C                   eval      ##pgfl = RRPGFL
     c                   if        ##pgfl = *blank
     c                   move      'B'           ##pgfl
     c                   endif
     C                   Eval      QQPEOExt = RRPEOA
     C                   other
     C                   clear                   ##pgfl
     C                   endsl
     C                   Eval      ##dbaf = ' '
     C*
     C                   call      'SRADDR'
     C                   parm                    #1co#             3
     C                   parm                    #1fnd             3
     C                   parm                    #1emp#            9
     C                   parm                    #1div             5
     C                   parm                    ##odr             1
     C                   parm                    ##nodr            1
     C                   parm                    ##aodr            1
     C                   parm                    ##emn1           50
     C                   parm                    ##emn2           50
     C                   parm                    ##emn3           50
     C                   parm                    ##emn4           50
     C                   parm                    ##emn5           50
     C                   parm                    ##emn6           50
     C                   parm                    ##emn7           50
     C                   parm      *zero         ##adrl            3 0
     C                   parm      *zero         ##naml            3 0
     C                   parm      *zero         ##fedl            3 0
     C                   parm      *blank        ##peo1           57
     C                   parm      *blank        ##peo2           57
     C                   parm                    ##pgfl            1
     C                   parm                    ##conl            3 0
     C                   parm                    ##amn1
     C                   parm                    ##amn2
     C                   parm                    ##amn3
     C                   parm                    ##amn4
     C                   parm                    ##amn5
     C                   parm                    ##amn6
     C                   parm                    ##amn7
     C                   parm                    ##amn8
     C                   parm                    ##amn9
     C                   parm      *zero         ##peol            3 0
     C                   parm                    ##dbaf            1
     C                   parm      *zero         ##al2             3 0
     C                   parm      *zero         ##nl2             3 0
     C                   parm      *zero         ##fl2             3 0
     C                   parm      *zero         ##pl2             3 0
     C                   parm      *zero         ##cl2             3 0
     C                   PARM                    QQPEOExt         10
     C                   PARM      *blanks       QQname1         120
     C                   PARM      *blanks       QQname2         120
     C                   PARM      *blanks       QQname3         120
     C                   PARM      *blanks       QQname4         120
     C*
     C                   If        isUseXml
     C                   Open      wxc7p
     C                   Exsr      ScriptXML
     C                   Close     wxc7p
     C                   Endif
     C                   SELECT
     C     CFDART        WHENEQ    *BLANK
     C                   MOVE      *BLANKS       QQFOLD
     C                   MOVE      *BLANKS       QQDCID
     C     CFDART        WHENEQ    'Y'
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       #1FOLD           80
     C                   PARM      *BLANKS       #1DCID           12
     C                   PARM      'CC'          #1PRE             2
     C                   PARM      QQCO#         #1CO#             3
     C                   PARM      QQFND         #1FND             3
     C                   PARM      'P'           #1RTCD            1
     C*
     C                   MOVE      #1FOLD        QQFOLD
     C                   MOVE      #1DCID        QQDCID
     C                   ENDSL
     C                   Clear                   QQSpool
     C                   If        isUseSpoolOut
     C                   Move      'Y'           QQSpool
     C                   Endif
     C*
     C                   CALL      'W0172C'      DOCPL
     C*
     C                   OPEN      WX#@P
     C                   READ      P#@01                                  60
     C  N60              DELETE    P#@01
     C                   CLOSE     WX#@P
     C*
     C                   ENDSR                                                  SRNOTC
     C*================================================================
     C*  CHKDIV - Processes records for non state rprtg divs
     C*================================================================
     C     CHKDIV        BEGSR
     C*
     C                   Z-ADD     CXDIV         SAVDIV            5 0
     C                   Z-ADD     CXCXDT        SVCXDT            7 0
     C                   Z-ADD     CXCDAT        SVCDAT            7 0
     C*
     C     CXKY2         CHAIN     PCX01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     CXRRN1        CXRRN
     C*
     C*   Process record if not state rpt div but has same canx eff date
     C*   and canx issue date
     C     CXDIV         IFNE      SAVDIV
     C     CXCXDT        ANDEQ     SVCXDT
     C     CXCDAT        ANDEQ     SVCDAT
     C                   MOVE      *BLANK        XXAUCX
     C     AUKY          CHAIN     PAU01                              60
     C     *IN60         IFEQ      *OFF
     C     AUCXFL        ANDEQ     'Y'
     C                   MOVE      'Y'           XXAUCX
     C                   ENDIF
     C*
     C*   If the Generate Audit Request flag = 'Y' then call W7231C
     C     CFAURQ        IFEQ      'Y'
     C     XXAUCX        ANDNE     'Y'
       //*** Beg Add *** 10/28/20 *****************************************
     C     ##allDiv      andeq     'Y'
       //*** End Add *** 10/28/20 *****************************************
     C*
     C                   EXSR      GTCXIN
     C*
     C                   SELECT
     C     AUTOCF        WHENEQ    *ON
     C                   MOVEL     $CRATR        @@ADTK
     C                   ENDSL
     C*
     C                   MOVE      CXDIV         QQDIV
     C                   SELECT
     C     CLTID         WHENEQ    '048'
     C                   CALL      'W7231C'      AURQ48
     C     CLTID         WHENEQ    '091'
     C                   CALL      'W7231C'      AURQ91
     C     CLTID         WHENEQ    '093'
     C                   CALL      'W7231C'      AURQ93
     C                   OTHER
     C                   CALL      'W7231C'      AURQPL
     C                   ENDSL
     C*
     C                   ENDIF                                                  CFAURQ='Y'
     C*
     C*   If the Update Quote Tracking flag = 'Y' then write a 'C' type
     C*   record to the Quote Tracking file
     C     CFQTRK        IFEQ      'Y'
      *
     C                   CALL      'WA002R'                             99
     C                   PARM                    CXCO#
     C                   PARM                    CXFND
     C                   PARM                    CXEMP#
     C                   PARM                    CXDIV
     C                   PARM      *ZERO         ##FYR             3 0
     C     KKAGT#        PARM      *ZERO         ##AGT#            5 0
     C                   PARM      *BLANK        ##ANAM           60
      *
     C                   Z-ADD     CXEMP#        KKEMP#
     C                   Z-ADD     CXDIV         KKDIV
     C                   MOVE      'W'           KKSTAT
     C     QTKY          SETGT     PQT01
     C     QTKY          READPE(N) PQT01                                  60
     C   60QTKY1         CHAIN(N)  PQT01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     KKAGT#        QTAGT#
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C                   Z-ADD     XXDATE        QTWKDT
     C                   MOVE      'C'           QTTYPE
     C*
     C                   MOVE      Q1USER        QTUSER
     C                   MOVE      DSTIME        QTTIME
     C                   MOVE      Q1PGM         QTPGM
     C                   Z-ADD     XXDATE        QTDT
     C*
     C                   MOVE      QTUSER        QTIUSR
     C                   MOVE      QTTIME        QTITM
     C                   MOVE      QTPGM         QTIPGM
     C                   MOVE      QTDT          QTIDT
     C*
     C                   WRITE     PQT01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  CFQTRK='Y'
     C*
     C     CXRRN         CHAIN     WDCXP                              60
     C*
     C*   Update Confirmation flag and Conf processed date
     C                   MOVE      'Y'           CXCFFL
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXCFPD
      *
     C                   MOVEL     Q1USER        CXUSER
     C                   MOVEL     Q1PGM         CXPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXDT
     C                   MOVEL     DSTIME        CXTIME
      *
     C                   UPDATE    PCX0P
     C*
     C     CFCFDR        IFNE      *BLANK
     C     isUseSpoolOut ANDEQ     *on
     C                   EXSR      DARTS
     C                   ENDIF
     C*
     C                   ENDIF                                                  CXDIV<>SAVDIV
     C*
     C     CXKY2         READE(N)  PCX01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHKDIV
     C*================================================================
     C*  DARTS - Write a DARTS remark
     C*================================================================
     C     DARTS         BEGSR
     C*
     C*   Establish key
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      CXCO#         QCCO#
     C                   MOVE      CXFND         QCFND
     C                   MOVE      CXEMP#        QCEMP#
     C                   MOVE      CXDIV         QCDIV
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  CFCFDR        Q#RMCD            5
     C                   MOVE      *BLANKS       Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVEL     'DATE'        EDTMSK
     C                   Z-ADD     CXCFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCFDT           10
     C                   Z-ADD     CXCFPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCFPD           10
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCXDT           10
     C                   Z-ADD     CXCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCDAT           10
     C     Q#RMCD        CHAIN     PCT01                              60
     C     CTDESC        CAT       YYCFDT:3      Q#COMM
     C                   CAT       XXCFDT:0      Q#COMM
     C                   CAT       ',':0         Q#COMM
     C                   CAT       YYCFPD:1      Q#COMM
     C                   CAT       XXCFPD:0      Q#COMM
     C                   CAT       ',':0         Q#COMM
     C                   CAT       YYCXDT:1      Q#COMM
     C                   CAT       XXCXDT:0      Q#COMM
     C                   CAT       ',':0         Q#COMM
     C                   CAT       YYCDAT:1      Q#COMM
     C                   CAT       XXCDAT:0      Q#COMM
     C                   CAT       '.':0         Q#COMM
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVE(P)   *BLANKS       Q#DCID           12
     C                   MOVEL(P)  'N'           Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL(P)  *BLANKS       Q#CSID            2
     C                   MOVEL(P)  *BLANKS       Q#CODE            2
     C                   SELECT
     C     CFDART        WHENEQ    *BLANK
     C                   MOVE      *BLANKS       Q#FOLD
     C                   MOVE      *BLANKS       Q#DCID
     C     CFDART        WHENEQ    'Y'
     C                   MOVE      #1FOLD        Q#FOLD
     C                   MOVE      #1DCID        Q#DCID
     C                   MOVE      'O'           Q#IMGF
     C                   ENDSL
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C                   MOVE      Q#SEQ#        ##SEQ#            5 0
     C*
     C                   ENDSR                                                  DARTS
     C*================================================================
     C*  SRISUO - Gets issuing office info
     C*================================================================
     C     SRISUO        BEGSR
     C*
     C*  Load the issuing office (company file) for NCCI form
     C*
     C*    If the alternate company name is not blank, use it instead of the
     C*    company name.
     C     FNANAM        IFNE      *BLANKS
     C*
     C                   MOVEL     FNAZIP        XXZIP5
     C                   MOVE      FNAZIP        XXZIP4
     C     FNANAM        CAT(P)    ',':0         XXISUO           90
     C                   CAT       FNAAD1:1      XXISUO
     C                   CAT       ',':0         XXISUO
     C                   CAT       FNACTY:1      XXISUO
     C                   CAT       ',':0         XXISUO
     C                   CAT       FNAST:1       XXISUO
     C                   CAT       ',':0         XXISUO
     C                   CAT       XXZIP5:1      XXISUO
     C*
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANK
     C                   CAT       '-':0         XXISUO
     C                   CAT       XXZIP4:0      XXISUO
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL     COZIP         XXZIP5
     C                   MOVE      COZIP         XXZIP4
     C     CONAME        CAT       ',':0         XXISUO
     C                   CAT(P)    COADR1:1      XXISUO
     C                   CAT       ',':0         XXISUO
     C                   CAT       COCTY:1       XXISUO
     C                   CAT       ',':0         XXISUO
     C                   CAT       COST:1        XXISUO
     C                   CAT       ',':0         XXISUO
     C                   CAT       XXZIP5:1      XXISUO
     C     XXZIP4        IFNE      *ZERO
     C     XXZIP4        ANDNE     *BLANK
     C                   CAT       '-':0         XXISUO
     C                   CAT       XXZIP4:0      XXISUO
     C                   ENDIF
     C*
     C                   ENDIF                                                  FNANAM IFNE *BLANKS
     C*
     C                   ENDSR                                                  SRISUO
     C*================================================================
     C*  SRAOUT - Gets issuing office info
     C*================================================================
     C     SRAOUT        BEGSR
     C*
     C*  Get agent name
     C*
     C     AAKY          CHAIN     PAA01                              60
     C     *IN60         IFEQ      '0'
     C     AGKY          CHAIN     PAG01                              60
     C                   ENDIF
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       XXANAM           40
     C                   ELSE
     C                   MOVEL     AGNAME        XXANAM
      *
      *
     C                   MOVEL(P)  AGNAME        CRANAM
     C                   MOVEL(P)  AGMAD1        CRAMAD
     C                   MOVE      AGARE1        CRAARE
     C                   MOVE      AGPHN1        CRAPHN
     C     AGMCTY        CAT(P)    ',':0         CRAMCZ
     C                   CAT(P)    AGMST:1       CRAMCZ
     C                   MOVEL(P)  AGMZIP        @@MZIP            5
     C                   CAT(P)    @@MZIP:1      CRAMCZ
      *
      * Get the name for the employer
      *
     C                   MOVEL     '1'           ##TYP1
     C     MEKY          CHAIN     PEC01                              80
     C     *IN(80)       IFEQ      *OFF
     C     ECTITL        IFNE      *BLANKS
     C                   MOVEL(P)  ECTITL        CRECN1
     C                   CAT(P)    ECFSN:1       CRECN1
     C                   ELSE
     C                   MOVEL(P)  ECFSN         CRECN1
     C                   ENDIF
      *
     C     ECMID         IFNE      *BLANKS
     C                   CAT(P)    ECMID:1       CRECN1
     C                   ENDIF
     C                   CAT(P)    ECLSN:1       CRECN1
     C                   ENDIF
      *
      *
     C                   ENDIF
     C*
     C*
     C*  Load agent name
     C                   MOVEL     XXANAM        XXAGNM           30
     C                   MOVEL     CXAGT#        XXA#C             5
     C     XXAGNM        CAT(P)    '/':1         XXAOUT           60
     C                   CAT       XXA#C:1       XXAOUT
     C*
     C                   ENDSR                                                  SRAOUT
      /free

       // *=========================================================================
       // *  ScriptXML - Reserve/Update XML record for Scriptura output.
       // *=========================================================================
       Begsr ScriptXML;

       xmlFile = ' ';
       message = %trim(processType);
       string = ' ';

       If isMassReq;

          In *Lock *dtaara;
          Out *dtaara;

          If ldMassFile <> ' ';
             xmlFile = %trim(ldMassFile);
          Else;
             StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
              'CX_Confirm':'000':'000':massFlag:xmlLink);
             In *Lock *dtaara;
             ldMassFile = %trim(xmlFile);
             Out *dtaara;
             Chain (xmlFile) xp100;
             XPCat = 'PS';
             Update xp100;
          Endif;

          isMassReq = *off;
          xmlFileName = xmlFile;
          xmlLink = xmlFile;

       Endif;

       StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x#RRN:'X'):xxPre:
        string:%editc(CXCo#:'X'):%editc(CXFnd:'X'):massFlag:xmlLink);

       If xmlFileName = ' ';
          xmlFileName = xmlFile;
       Endif;

       If xmlLink = ' ';
          Chain (xmlFile) xp100;
          xmlFile = XPXmlT;
       Else;
          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
       Endif;

       outputFile = XPFile;

       //* Set the output file name into the local area for additional documents to pick up.
       In *Lock *dtaara;
       ldOutputFile = outputFile;
       Out *dtaara;

       XPData = 'Y';

       XPPgm  = Q1PGM;
       XPUser = Q1USER;
       XPTime = DsTime;
       XPDt   = saveDate;

       Update xp100;

       //* Write the main work file record.
       Clear pc701;

       C7XML = xmlFile;

       C7Co#  = CXCo#;
       C7Fnd  = CXFnd;
       C7Emp# = CXEmp#;
       C7Div  = CXDiv;
       C7Fyr  = CXFyr;
       C7UNID = CRUNID;

       C7CARR = CRCARR;
       C7GFIN = CRGFIN;
       C7GSI# = CRGSI#;
       C7POL# = CRPOL#;
       C7CAP# = CRCAP#;
       C7OPOL = CROPOL;
       C7OPED = CROPED;
       C7LSID = CRLSID;

       C7CORR = CRCORR;
       C7TORE = CRTORE;
       C7FLAT = CRFLAT;
       C7PROR = CRPROR;
       C7SHRT = CRSHRT;
       C7NRNW = CRNRNW;
       C7CFFL = CRCFFL;
       C7ERR  = CRERR;

       C7ISUD = CRISUD;
       C7PFRD = CRPFRD;
       C7PTOD = CRPTOD;
       C7CAND = CRCAND;
       C7MLDT = xxMLDT;
       C7EFDT = x1EFDT;
       C7PCXD = CRPCXD;

       C7CNC# = CRCNC#;
       C7RPMY = CRRPMY;
       C7WEDT = 0;
       C7CNC# = CRCNC#;
       C7CRSN = CRCRSN;
       C7RESN = CRRESN;
       C7CXRN = CRCXRN;
       C7DESC = CRDESC;

       C7COM1 = CRCOM1;
       C7COM2 = CRCOM2;

       C7AMTD = CRAMTD;
       C7RFEE = CRRFEE;
       C7RFDS = CRRFDS;
       C7TDUE = CRTDUE;
       C7CEMN = CRCEMN;

       C7OCR  = CROCR;

       C7ForNam1 = QQName1;
       C7ForNam2 = QQName2;
       C7ForNam3 = QQName3;
       C7ForNam4 = QQName4;
       C7ForAdr1 = %trim(amn(##adrl));
       C7ForAdr2 = %trim(amn(##adrl+1));
       C7ForAdr3 = %trim(amn(##adrl+2));

       C7Fein = %editc(CRFein:'X');

       C7Pgm  = Q1PGM;
       C7User = Q1USER;
       C7Time = DsTime;
       C7Dt   = saveDate;

       Write pc701;

       Endsr;

      /end-free
     P*============================================
     P* Determine if the object exists
     P*============================================
     PObjExists        b
     DObjExists        pi             1n
     D object                        10    const
     D objType                        7    const

     D obj             s             10
     D type            s              7
     D error           s              1n

     C                   call      'SRCHKOBJ'
     C                   parm      object        obj
     C                   parm      objType       type
     C                   parm                    error

     C                   return    not error

     PObjExists        e
