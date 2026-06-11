     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H/TITLE W7061R - PAY HISTORY REPORT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/08/23 C:340 project 140WBY                                 *
     H*                Add Lost Time Indicator                        *
     H*                                                               *
     H* 12/09/15 C:62 PROJECT SY371                                   *
     H*               Add Scriptura processing.                       *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/22/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     F* 04/19/10  C:235 Project:  25WRG                               *
     F*                Claims General Retrofit                        *
     H*                                                               *
     F* 03/11/10  C:189 Project:  139WNS                              *
     F*                Pay History Sort By Check# / Transaction Date  *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant name     *
     H*                                                               *
     H* 02/22/07 C:53 Support: 57237                                  *
     H*               Added code for additional searching of WDRPP    *
     H*                                                               *
     H* 12/15/06 C:132 Project: 139WGE                                *
     H*                Use PARQDT instead of PAPODT if PARQDT not zero*
     H*                                                               *
     H* 03/01/04 C:53 Support: WC072/41345                            *
     H*               Prevent the to case field from being corrupted. *
     H*               Use another file for WMCMLD                     *
     H*                                                               *
     H* 01/28/04 C:15 Support: WC129/41025                            *
     H*               Added code to use WDRPP for the payee name.     *
     H*                                                               *
     H* 08/14/03 C:53 Support: WC051/38481                            *
     H*               Allow processing of all divisions for an employer
     H*                                                               *
     H* 07/17/03 C:53 Support: WC065/38013                            *
     H*               Correct weeks/days calc.  When a neg recovery   *
     H*               is processed, the weeks/days should be added.   *
     H*                                                               *
     H* 12/09/02 C:132 PROJECT WC577                                  *
     H*                Add Nature Code and Description to both the    *
     H*                work file and to the report.                   *
     H*                                                               *
     H* 08/13/01 C:61  PROJECT WC577                                  *
     H*                Add selections by Reserve Code and selections  *
     H*                by Reserve Type.                               *
     H*                                                               *
     H*  11/20/00 C:53 SEQ#: WC094/25139                              *
     H*               -CHANGED ARRAYS ELEMENTS FROM 100 TO 200.       *
     H*                                                               *
     H*  05/05/99 C:03 SEQ#: WC100/17978                              *
     H*               -CORRECT LOAD OF EMPLOYER NAME IN WRKF          *
     H*                                                               *
     H*  10/30/98 C:33 SEQ#: WC048/14741                              *
     H*               -USE CORRECT SELECTIONS                         *
     H*               -V3 STANDARDS                                   *
     H*                                                               *
     H*  05/13/98 C:15 SEQ#: WC095/11621                              *
     H*                Removed code which cleared PAAMT during the    *
     H*                middle of the DO loop, for no apparent reason. *
     H*                                                               *
     H*  04/03/98 C:33 SEQ#: WC060/10988                              *
     H*                VALIDATE AGAINST PAY VAL 1.                    *
     H*                                                               *
     H*  03/31/98 C:33 SEQ#: WC060/10841                              *
     H*                NEED TO VERIFY THE PAYMENT MEETS SELECTED      *
     H*                CRITERIA BEFORE CLEARING FIELDS.               *
     H*                                                               *
     H*  03/13/98 C:85 PROJECT: WC577                                 *
     H*                CHANGED FILE FROM WDPAL3 TO WDPAL5 TO SORT BY  *
     H*                TRANSACTION DATE.                              *
     H*                                                               *
     H* 10/23/97 C:58   New for Version 3.                            *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  71    WMCMP key CO#/FND/FYR (PCM02)                          *
     H*  72    WMCMP key CO#/FND/EMP#/DIV/FYR (PCM01)                 *
     H*  73    WMCMP key CO#/FND/EMP#/DIV (PCM01)                     *
     H*  74    WMCMP key CO#/FND (PCM01)                              *
     H*  75    WMCMP key CO#/FND/EMP# (PCM01)                         *
     H*  76    WMCMP key CO#/FND/FYR/EMP# (PCM03)                     *
     H*  84    Payment Value 1 PARM not equal blanks                  *
     H*  84    WDPAP key CO#/FND/FYR/CASE                             *
     H* N84    WDPAP key CO#/FND/FYR/CASE/BR2                         *
     H* 90    CHAIN TO WDPAP WITH CO/FND/FYR/CASE                     *
     H* 91    CHAIN TO WDPAP WITH CO/FND (FOR ALL CASES)              *
     H* 13    PRINT HEADER FOR CLAIM, IF NOT YET PRINTED.             *
     H* 14    PRINT COPYRIGHT LINE
     H* 23    NO EMPLOYER ENTERED - SELECT *ALL
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F*________________________________________________________________
     FWMEML     IF   E           K DISK
     F*      Employer Master --Co/Fnd/Emp/Div                    -EMKY-
     F*________________________________________________________________
      *  OFFICE MASTER
     FWMOFL     IF   E           K DISK
     FSMORL     IF   E           K DISK
      *  WORKERS COMP DEPARTMENT MASTER
     FWMDPL     IF   E           K DISK
      *  OCCUPATION MASTER
     FWTOCL     IF   E           K DISK
     F*  Payment Detail
     F*
     F*** BEGIN CHG *** 03/13/98 **************************************
     F**********WDPAL3  IF  E           K        DISK
     FWDPAL5    IF   E           K DISK
     F*** END   CHG *** 03/13/98 **************************************
     F                                     RENAME(PPA01:PPA03)
     F*
     F*** BEG ADD *** 02/22/07 ***************************
     FWDPAL4    IF   E           K DISK
     F                                     RENAME(PPA01:PPA04)
     F*** END ADD *** 02/22/07 ***************************
     F*** BEG ADD *** 03/11/10 ***************************
     FWDPALL    IF   E           K DISK
     F                                     RENAME(PPA01:PPA05)
     F*** END ADD *** 03/11/10 ***************************
     F*
     F*  Claim Master  CO FND EMP DIV FYR CASE
     FWMCML     IF   E           K DISK
     F*
     F*  Claim Master  CO FND FYR CASE
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F*** BEG ADD *** 08/14/03  *********************************
     F*** BEG CHG *** 03/01/04 *********************************
     F*  Claim Master  CO FND FYR EMP
     F*WMCMLD**IF**E           K        DISK
     FWMCMLP    IF   E           K DISK
     F*** END CHG *** 03/01/04 *********************************
     F                                     RENAME(PCM01:PCM03)
     F*** END ADD *** 08/14/03  *********************************
     F*________________________________________________________________
     FWMCDL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMPDL     IF   E           K DISK
     F*** Begin add *** 01/28/04 **************************************
     FWDRPL3    IF   E           K DISK
     F*** End add ***** 01/28/04 **************************************
     f*** Begin Add *** 12/09/15 ***************************************
     FSXPHPDTL  uf a e             disk    Rename(pphd01:pphd0p)
     FSXPHPCLM  uf a e             disk    Rename(pphc01:pphc0p)
     FSXPHPCLMT uf a e             disk    Rename(pph101:pph10p)
     FSXPHPRPT  uf a e             disk    Rename(pphr01:pphr0p)
     FSXPHPRPTT uf a e             disk    Rename(pph201:pph20p)
     FXMLPREL1  if   e           k disk    Rename(xp100:pxp01)
     f*** End   Add *** 12/09/15 ***************************************
     F*
     F*  Report Workfile
     FWFPHP     O    E             DISK    USROPN
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F                                     USROPN

     D*
     D*  Tables and Arrays   ==========================================
     D*** BEG CHG *** 11/20/00 *********************************
     D*********           CD        100  2               PMT CODES
     D*********           DS        100 25               CODE DESC
     D*********           AM        100 11 2             CODE AMTS
     D*********           CD2       100  2               BR2 CODES
     D*********           DS2       100 25               BR2 CODE DESC
     D*********           AM2       100 11 2             BR2 CODE AMT
     D*********           WK2       100  5 0             BR2 # OF WKS
     D*********           DA2       100  5 0             BR2 # OF DYS
     D*********           AM3       100 11 2             TOT CODE AMTS
     D*********           AM4       100 11 2             TOT BR2 CODE AMTS
     D*********           WK4       100  5 0             TOT BR2 # OF WKS
     D*********           DA4       100  5 0             TOT BR2 # OF DYS
     D CD              S              2    DIM(200)                             PMT CODES
     D DS              S             25    DIM(200)                             CODE DESC
     D AM              S             11  2 DIM(200)                             CODE AMTS
     D CD2             S              2    DIM(200)                             BR2 CODES
     D DS2             S             25    DIM(200)                             BR2 CODE DESC
     D AM2             S             11  2 DIM(200)                             BR2 CODE AMT
     D WK2             S              5  0 DIM(200)                             BR2 # OF WKS
     D DA2             S              5  0 DIM(200)                             BR2 # OF DYS
     D AM3             S             11  2 DIM(200)                             TOT CODE AMTS
     D AM4             S             11  2 DIM(200)                             TOT BR2 CODE AMTS
     D WK4             S              5  0 DIM(200)                             TOT BR2 # OF WKS
     D DA4             S              5  0 DIM(200)                             TOT BR2 # OF DYS
     D*** END ADD *** 02/22/07 ***************************
      *
      *
     D***  BEGIN ADD  ***  10/30/98  *********************************
     D TITLE           C                   CONST('Pay History Report')
     D***  END   ADD  ***  10/30/98  *********************************
      ***** BEG ADD ******* 08/13/01 **********************************
      *
     D                 DS
     D  XXRCD                  1     10
     D  XXRCD1                 1      2
     D  XXRCD2                 3      4
     D  XXRCD3                 5      6
     D  XXRCD4                 7      8
     D  XXRCD5                 9     10
      *
     D                 DS
     D  XXRTP                  1      4
     D  XXRTP1                 1      1
     D  XXRTP2                 2      2
     D  XXRTP3                 3      3
     D  XXRTP4                 4      4
      *
      ***** END ADD ******* 08/13/01 **********************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** BEG ADD ****** 05/25/07 **********************************
     D XXATTX          C                   CONST('*** NO AUTHORITY ***')
     d*** Begin Add *** 12/09/15 *********************************************
     D isScriptUsed    s              1A   inz('N')
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     D outCall         s             11
     D InXML           s             60
     D InReturn        s            250
     D OutMode         s              1
     D OutRrn          s              9
     D OutPrefix       s              2
     D OutString       s             10
     D OutCo#          s              3
     D OutFnd          s              3
     D isTrCd          s               n
     D chrKey          s                   Like(PCkey)
     D procType        s            100
     d massFlag        s              1    Inz('Y')
     d xmlLink         s             60    Inz(' ')
     d*** Beg   Add *** 02/08/23 *********************************************
     d xxLtSt          S              1
     d X@LTI           S              4
     d*** Beg   Add *** 02/08/23 *********************************************
     D*
     D* Scripture Processing Program
     D StLetter        pr                  ExtPgm('STLETTER')
     D   outCall_                    11
     D   InXML_                      60
     D   InReturn_                  250
     D   OutMode_                     1
     D   OutRrn_                      9
     D   OutPrefix_                   2
     D   OutString_                  10
     D   OutCo#_                      3
     D   OutFnd_                      3
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     D/copy sPrModule
     D/copy sPrXML
     D/copy sPrDate
     d*** End   Add *** 12/09/15 *********************************************

     I*** END CHG *** 11/20/00 *********************************
      *
     I*** BEG ADD *** 02/22/07 ***************************
     IPPA04
     I              PACO#                       P4CO#
     I              PAFND                       P4FND
     I              PAFYR                       P4FYR
     I              PACASE                      P4CASE
     I              PASEQ#                      P4SEQ#
     I              PACHK#                      P4CHK#
     I              PABASQ                      P4BASQ
     I              PATRDT                      P4TRDT
     I              PAPODT                      P4PODT
     I              PAFEIN                      P4FEIN
     I              PAMOD                       P4MOD
     I              PAAMT                       P4AMT
     I              PABR2                       P4BR2
     I              PABR7                       P4BR7
     I              PAFRDT                      P4FRDT
     I              PATODT                      P4TODT
     I              PAWKS                       P4WKS
     I              PADAYS                      P4DAYS
     I              PATRCD                      P4TRCD
     I              PATYPE                      P4TYPE
     I              PARECD                      P4RECD
     I              PARETP                      P4RETP
     I              PADESC                      P4DESC
     I              PAADST                      P4ADST
     I              PASTAT                      P4STAT
     I              PAUSER                      P4USER
     I              PAPGM                       P4PGM
     I              PATIME                      P4TIME
     I              PADT                        P4DT
     I              PABUL1                      P4BUL1
     I              PAPGTP                      P4PGTP
     I              PAEOB#                      P4EOB#
     I              PACKTP                      P4CKTP
     I              PAACCT                      P4ACCT
     I              PAOFF#                      P4OFF#
     I              PARCVD                      P4RCVD
     I              PADUE                       P4DUE
     I              PALTCD                      P4LTCD
     I              PADPCD                      P4DPCD
     I              PADPCU                      P4DPCU
     I              PARQST                      P4RQST
     I              PAFNAL                      P4FNAL
     I              PARCDT                      P4RCDT
     I              PARADJ                      P4RADJ
     I              PAEOBS                      P4EOBS
     I              PAXFYR                      P4XFYR
     I              PAXCAS                      P4XCAS
      *** END ADD ****** 05/25/07 **********************************

     C*
     C     *ENTRY        PLIST
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    X@FYR             3
     C                   PARM                    X@CASE            7
     C                   PARM                    X@OUT             1
     C                   PARM                    X@FRDT            8
     C                   PARM                    X@TODT            8
     C                   PARM                    X@TRCD            2
     C                   PARM                    X@TYPE            1
      ***** BEG DEL ******* 08/13/01 **********************************
     C*********            PARM           X@RECD  2
     C*********            PARM           X@RETP  1
      ***** END DEL ******* 08/13/01 **********************************
      ***** BEG ADD ******* 08/13/01 **********************************
     C                   PARM                    X@RCD1            2
     C                   PARM                    X@RCD2            2
     C                   PARM                    X@RCD3            2
     C                   PARM                    X@RCD4            2
     C                   PARM                    X@RCD5            2
     C                   PARM                    X@RTP1            1
     C                   PARM                    X@RTP2            1
     C                   PARM                    X@RTP3            1
     C                   PARM                    X@RTP4            1
      ***** END ADD ******* 08/13/01 **********************************
     C                   PARM                    QQQUE            10
     C                   PARM                    X@EMP#            9
     C                   PARM                    X@DIV             5
     C                   PARM                    X@BR2             2
     C                   PARM                    X@TCAS            7
      ***** BEG ADD ******* 03/11/10 **********************************
     C                   PARM                    X@SORT            1
      ***** END ADD ******* 03/11/10 **********************************
      ***** Beg Add ******* 02/08/23 **********************************
     C                   PARM                    X@LtSt            1
      ***** End Add ******* 02/08/23 **********************************
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*** Begin Chg *** 12/09/15 *****************************
     C*****DTCYMD        PLIST
     C     DTCYMD2       PLIST
     C*** End   Chg *** 12/09/15 *****************************
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** Begin Chg *** 12/09/15 *****************************
     C*****DTMDYY        PLIST
     C     DTMDYYYY      PLIST
     C*** End   Chg *** 12/09/15 *****************************
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** Begin Chg *** 12/09/15 *****************************
     C*****DTYYMD        PLIST
     C     DTYYYYMD      PLIST
     C*** End   Chg *** 12/09/15 *****************************
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDCY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*** Begin Add *** 12/09/15 *********************************************
      /Free
       // Determine if this module get spool file and/or XML output
       procType = 'Claim Payment History Report';
       isUseSpoolOut = UseSpoolOut(procType);
       isUseXML = UseScriptura(procType) Or
                  Use3rdXml(procType);
       if isUseXML;
         isScriptUsed = 'Y';
       else;
         isScriptUsed = 'N';
       Endif;
      ***** Beg ADD ***** 02/08/23 **************************************
       If Q1prms > 24;
          xxLtSt = X@LtSt;
       Else;
          xxLtSt = *Blanks;
       EndIf;
       If xxLtSt = ' ';
          X@LTI = '*ALL' ;
       Else;
          X@LTI = xxLtSt;
       EndIf;
      ***** End ADD ***** 02/08/23 **************************************
      /End-free
     C*** End   Add *** 12/09/15 ******************************************************
     C*                                                          "
     C*   SET-UP AND INITIALIZE                                  "
     C*                                                          "
     C     QQQUE         IFNE      ' '                                                "
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE                                                         "
     C                   SETON                                        08        BATCH MODE
     C                   END                                                          "
     C*
     C*  WORKERS COMP DEPARTMENT MASTER FILE
     C*
     C     DPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXDEPT
     C*
     C*   KEY LIST FOR OCCUPATION CODE FILE
     C*
     C     OCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE OFFICE MASTER FILE
     C*
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXOFC#
     C*
     C*    Employer Master
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR CLAIMANT DEMOGRAPHICS FILE.
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE REPORT OVERRIDE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR TABLE FILE - PAYMENT CODE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     TBKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR PAYEE DEMOGRAPHICS
     C*
     C     PDKEY         KLIST
     C                   KFLD                    PAFEIN            9 0
     C                   KFLD                    PAMOD             3 0
     C*
     C*  Payment Detail
     C*
     C     PAKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*** BEGIN DEL *** 03/13/98 **************************************
     C**********           KFLD           X@BR2
     C*** END   DEL *** 03/13/98 **************************************
     C*
     C     PAKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*** BEG ADD *** 02/22/07 ***************************
     C     PAKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PACHK#
     C*
     C     R2KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*
     C     RPKY0         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PACHK#
     C*** END ADD *** 02/22/07 ***************************
     C*
     C*  Claim Master
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C***  BEGIN DEL  ***  10/30/98  *********************************
     C**********           KFLD           XXCASE
     C***  END   DEL  ***  10/30/98  *********************************
     C*
     C     CMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C***  BEGIN ADD  ***  10/30/98  *********************************
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C***  END   ADD  ***  10/30/98  *********************************
     C                   KFLD                    XXFYR
     C*
     C     CMKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     CMKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** BEG ADD *** 08/14/03  *********************************
     C*
     C     CMKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C     CMKY6         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** BEG CHG *** 03/01/04 *********************************
     C***********          KFLD           XXFYR
     C***********          KFLD           XXEMP#
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C*** END CHG *** 03/01/04 *********************************
     C*** END ADD *** 08/14/03  *********************************
     C*** Begin add *** 01/28/04 **************************************
     C*  Key list for the View Check File
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    XXSEQ#
     C*** End add ***** 01/28/04 **************************************
     C*_______________________________________________________________
      ***** BEG ADD ******* 08/13/01 **********************************
     C                   MOVE      X@RCD1        XXRCD1
     C                   MOVE      X@RCD2        XXRCD2
     C                   MOVE      X@RCD3        XXRCD3
     C                   MOVE      X@RCD4        XXRCD4
     C                   MOVE      X@RCD5        XXRCD5
     C                   MOVE      X@RTP1        XXRTP1
     C                   MOVE      X@RTP2        XXRTP2
     C                   MOVE      X@RTP3        XXRTP3
     C                   MOVE      X@RTP4        XXRTP4
      ***** END ADD ******* 08/13/01 **********************************
     C*
     C*   Open output files to be used
     C*
     C     X@OUT         IFNE      'R'
     C                   OPEN      WFPHP
     C                   ENDIF                                                  X@OUT<>R
     C*
     C     X@OUT         IFNE      'F'
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout AndEQ     *On
     C*** End Add *** 12/09/15 ***************************************
     C                   OPEN      QSYSPRT
     C                   ENDIF                                                  X@OUT<>F
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C                   MOVE      X@CO#         XXCO#
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDCY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C     *LIKE         DEFINE    PAFYR         HLFYR
     C     *LIKE         DEFINE    PACASE        HLCASE
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMCASE        XXTCAS
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMDT          X7FRDT
     C     *LIKE         DEFINE    CMDT          X7TODT
      ***** BEG ADD ***** 12/09/02 **************************************
     C     *LIKE         DEFINE    CMNATR        HLNATR
      ***** END ADD ***** 12/09/02 **************************************
     C     *LIKE         DEFINE    PAOFF#        XXOFC#
     C     *LIKE         DEFINE    DPDEPT        XXDEPT
     C     *LIKE         DEFINE    OCKEY         XXKEY
     C     *LIKE         DEFINE    PAAMT         XPAMT
     C     *LIKE         DEFINE    *IN60         XXPRFL
     C     *LIKE         DEFINE    XXDATE        X8FRDT
     C     *LIKE         DEFINE    XXDATE        X8TODT
     C*** BEG ADD *** 02/22/07 ***************************
     C     *LIKE         DEFINE    PASEQ#        XBSEQ#
     C*** END ADD *** 02/22/07 ***************************
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C***  BEGIN ADD  ***  10/30/98  *********************************
     C*
     C*  Center Company name in display field
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        CONAME
     C***  END   ADD  ***  10/30/98  *********************************
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C                   MOVE      X@FND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C     FNANAM        COMP      *BLANK                             8888
      ***** BEG ADD ******* 08/13/01 **********************************
      *
      ***** BEG ADD ******* 03/11/10 **********************************
     C     X@SORT        IFEQ      'C'
     C                   MOVE      *ON           *IN89
     C                   ENDIF
      ***** END ADD ******* 03/11/10 **********************************
     C                   MOVEL(P)  FNANAM        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      50            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        FNANAM
      *
      ***** END ADD ******* 08/13/01 **********************************
     C*
     C*   SET THE CONDITIONING OUTPUT INDICATORS FOR THE PARMS.
     C*
     C                   MOVE      X@EMP#        XXEMP#
     C                   MOVE      X@DIV         XXDIV
     C*
     C     XXEMP#        IFEQ      *ZERO
     C     XXDIV         ANDEQ     *ZERO
     C                   MOVE      *ON           *IN77
     C                   ELSE
     C                   MOVE      *OFF          *IN77
     C                   ENDIF                                                  XXEMP#=*ZERO
     C*
     C     X@FRDT        COMP      '00000000'                             78
     C     X@FRDT        COMP      '00000000'                             78
     C     X@TODT        COMP      '00000000'                             79
     C     X@TRCD        COMP      *BLANK                                 80
     C     X@TYPE        COMP      *BLANK                                 81
      ***** BEG DEL ******* 08/13/01 **********************************
     C*********  X@RECD    COMP *BLANK                   82
     C*********  X@RETP    COMP *BLANK                   83
      ***** END DEL ******* 08/13/01 **********************************
      ***** BEG ADD ******* 08/13/01 **********************************
     C     XXRCD         COMP      *BLANKS                                82
     C     XXRTP         COMP      *BLANKS                                83
      ***** END ADD ******* 08/13/01 **********************************
     C     X@BR2         COMP      *BLANK                                 84
     C                   MOVE      *BLANKS       CD
     C                   MOVE      *BLANKS       DS
     C                   Z-ADD     0             AM
     C                   Z-ADD     0             AM3
     C                   Z-ADD     0             AM4
     C                   Z-ADD     0             WK4
     C                   Z-ADD     0             DA4
     C*
     C*   LOAD PAYMENT CODES INTO ARRAY
     C*
     C                   Z-ADD     1             X                 3 0
     C     TBKEY         CHAIN     PPC01                              50
     C     *IN50         DOWEQ     '0'
     C********** X         ANDLT100                        DEL 11/20/00
     C     X             ANDLT     200                                          ADD 11/20/00
     C                   MOVEL     PCKEY         CD(X)
     C                   MOVEL     PCDESC        DS(X)
     C                   ADD       1             X
     C     TBKEY         READE     PPC01                                  50
     C                   END
     C*
     C*   LOAD BR-2 CODES INTO ARRAY
     C*
     C                   MOVE      *BLANKS       CD2
     C                   MOVE      *BLANKS       DS2
     C                   Z-ADD     *ZERO         AM2
     C                   Z-ADD     *ZERO         WK2
     C                   Z-ADD     *ZERO         DA2
     C*
     C                   Z-ADD     1             X                 3 0
     C     TBKEY         CHAIN     PB201                              50
     C     *IN50         DOWEQ     '0'
     C********** X         ANDLT100                        DEL 11/20/00
     C     X             ANDLT     200                                          ADD 11/20/00
     C                   MOVEL     B2KEY         CD2(X)
     C                   MOVEL     B2DESC        DS2(X)
     C                   ADD       1             X
     C     TBKEY         READE     PB201                                  50
     C                   END
     C*
     C                   MOVE      X@FRDT        X8FRDT
     C                   MOVE      X@TODT        X8TODT
     C*
     C                   Z-ADD     X8FRDT        SRDTI
     C*** Begin Chg *** 12/09/15 *****************************
     C**********         CALL      'SRCVTDAT'    DTCYMD
     C                   CALL      'SRCVTDAT'    DTCYMD2
     C*** End   Chg *** 12/09/15 *****************************
     C                   MOVEL     SRDTO         X7FRDT
     C*
     C                   Z-ADD     X8TODT        SRDTI
     C*** Begin Chg *** 12/09/15 *****************************
     C**********         CALL      'SRCVTDAT'    DTCYMD
     C                   CALL      'SRCVTDAT'    DTCYMD2
     C*** End   Chg *** 12/09/15 *****************************
     C                   MOVEL     SRDTO         X7TODT
     C*
     C*   POSITION @ THE BEGINNING OF THE FILE FOR THIS CASE.
     C*
     C                   MOVE      X@FYR         XXFYR
     C                   MOVE      X@CASE        XXCASE
     C                   MOVE      *ON           *IN91
     C*
     C                   MOVE      X@TCAS        XXTCAS
     C*
     C*  Determine which Claim Master Generic Key to Use
     C*
     C***  BEGIN CHG  ***  10/30/98  *********************************
     C**********           SELEC
     C********** XXCASE    WHGT *ZERO
     C**********           MOVE *ON       *IN71            CMKY1
     C**********           MOVE *OFF      *IN91
     C********** XXFYR     WHGT *ZERO
     C**********           MOVE *ON       *IN72            CMKY2
     C********** XXEMP#    WHGT *ZERO
     C**********           MOVE *ON       *IN73            CMKY3
     C**********           OTHER
     C**********           MOVE *ON       *IN74            CMKY4
     C**********           ENDSL
     C                   SELECT
     C     XXEMP#        WHENGT    *ZERO
     C     XXFYR         ANDGT     *ZERO
     C*** BEG ADD *** 08/14/03  *********************************
     C     XXDIV         ANDGT     *ZERO
     C*** END ADD *** 08/14/03  *********************************
     C                   MOVE      *ON           *IN72                          CMKY2
     C*** BEG ADD *** 08/14/03  *********************************
     C     XXEMP#        WHENGT    *ZERO
     C     XXFYR         ANDGT     *ZERO
     C     XXDIV         ANDLE     *ZERO
     C                   MOVE      *ON           *IN76                          CMKY6
     C     XXEMP#        WHENGT    *ZERO
     C     XXFYR         ANDLE     *ZERO
     C     XXDIV         ANDLE     *ZERO
     C                   MOVE      *ON           *IN75                          CMKY5
     C*** END ADD *** 08/14/03  *********************************
     C     XXEMP#        WHENGT    *ZERO
     C                   MOVE      *ON           *IN73                          CMKY3
     C     XXFYR         WHENGT    *ZERO
     C                   MOVE      *ON           *IN71                          CMKY1
     C                   OTHER
     C                   MOVE      *ON           *IN74                          CMKY4
     C                   ENDSL
     C*
     C     XXCASE        IFGT      *ZERO
     C                   MOVE      *OFF          *IN91
     C                   ENDIF                                                  XXCASE>*ZERO
     C***  END   CHG  ***  10/30/98  *********************************
     C*** BEG ADD *** 03/01/04 ****************************
     C     XXCASE        IFGT      *ZERO
     C     XXTCAS        ORNE      9999999
     C                   MOVE      *ON           *IN51
     C                   ENDIF
     C*** END ADD *** 03/01/04 ****************************
     C*
     C*** Get the new claimant information
     C*
     C                   MOVE      *BLANKS       XXNAME           35
     C*
     C*   Get the Routing Distribution Information
     C*
     C     X@OUT         IFNE      'F'
     C*
     C                   MOVE      '0'           XXPRT             1
     C*
     C     ORKEY         CHAIN     POR01                              60
     C***  BEGIN ADD  ***  10/30/98  *********************************
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFEQ      *BLANKS                                      FIELD
     C                   MOVEL(P)  TITLE         ORTITL
     C                   ENDIF                                                  ORTITL=*BLANKS
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C*
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C*
     C     *IN60         IFEQ      *OFF
     C***  END   ADD  ***  10/30/98  *********************************
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C***  BEGIN ADD  ***  10/30/98  *********************************
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Center Text in Field
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        ORTITL
     C***  END   ADD  ***  10/30/98  *********************************
     C*
     C                   ENDIF                                                  X@OUT<>F     '0'
     C*** Beg Add *** 12/09/15 ***************************************
     C*
     C*** Write to Scriptura Payment History - Report Header
     C                   If        isScriptUsed = 'Y'
     C                   ExSr      GetXmlFileNm
     C                   Exsr      wrtSXPHPRPT
     C                   Endif
     C*** End Add *** 12/09/15 ***************************************
     C*
     C                   MOVE      '0'           *INOF
     C                   MOVE      '0'           *IN12
     C                   MOVE      '0'           *IN13
     C*
     C*  Position in Claim Master based on Generic key
     C   71CMKY1         CHAIN     PCM02                              61
     C***  BEGIN CHG  ***  10/30/98  *********************************
     C***72***** CMKY2     CHAINPCM02                61
     C   72CMKY2         CHAIN     PCM01                              61
     C***  END   CHG  ***  10/30/98  *********************************
     C   73CMKY3         CHAIN     PCM01                              61
     C   74CMKY4         CHAIN     PCM01                              61
     C*** BEG ADD *** 08/14/03  *********************************
     C   75CMKY5         CHAIN     PCM01                              61
     C   76CMKY6         CHAIN     PCM03                              61
     C*** END ADD *** 08/14/03  *********************************
     C*
     C***  BEGIN ADD  ***  10/30/98  *********************************
     C     *IN61         DOWEQ     *OFF
     C     CMCASE        ANDLT     XXCASE
     C*
     C   71CMKY1         READE     PCM02                                  61
     C   72CMKY2         READE     PCM01                                  61
     C   73CMKY3         READE     PCM01                                  61
     C   74CMKY4         READE     PCM01                                  61
     C*** BEG ADD *** 08/14/03  *********************************
     C   75CMKY5         READE     PCM01                                  61
     C   76CMKY6         READE     PCM03                                  61
     C*** END ADD *** 08/14/03  *********************************
     C*
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C     *IN61         IFEQ      *OFF
     C     CMCASE        ANDGT     XXTCAS
     C                   MOVE      *ON           *IN61
     C                   ENDIF                                                  *IN61=*OFF
     C***  END   ADD  ***  10/30/98  *********************************
     C*
     C     *IN61         IFEQ      '0'
     C*
     C*     Get employer master
     C     CMEMP#        IFNE      SVEMP#
     C     CMDIV         ORNE      SVDIV
     C     EMKY          CHAIN     PEM01                              67
     C     *IN67         IFEQ      *ON
     C                   MOVEL     *BLANKS       EMNAME
     C                   ENDIF                                                               '0'
     C                   Z-ADD     CMEMP#        SVEMP#            9 0
     C                   Z-ADD     CMDIV         SVDIV             5 0
     C                   ENDIF                                                               '0'
     C*
     C     CDKEY         CHAIN     PCD01                              43
     C     *IN43         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLNAM           56
     C                   MOVEL     CDFSN         QQFNAM           15
     C                   MOVEL     ' '           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        XXNAME
     C                   END                                                    END *IN43 EQ '0'
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVEL(P)  XXATTX        XXNAME
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C                   END                                                    END *IN61 EQ '0'
     C*
     C*** Set to the new case#
     C*
     C                   Z-ADD     CMFYR         HLFYR
     C                   MOVE      CMFYR         HLYEAR            3
     C                   Z-ADD     CMCASE        HLCASE
     C*** BEG DEL *** 03/01/04 ****************************
     C**********           MOVE CMCASE    XXCASE
     C*** END DEL *** 03/01/04 ****************************
     C*
      ***** BEG ADD ***** 12/09/02 **************************************
     C*  Get NATURE Description
     C                   MOVE      CMNATR        HLNATR
     C                   MOVEL(P)  HLNATR        XXKEY
     C     TBKY2         CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  NADESC        HLNDES           30
     C                   ELSE
     C                   MOVEL     'NO DESC'     HLNDES
     C                   ENDIF
      ***** END ADD ***** 12/09/02 **************************************
     C*
     C*
     C     *IN61         DOWEQ     *OFF
     C*** Beg Add *** 12/09/15 ***************************************
      ***** Beg ADD ***** 02/08/23 **************************************
            If xxLtSt = *blank or xxLtSt = CMltFl ;
      ***** End ADD ***** 02/08/23 **************************************
     C*
     C*** Write to Scriptura Payment History - Claim Header
     C                   If        isScriptUsed = 'Y'
     C                   Exsr      wrtSXPHPCLM
     C                   Endif
     C*** End Add *** 12/09/15 ***************************************
     C*
     C*  Position in Payment Detail based on Claim & Payment Value 1
      ***** BEG ADD ******* 03/11/10 **********************************
     C*
     C     X@SORT        IFEQ      'C'
     C  N84PAKY4         CHAIN     PPA05                              60
     C   84PAKY5         CHAIN     PPA05                              60
     C                   ELSE
      ***** END ADD ******* 03/11/10 **********************************
     C  N84PAKY4         CHAIN     PPA03                              60
     C   84PAKY5         CHAIN     PPA03                              60
      ***** BEG ADD ******* 03/11/10 **********************************
     C                   ENDIF
      ***** END ADD ******* 03/11/10 **********************************
     C*
     C*
     C*== Begin the MAINLINE ==========================================
     C     *IN60         DOWEQ     '0'
     C*
     C***  BEGIN ADD  ***  03/31/98  *********************************
     C*
     C*   THE PAYMENT TRANSACTION DATE MUST BE GREATER THAN OR EQUAL.
     C*
     C     X7FRDT        IFGT      *ZERO
     C     PATRDT        CABLT     X7FRDT        NEXT
     C                   ENDIF                                                  X7FRDT>*ZERO
     C*
     C*   THE PAYMENT TRANSACTION DATE MUST BE LESS THAN OR EQUAL.
     C*
     C     X7TODT        IFGT      *ZERO
     C     PATRDT        CABGT     X7TODT        NEXT
     C                   ENDIF                                                  X7TODT>*ZERO
     C*
     C*   THE PAYMENT CODE MUST BE EQUAL IF NOT BLANK.
     C*
     C     X@TRCD        IFNE      *BLANK
     C     PATRCD        CABNE     X@TRCD        NEXT
     C                   ENDIF                                                  X@TRCD<>*BLANK
     C*
     C*   THE PAYMENT TYPE MUST BE EQUAL IF NOT BLANK.
     C*
     C     X@TYPE        IFNE      *BLANK
     C     PATYPE        CABNE     X@TYPE        NEXT
     C                   ENDIF                                                  X@TYPE<>*BLANK
      ***** BEG DEL ******* 08/13/01 **********************************
     C**********
     C****THE*RESERVE CODE MUST BE EQUAL IF NOT BLANK.
     C**********
     C********** X@RECD    IFNE *BLANK
     C********** PARECD    CABNEX@RECD    NEXT
     C**********           ENDIF                           X@RECD<>*BLANK
     C**********
     C****THE*RESERVE TYPE MUST BE EQUAL IF NOT BLANK.
     C**********
     C********** X@RETP    IFNE *BLANK
     C********** PARETP    CABNEX@RETP    NEXT
     C**********           ENDIF                           X@RETP<>*BLANK
      ***** END DEL ******* 08/13/01 **********************************
      ***** BEG ADD ******* 08/13/01 **********************************
      *
      *   THE RESERVE CODE MUST BE EQUAL IF NOT BLANK.
      *
     C     XXRCD         IFNE      *BLANKS
     C     PARECD        SCAN      XXRCD         FOUND             3 0    85
     C  N85              GOTO      NEXT
     C                   ENDIF
      *
      *   THE RESERVE TYPE MUST BE EQUAL IF NOT BLANK.
      *
     C     XXRTP         IFNE      *BLANKS
     C     PARETP        SCAN      XXRTP         FOUND             3 0    85
     C  N85              GOTO      NEXT
     C                   ENDIF
      *
      ***** END ADD ******* 08/13/01 **********************************
     C***  END   ADD  ***  03/31/98  *********************************
     C***  BEGIN ADD  ***  04/03/98  *********************************
     C*
     C*  THE PAY VAL 1 CODE MUST BE EQUAL IF NOT BLANK.
     C*
     C     X@BR2         IFNE      *BLANK
     C     PABR2         CABNE     X@BR2         NEXT
     C                   ENDIF                                                  X@BR2<>*BLANK
     C***  END   ADD  ***  04/03/98  *********************************
     C*
     C                   ADD       1             NBRCLM            5 0
     C*
     C*** If Case changes, get Claimant Information again
     C*
     C     CMFYR         IFNE      HLFYR
     C     CMCASE        ORNE      HLCASE
     C*
     C     RCDS          IFGT      0
     C*
     C*** Accumulate the Case Totals
     C                   XFOOT     AM            XXTOTL           11 2
     C*
     C*** Print the "old" case totals
     C     X@OUT         IFNE      'F'
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout IfEQ      *On
     C*** End Add *** 12/09/15 ***************************************
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   EXCEPT    TOTL
     C*** Beg Add *** 12/09/15 ***************************************
     C                   EndIf
     C*** End Add *** 12/09/15 ***************************************
     C*
     C*
     C                   MOVE      '1'           *INOF
     C                   MOVE      '1'           *IN12
     C                   Z-ADD     1             X
     C********** X         DOWLT101                        DEL 11/20/00
     C     X             DOWLT     201                                          ADD 11/20/00
     C     CD(X)         ANDNE     *BLANK
     C*
     C     AM(X)         IFNE      0
     C                   MOVE      CD(X)         XXCDE             2
     C                   MOVE      *BLANKS       XXDESC           25
     C                   MOVEL     DS(X)         XXDESC
     C                   Z-ADD     AM(X)         XXAMT            11 2
     C*** Beg Add *** 12/09/15 ***************************************
     C*
     C*** Write to Scriptura Payment History - Claim Summary
     C                   If        isScriptUsed = 'Y'
     C                   Eval      isTrCd = *On
     C                   Exsr      wrtSXPHPCLMT
     C                   Endif
     C*** End Add *** 12/09/15 ***************************************
     C     *INOF         IFEQ      '1'
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout AndEQ     *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    SUMH
     C                   MOVE      '0'           *INOF
     C                   END
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout IfEQ      *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    SUM
     C*** Beg Add *** 12/09/15 ***************************************
     C                   EndIf
     C*** End Add *** 12/09/15 ***************************************
     C                   END
     C*
     C                   ADD       1             X
     C                   END
     C*
     C*   PRINT BR-2 CODE SUMMARY TABLE
     C     *INOF         IFEQ      '1'
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout AndEQ     *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout IfEQ      *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    SUMH2
     C*** Beg Add *** 12/09/15 ***************************************
     C                   EndIf
     C*** End Add *** 12/09/15 ***************************************
     C*
     C                   Z-ADD     1             X
     C********** X         DOWLT101                        DEL 11/20/00
     C     X             DOWLT     201                                          ADD 11/20/00
     C     CD2(X)        ANDNE     *BLANK
     C*
     C     AM2(X)        IFNE      0
     C                   MOVE      CD2(X)        XXCDE             2
     C                   MOVE      *BLANKS       XXDESC           25
     C                   MOVEL     DS2(X)        XXDESC
     C                   Z-ADD     AM2(X)        XXAMT            11 2
     C                   Z-ADD     WK2(X)        XXWKS             4 0
     C                   Z-ADD     DA2(X)        XXDYS             1 0
     C*** Beg Add *** 12/09/15 ***************************************
     C*
     C*** Write to Scriptura Payment History - Claim Summary
     C                   If        isScriptUsed = 'Y'
     C                   Eval      isTrCd = *Off
     C                   Exsr      wrtSXPHPCLMT
     C                   Endif
     C*** End Add *** 12/09/15 ***************************************
     C     *INOF         IFEQ      '1'
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout AndEQ     *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    SUMH2
     C                   MOVE      '0'           *INOF
     C                   END
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout IfEQ      *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    SUM2
     C*** Beg Add *** 12/09/15 ***************************************
     C                   EndIf
     C*** End Add *** 12/09/15 ***************************************
     C                   END
     C*
     C                   ADD       1             X
     C                   END                                                    DO X <= 100
     C*
     C                   END
     C                   ENDIF                                                  OUT ne F
     C*
     C*** Get the new claimant information
     C                   MOVE      *BLANKS       XXNAME           35
     C*
     C     CDKEY         CHAIN     PCD01                              43
     C     *IN43         IFEQ      '0'
     C                   MOVE      *BLANK        QQLNAM
     C                   MOVE      *BLANK        QQFNAM
     C                   MOVEL     CDLSN         QQLNAM           56
     C                   MOVEL     CDFSN         QQFNAM           15
     C                   MOVEL     ' '           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        XXNAME
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVEL(P)  XXATTX        XXNAME
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
      *
     C                   END                                                    END *IN43 EQ '0'
     C*
     C*** Set to the new case#
     C                   Z-ADD     CMFYR         HLFYR
     C                   MOVE      CMFYR         HLYEAR            3
     C                   Z-ADD     CMCASE        HLCASE
     C*
      ***** BEG ADD ***** 12/09/02 **************************************
     C*  Get NATURE Description
     C                   MOVE      CMNATR        HLNATR
     C                   MOVEL(P)  HLNATR        XXKEY
     C     TBKY2         CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  NADESC        HLNDES
     C                   ELSE
     C                   MOVEL     'NO DESC'     HLNDES
     C                   ENDIF
      ***** END ADD ***** 12/09/02 **************************************
     C*
     C                   MOVE      '0'           *IN12
     C                   MOVE      '0'           *IN13
     C                   ADD       AM            AM3
     C                   ADD       AM2           AM4
     C                   ADD       WK2           WK4
     C                   ADD       DA2           DA4
     C*
     C*** Initialize variables/arrays for new case totals
     C                   Z-ADD     *ZERO         RCDS
     C                   Z-ADD     *ZERO         AM
     C                   Z-ADD     *ZERO         AM2
     C                   Z-ADD     *ZERO         WK2
     C                   Z-ADD     *ZERO         DA2
     C*
     C                   END                                                    END NE HLFYR/HLCASE
     C*
     C     LOOP          TAG
     C***  BEGIN DEL  ***  03/31/98  *********************************
     C**********
     C****THE*PAYMENT TRANSACTION DATE MUST BE GREATER THAN OR EQUAL.
     C**********
     C********** X7FRDT    IFGT *ZERO
     C********** PATRDT    CABLTX7FRDT    NEXT
     C**********           END
     C**********
     C****THE*PAYMENT TRANSACTION DATE MUST BE LESS THAN OR EQUAL.
     C**********
     C********** X7TODT    IFGT *ZERO
     C********** PATRDT    CABGTX7TODT    NEXT
     C**********           END
     C**********
     C****THE*PAYMENT CODE MUST BE EQUAL IF NOT BLANK.
     C**********
     C********** X@TRCD    IFNE *BLANK
     C********** PATRCD    CABNEX@TRCD    NEXT
     C**********           END
     C**********
     C****THE*PAYMENT TYPE MUST BE EQUAL IF NOT BLANK.
     C**********
     C********** X@TYPE    IFNE *BLANK
     C********** PATYPE    CABNEX@TYPE    NEXT
     C**********           END
     C**********
     C****THE*RESERVE CODE MUST BE EQUAL IF NOT BLANK.
     C**********
     C********** X@RECD    IFNE *BLANK
     C********** PARECD    CABNEX@RECD    NEXT
     C**********           END
     C**********
     C****THE*RESERVE TYPE MUST BE EQUAL IF NOT BLANK.
     C**********
     C********** X@RETP    IFNE *BLANK
     C********** PARETP    CABNEX@RETP    NEXT
     C**********           END
     C***  END   DEL  ***  03/31/98  *********************************
     C*
     C*
     C*   ACCUMULATE AMOUNTS FOR EACH PAYMENT CODE
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C     PATYPE        OREQ      'Z'
     C*
     C*   SET UP AMOUNT FOR DETAIL PRINT
     C                   Z-ADD     PAAMT         XPAMT
     C*
     C                   Z-ADD     1             X                 3 0
     C     PATRCD        LOOKUP    CD(X)                                  60
     C     *IN60         IFEQ      '1'
     C                   ADD       PAAMT         AM(X)
     C                   END
     C*
     C*   ACCUMULATE AMOUNTS FOR EACH BR-2 CODE
     C*
     C                   Z-ADD     1             X
     C     PABR2         LOOKUP    CD2(X)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       PAAMT         AM2(X)
     C                   END
     C*
     C                   EXSR      SRWKDY                                       ACCUM WKS,DAYS
     C*
     C                   ELSE                                                   PATYPE= V, S, R
     C*
     C*  IF VOID, STOP, OR REVERSE, APPLY AMT TO CORRECT PAY CODE BUCKET
     C*
     C*   SET UP AMOUNT FOR DETAIL PRINT
     C                   Z-SUB     PAAMT         XPAMT
     C*
     C                   Z-ADD     1             X
     C     PATRCD        LOOKUP    CD(X)                                  60
     C     *IN60         IFEQ      '1'
     C                   SUB       PAAMT         AM(X)
     C                   END
     C*
     C*   IF VOID, STOP, OR REVERSE, APPLY AMT TO CORRECT BR-2 BUCKET
     C*
     C                   Z-ADD     1             X
     C     PABR2         LOOKUP    CD2(X)                                 60
     C     *IN60         IFEQ      '1'
     C                   SUB       PAAMT         AM2(X)
     C                   END
     C*
     C                   EXSR      SRWKDY
     C*
     C                   END                                                    PATYPE
     C*
     C*   CLEAR AMOUNT
     C*** BEGIN DLT *** 05/13/98 **************************************
     C******************** Z-ADD0         PAAMT
     C*** END DLT ***** 05/13/98 **************************************
     C*
     C*   CONVERT THE DATES TO MM/DD/YY FORMAT.
     C*
     C                   Z-ADD     PATRDT        SRDTI
     C*** Begin Chg *** 12/09/15 *****************************
     C**********         CALL      'SRCVTDAT'    DTMDYY
     C                   CALL      'SRCVTDAT'    DTMDYYYY
     C*** End   Chg *** 12/09/15 *****************************
     C                   MOVEL     SRDTO         MMTRDT            8 0
      ***** BEG CHG ***** 12/15/06 ***************************************
     C     PARQDT        IFNE      *ZEROS
     C                   Z-ADD     PARQDT        SRDTI
     C                   ELSE
     C                   Z-ADD     PAPODT        SRDTI
     C                   ENDIF
     C**************       Z-ADDPAPODT    SRDTI
      ***** END CHG ***** 12/15/06 ***************************************
     C*** Begin Chg *** 12/09/15 *****************************
     C**********         CALL      'SRCVTDAT'    DTMDYY
     C                   CALL      'SRCVTDAT'    DTMDYYYY
     C*** End   Chg *** 12/09/15 *****************************
     C                   MOVEL     SRDTO         MMPODT            8 0
     C                   Z-ADD     PAFRDT        SRDTI
     C*** Begin Chg *** 12/09/15 *****************************
     C**********         CALL      'SRCVTDAT'    DTMDYY
     C                   CALL      'SRCVTDAT'    DTMDYYYY
     C*** End   Chg *** 12/09/15 *****************************
     C                   MOVEL     SRDTO         MMFRDT            8 0
     C                   Z-ADD     PATODT        SRDTI
     C*** Begin Chg *** 12/09/15 *****************************
     C**********         CALL      'SRCVTDAT'    DTMDYY
     C                   CALL      'SRCVTDAT'    DTMDYYYY
     C*** End   Chg *** 12/09/15 *****************************
     C                   MOVEL     SRDTO         MMTODT            8 0
     C*
     C                   ADD       1             RCDS              5 0
     C*
     C*  GET THE PAYEE NAME
     C*
     C                   MOVE      *BLANKS       XXPYNM           30
     C*
     C*** Begin add *** 01/28/04 **************************************
     C                   Z-ADD     PASEQ#        XXSEQ#
     C     RPKY          CHAIN     PRP01                              61
     C*** BEG ADD *** 02/22/07 ***************************
     C  N61RPCHK#        COMP      PACHK#                             6161
     C*** END ADD *** 02/22/07 ***************************
     C*
     C     *IN61         IFEQ      *ON
     C*** BEG ADD *** 02/22/07 ***************************
     C     PABASQ        ANDNE     *ZEROS
     C*** END ADD *** 02/22/07 ***************************
     C                   Z-ADD     PABASQ        XXSEQ#
     C     RPKY          CHAIN     PRP01                              61
     C*** BEG ADD *** 02/22/07 ***************************
     C  N61RPCHK#        COMP      PACHK#                             6161
     C*** END ADD *** 02/22/07 ***************************
     C                   ENDIF
     C*
     C*** BEG ADD *** 02/22/07 ***************************
     C     *IN61         IFEQ      *ON
     C*
     C     PACHK#        IFNE      *ZEROS
     C     PABASQ        ANDNE     *ZEROS
     C*
     C*** Setup the sequence # to match
     C                   Z-ADD     PABASQ        XBSEQ#
     C*
     C*** Read payments for FND/CHK#
     C     PAKY          CHAIN     PPA04                              62
     C     *IN62         DOWEQ     *OFF
     C*
     C*** Match found
     C     XBSEQ#        IFEQ      P4SEQ#
     C                   Z-ADD     P4BASQ        XXSEQ#
     C*
     C*** Try for RP rcd.
     C     R2KY          CHAIN     PRP01                              61
     C  N61RPCHK#        COMP      PACHK#                             6161
     C*
     C*** Not found, reset and look for the next base seq#
     C     *IN61         IFEQ      *ON
     C                   Z-ADD     P4BASQ        XBSEQ#
     C*
     C** If this is the original rcd and still not found,
     C** leave to prevent endless loop.
     C     XBSEQ#        IFEQ      *ZEROS
     C                   LEAVE
     C                   ENDIF
     C*
     C     PAKY          READE     PPA04                                  62
     C                   ITER
     C*** Found RCD, leave
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C     PAKY          READE     PPA04                                  62
     C                   ENDDO
     C*
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 02/22/07 ***************************
     C*
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     RPPNAM        XXPYNM
     C                   ELSE
     C*** End add ***** 01/28/04 **************************************
      *
     C     PDKEY         CHAIN     PPD01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVE      *BLANK        QQLAST
     C                   MOVE      *BLANK        QQFRST
     C                   MOVEL     PDNAME        QQLAST           56
     C                   MOVEL     PDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXPYNM
     C*** BEG ADD *** 02/22/07 ***************************
     C                   ELSE
     C     CMFSN         CAT(P)    CMMID:1       XXCNAM           15
     C     XXCNAM        CAT       '.':0         XXCNAM
     C                   MOVEL(P)  CMLSN         QQLAST           56
     C                   MOVEL(P)  XXCNAM        QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXPYNM
     C                   END
     C*** END ADD *** 02/22/07 ***************************
     C*** BEG DEL *** 02/22/07 ***************************
     C**********           END
     C*** END DEL *** 02/22/07 ***************************
      *
     C*** Begin add *** 01/28/04 **************************************
     C                   ENDIF
     C*** End add ***** 01/28/04 **************************************
      *
     C*
     C*   Build and write the Workfile
     C*
     C     X@OUT         IFNE      'R'
     C                   EXSR      SRWKFL
     C                   ENDIF
     C*** Beg Add *** 12/09/15 ***************************************
     C*
     C*** Write to Scriptura Payment History - Claim Detail
     C                   If        isScriptUsed = 'Y'
     C                   Exsr      wrtSXPHPDTL
     C                   Endif
     C*** End Add *** 12/09/15 ***************************************
     C*
     C*   PRINT HEADER IF NOT YET PRINTED
     C*
     C     X@OUT         IFNE      'F'
     C*
     C     *IN13         IFEQ      '0'
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout AndEQ     *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    HEAD
     C                   MOVE      '1'           *IN13
     C                   END
     C*
     C*   PRINT THE HEADINGS IF OVERFLOW IS ON.
     C*
     C     *INOF         IFEQ      '1'
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout AndEQ     *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C*   PRINT THE DETAIL LINE ITEM.
     C*
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout IfEQ      *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    DET
     C*** Beg Add *** 12/09/15 ***************************************
     C                   EndIf
     C*** End Add *** 12/09/15 ***************************************
     C                   ENDIF                                                    OUT ne F
     C*
     C*** READ THE NEXT PAYMENTS RECORD
     C     NEXT          TAG
     C*
     C*
      ***** BEG ADD ******* 03/11/10 **********************************
     C*
     C     X@SORT        IFEQ      'C'
     C  N84PAKY4         READE     PPA05                                  60
     C   84PAKY5         READE     PPA05                                  60
     C                   ELSE
      ***** END ADD ******* 03/11/10 **********************************
     C  N84PAKY4         READE     PPA03                                  60
     C   84PAKY5         READE     PPA03                                  60
      ***** BEG ADD ******* 03/11/10 **********************************
     C                   ENDIF
      ***** END ADD ******* 03/11/10 **********************************
     C*
     C                   ENDDO                                                  *IN60
     C*
      ***** Beg Add ******* 02/08/23 **********************************
          EndIf;
      ***** Beg Add ******* 02/08/23 **********************************
     C*  Read Next Claim Master record
     C*
     C***  BEGIN CHG  ***  10/30/98  *********************************
     C***71***** CMKY2     READEPCM02                    61
     C***72***** CMKY2     READEPCM02                    61
     C   71CMKY1         READE     PCM02                                  61
     C   72CMKY2         READE     PCM01                                  61
     C***  END   CHG  ***  10/30/98  *********************************
     C   73CMKY3         READE     PCM01                                  61
     C   74CMKY4         READE     PCM01                                  61
     C*** BEG ADD *** 08/14/03  *********************************
     C   75CMKY5         READE     PCM01                                  61
     C   76CMKY6         READE     PCM03                                  61
     C*** END ADD *** 08/14/03  *********************************
     C*
     C     *IN61         IFEQ      '0'
     C     CMCASE        ANDGT     XXTCAS
     C                   MOVE      *ON           *IN61
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN61
     C*
     C*   PRINT THE REPORT TOTALS
     C*
     C     X@OUT         IFNE      'F'
     C*
     C*          NBRCLM    IFEQ 0
     C     RCDS          IFEQ      0
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout IfEQ      *On
     C*** End Add *** 12/09/15 ***************************************
     C                   MOVE      '1'           *IN17                          Suppress ClmM
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *IN17
     C                   EXCEPT    NOCLM
     C*** Beg Add *** 12/09/15 ***************************************
     C                   EndIf
     C*** End Add *** 12/09/15 ***************************************
     C*
     C                   ELSE
     C*    ACCUMULATE THE CASE TOTALS.
     C*
     C                   XFOOT     AM            XXTOTL           11 2
     C*
     C*   PRINT THE HEADINGS IF OVERFLOW IS ON.
     C*
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout IfEQ      *On
     C*** End Add *** 12/09/15 ***************************************
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   EXCEPT    TOTL
     C*** Beg Add *** 12/09/15 ***************************************
     C                   EndIf
     C*** End Add *** 12/09/15 ***************************************
     C*
     C     RCDS          IFGT      0
     C*
     C                   MOVE      '1'           *INOF
     C                   MOVE      '1'           *IN12
     C                   Z-ADD     1             X
     C********** X         DOWLT101                        DEL 11/20/00
     C     X             DOWLT     201                                          ADD 11/20/00
     C     CD(X)         ANDNE     *BLANK
     C*
     C     AM(X)         IFNE      0
     C                   MOVE      CD(X)         XXCDE             2
     C                   MOVE      *BLANKS       XXDESC           25
     C                   MOVEL     DS(X)         XXDESC
     C                   Z-ADD     AM(X)         XXAMT            11 2
     C*** Beg Add *** 12/09/15 ***************************************
     C*
     C*** Write to Scriptura Payment History - Claim Summary
     C                   If        isScriptUsed = 'Y'
     C                   Eval      isTrCd = *On
     C                   Exsr      wrtSXPHPCLMT
     C                   Endif
     C*** End Add *** 12/09/15 ***************************************
     C     *INOF         IFEQ      '1'
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout AndEQ     *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    SUMH
     C                   MOVE      '0'           *INOF
     C                   END
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout IfEQ      *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    SUM
     C*** Beg Add *** 12/09/15 ***************************************
     C                   EndIf
     C*** End Add *** 12/09/15 ***************************************
     C                   END
     C*
     C                   ADD       1             X
     C                   END
     C*
     C*
     C*   PRINT BR-2 CODE SUMMARY TABLE
     C*
     C     *INOF         IFEQ      '1'
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout AndEQ     *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout IfEQ      *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    SUMH2
     C*** Beg Add *** 12/09/15 ***************************************
     C                   EndIf
     C*** End Add *** 12/09/15 ***************************************
     C*
     C                   Z-ADD     1             X
     C********** X         DOWLT101                        DEL 11/20/00
     C     X             DOWLT     201                                          ADD 11/20/00
     C     CD2(X)        ANDNE     *BLANK
     C*
     C     AM2(X)        IFNE      0
     C                   MOVE      CD2(X)        XXCDE             2
     C                   MOVE      *BLANKS       XXDESC           25
     C                   MOVEL     DS2(X)        XXDESC
     C                   Z-ADD     AM2(X)        XXAMT            11 2
     C                   Z-ADD     WK2(X)        XXWKS             4 0
     C                   Z-ADD     DA2(X)        XXDYS             1 0
     C*** Beg Add *** 12/09/15 ***************************************
     C*
     C*** Write to Scriptura Payment History - Claim Summary
     C                   If        isScriptUsed = 'Y'
     C                   Eval      isTrCd = *Off
     C                   Exsr      wrtSXPHPCLMT
     C                   Endif
     C*** End Add *** 12/09/15 ***************************************
     C     *INOF         IFEQ      '1'
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout AndEQ     *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    SUMH2
     C                   MOVE      '0'           *INOF
     C                   END
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout IfEQ      *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    SUM2
     C*** Beg Add *** 12/09/15 ***************************************
     C                   EndIf
     C*** End Add *** 12/09/15 ***************************************
     C                   END
     C*
     C                   ADD       1             X
     C                   END                                                    DO X <= 100
     C*
     C                   END
     C*
     C*   IF RUN FOR ALL CASES, ACCUMULATE GRAND TOTAL (FUND LEVEL)
     C*
     C                   ADD       AM            AM3
     C                   ADD       AM2           AM4
     C                   ADD       WK2           WK4
     C                   ADD       DA2           DA4
     C                   XFOOT     AM3           XXGTOT           13 2
     C*
     C*   PRINT GRAND SUMMARY BREAKDOWN OF PAYMENT TYPE CODES
     C*
     C                   MOVE      '1'           *INOF
     C                   Z-ADD     1             X
     C********** X         DOWLT101                        DEL 11/20/00
     C     X             DOWLT     201                                          ADD 11/20/00
     C     CD(X)         ANDNE     *BLANK
     C*
     C     AM3(X)        IFNE      0
     C                   MOVE      CD(X)         XXCDE             2
     C                   MOVE      *BLANKS       XXDESC           25
     C                   MOVEL     DS(X)         XXDESC
     C                   Z-ADD     AM3(X)        XXAMT            11 2
     C*** Beg Add *** 12/09/15 ***************************************
     C*
     C*** Write to Scriptura Payment History - Claim Summary
     C                   If        isScriptUsed = 'Y'
     C                   Eval      isTrCd = *On
     C                   Exsr      wrtSXPHPRPTT
     C                   Endif
     C*** End Add *** 12/09/15 ***************************************
     C     *INOF         IFEQ      '1'
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout AndEQ     *On
     C*** End Add *** 12/09/15 ***************************************
     C                   MOVE      '1'           *IN17                          Suppress ClmM
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *IN17
     C                   EXCEPT    SUMH3
     C                   MOVE      '0'           *INOF
     C                   END
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout IfEQ      *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    SUM3
     C*** Beg Add *** 12/09/15 ***************************************
     C                   EndIf
     C*** End Add *** 12/09/15 ***************************************
     C                   END
     C*
     C                   ADD       1             X
     C                   END
     C*
     C*   PRINT GRAND SUMMARY BREAKDOWN OF PAYMENT VALUE 1 CODES
     C*
     C     *INOF         IFEQ      '1'
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout AndEQ     *On
     C*** End Add *** 12/09/15 ***************************************
     C                   MOVE      '1'           *IN17                          Suppress ClmM
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *IN17
     C                   MOVE      '0'           *INOF
     C                   END
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout IfEQ      *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    SUMH4
     C*** Beg Add *** 12/09/15 ***************************************
     C                   EndIf
     C*** End Add *** 12/09/15 ***************************************
     C*
     C                   Z-ADD     1             X
     C********** X         DOWLT101                        DEL 11/20/00
     C     X             DOWLT     201                                          ADD 11/20/00
     C     CD2(X)        ANDNE     *BLANK
     C*
     C     AM4(X)        IFNE      0
     C                   MOVE      CD2(X)        XXCDE             2
     C                   MOVE      *BLANKS       XXDESC           25
     C                   MOVEL     DS2(X)        XXDESC
     C                   Z-ADD     AM4(X)        XXAMT            11 2
     C                   Z-ADD     WK4(X)        XXWKS             4 0
     C                   Z-ADD     DA4(X)        XXDYS             1 0
     C*** Beg Add *** 12/09/15 ***************************************
     C*
     C*** Write to Scriptura Payment History - Claim Summary
     C                   If        isScriptUsed = 'Y'
     C                   Eval      isTrCd = *Off
     C                   Exsr      wrtSXPHPRPTT
     C                   Endif
     C*** End Add *** 12/09/15 ***************************************
     C     *INOF         IFEQ      '1'
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout AndEQ     *On
     C*** End Add *** 12/09/15 ***************************************
     C                   MOVE      '1'           *IN17                          Suppress ClmM
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *IN17
     C                   EXCEPT    SUMH4
     C                   MOVE      '0'           *INOF
     C                   END
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout IfEQ      *On
     C*** End Add *** 12/09/15 ***************************************
     C                   EXCEPT    SUM4
     C*** Beg Add *** 12/09/15 ***************************************
     C                   EndIf
     C*** End Add *** 12/09/15 ***************************************
     C                   END
     C*
     C                   ADD       1             X
     C                   END                                                    DO X <= 100
     C*
     C*** Beg Add *** 12/09/15 ***************************************
     C     isUseSpoolout IfEQ      *On
     C*** End Add *** 12/09/15 ***************************************
     C                   MOVE      '1'           *IN17                          SUPPRESS
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *IN17
     C                   EXCEPT    GTOT                                         PRINT GRAND TOTAL
     C*** Beg Add *** 12/09/15 ***************************************
     C                   EndIf
     C*** End Add *** 12/09/15 ***************************************
     C                   ENDIF                                                  NBRCLM>0
     C*
     C*
     C*   Close open files
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If %open(qsysprt);
       xxPrt = '0';
       Chain orKey POR01;
          //*** Beg Chg *** 12/09/15 ***************************************
          //********If %found;
          If %Found And isUseSpoolout;
          //*** End Chg *** 12/09/15 ***************************************
             Except Route;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   CLOSE     QSYSPRT
     C                   ENDIF                                                    OUT ne F
     C*
     C     X@OUT         IFNE      'R'
     C                   CLOSE     WFPHP
     C                   ENDIF
     C*
     C*   END THE JOB
     C*
     C                   MOVE      '1'           *INLR
     C*
     C****************************************************
     C*   SUBROUTINE CALCULATE WEEKS AND DAYS            *
     C****************************************************
     C     SRWKDY        BEGSR
     C*
     C*        CALCULATE WEEKS & DAYS
     C*
     C     PATYPE        IFEQ      'P'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'Z'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'A'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'M'
     C     PAAMT         ANDGT     0
     C*** BEG ADD *** 07/17/03 **************************************
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'S'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'V'
     C     PAAMT         ANDLT     0
     C*** END ADD *** 07/17/03 **************************************
     C*
     C                   ADD       PAWKS         WK2(X)
     C     DA2(X)        ADD       PADAYS        XXDAYS            2 0
     C     XXDAYS        IFGE      7
     C     XXDAYS        DIV       7             XXWEEK            1 0
     C                   ADD       XXWEEK        WK2(X)
     C                   MULT      7             XXWEEK
     C     XXDAYS        SUB       XXWEEK        DA2(X)
     C                   ELSE
     C                   Z-ADD     XXDAYS        DA2(X)
     C                   END
     C*
     C                   ELSE
     C*
     C                   SUB       PAWKS         WK2(X)
     C     DA2(X)        SUB       PADAYS        XXDAYS
     C     XXDAYS        IFLT      0
     C                   SUB       1             WK2(X)
     C     7             ADD       XXDAYS        DA2(X)
     C                   ELSE
     C                   Z-ADD     XXDAYS        DA2(X)
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*
     C****************************************************
     C*   SUBROUTINE - Write the Workfile
     C****************************************************
     C     SRWKFL        BEGSR
     C*
     C                   CLEAR                   PPH01
     C*
     C                   Z-ADD     CMCO#         PHCO#
     C                   Z-ADD     CMFND         PHFND
     C                   MOVEL     FNNAME        PHFNAM
     C                   Z-ADD     CMFYR         PHFYR
     C                   Z-ADD     PACASE        PHCASE
     C                   MOVEL     XXNAME        PHCNAM
     C                   Z-ADD     CMEMP#        PHEMP#
     C****  05/05/99 BGN CHG ***********************************
     C     EMKY          CHAIN     PEM01                              67
     C     *IN67         IFEQ      *ON
     C                   MOVEL     *BLANKS       EMNAME
     C                   ENDIF                                                               '0'
     C****  05/05/99 END CHG ***********************************
     C                   MOVEL     EMNAME        PHENAM
     C                   Z-ADD     CMDIV         PHDIV
     C                   Z-ADD     CMACDT        PHACDT
     C*
     C*  Get occupation description
     C                   MOVEL     CMOCCD        PHOCCD
     C                   MOVEL     CMOCDS        PHOCDS
     C     PHOCDS        IFEQ      *BLANK
     C     CMOCCD        ANDNE     *BLANK
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL(P)  CMOCCD        XXKEY
     C     OCKY          CHAIN     POC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  OCDESC        PHOCDS
     C                   ENDIF                                                  e IN60=OFF
     C                   ENDIF                                                  e OCDS=BLANK
     C*
      ***** BEG ADD ***** 12/09/02 **************************************
     C* Write the Nature Part and Description in file
     C                   MOVEL(P)  HLNATR        PHNATR
     C                   MOVEL(P)  HLNDES        PHNDES
      ***** END ADD ***** 12/09/02 **************************************
     C*
     C*  Get department description
     C                   MOVEL(P)  CMDEPT        PHDEPT
     C                   MOVE      *BLANK        PHDDES
     C                   MOVEL(P)  CMDEPT        XXDEPT
     C     DPKY          CHAIN     PDP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DPDESC        PHDDES
     C                   END
     C*
     C                   MOVEL     PATRCD        PHTRCD
     C                   MOVEL     PADESC        PHDESC
     C                   MOVEL     PATYPE        PHTYPE
     C                   MOVEL     PARECD        PHRECD
     C                   MOVEL     PARETP        PHRETP
     C*
     C                   Z-ADD     PATRDT        PHTRDT
      ***** BEG CHG ***** 12/15/06 ***************************************
     C     PARQDT        IFNE      *ZEROS
     C                   Z-ADD     PARQDT        PHPODT
     C                   ELSE
     C                   Z-ADD     PAPODT        PHPODT
     C                   ENDIF
     C************         Z-ADDPAPODT    PHPODT
      ***** END CHG ***** 12/15/06 ***************************************
     C                   Z-ADD     PAFRDT        PHFRDT
     C                   Z-ADD     PATODT        PHTODT
     C*
     C                   Z-ADD     PAWKS         PHWKS
     C                   Z-ADD     PADAYS        PHDYS
     C                   Z-ADD     PAFEIN        PHFEIN
     C                   Z-ADD     PAMOD         PHMOD
     C                   MOVEL     XXPYNM        PHNAME
     C                   Z-ADD     PACHK#        PHCHK#
     C*
     C*  Get payment value 1 description
     C                   MOVEL     PABR2         PHPV1
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     PABR2         XXKEY
     C     TBKY2         CHAIN     PB201                              60
     C     *IN60         IFEQ      *OFF
     C     B2STAT        ANDNE     'I'
     C                   MOVEL(P)  B2DESC        PHPV1D
     C                   ENDIF
     C*
     C*  Get payment value 2 description
     C                   MOVEL     PABR7         PHPV2
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     PABR7         XXKEY
     C     TBKY2         CHAIN     PB701                              60
     C     *IN60         IFEQ      *OFF
     C     B7STAT        ANDNE     'I'
     C                   MOVEL(P)  B7DESC        PHPV2D
     C                   ENDIF
     C*
     C                   Z-ADD     PAAMT         PHAMT
     C                   Z-ADD     PAEOB#        PHEOB#
     C                   MOVEL     PAPGTP        PHPGTP
     C                   MOVEL     PAACCT        PHACCT
     C                   MOVEL     PAOFF#        PHOFF#
     C*
     C*  Get office name
     C                   MOVEL     PAOFF#        XXOFC#
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     OFNAME        PHONAM
     C                   ENDIF
     C                   Z-ADD     PARCVD        PHRCVD
     C                   Z-ADD     PADUE         PHDUE
     C*
     C*  Get late reason description
     C                   MOVEL     PALTCD        PHLTCD
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PALTCD        XXKEY
     C     TBKY2         CHAIN     PLR01                              60
     C     *IN60         IFEQ      *OFF
     C     LRSTAT        ANDNE     'I'
     C                   MOVEL     LRDESC        PHLDES
     C                   ENDIF                                                   e IN60 OFF
     C*
     C                   MOVEL     PARQST        PHRQST
     C                   MOVEL     PAFNAL        PHFNAL
     C                   MOVEL     PARADJ        PHRADJ
     C*
     C                   WRITE     PPH01
     C                   ENDSR
     C*** Beg Add *** 12/09/15 *****************************************
      /Free
       //-------------------------------------------------------------
       // GetXMLFileNm - Get the XML File Name
       //--------------------------------------------------------------
           Begsr GetXMLFileNm;
            outCall = 'XMLREQUEST';
            OutPrefix = '  ';
            OutString = 'ClmPmtHist';
            InReturn  = procType;

            // Determine if Mass submission or Individual
            If (xxCase <> 0 And xxCase = xxTCas) Or
               (xxCase <> 0 and xxTCas = 0);
               massFlag = 'N';
            Else;
               massFlag = 'Y';
            EndIf;

            StLetter(OutCall:InXML:InReturn:outMode:OutRrn:
             OutPrefix:OutString:OutCo#:OutFnd:massFlag:xmlLink);

            Chain inXML pxp01;
            If %Found(xmlprel1);
               inXML = XPxmlT;
            EndIf;

           Endsr;
       //-------------------------------------------------------------
       // WrtSXPHPCLM - Write to Scriptura Payment History workfile -
       //               Claim Header
       //--------------------------------------------------------------
           Begsr WrtSXPHPCLM;

              Clear pphc0p;

              PHCxml   = inXML;
              PHCco#   = CMco#;
              PHCfnd   = CMfnd;
              PHCfYr   = CMfyr;
              PHCcase  = CMcase;
              PHCcNam  = xxName;
              PHCacDt  = dtMDYY(CMacDt);
              PHCnatr  = HLnatr;
              PHCnDes  = HLnDes;
              PHCocCd  = CMocCd;
              PHCocCds = CMocDs;

              If PHCocCds = ' ' And CMocCd <> ' ';
                 xxKey = CMocCd;
                 Chain (CMco#:xxkey) poc01;
                 If %Found(wtocl);
                    PHCocCds = OCdesc;
                 EndIf;
              EndIf;

              PHCemp#  = CMemp#;
              PHCdiv   = CMdiv;

              Chain (CMco#:CMfnd:CMemp#:CMdiv) pem01;
              If %Found(wmeml);
                  PHCempNam = EMname + EMnam2;
              EndIf;

              PHCiDt   = PHRiDt;

              Write pphc0p;

           Endsr;
       //-------------------------------------------------------------
       // WrtSXPHPCLMT - Write to Scriptura Payment History Workfile -
       //                Claim Summary
       //--------------------------------------------------------------
           Begsr WrtSXPHPCLMT;

              Clear pph10p;

              PH1xml  = inXML;
              PH1co#  = CMco#;
              PH1fnd  = CMfnd;
              PH1fYr  = HLfyr;
              PH1case = HLcase;

              If isTrCd;
                 PH1trCdTot = xxCde;
                 PH1trCdDs  = xxDesc;
                 PH1trCdAmt = xxAmt;
              Else;
                 PH1pv1Tot = xxCde;
                 PH1pv1Ds  = xxDesc;
                 PH1weeks  = xxWks;
                 PH1days   = xxDys;
                 PH1pv1Amt = xxAmt;
              EndIf;

              PH1iDt  = PHRiDt;

              Write pph10p;
           Endsr;
       //-------------------------------------------------------------
       // WrtSXPHPDTL - Write to Scriptura Payment History workfile -
       //               Detail
       //--------------------------------------------------------------
           Begsr WrtSXPHPDTL;

              Clear pphd0p;

              PHDxml  = inXML;
              PHDco#  = CMco#;
              PHDfnd  = CMfnd;
              PHDfyr  = CMfyr;
              PHDcase = CMcase;
              PHDtrCd = PAtrcd;
              PHDdesc = PAdesc;
              PHDType = PAtype;

              Select;
              When PAtype = 'A';
                 PHDtypeDs = 'Adjustment';
              When PAtype = 'M';
                 PHDtypeDs = 'Manual';
              When PAtype = 'P';
                 PHDtypeDs = 'Payment';
              When PAtype = 'R';
                 PHDtypeDs = 'Recovery';
              When PAtype = 'S';
                 PHDtypeDs = 'Stop Payment';
              When PAtype = 'V';
                 PHDtypeDs = 'Void';
              When PAtype = 'Z';
                 PHDtypeDs = 'Reversal';
              EndSl;

              PHDreCd = PAreCd;

              chrKey = PAreCd;
              Chain (xxCo#:chrKey) prt01;
              If %Found(WTABL);
                 PHDreCdDs = RTdesc;
              EndIf;

              PHDreTp = PAretp;

              Select;
              When PAreTp = 'C';
                 PHDreTpDs = 'Compensation';
              When PAreTp = 'E';
                 PHDreTpDs = 'Expense';
              When PAreTp = 'L';
                 PHDreTpDs = 'Legal';
              When PAreTp = 'M';
                 PHDreTpDs = 'Medical';
              EndSl;

              PHDtrDt = dtMDYY(PAtrDt);

              If PArqDt <> 0;
                 PHDpoDt = dtMDYY(PArqDt);
              Else;
                 PHDpoDt = dtMDYY(PApoDt);
              EndIf;

              PHDfrDt = dtMDYY(PAfrDt);
              PHDtoDt = dtMDYY(PAtoDt);
              PHDwks  = PAwks;
              PHDdys  = PAdays;
              PHDpv1  = PAbr2;

              xxKey = PAbr2;
              Chain tbky2 pb201;
              If %Found(WTABL);
                 PHDpv1Ds = B2desc;
              EndIf;

              PHDpv2  = PAbr7;

              xxKey = PAbr7;
              Chain tbky2 pb701;
              If %Found(WTABL);
                 PHDpv2Ds = B7desc;
              EndIf;

              PHDfein = PAfein;
              PHDmod  = PAmod;
              PHDname = xxPyNm;
              PHDchk# = PAchk#;
              PHDamt  = PAamt;
              PHDeob# = PAeob#;
              PHDpgtp = PApgtp;
              PHDfnal = PAfnal;
              PHDrAdj = PArAdj;

              PHDiDt  = PHRiDt;

              Write pphd0p;
           Endsr;
       //-------------------------------------------------------------
       // WrtSXPHPRPT - Write to Scriptura Payment History Workfile =
       //               Report Header
       //--------------------------------------------------------------
           Begsr WrtSXPHPRPT;

              Clear pphr0p;

              PHRxml  = inXML;
              PHRco#  = xxCo#;
              PHRfnd  = xxFnd;
              PHRcoNm  = %Trim(COname);

              Chain (PHRco#:PHRfnd) pfn01;
              If %Found(WMFNL);
                 If FNaNam <> ' ';
                    PHRcoNm  = FNaNam;
                 EndIf;
                 PHRfNam = FNname;
              EndIf;

              If x@fYr = ' ';
                 PHRFYR   = 0;
              Else;
                 PHRFYR   = %Int(x@fYr);
              EndIf;

              If x@emp# = ' ';
                 PHRemp#   = 0;
              Else;
                 PHRemp#   = %Int(x@emp#);
              EndIf;

              If x@div = ' ';
                 PHRdiv   = 0;
              Else;
                 PHRdiv   = %Int(x@div);
              EndIf;

              Chain (PHRco#:PHRfnd:PHRemp#:PHRdiv) pem01;
              If %Found(WMEML);
                 PHRemNm = EMname + EMnam2;
              EndIf;

              If x@case = ' ';
                 PHRfCas   = 0;
              Else;
                 PHRfCas   = %Int(x@case);
              EndIf;

              If x@tCas = ' ';
                 PHRtCas   = 0;
              Else;
                 PHRtCas   = %Int(x@tCas);
              EndIf;

              PHRSORT  = x@sort;

              Select;
              When x@sort = 'C';
                 PHRsortD = 'Check #/Transaction Date';
              When x@sort = 'T';
                 PHRsortD = 'Transaction Date';
              EndSl;

              If x@frDt = ' ';
                 PHRfrDt   = 0;
              Else;
                 PHRfrDt   = %Int(x@frDt);
              EndIf;

              If x@toDt = ' ';
                 PHRtoDt   = 0;
              Else;
                 PHRtoDt   = %Int(x@toDt);
              EndIf;

              PHRTRCD  = x@trCd;

              chrKey = x@trCd;
              Chain (xxCo#:chrKey) ppc01;
              If %Found(WTABL);
                 PHRtrCdS = PCdesc;
              EndIf;

              PHRTYPE  = x@type;

              Select;
              When x@type = 'A';
                 PHRtypDs = 'Adjustment';
              When x@type = 'M';
                 PHRtypDs = 'Manual';
              When x@type = 'P';
                 PHRtypDs = 'Payment';
              When x@type = 'R';
                 PHRtypDs = 'Recovery';
              When x@type = 'V';
                 PHRtypDs = 'Void';
              When x@type = 'S';
                 PHRtypDs = 'Stop Pay';
              When x@type = 'Z';
                 PHRtypDs = 'Reversal';
              EndSl;

              PHRreCd1 = x@rcd1;
              chrKey = x@rcd1;
              Chain (xxCo#:chrKey) prt01;
              If %Found(WTABL);
                 PHRrecDs1 = RTdesc;
              EndIf;

              PHRreCd2 = x@rcd2;
              chrKey = x@rcd2;
              Chain (xxCo#:chrKey) prt01;
              If %Found(WTABL);
                 PHRrecDs2 = RTdesc;
              EndIf;

              PHRreCd3 = x@rcd3;
              chrKey = x@rcd3;
              Chain (xxCo#:chrKey) prt01;
              If %Found(WTABL);
                 PHRrecDs3 = RTdesc;
              EndIf;

              PHRreCd4 = x@rcd4;
              chrKey = x@rcd4;
              Chain (xxCo#:chrKey) prt01;
              If %Found(WTABL);
                 PHRrecDs4 = RTdesc;
              EndIf;

              PHRreCd5 = x@rcd5;
              chrKey = x@rcd5;
              Chain (xxCo#:chrKey) prt01;
              If %Found(WTABL);
                 PHRrecDs5 = RTdesc;
              EndIf;

              If x@rcd1 = ' ' And x@rcd2 = ' ' And x@rcd3 = ' '
                 And x@rcd4 = ' ' And x@rcd5 = ' ';
                 PHRreCd1 = '**';
                 PHRrecDs1 = 'All Reserve Codes';
              EndIf;


              PHRreTp1 = x@rTp1;
              Select;
              When x@rTp1 = 'M';
                 PHRretDs1 = 'Medical';
              When x@rTp1 = 'C';
                 PHRretDs1 = 'Compenstion';
              When x@rTp1 = 'E';
                 PHRretDs1 = 'Expense';
              When x@rTp1 = 'L';
                 PHRretDs1 = 'Legal';
              EndSl;

              PHRreTp2 = x@rTp2;
              Select;
              When x@rTp2 = 'M';
                 PHRretDs2 = 'Medical';
              When x@rTp2 = 'C';
                 PHRretDs2 = 'Compenstion';
              When x@rTp2 = 'E';
                 PHRretDs2 = 'Expense';
              When x@rTp2 = 'L';
                 PHRretDs2 = 'Legal';
              EndSl;

              PHRreTp3 = x@rTp3;
              Select;
              When x@rTp3 = 'M';
                 PHRretDs3 = 'Medical';
              When x@rTp3 = 'C';
                 PHRretDs3 = 'Compenstion';
              When x@rTp3 = 'E';
                 PHRretDs3 = 'Expense';
              When x@rTp3 = 'L';
                 PHRretDs3 = 'Legal';
              EndSl;

              PHRreTp4 = x@rTp4;
              Select;
              When x@rTp4 = 'M';
                 PHRretDs4 = 'Medical';
              When x@rTp4 = 'C';
                 PHRretDs4 = 'Compenstion';
              When x@rTp4 = 'E';
                 PHRretDs4 = 'Expense';
              When x@rTp4 = 'L';
                 PHRretDs4 = 'Legal';
              EndSl;

              PHRPV1   = x@br2;
              chrKey = x@br2;
              Chain (xxCo#:chrKey) pb201;
              If %Found(WTABL);
                 PHRpv1Ds = B2desc;
              EndIf;

              PHRoutTyp = x@out;

              PHRiDt = %Dec(%Date():*cymd);

              Write pphr0p;
           Endsr;
       //-------------------------------------------------------------
       // WrtSXPHPRPTT - Write to Scriptura Payment History workfile
       //                Report Summary
       //--------------------------------------------------------------
           Begsr WrtSXPHPRPTT;

              Clear pph20p;

              PH2xml  = inXML;

              If isTrCd;
                 PH2trCdTot = xxCde;
                 PH2trCdDs  = xxDesc;
                 PH2trCdAmt = xxAmt;
              Else;
                 PH2pv1Tot = xxCde;
                 PH2pv1Ds  = xxDesc;
                 PH2weeks  = xxWks;
                 PH2days   = xxDys;
                 PH2pv1Amt = xxAmt;
              EndIf;

              PH2iDt  = PHRiDt;

              Write pph20p;
           Endsr;
      /End-Free
     C*** End Add *** 12/09/15 *****************************************
     C*
     C****************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
     O***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*
     O          E            ROUTE          1
     O                                           19 'GROUP:'
     O                       XXFND         4     45
     O*
     O          E            ROUTE          1
     O                                           19 'YEAR:'
     O               71      XXFYR         4     45
     O*** BEG ADD *** 08/14/03  *********************************
     O               72      XXFYR         4     45
     O*** END ADD *** 08/14/03  *********************************
     O               76      XXFYR         4     45
     O*** BEG CHG *** 08/14/03  *********************************
     O*************   N71N72             45 '*ALL'
     O              N71N72N76                    45 '*ALL'
     O*** END CHG *** 08/14/03  *********************************
     O*
     O          E            ROUTE          1
     O                                           19 'EMPLOYER:'
     O               71N77   XXEMP#        4     39
     O               71N77   XXDIV         4     45
     O               72N77   XXEMP#        4     39
     O               72N77   XXDIV         4     45
     O               73N77   XXEMP#        4     39
     O               73N77   XXDIV         4     45
     O               77                          45 '*ALL'
     O              N71N72N73                    45 '*ALL'
     O*** BEG ADD *** 08/14/03  *********************************
     O               75N77   XXEMP#        4     39
     O               75N77                       45 '*ALL'
     O               76N77   XXEMP#        4     39
     O               76N77                       45 '*ALL'
     O*** END ADD *** 08/14/03  *********************************
     O*
     O          E            ROUTE          1
     O                                           19 'CASE RANGE:'
     O*** BEG CHG *** 03/01/04  *********************************
     O***********      71      XXCASEZ   35
     O***********      71                37 '-'
     O***********      71      XXTCASZ   45
     O***********     N71                45 '*ALL'
     O               51      XXCASE        Z     35
     O               51                          37 '-'
     O               51      XXTCAS        Z     45
     O              N51                          45 '*ALL'
     O*** END CHG *** 03/01/04  *********************************
      **** BEG ADD ***** 03/11/10 **********************************
     O          E            ROUTE          1
     O                                           19 'SORT ORDER:'
     O                       X@SORT              42
     O              N89                          60 '- TRANSACTION DATE'
     O               89                          57 '- CHECK NUMBER/'
     O               89                          73 'TRANSACTION DATE'
      **** END ADD ****** 03/11/10 **********************************
     O*
     O          E            ROUTE          1
     O                                           19 'DATE RANGE:'
     O              N78N79   X8FRDT        Y     32
     O              N78N79                       34 '-'
     O              N78N79   X8TODT        Y     45
     O               78 79                       45 '*ALL'
     O*
     O          E            ROUTE          1
     O                                           19 'PAYMENT CODE:'
     O              N80      X@TRCD              45
     O               80                          45 '*ALL'
     O*
     O          E            ROUTE          1
     O                                           19 'TRANSACTION TYPE:'
     O              N81      X@TYPE              45
     O               81                          45 '*ALL'
     O*
     O          E            ROUTE          1
     O                                           19 'RESERVE CODE:'
      ***** BEG DEL ******* 08/13/01 **********************************
     O**********      N82      X@RECD    45
      ***** END DEL ******* 08/13/01 **********************************
      ***** BEG ADD ******* 08/13/01 **********************************
     O              N82      X@RCD1              45
     O              N82      X@RCD2            +001
     O              N82      X@RCD3            +001
     O              N82      X@RCD4            +001
     O              N82      X@RCD5            +001
      ***** END ADD ******* 08/13/01 **********************************
     O               82                          45 '*ALL'
     O*
     O          E            ROUTE          1
     O                                           19 'RESERVE TYPE:'
      ***** BEG DEL ******* 08/13/01 **********************************
     O*************   N83      X@RETP    45
      ***** END DEL ******* 08/13/01 **********************************
      ***** BEG ADD ******* 08/13/01 **********************************
     O              N83      X@RTP1              45
     O              N83      X@RTP2            +001
     O              N83      X@RTP3            +001
     O              N83      X@RTP4            +001
      ***** END ADD ******* 08/13/01 **********************************
     O               83                          45 '*ALL'
     O*
     O          E            ROUTE          1
     O                                           19 'PAYMENT VALUE 1:'
     O              N84      X@BR2               45
     O               84                          45 '*ALL'
      ***** Beg Add ******* 02/08/23 **********************************
     O          E            ROUTE          1
     O                                           19 'LOST TIME:'
     O                       X@LTI               45
      ***** End Add ******* 02/08/23 **********************************
      ***** BEG CHG ******* 08/13/01 **********************************
     O*********                          23 '(C) COPYRIGHT PROGRESSI'
     O*********                          46 'VE DATA SOLUTIONS, INC.'
     O*********                          52 ', 1997'
      *
      ****** BEG CHG ***** 01/28/10 **********************************
     O*****   E   60           ROUTE
     O****                             +001 '(C) Copyright'
     C*** BEG CHG ******** 03/11/10 ********************************************
     O************                     +001 'Progressive Data'
     O************                     +001 'Solutions, Inc.,'
     O****                             +001 'Stoneriver, inc.'
     C*** END CHG ******** 03/11/10 ********************************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ***** END CHG ****** 01/28/10 ***********************************
      *
      ***** END CHG ******* 08/13/01 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
      ***** BEG CHG ******* 08/13/01 **********************************
     O*********       N88      CONAME   100
     O*********        88      FNANAM   100
     O*********                XXDATEY  173
     O              N88      CONAME             110
     O               88      FNANAM             115
      ***** BEG CHG ******* 04/19/10 **********************************
     O******                   XXDATEY  181
     O                       XXDATE        Y    193
      ***** END CHG ******* 04/19/10 **********************************
      ***** END CHG ******* 08/13/01 **********************************
      *
     O          E            HEAD           2
     O                       Q1PGM               10
      ***** BEG CHG ******* 08/13/01 **********************************
     O*********                ORTITL   111
     O*********                         167 'PAGE:'
     O*********                PAGE  Z  173
     O                       ORTITL             120
      ***** BEG CHG ******* 04/19/10 **********************************
     O*********                         175 'PAGE:'
     O***********              PAGE  Z  181
     O                                          187 'PAGE:'
     O                       PAGE          Z    193
      ***** END CHG ******* 04/19/10 **********************************
      ***** END CHG ******* 08/13/01 **********************************
      *
     O          E            HEAD           1
     O                                           11 'SELECTIONS:'
     O                                           20 'YEAR:'
     O                       XXFYR         4     23
     O*
     O                                         + 10 'FROM CASE:'
     O                       XXCASE        Z   +  1
     O                                         +  1 'to'
     O                       XXTCAS        Z   +  1
     O*
     O                                         + 10 'DATES:'
     O              N78      X8FRDT            +  1 '  /  /    '
     O               78                        +  1 '*LOW'
     O                                         +  1 'to'
     O              N79      X8TODT            +  1 '  /  /    '
     O               79                        +  1 '*HIGH'
     O*
     O                                         + 10 'EMPLOYER:'
     O               77                        +  1 '*ALL'
     O              N77      X@EMP#            +  1
     O              N77      X@DIV             +  1
     O*================================================================
      *
      ***** BEG DEL ******* 08/13/01 **********************************
      ************
     O********E**2             HEAD
     O************                       38 'PAYMENT CODE:'
     O************    N80      X@TRCD  +  1
     O************     80                43 '*ALL'
     O************
     O************                     + 10 'TYPE:'
     O************    N81      X@TYPE  +  1
     O************     81              +  1 '*ALL'
     O************
     O************                     + 10 'RESERVE CODE:'
     O************    N82      X@RECD  +  1
     O************     82              +  1 '*ALL'
     O************
     O************                     + 10 'TYPE:'
     O************    N83      X@RETP  +  1
     O************     83              +  1 '*ALL'
     O************
     O************                     + 10 'PAYMENT VALUE 1:'
     O************    N84      X@BR2   +  1
     O************     84              +  1 '*ALL'
      ***** END DEL ******* 08/13/01 **********************************
      ***** BEG ADD ******* 08/13/01 **********************************
      *
     O          E            HEAD           2
     O                                         + 15 'PAYMENT CODE:'
     O              N80      X@TRCD            +  1
     O               80                          33 '*ALL'
      *
     O                                         + 10 'TRANSACTION TYPE:'
     O              N81      X@TYPE            +  1
     O               81                          65 '*ALL'
      *
     O                                         + 10 'RESERVE CODE:'
     O              N82      X@RCD1            +  1
     O              N82      X@RCD2            +  1
     O              N82      X@RCD3            +  1
     O              N82      X@RCD4            +  1
     O              N82      X@RCD5            +  1
     O               82                          93 '*ALL'
      *
     O                                         + 20 'RESERVE TYPE:'
     O              N83      X@RTP1            +  1
     O              N83      X@RTP2            +  1
     O              N83      X@RTP3            +  1
     O              N83      X@RTP4            +  1
     O               83                         131 '*ALL'
      *
     O                                         + 20 'PAYMENT VALUE 1:'
     O              N84      X@BR2             +  1
     O               84                         172 '*ALL'
      ***** Beg Add ******* 02/08/23 **********************************
     O                                         +  5 'LOST TIME:'
     O                       X@LTI              193
      *
      ***** End Add ******* 02/08/23 **********************************
      ***** END ADD ******* 08/13/01 **********************************
      *
     O          E            HEAD           1
     O                                           09 'GROUP:'
     O                       XXFND         Z     13
     O                       FNNAME              75
      *
     O          E   NLRN17   HEAD           1
     O                                           09 'YEAR:'
     O                       HLYEAR              13
     O                                           23 'CLAIM:'
     O                       HLCASE        Z     31
     O                                           39 'NAME:'
     O                       XXNAME              75
      ***** BEG ADD ***** 12/09/02 **************************************
     O                                         +  1 'NATURE CODE:'
     O                       HLNATR            +  1
     O                                         +  3 'NATURE DESCRIPTION:'
     O                       HLNDES            +  1
      ***** END ADD ***** 12/09/02 **************************************
      *
     O          E   N12      HEAD        1  1
     O                                           13 'TRANSACTION'
     O                                           23 'RESERVE'
     O                                           54 'SERVICE'
      *
     O          E   N12      HEAD           2
     O                                           13 'CODE   TYPE'
     O                                           24 'CODE TYPE'
     O                                           34 'TRAN DT'
     O                                           44 'POST DT'
     O                                           53 'FROM'
     O                                           64 'TO'
     O                                           70 'WKS'
     O                                           74 'DYS'
     O                                           78 'BR2'
     O                                           82 'BR7'
     O                                           94 'DESCRIPTION'
     O                                          119 'PAYEE'
     O                                          127 'MOD'
     O                                          138 'PAYEE NAME'
     O                                          167 'CHECK #'
     O                                          181 'AMOUNT'
     O*******BEG ADD********04/19/10**********************************
     O                                          193 'EOMB#'
     O*******END ADD********04/19/10**********************************
      *
     O          E            DET            1
     O                       PATRCD               5
     O                       PATYPE              11
     O                       PARECD              18
     O                       PARETP              22
     O                       MMTRDT         B    34 '  /  /    '
     O                       MMPODT         B    45 '  /  /    '
     O                       MMFRDT         B    56 '  /  /    '
     O                       MMTODT         B    67 '  /  /    '
     O                       PAWKS         ZB    70
     O                       PADAYS        ZB    73
     O                       PABR2          B    77
     O                       PABR7          B    81
     O                       PADESC         B   113
     O                       PAFEIN        ZB   123
     O                       PAMOD         ZB   127
     O                       XXPYNM         B   158
     O                       PACHK#        ZB   167
     O                       XPAMT         AB   183
     O*******BEG ADD********04/19/10**********************************
     O                       PAEOB#        ZB   193
     O*******END ADD********04/19/10**********************************
      *
     O          E            TOTL        1  1
     O                       NBRCLM        BB    14
     O                                          133 'COUNT:'
     O                       RCDS          B    140
     O                                          163 'TOTAL:'
     O                       XXTOTL        B    183
      *
     O          E            NOCLM       1
     O                                           58 '*** NO RECORDS MEET '
     O                                           79 'SELECTED CRITERIA ***'
      *
     O          E            SUMH        2  1
     O                                           23 '*** SUMMARY BY'
     O                                           45 'PAYMENT TYPE CODE ***'
      *
     O          E            SUMH        1  2
     O                                           13 'TYPE'
     O                                           53 'AMOUNT'
      *
     O          E            SUM            1
     O                       XXCDE               11
     O                       XXDESC              38
     O                       XXAMT         A     55
      *
     O          E            SUMH2       2  1
     O                                           23 '*** SUMMARY BY'
     O                                           48 'PAYMENT VALUE 1 CODE ***'
      *
     O          E            SUMH2       1  2
     O                                           13 'TYPE'
     O                                           45 'WEEKS'
     O                                           51 'DAYS'
     O                                           67 'AMOUNT'
      *
     O          E            SUM2           1
     O                       XXCDE               11
     O                       XXDESC              38
     O                       XXWKS         ZB    44
     O                       XXDYS         ZB    50
     O                       XXAMT         A     69
      *
     O          E            GTOT        2  1
     O                                           29 '*** GRAND TOTAL: ***'
     O                       XXGTOT        B     55
      *
     O          E            SUMH3       2  1
     O                                           29 '*** GRAND SUMMARY BY'
     O                                           51 'PAYMENT TYPE CODE ***'
      *
     O          E            SUMH3       1  2
     O                                           13 'TYPE'
     O                                           53 'AMOUNT'
      *
     O          E            SUM3           1
     O                       XXCDE               11
     O                       XXDESC              38
     O                       XXAMT         A     55
      *
     O          E            SUMH4       2  1
     O                                           29 '*** GRAND SUMMARY BY'
     O                                           54 'PAYMENT VALUE 1 CODE ***'
      *
     O          E            SUMH4       1  2
     O                                           13 'TYPE'
     O                                           45 'WEEKS'
     O                                           51 'DAYS'
     O                                           67 'AMOUNT'
      *
     O          E            SUM4           1
     O                       XXCDE               11
     O                       XXDESC              38
     O                       XXWKS         ZB    44
     O                       XXDYS         ZB    50
     O                       XXAMT         A     69
