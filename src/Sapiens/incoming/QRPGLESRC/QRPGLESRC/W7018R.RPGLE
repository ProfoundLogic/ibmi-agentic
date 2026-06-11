     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7018R - PRINT VOUCHER/CHECK (LASER) AND STUB
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM WILL PRINT A CHECK/VOUCHER AND STUB.           *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W0120R -  Manual Payment                                    *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   SRAMNT  - ASSIGN PAYMENT VERBAGE                            *
     H*   SRNAME  - FORMAT FIRST & LAST NAME INTO ONE FIELD           *
     H*                                                               *
     H* CREATED:  08/29/97  C:30  Project:  WC659                    *
     H*****************************************************************
     H* REVISIONS:                                                    *
      *                                                               *
      * 06/09/20 C:318 Project: SY420
      *                Mask SSN
      *
      *  09/10/19 C:062 Ticket: 82877(133)                            *
      *                 Remove FNGRPASC from program to match regular *
      *                 print.                                        *
      *                                                               *
      *  04/10/19 C:062 Project: 152WAB                               *
      *                 Apply invoice #.                              *
     H*                                                               *
     H*  03/06/19 C:180 PROJECT: 126WFR
     H*                 Add dashes to Void after x days for Client 126.
     H*                                                               *
     H*  08/21/18 C:180 PROJECT: 126WFR                               *
     H*                 Use payee's font for VOID AFTER x DAYS.       *
     H*                 Add variable text on check.                   *
     H*                                                               *
     H*  06/18/18 C:062 PROJECT: 129WVB                               *
     H*                 Move the micr line to the left for CoveRisk   *
     H*                                                               *
     H*  05/21/18 C:062 PROJECT: 129WVC                               *
     H*                 Use print blank lines flag.                   *
     H*                                                               *
     H*  04/11/18 C:053 PROJECT: 129WVC                               *
     H*                 Special printing of lines & no signature on $ limit
     H*                                                               *
     H*  09/26/17 C: 53 SUPPORT 46763                                 *
     H*                 Recomple for OCHKCHECK changes                *
     H*                                                               *
     H*  07/25/17 C:079 PROJECT: SY395                                *
     H*                 Add Scriptura processing                      *
     H*                                                               *
     H*  10/20/16 C: 53 PROJECT 34WCA                                 *
     H*                 Add Pay To info that was removed from clients
     H*                 version of overlay
     H*
     H*  01/21/16 C: 53 SUPPORT 39472
     H*                 BTB Check changes for CLT065 Use *IN23 for ERS
     H*                 Apply prior items: 11600-Move amount down for box
     H*                                    12219-Move MICR line down for bank
     H*                                                               *
     H*  02/05/13 C:283 Support: WC046 81382                          *
     H*                 *IN78 Condition 1st Signature Line to Print.  *
     H*                                                               *
     H* 09/23/11 C: 62 SUPPORT 78005                                  *
     H*                Extend employer key to include division.       *
     H*                                                               *
     H* 03/25/11 C: 53 SUPPORT 75860                                  *
     H*                Check changes to MICR line for CLT146          *
     H*                Change *IN20 to be used for base printing      *
     H*                                                               *
     H* 07/27/09 C:136 PROJECT: 114WUK (CLIENT SPECIFIC)              *
     H*                ALLOW FOR XML TYPE TRANSACTIONS                *
     H*                                                               *
     H* 10/12/07 C:190 Project: WC1557                                *
     H*                Condition the load of field @@SS# on the       *
     H*                new flag.                                      *
     H*                   1. If COPSS# = 'Y', execute the current     *
     H*                      load processing.                         *
     H*                   2. If COPSS# = blank, load blanks to        *
     H*                      @@SS#.                                   *
     H*                                                               *
     H* 10/03/07 C:33 Project: 45WFO Back to Base                     *
     H*                *IN78 condition 1st signature line to print    *
     H*                                                               *
     H* 06/20/07 C:53 114WMF  BACK TO BASE-114WED                     *
     H* 07/26/02 C:61  Project: 114WED                                *
     H*                Changed program to print 'Void after x days',  *
     H*                where x is BI50N3 (a value moved to a 3.0      *
     H*                numeric field for output purposes).  If the    *
     H*                number of days is zero, the verbiage will not  *
     H*                be printed on the checks.                      *
     H*                                                               *
     H* 04/27/07 C:15  Project: 71WHI                                 *
     H*                Chgd pgm to print 1st signature line for clt   *
     H*                71.                                            *
     H*               -71WCI: Add void verbiage.                      *
     H*               -71WCO: Removed "Paid by" text in OCHKDET2 and  *
     H*                       OFILDET2.                               *
     H*                                                               *
     H*04/25/07 C:053 SUPPORT 57936                                 *
     H*                Re-apply 16422 under original date 02/10/99   *
     H*2/10/99 C:15 Support: WC090/16422                            *
     H*             Changed pgm to use the second signature line if *
     H*             the base overlay is used.                       *
     H*                                                               *
     H*02/26/07 C:132 PROJECT 90WEY                                 *
     H*                Add "Void after 90 days" for client 090 to    *
     H*                move MOD back to base, like client 139.       *
     H*                                                               *
     H* 11/29/05 C:53  Project: 139WAD
     H*                Format changes for void msg.
     H*                *IN78 condition 1st signature line to print
     H*
     H* 11/28/05 C:53  Project: 136WAH
     H*                Format changes for payee # add void msg.
     H*                *IN15 condition xxx in numbers
     H*                *IN78 condition 1st signature line to print
     H*
     H* 04/25/05 C:156 PROJECT: 132WCN                                *
     H*                Print CA fraud statement on checks.            *
     H*                                                               *
     H* 10/20/03 C:53 Project: WC909                                  *
     H*               Added country info.                             *
     H*                                                               *
     H* 05/22/03 C:15 Support: WC065/31676                            *
     H*               Change access to WDRPP.                         *
     H*                                                               *
     H*  04/25/01 C:15 Project: WC1020                                *
     H*                Added bank code (PACKTP) to key list when      *
     H*                retrieving the bank info record.               *
     H*                                                               *
     H*  05/19/00 C:15 Support: WC034/20089                           *
     H*                Added the TRDT to key list for WDRPP.          *
     H*                                                               *
     H*  04/07/99 C:15 Support: WC090/17156                           *
     H*                Removed XXTRDT from key list used to retrieve  *
     H*                records from WDRPP due to incorrect date being *
     H*                loaded into RPTRDT.                            *
     H*                                                               *
     H*  10/29/98 C:15  Project:                                      *
     H*                 Re-compiled due to a change in OCHKCHECK.     *
     H*                                                               *
     H*  10/05/98 C:15  Support                                       *
     H*                 Load the claimant SS# into the payee box on   *
     H*                 the check if RPFEIN is blank.                 *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       51      GENERAL USAGE (CHAIN)                           *
     H*       61      ADDRESS 2 IS BLANK                              *
     H*****************************************************************
     H* FILE USAGE:                                                   *
     H*                                                               *
     H*   WMFNL  (WMFNL) : CO/FND                                     *
     H*   WDPAL5 (WDPAL5): CO/FND/FYR/CASE/DATE/SEQ#                  *
     H*   WMCML1 (WMCML1): CO/FND/FYR/CASE                            *
     H*   WMCDL  (WMCDL) : CO/SSN                                     *
     H*   WTABL  (PRC01) : CO/KEY                                     *
     H*          (PB201) : CO/KEY                                     *
     H*   WMCDL  (PCD01) : SS#                                        *
     H*   WMEML  (PEM01) : CO/FND/EMP#/DIV                            *
     H****WMPDL  (PPD01) : FEIN/MOD                                   *
     H*   WMBIL4 (PBI01) : CO/ACCT/FND/EFDT                           *
     H*   CHECKS         : VOUCHER                                    *
     H*****************************************************************
     FWMCXL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDPAL5    IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     F*** BEG ADD *** 10/20/03 *****************************
     FWTGTL     IF   E           K DISK
     F*** END ADD *** 10/20/03 *****************************
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     F****WMPDL   IF  E           K        DISK
      *
      *  Bank Information file
     F*** Begin chg *** 04/25/01 **************************************
     F*****WMBIL4  IF  E           K        DISK
     FWMBIL6    IF   E           K DISK
     F*** Begin chg *** 04/25/01 **************************************
      *
     FCHECKS    O    F  145        PRINTER OFLIND(*INOA)
     F                                     USROPN
      *
      *  Bank Account  file    - CO#/ACCT
     FWSCKL     IF   E           K DISK
      *
      *  Payment Detail Detail file
     FWDRPL     IF   E           K DISK
     F*** Begin add *** 05/22/03 **************************************
     FWDRPL3    IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     F*** End add ***** 05/22/03 **************************************
     F*** BEG ADD *** 10/12/07 ****************************************
     FSMCOL     IF   E           K DISK
     F*** END ADD *** 10/12/07 ****************************************
     F*** Beg Add *** 07/25/17 ***************************************
     FXMLPREL1  UF   E           K DISK
     FSXCCP     uf a e             disk    Rename(pcc01:scc0p)
     F                                     Prefix(s_)
     F*** End Add *** 07/25/17 ***************************************
     F*
      *
     F*

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D*** Begin chg *** 04/27/07 ************************************
     D*************       WCA     1   2 35               CONSTANTS     XC
     D WCA             S             35    DIM(3) CTDATA PERRCD(1)              CONSTANTS     XC
     D*** End chg ***** 04/27/07 ************************************
      *** BEGIN ADD *** 02/13/97 *****************************************
     D SIGMS1          S              4    DIM(1) CTDATA PERRCD(1)              SIGNATURE MSG PT 1
     D SIGMS2          S             26    DIM(1) CTDATA PERRCD(1)              SIGNATURE MSG PT 2
      *** END   ADD *** 02/13/97 *****************************************
      *
      *  WORK AREA FOR FUND, FUND YEAR, AND CASE FOR VOUCHER FIELD
      *
     D                 DS
     D  XXCLM#                 1     13  0
     D  PAFND                  1      3  0
     D  PAFYR                  4      6  0
     D  PACASE                 7     13  0
      *
      *  WORK AREA FOR CLAIM AND CHECK FOR VOUCHER FIELD
      *
     D                 DS
     D  XXCNBR                 1     10
     D  XXSLSH                 1      1
     D  PACHK#                 2      8  0
     D  XXSLS2                 9      9
     D  XXBLK1                10     10
      *
      *  WORK AREA FOR ZIP CODE
      *
     D***         DS
     D***                                     1   90PDZIP
     D***                                     1   50XXZIP1
     D***                                     6   90XXZIP2
     D                 DS
     D  RPZIP                  1      9  0
     D  XXZIP1                 1      5  0
     D  XXZIP2                 6      9  0
      *
      *** BEGIN ADD *** 02/13/97 ******************************************
      *
     D                 DS
     D  XXTRAN                 1     11
     D  XXAMPR                 1      1
     D  BITRNS                 2      5
     D  BIBNK#                 6      9
     D  BICKDG                10     10
     D  XXAMP2                11     11
     D*** BEGIN ADD *** 10/05/98 **************************************
     D                 DS
     D  XXSS#                  1      9
     D  SSPRE                  1      3
     D  SSMID                  4      5
     D  SSSUF                  6      9
     D*** END ADD ***** 10/05/98 **************************************
      *** BEGIN ADD **** 07/26/02 *************************************
     D                 DS
     D  VDY                    1      3
     D  VDY1                   1      1
     D  VDY2                   2      2
     D  VDY3                   3      3
      *** END ADD ****** 07/26/02 *************************************
      *
      *
      *  Constant
      *
     D*** BEG CHG ***** 07/27/09 **************************************
     D***********   'VOUCHER'             C         VOUCHR
     D VCHR1           C                   CONST('VOUCHER')
     D VCHR2           C                   CONST('XML    ')
     D*** END CHG ***** 07/27/09 **************************************
      *** END   ADD *** 02/13/97 ******************************************
     D XXADJ           C                   CONST('ADJUSTER:')
     D*** BEG ADD *** 11/28/05 **************************************
     D VOID90          C                   CONST('VOID AFTER 90 DAYS')
     D*** END ADD *** 11/28/05 **************************************
     D*** BEGIN ADD *** 07/26/02 **************************************
     D XVMSG1          C                   CONST('Void after')
     D XVMSG2          C                   CONST('days')
     D*** END ADD ***** 07/26/02 **************************************
      *** Begin Add *** 03/06/19 **************************************
     D UPPER           C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     D LOWER           C                   CONST('abcdefghijklmnopqrstuvwxyz')
      *** End   Add *** 03/06/19 **************************************
     D*** BEG ADD *** 10/20/16 **************************************
     D @@PAY1          C                   CONST('PAY TO THE')
     D @@PAY2          C                   CONST('ORDER OF')
     D*** END ADD *** 10/20/16 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D PIDS01        E DS                  EXTNAME(SZQ4P)
      *** Beg Add ***  11/25/13  ******************************************************
     D CltId           s              3
      *
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d cltID                          3    const
      *** End Add ***  11/25/13  ***********************************************HEADING
     D***  Begin Add  ***  07/25/17  **************************************
     D* Scripture Processing Program
     D StLetter        pr                  ExtPgm('STLETTER')
     D   outCall_                    11    const
     D   InXML_                      60
     D   InReturn_                  250
     D   OutMode_                     1    const
     D   OutRrn_                      9    const
     D   OutPrefix_                   2    const
     D   OutString_                  10    const
     D   OutCo#_                      3    const
     D   OutFnd_                      3    const
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     d
     d S4401C          pr                  ExtPgm('S4401C')
     d   DTASRC_                     61    const
     d
     DObjExists        pr             1n
     D object                        10    const
     D objType                        7    const
     d
     D isScriptChk     s              1A   inz
     D isUseSpoolChk   s              1n   inz
     D isUseXML        S              1n   inz
     d xmlLink         s                   like(XPLink)
     d xmlFileName     s                   like(XPXML)
     d xmlFileBlank    s                   like(XPXML)
     d xmlFile         s                   like(XPXML)
     d isUseScript     S              1n   inz
     D xxPre           s              2    inz('PB')
     d processType     c                   const('Claim Checks')
     d message         s            250    inz(*blanks)
     d x1Fnd           s              3A
     d x1Co#           s              3A
     D OutString       s             10
     D*
     D sys_date        s               D   datfmt(*USA)
     D xxIdt           s                   like(PAiDt)
     D*** beg add *** 04/11/18 *************************************
     D Lines129        S              1n   inz
     D*** end add *** 04/11/18 *************************************
      *** Begin Add *** 03/06/19 **************************************
     D @@Void23        S             23A   INZ
      *** End   Add *** 03/06/19 **************************************
     D                 DS
     D  DSdtTm                 1     14  0
     D  DStime                 1      6  0
     D  DSdate                 7     14  0
     D/copy sPrModule
     D/copy sPrXML
     D***  End   Add  ***  07/25/17  **************************************
      *
      *   CLAIMS/DARTS CONTROL KEY LIST
      *

     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    XXCLEX
      *
      *  CLAIMANT DEMOGRAPHICS FILE
      *
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
      *
      *  CLAIM FILE
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *  CLAIM CHECK PAYMENT
      *
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXTRDT            7 0
     C                   KFLD                    XXSEQ#
      *
      *  Payment Detail Detail file
      *
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    PACHK#
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   KFLD                    PATRDT
     C*** END ADD ***** 05/19/00 **************************************
     C*** BEGIN DLT *** 04/07/99 ***********************************************
     C******************** KFLD           XXTRDT
     C*** END DLT ***** 04/07/99 ***********************************************
     C*** Begin add *** 05/22/03 **************************************
     C*
     C     R2KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PASEQ#
     C*** End add ***** 05/22/03 **************************************
      *
      *  TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      *
      *  FUND MASTER
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C*** Begin Add *** 09/23/11 **************************************
     C                   KFLD                    CMDIV
     C*** End   Add *** 09/23/11 **************************************
      *
      *  BANK INFORMAITON FILE
      *
     C*** Begin dlt *** 04/25/01 **************************************
      *** BEGIN CHG 02/13/97 ***************************************************
     C********** BIKEY     KLIST
     C**********           KFLD           PACO#
     C**********           KFLD           XXAPPL  2
     C**********           KFLD           PAACCT
     C**********           KFLD           PAFND
     C**********           KFLD           XXEFDT
      **********
     C********** BIKEY1    KLIST
     C**********           KFLD           PACO#
     C**********           KFLD           XXAPPL
     C**********           KFLD           PAACCT
     C**********           KFLD           PAFND
      *** END   CHG 02/13/97 ***************************************************
     C*** End dlt ***** 04/25/01 **************************************
     C*** Begin add *** 04/25/01 **************************************
     C     BIKEY         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    XXAPPL            2
     C                   KFLD                    PAFND
     C                   KFLD                    PACKTP
     C                   KFLD                    PAACCT
     C                   KFLD                    XXEFDT
      *
     C     BIKEY1        KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    PAFND
     C                   KFLD                    PACKTP
     C                   KFLD                    PAACCT
     C*** End add ***** 04/25/01 **************************************
      *
      ***PAYEE DEMOGRAPHICS FILE
      ***
     C***        PDKY      KLIST
     C***                  KFLD           PAFEIN
     C***                  KFLD           PAMOD
      *
      *** BEGIN ADD *** 02/13/97 ***********************************************
      *
      *  Bank Account Master File
      *
     C     CKKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAACCT
     C*** BEG ADD *** 10/20/03 *****************************
      *
      *   KEY LIST FOR THE GENERIC TABLE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      *
     C*** END ADD *** 10/20/03 *****************************
      *
     C     *LIKE         DEFINE    CXADJ         X1ADJ
     C     *LIKE         DEFINE    PAMOD         @@MOD
     C     *LIKE         DEFINE    PACO#         @@CO#
     C     *LIKE         DEFINE    PAFND         @@FND
     C     *LIKE         DEFINE    PAFYR         @@FYR
     C     *LIKE         DEFINE    PACASE        @@CASE
     C     *LIKE         DEFINE    PABR2         @@BR2
     C     *LIKE         DEFINE    PARECD        @@RECD
     C     *LIKE         DEFINE    PAWKS         @@WKS
     C     *LIKE         DEFINE    PADAYS        @@DAYS
     C     *LIKE         DEFINE    PACHK#        @@CHK#
     C     *LIKE         DEFINE    X1NAME        @@NAME
     C*** BEG DEL *** 11/28/05 **************************************
     C*******    *LIKE     DEFN PAFEIN    @@FEIN
     C*** END DEL *** 11/28/05 **************************************
     C********   *LIKE     DEFN CMACDT    @@ACDT
     C     *LIKE         DEFINE    PAAMT         @@AMT
     C     *LIKE         DEFINE    QQAMT1        @@AMT1
     C     *LIKE         DEFINE    QQAMT2        @@AMT2
     C     *LIKE         DEFINE    X1NAME        @@PNAM
     C     *LIKE         DEFINE    RPADR1        @@ADR1
     C     *LIKE         DEFINE    RPADR2        @@ADR2
     C     *LIKE         DEFINE    PAACCT        @@ACCT
     C     *LIKE         DEFINE    BIEFDT        XXEFDT
     C*** BEG ADD *** 10/20/03 *****************************
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXTKEY
     C*** END ADD *** 10/20/03 *****************************
     C*** Beg add *** 04/10/19 **************************************
     C     *LIKE         DEFINE    PAINV#        @@INV#
     C*** End add *** 04/10/19 **************************************
      *
      *** END   ADD *** 02/13/97 ***********************************************
      *****************************************************************
      *  ENTRY PARM LIST
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQTRDT            7
     C                   PARM                    QQSEQ#            5
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
     C     DTMDY6        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       RtvCltID(CltId);
       If CltId = '129';
          *In71 = *On;
       Else;
          *In71 = *Off;
       EndIf;

       //*** Begin Add 08/21/18 ******************************************************
       If CltId = '126';
          *In19 = *On;
       Else;
          *In19 = *Off;
       EndIf;
       //*** End   Add 08/21/18 ******************************************************
      /End-Free
     C*** End Add ***  11/25/13  ******************************************************
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQTRDT        XXTRDT
     C                   MOVE      QQSEQ#        XXSEQ#            5 0
     C                   MOVE      '/'           XXSLSH            1
     C                   MOVE      '/'           XXSLS2            1
     C                   MOVE      '&'           XXAMPR            1
     C                   MOVE      '&'           XXAMP2            1
     C                   MOVE      *BLANKS       XXBLK2            2
     C                   MOVE      *BLANKS       XXBLK1            1
     C                   SETOFF                                       17
     C*
       // beg add *** 10/20/16 ******************************
      /Free
          *In62 = *Off;
       If CltId = '034' and XXFND=015;
          *In62 = *On;
       EndIf;
      /End-Free
       // end add *** 10/20/16 ******************************
     C                   Z-ADD     XXTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY6
     C                   MOVEL     SRDTO         @@DATE            6 0
      *
     C                   MOVEL     'VOUCHER'     XXVCH             7
     C*
     C****** PERFORM CVTHEX TO GET ATRN VALUE TO PASS TO THE PRINTER **
     C*****                                                          **
     C                   MOVEL     '03011B'      CHR             256
     C                   MOVE      *BLANKS       HEX             128
     C                   MOVE      *BLANK        RTCODE            1
     C                   CALL      'CVTHEX'
     C                   PARM                    CHR
     C                   PARM                    HEX
     C                   PARM                    RTCODE
     C                   MOVEL     HEX           ESC               3
      *** BEG ADD *** 11/28/05 **************************************
     C*** Beg add *** 04/10/19 **************************************
     C                   MOVE      *OFF          *IN63
     C*** End add *** 04/10/19 **************************************
     C*
     C* Check data client data area
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*
     C*** Begin add *** 04/27/07 ************************************
     C                   MOVEL     WCA(3)        XXTXT1            7
     C*** End add ***** 04/27/07 ************************************
      *** BEG ADD ***** 06/20/07 **************************************
     C*** CHANGED LENGHT OF 18 TO 19
     C*
     C                   MOVE      *BLANKS       @@VOID           19
      *** END ADD ***** 06/20/07 **************************************
      /Free
       //**** Begin Add 03/06/19 *****************************************
       @@Void23 = *blanks;
       //**** End   Add 03/06/19 *****************************************
      /End-Free
      *** BEGIN ADD *** 07/26/02 **************************************
     C                   MOVE      'N'           CKVOID            1            CK VOID DAYS
      *** END ADD ***** 07/26/02 **************************************
     C*
     C                   SELECT
     C     ##CLT#        WHENEQ    '136'
     C                   MOVE      *ON           *IN15                          COND XXX
     C                   MOVE      *BLANKS       @@VOID
     C                   MOVE      *ON           *IN78                          1ST UNDERLINE
     C*
      *** BEG ADD *** 11/29/05 **************************************
     C     ##CLT#        WHENEQ    '139'
     C                   MOVEL     VOID90        @@VOID
     C                   MOVE      *ON           *IN78                          1ST UNDERLINE
      *** END ADD *** 11/29/05 **************************************
     C*
     C*
      ***** BEG ADD ***** 02/26/07 ************************************
     C     ##CLT#        WHENEQ    '090'
     C                   MOVEL     VOID90        @@VOID
      ***** BEG CHG ***** 04/11/07 ************************************
     C*************        MOVE *ON       *IN78            1ST UNDERLINE
     C                   MOVE      *OFF          *IN78                          1ST UNDERLINE
      ***** END CHG ***** 04/11/07 ************************************
      ***** END ADD ***** 02/26/07 ************************************
     C*
     C*** Begin add *** 04/27/07 ************************************
     C     ##CLT#        WHENEQ    '071'
     C                   MOVEL     VOID90        @@VOID
     C                   MOVE      *BLANKS       XXTXT1            7
     C                   MOVE      *ON           *IN78                          1ST UNDERLINE
     C*** End add ***** 04/27/07 ************************************
     C*
     C***  Begin Add  ***  10/03/07  **************************************
     C     ##CLT#        WHENEQ    '045'
     C***  Begin Add  ***  02/05/13  **************************************
     C     ##CLT#        OREQ      '046'
     C***  End   Add  ***  02/05/13  **************************************
     C                   MOVE      *ON           *IN78                          1ST UNDERLINE
     C***  End   Add  ***  10/03/07  **************************************
     C*
     C                   OTHER
     C*
      *** BEGIN ADD *** 07/26/02 **************************************
     C                   MOVE      'Y'           CKVOID                         CK VOID DAYS
      *** END ADD ***** 07/26/02 **************************************
      *
     C                   MOVE      *OFF          *IN78
      *
      *** BEG DEL ***** 06/20/07 **************************************
     C***********          MOVE *BLANKS   @@VOID 18
      *** END DEL ***** 06/20/07 **************************************
     C                   ENDSL
     C*
      *** END ADD *** 11/28/05 **************************************
     C*
      *** BEG DEL *** 03/25/11 **************************************
      ****BEGIN ADD *** 08/22/07 **************************************
     C*********  ##CLT#    IFEQ '114'
     C*********            MOVE *OFF      *IN21
     C*********            ENDIF
      ****END ADD ***** 08/22/07 **************************************
      *** END DEL *** 03/25/11 **************************************
      *** BEG ADD *** 03/25/11 **************************************
      *** BEG ADD *** 03/25/11 **************************************
     C*
     C*** INDICATORS TO CONTROL MICR LINE FOR BASE/CLIENTS
     C*** *IN20 MUST BE ON FOR BASE TO PRINT. OFF TO PRINT MODS.
     C*
     C                   SELECT
     C*
     C* CLT114 REVERSED AS THEY HAVE A CLIENT VERSION
     C*
     C     ##CLT#        WHENEQ    '114'
     C                   MOVE      *ON           *IN20
     C                   MOVE      *OFF          *IN21
     C* CLT146
     C     ##CLT#        WHENEQ    '146'
     C                   MOVE      *OFF          *IN20
     C                   MOVE      *ON           *IN22
      *** Beg Add *** 01/21/16 **************************************
     C* CLT146
     C     ##CLT#        WHENEQ    '065'
     C                   MOVE      *OFF          *IN20
     C                   MOVE      *ON           *IN23
      *** End Add *** 01/21/16 **************************************
      *** Beg Add *** 06/18/18 **************************************
     C* CLT129
     C     ##CLT#        WHENEQ    '129'
     C                   MOVE      *OFF          *IN20
     C                   MOVE      *ON           *IN24
      *** End Add *** 06/18/18 **************************************
     C* BASE
     C                   OTHER
     C                   MOVE      *ON           *IN20
     C                   ENDSL
      *** END ADD *** 03/25/11 **************************************
     C*
      *** BEGIN ADD *** 05/15/97 **************************************
      *
      *  RETRIEVE ADJUSTER HEADING FROM CLAIMS/DARTS CONTROL FILE.
      *
     C     CXKY          CHAIN     PCX01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     CXADJ         ANDNE     *BLANKS
      *
     C                   MOVEL     CXADJ         X1ADJ
     C                   ELSE
      *
     C                   MOVEL     XXADJ         X1ADJ
      *
     C                   ENDIF
      *** END   ADD *** 05/15/97 **************************************
      *
      *  RETRIEVE FUND MASTER RECORD
      *
     C     FNKY          CHAIN     WMFNL                              51
     C     *IN51         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
      *** beg add *** 04/10/19 ****************************
     C*** Begin Del *** 09/10/19 **************************
     C*****FNGRPASC      IFEQ      'OPTACOMP'
     C**********         MOVE      *on           *in20
     C**********         ELSE
     C**********         MOVE      *off          *in20
     C**********         END
      *** end add *** 04/10/19 ****************************
     C*** End   Del *** 09/10/19 **************************
      *
     C                   MOVE      *BLANKS       XXFN             60
     C                   MOVE      *BLANKS       XXFAD1           40
     C                   MOVE      *BLANKS       XXFAD2           40
     C                   MOVE      *BLANKS       XXFCT            25
     C                   MOVE      *BLANKS       XXFST             2
     C                   MOVE      *BLANKS       XXFZIP            5 0
     C                   MOVEL     FNNAME        XXFN
     C                   MOVE      FNMAD1        XXFAD1
     C                   MOVE      FNMAD2        XXFAD2
     C                   MOVE      FNMCTY        XXFCT
     C                   MOVE      FNMST         XXFST
     C                   MOVEL     FNMZIP        XXFZIP
     C     XXFAD2        COMP      *BLANKS                                65
      *
      *  RETRIEVE CLAIM CHECK PAYMENT RECORD
      *
     C     PAKY          CHAIN     WDPAL5                             51
     C     *IN51         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C*** Begin Add *** 04/10/19 *******************************************
B04  C     PAINV#        IFNE      *BLANK
     C                   MOVEL     PAINV#        @@INV#
     C                   MOVE      '1'           *IN63
E04  C                   ENDIF
     C*** End   Add *** 04/10/19 *******************************************
      *
      *  Retrieve Payment Detail Detail record
      *
     C*** Begin add *** 05/22/03 **************************************
     C     R2KY          CHAIN     PRP02                              51
     C     *IN51         IFEQ      *ON
     C*** End add ***** 05/22/03 **************************************
     C     RPKY          CHAIN     PRP01                              51
     C*** Begin add *** 05/22/03 **************************************
     C                   ENDIF
     C*** End add ***** 05/22/03 **************************************
     C     *IN51         IFEQ      *OFF
     C*** Beg Add *** 07/25/17 *************************************
     C* Get the system date and store in initial date field
     C*
     C                   eval      sys_date = %date
     C     *cymd         Move      sys_date      xxIdt
     C                   time                    DSdtTm
        //  Determine if this module get spool file and/or XML output
      /Free
                         clear xmlFileName;
                         clear xmlLink;
                         isUseSpoolChk = UseSpoolOut('Claim Checks');
                         isUseXML = UseScriptura('Claim Checks')
                                    or Use3rdXml('Claim Checks');
                         if isUseXML;
                           isScriptChk = 'Y';
                         else;
                           isScriptChk = 'N';
                         Endif;
                         If        isScriptChk = 'Y';
                           Exsr      ScriptXml;
                         Endif;
      /end-free
     C*** End Add *** 07/25/17 ***************************************
     C*
     C*  RETRIEVE REMARKS
     C*
     C                   MOVEL     RPRMK1        XXKEY
     C     TBKY          CHAIN     PRC01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      RCDESC        XXREM1           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXREM1
     C                   END
     C*
     C                   MOVEL     RPRMK2        XXKEY
     C     TBKY          CHAIN     PRC01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      RCDESC        XXREM2           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXREM2
     C                   END
     C*
     C                   MOVEL     RPRMK3        XXKEY
     C     TBKY          CHAIN     PRC01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      RCDESC        XXREM3           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXREM3
     C                   END
     C*
     C                   MOVEL     RPCKCN        XXKEY                          JJJ 1-6-89
     C     TBKY          CHAIN     PRC01                              51        |  ADDED
     C     *IN51         IFEQ      '0'                                          |
     C                   MOVE      RCDESC        XXREM4           40            |
     C                   ELSE                                                   |
     C                   MOVE      *BLANKS       XXREM4                         |
     C                   END                                                    |
     C                   ENDIF
      *
      *  LOAD DOLLAR AMOUNT SPELLED OUT
      *
     C                   MOVE      PAAMT         QQAMT             9
     C                   MOVE      *BLANKS       QQAMT1           60
     C                   MOVE      *BLANKS       QQAMT2           60
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRAMNT'
     C                   PARM                    QQAMT
     C                   PARM                    QQAMT1
     C                   PARM                    QQAMT2
     C                   PARM                    QQTERM
      *
      *  RETRIEVE CLAIM  RECORD
      *
     C     CMKY          CHAIN     WMCML1                             51
     C     *IN51         IFEQ      '1'
     C                   MOVE      *ZEROS        CMSS#
     C                   MOVE      *BLANKS       CMACDT
     C                   MOVE      *BLANKS       CMCLEX
     C                   END
      *
      * GET CLAIMS EXAMINER TITLE
     C                   MOVE      CMCLEX        XXCLEX            3
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       UPTITL
     C                   MOVEL     *BLANKS       UPUSNM
     C                   ELSE
     C                   MOVEL     UPUSNM        XXUSNM           40
     C                   ENDIF
      *
      *** GET EMPLOYER NAME
      *
     C     EMKEY         CHAIN     PEM01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     EMNAME        XXENAM           40
     C                   ELSE
     C                   MOVEL     *BLANKS       XXENAM
     C                   END
      *
      *  RETRIEVE CLAIMANT DEMOGRAPHICS RECORD
      *
     C     CDKY          CHAIN     WMCDL                              51
     C     *IN51         IFEQ      '1'
     C                   MOVE      *BLANKS       XXNAME           47
     C                   ELSE
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        X1NAME           40
     C                   END
      *
      *  RETRIEVE PAYEE DEMOGRAPHICS RECORD
      *
     C***        PDKY      CHAINWMPDL                51
     C***        *IN51     IFEQ '1'
     C***                  MOVELX1NAME    X2NAME
     C***                  MOVELCDADR1    PDADR1
     C***                  MOVELCDCTY     PDCTY
     C***                  MOVELCDST      PDST
     C***                  MOVE CDZIP     PDZIP
     C***                  MOVE CDSS#     PAFEIN
     C***                  ELSE
     C***                  MOVE *BLANKS   XXLAST
     C***                  MOVE *BLANKS   XXFRST
     C***                  MOVELPDNAME    XXLAST
     C***                  MOVELPDFSN     XXFRST
     C***                  MOVE '1'       XXTERM
     C***                  CALL 'SRNAME'
     C***                  PARM           XXLAST
     C***                  PARM           XXFRST
     C***                  PARM           XXTERM
     C***                  MOVELXXLAST    X2NAME 40
     C***                  END
      *
      * GET PAYMENT VALUE ONE DESCRIPTION
      *
     C                   MOVEL     PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              51
     C     *IN51         IFEQ      '1'
     C                   MOVE      *BLANKS       B2DESC
     C                   END
     C                   MOVEL     B2KEY         XXB2D2            3
     C                   MOVEL(P)  B2DESC        XXB2DS           30
      *
      *  RETRIEVE RESERVE CODE DESCRIPTION
      *
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     PARECD        XXKEY
     C     TBKY          CHAIN     PRT01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     RTDESC        XXRTDS           15
     C                   MOVEL     RTDESC        XXRTD2           30
     C                   ELSE
     C                   MOVE      *BLANKS       XXRTDS
     C                   END
     C                   MOVEL     *BLANKS       XXKEY
     C*** Beg Add *** 07/25/17 ***************************************
     C                   move      qqTrDt        x1Date
     C                   z-add     x1date        SRdti
     C                   call      'SRCVTDAT'    DTmdyy
     C                   movel     SRdto         x1Date            8 0
     C*** End Add *** 07/25/17 ***************************************
      *
      *  REVERSE DATES TO PRINT
      *
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY6
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*** Beg Add *** 07/25/17 ***************************************
     C                   z-add     xxFrDt        SRdti
     C                   call      'SRCVTDAT'    datChk
     C                   movel     SRdto         x2FrDt            8 0
     C*** End Add *** 07/25/17 ***************************************
      *
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY6
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*** Beg Add *** 07/25/17 ***************************************
     C                   z-add     xxToDt        SRdti
     C                   call      'SRCVTDAT'    datChk
     C                   movel     SRdto         x2ToDt            8 0
     C*** End Add *** 07/25/17 ***************************************
      *
      *  LOAD CITY, STATE and ZIP from Payment Detail Detail file
      *
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     RPST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZIP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
      *
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     RPCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ            40
     C*
     C*** BEG ADD *** 10/20/03 *****************************
     C     RPCTRY        IFNE      *BLANKS
     C     RPCTRY        ANDNE     'USA'
     C                   MOVEL(P)  RPADR3        XXCSZ
     C                   MOVEL(P)  'CTRY'        XXTBID
     C                   MOVEL(P)  RPCTRY        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        XXADR4           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXADR4
     C                   ENDIF
     C*
     C********             MOVELRPADR3    XXADR4    P
     C                   END
     C*** END ADD *** 10/20/03 *****************************
      *
      *  Check for a Blank Second Address Line
      *
     C                   MOVE      '0'           *IN61
     C     RPADR2        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN61
     C                   END
      *
     C                   MOVE      *OFF          *IN75                          Regular Check
      *
      *  Access Bank Account Master to obtain Voucher Flag (Y/N)
      *
     C     CKKEY         CHAIN     PCK01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     CKVCHR        ANDEQ     'Y'
      *** BEG ADD ***** 07/27/09 **********************************
     C     *IN60         OREQ      *OFF
     C     CKVCHR        ANDEQ     'X'
     C                   SELECT
     C     CKVCHR        WHENEQ    'Y'
     C                   MOVEL     VCHR1         VOUCHR            7
     C     CKVCHR        WHENEQ    'X'
     C                   MOVEL     VCHR2         VOUCHR            7
     C                   ENDSL
      *** END ADD ***** 07/27/09 **********************************
     C                   MOVE      *ON           *IN75                          Voucher NO MICR
     C                   ELSE
     C                   MOVE      *OFF          *IN75                          Check   MICR
     C                   ENDIF
      *
      *    GET BANK INFORMATION
      *
     C                   MOVE      'WC'          XXAPPL
     C                   Z-ADD     XXTRDT        XXEFDT
      *
     C     BIKEY         SETLL     PBI01
     C     BIKEY1        READE     PBI01                                  51
      *
     C     *IN51         DOWEQ     *OFF
     C     BISTAT        ANDEQ     'I'
     C     BIKEY1        READE     PBI01                                  51
     C                   ENDDO
      *
     C     *IN51         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
      *
      *** BEGIN ADD *** 07/26/02 **************************************
      *
      ** ONLY CHECK FOR # OF VOID DAYS IF NO CLIENT MOD ALREADY EXISTS
      *
     C     CKVOID        IFEQ      'Y'
      *
      * This code initializes the 'Void after x days' display field
      * If the number of days (BI50N3) is *zero the message
      * field (XVOAFT) is blanked out
      *
     C                   MOVE      *BLANKS       XVOAFT
      /Free
       //**** Begin Add 03/06/19 *****************************************
       @@Void23 = *blanks;
       //**** End   Add 03/06/19 *****************************************
      /End-Free
      *
     C     BI50N3        IFGT      *ZERO
     C                   MOVE      BI50N3        XVDAYS            3 0
     C                   MOVE      XVDAYS        VDY
     C                   MOVE      *BLANKS       XXVDAY            3
      *
     C     XVDAYS        IFLT      10
     C                   MOVE      *BLANK        VDY1
     C                   MOVE      *BLANK        VDY2
     C                   MOVEL     VDY3          XXVDAY
      *
     C                   ELSE
      *
     C     XVDAYS        IFLT      100
     C                   MOVE      *BLANK        VDY1
     C                   MOVEL     VDY2          XXVDAY
     C     XXVDAY        CAT       VDY3:0        XXVDAY
      *
     C                   ELSE
      *
     C     XVDAYS        IFGT      99
     C                   MOVE      VDY           XXVDAY
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL     XVMSG1        XVOAFT           19
     C     XVOAFT        CAT       XXVDAY:1      XVOAFT
     C     XVOAFT        CAT       XVMSG2:1      XVOAFT
     C                   MOVEL     XVOAFT        @@VOID
      /Free
       //**** Begin Add 03/06/19 ********************************************
       @@Void23 = '- ' + %xlate(Lower:Upper:xvmsg1) + ' ' +
                   %trim(XXVDAY) + ' ' +  %xlate(Lower:Upper:XVMSG2) + ' -';
       //**** End   Add 03/06/19 ********************************************
      /End-Free
     C                   ENDIF
     C                   ENDIF
      *
      *** END ADD ***** 07/26/02 **************************************
      *
      *  Assign MACRO to PCL5 control characters with Bank Information Macro ID
      *
     C                   MOVE      BIMACR        TEMP3             4
     C     '&f'          CAT       TEMP3         TEMP5             6
     C     TEMP5         CAT       'y3X'         MACRO             9
      *
     C     BIAXOU        IFEQ      *BLANK
     C                   MOVE      *BLANKS       XXAXOU
     C                   MOVE      XXCNBR        XXAXOU           20
     C                   ELSE
     C                   MOVE      BIAXOU        XXAXOU
     C                   END
      *
     C     BIBNK#        CAT(P)    '/'           XXBT#5            5
     C     XXBT#5        CAT(P)    BITRNS        XXBT#             9
      *
      * CHECK FOR AN AMOUNT IN BI$LMT
      *
     C                   MOVE      *OFF          *IN85
     C                   MOVE      *OFF          *IN83
     C     BI$LMT        IFGT      0
     C     PAAMT         ANDGT     BI$LMT
     C                   MOVE      '1'           *IN85
     C                   MOVE      *ON           *IN83
     C*** beg add *** 04/11/18 *************************************
      /Free
       RtvCltID(CltId);
       //** Begin Chg *** 05/21/18 *********************************
       //*** If CltId = '129';
       If CltId = '129' And BI1A2 = 'Y';
       //** End   Chg *** 05/21/18 *********************************
          Lines129 = *On;
       Else;
          Lines129 = *Off;
       EndIf;
      /End-Free
     C*** end add *** 04/11/18 *************************************
     C                   END
      *
      * CHECK FOR A SIGNATURE MACRO ID
      *
     C     BI10A1        IFNE      *BLANK
     C                   MOVEL     BI10A1        CH1               7
     C                   MOVE      '1'           *IN86
     C*** beg add *** 04/11/18 *************************************
     C                   MOVE      *ON           *IN84
     C*** end add *** 04/11/18 *************************************
     C                   END
      *
      * CHECK FOR SECOND SIGNATURE MACRO ID
      *
     C     BI10A2        IFNE      *BLANK
     C                   MOVEL     BI10A2        CH2               7
     C                   MOVE      '1'           *IN87
     C                   MOVE      *ON           *IN83
     C                   END
      *
     C*** BEGIN ADD *** 02/10/99 *************************************
     C*  Do NOT print the second signature line if the MACRO ID is
     C*    something other than 2.
     C*
     C     ##CLT#        IFEQ      '090'
     C     BIMACR        IFNE      2
     C                   MOVE      *OFF          *IN83
     C                   END
     C                   ENDIF
     C*
     C*** beg add *** 04/11/18 *************************************
      /Free
       // On indicators to print lines, Off to suppress signatures.
       If Lines129 = *on;
          *In84 = *On;
          *In78 = *On;
          *In86 = *Off;
          *In87 = *Off;
       EndIf;
      /End-Free
     C*** end add *** 04/11/18 *************************************
     C*** END ADD ***** 02/10/99 *************************************
      *
      *  PRINT FUND SHORT NAME IF NO LOGO
      *
     C     BILOGO        IFEQ      'N'
     C                   MOVE      '1'           *IN73
     C                   END
      *
      *  PRINT TRUST NAME ON VOUCHER BASED ON ARRAY
      *
     C     BITRST        IFEQ      'Y'
     C                   MOVEL     WCA(2)        XXWCA            35
     C                   ELSE
     C                   MOVEL     WCA(1)        XXWCA            35
     C                   END
      *
     C     PAFEIN        IFGT      *ZERO
     C*** BEG DEL *** 11/28/05 **************************************
     C***********          Z-ADDPAFEIN    @@FEIN
     C*** END DEL *** 11/28/05 **************************************
     C*** BEG ADD *** 11/28/05 **************************************
     C                   MOVE(P)   PAFEIN        @@FEIN            9
     C   15              MOVEL     'XXXXX'       @@FEIN
     C*** END ADD *** 11/28/05 **************************************
     C                   MOVEL     PAMOD         @@MOD
     C                   MOVE      *ON           *IN44
     C                   ELSE
     C*** BEGIN ADD *** 10/05/98 **************************************
     C******************** MOVE CDSS#     @@FEIN
     C*** END ADD ***** 10/05/98 **************************************
     C                   MOVE      *ZERO         @@MOD
     C                   MOVE      *OFF          *IN44
     C                   ENDIF
      *
     C                   Z-ADD     PACASE        @@CASE
     C                   Z-ADD     PACHK#        @@CHK#
     C                   MOVEL     X1NAME        @@NAME
     C                   Z-ADD     PAAMT         @@AMT
     C                   MOVEL     QQAMT1        @@AMT1
     C                   MOVEL     QQAMT2        @@AMT2
     C                   MOVEL     RPPNAM        @@PNAM
     C                   MOVEL     RPADR1        @@ADR1
     C                   MOVEL     RPADR2        @@ADR2
     C                   MOVEL     PAACCT        @@ACCT
     C                   MOVEL     PACO#         @@CO#
     C                   MOVEL     PAFND         @@FND
     C                   MOVEL     PAFYR         @@FYR
     C                   MOVEL     PABR2         @@BR2
     C                   MOVEL     PARECD        @@RECD
     C                   MOVEL     PAWKS         @@WKS
     C                   MOVEL     PADAYS        @@DAYS
      *
     C*** BEGIN ADD *** 10/05/98 **************************************
     C                   MOVE      CMSS#         XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYP            25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C*** BEG DEL *** 11/28/05 **************************************
     C*******              MOVELSSPRE     @@SS#  11 P
     C*******              CAT  '-':0     @@SS#
     C*******              CAT  SSMID:0   @@SS#
     C*** END DEL *** 11/28/05 **************************************
     C*** BEG ADD *** 11/28/05 **************************************
     C*** BEG CHG *** 10/12/07 **************************************
     C     XXCO#         CHAIN     PCO01                              99
     C     *IN99         IFEQ      *OFF
     C     COPSS#        ANDEQ     'Y'
     C  N15              MOVEL(P)  SSPRE         @@SS#            11
     C   15              MOVEL(P)  'XXX'         @@SS#            11
     C                   CAT       '-':0         @@SS#
     C  N15              CAT       SSMID:0       @@SS#
     C   15              CAT       'XX':0        @@SS#
     C                   CAT       '-':0         @@SS#
     C                   CAT       SSSUF:0       @@SS#
     C                   ELSE
     C***  Begin Add  ***  06/09/20  *******************************
     C                   EVAL      XXSS#=*BLANKS
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVEL(P)  *BLANKS       @@SS#
     C                   ENDIF                                                  COPSS# = 'Y'
     C**N15****            MOVELSSPRE     @@SS#  11 P
     C***15****            MOVEL'XXX'     @@SS#  11 P
     C**********           CAT  '-':0     @@SS#
     C**N15*****           CAT  SSMID:0   @@SS#
     C**********           CAT  'XX':0    @@SS#
     C*** END ADD *** 11/28/05 **************************************
     C**********           CAT  '-':0     @@SS#
     C**********           CAT  SSSUF:0   @@SS#
     C*** END ADD ***** 10/05/98 **************************************
     C*** END CHG *** 10/12/07 **************************************
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY6
     C                   MOVEL     SRDTO         @@ACDT            6 0
     C*** Beg Add *** 07/25/17 ***************************************
     C                   z-add     CMacDt        SRdti
     C                   call      'SRCVTDAT'    DTmdyy
     C                   movel     SRdto         x1AcDt            8 0
     C*** End Add *** 07/25/17 ***************************************
      *
     C                   MOVE      *OFF          *IN66
     C     PARETP        IFEQ      'M'
     C                   MOVE      *ON           *IN66
     C                   ENDIF
      *
     C                   MOVE      *OFF          *IN07                          NOT Group Type
     C                   MOVE      *ON           *IN77                          First Time
      *
      *  OPEN VOUCHER SPOOL FILE, PRINT VOUCHER AND STUB, THEN CLOSE FILE
      *
     C                   OPEN      CHECKS
     C                   EXCEPT    CHECK
     C                   EXCEPT    DET2
      ***  BEGIN ADD  ***  04/25/05 ***********************************
      * PRINT FRAUD STATEMENT IF INDICATED
     C     B2FLG1        IFEQ      'Y'
     C     CMJRST        ANDEQ     'CA'
     C                   EXCEPT    FRAUD
     C                   ENDIF
      ***  END   ADD  ***  04/25/05 ***********************************
     C                   CLOSE     CHECKS
     C*** Beg Add *** 07/25/17 ***************************************
      /Free

       //* Write to Scriptura Workfile
                         If isScriptChk = 'Y';
                            Exsr wrtSXCCP;
                            Select;
                            When xmlFileName   = *Blanks;
                               When objExists('PDSDEV':'*LIB');
                               Monitor;
                                 S4401C(xmlFileName + '$');
                               On-error;
                               Endmon;
                            Other;
                               S4401C(xmlFileName + '$');
                            Endsl;
                         Endif;
      /End-free
     C*** End Add *** 07/25/17 ***************************************
      *
     C     TERM          TAG
     C                   SETON                                        LR
       //*** Beg Add 07/25/17 ********************************************
      /Free

       //-------------------------------------------------------------
       // WrtSXCCP - Write to Scriptura Claim Check workfile
       //--------------------------------------------------------------
           Begsr WrtSXCCP;

              Clear scc0p;

              s_CCXML  = xmlFile;
              s_CCAXOU = xxAxou;
              s_CCTRAN = xxTran;
              s_CCONUS = BIonus;
              s_CCRTUP = BIrtUp;
              s_CCRTLW = BIrtLw;
              s_CCCHK# = RPchk#;
              s_CC40A1 = BI40A1;
              s_CC40A2 = BI40A2;
              s_CC40A3 = BI40A3;
              s_CC40A4 = BI40A4;
              monitor;
                s_CCDATE = %dec(%date(x1Date: *usa));
              on-error;
                s_CCDATE = 0;
              endmon;
              s_CCCLM# = xxClm#;
              s_CCCLNM = @@name;
      //***  Begin Chg  ***  06/09/20  *******************************
      //****  s_CCSS#  = @@ss#;
              s_CCSS#  = xxss#;
      //***  End Chg  ***  06/09/20  *******************************
              s_CCFEIN = @@fein;
              monitor;
                s_CCACDT = %dec(%date(x1AcDt: *usa));
              on-error;
                s_CCACDT = 0;
              endmon;
              s_CCAI01 = BIai01;
              s_CCAMT  = PAamt;
              s_CCAMT1 = @@amt1;
              s_CCAMT2 = @@amt2;
              s_CCPNAM = @@pnam;
              s_CCPAD1 = @@adr1;
              s_CCPAD2 = @@adr2;
              s_CCPAD3 = xxCSZ;
              s_CCPAD4 = xxAdr4;
              s_CCVOID = @@void;
              s_CCSGMS1= SIGMS1(1);
              s_CC$LMT = BI$lmt;
              s_CCSGMS2= SIGMS2(1);
              s_CCCMD1 = BIcmd1;
              s_CCCMD2 = BIcmd2;
              s_CCACCT = @@Acct;
              s_CCVCHR = vouchr;
              s_CCTXT1 = xxTxt1;
              s_CCFNAME= xxFn;
              s_CCFAD1 = xxFad1;
              s_CCFAD2 = xxFad2;
              s_CCFCTY = xxFct;
              s_CCFST  = xxFst;
              s_CCFZIP = xxFzip;
              s_CCMOD  = @@mod;
              s_CCEMP# = EMemp#;
              s_CCDIV  = EMdiv;
              s_CCEMNM = EMname;
              s_CCCO#  = RPco#;
              s_CCFND  = RPfnd;
              s_CCFYR  = RPfyr;
              s_CCCASE = RPcase;
              s_CCADJH = x1Adj;
              s_CCCLEX = CMClex;
              s_CCUSNM = xxUsNm;
              s_CCBR2  = PAbr2;
              s_CCB2DS = xxB2Ds;
              s_CCRECD = @@reCd;
              s_CCRCDS = xxRtD2;
              monitor;
                s_CCFRDT = %dec(%date(x2FrDt: *usa));
              on-error;
                s_CCFRDT = 0;
              endmon;
              s_CCTODT = x2ToDt;
              monitor;
                s_CCTODT = %dec(%date(x2ToDt: *usa));
              on-error;
                s_CCTODT = 0;
              endmon;
              s_CCWKS  = @@wks;
              s_CCDAYS = @@days;
              s_CCREM1 = xxRem1;
              s_CCREM2 = xxRem2;
              s_CCREM3 = xxRem3;
              s_CCREM4 = xxRem3;
              s_CCRMDS = RPrmds;
              s_CCiDt  = xxIdt;
              Write sCC0p;
           Endsr;
       // *=========================================================================
       // *  ScriptXML - Reserve/Update XML record for Scriptura output.
       // *=========================================================================
       Begsr ScriptXML;

       xmlFile = ' ';
       message = %trim(processType);
       outstring = ' ';
       x1Co# = %char(PAco#);
       x1Fnd = %char(PAfnd);

       StLetter('XMLREQUEST':xmlFile:message:'M':'000000000':xxPre:
        outString:x1Co#:x1Fnd:' ':xmlLink);

       If xmlFileName = ' ';
          xmlFileName = xmlFile;
       Endif;

       If xmlLink = ' ';
          Chain (xmlFile) xp100;
          xmlFile = XPXmlT;
       Else;
          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
       Endif;

       XPData = 'Y';

       XPPgm  = Q1PGM;
       XPUser = Q1USER;
       XPTime = DsTime;
       XPDt   = xxIdt;

       Update xp100;

       Endsr;
      /end-Free
       //*** End Add 07/25/17 ********************************************
      *********************************************************************
      *
     O/COPY OCHKCHECK
      *_________________________________________________________________
      *
      *  Copybook for file CHECKS except name DET2
     O/COPY OCHKDET2
      *_________________________________________________________________
      *** begin add ***  07/25/17  ****************************************
     P*====================================================================
     P* Determine if the object exists
     P*====================================================================
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
      *** end add *****  07/25/17  ****************************************
**  CONSTANTS
WORKERS' COMPENSATION ACCOUNT
ACORDIA WORKERS' COMPENSATION ACCT.
Non-Negotiable after 90 Days
** SIGNATURE MESSAGE PART ONE
OVER
** SIGNATURE MESSAGE PART TWO
 REQUIRES SECOND SIGNATURE
