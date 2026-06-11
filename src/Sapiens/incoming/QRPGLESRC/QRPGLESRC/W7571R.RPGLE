     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7571R - Self Reporting Late Notices
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS IS THE PROMPT PROGRAM FOR SELF-REPORTING LATE NOTICES.   *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W7571C                                                      *
     H* CALLS TO:                                                     *
     H*   *NONE                                                       *
     H*                                                               *
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*   08  BATCH MODE                                              *
     H*   09  DEMAND MODE                                             *
     H*   50  DO LOOP FOR PROCESSING PEB01                            *
     H*   60  GENERAL CHAIN & LOKUP                                   *
     H*   66  WMFNL/FNCGFL = "Y"                                      *
     H*   79  COMMERCIAL FUND                                         *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/18/22 C:79  Support: 140/PHW101484                         *
     H*                Populate L8prtBal for Scriptura Criteria File  *
     H*                                                               *
     H* 11/02/21 C:300 Support: 98948 (UBIC)                          *
     H*                Load L8RPTDT with the date of the last day of  *
     H*                month/year being loaded to L8RPTMTH.           *
     H*                                                               *
     H* 05/18/21 C:65  Project: WC2760                                *
     H*                Add processing to generate report in Scriptura *
     H*                and ensure report only mode works.             *
     H*                                                               *
     H* 11/11/20 C:062 Project: WC2760                                *
     H*                Add Scritura processing                        *
     H*                                                               *
     H* 12/11/15 C:062 Support: 35065(133) 133WNI                     *
     H*                Change date Format'                            *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/13/15 C:53  Ticket: 35065
     H*                Format phone # correctly.
     H*
     h* 08/26/14 C:307 Project: SY354                                 *
     h*                -Implement Routing Page Changes.               *
     h*                                                               *
     H* 08/17/11 C:190  Project: WC2019                               *
     H*                 Pull the cancellation notice days from        *
     H*                 the new fields for new policies.              *
     H*                                                               *
     H* 05/23/11 C:136 SUPPORT: WA099 73548                           *
     H*                ADJUST A/R BALANCE CALC'S FOR NEW DEPOSIT      *
     H*                REFUND AND VOID CODES.                         *
     H*                                                               *
     H* 09/24/10 C:33  Seq#: WA132/74182                              *
     H*               -Write additional remarks appropriately.        *
     H*               -Print a seperate spoolfile for the combined    *
     H*                late notices.                                  *
     H*                                                               *
     H* 05/20/10 C:220 PROJECT: 72456                                 *
     H*                Pass &RTCD = '*' to skip the deletion of the   *
     H*                spoolfile : X-REF 72102                        *
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/01/09 C:189 PROJECT: 102WEW Priss 7                        *
     H*                 Remove the old DARTS code from generating     *
     H*                                                               *
     H*                                                               *
     H* 03/21/09 C:189 PROJECT: 102WEW                               *
     H*                 SR Lt Notice-Resolve to Darts/Cert Ltrs       *
     H*                                                               *
     H* 12/19/08 C:33  Project: 102WEP   WA102/66490                  *
     H*                Calculate the "Report by" date based upon the  *
     H*                 greater of "Due date" or current date for 102.*
     H*                                                               *
     H* 08/21/08 C:191 PROJECT: 102WEP                                *
     H*                BACK TO BASE :                                 *
     H*                 - SELECT CRITERIA BY STATE/LATE NOTICES       *
     H*                 - S/R LATE NOTICE - NOTICE OF INTENT          *
     H*                 - S/R LATE NOTICE - REPORT BY DATE CHANGE     *
     H*                                                               *
     H* 01/19/08 C:136 SUPPORT: WA146 62264                           *
     H*                MONTHLY SELF-REPORTING POLICIES ARE BEING      *
     H*                DUNNED IF VENDOR PROCESSING IS ENABLED.        *
     H*                                                               *
     H* 09/17/07 C:183 PROJECT: 131WIM                                *
     H*                WORDING CHANGE ON INSURED LATE NOTICE.         *
     H*                                                               *
     H* 07/18/07 C:136 PROJECT: WC1491                                *
     H*                IMPLEMENT CODING FOR DEPOSIT REFUND AND VOIDS  *
     H*                                                               *
     H* 08/09/06 C:136 PROJECT: 136WBG                                *
     H*                ADD DEPOSIT BY POLICY YEAR MODE                *
     H*                                                               *
     H* 07/07/05 C: 58 PROJECT: WC1205
     H*              - Access activity status code from employer
     H*                detail WDELP rather than master WMEMP.
     H*                                                               *
     H* 06/24/05 C:33  Project: 109WDZ                                *
     H*                {109AM}                                        *
     H*                S/R Employer - call API to determine if should *
     H*                               be considered for processing.   *
     H*                                                               *
     H* 09/24/04 C:53  PROJECT 132WBD                                 *
     H*                ADD AGENT HEADER VARIABLE FIELD                *
     H*                                                                *
     H* 02/19/04 C:136 PROJECT: WC1033                                *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H*  10/27/03  C:33  Project: WC1132                               *
     H*                  Revamp Dunning Process                        *
     H*                                                               *
     H*  08/22/03 C:136 Support: WC1117 #38700                        *
     H*                 Correct verbiage on line 2 of comments.       *
     H*                                                               *
     H*  06/25/03 C:136 SUPPORT: WC1115                               *
     H*                 Correct code that excludes employers from     *
     H*                 being picked up if their billing frequency    *
     H*                 or day of week doesn't match report criteria. *
     H*                                                               *
     H*  06/02/03 C:132 Project: WC1117                               *
     H*                 Changed to use GROUP NAME instead of Office   *
     H*                 Accounting Name if FNCGFL <> "Y" for          *
     H*                 Self-Insured Groups                           *
     H*                                                               *
     H*  05/08/03 C:132 Project: WC1117                               *
     H*                 REMOVED LOCK BOX address check in group       *
     H*                 auxiliary file, as requested by client 129.   *
     H*                                                               *
     H*  02/18/03 C:136 Project: WC1115                               *
     H*                 If vendor processing is enabled, report       *
     H*                 frequency and report date will be used to     *
     H*                 include/exclude policies in late notice.      *
     H*                                                               *
     H*  02/13/03 C:132 Project: WC1117                               *
     H*                 Added LOCK BOX address check in group         *
     H*                 auxiliary file. If present in WMF2P, use the  *
     H*                 Lock box address instead of accounting office *
     H*                 address.                                      *
     H*                                                               *
     H* 12/11/02 C:61  SUPPORT WA081 35144                            *
     H*                Get most recent ACTIVE CX reason code record.  *
     H*                                                               *
     H* 02/28/02 C:61  SUPPORT WA100 31227                            *
     H*                Print Carrier Policy # WMRPP/RPCAP# if entered.*
     H*                Otherwise, print grp/emp/fyr.                  *
     H*                                                               *
     H* 12/20/01 C:61  SUPPORT WA107 30434                            *
     H*                Print "Out of Country" address if entered.     *
     H*                                                               *
     H* 03/14/01  C:35   PROJECT: WC577/SUPPORT                       *
     H*                  PRINT VALUES LESS THAN 0 SO THAT TOTAL MAKES *
     H*                  SENSE.                                       *
     H*                                                               *
     H* 11/17/00  C:35   PROJECT: WC577                               *
     H*                  ADD CODE FOR INCLUDE/EXCLUDE ACTIVITY STATUS *
     H*                  CODES.                                       *
     H*                                                               *
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H*  03/31/00 C:31  Support: 22408
     H*                 1. Add initialization of CMIPGM
     H*                                                               *
     H* 03/03/00 C:118 PROJECT: WC974                                 *
     H*                Add an entry parm for 'Generate report' flag.  *
     H*                Condition the EXCPT statements for the forms so*
     H*                they are executed only if this flag is blank.  *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H* 02/24/99  C:87   PROJECT: SUPPORT TASB #NONE                  *
     H*                  Change to use the current date as opposed to *
     H*                  the cancel date when reading the canx        *
     H*                  reason code table file(WTCRP).               *
     H*                                                               *
     H* 01/21/99  C:87  PROJECT: SUPP                                 *
     H*                 Need to load KKEFDT with the cancel date in   *
     H*                 order to get cancellation reason desc.        *
     H*                 Reset federal name indicator (*IN77).         *
     H*                                                               *
     H* 12/10/98 C:61 SUPPORT 14595                                   *
     H*               Only process active (WMA3P/A3STAT = 'A')        *
     H*               Employer 3rd Address records.                   *
     H*                                                               *
     H*  11/10/98 C:87 PROJECT: WC852                                 *
     H*                Change to use WTCRL instead of WTABL to get    *
     H*                the cancellation reason code record.           *
     H*                                                               *
     H*  08/25/98 C:87 PROJECT: CM033                                 *
     H*                WHEN SORTING BY AGENT NUMBER('AN'), START A    *
     H*                NEW PAGE FOR EACH AGENT# AND SUM NUMBER OF     *
     H*                EMPLOYERS FOR EACH PAGE.                       *
     H*                                                               *
     H*  07/28/98 C:65 PROJECT: CM039                                 *
     H*                USE THE ACCTING OFFICE FIELD FOR DIRECT INQRY. *
     H*                                                               *
     H*  06/01/98 C:65 PROJECT: CM039                                 *
     H*                USE THE ACCOUNTING OFFICE INFO FOR ADDRESSES   *
     H*                AND NAMES. ALSO CHANGE DETACH MESSAGE.         *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                ADDED CTSMCD MOVED TO CMSMCD.                  *
     H*                                                               *
     H*  02/24/98 C:87 PROJECT CM034                                  *
     H*                In Darts data structure- corrected length of   *
     H*                  emp# and div.                                *
     H*                Changed date handling to correct write to      *
     H*                  Darts file.                                  *
     H*                                                               *
     H*  01/23/98 C:87 PROJECT CM034                                  *
     H*                Cancellation message at bottom changed so that *
     H*                  it is not implied that paying premium will   *
     H*                  resolve cancellation status.                 *
     H*                Verbiage changes made to differentiate between *
     H*                  accts pending cancellation and active accts. *
     H*                Adjust output to not print balances(other than *
     H*                  'Total Amount Due') if <= 0.                 *
     H*                                                               *
     H*  06/25/97 C:79 PROJECT WC711                                  *
     H*                V3 CHANGES/STANDARDS                           *
     H*                                                               *
     H*  06/13/97 C:62 PROJECT WC681                                  *
     H*                CHANGE HARD-CODED 800 AREA CODE TO FNPH1A      *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     F* COMPANY MASTER FILE
     FSMCOL     IF   E           K DISK
     F* USER PARAMETER FILE
     FWMUPL     IF   E           K DISK
     F* OUTPUT ROUTING FILE
     FSMORL     IF   E           K DISK
     F* TABLE MASTER FILE
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F*
     FWMRSL     IF   E           K DISK
     FWTCRL     IF   E           K DISK
     F* FUND MASTER FILE
     FWMFNL     IF   E           K DISK
     F* FUND AUXILIARY FILE
     FWDF2L     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     F* FUND DETAIL FILE
     FWDF1L     IF   E           K DISK
     F*  EMPLOYR A/R FILES
     FWMEAL8    IF   E           K DISK
     FWMEAL     IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     F* EMPLOYER MASTER FILE
     FWMEML     IF   E           K DISK
     F*  EMPLOYER DETAIL FILE
     FWDELL     IF   E           K DISK    infds(dbfds)
     F*  EMPLOYER THIRD ADDRESS FILE
     FWMA3L     IF   E           K DISK
     F*  SELF-REPORTING PAYROLL TRANSACTION FILE
     FWASRL2    IF   E           K DISK
     F*  SELF-REPORTING PAYROLL TRANSACTION FILE
     FWASRL8    IF   E           K DISK
     F                                     RENAME(PSR01:PSR02)
     F*  SELECTED EMPLOYER WORKFILE
     FWXEBL     IF   E           K DISK
     F* CANCELLATION DETAIL FILE
     FWDCXL1    IF   E           K DISK
     FWMOFL     IF   E           K DISK
     FWTAVL     IF   E           K DISK
      *
      * Carrier/State Reporting Master
     FWMRPL     IF   E           K DISK
      *
     FWMEXL     IF   E           K DISK
     F* Certificate holders file
     FWMCEL2    IF   E           K DISK
     FWDCXL     IF   E           K DISK
     F                                     RENAME(PCX01:PCX02)
     F*
     FWMAAL1    IF   E           K DISK
     FWMAPL     IF   E           K DISK
     FWMAHL1    IF   E           K DISK
     F* Agent Master
     Fwmagl     if   e           k Disk    Rename(pag01:pag00)
     F* Run Criteria
     Fwxl8psrc  o    e             disk    usropn rename(pl801:pl80psrc)
     F* Group Information
     Fwxl8psr1  o    e             disk    usropn rename(pl801:pl80psr1)
     F* Policy Information
     Fwxl8psr2  o    e             disk    usropn rename(pl801:pl80psr2)
     F* Report Information
     Fwxl8psr3  o    e             disk    usropn rename(pl801:pl80psr3)
     F* Line Item Information
     Fwxl8psr4  o    e             disk    usropn rename(pl801:pl80psr4)
     Fxmlprel1  uf   e           k disk    rename(xp100:xp101)
     FSTSP1L    IF   E           K DISK    rename(psp011:psp010)
     F* LATE NOTICES SPOOL FILE
     FQSYSPRT   O    F   80        PRINTER USROPN
     F*
     FQSYSPRT2  O    F   80        PRINTER USROPN
     F* LATE NOTICE REPORT SPOOL FILE
     FQSYSPRT1  O    F   80        PRINTER OFLIND(*INOA) USROPN
     F* Resolved output
     FQSYSPRT3  O    F   80        PRINTER USROPN

     D*
     D MNTH            S              9    DIM(12) CTDATA PERRCD(1)
     D AS              S              2    DIM(5) ASCEND
     D ASDS            S             30    DIM(30)
     D*
     D XXLIT1          C                   CONST('the month of ')
     D XXLIT2          C                   CONST('which was due by')
     D XXLIT3          C                   CONST('.  To avoid')
     D XXLIT4          C                   CONST('.  Please send the')
     D XXLIT5          C                   CONST('.  Payment and    ')
     D UNDLIN          C                   CONST('_______________')
     D OTHBAL          C                   CONST('Other Previous -
     D                                     Balances:')
     D AGT             C                   CONST('     AGENT')
     D AGTL            C                   CONST('Agent     ')
     D DLTSPL          C                   CONST('DLTSPLF')
     D PRT3            C                   CONST('FILE(QSYSPRT3)')
     D*
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRMCY                 1      6  0
     D  XXMN                   1      2  0
     D  XXYR                   3      6  0
      *
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D QQCMT           DS
     D  QQCMT1                 1      2
     D  QQCMT2                 3      4
     D  QQCMT3                 5      6
     D  QQCMT4                 7      8
     D  QQCMT5                 9     10
     D                 DS
     D  ZZKEY                  1     40
     D  ZZCO#                  1      3  0
     D  ZZFND                  4      6  0
     D  ZZEMP#                 7     15  0
     D  ZZDIV                 16     20  0
     D ##SR            C                   CONST('S/R Late Notice')
     D ##CM            C                   CONST('Document was not abl-
     D                                     e to convert to PDF,-
     D                                      not attached.')
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     d dbfds           ds
     d  dbrrn                397    400b 0

     d qqXML           s             61
     d x#RRN           s              9  0 inz(0)

     d xmlFile         s                   like(L8XML)
     d xmlLink         s                   like(XPLink)
     d xmlFileBlank    s                   like(XPXML)
     d xmlFileName     s                   like(XPXML)
     d xmlRptFile      s                   like(XPXML) inz(' ')

     d isMassReq       s              1n
     d massFlag        s              1

     d isUseSpoolOut   s              1n   inz
     d isUseXML        S              1n   inz

     d string          s             10    inz(*blanks)
     d message         s            250    inz(*blanks)
     d xxPre           s              2    inz('LN')
     d xxProcessType   s            100
     d processType     c                   const('SR Late Notice')

     d count           s              5  0
     d i               s              1  0
     d noLateNotices   s               n   Inz(*On)

     d lineDesc        s             40a
     d lineAmt         s             11s 2

     d*** beg add *****  11/02/21  *****************************************************
     d lastday         s               d
     d*** end add *****  11/02/21  *****************************************************

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
     d
     d S4401C          pr                  ExtPgm('S4401C')
     d   DTASRC_                     61    const
     d
     d/copy sPrModule
     d/copy sPrXML

     C*
     C******************************************************************
     C*    LIKE DEFINES
     C******************************************************************
     C*
     C     *LIKE         DEFINE    EAPODT        XXDAT
     C     *LIKE         DEFINE    EAPODT        XXDAT1
     C     *LIKE         DEFINE    FNMAD1        XFMAD1
     C     *LIKE         DEFINE    FNMAD1        XFMAD2
     C     *LIKE         DEFINE    FNMAD1        XFMAD3
     C     *LIKE         DEFINE    EMMAD1        XXADR1
     C     *LIKE         DEFINE    EMMAD1        XXADR2
     C     *LIKE         DEFINE    EMMAD1        XXADR2SAV
     C     *LIKE         DEFINE    EMMAD1        XXADR3
     C     *LIKE         DEFINE    OFAAD1        XXAAD1
     C     *LIKE         DEFINE    OFAAD1        XXAAD2
     C     *LIKE         DEFINE    OFAAD1        XXAAD3
     C     *LIKE         DEFINE    EBCO#         XXCO#
     C     *LIKE         DEFINE    EBFND         XXFND
     C     *LIKE         DEFINE    EBFYR         XXFYR
     C     *LIKE         DEFINE    EBEMP#        XXEMP#
     C     *LIKE         DEFINE    EBDIV         XXDIV
     C     *LIKE         DEFINE    SRRPMY        XXCYM
     C     *LIKE         DEFINE    EMEMP#        RSFREN
     C     *LIKE         DEFINE    EMEMP#        RSTOEN
     C     *LIKE         DEFINE    EMEMP#        XXFREN
     C     *LIKE         DEFINE    EMEMP#        XXTOEN
     C     *LIKE         DEFINE    EMFDNM        XXFDNM
     C     *LIKE         DEFINE    EBAGT#        XXFRAN
     C     *LIKE         DEFINE    EBAGT#        XXTOAN
     C     *LIKE         DEFINE    EBAGT#        HLDAGT
     C     *LIKE         DEFINE    ELRTCD        KKRTCD
     C     *LIKE         DEFINE    CREFDT        KKEFDT
     C     *LIKE         DEFINE    CRST          KKST
     C     *LIKE         DEFINE    CEADR1        XCADR1
     C     *LIKE         DEFINE    CEADR1        XCADR2
     C     *LIKE         DEFINE    CEADR1        XCADR3
     C     *LIKE         DEFINE    CXCXDT        ZZSTDT
     C     *LIKE         DEFINE    CXCXDT        ZZENDT
     C     *LIKE         DEFINE    AAAGT#        XXAGT#
     C     *LIKE         DEFINE    APFEIN        XXFEIN
     C     *LIKE         DEFINE    APMOD         XXMOD
     C     *LIKE         DEFINE    APPROD        XXPROD
     C     *LIKE         DEFINE    AHEFDT        XXEFDT
     C******************************************************************
     C*    KEY LISTING
     C******************************************************************
      *
      *   Carrier/State reporting Master
      *
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C*
     C* KEY LIST FOR THE COMPANY MASTER FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* KEY LIST FOR THE OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  BILLING COMMENT FILE
     C*
     C     BCKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBCOM           10
     C*
     C* KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* KEY LIST FOR THE OFFICE MASTER FILE
     C*
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMOFF#
     C*
     C* KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EBFYR
     C*
     C     F1KEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EAFYR
     C*
     C* KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  REMARK TYPE TABLE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    QQDRMK
     C*
     C     CTKY1         KLIST
     C                   KFLD                    QQDTMK
     C*
     C* KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EBFYR
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C*
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C*
     C* KEY LIST FOR THE EMPLOYERS THIRD ADDRESS FILE
     C*
     C     A3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C* KEY LIST FOR THE SELF REPORTING PAYROLL TRANSACTION FILE
     C*
     C     SRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EBFYR
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C                   KFLD                    XXCYM
     C*
     C* KEY LIST FOR THE SELF REPORTING PAYROLL TRANSACTION FILE, VENDOR
     C* PROCESSING ENABLED
     C*
     C     SRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EBFYR
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C                   KFLD                    KKRPDT            7 0
     C*
     C* KEY LIST FOR THE COMMERCIAL REPORT WORKFILE
     C*
     C     EBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* KEY LIST FOR THE TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKRTCD
     C*
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKST
     C                   KFLD                    XXKEY
     C                   KFLD                    KKEFDT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKST
     C                   KFLD                    XXKEY
     C     CRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKST
     C                   KFLD                    ##KEY
     C                   KFLD                    KKEFDT
     C*
     C     CRKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKST
     C                   KFLD                    ##KEY            10
     C*
     C* KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C                   KFLD                    EBFYR
     **
     **  KEY LIST FOR CANX DETAIL FILE
     **
     C     CXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    CD(C)
     C*  Key list for certificate holders file
     C     CEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     CXKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C     APKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXPROD
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXAGT#
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C****************************************************************
     C* ENTRY PARAMETERS
     C****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQRPTF            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQSRT1            2
     C                   PARM                    QQSRT2            2
     C                   PARM                    QQFREA            2
     C                   PARM                    QQTOEA            2
     C                   PARM                    QQFREN            9
     C                   PARM                    QQTOEN            9
     C                   PARM                    QQFRAA            2
     C                   PARM                    QQTOAA            2
     C                   PARM                    QQFRAN            5
     C                   PARM                    QQTOAN            5
     C                   PARM                    QQAMTD            9
     C                   PARM                    QQRMCY            6
     C                   PARM                    QQPDDT            8
     C                   PARM                    QQRBDT            8
     C                   PARM                    QQSTDT            8
     C                   PARM                    QQENDT            8
     C                   PARM                    QQCMT            10
     C                   PARM                    QQDRMK            5
     C                   PARM                    QQIEFL            1
     C                   PARM                    QQASTC           10
     C                   PARM                    QQRPDT            8
     C                   PARM                    QQFREQ            1
     C                   PARM                    QQPRBL            1
     C                   PARM                    QQJRST            2
     C                   PARM                    QQDFLG            1
     C                   PARM                    QQDTMK            5
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
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '132'
     C                   SETON                                        67
     C                   ELSE
     C                   SETOFF                                       67
     C                   ENDIF
     C     CLTID         IFEQ      '102'
     C                   SETON                                        71
     C                   ELSE
     C                   SETOFF                                       71
     C                   ENDIF
     C*
     C                   MOVE      QQPDDT        XXPDDT            8 0
     C                   MOVE      QQRBDT        XXRBDT            8 0
     C                   MOVE      QQRMCY        XXRMCY
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQDATE        XXRNDT            8 0
     C                   MOVE      QQAMTD        XQAMTD            9 2
     C                   MOVE      QQFREA        XXFREA            2
     C                   MOVE      QQTOEA        XXTOEA            2
     C                   MOVE      QQFREN        XXFREN
     C                   MOVE      QQTOEN        XXTOEN
     C                   MOVE      QQFRAA        XXFRAA            2
     C                   MOVE      QQTOAA        XXTOAA            2
     C                   MOVE      QQFRAN        XXFRAN
     C                   MOVE      QQTOAN        XXTOAN
     C                   MOVEA     QQASTC        AS
       // Set Mass flag
       isMassReq = *On;
       massFlag = 'Y';
       If QQRptF <> ' ';
          isMassReq = *Off;
          massFlag = ' ';
       EndIf;
     C****************************************************************
     C* Set Up and Initialize
     C****************************************************************
      /Free

       // Determine if Scriptura and/or Spool Output
       xxProcessType = processType;
       isUseSpoolOut = UseSpoolOut(xxProcessType);
       isUseXML = UseScriptura(xxProcessType)
        or Use3rdXml(xxProcessType);

       // Open Printer Files if Spool Files are generated
       If isUseSpoolOut;
          Open QSYSPRT;
          Open QSYSPRT1;
          Open QSYSPRT2;
       Endif;

      /end-free
     C     QQQUE         IFEQ      *BLANK
     C                   SETON                                        08
     C                   ELSE
     C                   SETON                                        09
     C                   ENDIF
     C*
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     *ZERO         XXCNT             5 0
     C                   MOVE      *ALL'_'       SEPA80           75
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT             7 0
      *
     C                   MOVE      QQENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT            7 0
     C*
     C                   MOVE      QQPDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7PDDT            7 0
      *
     ***************************************************************
     **  BUILD LINE 2 OF LATE NOTICE COMMENTS (BECAUSE OF MONTH VAR.)
     ***************************************************************
      *
      * QQFREQ WILL BE BLANK IF VENDOR PROCESSING ISN'T ENABLED (WCFL23 <> Y IN
      * SWCPF)
      *
     C     QQFREQ        IFEQ      *BLANK
     C                   SETOFF                                       41
     C                   MOVE      MNTH(XXMN)    XXMNTH            9
     C     ' '           CHECK     XXMNTH        ST                2 0
     C                   SUBST(P)  XXMNTH:ST     XXMNTH
     **
     C     XXLIT1        CAT(P)    XXMNTH        XXLIN2           56
     C                   CAT       ',':0         XXLIN2
     C                   MOVE      XXYR          XXYR1             4
     C                   CAT       XXYR1:1       XXLIN2
     **
     C                   CAT       XXLIT2:1      XXLIN2
     **
     **     CONVERT THE POSTING DATE TO A CHARACTER FIELD MM/DD/CCYY.
     **
     C                   MOVEL     XXPDDT        XXNUM
     C                   MOVEL     'DATE'        EDTMSK
     **
     C                   CALL      'WA003R'      CHGPL
     **
     C                   MOVEL     XXNUM         XXPD1            10
     **
     C                   CAT       XXPD1:1       XXLIN2
     C                   ELSE
     C                   SETON                                        41
     C                   MOVE      QQRPDT        XXRPDT            8 0
     C                   Z-ADD     XXRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCYM
     C                   ENDIF
     ***************************************************************
     **     MOVE REPORTING DATE INTO MONTH AND YEAR LITERAL
     ***************************************************************
     C     QQFREQ        IFEQ      *BLANK
     C                   MOVEL(P)  MNTH(XXMN)    XXRPMY           15
     C                   MOVE      ',     '      XXRPMY
     C                   MOVE      XXYR1         XXRPMY
     C                   ENDIF
     C     CLTID         IFEQ      '109'
     C                   Z-ADD     *ZERO         XXCYM
     C                   ENDIF
     C                   Z-ADD     XXRMCY        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C     SRERR         IFEQ      *ZERO
     C                   MOVEL     SRDTO         XXCYM
     C                   MOVEL     SRDTO         ##YYMM            5
     C                   ENDIF
     C****************************************************************
     C* MOVE ENTRY PARMS TO ROUTING PAGE
     C****************************************************************
     C                   MOVE      QQCO#         RSCO#             3 0
     C                   MOVE      QQFND         RSFND             3 0
     C                   SELECT
     C     QQJRST        WHENEQ    *BLANK
     C                   MOVEL(P)  '*ALL'        RXJRST           16
     C     QQJRST        WHENNE    *BLANK
     C                   MOVEL(P)  QQJRST        RXJRST
     C                   ENDSL
     C                   MOVE      QQDATE        RSDATE            8 0
     C                   MOVE      QQRPTF        RSRPTF            1
     C                   MOVE      *BLANK        RSFREA            2
     C                   MOVE      *BLANK        RSTOEA            2
     C                   MOVE      *ZEROS        RSFREN
     C                   MOVE      *ZEROS        RSTOEN
     C                   MOVE      *BLANK        RSFRAA            2
     C                   MOVE      *BLANK        RSTOAA            2
     C                   MOVE      *ZEROS        RSFRAN            5 0
     C                   MOVE      *ZEROS        RSTOAN            5 0
      *
      *
     ** DISPLAY CORRECT SORT CODES
     **
      * * * * * * *
      *   SORT 1
      * * * * * * *
     C                   SELECT
      *
     C     QQSRT1        WHENEQ    'AN'
     C     EXAHDR        IFNE      *BLANKS
     C                   MOVEL(P)  EXAHDR        RSSRT1           13
     C                   ELSE
     C                   MOVEL(P)  'AGENT #'     RSSRT1
     C                   ENDIF
     C                   MOVE      *ON           AGTSRT            1
      *
     C     QQSRT1        WHENEQ    'AA'
     C     EXAHDR        IFNE      *BLANKS
     C     EXAHDR        CAT(P)    'NAME':1      RSSRT1
     C                   ELSE
     C     'AGENT'       CAT       'NAME':1      RSSRT1
     C                   ENDIF
      *
     C     QQSRT1        WHENEQ    'EN'
     C     'EMPLOYER'    CAT       '/DIV':0      RSSRT1
      *
     C     QQSRT1        WHENEQ    'EA'
     C     'EMPLOYER'    CAT       'NAME':1      RSSRT1
      *
     C                   ENDSL
      * * * * * * *
      *   SORT 2
      * * * * * * *
     C                   SELECT
      *
     C     QQSRT2        WHENEQ    'EN'
     C     'EMPLOYER'    CAT       '/DIV':0      RSSRT2           13
      *
     C     QQSRT2        WHENEQ    'EA'
     C     'EMPLOYER'    CAT       'NAME':1      RSSRT2
      *
     C     QQSRT2        WHENEQ    *BLANK
     C                   MOVEL     'NONE'        RSSRT2
      *
     C                   ENDSL
     **
     C*
     C     XXTOAN        IFNE      *ZERO
     C                   MOVE      QQFRAN        RSFRAN
     C                   MOVE      QQTOAN        RSTOAN
     C                   ENDIF
     **
     C     XXTOAA        IFNE      '  '
     C                   MOVE      QQFRAA        RSFRAA
     C                   MOVE      QQTOAA        RSTOAA
     C                   ENDIF
     **
     C     XXTOEN        IFNE      *ZERO
     C                   MOVE      QQFREN        RSFREN
     C                   MOVE      QQTOEN        RSTOEN
     C                   ENDIF
     **
     C     XXTOEA        IFNE      '  '
     C                   MOVE      QQFREA        RSFREA
     C                   MOVE      QQTOEA        RSTOEA
     C                   ENDIF
     C*
     C                   MOVE      QQAMTD        RSAMTD            9 2
     C                   MOVE      QQRMCY        RSRMCY            6 0
     C                   MOVE      QQPDDT        RSPDDT            8 0
     C                   MOVE      QQRBDT        RSRBDT            8 0
      *
      * SORT THE ACTIVITY STATUS ARRAY
      *
     C                   SORTA     AS
     C                   MOVE      *BLANK        ASDS
      *
     C     QQIEFL        IFNE      *BLANK
      *
      * Fill the Code Decription Array which Corresponds to the Code
      *
     C     1             DO        5             Y
      *
      * Indicators 15 to 19 are used for Output Purposes
      *
     C     AS(Y)         IFNE      *BLANK
      *
     C     Y             ADD       14            Z1                2 0
     C                   MOVE      *ON           *IN(Z1)
     C                   MOVEL(P)  AS(Y)         XXKEY
      *
     C     TBKY          CHAIN     PAV01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AVDESC        ASDS(Y)
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDDO
      *
      *   SET INDICATOR FOR EXCLUDE VS INCLUDE VERBIAGE
      *
     C     QQIEFL        IFEQ      'E'
     C                   MOVE      *OFF          *IN14
     C                   ELSE
     C                   MOVE      *ON           *IN14
     C                   ENDIF
      *
     C                   ELSE
      *
      *   PRINT ALL CODES ON ROUTING PAGE
      *
     C                   MOVE      *ON           *IN14
     C                   MOVE      '*ALL'        RSASTC            4
      *
     C                   ENDIF
     C*
     C* GET THE USER PARAMETER RECORD
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C* GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        CONAME
     C                   ENDIF
     C*
     C* GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     **
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        XFNAME           40
      *
     C     FNCGFL        COMP      'Y'                                    66
     **
     ** USE ALTERNATE FUND NAME IF FOUND IN FUND MASTER
     **
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF
     **
     C*
     C     FNKEY         CHAIN     PEX01                              61
     C*
     C     *IN61         IFEQ      *OFF
     C     EXAHDR        IFEQ      *BLANKS
     C                   MOVEL(P)  AGT           EXAHDR
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL(P)  AGT           EXAHDR
     C                   ENDIF
     C*
     C     EXAHDR        IFEQ      AGT
     C     AGTL          CAT(P)    ':':0         XXAHDR           15
     C                   ELSE
     C     EXAHDR        CAT(P)    ':':0         XXAHDR
     C                   ENDIF
     C                   MOVEL(P)  EXAHDR        XPAHDR           10
     C                   ENDIF
     **
     C*
     C* REMOVE LEADING BLANKS & CENTER NAME
     C*
     C     ' '           CHECK     CONAME        Y                 2 0    60
     C     *IN60         IFEQ      *ON
     C                   SUBST(P)  CONAME:Y      XXCONM           40
     C     ' '           CHECKR    XXCONM        Y
     C     40            SUB       Y             Y
     C                   DIV       2             Y
     C     ' '           CAT       XXCONM:Y      XCCONM           40
     C                   ENDIF
     C*
     C* GET THE FUND AUXILIARY RECORD
     C*
     C     F2KEY         CHAIN     PF201                              60
     **
     C*
     C*   LOAD FUND ADDRESS, CITY, STATE, & ZIP
     C*
     C                   MOVEL(P)  FNMAD1        XFMAD1
     C                   MOVEL(P)  FNMAD2        XFMAD2
     C                   MOVEL(P)  FNMCTY        XXCITY           25
     C                   MOVEL     FNMST         XXSTAT            2
     C                   MOVEL     FNMZIP        XXZIP1            5
     C                   MOVE      FNMZIP        XXZIP2            4
     C*
     C                   EXSR      SRCSTZ
     C                   MOVEL     XXCSTZ        XFMAD3
     C*
     C     FNMAD2        IFEQ      *BLANK
     C                   MOVEL     XFMAD3        XFMAD2
     C                   MOVEL     *BLANK        XFMAD3
     C                   ENDIF
     C*
     C*   GET CORRECT FUND PHONE NUMBER TO WORK FIELDS
     C*
     C*
     C*
     C*  GET COMMENT CODE DESCRIPTIONS (1-5)
     C*
     C     QQCMT1        IFNE      *BLANK
     C                   MOVEL     QQCMT1        XXBCOM
     C     BCKEY1        CHAIN     PBC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCM01           40
     C                   ENDIF
     C                   ENDIF
     C*
     C     QQCMT2        IFNE      *BLANK
     C                   MOVEL     QQCMT2        XXBCOM
     C     BCKEY1        CHAIN     PBC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCM02           40
     C                   ENDIF
     C                   ENDIF
     C*
     C     QQCMT3        IFNE      *BLANK
     C                   MOVEL     QQCMT3        XXBCOM
     C     BCKEY1        CHAIN     PBC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCM03           40
     C                   ENDIF
     C                   ENDIF
     C*
     C     QQCMT4        IFNE      *BLANK
     C                   MOVEL     QQCMT4        XXBCOM
     C     BCKEY1        CHAIN     PBC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCM04           40
     C                   ENDIF
     C                   ENDIF
     C*
     C     QQCMT5        IFNE      *BLANK
     C                   MOVEL     QQCMT5        XXBCOM
     C     BCKEY1        CHAIN     PBC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCM05           40
     C                   ENDIF
     C                   ENDIF
     C*
     C* GET THE DISTRIBUTION INFO & PRINT IF FOUND
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      ORTITL
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C*  Center title for output
     C                   MOVEL(P)  ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        ORTITL
     C                   ENDIF
     C*
     C*
     C*  INITIALIZE AGENT COUNT
     C                   Z-ADD     0             AGTCNT            5 0
     C                   MOVE      *ON           *INOA
     C*
     C* MAIN LOOP STARTS HERE
     C*
     C     EBKY          SETLL     PEB01
     C     *IN50         DOWEQ     *OFF
     C     EBKY          READE     PEB01                                  50
     C*
     C     QQJRST        IFNE      *BLANK
     C     EBJRST        ANDNE     QQJRST
     C                   GOTO      NEXTEB
     C                   ENDIF
     C*   IF SORT IS BY AGENT# AND CURRENT AGENT IS DIFFERENT FROM LAST
     C*   AGENT PROCESSED: PRINT AGENT TOTALS FOR PREVIOUS AGENT AND
     C*   RESET AGENT TOTAL BUCKETS
     C     AGTSRT        IFEQ      *ON
     C     EBAGT#        IFNE      HLDAGT
     C     *IN50         OREQ      *ON
     C*
     C*     IF AGNTCNT = 0, DO NOT PRINT TOTALS
     C*
     C     AGTCNT        IFNE      0
     C*
     C                   If        isUseSpoolOut
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOA
     C                   EXCEPT    AGTOTL
     C                   ELSE
     C                   EXCEPT    AGTOTL
     C                   ENDIF
     C                   Endif
     C*
     C                   Z-ADD     *ZERO         AGTCNT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     EBAGT#        HLDAGT
     C                   MOVE      *ON           *INOA
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C*  EXIT AT EOF
     C*
     C     *IN50         IFEQ      *ON
     C                   ITER
     C                   ENDIF
     C*
     C*  GET NEXT, IF NOT A SELF-REPORTING EMPLOYER
     C*
     C     EBSEFL        IFNE      'S'
     C                   ITER
     C                   ENDIF
     **
     **  LOAD KEY FIELDS
     **
     C                   MOVE      EBFND         XXFND
     C                   MOVE      EBFYR         XXFYR
     C                   MOVE      EBEMP#        XXEMP#
     C                   MOVE      EBDIV         XXDIV
     **
     **   LOAD POLICY NUMBER
     **
     C                   CALL      'WA022R'
     C                   PARM      *BLANKS       XXPLCY           15
     C                   PARM                    XXEMP#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
      *
     C                   MOVEL(P)  *BLANKS       XXFIL#           15
     C                   MOVEL(P)  *BLANKS       X1PLCY           18
     C     RPKY          CHAIN     PRP01                              52
      *
     C     *IN52         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANKS
     C                   MOVE      XXPLCY        XXFIL#
     C                   MOVEL(P)  RPCAP#        X1PLCY
      *
     C                   ELSE
      *
     C                   MOVE(P)   XXPLCY        X1PLCY
     C                   SETON                                        52
     C                   ENDIF
      *
     **
     C                   MOVEL     EBENAM        XXNM2             2
     C                   MOVEL     EBANAM        XXAA2             2
     C*
     C*  GET NEXT, IF POLICY NOT YET ACTIVE, OR NOT ACTIVE ON RUN DT
     C*
     C     EBPFDT        IFEQ      *ZERO
     C     EBPFDT        ORGT      XXDATE
     C                   ITER
     C                   ENDIF
     C*
     C*  GET NEXT, IF POLICY CANCELLED PRIOR TO RUN DATE
     C*
     C     EBCNDT        IFNE      *ZERO
     C     EBCNDT        ANDLT     XXDATE
     C                   ITER
     C                   ENDIF
     C*
     C     CLTID         IFEQ      '102'
     C                   SETOFF                                       65
     C                   MOVE      QQSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZSTDT
     C*
     C                   MOVE      QQENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZENDT
     C*
     C*  Set on *IN65 if policy has a lapse in coverage for reporting month
     C     CXKY2         CHAIN     PCX02                              60
     C     *IN60         DOWEQ     *OFF
     C     *IN65         ANDEQ     *OFF
     C*
     C     CXCXDT        IFLT      ZZSTDT
     C     CXREDT        ANDGT     ZZENDT
     C                   SETON                                        65
     C                   ENDIF
     C*
     C     CXKY2         READE     PCX02                                  60
     C                   ENDDO
     C*
     C*  Get next if *IN65 is on (lapse in coverage for month)
     C     *IN65         IFEQ      *ON
     C                   ITER
     C                   ENDIF
     C                   ENDIF
     C* GET PRODUCER NAME
     C   71              EXSR      PRODNM
     C*
     C* QQFREQ WILL BE BLANK IF VENDOR PROCESSING IS NOT ENABLED
     C*
     C     QQFREQ        IFEQ      *BLANK
     C*
     C*  GET PAYROLL TRANS RECORD, IF RECORD FOUND, SKIP
     C*
     C     SRKEY         CHAIN     PSR01                              60
     C*  ONLY SKIP IF TOTAL PAYROLL > 0 OR ALL PAYROLL RECORDS ARE 0.
     C*
     C  N60              MOVE      'Y'           XXPYRL            1
     C   60              MOVE      'N'           XXPYRL
     C                   Z-ADD     0             XXAMTP            9 2
     C                   MOVE      'N'           XXGT0             1
     C*
     C     *IN60         DOWEQ     *OFF
     C                   ADD       SRAMTP        XXAMTP
     C     XXAMTP        IFGT      0
     C                   MOVE      'Y'           XXGT0
     C                   ENDIF
     C     SRKEY         READE     PSR01                                  60
     C                   ENDDO
     C*
     C     XXAMTP        IFGT      0
     C     XXGT0         OREQ      'N'
     C     XXPYRL        ANDEQ     'Y'
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C                   ITER
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*  VENDOR PROCESSING ENABLED REQUIRES THESE CHECKS
     C*        MAKE SURE BSCD FREQ AND PARM FREQ MATCH
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *ON
     C                   ITER
     C                   Else
     C                   eval      x#RRN = dbRRN
     C                   ENDIF
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *ON
     C                   ITER
     C                   ELSE
     C*
     C     QQFREQ        IFEQ      'M'
     C     BSFREQ        ANDNE     'M'
     C     QQFREQ        OREQ      'W'
     C     BSFREQ        ANDNE     'W'
     C                   ITER
     C                   ENDIF
     C*
     C*        MAKE SURE THAT THE REPORT'S DAYS OF WEEK MATCHES THE DAY
     C*        OF WEEK ATTACHED TO THE BILLING SCHEDULE CODE
     C*
     C     BSFREQ        IFEQ      'W'
     C                   MOVE      QQRPDT        ##RPDT            8 0
     C                   CALL      'SRDAY8'
     C                   PARM                    ##RPDT
     C                   PARM      *BLANK        ##SDAY            4
     C                   PARM      *BLANK        ##LDAY            9
     C                   PARM      *BLANK        ##ERR             1
     C*
     C                   MOVEL     ##SDAY        XXDOWK            2
     C     XXDOWK        IFNE      BSDOWK
     C                   ITER
     C                   ENDIF
     C*
     C*
     C*
     C*  ONLY SKIP IF TOTAL PAYROLL > 0 OR ALL PAYROLL RECORDS ARE 0
     C*
     C                   MOVE      QQRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKRPDT
      *
     C     SRKY2         CHAIN     PSR02                              60
     C  N60              MOVE      'Y'           XXPYRL
     C   60              MOVE      'N'           XXPYRL
     C                   Z-ADD     0             XXAMTP
     C                   MOVE      'N'           XXGT0
     C*
     C     *IN60         DOWEQ     *OFF
     C                   ADD       SRAMTP        XXAMTP
     C     XXAMTP        IFGT      0
     C                   MOVE      'Y'           XXGT0
     C                   ENDIF
     C     SRKY2         READE     PSR02                                  60
     C                   ENDDO
     C*
     C     XXAMTP        IFGT      0
     C     XXGT0         OREQ      'N'
     C     XXPYRL        ANDEQ     'Y'
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C                   ITER
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*  GET PAYROLL TRANS RECORD, IF RECORD FOUND, SKIP
     C*  ONLY SKIP IF TOTAL PAYROLL > 0 OR ALL PAYROLL RECORDS ARE 0.
     C*
     C     SRKEY         CHAIN     PSR01                              60
     C  N60              MOVE      'Y'           XXPYRL            1
     C   60              MOVE      'N'           XXPYRL
     C                   Z-ADD     0             XXAMTP            9 2
     C                   MOVE      'N'           XXGT0             1
     C*
     C     *IN60         DOWEQ     *OFF
     C                   ADD       SRAMTP        XXAMTP
     C     XXAMTP        IFGT      0
     C                   MOVE      'Y'           XXGT0
     C                   ENDIF
     C     SRKEY         READE     PSR01                                  60
     C                   ENDDO
     C*
     C     XXAMTP        IFGT      0
     C     XXGT0         OREQ      'N'
     C     XXPYRL        ANDEQ     'Y'
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C                   ITER
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  GET THE FUND DETAIL RECORD
     C*
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      *ON
     C                   ITER
     C                   ENDIF
     C*
     C*  GET THE EMPLOYER MASTER RECORD
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  ELASCD        EMASTS
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C     QQIEFL        ANDNE     *BLANK
      *
     C                   Z-ADD     1             Y
     C     EMASTS        LOOKUP    AS(Y)                                  60
      *
      *   CHECK IF CODES ARE 'EXCLUDE'
      *
     C     QQIEFL        IFEQ      'E'
      *
     C     EMASTS        IFEQ      *BLANK
     C                   MOVE      *ON           *IN60
     C                   ELSE
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ELSE
      *
     C     EMASTS        IFEQ      *BLANK
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
      *
     C                   ENDIF
     C     *IN60         IFEQ      *ON
     C                   ITER
     C                   ENDIF
     C                   MOVE      *BLANKS       OFCKNM
     C                   MOVE      *BLANKS       XXNAME
     C*
     C     OFKY          CHAIN     POF01                              60
     **
     C     *IN60         IFEQ      *OFF
     **
     C   66' '           CHECK     OFACNM        Y                 2 0    60
     C  N66' '           CHECK     XFNAME        Y                 2 0    60
     C     *IN60         IFEQ      *ON
     C   66              SUBST(P)  OFACNM:Y      XXCONM           40
     C  N66              SUBST(P)  XFNAME:Y      XXCONM           40
     C     ' '           CHECKR    XXCONM        Y
     C     40            SUB       Y             Y
     C                   DIV       2             Y
     C     ' '           CAT       XXCONM:Y      XCCONM           40
     C                   ENDIF
     C*
     C   66              MOVEL(P)  OFACNM        XXACNM           40
     C  N66              MOVEL(P)  XFNAME        XXACNM           40
     C*
     C                   MOVEL(P)  OFAAD1        XXAAD1
     C                   MOVEL(P)  OFAAD2        XXAAD2
     C                   MOVEL(P)  OFACTY        XXCITY           25
     C                   MOVEL     OFAST         XXSTAT            2
     C                   MOVEL     OFAZIP        XXZIP1            5
     C                   MOVE      OFAZIP        XXZIP2            4
     C*
     C                   EXSR      SRCSTZ
     C                   MOVEL     XXCSTZ        XXAAD3
     C*
     C     OFAAD2        IFEQ      *BLANK
     C                   MOVEL     XXAAD3        XXAAD2
     C                   MOVEL     *BLANK        XXAAD3
     C                   ENDIF
     **
     C*
     C*
     C*   MOVE CONTACT USER NAME AND PHONE TO OUTPUT FIELD
     C*
     C                   MOVEL(P)  OFLNNM        XXNAME           49
     C     OFAPHN        IFNE      *ZERO
     C                   MOVE      OFAARA        XXAREA            3
     C                   MOVEL     OFAPHN        XXPHN1            3
     C                   MOVE      OFAPHN        XXPHN2            4
     C                   CAT       ',':0         XXNAME
     C                   CAT       '(':1         XXNAME
     C                   CAT       XXAREA:0      XXNAME
     C                   CAT       ')':0         XXNAME
     C                   CAT       XXPHN1:1      XXNAME
     C                   CAT       '-':1         XXNAME
     C                   CAT       XXPHN2:1      XXNAME
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* Get the employer detail record
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *ON
     C                   ITER
     C                   Else
     C                   eval      x#RRN = dbRRN
     C                   ENDIF
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '109'
      *
      *  If S/R employer, check if reporting month
      *
     C     EBSEFL        IFEQ      'S'
     C     XXCYM         ANDNE     *ZERO
      *
     C                   MOVE      EBCO#         ##CO#
     C                   MOVE      ELPFDT        ##PFDT
     C                   MOVE      ELBSCD        ##BSCD
      *
     C                   CALL      'WA114R'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##PFDT            7
     C                   PARM                    ##BSCD            3
     C                   PARM                    ##YYMM            5
     C                   PARM      *BLANK        ##EXFL            1
     C                   PARM      *BLANK        ##TDFL            1
      *
     C     ##EXFL        IFEQ      'Y'
     C                   ITER
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     **
     **  GET CURRENT CANCELLATION DATE BUCKET
     **
     C                   Z-ADD     1             C                 1 0
     C     *ZERO         LOOKUP    ED(C)                                  60
     **
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             C
     C                   ELSE
     C                   SUB       1             C
     C                   ENDIF
     **
     C     C             DOWNE     *ZERO
     C     XXENDT        IFGE      ED(C)
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             C
     C                   ENDDO
     **
     **  SETUP THE ACCOUNT STATUS INDICATORS: 72 = ACTIVE
     **                                       73 = CANCEL PEND
     C                   MOVE      *OFF          *IN72
     C                   MOVE      *OFF          *IN73
      * Get Days until cancel effective
     C     QQRBDT        IFLE      *ZEROS
     C                   MOVE      ELRTCD        KKRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     *HIVAL        KKEFDT
     C                   MOVE      RSJRST        KKST
     C                   MOVEL     'NP'          ##KEY
     C     CRKY2         SETGT     PCR01
     C     CRKY3         READPE    PCR01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY3         READPE    PCR01                                  60
     C                   ENDDO
      *
     C                   ENDIF
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     CRNOTC        ##DAYS            3 0
      *
      *  NOW BUILD THE CANCEL DATE FOR PRINTING
     C                   SELECT
     C     SYSDT         WHENGT    X7PDDT
     C                   Z-ADD     DSDATE        SRDATE            8 0
     C                   OTHER
     C                   Z-ADD     XXPDDT        SRDATE            8 0
     C                   ENDSL
     C     CRNDNW        IFGT      0
     c* Number of days to consider policy new
     C                   Z-ADD     CRNDNW        QQDAYS
     C*
     c** Calculate new work date
     C                   CALL      'SRPAST8'
     C                   PARM      SRDATE        QQDT              8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1
      *
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X@WRDT            7 0
     C                   ENDIF
      *
     C                   Z-ADD     XXCO#         #@CO#
     C                   Z-ADD     XXFND         #@FND
     C                   Z-ADD     EBEMP#        #@EMP#
     C                   Z-ADD     EBDIV         #@DIV
     C                   Z-ADD     *ZEROS        #@DATE
     C                   CALL      'WA084R'
     C                   PARM                    #@CO#             3 0
     C                   PARM                    #@FND             3 0
     C                   PARM                    #@EMP#            9 0
     C                   PARM                    #@DIV             5 0
     C                   PARM                    #@DATE            7 0
     C*
     C     #@DATE        IFGE      X@WRDT
     C                   Z-ADD     CRNTNP        ##DAYS
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     ##DAYS        SRDAYS            5 0
     C                   MOVE      *BLANKS       SRERR             1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   MOVE      SRDATE        XXRBDT
     C                   MOVE      SRDATE        RSRBDT
      *
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXRBDT
     C                   Z-ADD     *ZEROS        RSRBDT
     C                   ENDIF
     C                   ENDIF
     **
     C     CD(C)         IFEQ      *ZERO
     C                   MOVE      *ON           *IN72
     C     XXLIN2        CAT(P)    XXLIT3:0      XXLN2A           74
     C                   ENDIF
     **
     C     CD(C)         IFGT      XXENDT
     **
     C                   MOVE      *ON           *IN73
     C     *IN67         IFEQ      *OFF
     C     XXLIN2        CAT(P)    XXLIT4:0      XXLN2A
     C                   ELSE
     C     XXLIN2        CAT(P)    XXLIT5:0      XXLN2A
     C                   ENDIF
     **
     C                   Z-ADD     CD(C)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCXDT            8 0
     **
      *   IF CANX PENDING (IN73 = *ON) THEN GET CANX REASON FROM FILE
      *
     C     CXKEY         CHAIN     PCX01                              60
      *
     C                   MOVEL(P)  CXRSN         XXKEY            10
     C     CLTID         IFEQ      '102'
     C                   Z-ADD     CD(C)         KKEFDT
     C                   ELSE
     C                   Z-ADD     SYSDT         KKEFDT
     C                   ENDIF
      *
     C*
     C                   MOVE      ELRTCD        KKRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        KKST
     C                   MOVE      *BLANKS       XXRESN
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
      *
     C                   ENDIF
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CRDESC        XXRESN           60
     C                   ENDIF
     **
     C                   ENDIF
      *
     **
     **  IF FEDERAL NAME IS NOT BLANK THEN PRINT IT
     **
     C                   MOVEL(P)  EMFDNM        XXFDNM
     C*
     C                   MOVE      *OFF          *IN77
     **
     C     XXFDNM        IFNE      *BLANKS
     C                   MOVE      *ON           *IN77
     C                   ENDIF
     C*
     C* GET THE EMPLOYER DETAIL RECORD
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *ON
     C                   ITER
     C                   Else
     C                   eval      x#RRN = dbRRN
     C                   ENDIF
     C*
     C* CALCULATE AMOUNT DUE AND COMPARE TO MAXIMUM AMOUNT
     C*
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT
     C*
     C     EBCNDT        IFEQ      *ZERO
     C     EBCNDT        ORLT      EBPFDT
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C                   ELSE
     C                   Z-ADD     EBCNDT        XXDAT1
     C                   ENDIF
     C*
     C                   EXSR      SRAMTD
     C*
     C     XQAMTD        IFNE      *ZERO
     C     XXAMTD        ANDGT     XQAMTD
     C                   ITER
     C                   ENDIF
     C*
     C*  ADD 1 FOR AGENT COUNT
     C                   ADD       1             AGTCNT
     C*
     C*   IF SORT IS BY AGENT# AND CURRENT AGENT IS DIFFERENT FROM LAST
     C*   AGENT PROCESSED START A NEW PAGE
     C     AGTSRT        IFEQ      *ON
     C     EBAGT#        ANDNE     HLDAGT
     C                   MOVE      *ON           *INOA
     C                   ENDIF
     C*
     C* PRINT THE LATE NOTICE REPORT
     C*
     C     *INOA         IFEQ      *ON
     C                   If        isUseSpoolOut
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOA
     C                   EndIf
     C                   ENDIF
     C*
     C                   ADD       1             XXCNT
     C                   If        isUseSpoolOut
     C                   EXCEPT    DET
     C                   EndIf
     C*
     C* PRINT THE LATE NOTICE
     C*   IF THE 'GENERATE REPORT ONLY' FLAG IS BLANK
     C*
     C*
     C*
     C                   EXSR      SRLOAD
     C*
     C*  WRITE A DARTS RECORD
     C*
     C*
     C*  SET VALUE OF HOLD AGENT# TO CURRENT AGENT#
     C                   Z-ADD     EBAGT#        HLDAGT
     C*
     C     XXOPN3        IFNE      *ON
     C                   If        isUseSpoolOut
     C                             and QQRptF = ' '
     C                   OPEN      QSYSPRT3
     C                   MOVE      *ON           XXOPN3            1
     C                   ENDIF
     C                   EndIf
     C                   If        isUseSpoolOut
     C                             and QQRptF = ' '
     C                   EXCEPT    NOTICE
     C                   EndIf

       // Open Work files if Scriptura
       If isUseXML;
          // Open Work Files
          Open WXL8PSRC;
          Open WXL8PSR1;
          Open WXL8PSR2;
          Open WXL8PSR3;
          Open WXL8PSR4;

          // Setup Scriptura
          ExSr ScriptXML;

          // Write to Work files
          ExSr WriteWXL8PSRC;
          ExSr WriteWXL8PSR1;
          ExSr WriteWXL8PSR2;
          ExSr WriteWXL8PSR3;
          ExSr WriteWXL8PSR4;

          // Close Work Files
          Close WXL8PSRC;
          Close WXL8PSR1;
          Close WXL8PSR2;
          Close WXL8PSR3;
          Close WXL8PSR4;

       Endif;
     C                   If        QQRptF = ' '
     C     QQDFLG        IFEQ      'Y'
     C     QQDTMK        ORNE      ' '
     C                   EXSR      DARTS#
     C                   ENDIF
     C     CLTID         IFEQ      '102'
     C*  Print out a notice for each certificate holder
     C     CEKY          CHAIN     PCE01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  CEADR1        XCADR1
     C                   MOVEL(P)  CEADR2        XCADR2
     C                   MOVEL(P)  CECTY         XXCITY
     C                   MOVE      CEST          XXSTAT
     C                   MOVEL     CEZIP         XXZIP1
     C                   MOVE      CEZIP         XXZIP2
     C*
     C                   EXSR      SRCSTZ
     C     XCADR2        IFEQ      *BLANK
     C                   MOVEL(P)  XXCSTZ        XCADR2
     C                   MOVE      *BLANK        XCADR3
     C                   ELSE
     C                   MOVEL(P)  XXCSTZ        XCADR3
     C                   ENDIF
     C*
     C*
     C                   If        isUseSpoolOut
     C                   EXCEPT    NOTIC2
     C                   EndIf
     C*
     C     CEKY          READE     PCE01                                  60
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF
     C*
     C     NEXTEB        TAG
     C                   ENDDO
     C*
     C     XXCNT         IFGT      *ZERO
     C                   If        isUseSpoolOut
     C                   EXCEPT    TOT
     C                   EndIf
     C                   ENDIF
     C*
     C* If not resolving, delete spoolfile (QSYSPRT)
     C     QQDFLG        IFNE      'Y'
     C     isUseSpoolOut AndEq     *On
     C     XXOPN3        IFEQ      *ON
     C                   CLOSE     QSYSPRT
     C                   ENDIF
     C     DLTSPL        CAT(P)    PRT3:1        ##CMD
     C                   Z-ADD     25            ##LEN
     C                   CALL      'QCMDEXC'                            99
     C                   PARM                    ##CMD           255
     C                   PARM                    ##LEN            15 5
     C                   ENDIF
     C*
      /Free
       xxprt = '1';
       Chain orkey POR01;
       If %found;
          If isUseSpoolOut;
             Except Route;
          EndIf;
          If noLateNotices;
             Exsr ScriptRemove;
          EndIf;
       Endif;
       If xmlFileName <> *blank;
          QQXml = xmlFileName + '$';
          S4401C(QQxml);
       Endif;
      /End-free
     C                   MOVE      *ON           *INLR
       // *=========================================================================
       // *  ScriptXML - Reserve/Update XML record for Scriptura output.
       // *=========================================================================
       Begsr ScriptXML;

          xmlFile  = ' ';
          message  = %trim(processType);
          string   = ' ';

          If isMassReq;

             StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
              'SRLateNot':'000':'000':massFlag:xmlLink);

             isMassReq = *off;
             xmlFileName = xmlFile;
             xmlLink = xmlFile;
             Chain (xmlFile) xp101;
             XPCat = 'PS';
             Update xp101;

          //* Request record for Report.
             xmlFile = ' ';
             message = %trim(processType);

             StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
              'SRLateRpt ':'000':'000':massFlag:xmlLink);

             xmlRptFile = xmlFile;

             Chain (xmlFileBlank:xmlLink:xmlFile) xp101;
             XPType = %trim(processType) + ' Report';
             XPDcid = 'SR Late Notice Report.scd';

             //* Determine report template information.
             Setgt (XPType:sysDt) psp010;
             Readpe(n) (XPType) psp010;
             If not %eof;
                If S1SCltT <> ' ';
                   XPDcid = %trim(S1SCltT);
                   XPPath = %trim(S1PathC);
                Else;
                   If S1SDefT <> ' ';
                      XPDcid = %trim(S1SDefT);
                      XPPath = %trim(S1PathD);
                   Endif;
                Endif;
             Endif;

             XPDart = ' ';
             XPRmcd = ' ';
             XPData = ' ';

             Update xp101;

          Endif;

       If QQRptF = ' ';
          StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x#RRN:'X'):xxPre:
           string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:xmlLink);

          If xmlFileName = ' ';
             xmlFileName = xmlFile;
          Endif;

          If xmlLink = ' ';
             Chain (xmlFile) xp101;
             xmlFile = XPXmlT;
          Else;
             Chain (xmlFileBlank:xmlLink:xmlFile) xp101;
          Endif;

          XPData = 'Y';

          XPPgm  = Q1PGM;
          XPUser = Q1USER;
          XPTime = DsTime;
          XPDt   = sysDt;

          Update xp101;
       Else;
       //* Build Report request and workfile name if report only mode.
          If xmlFileName = *blanks;

             //* Request record for Report.
             xmlFile = ' ';
             message = %trim(processType);

             StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
              'SRLateRpt ':'000':'000':massFlag:xmlLink);

             xmlFileName = xmlFile;

             Chain (xmlFile) xp101;
             xmlRptFile = XPXmlT;
             XPType = %trim(processType) + ' Report';
             XPDcid = 'SR Late Notice Report.scd';

             //* Determine report template information.
             Setgt (XPType:sysDt) psp010;
             Readpe(n) (XPType) psp010;
             If not %eof;
                If S1SCltT <> ' ';
                   XPDcid = %trim(S1SCltT);
                   XPPath = %trim(S1PathC);
                Else;
                   If S1SDefT <> ' ';
                      XPDcid = %trim(S1SDefT);
                      XPPath = %trim(S1PathD);
                   Endif;
                Endif;
             Endif;

             XPDart = ' ';
             XPRmcd = ' ';
             XPData = ' ';
             Update xp101;

             xmlFile = ' ';

          Endif;

       Endif;

       Endsr;
       // *=========================================================================
       // *  ScriptRemove - Remove the record for this interim as it has no payroll.
       // *=========================================================================
       Begsr ScriptRemove;

          If xmlLink = ' ';
             Chain (xmlFileName) xp101;
             //* Cancel the Scriptura requeset since no form to generate.
             xmlFileName = ' ';
          Else;
             Chain (xmlFileBlank:xmlLink:xmlFile) xp101;
          Endif;

          If %found;
             Delete xp101;
          Endif;

       Endsr;
     C*****************************************************************
     C*   SRAMTD - SUBROUTINE TO CALCULATE THE AMOUNT DUE
     C*****************************************************************
     C     SRAMTD        BEGSR
     C*
     C                   Z-ADD     *ZERO         XXAMTD            9 2
     C                   Z-ADD     *ZERO         XXDEPD            9 2
     C                   Z-ADD     *ZERO         XXPRMD            9 2
     C                   Z-ADD     *ZERO         XXEXCD            9 2
     C                   Z-ADD     *ZERO         XXDEDD            9 2
     C                   Z-ADD     *ZERO         XXFEED            9 2
     C                   Z-ADD     *ZERO         XXTOTD            9 2
     C*
     C*   START READING THE EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKEY         SETLL     PEA01
     C     EAKEY         READE     PEA01                                  60
B1   C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*   ADD TO OR SUBTRACT FROM AMOUNT DUE IF POST DATE < OR = TODAY
     C*   CALCULATE AGING ON PAST DUE AMOUNTS ACCORDING TO THE ENTERED
     C*   RUN DATE.
     C*   IF THE EMPLOYER HAS BEEN CANCELLED; INCLUDE A & B TYPE
     C*   RECORDS UP TO CANCEL DATE AND INCLUDE ALL OTHER A/R TYPES
     C*   UP TO THE RUN DATE.
     C*
B2   C     XXDAT         IFEQ      XXDAT1
     C     XXDAT         OREQ      999999
     C*
B3   C     EAPODT        IFLE      XXDAT
     C* PREM & EXP CONST BILLED
     C                   EXSR      ARBKET
     C*
E3   C                   ENDIF
     C*
X2   C                   ELSE
     C*
B3   C     EAPODT        IFLE      XXDAT
     C*
B4   C     EAPODT        IFLE      XXDAT1
     C                   EXSR      ARBKET
E4   C                   ENDIF
E3   C                   ENDIF
E2   C                   ENDIF
E2   C                   ENDIF
     C*
     C     EAKEY         READE     PEA01                                  60
E1   C                   ENDDO
      *
      *  TURN ON THE DEPOSIT MODE BY YEAR FLAG (XXDPYM) IF THE POLICY YEAR
      *  IS DEFINED AS SUCH IN WDF1P
      *
     C     F1DPYM        IFEQ      'Y'
     C                   MOVE      'Y'           XXDPYM            1
     C                   ELSE
     C                   MOVE      'N'           XXDPYM            1
     C                   ENDIF
     C*
     C     EAKY1         SETLL     PEA02
     C     EAKY1         READE     PEA02                                  60
B1   C     *IN60         DOWEQ     *OFF
     C*
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'D'
     C*
     C     EAFYR         IFLT      EBFYR
     C     XXDPYM        ANDEQ     'N'
     C     EAFYR         OREQ      EBFYR
      *
     C                   MOVE      'N'           XXPYDM            1
     C     EAFYR         IFLT      EBFYR
     C     F1KEY2        SETLL     PF101
     C     F1KEY2        READE     PF101                                  60
     C     F1DPYM        IFEQ      'Y'
     C                   MOVE      'Y'           XXPYDM            1
     C                   ENDIF
     C                   ENDIF
      *
     C     XXPYDM        IFEQ      'N'
      *
B2   C     XXDAT         IFEQ      XXDAT1
     C     XXDAT         OREQ      999999
     C*
B3   C     EAPODT        IFLE      XXDAT
B4   C     EAARTY        IFEQ      'H'
B4   C     EAARTY        OREQ      'R'
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XXDEPD
X4   C                   ELSE
B5   C     EAARTY        IFEQ      'I'
     C     EAARTY        OREQ      'T'
     C                   SUB       EAAMT         XXAMTD
     C                   SUB       EAAMT         XXDEPD
E5   C                   ENDIF
E4   C                   ENDIF
E3   C                   ENDIF
     C*
X2   C                   ELSE
     C*
B3   C     EAPODT        IFLE      XXDAT
B4   C     EAPODT        IFLE      XXDAT1
     C     EAARTY        ANDEQ     'H'
     C     EAARTY        OREQ      'R'
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XXDEPD
E4   C                   ENDIF
B4   C     EAARTY        IFEQ      'I'
     C     EAARTY        OREQ      'T'
     C                   SUB       EAAMT         XXAMTD
     C                   SUB       EAAMT         XXDEPD
E4   C                   ENDIF
E3   C                   ENDIF
     C*
E2   C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     EAKY1         READE     PEA02                                  60
E1   C                   ENDDO
     C*
     C* RESET GROUP DETAIL
     C*
     C     F1KEY         SETLL     PF101
     C     F1KEY         READE     PF101                                  60
     C*
     C     XXDEPD        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN74
     C                   ELSE
     C                   MOVE      *ON           *IN74
     C                   ENDIF
     C*
     C     XXPRMD        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN75
     C                   ELSE
     C                   MOVE      *ON           *IN75
     C                   ENDIF
     C*
     C     XXEXCD        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN76
     C                   ELSE
     C                   MOVE      *ON           *IN76
     C                   ENDIF
     C*
     C     XXDEDD        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN78
     C                   ELSE
     C                   MOVE      *ON           *IN78
     C                   ENDIF
     C*
     C     XXFEED        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN79
     C                   ELSE
     C                   MOVE      *ON           *IN79
     C                   ENDIF
     C*
     C                   MOVEL     UNDLIN        XXUNDL           15
     C                   MOVEL     OTHBAL        XXOBAL           30
     C*
     C                   MOVE      *ON           *IN80
     C*
     C     QQPRBL        IFEQ      'N'
     C                   MOVE      *OFF          *IN74
     C                   MOVE      *OFF          *IN75
     C                   MOVE      *OFF          *IN76
     C                   MOVE      *OFF          *IN78
     C                   MOVE      *OFF          *IN79
     C                   MOVE      *OFF          *IN80
     C                   CLEAR                   XXUNDL
     C                   CLEAR                   XXOBAL
     C                   ENDIF
     C*
     C     XXDEPD        ADD       XXPRMD        XXTOTD
     C                   ADD       XXEXCD        XXTOTD
     C                   ADD       XXDEDD        XXTOTD
     C                   ADD       XXFEED        XXTOTD
     C*
     C                   ENDSR
     C******************************************************************
     C*   SRLOAD - SUBROUTINE TO LOAD WORK FIELDS WITH NAMES AND ADDRESS
     C******************************************************************
     C     SRLOAD        BEGSR
     C*
     C*   LOAD EMPLOYER ADDRESS, CITY, STATE, & ZIP
     C*
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL(P)  EMMAD1        XXADR1
     C                   MOVEL(P)  EMMAD2        XXADR2
     C                   MOVEL(P)  EMMCTY        XXCITY
     C                   MOVEL     EMMST         XXSTAT
     C                   MOVEL     EMMZIP        XXZIP1
     C                   MOVE      EMMZIP        XXZIP2
     C                   ELSE
     C                   MOVEL(P)  EMPAD1        XXADR1
     C                   MOVEL(P)  EMPAD2        XXADR2
     C                   MOVEL(P)  EMPCTY        XXCITY
     C                   MOVEL     EMPST         XXSTAT
     C                   MOVEL     EMPZIP        XXZIP1
     C                   MOVE      EMPZIP        XXZIP2
     C                   ENDIF
     **
     ** LOAD THE BILLING ADDRESS IF ITS NOT BLANK
     **
     C                   MOVEL     *BLANKS       A3OOC
     C     A3KY          CHAIN     PA301                              60
     **
     C     *IN60         IFEQ      *ZERO
     C     A3STAT        ANDEQ     'A'
     C     A3ADR1        IFNE      *BLANK
     **
     C                   MOVEL(P)  A3ADR1        XXADR1
     C                   MOVEL(P)  A3ADR2        XXADR2
     C                   MOVEL(P)  A3CTY         XXCITY
     C                   MOVEL     A3ST          XXSTAT
     C                   MOVEL     A3ZIP1        XXZIP1
     C                   MOVEL     A3ZIP2        XXZIP2
     **
     C                   ENDIF
     **
     C                   ENDIF
     C*
     C                   EXSR      SRCSTZ
     C                   MOVEL(P)  XXCSTZ        XXADR3
      *
      *
      *  Print 'OUT OF COUNTRY ADDRESS' if entered
      *
     C     A3OOC         IFNE      *BLANKS
     C                   MOVEL(P)  A3OOC         XXADR3
     C                   ENDIF
      *
     C*
     C*  REMOVE BLANK LINE FROM EMPLOYER ADDRESS IF NECESSARY
     C*
     c                   movel(p)  xxAdr2        xxAdr2Sav
     C     XXADR2        IFEQ      *BLANK
     C                   MOVEL(P)  XXADR3        XXADR2
     C                   MOVEL     *BLANK        XXADR3
     C                   ENDIF
     C                   ENDSR
      *
      *
      *
     C*****************************************************************
     C*  SRCSTZ - SUBROUTINE TO BUILD THE CITY, STATE & ZIP LINE        *
     C*****************************************************************
     C     SRCSTZ        BEGSR
     C*
     C     XXCITY        CAT(P)    ',':0         XXCSTZ           40
     C                   CAT       XXSTAT:1      XXCSTZ
     C                   CAT       XXZIP1:1      XXCSTZ
     C     XXZIP2        IFNE      *ZERO
     C     XXZIP2        ANDNE     *BLANK
     C                   CAT       '-':0         XXCSTZ
     C                   CAT       XXZIP2:0      XXCSTZ
     C                   ENDIF
     C*
     C                   ENDSR
     C********************************************************************
     C*  DARTS SUBROUTINE  -  CREATE DARTS RECORD
     C********************************************************************
     C     DARTS         BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANK        QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      EMEMP#        QCEMP#
     C                   MOVE      EMDIV         QCDIV
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVE      EBFYR         XAFYR             2
     C                   MOVE      EBAGT#        XAAGT#            5
     C                   MOVE      XXMN          XAMN              2
     C                   MOVE      XXYR          XAYR              4
     C                   MOVEL     QQDATE        XARNDM            2
     C                   MOVE      XXDATE        XARNDD            2
     C                   MOVE      QQDATE        XARNDY            4
     C     CTKY          CHAIN     PCT01                              60
     C                   MOVEL(P)  CTDESC        Q#COMM          256
     C                   CAT       'FUND':1      Q#COMM
     C                   CAT       'YEAR:':1     Q#COMM
     C                   CAT       XAFYR:1       Q#COMM
     C                   CAT       'PAST D':1    Q#COMM
     C                   CAT       'UE MON':0    Q#COMM
     C                   CAT       'TH:':0       Q#COMM
     C                   CAT       XAMN:1        Q#COMM
     C                   CAT       '/':0         Q#COMM
     C                   CAT       XAYR:0        Q#COMM
     C                   CAT       'RUN':1       Q#COMM
     C                   CAT       'DATE:':1     Q#COMM
     C                   CAT       XARNDM:1      Q#COMM
     C                   CAT       '/':0         Q#COMM
     C                   CAT       XARNDD:0      Q#COMM
     C                   CAT       '/':0         Q#COMM
     C                   CAT       XARNDY:0      Q#COMM
     C                   CAT       'AGENT#':1    Q#COMM
     C                   CAT       XAAGT#:0      Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  *BLANK        Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVEL(P)  QQDRMK        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
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
     C*
     C     EDARTS        ENDSR
     C*================================================================
     C* PRODNM - Get producer name
     C*================================================================
     C*
     C     PRODNM        BEGSR
     C*
     C                   MOVE      EBAGT#        XXAGT#
     C*
     C                   MOVEL     *BLANKS       XPNAME           40
     C     AAKY          CHAIN     PAA01                              60
     C*
     C                   MOVE      AAFEIN        XXFEIN
     C                   MOVE      AAMOD         XXMOD
     C*
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  60
     C*
     C                   MOVE      AHPROD        XXPROD
     C*
     C  N60APKY          CHAIN     PAP01                              60
     C  N60APFSN         IFEQ      *BLANKS
     C*
     C                   MOVEL     APLSN         XPNAME
     C*
     C                   ELSE
     C*
     C     APFSN         CAT(P)    APLSN:1       XPNAME
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* DARTS# - PRODUCE THE DART REMARK & RESOLVE TO DARTS
     C*================================================================
     C     DARTS#        BEGSR
     C*
     C                   MOVE      *BLANKS       ##FLR
     C                   MOVE      *BLANKS       ##DCID
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
     C*
     C     QQDFLG        IFEQ      'Y'
     C     XXOPN3        IFEQ      *ON
     C     isUseSpoolOut AndEq     *On
     C                   CLOSE     QSYSPRT3
     C                   MOVE      *OFF          XXOPN3
     C                   ENDIF
      *
      *  If office is installed, resolve output for DARTS
      *
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       ##FLR            80
     C                   PARM      *BLANKS       ##DCID           12
     C                   PARM      'LN'          ##PRE             2
     C                   PARM      QQCO#         ##CO#             3
     C                   PARM      QQFND         ##FND             3
     C                   PARM      'P'           ##RTCD            1
      *
     C                   CALL      'CRTRSLV'
     C                   PARM                    ##FLR            80
     C                   PARM                    ##DCID           12
     C                   PARM      'QSYSPRT3'    ##RPT            10
     C                   PARM                    ##RTCD            1
      *
     C     ##RTCD        IFNE      ' '
     C                   MOVE      *BLANKS       ##FLR
     C                   MOVE      *BLANKS       ##DCID
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL(P)  ##FLR         Q#FOLD           80
     C                   MOVEL(P)  ##DCID        Q#DCID           12
      *
      *   ESTABLISH KEY
      *
     C                   MOVE      *BLANK        QCKEY
     C                   MOVE      XXCO#         ZZCO#
     C                   MOVE      XXFND         ZZFND
     C                   MOVE      EMEMP#        ZZEMP#
     C                   MOVE      EMDIV         ZZDIV
     C*
     C     QQDTMK        IFNE      ' '
     C*
     C     CTKY1         CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CTDESC        Q#COMM          256
     C                   ELSE
     C                   MOVEL(P)  ##SR          Q#COMM          256
     C                   ENDIF
      *
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  ZZKEY         Q#KEY            40
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  'N'           Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVEL(P)  QQDTMK        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  'O'           Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   MOVEL(P)  CTDESC        Q#COMM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
      *
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
     C                   PARM                    QQDTMK
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
      *
     C* Make note in DARTS that document was not able to be resolved.
     C     QQDFLG        IFEQ      'Y'
     C     ##DCID        ANDEQ     *BLANKS
      *
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM      ##CM          Q#CM75           75
     C                   PARM                    Q#PNAM           10
     C                   PARM                    Q#SECL            3
     C                   PARM                    Q#USER           10
     C                   PARM                    Q#PGM            10
     C                   PARM                    Q#RMCD            5
     C                   PARM      *BLANKS       Q#ASQ#            5
      *
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
      *================================================================
      * ARBKET -  DROP A/R AMOUNT INTO APPROPRIATE BUCKET
      *================================================================
     C     ARBKET        BEGSR
     C                   SELECT
     C     TTCTYP        WHENEQ    'P'
     C                   SELECT
     C     EAARTY        WHENEQ    'P'
     C     EAARTY        OREQ      'T'
     C                   SUB       EAAMT         XXAMTD
     C                   SUB       EAAMT         XXPRMD
     C                   OTHER
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XXPRMD
     C                   ENDSL
     C     TTCTYP        WHENEQ    'E'
     C                   SELECT
     C     EAARTY        WHENEQ    'P'
     C                   SUB       EAAMT         XXAMTD
     C                   SUB       EAAMT         XXEXCD
     C                   OTHER
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XXEXCD
     C                   ENDSL
     C     EAARTY        WHENEQ    'F'
     C                   ADD       EAAMT         XXAMTD
     C                   SELECT
     C     TTCTYP        WHENEQ    'R'
     C                   ADD       EAAMT         XXDEDD
     C                   OTHER
     C                   ADD       EAAMT         XXFEED
     C                   ENDSL
     C     EAARTY        WHENEQ    'Q'
     C                   SUB       EAAMT         XXAMTD
     C                   SELECT
     C     TTCTYP        WHENEQ    'R'
     C                   SUB       EAAMT         XXDEDD
     C                   OTHER
     C                   SUB       EAAMT         XXFEED
     C                   ENDSL
     C                   ENDSL
     C                   ENDSR
       //=================================================================
       // Write to S/R Late Notice - Run Criteria                           quests
       //=================================================================
       Begsr WriteWXL8PSRC;

          Clear pl80psrc;

          L8xml      = xmlFile;
          L8co#      = RSco#;
          L8fnd      = RSfnd;
          L8rateSt   = RXjrSt;
          L8runDt    = RSdate;
          L8rptOnly  = RSrptF;
          L8sort1    = RSsrt1;
          L8sort2    = RSsrt2;
          L8agt#Frm  = RSfrAN;
          L8Agt#To   = RStoAN;
          L8agtNFrm  = RSfrAA;
          L8agtNTo   = RStoAA;
          L8emp#Frm  = RSfrEN;
          L8emp#To   = RStoEN;
          L8empNFrm  = RSfrEA;
          L8empNTo   = RStoEA;
          L8balDt    = xxRnDt;
          L8maxAmt   = RSamtD;
          L8rptDt    = xxRpDt;
          L8rptMth   = RSrmcy;
          //*** begin add ***  11/02/21  ***********************************
          If L8rptDt = 0 and L8rptMth <> 0;
             // Convert MMYYYY into MMDDYYYY
             L8rptDt = %dec(%subst(%editc(L8rptMth:'4'):1:2) + '01' +
                       %subst(%editc(L8rptMth:'4'):3:4):8:0);
             lastday = %date(L8rptDt:*USA);
             // Get last day of each month
             lastday = lastday + %months(1);
             lastday = lastday - %days(1);
             L8rptDt = %dec(%char(lastday:*USA0):8:0);
          Endif;
          //*** end   add ***  11/02/21  ***********************************
          L8pastDue  = RSpdDt;
          L8rptByDt  = RSrbDt;
          L8freq     = qqFreq;
          //*** Beg Chg *** 03/18/22 ******************************************
          //***L8prtBal   = ' ';
          If qqPrBl = 'Y';
             L8prtBal = 'Y';
          Else;
             L8PrtBal = ' ';
          Endif;
          //*** End Chg *** 03/18/22 ******************************************
          L8comCd1   = qqCmt1;
          L8comCdDs1 = xxCM01;
          L8comCd2   = qqCmt2;
          L8comCdDs2 = xxCM02;
          L8comCd3   = qqCmt3;
          L8comCdDs3 = xxCm03;
          L8comCd4   = qqCmt4;
          L8comCdDs4 = xxCm04;
          L8comCd5   = qqCmt5;
          L8comCdDs5 = xxCm05;
          L8actCdIE  = RSastc;
          L8actCd1   = as(1);
          L8actCdDs1 = asds(1);
          L8actCd2   = as(2);
          L8actCdDs2 = asds(2);
          L8actCd3   = as(3);
          L8actCdDs3 = asds(3);
          L8actCd4   = as(4);
          L8actCdDs4 = asds(4);
          L8actCd5   = as(5);
          L8actCdDs5 = asds(5);
          L8iDt      = sysDt;

          If xmlFile <> ' ';
             Write pl80psrc;
          Endif;
          If xmlRptFile <> ' ';
             L8xml = xmlRptFile;
             Write pl80psrc;
          Endif;
          noLateNotices = *Off;

       Endsr;
       //=================================================================
       // Write to S/R Late Notice - Group Information                      quests
       //=================================================================
       Begsr WriteWXL8PSR1;

          Clear pl80psr1;

          L8xml     = xmlFile;
          L8co#     = xxCo#;
          L8coName  = xcCoNm;
          L8coAdr1  = COadr1;
          L8coAdr2  = COadr2;
          L8coCity  = COcty;
          L8coSt    = COst;
          L8coZip   = COzip;
          L8fnd     = FNfnd;
          L8fndName = xfName;
          L8fndAdr1 = xfMAd1;
          L8fndAdr2 = xfMAd2;
          L8fndCity = FNmCty;
          L8fndSt   = FNmSt;
          L8fndZip  = FNmZip;
          L8iDt     = sysDt;

          If xmlFile <> ' ';
             Write pl80psr1;
          Endif;
          If xmlRptFile <> ' ';
             L8xml = xmlRptFile;
             Write pl80psr1;
          Endif;

       Endsr;
       //=================================================================
       // Write to S/R Late Notice - Policy Information                     quests
       //=================================================================
       Begsr WriteWXL8PSR2;

          // Access Files
          ExSr ProdNm;
          Chain (xxCo#:AAfein:AAmod) pag00;

          Clear pl80psr2;

          L8xml      = xmlfile;
          L8co#      = xxCo#;
          L8fnd      = xxFnd;
          L8emp#     = EBemp#;
          L8div      = EBdiv;
          L8year     = EBfyr;
          L8pol#     = x1Plcy;
          L8file#    = xxFil#;
          L8empName  = EBeNam;
          L8empFdNm  = xxFdNm;
          L8empAdr1  = xxAdr1;
          L8empAdr2  = xxAdr2Sav;
          L8empCity  = xxCity;
          L8empSt    = xxStat;
          L8empZip   = %Dec(xxZip1 + xxZip2:9:0);
          L8contact  = OFlnNm;
          L8cxDt     = xxCxDt;
          L8cxReason = CXrsn;
          L8cxDesc   = xxResn;
          L8audited  = ELfaf;
          L8agt#     = EBagt#;
          L8agtHdg   = xxAHdr;
          L8agtName  = EBaNam;

          If AGmad1 = ' ';
             L8agtAdr1  = AGpAd1;
             L8agtAdr2  = ' ';
             L8agtCity  = AGpCty;
             L8agtSt    = AGpSt;
             L8agtZip   = AGpZip;
          Else;
             L8agtAdr1  = AGmAd1;
             L8agtAdr2  = ' ';
             L8agtCity  = AGmCty;
             L8agtSt    = AGmSt;
             L8agtZip   = AGmZip;
          Endif;

          L8agtStat  = AGstat;
          L8agtFein  = AAfein;
          L8agtMod   = AAmod;
          L8prod     = APprod;
          L8prodName = xpName;

          L8prodAdr1 = APmAd1;
          L8prodAdr2 = ' ';
          L8prodCity = APmCty;
          L8prodSt   = APmSt;
          L8prodZip  = APmZip;

          L8office   = EMoff#;
          L8offName  = xxAcNm;
          L8offAdr1  = OFaAd1;
          L8offAdr2  = OFaAd2;
          L8offCity  = OFaCty;
          L8offSt    = OFaSt;
          L8offZip   = OFaZip;
          L8iDt      = sysDt;

          If xmlFile <> ' ';
             Write pl80psr2;
          Endif;
          If xmlRptFile <> ' ';
             L8xml = xmlRptFile;
             Write pl80psr2;
          Endif;

       Endsr;
       //=================================================================
       // Write to S/R Late Notice - Report Information                     quests
       //=================================================================
       Begsr WriteWXL8PSR3;

          Clear pl80psr3;

          L8xml     = xmlFile;
          L8co#     = xxCo#;
          L8fnd     = xxFnd;
          L8emp#    = EBemp#;
          L8div     = EBdiv;
          L8year    = EBfyr;
          L8dueDt   = xxRbDt;
          L8dueAmt  = xxTotD;
          L8payable = OFckNm;
          L8inqName = OFlnNm;
          L8inqArea = %EditC(OFaAra:'X');
          L8inqPhn# = %EditC(OFaPhn:'X');
          L8iDt     = sysDt;

          If xmlFile <> ' ';
             Write pl80psr3;
          Endif;
          If xmlRptFile <> ' ';
             L8xml = xmlRptFile;
             Write pl80psr3;
          Endif;

       Endsr;
       //=================================================================
       // Write to S/R Late Notice - Line Item Criteria                     quests
       //=================================================================
       Begsr WriteWXL8PSR4;

          Clear pl80psr4;

          For i=1 to 5;

             Select;
             When i=1;
                lineDesc = 'Deposit Due';
                lineAmt  = xxDepD;
             When i=2;
                lineDesc = 'Premium Due';
                lineAmt  = xxPrmD;
             When i=3;
                lineDesc = 'Expense Constant Due';
                lineAmt  = xxExcD;
             When i=4;
                lineDesc = 'Deductible Fees Due';
                lineAmt  = xxDedD;
             When i=5;
                lineDesc = 'Fees Due';
                lineAmt  = xxFeeD;
             EndSl;

             L8xml      = xmlfile;
             L8co#      = xxCo#;
             L8fnd      = xxFnd;
             L8emp#     = EBemp#;
             L8div      = EBdiv;
             L8year     = EBfyr;
             L8lineDesc = lineDesc;
             L8lineAmt  = lineAmt;
             L8iDt      = sysDt;

             If L8lineAmt <> 0;
                If xmlFile <> ' ';
                   Write pl80psr4;
                Endif;
             EndIf;
          EndFor;

       Endsr;
     O******************************************************************
     O* Late Notice REPORT
     O******************************************************************
     OQSYSPRT1  E            ROUTE          1 06
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL            +001
     O          E            ROUTE          1
     O                                           22 'COMPANY        :'
     O                       RSCO#               26
     O          E            ROUTE          1
     O                                           22 'GROUP          :'
     O                       RSFND               26
     O          E            ROUTE          1
     O                                           22 'RATING STATE   :'
     O                       RXJRST            +001
     O          E            ROUTE          1
     O                                           22 'RUN DATE       :'
     O                       RSDATE        Y     33
     O          E            ROUTE          1
     O                                           22 'REPORT ONLY    :'
     O                       RSRPTF              24
     O          E            ROUTE          1
     O                                           22 'PRIMARY SORT   :'
     O                       RSSRT1              36
     O          E            ROUTE          1
     O                                           22 'SECONDARY SORT :'
     O                       RSSRT2              36
     O          E            ROUTE          1
     O                                           22 'AGT # RANGE    :'
     O                       RSFRAN        Z     33
     O                                           35 '-'
     O                       RSTOAN        Z     46
     O          E            ROUTE          1
     O                                           22 'AGT NAME RANGE :'
     O                       RSFRAA              33
     O                                           35 '-'
     O                       RSTOAA              46
     O          E            ROUTE          1
     O                                           22 'EMP NAME RANGE :'
     O                       RSFREA              33
     O                                           35 '-'
     O                       RSTOEA              46
     O          E            ROUTE          1
     O                                           22 'EMP # RANGE    :'
     O                       RSFREN        Z     33
     O                                           35 '-'
     O                       RSTOEN        Z     46
     O          E            ROUTE          1
     O                                           22 'MAXIMUM AMT    :'
     O                       RSAMTD        J     36
     O          E   N41      ROUTE          1
     O                                           22 'MONTH OF       :'
     O                       RSRMCY              30 '  /    '
     O          E    41      ROUTE          1
     O                                           22 'REPORT DATE    :'
     O                       XXRPDT              33 '  /  /    '
     O          E            ROUTE          1
     O                                           22 'PAST DUE DATE  :'
     O                       RSPDDT        Y     33
     O          E            ROUTE          1
     O                                           22 'REPORT BY DATE :'
     O                       RSRBDT              33 '  /  /    '
     O          E            ROUTE       1  1
     O               14                          32 'ACTIVITY CODES INCLUDED'
     O               14                          47 'IN CALCULATION'
     O              N14                          33 'ACTIVITY CODES EXCLUDED'
     O              N14                          50 'FROM CALCULATION'
     O          E       14   ROUTE          1
     O                       RSASTC              26
     O          E       15   ROUTE          1
     O                  15                        6 '1.'
     O                       AS(1)                9
     O                       ASDS(1)             40
     O          E       16   ROUTE          1
     O                  16                        6 '2.'
     O                       AS(2)                9
     O                       ASDS(2)             40
     O          E       17   ROUTE          1
     O                  17                        6 '3.'
     O                       AS(3)                9
     O                       ASDS(3)             40
     O          E       18   ROUTE          1
     O                  18                        6 '4.'
     O                       AS(4)                9
     O                       ASDS(4)             40
     O          E       19   ROUTE          1
     O                  19                        6 '5.'
     O                       AS(5)                9
     O                       ASDS(5)             40
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O* Report Headings
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XCCONM              60
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              69
     O                                           73 'PAGE:'
     O                       PAGE          Z     80
     O          E            HEAD           2
     O                                           39 'RUN DATE:'
     O                       XXRNDT        Y     50
     O                                           63 'MONTH OF:'
     O                       XXRPMY              80
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       FNNAME              71
     O          E            HEAD           2
     O                                           12 'EMPLR'
     O                                           19 'DIV'
     O                                           23 'YR'
     O                                           32 'NAME'
     O                       XPAHDR              80
     O* Detail Line
     O          E            DET            1
     O                       EBEMP#        Z     12
     O                       EBDIV         Z     19
     O                       EBFYR         Z     23
     O                       EBENAM              68
     O                       EBAGT#        Z     80
     O* Total Line
     O          E            AGTOTL      1  1
     O                       XXAHDR              22
     O                       HLDAGT        Z     29
     O                                           39 'NUMBER OF'
     O                                           50 'EMPLOYERS:'
     O                       AGTCNT        Z     56
     O* Total Line
     O          E            TOT         2  2
     O                                           39 'TOTAL NUMBER OF'
     O                                           50 'EMPLOYERS:'
     O                       XXCNT         Z     56
     O******************************************************************
     O*   MONTHLY INTERIM LATE NOTICES
     O******************************************************************
     OQSYSPRT   E   N71      NOTICE         1 07
     O              N71                        +015 'M O N T H L Y    I N'
     O              N71                        +000 ' T E R I M    L A T '
     O              N71                        +000 'E    N O T I C E'
     O          E    71      NOTICE         1 07
     O                                         +015 'NOTICE OF INTENT TO '
     O                                         +000 'CANCEL WORKERS COMPE'
     O                                         +000 'NSATION COVERAGE'
     O          E    71      NOTICE         1
     O                                         +030 'FOR NON-PAYMENT OF '
     O                                         +000 'PREMIUM'
     O          E            NOTICE         1
     O                       SEPA80            +005
     O          E            NOTICE         1
     O                       XXACNM            +006
     O                                           60 'Run Date   :'
     O                       XXRNDT              79 '  /  /    '
     O          E            NOTICE         1
     O                       XXAAD1            +006
     O                                           60 'Policy #   :'
     O                       X1PLCY              79
     O          E            NOTICE         1
     O                       XXAAD2            +006
     O                                           60 'Division   :'
     O                       EBDIV               79
      *
     O          E            NOTICE         1
     O                       XXAAD3            +006
     O                                           60 'Policy Year:'
     O                       EBFYR         Z     79
      *
     O          E            NOTICE         2
     O              N52                          60 'File #     :'
     O              N52      XXFIL#              79
      *
     O          E            NOTICE         0
     O                       EBENAM            +010
     O          E    77      NOTICE      1
     O               77      XXFDNM            +010
     O          E            NOTICE         1
     O                                           60 'Report     :'
     O              N41      XXRPMY              76
     O               41      XXRPDT              76 '  /  /    '
     O          E            NOTICE         1
     O                       XXADR1            +010
     O                                           60 'Due Date   :'
     O                       XXPDDT              76 '  /  /    '
     O          E            NOTICE         1
     O                       XXADR2            +010
     O          E            NOTICE         2
     O                       XXADR3            +010
     O          E            NOTICE         3
     O                       SEPA80            +005
     O          E            NOTICE         1
     O                                         +006 'We have not received'
     O                                         +000 ' your premium or you'
     O              N41                          66 'r monthly payroll re'
     O              N41                          74 'port for'
     O               41                          66 'r payroll report for'
     O               41      XXRPDT              77 '  /  /    '
      *
      *   PRINT OUT VERBIAGE FOR ACTIVE ACCOUNTS
      *
     O          E    72      NOTICE         1
     O              N41      XXLN2A              80
     O               41                          23 'which was due by '
     O               41      XXPDDT              33 '  /  /    '
     O               41                          57 '. To avoid cancellation,'
     O               41N67                       73 'please send the'
     O               41 67                       76 'payment and report'
     O          E    41 72   NOTICE         1
     O              N67                          14 'payment '
     O              N67                          32 'and the report by '
     O              N67      XXRBDT              42 '  /  /    '
     O              N67                          66 '. If payment and report '
     O              N67                          78 'have already'
     O               67                          23 'must be received '
     O               67                          41 'in our lockbox by '
     O               67      XXRBDT              51 '  /  /    '
     O               67                          75 '. If payment and report '
     O          E    41 72   NOTICE         2
     O              N67                          26 'been mailed, please '
     O              N67                          48 'disregard this notice.'
     O               67                          30 'have already been mailed'
     O               67                          54 ', please disregard this '
     O               67                          61 'notice.'
     O          E    72N41   NOTICE         1
     O              N67                          20 'cancellation, '
     O              N67                          44 'please send the payment '
     O              N67                          62 'and the report by '
     O              N67      XXRBDT              72 '  /  /    '
     O              N67                          77 '.  If'
     O               67                          20 'cancellation, '
     O               67                          44 'payment and report must '
     O               67                          68 'be received in our lockb'
     O               67                          74 'ox by '
     O          E    72N41   NOTICE         1
     O              N67                          18 'payment and '
     O              N67                          42 'report have already been'
     O              N67                          66 ' mailed, please disregar'
     O              N67                          80 'd this notice.'
     O               67      XXRBDT              16 '  /  /    '
     O               67                          40 '. If payment and report '
     O               67                          64 'have already been mailed'
     O               67                          74 ',  please '
     O          E    72N41   NOTICE         1
     O               67                          28 'disregard this notice.'
      *
      *  PRINT OUT VERBIAGE FOR ACCOUNTS PENDING CANCELLATION
      *
     O          E    73N41   NOTICE         1
     O                       XXLN2A            +006
     O          E    73N41   NOTICE         1
     O              N67                          28 'payment and the report'
     O              N67                          31 ' by'
     O              N67      XXRBDT              41 '  /  /    '
     O              N67                          65 '.  If payment and report'
     O              N67                          78 ' have already'
     O               67                          29 'report must be received'
     O               67                          48 ' in our lockbox by '
     O               67      XXRBDT              58 '  /  /    '
     O               67                          76 '.  If payment and '
     O          E    73N41   NOTICE         2
     O              N67                          29 'been mailed, please dis'
     O              N67                          48 'regard this notice.'
     O               67                          29 'and report have already'
     O               67                          53 ' been mailed, please dis'
     O               67                          72 'regard this notice.'
     O          E    73 41   NOTICE         1
     O                                           23 'which was due by '
     O                       XXPDDT              33 '  /  /    '
     O              N67                          51 '. Please send the '
     O              N67                          73 'payment and the report'
     O               67                          55 '. Payment and report m'
     O               67                          77 'ust be received in our'
     O          E    73 41   NOTICE         1
     O              N67                          09 'by '
     O              N67      XXRBDT              19 '  /  /    '
     O              N67                          43 '. If payment and report '
     O              N67                          67 'have already been sent, '
     O              N67                          73 'please'
     O               67                          17 'lockbox by '
     O               67      XXRBDT              27 '  /  /    '
     O               67                          50 '. If payment and report'
     O               67                          74 ' have already been sent,'
     O          E    73 41   NOTICE         2
     O              N67                          28 'disregard this notice.'
     O               67                          22 'please disregard'
     O               67                          35 ' this notice.'
     O          E            NOTICE         2
     O                                         +006 'PLEASE NOTE:  If no '
     O                                         +000 'payment is due, a re'
     O                                         +000 'port must still be m'
     O                                         +000 'ailed.'
     O          E            NOTICE         2
     O              N71      XXOBAL            +006
     O          E    74N71   NOTICE      0
     O                                         +016 'Deposit Due:'
     O                       XXDEPD        J     65
     O          E    75N71   NOTICE      1
     O                                         +016 'Premium Due:'
     O                       XXPRMD        J     65
     O          E    76N71   NOTICE      1
     O                                         +016 'Expense Constant Due:'
     O                       XXEXCD        J     65
     O          E    78N71   NOTICE      1
     O                                         +016 'Deductible Fees Due:'
     O                       XXDEDD        J     65
     O          E    79N71   NOTICE      1
     O                                         +016 'Fees Due:'
     O                       XXFEED        J     65
     O          E   N71      NOTICE         0
     O                       XXUNDL              65
     O          E    80N71   NOTICE      1  2
     O                                         +016 'Total Amount Due:'
     O                       XXTOTD        J     65 '$'
     O          E            NOTICE         3
     O                       SEPA80            +005
     O          E            NOTICE         2
     O                                         +006 'Make Checks Payable to:'
     O                       OFCKNM            +002
     O          E            NOTICE         2
     O                                         +006 'Serviced By:           '
     O                       XXCONM            +002
     O          E            NOTICE         2
     O                                         +006 'Direct Inquiries to:   '
     O                       XXNAME            +002
     O          E            NOTICE         1
     O                       XXCM01            +006
     O          E            NOTICE         1
     O                       XXCM02            +006
     O          E            NOTICE         1
     O                       XXCM03            +006
     O          E            NOTICE         1
     O                       XXCM04            +006
     O          E            NOTICE         2
     O                       XXCM05            +006
     O          E            NOTICE         1
     O               73                        +008 'YOUR POLICY IS CURRE'
     O               73                        +000 'NTLY PENDING CANCELL'
     O               73                        +000 'ATION EFFECTIVE'
     O               73      XXCXDT            +001 '  /  /    '
     O               73                        +000 '.'
     O          E            NOTICE         2
     O               73                        +008 'REASON: '
     O               73      XXRESN            +001
     O          E   N71      NOTICE         2
     O                       XXAHDR            +006
     O                       EBAGT#        Z   +001
     O                       EBANAM            +002
     O          E    71      NOTICE      1  1
     O                                         +006 'cc:  CERTIFICATE HOLDERS'
     O          E    71      NOTICE         1
     O                                         +011 'INSURED'
     O          E    71      NOTICE         1
     O                                         +011 'AGENT:'
     O                       EBAGT#        Z   +001
     O                       EBANAM            +002
     O          E    71      NOTICE         1
     O                                         +011 'PRODUCER:'
     O                       APPROD        Z   +001
     O                       XPNAME            +002
     O          E    71      NOTICE         1
     O                                         +011 'FILE'
     O******************************************************************
     O*   CERTIFICATE HOLDERS NOTICE
     O******************************************************************
     OQSYSPRT2  E            NOTIC2         1 07
     O                                         +015 'NOTICE OF INTENT TO '
     O                                         +000 'CANCEL WORKERS COMPE'
     O                                         +000 'NSATION COVERAGE'
     O          E            NOTIC2         1
     O                                         +030 'FOR NON PAYMENT OF '
     O                                         +000 'PREMIUM'
     O          E            NOTIC2         1
     O                       SEPA80            +005
     O          E            NOTIC2      1  1
     O                       XXACNM            +006
     O                                           60 'Run Date   :'
     O                       XXRNDT              76 '  /  /    '
     O          E            NOTIC2         1
     O                       XXAAD1            +006
     O                                           60 'Policy #   :'
     O                       XXPLCY              76
     O          E            NOTIC2         1
     O                       XXAAD2            +006
     O                                           60 'Division   :'
     O                       EBDIV               76
     O          E            NOTIC2         3
     O                       XXAAD3            +006
     O                                           60 'Policy Year:'
     O                       EBFYR         Z     76
     O          E            NOTIC2         1
     O                       CENAME            +010
     O                                           60 'Report     :'
     O                       XXRPMY              76
     O          E            NOTIC2         1
     O                       XCADR1            +010
     O                                           60 'Due Date   :'
     O                       XXPDDT              76 '  /  /    '
     O          E            NOTIC2         1
     O                       XCADR2            +010
     O          E            NOTIC2         3
     O                       XCADR3            +010
     O          E            NOTIC2      1  0
     O                       EBENAM            +010
     O          E    77      NOTIC2      1
     O               77      XXFDNM            +010
     O          E            NOTIC2      1  1
     O                       XXADR1            +010
     O          E            NOTIC2         1
     O                       XXADR2            +010
     O          E            NOTIC2         2
     O                       XXADR3            +010
     O          E            NOTIC2         3
     O                       SEPA80            +005
     O          E            NOTIC2         1
     O                                         +006 'We have not received'
     O                                         +000 ' your premium or you'
     O                                         +000 'r monthly payroll re'
     O                                         +000 'port for'
      *
      *   PRINT OUT VERBIAGE FOR ACTIVE ACCOUNTS
      *
     O          E    72      NOTIC2         1
     O                       XXLN2A            +006
     O          E    72      NOTIC2         1
     O                                         +006 'cancellation,'
     O                                         +001 'please send the payment '
     O                                         +000 'and the report by '
     O                       XXRBDT            +000 '  /  /    '
     O                                         +000 '.  If'
     O          E    72      NOTIC2         2
     O                                         +006 'payment and '
     O                                         +000 'report have already been'
     O                                         +001 'mailed, please disregard'
     O                                         +001 'this notice.'
      *
      *  PRINT OUT VERBIAGE FOR ACCOUNTS PENDING CANCELLATION
      *
     O          E    73      NOTIC2         1
     O                       XXLN2A            +006
     O          E    73      NOTIC2         1
     O                                         +006 'payment and the report'
     O                                         +001 'by'
     O                       XXRBDT            +000 '  /  /    '
     O                                         +000 '.  If payment and report'
     O                                         +001 'have already'
     O          E    73      NOTIC2         2
     O                                         +006 'been mailed, please dis'
     O                                         +000 'regard this notice.'
     O          E            NOTIC2         3
     O                                         +006 'PLEASE NOTE:  If no '
     O                                         +000 'payment is due, a re'
     O                                         +000 'port must still be m'
     O                                         +000 'ailed.'
     O          E            NOTIC2         3
     O                       SEPA80            +005
     O          E            NOTIC2         2
     O                                         +006 'Make Checks Payable to:'
     O                       OFCKNM            +002
     O          E            NOTIC2         2
     O                                         +006 'Serviced By:           '
     O                       XXCONM            +002
     O          E            NOTIC2         2
     O                                         +006 'Direct Inquiries to:   '
     O                       XXNAME            +002
     O          E            NOTIC2         1
     O                       XXCM01            +006
     O          E            NOTIC2         1
     O                       XXCM02            +006
     O          E            NOTIC2         1
     O                       XXCM03            +006
     O          E            NOTIC2         1
     O                       XXCM04            +006
     O          E            NOTIC2         2
     O                       XXCM05            +006
     O          E    73      NOTIC2         1
     O               73                        +008 'YOUR POLICY IS CURRE'
     O               73                        +000 'NTLY PENDING CANCELL'
     O               73                        +000 'ATION EFFECTIVE'
     O               73      XXCXDT            +001 '  /  /    '
     O               73                        +000 '.'
     O          E    73      NOTIC2         2
     O               73                        +008 'REASON: '
     O               73      XXRESN            +001
     O*
     O          E            NOTIC2      1  1
     O                                         +006 'cc:  CERTIFICATE HOLDERS'
     O          E            NOTIC2         1
     O                                         +011 'INSURED'
     O          E            NOTIC2         1
     O                                         +011 'AGENT:'
     O                       EBAGT#        Z   +001
     O                       EBANAM            +002
     O          E            NOTIC2         1
     O                                         +011 'PRODUCER:'
     O                       APPROD        Z   +001
     O                       XPNAME            +002
     O          E            NOTIC2         1
     O                                         +011 'FILE'
     OQSYSPRT3  E   N71      NOTICE         1 07
     O              N71                        +015 'M O N T H L Y    I N'
     O              N71                        +000 ' T E R I M    L A T '
     O              N71                        +000 'E    N O T I C E'
     O          E    71      NOTICE         1 07
     O                                         +015 'NOTICE OF INTENT TO '
     O                                         +000 'CANCEL WORKERS COMPE'
     O                                         +000 'NSATION COVERAGE'
     O          E    71      NOTICE         1
     O                                         +030 'FOR NON-PAYMENT OF '
     O                                         +000 'PREMIUM'
     O          E            NOTICE         1
     O                       SEPA80            +005
     O          E            NOTICE         1
     O                       XXACNM            +006
     O                                           60 'Run Date   :'
     O                       XXRNDT              79 '  /  /    '
     O          E            NOTICE         1
     O                       XXAAD1            +006
     O                                           60 'Policy #   :'
     O                       X1PLCY              79
     O          E            NOTICE         1
     O                       XXAAD2            +006
     O                                           60 'Division   :'
     O                       EBDIV               79
      *
     O          E            NOTICE         1
     O                       XXAAD3            +006
     O                                           60 'Policy Year:'
     O                       EBFYR         Z     79
      *
     O          E            NOTICE         2
     O              N52                          60 'File #     :'
     O              N52      XXFIL#              79
      *
     O          E            NOTICE         0
     O                       EBENAM            +010
     O          E    77      NOTICE      1
     O               77      XXFDNM            +010
     O          E            NOTICE         1
     O                                           60 'Report     :'
     O              N41      XXRPMY              76
     O               41      XXRPDT              76 '  /  /    '
     O          E            NOTICE         1
     O                       XXADR1            +010
     O                                           60 'Due Date   :'
     O                       XXPDDT              76 '  /  /    '
     O          E            NOTICE         1
     O                       XXADR2            +010
     O          E            NOTICE         2
     O                       XXADR3            +010
     O          E            NOTICE         3
     O                       SEPA80            +005
     O          E            NOTICE         1
     O                                         +006 'We have not received'
     O                                         +000 ' your premium or you'
     O              N41                          66 'r monthly payroll re'
     O              N41                          74 'port for'
     O               41                          66 'r payroll report for'
     O               41      XXRPDT              77 '  /  /    '
      *
      *   PRINT OUT VERBIAGE FOR ACTIVE ACCOUNTS
      *
     O          E    72      NOTICE         1
     O              N41      XXLN2A              80
     O               41                          23 'which was due by '
     O               41      XXPDDT              33 '  /  /    '
     O               41                          57 '. To avoid cancellation,'
     O               41N67                       73 'please send the'
     O               41 67                       76 'payment and report'
     O          E    41 72   NOTICE         1
     O              N67                          14 'payment '
     O              N67                          32 'and the report by '
     O              N67      XXRBDT              42 '  /  /    '
     O              N67                          66 '. If payment and report '
     O              N67                          78 'have already'
     O               67                          23 'must be received '
     O               67                          41 'in our lockbox by '
     O               67      XXRBDT              51 '  /  /    '
     O               67                          75 '. If payment and report '
     O          E    41 72   NOTICE         2
     O              N67                          26 'been mailed, please '
     O              N67                          48 'disregard this notice.'
     O               67                          30 'have already been mailed'
     O               67                          54 ', please disregard this '
     O               67                          61 'notice.'
     O          E    72N41   NOTICE         1
     O              N67                          20 'cancellation, '
     O              N67                          44 'please send the payment '
     O              N67                          62 'and the report by '
     O              N67      XXRBDT              72 '  /  /    '
     O              N67                          77 '.  If'
     O               67                          20 'cancellation, '
     O               67                          44 'payment and report must '
     O               67                          68 'be received in our lockb'
     O               67                          74 'ox by '
     O          E    72N41   NOTICE         1
     O              N67                          18 'payment and '
     O              N67                          42 'report have already been'
     O              N67                          66 ' mailed, please disregar'
     O              N67                          80 'd this notice.'
     O               67      XXRBDT              16 '  /  /    '
     O               67                          40 '. If payment and report '
     O               67                          64 'have already been mailed'
     O               67                          74 ',  please '
     O          E    72N41   NOTICE         1
     O               67                          28 'disregard this notice.'
      *
      *  PRINT OUT VERBIAGE FOR ACCOUNTS PENDING CANCELLATION
      *
     O          E    73N41   NOTICE         1
     O                       XXLN2A            +006
     O          E    73N41   NOTICE         1
     O              N67                          28 'payment and the report'
     O              N67                          31 ' by'
     O              N67      XXRBDT              41 '  /  /    '
     O              N67                          65 '.  If payment and report'
     O              N67                          78 ' have already'
     O               67                          29 'report must be received'
     O               67                          48 ' in our lockbox by '
     O               67      XXRBDT              58 '  /  /    '
     O               67                          76 '.  If payment and '
     O          E    73N41   NOTICE         2
     O              N67                          29 'been mailed, please dis'
     O              N67                          48 'regard this notice.'
     O               67                          29 'and report have already'
     O               67                          53 ' been mailed, please dis'
     O               67                          72 'regard this notice.'
     O          E    73 41   NOTICE         1
     O                                           23 'which was due by '
     O                       XXPDDT              33 '  /  /    '
     O              N67                          51 '. Please send the '
     O              N67                          73 'payment and the report'
     O               67                          55 '. Payment and report m'
     O               67                          77 'ust be received in our'
     O          E    73 41   NOTICE         1
     O              N67                          09 'by '
     O              N67      XXRBDT              19 '  /  /    '
     O              N67                          43 '. If payment and report '
     O              N67                          67 'have already been sent, '
     O              N67                          73 'please'
     O               67                          17 'lockbox by '
     O               67      XXRBDT              27 '  /  /    '
     O               67                          50 '. If payment and report'
     O               67                          74 ' have already been sent,'
     O          E    73 41   NOTICE         2
     O              N67                          28 'disregard this notice.'
     O               67                          22 'please disregard'
     O               67                          35 ' this notice.'
     O          E            NOTICE         2
     O                                         +006 'PLEASE NOTE:  If no '
     O                                         +000 'payment is due, a re'
     O                                         +000 'port must still be m'
     O                                         +000 'ailed.'
     O          E            NOTICE         2
     O              N71      XXOBAL            +006
     O          E    74N71   NOTICE      0
     O                                         +016 'Deposit Due:'
     O                       XXDEPD        J     65
     O          E    75N71   NOTICE      1
     O                                         +016 'Premium Due:'
     O                       XXPRMD        J     65
     O          E    76N71   NOTICE      1
     O                                         +016 'Expense Constant Due:'
     O                       XXEXCD        J     65
     O          E    78N71   NOTICE      1
     O                                         +016 'Deductible Fees Due:'
     O                       XXDEDD        J     65
     O          E    79N71   NOTICE      1
     O                                         +016 'Fees Due:'
     O                       XXFEED        J     65
     O          E   N71      NOTICE         0
     O                       XXUNDL              65
     O          E    80N71   NOTICE      1  2
     O                                         +016 'Total Amount Due:'
     O                       XXTOTD        J     65 '$'
     O          E            NOTICE         3
     O                       SEPA80            +005
     O          E            NOTICE         2
     O                                         +006 'Make Checks Payable to:'
     O                       OFCKNM            +002
     O          E            NOTICE         2
     O                                         +006 'Serviced By:           '
     O                       XXCONM            +002
     O          E            NOTICE         2
     O                                         +006 'Direct Inquiries to:   '
     O                       XXNAME            +002
     O          E            NOTICE         1
     O                       XXCM01            +006
     O          E            NOTICE         1
     O                       XXCM02            +006
     O          E            NOTICE         1
     O                       XXCM03            +006
     O          E            NOTICE         1
     O                       XXCM04            +006
     O          E            NOTICE         2
     O                       XXCM05            +006
     O          E            NOTICE         1
     O               73                        +008 'YOUR POLICY IS CURRE'
     O               73                        +000 'NTLY PENDING CANCELL'
     O               73                        +000 'ATION EFFECTIVE'
     O               73      XXCXDT            +001 '  /  /    '
     O               73                        +000 '.'
     O          E            NOTICE         2
     O               73                        +008 'REASON: '
     O               73      XXRESN            +001
     O          E   N71      NOTICE         2
     O                       XXAHDR            +006
     O                       EBAGT#        Z   +001
     O                       EBANAM            +002
     O          E    71      NOTICE      1  1
     O                                         +006 'cc:  CERTIFICATE HOLDERS'
     O          E    71      NOTICE         1
     O                                         +011 'INSURED'
     O          E    71      NOTICE         1
     O                                         +011 'AGENT:'
     O                       EBAGT#        Z   +001
     O                       EBANAM            +002
     O          E    71      NOTICE         1
     O                                         +011 'PRODUCER:'
     O                       APPROD        Z   +001
     O                       XPNAME            +002
     O          E    71      NOTICE         1
     O                                         +011 'FILE'
**     MNTH
  January
 February
    March
    April
      May
     June
     July
   August
September
  October
 November
 December
