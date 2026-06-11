     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H Bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7226R - RETROSPECTIVE SETTLEMENT PROCESS
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  THIS PROGRAM IS USED TO GENERATE SETTLEMENTS FOR RETROSPEC-  *
     H*  TIVE RATED POLICIES.  THESE POLICIES WILL CALCULATE THEIR    *
     H*  RETROSPECTIVE PREMIUM AND THEN A BILLING RECORD WILL BE      *
     H*  WRITTEN.  A SETTLEMENT LETTER WILL BE GENERATED AND RESOLVED *
     H*  TO DARTS IF SELECTED.                                        *
     H*                                                               *
     H*  THE PROCESS CAN BE RUN EITHER FOR A SINGLE POLICY OR FOR     *
     H*  A MONTH/YEAR WITHIN A RANGE OF GROUPS.  IF RUNNING IN MASS   *
     H*  THEN WE WILL PICK UP ANY POLICIES WHICH HAVE EXPIRED SIX     *
     H*  MONTHS PRIOR TO OUR MONTH/YEAR.  ADDITIONALLY, WE WILL PICK  *
     H*  UP ANY POLICIES 12 MONTHS BACK FROM THAT DATE AND KEEP       *
     H*  CHECKING 12 MONTHS BACK UNTIL WE RUN OUT OF POLICIES. IF THE *
     H*  POLICY HAS NOT SET ON ITS FINAL SETTLEMENT FLAG IT WILL RUN. *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    SETTLEMENT PROCESS PROMPT (W7225R)                         *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  PROGRAMMER: C:65                                             *
     H*  CREATED:    08/23/05              PROJECT: 114WHO            *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/14/24 C:291 PROJECT: WC2974                                *
     H*                Populate Balance to Increasd Limit Min fields. *
     H*                                                               *
     H* 02/27/24 C:333 PROJECT: WC2953                                *
     H*                Premium History Balance to Minimum             *
     H*                                                               *
     H* 09/02/21 C:79  Project: WC2663                                *
     H*                Additional IC changes for claims replentish.   *
     H*                                                               *
     H* 03/25/21 C:79  Project: WC2663                                *
     H*                Invoice Cloud changes                          *
     H*                                                               *
     H* 07/26/17 C:62  Project: 150WAL                                *
     H*                Add Territorial Differential Factor Process.   *
     H*                                                               *
     H* 06/22/17 C:65  PROJECT: Phw# 46465 (CoveRisk)                 *
     H*                Populate ARAP factors into employer history.   *
      *                                                               *
      * 04/07/17 C:062 Project 136WHF                                 *
      *                Add Territory Rating for CareWest.             *
     H*                                                               *
     H* 02/14/13 C:65  SUPPORT: Redmine SY266 # 2337 (CSI)            *
     H*                Correct the short rate cancellation maximum    *
     H*                premium calculation to accumulate payroll      *
     H*                premium from all divisions.                    *
     H*                                                               *
     H* 02/02/12 C:279 SUPPORT: 79133                                 *
     H*                Modified to correct changes for supprt 78866   *
     H*                and consider only non-zero ELRETN to limit the *
     H*                loss calculation.                              *
     H*                                                               *
     H* 01/20/12 C:279 SUPPORT: 78866                                 *
     H*                Modified to use ELRETN to limit the LOSS       *
     H*                Calculation.                                   *
     H*                                                               *
     H* 07/25/11 C:191 PROJECT: SY255 PRISS 149                       *
     H*                FIX 114WVZ CODE TO CHECK BSLPGM OF P AND I FOR *
     H*                NON CSI CLIENT                                 *
     H*                                                               *
     H* 02/16/11 C:246 PROJECT: 132WMJ PRISS 14                       *
     H*                Added Inital program user and date to the A/R  *
     H*                batch file to display the date on A/R batch    *
     H*                screen when transaction being written in batch *
     H*                file.                                          *
     H*                                                               *
     H* 11/23/10 C:255 PROJECT: 114WVZ                                *
     H*                This program will accept new parameter passed  *
     H*                by W7226C(Bill Schedule Plan type). Based on   *
     H*                the value in it program calculate Retrospective*
     H*                premium calculation or Retention plan calcula- *
     H*                tion.                                          *
     H*                                                               *
     H* 07/20/10 C:65  PROJECT: 114WRH                                *
     H*                REVISE THE SETTLEMENT CALCULATION FOR SHARING  *
     H*                PREMIUM ACROSS THE GROUP BY BASING EACH        *
     H*                POLICY'S SHARE OF THE TOTAL PREMIUM BASED ON   *
     H*                THEIR STANDARD PREMIUM AND TO SHARE THE TOTAL  *
     H*                PREMIUM BASED ON THAT SHARE, REGARDLESS OF     *
     H*                MINIMUM, MAXIMUM, OR NOT HITTING EITHER.       *
     H*                                                               *
     H* 06/21/10 C:255 Project: 114WVY                                *
     H*                Populate the 2nd period deviations information *
     H*                into employer history file for split deviations*
     H*                                                               *
     H* 02/25/10 C:33  Project: 25WRC  Priss #13                      *
     H*                Process Retro independently of Large Deductible*
     H*                                                               *
     H* 01/16/10 C:191 Project: 25WQI PRISS #26                       *
     H*               - SUPRESS LEADING ZEROS FROM STATEMENT NUMBER   *
     H*                 CHANGE FIELD TO 10 NUMERIC                    *
     H*                                                               *
     H* 01/14/10 C:190 Project: 25WRC                                 *
     H*                When writing the employer history record for   *
     H*                TASB, copy the field for stabilization factor  *
     H*                for deviation #2 instead of the normal perce-  *
     H*                ntage field ELDEV2.                            *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -For 025, load the invoice # when writing       *
     H*                 billing records                               *
     H*               -Print 'Statement #' along with the invoice #   *
     H*                                                               *
     H* 10/08/09 C:180 SUPPORT 70192 MISSING PDF                     *
     H*
     H* 05/21/09 C:220 SUPPORT: 68429                                 *
     H*                RETRO SETTLEMENT LETTER NEED CORRECTION        *
     H*                                                               *
     H* 04/07/09 C:136 PROJECT: 139WLE                                *
     H*                ALLOW RETROSPECTIVE RATING ON S/R POLICIES     *
     H*                                                               *
     H* 12/25/08 C:197 PROJECT: 114WRH                                *
     H*                RETRO - ENDORSEMENT AND PROCESSING             *
     H*                                                               *
     H* 09/04/08 C:062 PROJECT: WC1611                                *
     H*                USE ELNWRW INSTEAD OF EQNWRW                   *
     H*                                                               *
     H* 04/04/08 C:136 SUPPORT: WA114 62581                           *
     H*                CORRECTIONS NEEDED FOR 114WOH TO HANDLE        *
     H*                SUMMARIZED POLICIES CORRECTLY                  *
     H*                                                               *
     H* 10/04/07 C:136 SUPPORT: WA114 60409                           *
     H*                CORRECT WHICH PERIOD IS BEING CALCULATED FOR   *
     H*                A SINGLE RUN.                                  *
     H*                                                               *
     H* 08/30/07 C:193 PROJECT: 114WOH                                *
     H*                CHANGE RETRO CALCULATION BY POLICY             *
     H*                                                               *
     H* 06/11/07 C:136 SUPPORT: WA114 55857                           *
     H*                DO NOT NOT REPRINT ON MASS BILLING UNLESS      *
     H*                A PRIOR SETTLEMENT REPORT HAS BEEN RUN.        *
     H*                                                               *
     H* 04/09/07  C:65  PROJECT:  114WMF                              *
     H*                 CHANGE PREMIUM NOTICE CALL TO USE BASE PARM   *
     H*                 LIST FOR CLIENT 114 (BACK TO BASE). PASS THE  *
     H*                 RESOLVE TO DARTS FLAG ALONG TO CALL TOO.      *
     H*                                                               *
     H* 01/09/07  C:65  PROJECT:  139WGA                              *
     H*                 CHANGE PROCESSING FROM WRITING WMEAP RECORDS  *
     H*                 TO WRITING WDEAP A/R BATCH RECORDS.  ALSO,    *
     H*                 REMOVE ANY CLAIMS REPLINISHMENT BILLINGS      *
     H*                 FROM AN A/R SUMMARIZED DIVISION IF PRESENT.   *
     H*                                                               *
     H*****************************************************************
     F*
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMEPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     UF   E           K DISK
     FWDELL4    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     FWMCML     IF   E           K DISK
     FWMRHL1    IF   E           K DISK
     FWMEAL     UF A E           K DISK
     FWDEAL     IF   E           K DISK
     F                                     RENAME(PEA01:PEA01B)
     FWDEAL4    UF A E           K DISK
     F                                     RENAME(PEA01:PEA02B)
     FWDEAL2    IF   E           K DISK
     F                                     RENAME(PEA01:PEA03B)
     FWMEAL24   IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWMRSL     IF   E           K DISK
     FWMOFL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMAPL     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDCXL2    IF   E           K DISK
     FWTCRL     IF   E           K DISK
     FWDEHL     O  A E             DISK
     FWDEHL4    IF   E           K DISK
     f*** Beg Add *** 03/25/21 **************************************************************
     fwximp     o  a e             disk
     f*** End Add *** 03/25/21 **************************************************************
     FW7226O    O    E             PRINTER OFLIND(*IN98)
     F                                     USROPN

     D*
     D COM             S             50    DIM(3)
     D MNTH            S              9    DIM(12) CTDATA PERRCD(1)
     D TIT             S             60    DIM(10) CTDATA PERRCD(1)             TITLE TABLE
     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)            # DAYS IN MONTHS
     D SR              S             15    DIM(20) CTDATA PERRCD(1)             TITLE TABLE
     D EMP             S             14  0 DIM(9999)                            EMP
     D PRM             S             11  2 DIM(9999)                            PREMIUM
     D STD             S             11  2 DIM(9999)                            STANDARD PREMIUM
     D*
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')
     d*** Beg Add *** 09/02/21 **********************************************
     d intInv#ClmRep   s                   Like(EAnInv)
     d*** End Add *** 09/02/21 **********************************************
     d*** Beg Add *** 03/25/21 **********************************************
     d isInvoiceCloud  s              1n   inz
     d isICgroup       s              1n   Inz
     d xxRoot          s             10    Inz(' ')
     d inKey           s             20
     d str3rdVrb       s                   Like(OF3rdVrb)
     d strInv#Hd       s             15
     d strInv#         s             40
     d dtSysDate       s               d   datfmt(*ISO)
     d
     d DocRoot         pr                  ExtPgm('RTVDOCROOT')
     d   $#Root_                     10
     d
     d/copy wPrar
     d/copy sPrModule
     d/copy wPrPmtSvc
     d*** End Add *** 03/25/21 **********************************************
     D*
     D                 DS
     D  QQDCID                 1     12
     D  QQ1ST                  1      2
     D  QQ2ND                  6      8
     D  QQ3RD                 10     12
     D*
     D                 DS
     D  OFAPHN                 1      7  0
     D  OFAPH1                 1      3
     D  OFAPH2                 4      7
     D                 DS
     D  AGPHN1                 1      7  0
     D  XXAPH1                 1      3
     D  XXAPH2                 4      7
     D                 DS
     D  OFAZIP                 1      9  0
     D  OFMZ1                  1      5
     D  OFMZ2                  6      9
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
     D  XXRPYM                 1      5  0
     D  XXYY                   1      3  0
     D  XXMM                   4      5  0
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0
     D                 DS
     D  XQDT                   1      8  0
     D  XXMTH                  1      2  0
     D  XXDAY                  3      4
     D  XXYR                   5      8
     D                 DS
     D  QQPOL#                 1     15
     D  QPFND                  1      3
     D  QPEMP#                 4     12
     D  QPFYR                 13     15
     D                 DS
     D  DSDT2                  1      8  0
     D  DSMM2                  1      2
     D  DSDD2                  3      4
     D  DSYY2                  5      8
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECTIVE DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C*
     C*   COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  CLAIMS FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  RESERVE HISTORY FILE.
     C*
     C     RHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  EMPLOYER A/R FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXSDIV
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER A/R BATCH FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSTAT
     C*
     C     EAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBAT#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXADIV
     C                   KFLD                    XXFYR
     C*
     C     EAKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBAT#
     C*
     C     EAKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EAKEY4        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXSDIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPTDT            7 0
      *
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*
     C*   EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE.
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  FUND YEAR KEY LIST.
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  OFFICE MASTER
     C*
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMOFF#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C* Key List for the Agent Producer Master File
     C*
     C     APKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    AHPROD
     C*
     C* CANCELLATION REASON FILE
     C*
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    CXCDAT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFFND
     C     *LIKE         DEFINE    ELFND         XXTFND
     C     *LIKE         DEFINE    ELFND         SVFND
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELEMP#        @@EMP#
     C     *LIKE         DEFINE    ELDIV         @@DIV
     C     *LIKE         DEFINE    ELDIV         XXSDIV
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    ELRDF1        XXRDF
     C     *LIKE         DEFINE    EAPODT        XXLPDT
     C     *LIKE         DEFINE    EAPODT        XXEFD
     C     *LIKE         DEFINE    EAPODT        XXEXD
     C     *LIKE         DEFINE    EAPODT        X1FROM
     C     *LIKE         DEFINE    EAPODT        X1TO
     C     *LIKE         DEFINE    ELDIV         XXADIV
     C     *LIKE         DEFINE    EASYCD        XXSYCD
     C     *LIKE         DEFINE    TTDESC        XXDESC
     C     *LIKE         DEFINE    TTTYPE        XXARTY
     C     *LIKE         DEFINE    TTCTYP        XXCTYP
     C     *LIKE         DEFINE    ELEMP#        SVEMP#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFFND            3
     C                   PARM                    QQTFND            3
     C                   PARM                    QQPMYR            6
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQLVDT            8
     C                   PARM                    QQBAR             2
     C                   PARM                    QQINTD            2
     C                   PARM                    QQDART            1
     C                   PARM                    QQRMCD            5
     C                   PARM                    QQRPRN            1
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQBSPT            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
       //*** Beg Add *** 03/25/21 ****************************************
       isInvoiceCloud = IsModEnabled('Invoice Cloud');
       DocRoot(xxRoot);
       //*** End Add *** 03/25/21 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFFND        XXFFND
     C                   MOVE      QQTFND        XXTFND
     C                   MOVE      QQPMYR        XXPMYR            6 0
     C*
     C     Q1PRMS        IFGT      15
     C                   MOVE      QQBSPT        XXBSPT            1
     C                   ENDIF
     C*
     C                   Z-ADD     XXPMYR        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRPYM
     C                   DO        6
     C                   SUB       1             XXMM
     C     XXMM          IFEQ      0
     C                   SUB       1             XXYY
     C                   Z-ADD     12            XXMM
     C                   ENDIF
     C                   ENDDO
     C                   Z-ADD     XXRPYM        SVRPYM            5 0
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT            7 0
     C*
     C* POSTING DATE FOR BILLINGS (THE PRESENT DATE PLUS INTERIM DAYS)
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT            7 0
     C*
     C                   MOVE      QQINTD        XXINTD            2 0
     C*
     C     XXINTD        IFNE      *ZERO
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      XXINTD        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          XXDT8             8 0
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXINTD IFNE
     C*
     C                   MOVE      QQFND         SVFND
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQLVDT        XXLVDT            8 0
     C                   Z-ADD     0             XXSEQ#            2 0
     C                   Z-ADD     0             XXBAT#            9 0
     C                   Z-ADD     0             XXTOTB           11 2
     C                   MOVE      *BLANKS       XXSYCD
     C*
     C*  FIND INFO FOR A/R BILLED CODE
     C*
     C                   MOVEL(P)  QQBAR         XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     TTDESC        XXDESC
     C                   MOVE      TTTYPE        XXARTY
     C                   MOVE      TTCTYP        XXCTYP
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* BUILD LOSSES AS OF DATE FOR MASS RUN.
     C*
     C     XXPMYR        IFNE      *ZERO
     C                   MOVEL     XXPMYR        XX                2 0
     C                   MOVEL     XXPMYR        XXMTH
     C                   MOVE      XXPMYR        XXYR
     C                   MOVE      MD(XX)        XXDAY
     C                   MOVE      XQDT          XXLVDT
     C                   ENDIF                                                  XXPMYR IFNE
     C*
     C                   MOVE      XXLVDT        XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MNTH(X)       XQLVDT           20
     C                   CAT       XXDAY:1       XQLVDT
     C                   CAT       ',':0         XQLVDT
     C                   CAT       XXYR:1        XQLVDT
     C                   CAT       '.':0         XQLVDT
     C*
     C                   Z-ADD     XXLVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXLDAT            7 0
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C* PROCESS SINGLE POLICY REQUEST.
     C*
     C     SVFND         IFNE      *ZERO
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   MOVEL     EMDIV         XXSDIV
     C                   MOVEL     XXSDIV        XXDIV
     C                   LEAVE
     C*
     C                   ELSE
     C     EMKY1         READE     PEM01                                  60
     C                   ENDIF
     C*
     C                   ENDDO
     C     ELKY          CHAIN(N)  PEL01                              40
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              41
     C*
     C                   MOVE      *OFF          XXRTRO
     C     *IN41         IFEQ      *OFF
     C     BSLPGM        IFEQ      XXBSPT
     C     XXBSPT        OREQ      'B'
     C     BSLPGM        OREQ      'P'
     C     XXCLT#        ANDNE     '114'
     C     BSLPGM        OREQ      'I'
     C     XXCLT#        ANDNE     '114'
     C                   MOVE      *ON           XXRTRO
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRTRO        IFEQ      *ON
     C*
     C* DETERMINE WHICH ADJUSTMENT PERIOD WE ARE IN FOR SINGLE REQUEST
     C*
     C                   MOVEL     ELPTDT        XXRPYM
     C                   DO        18
     C                   ADD       1             XXMM
     C     XXMM          IFEQ      13
     C                   ADD       1             XXYY
     C                   Z-ADD     1             XXMM
     C                   ENDIF
     C                   ENDDO
     C*
     C* WE ARE EITHER IN 1ST, 2ND, OR 3RD ADJUSTMENT FOR RETRO DEV FACTOR
     C*
     C                   MOVEL     XXLDAT        XXYM              5 0
     C     XXYM          IFLT      XXRPYM
     C                   Z-ADD     1             XXSEQ#
     C                   ELSE
     C*
     C                   ADD       1             XXYY
     C     XXYM          IFLT      XXRPYM
     C                   Z-ADD     2             XXSEQ#
     C                   ELSE
     C*
     C                   ADD       1             XXYY
     C     XXYM          IFLT      XXRPYM
     C                   Z-ADD     3             XXSEQ#
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* PROCESS SETTLEMENT.
     C*
     C                   MOVEL     *ZEROS        XXTPRM           11 2
     C                   MOVEL     *ZEROS        XTPREM           11 2
     C                   MOVEL     *ZEROS        XXDIVC            5 0
     C                   MOVEL     *ZEROS        XTPAY            11 2
     C                   MOVEL     *ZEROS        XTPAY1           11 2
     C                   MOVEL     *ZEROS        XTPAY2           11 2
     C                   MOVEL     *ZEROS        XTPAY3           11 2
     C                   MOVE      *OFF          *IN68
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     EXRETG        IFEQ      'Y'
     C                   MOVE      *ON           *IN68
     C*
     C                   EXSR      GRPP
     C*
     C                   ENDIF                                                  EXRETG ='Y'
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C     ELKY3         CHAIN     PEL03                              62
     C     *IN62         DOWEQ     *OFF
     C*
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              63
     C*
     C                   MOVE      *OFF          XXRTRO            1
     C     *IN63         IFEQ      *OFF
     C     BSLPGM        IFEQ      XXBSPT
     C     XXBSPT        OREQ      'B'
     C     BSLPGM        OREQ      'P'
     C     XXCLT#        ANDNE     '114'
     C     BSLPGM        OREQ      'I'
     C     XXCLT#        ANDNE     '114'
     C                   MOVE      *ON           XXRTRO
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRTRO        IFEQ      *ON
     C     ELPFDT        ANDNE     ELPTDT
     C*
     C                   MOVEL     ELDIV         XXDIV
     C                   EXSR      PRMC
     C                   ADD       XXAMT         XXTPRM
     C                   ADD       XXPREM        XTPREM
     C                   ADD       XXPAY         XTPAY
     C                   ADD       XXPAY1        XTPAY1
     C                   ADD       XXPAY2        XTPAY2
     C                   ADD       XXPAY3        XTPAY3
     C                   ADD       1             XXDIVC
     C*
     C                   ENDIF
     C*
     C     ELKY3         READE     PEL03                                  62
     C                   ENDDO
     C     ELKY3         CHAIN     PEL03                              62
     C     *IN62         DOWEQ     *OFF
     C*
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              63
     C*
     C                   MOVE      *OFF          XXRTRO            1
     C     *IN63         IFEQ      *OFF
     C     BSLPGM        IFEQ      XXBSPT
     C     XXBSPT        OREQ      'B'
     C     BSLPGM        OREQ      'P'
     C     XXCLT#        ANDNE     '114'
     C     BSLPGM        OREQ      'I'
     C     XXCLT#        ANDNE     '114'
     C                   MOVE      *ON           XXRTRO
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRTRO        IFEQ      *ON
     C     ELPFDT        ANDNE     ELPTDT
     C*
     C                   MOVEL     ELDIV         XXDIV
     C                   EXSR      RETRO
     C*
     C                   ENDIF
     C*
     C     ELKY3         READE     PEL03                                  62
     C                   ENDDO
     C*
     C                   ENDIF                                                  *IN41 IFEQ
     C*
     C                   ENDIF                                                  *IN40 IFEQ
     C*
     C                   ELSE                                                   SVFND IFNE
     C*
     C* PROCESS MASS REQUEST.
     C*
     C                   Z-ADD     XXFFND        XXFND
     C*
     C                   Z-ADD     0             S2FND             3 0
     C                   Z-ADD     0             S2FYR             3 0
     C*
     C     FNKY          SETLL     PFN01
     C                   READ      PFN01                                  40
     C*
     C     *IN40         DOWEQ     *OFF
     C     FNFND         ANDLE     XXTFND
     C*
     C                   Z-ADD     FNFND         XXFND
     C                   Z-ADD     SVRPYM        XXRPYM
     C                   Z-ADD     1             XXSEQ#
     C                   MOVE      *OFF          *IN68
     C*
     C     FNKY          SETLL     PF101
     C     FNKY          READE     PF101                                  41
     C     *IN41         IFEQ      *OFF
     C*
     C* PROCESS EACH EXPIRATION MONTH UNTIL WE HIT FIRST MONTH OF GROUP.
     C*
     C                   MOVEL     F1STPO        XFYM              5 0
     C*
     C     XXRPYM        DOWGE     XFYM
     C*
     C                   MOVEL     XXRPYM        XXPTDT
     C                   MOVE      '01'          XXPTDT
     C*
     C     ELKY1         SETLL     PEL02
     C     ELKY2         READE     PEL02                                  42
     C*
     C     *IN42         DOWEQ     *OFF
     C*
     C                   MOVEL     ELPTDT        XXYM              5 0
     C     XXYM          IFEQ      XXRPYM
     C*
     C                   MOVEL     ELEMP#        XXEMP#
     C                   MOVEL     ELDIV         XXDIV
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C     EMSTRP        ANDEQ     'Y'
     C                   MOVEL     EMDIV         XXSDIV
     C     ELFSET        IFNE      'Y'
     C     QQRPRN        ANDNE     'Y'
     C     QQRPRN        OREQ      'Y'
     C     ELSTAM        ANDNE     *ZERO
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              43
     C*
     C                   MOVE      *OFF          XXRTRO
     C     *IN43         IFEQ      *OFF
     C     BSLPGM        IFEQ      XXBSPT
     C     XXBSPT        OREQ      'B'
     C     BSLPGM        OREQ      'P'
     C     XXCLT#        ANDNE     '114'
     C     BSLPGM        OREQ      'I'
     C     XXCLT#        ANDNE     '114'
     C                   MOVE      *ON           XXRTRO
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRTRO        IFEQ      *ON
     C*
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C                   Z-ADD     ELFYR         XXFYR
     C*
     C                   MOVEL     *ZEROS        XXTPRM
     C                   MOVEL     *ZEROS        XTPREM
     C                   MOVEL     *ZEROS        XTPAY
     C                   MOVEL     *ZEROS        XTPAY1
     C                   MOVEL     *ZEROS        XTPAY2
     C                   MOVEL     *ZEROS        XTPAY3
     C                   MOVEL     *ZEROS        XXDIVC
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     EXRETG        IFEQ      'Y'
     C                   MOVE      *ON           *IN68
     C*
     C     S2FND         IFNE      XXFND
     C     S2FYR         ORNE      XXFYR
     C*
     C                   MOVEL     XXFND         S2FND
     C                   MOVEL     XXFYR         S2FYR
     C*
     C                   Z-ADD     ELEMP#        SVEMP#
     C*
     C                   EXSR      GRPP
     C*
     C                   Z-ADD     SVEMP#        XXEMP#
     C*
     C                   ENDIF                                                  S2FND<> XXFND
     C*
     C                   ENDIF                                                  EXRETG ='Y'
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C     ELKY3         CHAIN     PEL03                              62
     C     *IN62         DOWEQ     *OFF
     C*
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              63
     C*
     C                   MOVE      *OFF          XXRTRO            1
     C     *IN63         IFEQ      *OFF
     C     BSLPGM        IFEQ      XXBSPT
     C     XXBSPT        OREQ      'B'
     C     BSLPGM        OREQ      'P'
     C     XXCLT#        ANDNE     '114'
     C     BSLPGM        OREQ      'I'
     C     XXCLT#        ANDNE     '114'
     C                   MOVE      *ON           XXRTRO
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRTRO        IFEQ      *ON
     C     ELPFDT        ANDNE     ELPTDT
     C*
     C                   MOVEL     ELDIV         XXDIV
     C                   EXSR      PRMC
     C                   ADD       XXAMT         XXTPRM
     C                   ADD       XXPREM        XTPREM
     C                   ADD       XXPAY         XTPAY
     C                   ADD       XXPAY1        XTPAY1
     C                   ADD       XXPAY2        XTPAY2
     C                   ADD       XXPAY3        XTPAY3
     C                   ADD       1             XXDIVC
     C*
     C                   ENDIF
     C*
     C     ELKY3         READE     PEL03                                  62
     C                   ENDDO
     C     ELKY3         CHAIN     PEL03                              62
     C     *IN62         DOWEQ     *OFF
     C*
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              63
     C*
     C                   MOVE      *OFF          XXRTRO            1
     C     *IN63         IFEQ      *OFF
     C     BSLPGM        IFEQ      XXBSPT
     C     XXBSPT        OREQ      'B'
     C     BSLPGM        OREQ      'P'
     C     XXCLT#        ANDNE     '114'
     C     BSLPGM        OREQ      'I'
     C     XXCLT#        ANDNE     '114'
     C                   MOVE      *ON           XXRTRO
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRTRO        IFEQ      *ON
     C     ELPFDT        ANDNE     ELPTDT
     C*
     C                   MOVEL     ELDIV         XXDIV
     C                   EXSR      RETRO
     C*
     C                   ENDIF
     C*
     C     ELKY3         READE     PEL03                                  62
     C                   ENDDO
     C*
     C                   ENDIF                                                  *IN43 IFEQ
     C*
     C                   ENDIF                                                  ELFSET IFNE
     C*
     C                   ENDIF                                                  EMSTRP='Y'
     C*
     C     ELKY2         READE     PEL02                                  42
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN42
     C                   ENDIF                                                  XXYM IFEQ
     C*
     C                   ENDDO                                                  *IN42 DOWEQ
     C*
     C                   SUB       1             XXYY
     C                   ADD       1             XXSEQ#
     C                   ENDDO                                                  XXRPYM DOWGE
     C*
     C                   ENDIF                                                  *IN41 IFEQ
     C*
     C                   READ      PFN01                                  40
     C                   ENDDO                                                  *IN40 DOWEQ
     C*
     C                   ENDIF                                                  SVFND IFNE
     C*
     C     XXBAT#        IFNE      *ZERO
      *
      *  UPDATE ALL BATCH RECORDS CREATED WITH THE TOTAL BATCH AMOUNT
      *
     C     EAKEY2        CHAIN     PEA02B                             60
      *
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     XXTOTB        EABATA
     C                   UPDATE    PEA02B
     C     EAKEY2        READE     PEA02B                                 60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
      *  Delete the A/R batch dummy record.
      *
     C                   MOVE      *HIVAL        XXFND
     C                   MOVE      *HIVAL        XXEMP#
     C                   MOVE      *HIVAL        XXADIV
     C                   MOVE      *HIVAL        XXFYR
     C     EAKEY1        CHAIN     PEA02B                             60
     C  N60              DELETE    PEA02B
      *
     C                   ENDIF                                                  XXBAT# IFNE
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*====================================================================
     C* Get the invoice # for billing records being created
     C*====================================================================
     C     GETINV        BEGSR
     C*
     C     *LIKE         DEFINE    EACO#         #ICO#
     C     *LIKE         DEFINE    EAFND         #IFND
     C     *LIKE         DEFINE    EAEMP#        #IEMP#
     C     *LIKE         DEFINE    EADIV         #IDIV
     C     *LIKE         DEFINE    EAFYR         #IFYR
     C     *LIKE         DEFINE    EAPODT        #IDT
     C     *LIKE         DEFINE    EANINV        #IINV#
     C*
     C     XXCLT#        IFEQ      '025'
     C*
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'F'
     C     EAARTY        OREQ      'H'
     C*
     C                   CALL      'SRINV#'
     C                   PARM      EACO#         #ICO#
     C                   PARM      EAFND         #IFND
     C                   PARM      EAEMP#        #IEMP#
     C                   PARM      EADIV         #IDIV
     C                   PARM      EAFYR         #IFYR
     C                   PARM      EAPODT        #IDT
     C     EANINV        PARM                    #IINV#
     C*
     C                   Z-ADD     EANINV        EAINV#
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  RETRO SUBROUTINE  -  PROCESS SETTLEMENT FOR POLICY.
     C********************************************************************
     C     RETRO         BEGSR
     C*
     C     F1KY          CHAIN     PF101                              60
     C                   MOVE      *BLANKS       XXST              2
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXST
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVE      *ON           *IN62
     C                   ELSE
     C                   MOVE      *OFF          *IN62
     C                   ENDIF
     C*
     C*  FIND IF THE EMPLOYER IS CANCELLED AND WHEN.
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   Z-ADD     1             XXX               1 0
     C                   MOVE      *BLANK        SVCNCL            1
     C                   DO        5
     C     CD(XXX)       IFGE      ELPFDT
     C     CD(XXX)       ANDLE     ELPTDT
     C     ED(XXX)       ANDNE     0
     C                   MOVE      'Y'           SVCNCL
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             XXX
     C                   ENDDO
     C*
     C* SAVE NCCI CANCEL CODE IF THE POLICY IS CANCELLED.
     C*
     C                   MOVE      *BLANK        XXCXCD            2
     C     SVCNCL        IFEQ      'Y'
     C*
     C     ELKY          SETGT     PCX01
     C     ELKY          READPE    PCX01                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CXRSN         XXKEY
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CRNCCI        XXCXCD
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  SVCNCL IFEQ
     C*
     C*   XXMIN AND XXMAX ARE SET BASED ON XTPREM VALUE
     C*
     C     EXRETG        IFEQ      'Y'
     C*
     C     ELMNFC        MULT(H)   XGPREM        XXMIN            11 0
     C                   Z-ADD(H)  XGPREM        XXMAXP           11 0
     C     ELMXFC        MULT(H)   XGPREM        XXMAX            11 0
     C*
     C                   Z-ADD(H)  XGPREM        XTPRM1            9 0
     C                   Z-ADD(H)  XGPREM        XTPRM2            9 0
     C*
     C                   ELSE
     C*
     C     ELMNFC        MULT(H)   XTPREM        XXMIN            11 0
     C                   Z-ADD(H)  XTPREM        XXMAXP           11 0
     C     ELMXFC        MULT(H)   XTPREM        XXMAX            11 0
     C*
     C                   Z-ADD(H)  XTPREM        XTPRM1            9 0
     C                   Z-ADD(H)  XTPREM        XTPRM2            9 0
     C                   ENDIF
     C*
     C                   EXSR      PRMC
     C*
     C* ADJUST MINIMUM/MAXIMUM PREMIUM VALUES FOR CANCELLATIONS.
     C*
     C                   MOVE      *OFF          *IN64
     C                   MOVE      *OFF          *IN65
     C*
     C     XXDIV         IFEQ      XXSDIV
     C     EXRETG        OREQ      'Y'
     C*
     C                   MOVE      *ON           *IN65
     C*
     C     SVCNCL        IFEQ      'Y'
     C*
     C*  GET NUMBER OF EMPLOYER POLICY DAYS
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   MOVE      XQFROM        QQFROM            8 0
     C                   MOVE      XQTO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   EXSR      SRLEAP
     C*
     C                   Z-ADD     QQDAYS        PDAYS             5 0
     C*
     C     PDAYS         DIV(H)    365           XXPROR            5 4
     C*
     C* FOR NON-PAYMENT CANCELLATIONS, MAXIMUM PREMIUM IS ANNUALIZED.
     C*
     C     XXCXCD        IFEQ      '05'
     C*
     C     XTPREM        DIV(H)    XXPROR        XXMAX
     C                   Z-ADD(H)  XXMAX         XXMAXP
     C                   Z-ADD(H)  XXMAXP        XTPRM2
     C                   MULT(H)   ELMXFC        XXMAX
     C*
     C                   ENDIF                                                  XXCXCD IFEQ
     C*
     C* IF POLICY IS CANCELLED BY INSURED AND SHORT-RATED THEN MINIMUM
     C* AND MAXIMUM PREMIUM VALUES ARE SET TO SHORT RATE PREMIUM.
     C* MAXIMUM PREMIUM IS SET TO ANNUALIZED PAYROLL X RATES X EXP MOD.
     C*
     C     ELSRPR        IFEQ      'S'
     C*
     C                   MOVE      *ON           *IN64
     C*
     C                   Z-ADD(H)  XTPREM        XXMIN
     C*
     C                   Z-ADD     0             XXMAX
     C*
     C* USE PAYROLLS FOR EACH MODIFIER IF SPLIT FOR AUDIT.
     C*
     C     F1SPLA        IFEQ      'Y'
     C     *IN62         ANDEQ     *ON
     C*
     C* FIRST MODIFIER
     C*
     C     XTPAY1        DIV(H)    XXPROR        XXWK10
     C     ELEXM1        IFNE      *ZERO
     C                   MULT(H)   ELEXM1        XXWK10
     C                   ENDIF
     C                   ADD       XXWK10        XXMAX
     C*
     C* SECOND MODIFIER
     C*
     C     XTPAY2        DIV(H)    XXPROR        XXWK10
     C                   MULT(H)   ELEXM2        XXWK10
     C                   ADD       XXWK10        XXMAX
     C*
     C* THIRD MODIFIER
     C*
     C     XTPAY3        DIV(H)    XXPROR        XXWK10
     C                   MULT(H)   ELEXM3        XXWK10
     C                   ADD       XXWK10        XXMAX
     C*
     C                   Z-ADD(H)  XXMAX         XXMAXP
     C                   Z-ADD(H)  XXMAXP        XTPRM2
     C                   MULT(H)   ELMXFC        XXMAX
     C*
     C                   ELSE
     C*
     C* PRORATE PAYROLL TOTAL FOR EACH MODIFIER.
     C*
     C*  FIRST MODIFIER
     C*
     C                   Z-ADD     ELPFDT        XXEFD
     C*
     C     ELEMD2        IFEQ      *ZERO
     C     ELEMD2        ORGT      ELPTDT
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELEMD2        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      PREMA
     C                   Z-ADD(H)  XXPAYA        XXPAY1
     C*
     C     XXPAY1        DIV(H)    XXPROR        XXWK10
     C     ELEXM1        IFNE      *ZERO
     C                   MULT(H)   ELEXM1        XXWK10
     C                   ENDIF
     C                   ADD       XXWK10        XXMAX
     C*
     C*  SECOND MODIFIER
     C*
     C     ELEMD2        IFNE      *ZERO
     C*
     C                   Z-ADD     ELEMD2        XXEFD
     C     ELPTDT        IFLT      ELEMD2
     C                   Z-ADD     ELPTDT        XXEFD
     C                   ENDIF
     C*
     C     ELEMD3        IFEQ      *ZERO
     C     ELEMD3        ORGT      ELPTDT
     C                   Z-ADD     ELPTDT        XXEXD
     C                   ELSE
     C                   Z-ADD     ELEMD2        XXEXD
     C                   ENDIF
     C*
     C                   EXSR      PREMA
     C                   Z-ADD(H)  XXPAYA        XXPAY2
     C*
     C     XXPAY2        DIV(H)    XXPROR        XXWK10
     C     ELEXM2        IFNE      *ZERO
     C                   MULT(H)   ELEXM2        XXWK10
     C                   ENDIF
     C                   ADD       XXWK10        XXMAX
     C*
     C                   ENDIF                                                  ELEMD2 IFNE
     C*
     C*  THIRD MODIFIER
     C*
     C     ELEMD3        IFNE      *ZERO
     C*
     C                   Z-ADD     ELEMD3        XXEFD
     C     ELPTDT        IFLT      ELEMD3
     C                   Z-ADD     ELPTDT        XXEFD
     C                   ENDIF
     C*
     C                   Z-ADD     ELPTDT        XXEXD
     C*
     C                   EXSR      PREMA
     C                   Z-ADD(H)  XXPAYA        XXPAY3
     C*
     C     XXPAY3        DIV(H)    XXPROR        XXWK10
     C     ELEXM3        IFNE      *ZERO
     C                   MULT(H)   ELEXM3        XXWK10
     C                   ENDIF
     C                   ADD       XXWK10        XXMAX
     C*
     C                   ENDIF                                                  ELEMD3 IFNE
     C*
     C                   Z-ADD(H)  XXMAX         XXMAXP
     C                   Z-ADD(H)  XXMAXP        XTPRM2
     C                   MULT(H)   ELMXFC        XXMAX
     C*
     C                   ENDIF                                                  F1SPLA IFEQ
     C*
     C                   ENDIF                                                  ELSRPR IFEQ
     C*
     C                   ENDIF                                                  SVCNCL IFEQ
     C*
     C* CHECK IF PREMIUM IS ABOVE OR BELOW MAXIMUM/MINIMUM.
     C*
     C                   MOVE      *ZEROS        XXDIFF           11 2
     C*
     C     EXRETG        IFNE      'Y'
     C*
     C     XXTPRM        IFLT      XXMIN
     C     XXMIN         SUB       XXTPRM        XXDIFF
     C                   ENDIF
     C*
     C     XXTPRM        IFGT      XXMAX
     C     XXMAX         SUB       XXTPRM        XXDIFF
     C                   ENDIF
     C*
     C                   ELSE                                                   EXRETG <> 'Y'
     C*
     C     XXPREM        DIV(H)    XGPREM        X1FACT
     C     X1FACT        MULT(H)   XGAMT2        XXWK10
     C     XXWK10        SUB       XXAMT         XXDIFF
     C*
     C* APPLY ROUNDING DIFFERENCE TO EACH POLICY UNTIL IT RUNS OUT.
     C*
     C     XGDIFF        IFNE      0
     C*
     C     XGDIFF        IFLT      0
     C*
     C     XGDIFF        IFGT      SVDIFF
     C                   ADD       XGDIFF        XXDIFF
     C                   SUB       XGDIFF        XGDIFF
     C                   ELSE
     C                   ADD       SVDIFF        XXDIFF
     C                   SUB       SVDIFF        XGDIFF
     C                   ENDIF
     C*
     C                   ENDIF                                                  XGDIFF < 0
     C*
     C     XGDIFF        IFGT      0
     C*
     C     XGDIFF        IFLT      SVDIFF
     C                   ADD       XGDIFF        XXDIFF
     C                   SUB       XGDIFF        XGDIFF
     C                   ELSE
     C                   ADD       SVDIFF        XXDIFF
     C                   SUB       SVDIFF        XGDIFF
     C                   ENDIF
     C*
     C                   ENDIF                                                  XGDIFF > 0
     C*
     C                   ENDIF                                                  XGDIFF <>0
     C*
     C                   ENDIF                                                  EXRETG <>'Y'
     C*
     C                   ADD       XXDIFF        XXAMT
     C*
     C                   ENDIF                                                  XXDIV=XXSDIV
     C*
     C* ADD TERRORISM AND EXPENSE CONSTANT TO RETRO PREMIUM.
     C*
     C     XXAMT         ADD       ELPRTD        XXAMTT           11 2
     C*
     C     ELFAF         IFEQ      'A'
     C                   ADD       ELATER        XXAMTT
     C                   ADD       ELADTR        XXAMTT
     C                   ELSE
     C                   ADD       ELETER        XXAMTT
     C                   ADD       ELEDTR        XXAMTT
     C                   ENDIF                                                  ELFAF IFEQ
     C*
     C*
     C* FIND STATE REPORTING DIVISION FOR A/R SUMMARY.
     C*
     C                   MOVE      *OFF          *IN61
     C*
     C     ELARSM        IFEQ      'Y'
     C*
     C                   MOVE      *ON           *IN61
     C*
     C     XXDIV         IFNE      XXSDIV
     C                   EXSR      REVAR
     C                   ENDIF                                                  XXDIV IFNE
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C*
     C* BILLED PREMIUM WILL BE PRORATED PREMIUM + CLAIMS REPL BILLINGS.
     C*
     C                   SELECT
     C     BSSEFL        WHENEQ    'E'
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELAAPR        XXARAM           11 2
     C                   ELSE
     C                   Z-ADD     ELANPR        XXARAM
     C                   ENDIF                                                  ELFAF IFEQ
     C     BSSEFL        WHENEQ    'S'
     C                   Z-ADD     0             XXARAM
     C                   ENDSL
     C*
     C* TOTAL AMT OF PREMIUM AND CLAIMS REPLINISHMENT BILLINGS IN A/R.
     C*
     C  N61EAKY          CHAIN(N)  PEA01                              60
     C   61EAKY1         CHAIN(N)  PEA02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   SELECT
     C     BSSEFL        WHENEQ    'E'
     C     EAARTY        IFEQ      'B'
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'C'
     C                   ADD       EAAMT         XXARAM
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  EAARTY IFEQ
     C     BSSEFL        WHENEQ    'S'
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'D'
     C                   ADD       EAAMT         XXARAM
     C                   ENDIF
     C                   ENDSL
     C*
     C  N61EAKY          READE(N)  PEA01                                  60
     C   61EAKY1         READE(N)  PEA02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* ADD IN UNTRANSFERRED BATCH RECORDS TO PRIOR BILLED AS WELL.
     C*
     C  N61EAKEY3        CHAIN     PEA03B                             60
     C   61EAKEY4        CHAIN     PEA03B                             60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EATFFL        IFNE      'Y'
     C*
     C     *IN61         IFEQ      *OFF
     C     *IN61         OREQ      *ON
     C     EARDIV        ANDEQ     XXDIV
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'C'
     C     TTTYPE        ANDEQ     'B'
     C                   ADD       EAAMT         XXARAM
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C  N61EAKEY3        READE     PEA03B                                 60
     C   61EAKEY4        READE     PEA03B                                 60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* SETTLEMENT AMOUNT IS THE DIFFERENCE BETWEEN RETRO PREM AND A/R.
     C*
     C     XXAMTT        SUB       XXARAM        XXAR             11 2
     C*
     C* WRITE AN A/R BATCH RECORD FOR THE SETTLEMENT AMOUNT.
     C*
     C     XXAR          IFNE      *ZERO
     C     QQRPRN        ANDNE     'Y'
     C*
     C     XXBAT#        IFEQ      *ZERO
     C                   EXSR      SRBAT
     C                   ENDIF
     C*
     C                   CLEAR                   PEA02B
     C*
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   Z-ADD     XXEMP#        EAEMP#
     C  N61              Z-ADD     XXDIV         EADIV
     C   61              Z-ADD     XXSDIV        EADIV
     C   61              Z-ADD     XXDIV         EARDIV
     C*
     C                   Z-ADD     XXBAT#        EABAT#
     C                   Z-ADD     *ZEROS        EACHK#
     C                   Z-ADD     *ZEROS        EABATA
     C                   MOVE      XXSYCD        EASYCD
     C                   MOVE      'N'           EATFAP
     C                   MOVE      'N'           EATFFL
     C                   Z-ADD     *ZEROS        EATFDT
     C                   Z-ADD     *ZEROS        EATFAM
     C*
     C                   MOVE      QQBAR         EAARCD
     C                   MOVEL     XXDESC        EADESC
     C                   MOVE      XXARTY        EAARTY
     C                   MOVE      XXCTYP        EACTYP
     C*
     C                   Z-ADD     XXAR          EAAMT
     C                   Z-ADD     XXAR          EACKAM
     C                   ADD       EAAMT         XXTOTB
     C*
     C                   MOVE      XXPODT        EAPODT
     C                   MOVE      XXTRDT        EATRDT
     C*
     C                   MOVE      *BLANKS       EASTAT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
      ** Move the intial Date,User and Program name to display the date
      ** in A/R Batch screen when individual transactions are being
      ** writtern.
     C                   MOVE      Q1USER        EAIUSR
     C                   MOVE      Q1PGM         EAIPGM
     C                   MOVEL     SRDTO         EAIDT
     C*
     C                   EXSR      GETINV

       //*** Beg Add *** 03/25/21 ******************************************
       // Find any other claims replenishment billing records for the same
       // posting date, and if it finds any, use that invoice #.  If it
       // does not find one, then request a new invoice #.
       If isInvoiceCloud and IcGroupOpt(EAco#:EAfnd);
          //*** Beg Add *** 09/02/21 *******************************************
          intInv#ClmRep = 0;
          Exec Sql Select EAninv Into :intInv#ClmRep From WMEAP
           Where EAco#=:EAco# And EAfnd=:EAfnd And
           EAemp#=:EAemp# And EAdiv=:EAdiv And EAfyr=:EAfyr
           And EACTyp = 'C' And
           EApoDt=:EApoDt And EAarTy in ('B');

          // If not found check A/R batch file WDEAP
          If intInv#ClmRep = 0;
             Exec Sql Select EAinv# Into :intInv#ClmRep From WDEAP
              Where EAco#=:EAco# And EAfnd=:EAfnd And
              EAemp#=:EAemp# And EAdiv=:EAdiv And EAfyr=:EAfyr
              And EACTyp = 'C' And
              EApoDt=:EApoDt and EAarCd = :qqBar and EAstat <> 'T';
          Endif;
          If intInv#ClmRep = 0;
          //*** End Add *** 09/02/21 *******************************************
             inKey =  %EditC(EAco#:'X') + %EditC(EAfnd:'X') + %EditC(EAemp#:'X')
               + %EditC(EAdiv:'X');
             EAinv# = GetNext3rdPartySeq#(inKey);
          //*** Beg Add *** 09/02/21 *******************************************
          Else;
             EAinv# = intInv#ClmRep;
          Endif;
          //*** End Add *** 09/02/21 *******************************************
       Endif;
       //*** End Add *** 03/25/21 ******************************************
     C                   WRITE     PEA02B
     C*
     C                   ENDIF                                                  XXAR IFNE
     C*
     C* UPDATE EMPLOYER DETAIL WITH SETTLEMENT AMOUNT.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C     QQRPRN        ANDNE     'Y'
     C*
     C                   Z-ADD     XXAMTT        ELSTAM
     C                   MOVE      XXFSET        ELFSET
     C                   MOVEL     Q1PGM         ELPGM
     C                   MOVEL     Q1USER        ELUSER
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   MOVEL     SRDTO         ELSTDT
     C                   Z-ADD     DSTIME        ELTIME
     C                   UPDATE    PEL01
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* CREATE EMPLOYER HISTORY WITH RETROSPECTIVE PREMIUM.
     C*
     C     QQRPRN        IFNE      'Y'
     C*
     C     ELKY          SETGT     PEQ01
     C     ELKY          READPE    PEQ01                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C                   CLEAR                   PEH01
     C*
     C                   MOVE      XXPODT        EHEFDT
     C                   MOVE      XXTRDT        EHTRDT
     C                   MOVE      'X'           EHFAF
     C                   MOVE      *BLANK        EHFAF2
     C                   MOVE      *BLANK        EHREVF
     C                   MOVE      *BLANK        EHRWFL
     C                   MOVE      *ZEROS        EHRWDT
     C                   Z-ADD     XXAMTT        EHWRPR
     C*
     C                   MOVE      ELASP1        EHASP1
     C                   MOVE      ELCREF        EHCREF
     C                   MOVE      ELCREX        EHCREX
     C                   MOVE      ELASP2        EHASP2
     C                   MOVE      ELDVEF        EHDVEF
     C                   MOVE      ELD1S2        EHD1S2
     C                   MOVE      ELD1D2        EHD1D2
     C                   MOVE      ELD1X2        EHD1X2
     C                   MOVE      ELD2S2        EHD2S2
     C                   MOVE      ELD2D2        EHD2D2
     C                   MOVE      ELD2X2        EHD2X2
     C                   MOVE      ELDVEX        EHDVEX
     C                   MOVE      ELASP3        EHASP3
     C                   MOVE      ELDISC        EHDISC
     C                   MOVE      ELEXM1        EHEXM1
     C                   MOVE      ELEMD1        EHEMD1
     C                   MOVE      ELEXM2        EHEXM2
     C                   MOVE      ELEMD2        EHEMD2
     C                   MOVE      ELEXM3        EHEXM3
     C                   MOVE      ELEMD3        EHEMD3
     C                   Move      ELARP1        EHARP1
     C                   Move      ELARP2        EHARP2
     C                   Move      ELARP3        EHARP3
     C                   MOVE      ELILCD        EHILCD
     C                   MOVE      ELILEF        EHILEF
     C                   MOVE      ELILEX        EHILEX
     C                   MOVE      ELDCDE        EHDCDE
     C                   MOVE      ELDDCT        EHDDCT
     C                   MOVE      ELCXBL        EHCXBL
     C                   MOVE      ELDCEF        EHDCEF
     C                   MOVE      ELDCEX        EHDCEX
     C                   MOVE      ELASPM        EHASPM
     C                   MOVE      ELMCEF        EHMCEF
     C                   MOVE      ELMCEX        EHMCEX
     C                   MOVE      ELASPS        EHASPS
     C                   MOVE      ELSCEF        EHSCEF
     C                   MOVE      ELSCEX        EHSCEX
     C                   MOVE      ELRTCD        EHRTCD
     C                   MOVE      ELSTAT        EHSTAT
     C                   MOVE      ELDBCR        EHDBCR
     C                   Select
     C                   When      xxClt# = '025'
     C                   Eval      EHstFc = ELstFc
     C                   When      xxClt# = '136'
     C                   Eval      EHdev2F   = ELdev2F
     C                   Other
     C                   Eval      EHdev2 = ELdev2
     C                   Endsl
     C                   Eval      EHdev2WBP = ELdev2BP
     C                   Eval      EHdev2WAP = ELdev2AP
     C                   Eval      EHtdf     = ELtdf
     C                   MOVE      ELD2EF        EHD2EF
     C                   MOVE      ELD2EX        EHD2EX
     C                   MOVE      ELSDEF        EHSDEF
     C                   MOVE      ELSDEX        EHSDEX
     C                   MOVE      ELLFEF        EHLFEF
     C                   MOVE      ELAVCC        EHAVCC
     C                   MOVE      ELINL2        EHI2CD
     C                   MOVE      ELI2EF        EHI2EF
     C                   MOVE      ELI2EX        EHI2EX
     C                   MOVE      ELWVPC        EHWVPC
     C                   MOVE      *BLANKS       EHFWAT
     C                   MOVE      ELWMIN        EHWMIN
     C                   MOVE      ELWVFL        EHWVFL
     C                   MOVE      ELDCD2        EHDCD2
     C                   MOVE      EL2DEF        EH2DEF
     C                   MOVE      EL2DEX        EH2DEX
     C                   MOVE      ELDD2P        EHDD2P
     C                   MOVE      ELCMUL        EHCMUL
     C*
     C                   MOVE      EQCO#         EHCO#
     C                   MOVE      EQFND         EHFND
     C                   MOVE      EQFYR         EHFYR
     C                   MOVE      EQEMP#        EHEMP#
     C                   MOVE      EQDIV         EHDIV
     C                   MOVE      EQBSCD        EHBSCD
     C                   MOVE      EQPFDT        EHPFDT
     C                   MOVE      EQPTDT        EHPTDT
     C                   MOVE      EQANDT        EHANDT
     C                   MOVE      EQPROR        EHPROR
       //*** Beg Add ***** 02/27/24 ***************************
        EHWBAL  = EQWBAL;
        EHWAMAN = EQWAMAN;
        EHWASTD = EQWASTD;
        EHWANORM = EQWANORM;
        EHWAANN = EQWAANN;
        EHWASBJ = EQWASBJ;
        EHWAMOD = EQWAMOD;
        EHWAIL1 = EQWAIL1;
        EHWAIL2 = EQWAIL2;
        EHWADC1 = EQWADC1;
        EHWADC2 = EQWADC2;
        EHWADC3 = EQWADC3;
        EHWADV2 = EQWADV2;
        EHWADCT = EQWADCT;
        EHWADD2 = EQWADD2;
        EHWADCM = EQWADCM;
        EHWADCS = EQWADCS;
        EHWADC  = EQWADC;
        EHWAWAV = EQWAWAV;
        EHWATCA = EQWATCA;
        EHWAPRD = EQWAPRD;
        EHWAPRF = EQWAPRF;
        EHWAXPA = EQWAXPA;
        EHWAVLC = EQWAVLC;
        EHWAARC = EQWAARC;
        EHWAD12 = EQWAD12;
        EHWAD22 = EQWAD22;
        EHWAHCN = EQWAHCN;
        EHWAUSA = EQWAUSA;
        EHWAXP1 = EQWAXP1;
        EHWAXP2 = EQWAXP2;
        EHWAXP3 = EQWAXP3;
        EHWARP1 = EQWARP1;
        EHWARP2 = EQWARP2;
        EHWARP3 = EQWARP3;
        EHWALOS = EQWALOS;
       //*** End Add ***** 02/27/24 ***************************
       //*** Beg Add ***** 08/14/24 ***************************
        EHWBIL = EQWBIL;
        EHIBIL = EQIBIL;
        EHWBIL2 = EQWBIL2;
        EHIBIL2 = EQIBIL2;
       //*** End Add ***** 08/14/24 ***************************
     C                   MOVE      EQWMPR        EHWMPR
     C                   Z-ADD     0             EHXMPR
     C                   MOVE      EQIMPR        EHIMPR
     C                   Z-ADD     0             EHJMPR
     C                   MOVE      EQWSPR        EHWSPR
     C                   Z-ADD     0             EHXSPR
     C                   MOVE      EQISPR        EHISPR
     C                   Z-ADD     0             EHJSPR
     C                   MOVE      EQWNMP        EHWNMP
     C                   Z-ADD     0             EHXNMP
     C                   MOVE      EQINMP        EHINMP
     C                   Z-ADD     0             EHJNMP
     C                   MOVE      EQWAPR        EHWAPR
     C                   Z-ADD     0             EHXAPR
     C                   MOVE      EQIAPR        EHIAPR
     C                   Z-ADD     0             EHJAPR
     C                   MOVE      EQWMNP        EHWMNP
     C                   MOVE      EQIMNP        EHIMNP
     C                   MOVE      EQPRTD        EHPRTD
     C                   MOVE      EQWDC1        EHWDC1
     C                   Z-ADD     0             EHXDC1
     C                   MOVE      EQIDC1        EHIDC1
     C                   Z-ADD     0             EHJDC1
     C                   MOVE      EQWDC2        EHWDC2
     C                   Z-ADD     0             EHXDC2
     C                   MOVE      EQIDC2        EHIDC2
     C                   Z-ADD     0             EHJDC2
     C                   MOVE      EQD1W2        EHD1W2
     C                   Z-ADD     0             EHD1C2
     C                   MOVE      EQD1I2        EHD1I2
     C                   Z-ADD     0             EHD1H2
     C                   MOVE      EQWDC3        EHWDC3
     C                   Z-ADD     0             EHXDC3
     C                   MOVE      EQIDC3        EHIDC3
     C                   Z-ADD     0             EHJDC3
     C                   MOVE      EQWXP1        EHWXP1
     C                   Z-ADD     0             EHXXP1
     C                   MOVE      EQIXP1        EHIXP1
     C                   Z-ADD     0             EHJXP1
     C                   MOVE      EQWXP2        EHWXP2
     C                   Z-ADD     0             EHXXP2
     C                   MOVE      EQIXP2        EHIXP2
     C                   Z-ADD     0             EHJXP2
     C                   MOVE      EQWXP3        EHWXP3
     C                   Z-ADD     0             EHXXP3
     C                   MOVE      EQIXP3        EHIXP3
     C                   Z-ADD     0             EHJXP3
     C                   MOVE      EQWIN1        EHWIN1
     C                   Z-ADD     0             EHXIN1
     C                   MOVE      EQIIN1        EHIIN1
     C                   Z-ADD     0             EHJIN1
     C                   MOVE      EQWDCT        EHWDCT
     C                   Z-ADD     0             EHXDCT
     C                   MOVE      EQIDCT        EHIDCT
     C                   Z-ADD     0             EHJDCT
     C                   MOVE      EQWDCM        EHWDCM
     C                   Z-ADD     0             EHXDCM
     C                   MOVE      EQIDCM        EHIDCM
     C                   Z-ADD     0             EHJDCM
     C                   MOVE      EQWDCS        EHWDCS
     C                   Z-ADD     0             EHXDCS
     C                   MOVE      EQIDCS        EHIDCS
     C                   Z-ADD     0             EHJDCS
     C                   MOVE      EQWDCR        EHWDCR
     C                   Z-ADD     0             EHXDCR
     C                   MOVE      EQIDCR        EHIDCR
     C                   Z-ADD     0             EHJDCR
     C                   MOVE      EQWDC         EHWDC
     C                   Z-ADD     0             EHXDC
     C                   MOVE      EQIDC         EHIDC
     C                   Z-ADD     0             EHJDC
     C                   MOVE      EQWSBJ        EHWSBJ
     C                   Z-ADD     0             EHXSBJ
     C                   MOVE      EQISBJ        EHISBJ
     C                   Z-ADD     0             EHJSBJ
     C                   MOVE      EQWMPM        EHWMPM
     C                   Z-ADD     0             EHXMPM
     C                   MOVE      EQIMPM        EHIMPM
     C                   Z-ADD     0             EHJMPM
     C                   MOVE      EQWFRS        EHWFRS
     C                   Z-ADD     0             EHXFRS
     C                   MOVE      EQIFRS        EHIFRS
     C                   Z-ADD     0             EHJFRS
     C                   MOVE      EQWDV2        EHWDV2
     C                   Z-ADD     0             EHXDV2
     C                   MOVE      EQIDV2        EHIDV2
     C                   Z-ADD     0             EHJDV2
     C                   MOVE      EQD2W2        EHD2W2
     C                   Z-ADD     0             EHD2C2
     C                   MOVE      EQD2I2        EHD2I2
     C                   Z-ADD     0             EHD2H2
     C                   MOVE      EQTXPR        EHTXPR
     C                   MOVE      EQWIN2        EHWIN2
     C                   Z-ADD     0             EHXIN2
     C                   MOVE      EQIIN2        EHIIN2
     C                   Z-ADD     0             EHJIN2
     C                   MOVE      EQWWAV        EHWWAV
     C                   Z-ADD     0             EHXWAV
     C                   MOVE      EQIWAV        EHIWAV
     C                   Z-ADD     0             EHJWAV
     C                   MOVE      EQWADL        EHWADL
     C                   Z-ADD     0             EHXADL
     C                   MOVE      EQIADL        EHIADL
     C                   Z-ADD     0             EHJADL
     C                   MOVE      EQWBDC        EHWBDC
     C                   Z-ADD     0             EHXBDC
     C                   MOVE      EQIBDC        EHIBDC
     C                   Z-ADD     0             EHJBDC
     C                   MOVE      EQWTDC        EHWTDC
     C                   Z-ADD     0             EHXTDC
     C                   MOVE      EQITDC        EHITDC
     C                   Z-ADD     0             EHJTDC
     C                   MOVE      ELNWRW        EHNWRW
     C                   MOVE      EQWLOS        EHWLOS
     C                   Z-ADD     0             EHXLOS
     C                   MOVE      EQILOS        EHILOS
     C                   Z-ADD     0             EHJLOS
     C                   MOVE      EQPRTF        EHPRTF
     C                   MOVE      EQWTER        EHWTER
     C                   Z-ADD     0             EHXTER
     C                   MOVE      EQITER        EHITER
     C                   Z-ADD     0             EHJTER
     C                   MOVE      EQWRP1        EHWRP1
     C                   Z-ADD     0             EHXRP1
     C                   MOVE      EQIRP1        EHIRP1
     C                   Z-ADD     0             EHJRP1
     C                   MOVE      EQWRP2        EHWRP2
     C                   Z-ADD     0             EHXRP2
     C                   MOVE      EQIRP2        EHIRP2
     C                   Z-ADD     0             EHJRP2
     C                   MOVE      EQWRP3        EHWRP3
     C                   Z-ADD     0             EHXRP3
     C                   MOVE      EQIRP3        EHIRP3
     C                   Z-ADD     0             EHJRP3
     C                   MOVE      EQWDD2        EHWDD2
     C                   Z-ADD     0             EHXDD2
     C                   MOVE      EQIDD2        EHIDD2
     C                   Z-ADD     0             EHJDD2
     C                   MOVE      EQWVLC        EHWVLC
     C                   MOVE      EQIVLC        EHIVLC
     C                   Z-ADD     0             EHXVLC
     C                   Z-ADD     0             EHJVLC
     C                   MOVE      EQWDTR        EHWDTR
     C                   Z-ADD     0             EHXDTR
     C                   MOVE      EQIDTR        EHIDTR
     C                   Z-ADD     0             EHJDTR
     C*
     C                   Z-ADD     EHWAPR        SVWAPR           11 2
     C*
     C     *IN60         DOWEQ     *OFF
     C     EQFAF         ANDNE     'X'
     C     ELKY          READPE    PEQ01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     *IN60         IFEQ      *OFF
     C     EHWRPR        SUB       EQWRPR        EHXRPR
     C                   ELSE
     C     EHWRPR        SUB       SVWAPR        EHXRPR
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   MOVE      Q1USER        EHUSER
     C                   MOVE      Q1PGM         EHPGM
     C                   MOVE      DSTIME        EHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
     C                   MOVEL     SRDTO         EHIDT
     C                   MOVE      Q1USER        EHIUSR
     C*
     C                   WRITE     PEH01
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  QQRPRN IFNE
     C*
     C* PRINT SETTLEMENT LETTER.
     C*
     C     ELFSET        IFEQ      'Y'
     C                   MOVE      *ON           *IN56
     C                   ELSE
     C                   MOVE      *OFF          *IN56
     C                   ENDIF
     C*
     C*Set Indicator on for client 114 to print Retention premium otherwise
     C*Retrospective premium.
     C*
     C     BSLPGM        IFEQ      'R'
     C                   MOVE      *ON           *IN71
     C                   ELSE
     C                   MOVE      *OFF          *IN71
     C                   ENDIF
     C*
     C                   Z-ADD     ELBSFC        X1BSFC
     C                   Z-ADD     ELLCVF        X1LCVF
     C                   Z-ADD     XXPRM         X1PRM
     C                   Z-ADD     XXPRM         X2PRM
     C                   Z-ADD     XXFPRM        X1FPRM
     C                   Z-ADD     XXFPRM        X2FPRM
     C                   Z-ADD     XXBPM         X1BPM
     C                   Z-ADD     XXBPM         X2BPM
     C                   Z-ADD     XXFBPR        X1FBPR
     C                   Z-ADD     XXFBPR        X2FBPR
     C                   Z-ADD     XXPREM        X1PREM
     C     XOLOSS        SUB       XOLOSF        XOLOS1
     C     XCLOSS        SUB       XCLOSF        XCLOS1
     C                   Z-ADD     XOLOSF        XOFLS1
     C                   Z-ADD     XCLOSF        XCFLS1
     C                   Z-ADD     XXLOSF        XTFLS1
     C                   Z-ADD     XXLOSF        XTFLS2
     C     XXLOSS        SUB       XXLOSF        XTLOS1
     C     XXLOSS        SUB       XXLOSF        XTLOS2
     C     XXCLOS        SUB       XXCLOF        X1CLOS
     C     XXCLOS        SUB       XXCLOF        X2CLOS
     C                   Z-ADD     XXCLOF        X1FCLS
     C                   Z-ADD     XXCLOF        X2FCLS
     C                   Z-ADD     XCELP         XDELP
     C                   Z-ADD     XCELP         XEELP
     C                   Z-ADD     XCELPF        XDELPF
     C                   Z-ADD     XCELPF        XEELPF
     C     XDELP         ADD       XDELPF        XXELP
     C                   Z-ADD     XCRDP         XDRDP
     C                   Z-ADD     XCRDP         XERDP
     C                   Z-ADD     XCRDPF        XDRDPF
     C                   Z-ADD     XCRDPF        XERDPF
     C     XDRDP         ADD       XDRDPF        XXRDP
     C                   Z-ADD     XTTOTF        XUTOTF
     C                   Z-ADD     XTTOTL        XUTOTL
     C                   Z-ADD     XXTOTF        X1TOTF
     C                   Z-ADD     XXTOTL        X1TOTL
     C                   Z-ADD     XXMIN         X1MIN
     C                   Z-ADD     XXMAX         X1MAX
     C                   Z-ADD     XXAMT         X1AMT
     C                   Z-ADD     XXAMTT        X1AMTT
     C     XXAMTT        SUB       XXAMT         X1AMTD
     C                   Z-ADD     XXARAM        X1ARAM
     C                   Z-ADD     XXAR          X1AR
     C*
     C     BSLPGM        IFEQ      'R'
     C                   MOVEL     TIT(7)        XQTITL           40
     C                   ELSE
     C                   MOVEL     TIT(1)        XQTITL           40
     C                   ENDIF
     C*
     C     BSLPGM        IFEQ      'R'
     C*
     C     XXSEQ#        IFGT      0
     C     XXSEQ#        ANDLT     21
     C                   Z-ADD     XXSEQ#        Z                 2 0
     C                   MOVEL(P)  SR(Z)         XXRHD
     C     XXRHD         CAT       TIT(10):1     XXRHD
     C                   ELSE
     C                   MOVEL(P)  TIT(10)       XXRHD
     C                   ENDIF                                                  XXSEQ# IFGT
     C                   ELSE
     C     XXSEQ#        IFGT      0
     C     XXSEQ#        ANDLT     21
     C                   Z-ADD     XXSEQ#        Z                 2 0
     C                   MOVEL(P)  SR(Z)         XXRHD
     C     XXRHD         CAT       TIT(6):1      XXRHD
     C                   ELSE
     C                   MOVEL(P)  TIT(6)        XXRHD
     C                   ENDIF                                                  XXSEQ# IFGT
     C*
     C                   ENDIF                                                  BSLPGM = 'R'
     C*
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C                   MOVEL     FNMCTY        XXLAST           56
     C                   MOVEL     FNMST         XXSTZP           13
     C                   MOVE      FNMZIP        XXSTZP
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C                   MOVEL     FNMZIP        XXST10           10
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   ENDIF                                                  XXZIP4 IFEQ
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C*
     C                   MOVEL(P)  FNNAME        XQFNNM           60
     C                   MOVEL(P)  FNMAD1        XQFAD1           40
     C*
     C     FNMAD2        IFEQ      *BLANKS
     C                   MOVEL(P)  XXLAST        XQFAD2           40
     C                   MOVE      *BLANKS       XQFCTY           40
     C                   ELSE
     C                   MOVEL(P)  FNMAD2        XQFAD2           40
     C                   MOVEL(P)  XXLAST        XQFCTY           40
     C                   ENDIF                                                  FNMAD2 IFEQ
     C*
     C                   MOVEL(P)  'Policy R'    XQQREF           20
     C                   CAT       'efere':0     XQQREF
     C                   CAT       'nce:':0      XQQREF
     C*
     C                   MOVE      XXFND         QPFND
     C                   MOVE      XXEMP#        QPEMP#
     C                   MOVE      XXFYR         QPFYR
     C                   MOVE      QQPOL#        XQPOL#           15
       //*** Beg Add *** 03/25/21 ******************************************
       If EAinv# <> 0;
          strInv#Hd = 'Invoice #: ';
          strInv# = %Editc(xxCo#:'X') + %Editc(xxFnd:'X') + %Editc(xxEmp#:'X')
           + '-' + %Editc(EAInv#:'X');
       Else;
          strInv#Hd = *blanks;
          strInv# = *blanks;
       Endif;
       //*** End Add *** 03/25/21 ******************************************
     C*
     C                   MOVEL(P)  'For Peri'    XQPERD           20
     C                   CAT       'od:':0       XQPERD
     C*
     C                   MOVEL(P)  'Print'       XQPROD           20
     C                   CAT       'Date:':1     XQPROD
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MNTH(X)       XQPRDT           20
     C                   CAT       XXDAY:1       XQPRDT
     C                   CAT       ',':0         XQPRDT
     C                   CAT       XXYR:1        XQPRDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MNTH(X)       XQPFDT           20
     C                   CAT       XXDAY:1       XQPFDT
     C                   CAT       ',':0         XQPFDT
     C                   CAT       XXYR:1        XQPFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MNTH(X)       XQPTDT           20
     C                   CAT       XXDAY:1       XQPTDT
     C                   CAT       ',':0         XQPTDT
     C                   CAT       XXYR:1        XQPTDT
     C*
     C                   MOVEL(P)  XQPFDT        XQPOLP           45
     C                   CAT       ' to':1       XQPOLP
     C                   CAT       XQPTDT:1      XQPOLP
     C*
     C     ELKY          CHAIN(N)  PEL01                              60
     C*
     C     XXDIVC        IFGT      1
     C                   MOVEL(P)  'Division'    XQDIVH           20
     C                   CAT       ':':0         XQDIVH
     C                   MOVE      XXDIV         XQDIV             5
     C                   ELSE
     C                   MOVE      *BLANKS       XQDIVH
     C                   MOVE      *BLANKS       XQDIV
     C                   ENDIF                                                  XXDIVC IFGT
     C*
     C                   MOVE      *BLANKS       XQANNH
     C                   MOVE      *BLANKS       XQANND
     C*
     C                   MOVE      '0'           *IN87
     C     ELANDT        COMP      0                                      88
     C  N88ELANDT        COMP      ELPFDT                                 88
     C  N88              Z-ADD     ELANDT        XXANDT
     C  N88              ADD       1             XXANYR
     C  N88XXANDT        COMP      ELPFDT                               8888
     C  N88              MOVE      '1'           *IN87
     C*
     C     *IN87         IFEQ      *ON
     C                   MOVEL(P)  'Annivers'    XQANNH           20
     C                   CAT       'ary Da':0    XQANNH
     C                   CAT       'te:':0       XQANNH
     C                   Z-ADD     ELANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MNTH(X)       XQANND           20
     C                   CAT       XXDAY:1       XQANND
     C                   CAT       ',':0         XQANND
     C                   CAT       XXYR:1        XQANND
     C                   ENDIF                                                  *IN87 IFEQ
     C*
     C                   MOVEL(P)  'Rating S'    XQRSTH           20
     C                   CAT       'tate:':0     XQRSTH
     C*
     C     ELKY          CHAIN     PRP01                              60
     C     *IN60         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANKS
     C                   MOVEL(P)  RPCAP#        XXCAP#           25
     C                   MOVEL(P)  'Carrier'     XQCAPH           20
     C                   CAT       'Pol#:':1     XQCAPH
     C                   MOVE      *ON           *IN66
     C                   ELSE
     C                   MOVE      *BLANKS       XXCAP#
     C                   MOVE      *BLANKS       XQCAPH
     C                   MOVE      *OFF          *IN66
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   MOVE      *BLANKS       XQCXHD
     C                   MOVE      *BLANKS       XQCXDT
     C                   MOVE      '0'           *IN86
     C*
     C     SVCNCL        IFEQ      'Y'
     C                   MOVE      '1'           *IN86
     C                   MOVEL(P)  'Cancella'    XQCXHD           20
     C                   CAT       'tion D':0    XQCXHD
     C                   CAT       'ate:':0      XQCXHD
     C                   Z-ADD     EMCAD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MNTH(X)       XQCXDT           20
     C                   CAT       XXDAY:1       XQCXDT
     C                   CAT       ',':0         XQCXDT
     C                   CAT       XXYR:1        XQCXDT
     C                   ENDIF                                                  SVCNCL IFEQ
     C*
     C                   MOVE      XXCO#         ##CO#             3
     C                   MOVE      XXFND         ##FND             3
     C                   MOVE      XXEMP#        ##EMP#            9
     C                   MOVE      XXDIV         ##DIV             5
     C                   MOVE      'W'           ##ODR             1
     C                   MOVE      ' '           ##NODR            1
     C                   MOVE      ' '           ##AODR            1
     C                   MOVE      *BLANKS       ##EMN1           50
     C                   MOVE      *BLANKS       ##EMN2           50
     C                   MOVE      *BLANKS       ##EMN3           50
     C                   MOVE      *BLANKS       ##EMN4           50
     C                   MOVE      *BLANKS       ##EMN5           50
     C                   MOVE      *BLANKS       ##EMN6           50
     C                   MOVE      *BLANKS       ##EMN7           50
     C                   CALL      'SRADDR'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ODR
     C                   PARM                    ##NODR
     C                   PARM                    ##AODR
     C                   PARM                    ##EMN1
     C                   PARM                    ##EMN2
     C                   PARM                    ##EMN3
     C                   PARM                    ##EMN4
     C                   PARM                    ##EMN5
     C                   PARM                    ##EMN6
     C                   PARM                    ##EMN7
     C*
     C                   MOVE      ##EMN1        XXEMN1           50
     C                   MOVE      ##EMN2        XXEMN2           50
     C                   MOVE      ##EMN3        XXEMN3           50
     C                   MOVE      ##EMN4        XXEMN4           50
     C                   MOVE      ##EMN5        XXEMN5           50
     C                   MOVE      ##EMN6        XXEMN6           50
     C                   MOVE      ##EMN7        XXEMN7           50
     C*
     C                   CLEAR                   XXAHDR
     C                   MOVE      *BLANKS       XXPROD           45
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     EXAHDR        IFEQ      *BLANKS
     C                   MOVEL(P)  'Agent'       EXAHDR
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL(P)  'Agent'       EXAHDR
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   MOVEL(P)  EXAHDR        XXAHDR           16
     C     XXAHDR        CAT(P)    ':':0         XXAHDR
     C*
     C     AHKEY         SETGT     PAH01
     C     AHKEY         READPE    PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C     AHKEY         READPE    PAH01                                  50
     C                   END
     C*
     C     *IN50         IFEQ      '0'
     C                   MOVEL     AHAGT#        XXAGT#
     C*
     C     AAKEY         CHAIN     PAA01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     AAFEIN        XXFEIN
     C                   MOVEL     AAMOD         XXMOD
     C                   ELSE
     C                   MOVE      *ZEROS        XXFEIN
     C                   MOVE      *ZEROS        XXMOD
     C                   END
     C*
     C     APKY          CHAIN     PAP01                              50
     C     *IN50         IFEQ      *OFF
     C*
     C     APFSN         IFNE      *BLANKS
     C                   MOVEL     APFSN         XXPROD
     C                   CAT       APLSN:1       XXPROD
     C                   ELSE
     C                   MOVEL     APLSN         XXPROD
     C                   ENDIF                                                  APFSN IFNE
     C*
     C                   ENDIF                                                  *IN50 IFEQ
     C*
     C     AGKEY         CHAIN     PAG01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     AGNAME        ANAME            34
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C     AGMAD1        IFNE      *BLANKS
     C                   MOVEL     AGMAD1        AADD1            34
     C                   MOVEL     AGMCTY        XXSHRT           19
     C                   MOVEL     XXSHRT        XXLAST           56
     C                   MOVEL     AGMST         XXSTZP           13
     C                   MOVE      AGMZIP        XXSTZP
     C                   ELSE
     C                   MOVEL     AGPAD1        AADD1
     C                   MOVEL     AGPCTY        XXSHRT
     C                   MOVEL     XXSHRT        XXLAST
     C                   MOVEL     AGPST         XXSTZP
     C                   MOVE      AGPZIP        XXSTZP
     C                   ENDIF
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C     AGMAD1        IFNE      *BLANKS
     C                   MOVEL     AGMZIP        XXST10           10
     C                   ELSE
     C                   MOVEL     AGPZIP        XXST10           10
     C                   ENDIF
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   ENDIF
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        ACSZ             34
     C                   ELSE
     C                   MOVE      *BLANKS       ANAME
     C                   MOVE      *BLANKS       AADD1
     C                   MOVE      *BLANKS       ACSZ
     C                   MOVE      *ZEROS        AGARE1
     C                   MOVE      *ZEROS        AGPHN1
     C                   END
     C*
     C                   ELSE
     C                   MOVE      *BLANK        AHAGT#
     C                   MOVE      *BLANK        ANAME
     C                   MOVE      *BLANKS       AADD1
     C                   MOVE      *BLANKS       ACSZ
     C                   MOVE      *ZEROS        AGARE1
     C                   MOVE      *ZEROS        AGPHN1
     C                   ENDIF                                                  *IN50 IFEQ
     C*
     C                   MOVE      *BLANKS       XQAPHN
     C     AGPHN1        IFEQ      *ZEROS
     C                   MOVE      *OFF          *IN69
     C                   ELSE
     C                   MOVE      *ON           *IN69
     C                   MOVEL     '('           XQAPHN
     C                   MOVE      AGARE1        XXAREA            3
     C     XQAPHN        CAT       XXAREA:0      XQAPHN
     C     XQAPHN        CAT       ')':0         XQAPHN
     C     XQAPHN        CAT       XXAPH1:1      XQAPHN
     C     XQAPHN        CAT       '-':0         XQAPHN
     C     XQAPHN        CAT       XXAPH2:0      XQAPHN
     C                   ENDIF
     C*
     C                   MOVEL     XXPROD        X1PRNM
     C*
     C     ANAME         IFNE      *BLANKS
     C                   MOVEL     AGNAME        XQAGNM           45
     C                   ELSE
     C                   MOVE      *BLANKS       XQAGNM
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XQCONM           40
     C                   MOVE      *BLANKS       XQCOM1           50
     C                   MOVE      *BLANKS       XQCOM2           50
     C                   MOVE      *BLANKS       XQCOM3           50
     C                   MOVE      *BLANKS       XQCOM4           50
     C                   Z-ADD     1             A                 2 0
     C                   MOVE      *BLANKS       COM
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  OFAAD1        COM(A)
     C                   ADD       1             A
     C*
     C     OFAAD2        IFNE      *BLANKS
     C                   MOVEL(P)  OFAAD2        COM(A)
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   MOVEL(P)  OFACTY        XXMADR           50
     C     XXMADR        CAT       ',':0         XXMADR
     C     XXMADR        CAT       OFAST:1       XXMADR
     C     XXMADR        CAT       OFMZ1:1       XXMADR
     C     OFMZ2         IFNE      '0000'
     C     XXMADR        CAT       '-':0         XXMADR
     C     XXMADR        CAT       OFMZ2:0       XXMADR
     C                   ENDIF
     C                   MOVEL(P)  XXMADR        COM(A)
     C                   ADD       1             A
     C*
     C                   MOVEL     OFACNM        XQCONM
     C                   MOVEL     COM(1)        XQCOM1
     C                   MOVEL     COM(2)        XQCOM2
     C*
     C     COM(3)        IFNE      *BLANKS
     C                   MOVEL     COM(3)        XQCOM3
     C*
     C     OFAPHN        IFNE      *ZERO
     C                   MOVEL     '('           XQCOM4
     C                   MOVE      OFAARA        XXAREA            3
     C     XQCOM4        CAT       XXAREA:0      XQCOM4
     C     XQCOM4        CAT       ')':0         XQCOM4
     C     XQCOM4        CAT       OFAPH1:1      XQCOM4
     C     XQCOM4        CAT       '-':0         XQCOM4
     C     XQCOM4        CAT       OFAPH2:0      XQCOM4
     C                   ENDIF                                                  OFAPHN IFNE
     C*
     C                   ELSE
     C*
     C     OFAPHN        IFNE      *ZERO
     C                   MOVEL     '('           XQCOM3
     C                   MOVE      OFAARA        XXAREA            3
     C     XQCOM3        CAT       XXAREA:0      XQCOM3
     C     XQCOM3        CAT       ')':0         XQCOM3
     C     XQCOM3        CAT       OFAPH1:1      XQCOM3
     C     XQCOM3        CAT       '-':0         XQCOM3
     C     XQCOM3        CAT       OFAPH2:0      XQCOM3
     C                   ENDIF                                                  OFAPHN IFNE
     C*
     C                   ENDIF                                                  COM,3 IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   MOVEL(P)  TIT(2)        XXPAGH           15
     C*
     C     FNKY          CHAIN     PF201                              60
     C*
     C     XXCLT#        IFEQ      '025'
     C                   Z-ADD     5.36          STMHV
     C     'State'       CAT(P)    'ment #':0    XQSTMH
     C                   Z-ADD     EANINV        XQSTMD
     C                   ENDIF
       //*** Beg Add *** 03/25/21 *****************************************************
       *in76 = *off;
       str3rdVrb = *blanks;
       If (ICPolicyOpt(xxCo#:xxFnd:xxEmp#) = 'Y');
          If F2rpCd <> ' ';
             str3rdVrb = OF3rdVrb;
             *in76 = *on;
          Endif;
       Endif;
       //*** End Add *** 03/25/21 *****************************************************
     C*
     C* PRINT STATEMENT.
     C*
     C                   OPEN      W7226O
     C*
     C                   WRITE     QHEAD
     C                   WRITE     QPREM
     C                   WRITE     QFOOT
     C*
     C                   CLOSE     W7226O
     C*
     C* RESOLVE TO DARTS.
     C*
     C     QQRMCD        IFNE      *BLANKS
     C                   EXSR      DARTS
       //*** Beg Add *** 03/25/21 **************************************************************
       // Write to Image Cross Reference File WMIXP
       If IsModEnabled('Invoice Cloud') and IcGroupOpt(EAco#:EAfnd);
          Exsr SrImageXref;
       Endif;
       //*** End Add *** 03/25/21 **************************************************************
     C                   ENDIF
     C*
     C                   EXSR      NOTICE
     C*
     C                   ENDSR                                                  RETRO SUBR
     C*
     C**********************************************************************
     C*  CLAIM SUBROUTINE  -  READ INCURRED LOSSES FROM THE WMRHP FILE.
     C********************************************************************
     C     CLAIM         BEGSR
     C*
     C                   Z-ADD     0             XXCAMT           11 2
     C*
     C     RHKY          CHAIN     PRH01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     RHEFDT        IFLE      XXLDAT
     C*
     C     1             DO        10            X                 2 0
     C                   ADD       AR(X)         XXCAMT
     C                   ENDDO
     C*
     C                   ENDIF                                                  RHEFDT IFLE
     C*
     C     RHKY          READE     PRH01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXCAMT        IFGT      ELRETN
     C     ELRETN        ANDNE     *ZERO
     C                   Z-ADD     ELRETN        XXCAMT
     C                   ENDIF
     C*
     C                   ENDSR                                                  CLAIM SUBR
     C*
     C**********************************************************************
     C*  DARTS SUBROUTINE  -  CREATE DARTS RECORD
     C********************************************************************
     C     DARTS         BEGSR
     C*
     C                   MOVE      *BLANKS       QQFOLD           80
     C                   MOVE      *BLANKS       QQDCID
     C*
     C                   MOVE      'BL'          QQPRE             2
     C                   MOVE      XXFND         Q1FND             3
     C                   MOVE      'P'           QQRTCD
     C                   CALL      'SNXTDCID'
     C                   PARM                    QQFOLD
     C                   PARM                    QQDCID
     C                   PARM                    QQPRE
     C                   PARM                    QQCO#
     C                   PARM                    Q1FND
     C                   PARM                    QQRTCD            1
     C                   PARM      '00000'       QQSUB#            5
     C                   PARM      *BLANKS       QQPFLR           80
     C*
     C                   CALL      'W7226C1'
     C                   PARM                    QQDCID
     C                   PARM                    QQFOLD
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANK        QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXEMP#        QCEMP#
     C                   MOVE      XXDIV         QCDIV
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C*
     C                   MOVE      *BLANKS       Q#COMM
     C     BSLPGM        IFEQ      'R'
     C                   MOVEL(P)  TIT(8)        Q#COMM          256
     C                   ELSE
     C                   MOVEL(P)  TIT(3)        Q#COMM          256
     C                   ENDIF
     C     Q#COMM        CAT       UPUSNM:1      Q#COMM
     C     Q#COMM        CAT       ', on':0      Q#COMM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         DSDT2
     C*
     C     Q#COMM        CAT       DSMM2:1       Q#COMM
     C     Q#COMM        CAT       '/':0         Q#COMM
     C     Q#COMM        CAT       DSDD2:0       Q#COMM
     C     Q#COMM        CAT       '/':0         Q#COMM
     C     Q#COMM        CAT       DSYY2:0       Q#COMM
     C     Q#COMM        CAT       'for':1       Q#COMM
     C     Q#COMM        CAT       'policy':1    Q#COMM
     C     Q#COMM        CAT       'year':1      Q#COMM
     C*
     C     XXFYR         IFGT      99
     C                   MOVE      XXFYR         XBFYR             3
     C     Q#COMM        CAT       XBFYR:1       Q#COMM
     C                   ELSE                                                   FYR > 99
     C                   MOVE      XXFYR         XAFYR             2
     C     Q#COMM        CAT       XAFYR:1       Q#COMM
     C                   ENDIF                                                  FYR > 99
     C*
     C     Q#COMM        CAT       '.':0         Q#COMM
     C     BSLPGM        IFEQ      'R'
     C     Q#COMM        CAT       TIT(9):2      Q#COMM
     C                   ELSE
     C     Q#COMM        CAT       TIT(4):2      Q#COMM
     C                   ENDIF
     C     Q#COMM        CAT       '$':1         Q#COMM
     C                   Z-ADD     XXAMTT        XXWK90            9 0
     C*
     C     XXAMTT        IFNE      *ZERO
     C                   MOVE      XXWK90        XXWK9A            9
     C     '0'           CHECK     XXWK9A        X
     C                   SUBST(P)  XXWK9A:X      XXWK9A
     C                   ELSE                                                   ANPR <> 0
     C                   MOVEL(P)  '0'           XXWK9A
     C                   ENDIF                                                  ANPR <> 0
     C*
     C     Q#COMM        CAT       XXWK9A:0      Q#COMM
     C     Q#COMM        CAT       '.':0         Q#COMM
     C     Q#COMM        CAT       '00':0        Q#COMM
     C*
     C     *IN56         IFEQ      *ON
     C     Q#COMM        CAT       TIT(5):1      Q#COMM
     C                   ELSE
     C     Q#COMM        CAT       '.':0         Q#COMM
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  QQDCID        Q#DCID           12
     C                   MOVEL(P)  QQFOLD        Q#FOLD           80
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZERO         Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  *BLANK        Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVEL(P)  QQRMCD        Q#RMCD            5
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
     C                   ENDSR
     C*
     C**********************************************************************
     C*  NOTICE SUBROUTINE - PRINT A PREMIUM NOTICE
     C********************************************************************
     C     NOTICE        BEGSR
     C*
     C                   MOVE      XXFND         Q1FND             3
     C                   MOVE      XXEMP#        Q1EMP#            9
     C                   MOVE      XXFYR         Q1FYR             3
     C                   MOVE      XXDIV         Q1DIV             5
     C                   MOVE      *ZEROS        QQFRDT
     C                   MOVE      *ZEROS        QQTODT
     C                   MOVE      *ZEROS        QQFREN
     C                   MOVE      *ZEROS        QQTOEN
     C                   MOVE      *ZEROS        QQFRAN
     C                   MOVE      *ZEROS        QQTOAN
     C                   MOVE      QQOUTQ        QQQUE
     C                   Z-ADD     XXPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQDATE            8
     C                   MOVE      QQRMCD        QQDRMK
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     EXPNRM        ANDNE     *BLANKS
     C                   MOVEL     EXPNRM        QQDRMK
     C                   ENDIF
     C*
     C                   CALL      'W7321R'
     C                   PARM                    QQCO#
     C                   PARM                    Q1FND
     C                   PARM                    QQDATE
     C                   PARM                    QQDATE
     C                   PARM      '000'         QQADDU            3
     C                   PARM      'Y'           QQEXDF            1
     C                   PARM      '  '          QQCOM1            2
     C                   PARM      '  '          QQCOM2            2
     C                   PARM      '  '          QQCOM3            2
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM      '  '          QQSRT1            2
     C                   PARM      '  '          QQSRT2            2
     C                   PARM      '  '          QQFREA            2
     C                   PARM      '  '          QQTOEA            2
     C                   PARM      '  '          QQFRAA            2
     C                   PARM      '  '          QQTOAA            2
     C                   PARM                    QQFREN            9
     C                   PARM                    QQTOEN            9
     C                   PARM                    QQFRAN            5
     C                   PARM                    QQTOAN            5
     C                   PARM                    QQQUE            10
     C                   PARM      'D'           QQMODE            1
     C                   PARM                    QQDRMK            5
     C                   PARM      '000'         QQIDYS            3
     C                   PARM      'Y'           QQGRMA            1
     C                   PARM      '0000000'     QQMINB            7
     C                   PARM      ' '           QQDEP             1
     C                   PARM      'Y'           QQEXP             1
     C                   PARM      QQDART        QQPDRT            1
     C*
     C                   ENDSR
       //*** Beg Add *** 03/25/21 **************************************************************
       //===========================================================
       // Write to Image Cross Reference File WMIXP
       //===========================================================
       Begsr SrImageXref;
          IMco# = EAco#;
          IMfnd = EAfnd;
          IMEmp# = EAemp#;
          IMdiv  = EAdiv;
          IMinv# = EAinv#;
          IMtype = 'Retrospective Settlement';
          IMpoDt = EApoDt;
          IMUNCN = '/' + %trim(xxRoot) + '/' + %trim(Q#Fold) + '/' +
           %trim(Q#Dcid);
          IMseq# = %Dec(q#Seq#:5:0);
          IMstat = 'A';
          IMuser = Q1user;
          IMpgm  = Q1pgm;
          dtsysDate = %Date();
          IMdt   = %Dec(%Char(dtSysDate:*cymd0):7:0);
          IMtime = %Dec(%Time():*HMS);
          IMiUsr = IMuser;
          IMiPgm = IMPgm;
          IMiDt  = IMdt;
          IMiTim = IMtime;
          Write pim01;
       Endsr;
       //*** End Add *** 03/25/21 **************************************************************
     C*
     C*****************************************************************
     C*
     C* SUBROUTINE TO DETERMINE IF A DATE RANGE CONTAINS FEBRUARY 29TH.
     C*
     C     SRLEAP        BEGSR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     0             XXLPDT
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1FROM
     C*
     C                   Z-ADD     XQTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TO
     C*
     C                   Z-ADD     X1FROM        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      X1FROM
     C                   Z-ADD     X1TO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      X1FROM
     C     XXLPDT        ORGT      X1TO
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C*
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XQFROM
     C*
     C     SRERR         IFNE      '1'
     C     XXLPDT        ANDNE     X1FROM
     C                   SUB       1             QQDAYS
     C                   ELSE
     C                   Z-ADD     0             XXLPDT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  SRLEAP
     C*
     C*****************************************************************
     C*
     C*  SUBROUTINE FOR SPLITTING PAYROLL TOTAL FOR MODIFIERS.
     C*
     C     PREMA         BEGSR
     C*
     C                   Z-ADD     XTPAY         XXPAYA           11 2
     C*
     C     XXEFD         IFGT      ELPFDT
     C     XXEFD         ANDLT     ELPTDT
     C     XXEXD         ORGT      ELPFDT
     C     XXEXD         ANDLT     ELPTDT
     C*
     C*  GET NUMBER OF DAYS AFFECTED BY THE MODIFIER IN THE
     C*  PERIOD
     C*
     C     XXEFD         IFGT      ELPFDT
     C                   Z-ADD     XXEFD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   ENDIF                                                  XXEFD IFGT
     C*
     C     XXEXD         IFLT      ELPTDT
     C                   Z-ADD     XXEXD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C                   ELSE
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C                   ENDIF                                                  XXEXD IFLT
     C*
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      XQTO          QQTO
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   EXSR      SRLEAP
     C*
     C                   Z-ADD     QQDAYS        VDAYS             5 0
     C*
     C                   Z-ADD     *ZEROS        XXDPRO            5 4
     C*
     C     PDAYS         IFNE      *ZEROS
     C     VDAYS         DIV(H)    PDAYS         XXDPRO
     C                   ENDIF                                                  PDAYS IFNE
     C*
     C                   ELSE
     C*
     C     XXEFD         IFLE      ELPFDT
     C     XXEXD         ANDGE     ELPTDT
     C                   Z-ADD     1             XXDPRO
     C                   ELSE
     C*
     C     XXEFD         IFGE      ELPTDT
     C     XXEXD         ORLE      ELPFDT
     C                   Z-ADD     *ZEROS        XXDPRO
     C                   ENDIF                                                  XXEFD  >= ELPTDT
     C*
     C                   ENDIF                                                  XXEFD  <= ELPFDT
     C*
     C                   ENDIF                                                  XXEFD  > ELPFDT
     C*                                                    & XXEFD  < ELPTDT
     C*
     C*  PRORATE THE PAYROLL AMOUNT.
     C*
     C     XXPAYA        MULT(H)   XXDPRO        XXWK10
     C                   Z-ADD     XXWK10        XXPAYA
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*
     C*  SUBROUTINE FOR REVERSING CLAIMS REPLINISHMENT BILLINGS OFF A
     C*  SUMMARIZED DIVISION.
     C*
     C     REVAR         BEGSR
     C*
     C                   Z-ADD     0             XXATOT           11 2
     C*
     C* ACCUMULATE ANY CLAIMS REPLINISHMENT BILLINGS ON DIV, DELETE OFF ANY
     C* IN FUTURE.
     C*
     C     EAKY          CHAIN     PEA01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EAARTY        IFEQ      'B'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'C'
     C*
     C     EAPODT        IFLT      XXPODT
     C                   ADD       EAAMT         XXATOT
     C                   ELSE
     C                   DELETE    PEA01
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C     EAKY          READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* WRITE AN REVERSING A/R RECORD FOR THE REMAINING BILLED AMOUNT.
     C*
     C     XXATOT        IFNE      *ZERO
     C     QQRPRN        ANDNE     'Y'
     C*
     C                   CLEAR                   PEA01
     C*
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   Z-ADD     XXEMP#        EAEMP#
     C                   Z-ADD     XXDIV         EADIV
     C                   MOVE      XXPODT        EAPODT
     C                   MOVE      XXTRDT        EATRDT
     C                   Z-SUB     XXATOT        EAAMT
     C*
     C                   MOVE      QQBAR         EAARCD
     C                   MOVEL     XXDESC        EADESC
     C                   MOVE      XXARTY        EAARTY
     C                   MOVE      XXCTYP        EACTYP
     C*
     C                   MOVE      'A'           EASTAT
     C                   MOVE      ' '           EAEDIT
     C                   Z-ADD     0             EABUL1
     C                   Z-ADD     0             EACHK#
     C                   Z-ADD     0             EANINV
     C                   MOVE      *BLANK        EALATE
     C*
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   EXSR      GETINV
     C                   WRITE     PEA01
     C*
     C                   ENDIF                                                  XXATOT IFNE
     C*
     C                   ENDSR                                                  REVAR SUBR
     C*****************************************************************
     C*
     C*  RESERVE NEXT AVAILABLE BATCH NUMBER FOR A/R BATCH.
     C*
     C     SRBAT         BEGSR
     C*
     C*  FIND A VALID SYSTEM TABLE CODE
     C*
     C     XXCO#         SETLL     PSY01
     C     *IN60         DOUEQ     *ON
     C     XXCO#         READE     PSY01                                  60
     C     SYTYPE        IFEQ      'Y'
     C     SYSTAT        ANDNE     'I'
     C                   MOVEL     SYKEY         XXSYCD
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVE      *BLANKS       XXSTAT            1
     C     EAKEY         SETGT     PEA01B
     C                   READP     PEA01B                                 60
     C     *IN60         IFEQ      '0'
     C     EACO#         ANDEQ     XXCO#
     C     1             ADD       EABAT#        XXVAL1            9 0
     C                   END
     C*
     C                   MOVE      'T'           XXSTAT
     C     EAKEY         SETGT     PEA01B
     C                   READP     PEA01B                                 60
     C     *IN60         IFEQ      '0'
     C     EACO#         ANDEQ     XXCO#
     C     1             ADD       EABAT#        XXVAL2            9 0
     C                   END
     C*
     C     XXVAL1        IFGT      XXVAL2
     C                   Z-ADD     XXVAL1        XXBAT#
     C                   ELSE
     C                   Z-ADD     XXVAL2        XXBAT#
     C                   END
      *
     C     XXBAT#        IFEQ      *ZEROS
     C                   Z-ADD     1             XXBAT#
     C                   END
      *
      *  Write an A/R batch dummy record, to hold the bat#.
      *
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXBAT#        EABAT#
     C                   Z-ADD     0             EABATA
     C                   MOVE      *HIVAL        EAFND
     C                   MOVE      *HIVAL        EAFYR
     C                   MOVE      *HIVAL        EAEMP#
     C                   MOVE      *HIVAL        EADIV
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   Z-ADD     *ZEROS        EAPODT
     C                   Z-ADD     *ZEROS        EACHK#
     C                   Z-ADD     *ZEROS        EACKAM
     C                   Z-ADD     *ZEROS        EAAMT
     C                   MOVE      *BLANKS       EASYCD
     C                   MOVE      'N'           EATFAP
     C                   MOVE      'N'           EATFFL
     C                   Z-ADD     *ZEROS        EATFDT
     C                   Z-ADD     *ZEROS        EATFAM
     C                   MOVE      *BLANKS       EASTAT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   Z-ADD     DSTIME        EATIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVEL     SRDTO         EAIDT
     C                   MOVE      Q1USER        EAIUSR
     C                   MOVE      Q1PGM         EAIPGM
     C                   Z-ADD     *ZEROS        EAINV#
     C                   Z-ADD     *ZEROS        EATRDT
     C                   MOVE      *BLANKS       EALATE
      *
     C                   EXSR      GETINV
     C                   WRITE     PEA02B
     C*
     C                   ENDSR                                                  SRBAT SUBR
     C*****************************************************************
     C*****************************************************************
     C*  PREMIUM CALCULATION SUBROUTINE                               *
     C*****************************************************************
     C     PRMC          BEGSR
     C*
     C                   MOVE      *ZEROS        XXAMT            11 0
     C* ACCUMULATE PAYROLL/PREMIUM TOTALS.
     C*
     C                   Z-ADD     0             XXPAY            11 2
     C                   Z-ADD     0             XXPAYF           11 2
     C                   Z-ADD     0             XXFACT            5 4
     C                   Z-ADD     0             XXPAY1           11 2
     C                   Z-ADD     0             XXPAY2           11 2
     C                   Z-ADD     0             XXPAY3           11 2
     C     F1KY          CHAIN     PF101                              60
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVE      *ON           *IN62
     C                   ELSE
     C                   MOVE      *OFF          *IN62
     C                   ENDIF
     C*
     C     EPKY          CHAIN     PEP01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
     C*
     C  N62              ADD       EPEMPR        XXPAY
     C   62              ADD       EPAMPR        XXPAY
     C*
     C     EPSPEC        IFEQ      'F'
     C  N62              ADD       EPEMPR        XXPAYF
     C   62              ADD       EPAMPR        XXPAYF
     C                   ENDIF
     C*
     C* ACCUMULATE PREMIUM BY MODIFIER IF SPLIT FOR AUDITS
     C*
     C     F1SPLA        IFEQ      'Y'
     C     *IN62         ANDEQ     *ON
     C*
     C                   SELECT
     C     EPEFDT        WHENGE    ELEMD3
     C     ELEMD3        ANDNE     *ZERO
     C     ELEXM3        ANDNE     *ZERO
     C                   ADD       EPAMPR        XXPAY3
     C     EPEFDT        WHENGE    ELEMD2
     C     ELEMD2        ANDNE     *ZERO
     C     ELEXM2        ANDNE     *ZERO
     C                   ADD       EPAMPR        XXPAY2
     C                   OTHER
     C                   ADD       EPAMPR        XXPAY1
     C                   ENDSL
     C*
     C                   ENDIF                                                  F1SPLA IFEQ
     C*
     C                   ENDIF                                                  EPSTAT IFNE
     C*
     C     EPKY          READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXPAY         IFNE      *ZERO
     C     XXPAYF        DIV(H)    XXPAY         XXFACT
     C                   ENDIF
     C*
     C* DETERMINE IF F-CLASS PAYROLLS/RATES NEED TO BE DISPLAYED.
     C*
     C                   MOVE      *OFF          *IN63
     C                   Z-ADD     ELTAXF        XXTAXF
     C                   Z-ADD     ELELPF        XXELPF
     C*
     C     XXFACT        IFNE      *ZERO
     C*
     C     ELTAXF        IFNE      *ZERO
     C     ELTAXF        ANDNE     ELTAX
     C     ELELPF        ORNE      *ZERO
     C     ELELPF        ANDNE     ELELP
     C                   MOVE      *ON           *IN63
     C                   ENDIF                                                  ELTAXF IFNE
     C*
     C     *IN63         IFEQ      *ON
     C*
     C     ELTAXF        IFEQ      *ZERO
     C                   Z-ADD     ELTAX         XXTAXF            5 4
     C                   ENDIF
     C*
     C     ELELPF        IFEQ      *ZERO
     C                   Z-ADD     ELELP         XXELPF            5 4
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN63 IFEQ
     C*
     C                   ENDIF                                                  XXFACT IFNE
     C*
     C                   SELECT
     C*When BSLPGM = 'R' Then calculate Premium for Retention Plan
     C     BSLPGM        WHENEQ    'R'
     C     ELFAF         IFEQ      'A'
     C     ELASPR        SUB(H)    ELADC3        XXPREM
     C                   ELSE
     C*
     C     ELEMNP        IFNE      'Y'
     C     ELSTPR        SUB       ELEDC3        XXPREM
     C                   MULT(H)   ELPROR        XXPREM
     C                   ELSE
     C                   Z-ADD(H)  ELSTPR        XXPREM
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELFAF IFEQ
     C*When BSLPGM <> 'R' then Existing premium Calculation
     C     BSLPGM        WHENNE    'R'
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD(H)  ELASPR        XXPREM           11 0
     C                   ELSE
     C*
     C     ELEMNP        IFNE      'Y'
     C     ELPROR        MULT(H)   ELSTPR        XXPREM
     C                   ELSE
     C                   Z-ADD(H)  ELSTPR        XXPREM
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELFAF IFEQ
     C*
     C                   ENDSL
     C*
     C* CALCULATE MINIMUM, MAXIMUM, AND BASIC PREMIUM AMOUNTS.
     C*
     C     ELBSFC        MULT(H)   XXPREM        XXBPRM           11 0
     C     XXFACT        MULT(H)   XXPREM        XXFPRM           11 0
     C     XXPREM        SUB       XXFPRM        XXPRM            11 0
     C     ELBSFC        MULT(H)   XXFPRM        XXFBPR           11 0
     C     XXBPRM        SUB       XXFBPR        XXBPM            11 0
     C*
     C* CALCULATE EXCESS LOSS PREMIUM VALUES.
     C*
     C     ELELP         MULT(H)   XXPRM         XAELP            11 0
     C     ELELPF        MULT(H)   XXFPRM        XAELPF           11 0
     C*
     C     ELLCVF        MULT(H)   XAELP         XCELP            11 0
     C     ELLCVF        MULT(H)   XAELPF        XCELPF           11 0
     C*
     C* CALCULATE RETRO DEVELOPMENT PREMIUM VALUES.
     C*
     C                   SELECT
     C     XXSEQ#        WHENEQ    1
     C                   Z-ADD     ELRDF1        XXRDF
     C     XXSEQ#        WHENEQ    2
     C                   Z-ADD     ELRDF2        XXRDF
     C     XXSEQ#        WHENEQ    3
     C                   Z-ADD     ELRDF3        XXRDF
     C                   OTHER
     C                   Z-ADD     0             XXRDF
     C                   ENDSL
     C*
     C     XXRDF         MULT(H)   XXPRM         XARDP            11 0
     C     XXRDF         MULT(H)   XXFPRM        XARDPF           11 0
     C*
     C     ELLCVF        MULT(H)   XARDP         XCRDP            11 0
     C     ELLCVF        MULT(H)   XARDPF        XCRDPF           11 0
     C*
     C* TOTAL ALL INCURRED LOSSES AND CHECK IF ALL CLAIMS ARE CLOSED.
     C*
     C                   MOVE      'Y'           XXFSET            1
     C                   Z-ADD     0             XXLOSS           11 0
     C                   Z-ADD     0             XXLOSF           11 0
     C                   Z-ADD     0             XOLOSS           11 2
     C                   Z-ADD     0             XCLOSS           11 2
     C                   Z-ADD     0             XOLOSF           11 2
     C                   Z-ADD     0             XCLOSF           11 2
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     CMSTAT        IFNE      'C'
     C*
     C                   EXSR      CLAIM
     C                   ADD       XXCAMT        XOLOSS
     C*
     C     CMSPEC        IFEQ      'F'
     C                   EXSR      CLAIM
     C                   ADD       XXCAMT        XOLOSF
     C                   ENDIF                                                  CMSPEC IFEQ
     C*
     C                   ELSE
     C*
     C                   EXSR      CLAIM
     C                   ADD       XXCAMT        XCLOSS
     C*
     C     CMSPEC        IFEQ      'F'
     C                   EXSR      CLAIM
     C                   ADD       XXCAMT        XCLOSF
     C                   ENDIF                                                  CMSPEC IFEQ
     C*
     C                   ENDIF                                                  CMSTAT IFNE
     C*
     C     CMSTAT        IFNE      'C'
     C                   MOVE      *BLANK        XXFSET
     C                   ENDIF
     C*
     C     CMKY          READE     PCM01                                  60
     C                   ENDDO
     C*
     C                   Z-ADD(H)  XOLOSS        XXWK10           11 0
     C                   Z-ADD     XXWK10        XOLOSS
     C                   Z-ADD(H)  XCLOSS        XXWK10           11 0
     C                   Z-ADD     XXWK10        XCLOSS
     C                   Z-ADD(H)  XOLOSF        XXWK10           11 0
     C                   Z-ADD     XXWK10        XOLOSF
     C                   Z-ADD(H)  XCLOSF        XXWK10           11 0
     C                   Z-ADD     XXWK10        XCLOSF
     C*
     C     XOLOSS        ADD       XCLOSS        XXLOSS
     C*
     C     XOLOSF        ADD       XCLOSF        XXLOSF
     C*
     C* CALCULATE CONVERTED LOSSES.
     C*
     C     ELLCVF        MULT(H)   XXLOSS        XXCLOS           11 0
     C     ELLCVF        MULT(H)   XXLOSF        XXCLOF           11 0
     C*
     C* TOTAL BASIC PREMIUM AND CONVERTED LOSSES.
     C*
     C     XXCLOS        ADD       XXBPRM        XXTOTL           11 0
     C     XXCLOF        ADD       XXFBPR        XXTOTF           11 0
     C*
     C* APPLY TAX MULTIPLIER.
     C*
     C                   SUB       XXTOTF        XXTOTL
     C                   Z-ADD     XXTOTF        XTTOTF
     C                   Z-ADD     XXTOTL        XTTOTL
     C*
     C                   ADD       XCELP         XTTOTL
     C                   ADD       XCRDP         XTTOTL
     C                   ADD       XCELPF        XTTOTF
     C                   ADD       XCRDPF        XTTOTF
     C*
     C                   ADD       XCELP         XXTOTL
     C                   ADD       XCRDP         XXTOTL
     C                   ADD       XCELPF        XXTOTF
     C                   ADD       XCRDPF        XXTOTF
     C*
     C                   MULT(H)   ELTAX         XXTOTL
     C                   MULT(H)   XXTAXF        XXTOTF
     C     XXTOTL        ADD       XXTOTF        XXAMT
     C                   Z-ADD     XXAMT         XSTOTL
     C*
     C                   ENDSR
     C*****************************************************************
     C*  GROUP PREMIUM CALCULATION SUBROUTINE                               *
     C*****************************************************************
     C     GRPP          BEGSR
     C*
     C                   MOVE      XXEMP#        @@EMP#
     C                   MOVE      XXDIV         @@DIV
     C                   Z-ADD     *ZEROS        XGAMT            13 2
     C                   Z-ADD     *ZEROS        XGPREM           13 2
     C                   Z-ADD     *ZEROS        XGDIFF           13 2
     C                   Z-ADD     *ZEROS        SVMXFC            5 4
     C                   Z-ADD     *ZEROS        SVMNFC            5 4
     C                   CLEAR                   EMP
     C                   CLEAR                   STD
     C                   CLEAR                   PRM
     C                   Z-ADD     *ZEROS        B                 5 0
     C*
     C     ELKY4         CHAIN     PEL03                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C*
     C                   MOVE      *OFF          XXRTRO            1
     C     *IN60         IFEQ      *OFF
     C     BSLPGM        IFEQ      XXBSPT
     C     XXBSPT        OREQ      'B'
     C     BSLPGM        OREQ      'P'
     C     XXCLT#        ANDNE     '114'
     C     BSLPGM        OREQ      'I'
     C     XXCLT#        ANDNE     '114'
     C                   MOVE      *ON           XXRTRO
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRTRO        IFEQ      *ON
     C     ELPFDT        ANDNE     ELPTDT
     C*
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   MOVEL     ELDIV         XXDIV
     C                   EXSR      PRMC
     C                   ADD       XXAMT         XGAMT
     C                   ADD       XXPREM        XGPREM
     C*
     C     SVMXFC        IFEQ      0
     C                   MOVEL     ELMXFC        SVMXFC
     C                   ENDIF                                                  SVMXFC    IFEQ 0
     C*
     C     SVMNFC        IFEQ      0
     C                   MOVEL     ELMNFC        SVMNFC
     C                   ENDIF                                                  SVMNFC    IFEQ 0
     C*
     C                   Z-ADD     1             C                 5 0
     C*
     C                   MOVEL     XXEMP#        XXEMPD           14 0
     C                   MOVE      XXDIV         XXEMPD
     C     XXEMPD        LOOKUP    EMP(C)                                 60
     C     *IN60         IFEQ      '0'
     C                   ADD       1             B
     C                   Z-ADD     XXEMPD        EMP(B)
     C                   Z-ADD     XXPREM        STD(B)
     C*
     C                   ELSE
     C*
     C                   ADD       XXPREM        STD(C)
     C*
     C                   ENDIF                                                  *IN60 = '0'
     C*
     C                   ENDIF                                                  *IN60 = 0
     C*
     C     ELKY4         READE     PEL03                                  60
     C*
     C                   ENDDO                                                  *IN60= 0
     C*
     C*
     C     SVMXFC        MULT(H)   XGPREM        XXMAX
     C     SVMNFC        MULT(H)   XGPREM        XXMIN
     C*
     C                   Z-ADD     0             XXDIFF
     C                   Z-ADD     XGAMT         XGTOTL
     C                   Z-ADD     XGAMT         XGAMT2           13 2
     C*
     C     XGAMT         IFLT      XXMIN
     C                   Z-ADD     XXMIN         XGAMT2
     C                   ENDIF                                                  XGAMT < XXMIN
     C*
     C     XGAMT         IFGT      XXMAX
     C                   Z-ADD     XXMAX         XGAMT2
     C                   ENDIF                                                  XGAMT > XXMAX
     C*
     C     1             DO        9999          C
     C*
     C     EMP(C)        IFNE      0
     C     STD(C)        DIV(H)    XGPREM        X1FACT            7 6
     C     X1FACT        MULT(H)   XGAMT2        XXWK10
     C                   ADD       XXWK10        PRM(C)
     C                   ENDIF                                                  EMP,C <> 0
     C*
     C                   ENDDO
     C*
     C                   XFOOT     PRM           XXTEST           13 2
     C*
     C*DIFFERENCE BETWEEN THE TOTAL PREMIUM CURRENTLY BILLED OUT TO POLICIES TO
     C*MEET MAX OR MIN AND THE ACTUAL MAX OR MIN.
     C*
     C     XGAMT2        SUB       XXTEST        XGDIFF
     C*
     C     XGDIFF        IFNE      *ZERO
     C*
     C     XGDIFF        DIV(H)    B             SVDIFF            5 0
     C*
     C     SVDIFF        IFEQ      0
     C     XGDIFF        IFLT      0
     C                   SUB       1             SVDIFF
     C                   ELSE
     C                   ADD       1             SVDIFF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXDIFF <> 0
     C*
     C                   MOVE      @@EMP#        XXEMP#
     C                   MOVE      @@DIV         XXDIV
     C                   ENDSR                                                  GRPP
     C**************************************************************************
**
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
**
RETROSPECTIVE RATING SETTLEMENT LETTER
Page 1 of 1
A retrospective settlement letter was requested by
The retrospective premium was
and was printed as a final settlement.
Calculation of Retrospective Premium:
RETENTION PLAN SETTLEMENT LETTER
A retention settlement letter was requested by
The retention premium was
Calculation of Retention Premium:
** MD - NUMBER OF DAYS PER MONTH
312831303130313130313031
**
First
Second
Third
Fourth
Fifth
Sixth
Seventh
Eighth
Ninth
Tenth
Eleventh
Twelfth
Thirteenth
Fourteenth
Fifteenth
Sixteenth
Seventeenth
Eighteenth
Nineteenth
Twentieth
