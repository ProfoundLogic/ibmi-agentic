     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7191R - Employer's Experience Report - Output Workfile
     H*****************************************************************
     H*    PROGRAM DESCRIPTION:                                       *
     H*      BUILDS THE WORKFILE FOR EMPLOYER'S EXPERIENCE REPORT,    *
     H*      USING SELECTION PARMS PASSED FROM W7190R.                *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      W7191C                                                   *
     H*    CALLS TO:                                                  *
     H*      NONE                                                     *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/11/21 C:180 Ticket: 94545                                  *
     H*                Clear out SIC desc if not SIC code             *
     H*                                                               *
     H* 07/27/18 C:062 Ticket: 70529(065)                             *
     H*                Exclude policies without claims from work file.*
     H*                                                               *
     H* 05/31/18 C:062 Ticket: 70529(065)                             *
     H*                Remove items where EEsort = *HIVAL             *
     H*                                                               *
     H* 06/29/17 C:062 Ticket: 49943(114)                             *
     H*                Use claim jurisdiction state.                  *
     H*                                                               *
     H* 11/22/13 C:190 Project: 129WSB                                *
     H*                Modify Claim skip condition.                   *
     H*                                                               *
     H* 08/31/12 C:282 PROJECT: 139WQS                                *
     H*                Processing to retrieve the Carrier Policy      *
     H*                Number                                         *
     H*                                                               *
     H* 05/19/10 C:249 PROJECT: SEQ7222                               *
     H*                Do no add to totals when required loss time
     H*                is not selected.
     H*
     H* 04/20/09 C:190 PROJECT: WC1473                                *
     H*                 Use new logical(s) to get the description by  *
     H*                 extension.
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant info     *
     H*                                                               *
     H* 03/01/07 C:61  PROJECT 132WHJ                                 *
     H*                Only load last 4 digits of SS# if prompt       *
     H*                select is '4'.                                 *
     H*                                                               *
     H*  11/13/06 C:15  Project: 71WHI  (Seq# 11)                     *
     H*                 Do not load the SS# if prompt select = 'N'.   *
     H*                                                               *
     H*  09/20/06  C:58  Project: 71WHI
     H*            Add parms Lost time flag, Total incurred and Gt/Lt
     H*
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*
     H*  02/18/06 C:132 PROJECT: WC1221                               *
     H*                - Add new fields for Recoveries - Current      *
     H*                  period and To-Date fields                    *
     H*                - Calculate new recovery amounts for both      *
     H*                  current period and To-Date.                  *
     H*                - Update new fields in output file             *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 11/14/01 C:15  Support: WC094/29732                           *
     H*                Changed the calc for 'this period' amounts to  *
     H*                prevent entries entered after the 'as of date' *
     H*                from being included.                           *
     H*                                                               *
     H* 04/08/01 C:61  PROJECT WC577                                  *
     H*                Add Rating State and Rate Set Code to work     *
     H*                file.                                          *
     H*                                                               *
     H*  08/29/00  C:61  Project: WC577                               *
     H*                  Correct value loaded to Shift Description    *
     H*                  WXEEP/EESHDS.                                *
     H*                                                               *
     H*  08/21/00  C:53  Project: WC577                               *
     H*                  Correct CMSIC code comparison value          *
     H*                                                               *
     H*  05/05/00  C:15  Support: WC096/22751                         *
     H*                  Chgd pgm to use the reserve history eff dt   *
     H*                  instead of the maint date to determine when  *
     H*                  the record was written.                      *
     H*                                                               *
     H*  01/13/00  C:33  SEQ#: WC033/21402                            *
     H*                  CLEAR THE WORKFILE FOR EMPLOYERS THAT DO NOT *
     H*                  HAVE ANY CLAIMS                              *
     H*                                                               *
     H* 10/06/99 C:90 PROJECT: WC945                                  *
     H*                FIND VALID EXTENSION WITH STATE EQUAL TO BLANK *
     H*                IF NOT VALID, THEN FIND VALID EXT WITH STATE   *
     H*                EQUAL TO THE RATING STATE.                     *
     H*                                                               *
     H*  05/27/99  C:33  SEQ#: WC999/18136                            *
     H*                  LOAD THE CORRECT EMPLOYER NAME TO THE WORK-  *
     H*                  FILE EVEN IF THERE ARE NO CLAIM FOR THE EMPLR*
     H*                                                               *
     H*  09/22/98  C:33  SEQ#: WC071/13656                            *
     H*                  USE SRDAY8 INSTEAD OF SRDAY                  *
     H*                                                               *
     H*  08/11/98  C:65  PROJECT: WC843                               *
     H*                  DISPLAY EXTENSION DESCRIPTION IF NOT BLANK.  *
     H*                                                               *
     H*  05/29/98 C:33 SEQ#: WC078/11786                              *
     H*                CONSIDER QQDIV AS ALPHA                        *
     H*                                                               *
     H*  05/14/98 C:33 SUPPORT                                        *
     H*               -CALCULATE AGE BASED ON ACCIDENT DATE           *
     H*               -CLEAR EEDTOB                                   *
     H*                                                               *
     H*  11/03/97 C:58 PROJECT 60APJ                                  *
     H*                V3 VERSION
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*     11   -      CONDITION FIRST TIME THROUGH PROGRAM          *
     H*     31   -      CONDITION FOR COMPENSATION                    *
     H*     32   -      CONDITION FOR MEDICAL                         *
     H*     33   -      CONDITION FOR LEGAL                           *
     H*     34   -      CONDITION FOR OTHER                           *
     H*     60   -      GENERAL CHAINING                              *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*     WDPAL3 (PPA02) CO#/FND/FYR/CASE                           *
     H*     WMCML5 (PCM01) CO#/FND/FYR/EMP#/DIV/CASE                  *
     H*     WMPRL  (PPR01) CO#/CLASS/EFDT/     OMIT STAT=I            *
     H*     WDELL1 (PEL01) CO#/FND/FYR/EMP#/DIV                       *
     H*     WMAHL  (PAH01) CO#/FND/EMP#/DIV/FYR/EFDT/AGT#             *
     H*     WMCDL  (PCD01) CDSS#                                      *
     H*     WTABL  (PRT01) CO#/KEY                                    *
     H*     WMCHL  (PCH01) CO#/FND/FYR/CASE/EFDT/TIME                 *
     H*     WMRHL1 (PRH01) CO#/FND/FYR/CASE                           *
     H**    WMEML (PEM01): CO#/FND/EMP#/DIV                          **
     H**    SMCOL (PCO01): CO#                                       **
     H*     SMORL  (POR01) ORPNAM,ORFIL#                              *
     H**    WMFNL (PFN01): CO#/FND                                   **
     H**    WDF1L (PF101): CO#/FND/FYR                               **
     H**    WMOFL (POF01): CO#/FND/FYR/EMP#/DIV/OFF#                 **
     H*****************************************************************
     H*
     FWMCML5    IF   E           K DISK
     F* * * Claims File
     F*
     FWMCDL     IF   E           K DISK
     F* * * Claimant Demographics File
     F*
     FWMFNL     IF   E           K DISK
     F* * * Fund Master File
     F*
     FWDF1L     IF   E           K DISK
     F* * * Fund Detail File
     F*
     FWMCHL     IF   E           K DISK
     F* * * Claim History File
     F*
     FWMPRL1    IF   E           K DISK
     F* * * Payroll Class File
     F*
     FWDPAL3    IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     F* * * Claims Payment Detail File
     F*
     FWMRHL1    IF   E           K DISK
     F* * * Reserve History File
     F*
     FWMEML     IF   E           K DISK
     F* * * Employer File
     F*
     FSMORL     IF   E           K DISK
     F* * * Batch Routing File
     F*
     FSMCOL     IF   E           K DISK
     F* * * Company File
     F*
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PVS01)
     F* * * Table File
     F*
     FWMDPL     IF   E           K DISK
     F* * * Department File
     F*
     FWMREL     IF   E           K DISK
     F* * * Region Master File
     F*
     FWMALL     IF   E           K DISK
     F* * * Address Location File
     F*
     F*
     F*** Begin Chg *** 05/31/18 *************************************
     F*** WXEEL     O    E             DISK
     FWXEEL     uf a E           k DISK
     F*** End   Chg *** 05/31/18 *************************************
     F* * * Work File Output
     F*
     FWDELL1    IF   E           K DISK
     F* * * Employer Detail File
     F*
     FWMAHL     IF   E           K DISK
     F* * * AGENT HISTORY File
     F*
     FWTGTL     IF   E           K DISK
     F* * * OFFICE MASTER FILE
     F*
     FWMOFL     IF   E           K DISK
     F* * * Generic Table File
     FWMPXL1    IF   E           K DISK
     FWMEPL7E   IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F                                     RENAME(PRS01:PRS02)
     F*
      ***  BEG ADD  ***  08/31/12  *****************************************
     Fwmrpl2    if   e           k disk
      ***  END ADD  ***  08/31/12  *****************************************

     D*
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
     D                 DS
     D  RHS                    1     90  2
     D                                     DIM(10)                              CHGS TO RESERVE
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
     D  RHR                    1     20
     D                                     DIM(10)                              RESERVE TYPES
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D**
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D XXATTX          C                   CONST('*** NO AUTHORITY ***')

     C*****************************************************************
     C*         *LIKE DEFINES
     C*****************************************************************
     C*
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMFYR         XXFYR3
     C     *LIKE         DEFINE    CMEMP#        XQEMP#
     C     *LIKE         DEFINE    CMDIV         XQDIV
     C     *LIKE         DEFINE    CMEMP#        XXEMPR
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    PRCLAS        XXCLAS
     C     *LIKE         DEFINE    PREFDT        XXEFDT
     C     *LIKE         DEFINE    CMCTG1        XXCTGY
     C     *LIKE         DEFINE    CMCTG1        XXCTG1
     C     *LIKE         DEFINE    CMDEPT        XXDEPT
     C     *LIKE         DEFINE    PAKEY         XXKEY                          Table Key
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    CMSIC         XXSIC
     C     *LIKE         DEFINE    PXST          XXST
     C     *LIKE         DEFINE    EEJRST        SVJRST
     C     *LIKE         DEFINE    EERTCD        SVRTCD
     C     *LIKE         DEFINE    EMMZIP        XXZIP
     C     *LIKE         DEFINE    XXTINC        XXITOT
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXEMPR
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXFYR3
     C*
     C     ELKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXFYR3
     C                   KFLD                    XQEMP#
     C                   KFLD                    XQDIV
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXFYR3
     C                   KFLD                    XQEMP#
     C*
     C*   Key List for the Region Master File
     C*
     C     REKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    EMRGN#
     C*
     C*   Key List for Generic Table File
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*   Key List for the Entity/location File
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXCTG1
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXFYR3
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*   KEY LIST FOR THE HISTORY FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXFYR3
     C                   KFLD                    XXCASE
     C                   KFLD                    XXYMD             7 0
     C*
     C*   KEY LIST FOR FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C     F1KY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXFYR3
     C*
     C*    Key List for Department Master File
     C*
     C     DPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXDEPT
     C*
     C*   KEY LIST FOR PAYROLL CLASS FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEFDT
     C*
     C*   KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCTGY
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE CLAIMS PAYMENT FILE
     C*
     C     P2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXFYR3
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE RESERVE HISTORY FILE
     C*
     C     RHKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*   KEY LIST FOR THE AGENT HISTORY MASTER FILE
     C*
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXEMPR
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR3
     C                   KFLD                    XXYMD
     C*
     C*   KEY LIST FOR THE
     C*
     C     OFKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMOFF#
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C                   KFLD                    EPEFDT
     C     PXKY1         KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
      *
      * RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      * PAYROLL FILE
      *
     C     EPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMCLAS
     C                   KFLD                    CMSPEC
     C                   KFLD                    CMEXT
      ***  BEG ADD  ***  08/31/12  *****************************************
      *   Key List for the Carrier/State Reporting Master File
      *
     c     keyWMRPL2     Klist
     c                   Kfld                    XXco#
     c                   Kfld                    XXfnd
     c                   Kfld                    XQemp#
     c                   Kfld                    XQdiv
     c                   Kfld                    XXfyr
      ***  END ADD  ***  08/31/12  ****************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDTE             8
     C                   PARM                    QQFYR             3
     C                   PARM                    QQSELO            1
     C                   PARM                    QQSELC            1
     C                   PARM                    QQMNTH            3
     C                   PARM                    QQSORT            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQAGT#            5
     C                   PARM                    QQSAGT            1
     C*** Begin Add *** 07/27/18 ****************************************
     C                   PARM                    QQEMPC            1
     C*** End   Add *** 07/27/18 ****************************************
     C                   PARM                    QQLTFL            1
     C                   PARM                    QQTINC           13
     C                   PARM                    QQGTLT            1
     C                   PARM                    QQPRSS            1
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
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDTE         XXMDY             8 0
     C                   Z-ADD     XXMDY         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C                   MOVEL     XXYMD         XXYYMM            5 0
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR3
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQSELO        XXSELO            1
     C                   MOVE      QQSELC        XXSELC            1
     C                   MOVE      QQSORT        XXSORT            1
     C                   MOVE      QQMNTH        MNTH              3 0
     C                   MOVE      QQEMP#        XQEMP#
     C                   MOVE      QQDIV         XQDIV
     C                   MOVE      *ZEROS        CSCNT             7 0
     C                   MOVE      QQAGT#        XXAGT#
     C                   MOVE      QQSAGT        XXSAGT            1
     C                   MOVE      QQTINC        XXTINC           13 2
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C  N11              EXSR      SR1ST
     C*
     C*  CHECK FOR A BLANK FUND
     C*
     C     XXFND         IFEQ      0
     C     XXCO#         CHAIN     PFN01                              60
     C                   ELSE
     C     FNKEY         CHAIN     PFN01                              60
     C                   END
     C     *IN60         DOWEQ     '0'
     C                   MOVE      FNFND         XXFND
     C*
     C*  GET THE FUND YEAR RECORD
     C*
     C     XXFYR         IFEQ      0
     C     F1KEY         CHAIN     PF101                              60
     C                   ELSE
     C     F1KY2         CHAIN     PF101                              60
     C                   END
     C     *IN60         DOWEQ     '0'
     C                   MOVE      F1FYR         XXFYR3
     C*
     C*  READ EMPLOYER DETAIL FILE AS PRIMARY
     C*
     C     XQEMP#        IFEQ      *ZERO
     C     ELKEY         CHAIN     PEL01                              60
     C                   ELSE
     C*
     C     QQDIV         IFEQ      *BLANK
     C     ELKY2         CHAIN     PEL01                              60
     C                   ELSE
     C*
     C     ELKEY1        CHAIN     PEL01                              60
     C                   ENDIF                                                  QQDIV=*BLANKS
     C                   END
     C*
     C     *IN60         DOWEQ     '0'
      *
     C                   MOVE      ELRTCD        SVRTCD
     C     RSKY          CHAIN     PRS02                              60
     C  N60              MOVE      RSJRST        SVJRST
     C   60              MOVE      *BLANKS       SVJRST
      *
     C*
     C                   Z-ADD     ELEMP#        XXEMPR
     C                   Z-ADD     ELDIV         XXDIV
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C*  READ CLAIM FILE
     C*
     C                   Z-ADD     *ZERO         XXCASE
     C     CMKEY1        CHAIN     PCM01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   MOVE      CMCASE        XXCASE
     C*
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   SELECT
      ***  beg chg  ***  11/22/13  *****************************************
     C**** QQLTFL        WHENEQ    'L'
     C*****CMLTFL        ANDNE     'I'
     C*****              GOTO      ENDA
     C*****QQLTFL        WHENEQ    'M'
     C*****CMLTFL        ANDNE     'M'
      /free
       When (qqLTFL = 'L' and CMltfl = 'I') or
            (qqLTFL = 'M' and CMltfl = 'M') or
            (qqLTFL = 'R' and CMltfl = 'R') or
            (qqLTFL = 'l' and CMltfl <> 'I') or
            (qqLTFL = 'm' and CMltfl <> 'M') or
            (qqLTFL = 'r' and CMltfl <> 'R');
      /end-free
      ***  end chg  ***  11/22/13  *****************************************
     C                   GOTO      ENDA
     C                   ENDSL
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *OFF          XXAUTH            1
     C                   ELSE
     C                   MOVE      *ON           XXAUTH
     C                   ENDIF                                                     RTCD = '2'
     C*
     C*  Clear output workfile
     C                   CLEAR                   PEE01
     C*
     C*  GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
     C*
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60
     C     *IN60         IFEQ      '1'
     C     CHCO#         ORNE      CMCO#
     C     CHFND         ORNE      CMFND
     C     CHFYR         ORNE      CMFYR
     C     CHCASE        ORNE      CMCASE
     C                   MOVE      CMSTAT        CHOCST
     C                   MOVE      CMENTD        CHEFDT
     C                   END
     C*
     C*    IF CLAIM IS CLOSED, OUTPUT 'C' TO WORK FILE
     C*    IF CLAIM IS OPEN, OUTPUT 'BLANK' TO WORK FILE
     C*
     C     CHOCST        IFEQ      'C'
     C                   MOVE      'C'           EEOPCL
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EECLDT
     C                   END
     C*
     C****************************************************************
     C*
     C*     CALL PGM TO CALCULATE DAYS SINCE CLAIM WAS CLOSED
     C*
     C     EECLDT        IFNE      *ZERO
     C                   Z-ADD     EECLDT        QQDOB             8 0
     C                   MOVE      QQDTE         QQMDY             8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQDOB
     C                   PARM                    QQMDY
     C                   PARM                    QQDAYS
     C                   PARM      ' '           QQERR             1
     C                   MOVE      QQDAYS        EEDAYS            5 0
     C     EEDAYS        DIV(H)    30            EEMNTH            5 2
     C*
     C                   ENDIF
     C*
     C*   INITIALLY WRITE ALL RECORDS TO WXEEL (FOR CALC. PURPOSES),
     C*   USE EESORT TO DISTINGUISH WHICH RECORDS SHOULD BE PRINTED
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EEACDT
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EERPDT
     C*
     C                   MOVE      XXCO#         EECO#
     C                   MOVE      CMFND         EEFND
     C                   MOVEL     FNNAME        EEFNAM
     C                   MOVE      CMEMP#        EEEMP#
     C                   MOVE      CMDIV         EEDIV
     C                   MOVEL     EMNAME        EEENAM
     C                   MOVE      EMRGN#        EERGN#
     C*
      *
      *  Get Shift Description
      *
     C                   MOVEL(P)  'SHFT'        XXTBID
     C                   MOVEL(P)  CMSHFT        XXKEY
     C     GTKY          CHAIN     PGT01                              61
      *
     C     *IN61         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        EESHDS
      *
     C                   ELSE
      *
     C                   MOVE(P)   *BLANKS       EESHDS
     C                   ENDIF
      *
     C                   MOVE      CMJRST        EEJRST
     C                   MOVE      CMOFF#        EEOFF#
     C     OFKEY         CHAIN     POF01                              60
     C                   MOVEL     OFNAME        EEOFDS
     C                   MOVE      CMECS#        EEECS#
     C                   MOVE      CMESPV        EEESPV
     C     CMSIC         IFEQ      *BLANKS                                      ADD 08/21/00
     C                   MOVE      EMSIC         EESIC
     C                   ELSE
     C                   MOVE      CMSIC         EESIC
     C                   ENDIF
     C*
      //*** Begin Add *** 03/11/21 *****************************************
                         If        EEsic <> *Blank;
      //*** End Add *** 03/11/21 *****************************************
     C                   MOVEL(P)  'SIC'         XXTBID
     C                   MOVEL(P)  EESIC         XXKEY
     C                   MOVEL     *BLANKS       EESICD
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        EESICD
      //*** Begin Add *** 03/11/21 *****************************************
                         Else;
                            EEsicd = *Blanks;
                         Endif;
      //*** End Add *** 03/11/21 *****************************************
     C*
     C*  Get region description
     C     REKY          CHAIN     PRE01                              60
     C     *IN60         IFEQ      *OFF
     C     RESTAT        ANDEQ     'A'
     C                   MOVEL(P)  REDESC        EERDES
     C                   END
     C                   MOVE      *BLANKS       EESS#
     C     Q1PRMS        IFGE      18
     C     QQPRSS        ANDEQ     'Y'
     C     XXAUTH        ANDEQ     *ON
     C                   MOVE      CMSS#         EESS#
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      18
     C     QQPRSS        ANDEQ     '4'
     C     XXAUTH        ANDEQ     *ON
     C                   MOVE      CMSS#         XXSS#4            4
     C                   MOVE      *BLANKS       EESS#
     C                   MOVE      XXSS#4        EESS#
     C                   ENDIF
      *
     C                   MOVE      CMFYR         EEFYR
     C                   Z-ADD     CMCASE        EECASE
     C                   MOVE      CMCLAS        EECLAS
     C                   MOVE      CMCTG1        EECTG1
     C*
     C*  Get location description
     C                   MOVEL     CMCTG1        XXCTG1
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         IFEQ      *OFF
     C     ALSTAT        ANDEQ     'A'
     C                   MOVEL     ALNAME        EELNAM
     C                   ENDIF                                                    60 = OFF
     C*
     C                   MOVE      CMCTG2        EECTG2
     C                   Z-ADD     CMACTM        EEACTM
     C*
     C                   Z-ADD     CMLOST        EELOST
     C                   Z-ADD     CMAWW         EEAWW
     C*
     C*   Date Received
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EERCVD
     C*
     C                   MOVEL     CMACST        EEACST
     C*
     C*  Get Activity Status Description
     C                   MOVE(P)   *BLANK        EEADES
     C                   MOVE(P)   *BLANK        XXKEY
     C                   MOVEL(P)  'ACST'        XXTBID
     C                   MOVEL(P)  CMACST        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        EEADES
     C                   ENDIF
     C*
     C                   MOVEL     CMOCDS        EEOCDS
     C                   MOVEL     EMSEMP        EERISK
     C                   MOVE      CMNATR        EENATR
     C                   MOVE      CMPART        EEPART
     C                   MOVE      CMCAUS        EECAUS
     C*
     C*  Get accident description codes
     C*  NATURE
     C                   MOVEL     *BLANK        XXKEY
     C                   MOVEL(P)  CMNATR        XXKEY
     C     TBKY          CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C     NASTAT        ANDEQ     'A'
     C                   MOVEL(P)  NADESC        EENADS
     C                   END
     C*  BODY PART
     C                   MOVEL     *BLANK        XXKEY
     C                   MOVEL(P)  CMPART        XXKEY
     C     TBKY          CHAIN     PPA01                              60
     C     *IN60         IFEQ      *OFF
     C     PASTAT        ANDEQ     'A'
     C                   MOVEL(P)  PADESC        EEPADS
     C                   END
     C*  CAUSE
     C                   MOVEL     *BLANK        XXKEY
     C                   MOVEL(P)  CMCAUS        XXKEY
     C     TBKY          CHAIN     PCA01                              60
     C     *IN60         IFEQ      *OFF
     C     CASTAT        ANDEQ     'A'
     C                   MOVEL(P)  CADESC        EECADS
     C                   END
     C*
     C                   MOVE      CMLTFL        EELSTM
     C                   MOVEL     CMACDS        EEACDS
     C                   MOVEL     CMACD2        EEACD2
     C                   MOVEL     CMACL#        EEACL#
     C                   MOVEL     CMACNM        EELNAM
     C*
     C*  Get department description
     C                   MOVEL(P)  CMDEPT        EEDEPT
     C                   MOVE      *BLANK        EEDDES
     C                   MOVEL(P)  CMDEPT        XXDEPT
     C     DPKY          CHAIN     PDP01                              60
     C     *IN60         IFEQ      *OFF
     C     DPSTAT        ANDEQ     'A'
     C                   MOVEL(P)  DPDESC        EEDDES
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMSHFT        EESHFT
     C**
     C*  GET CLAIMANT DEMOGRAPHICS FILE
     C*
     C                   MOVE      CMSS#         XXSS#
     C                   MOVE      *BLANKS       EECLMN
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*     CALL PGM TO RE-ARRANGE CLAIMANT NAME
     C*
     C                   MOVE      *BLANKS       QQLAST
     C                   MOVE      *BLANKS       QQFRST
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C* Check claim authority before moving claimant personal info
     C     XXAUTH        IFEQ      *ON
     C                   MOVEL     QQLAST        EECLMN
     C*
     C                   MOVE      CDADR1        EEADR1
     C                   MOVE      CDADR2        EEADR2
     C                   MOVE      CDCTY         EECTY
     C                   MOVE      CDST          EEST
     C                   Z-ADD     CDZIP         EEZIP
     C                   MOVEL(P)  CDCTRY        EECTRY
     C                   MOVEL(P)  CDADR3        EEADR3
     C*
     C                   Z-ADD     CDAREA        EEAREA
     C                   Z-ADD     CDPHN#        EEPHN#
     C*
     C                   Z-ADD     CDWARA        EEWARA
     C                   Z-ADD     CDWPH#        EEWPH#
     C                   ELSE
     C                   MOVEL     XXATTX        EECLMN
     C*
     C                   MOVE      *BLANKS       EEADR1
     C                   MOVE      *BLANKS       EEADR2
     C                   MOVE      *BLANKS       EECTY
     C                   MOVE      *BLANKS       EEST
     C                   Z-ADD     *ZEROS        EEZIP
     C*
     C                   MOVEL(P)  *BLANKS       EECTRY
     C                   MOVEL(P)  *BLANKS       EEADR3
     C*
     C                   Z-ADD     *ZEROS        EEAREA
     C                   Z-ADD     *ZEROS        EEPHN#
     C*
     C                   Z-ADD     *ZEROS        EEWARA
     C                   Z-ADD     *ZEROS        EEWPH#
     C                   ENDIF
     C                   MOVEL     CDSEX         EESEX
     C                   MOVEL     CDMSTS        EEMSTS
     C                   MOVEL     CDCNTY        EECNTY
     C*
     C*  Get County Description
     C     CDCNTY        IFNE      *BLANK
     C     CDST          ANDNE     *BLANK
     C                   MOVE      *BLANK        XXKEY
     C     CDST          CAT       CDCNTY:0      XXKEY
     C*
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *OFF
     C     CNSTAT        ANDEQ     'A'
     C                   MOVEL(P)  CNDESC        EECDES
     C                   ENDIF                                                  60 = OFF
     C                   ENDIF                                                  ST NE BLANK
     C*
     C                   MOVE      CDSEX         EESEX
     C*
     C****************************************************************
     C**
     C**    CALL PGM TO CALCULATE AGE
     C**
     C                   CLEAR                   EEDTOB
     C*
     C     CDDTOB        IFNE      *ZERO
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQDOB             8 0
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQMDY
     C*
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQDOB
     C                   PARM                    QQMDY
     C                   PARM                    QQDAYS
     C                   PARM      ' '           QQERR
     C                   MOVE      QQDAYS        EEDAYS            5 0
     C     EEDAYS        DIV       365.25        EEAGE
     C                   ELSE
     C                   Z-ADD     *ZERO         EEAGE
     C                   END
     C                   Z-ADD     QQDOB         EEDTOB
     C                   ENDIF
     C*
     C****************************************************************
     C**
     C**    CALL PGM TO CALCULATE DAY OF THE WEEK
     C**
     C     EEACDT        IFNE      *ZERO
     C                   Z-ADD     EEACDT        SRDATE            8 0
     C                   MOVE      *BLANK        SRSDAY            4
     C                   MOVE      *BLANK        SRLDAY            9
     C                   MOVE      ' '           QQERR             1
     C                   CALL      'SRDAY8'
     C                   PARM                    SRDATE                         M/D/Y
     C                   PARM                    SRSDAY                         Abbreviation
     C                   PARM                    SRLDAY                         Full Day Name
     C                   PARM                    QQERR                          Error Flag
     C                   MOVE      SRLDAY        EEDAYW
     C                   ELSE
     C                   MOVE      *BLANK        EEDAYW
     C                   END
     C**
     C****************************************************************
     C*
     C*  CHAIN TO EMPLOYER FILE TO GET REGION
     C*
     C                   MOVE      CMEMP#        XXEMPR
     C                   MOVE      CMDIV         XXDIV
     C     EMKEY         CHAIN     PEM01                              60
     C                   MOVE      EMRGN#        EERGN#
     C*
     C*  Get region description
     C     REKY          CHAIN     PRE01                              60
     C     *IN60         IFEQ      *OFF
     C     RESTAT        ANDEQ     'A'
     C                   MOVEL(P)  REDESC        EERDES
     C                   END
     C*
     C*   GET THE MOST CURRENT ACTIVE AGENT# FOR THAT FYR
     C*
     C     AHKEY         SETGT     PAH01
     C                   READP     PAH01                                  60
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C     AHCO#         ORNE      XXCO#
     C     AHFND         ORNE      FNFND
     C     AHEMP#        ORNE      XXEMPR
     C     AHDIV         ORNE      XXDIV
     C     AHFYR         ORNE      XXFYR3
     C                   Z-ADD     0             AHAGT#
     C                   END
     C     XXAGT#        IFNE      0
     C     AHAGT#        ANDNE     XXAGT#
     C                   GOTO      ENDA
     C                   END                                                    *
     C                   Z-ADD     AHAGT#        EEAGT#
     C*
     C*  GET MOST CURRENT PAYROLL CLASS RECORD FOR DESCRIPTION
     C*
     C                   MOVE      CMCLAS        XXCLAS
     C                   MOVE      XXYMD         XXEFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C     PRCO#         IFEQ      CMCO#
     C     PRCLAS        ANDEQ     CMCLAS
     C                   MOVEL     PRDESC        EECLDS
     C                   ELSE
     C                   MOVE      *BLANK        EECLDS
     C                   END
     C     EPKY          CHAIN     PEP01                              60
     C     *IN60         IFEQ      *OFF
     C     EPEXT         ANDNE     *BLANKS
     C                   MOVE      *BLANKS       XXST
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        EECLDS
     C   60RSKY          CHAIN     PRS02                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL(P)  PXDESC        EECLDS
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*  GET CLAIMS PAYMENT FILE FOR ACCUMULATING PAYMENTS THIS PERIOD
     C*
     C                   MOVE      '000'         *IN(31)
     C                   Z-ADD     *ZERO         EEPCMP
     C                   Z-ADD     *ZERO         EEPMED
     C                   Z-ADD     *ZERO         EEPLEG
     C                   Z-ADD     *ZERO         EEPOTH
      * ZERO OUT NEW WORK FIELDS
     C                   Z-ADD     *ZEROS        EEGCMP                         COMP GROSS PTD
     C                   Z-ADD     *ZEROS        EEGMED                         MED GROSS PTD
     C                   Z-ADD     *ZEROS        EEGLEG                         LEGAL GROSS PTD
     C                   Z-ADD     *ZEROS        EEGEXP                         OTHER GROSS PTD
     C                   Z-ADD     *ZEROS        EERTCO                         COMP RECOVER TD
     C                   Z-ADD     *ZEROS        EERTME                         MED  RECOVER TD
     C                   Z-ADD     *ZEROS        EERTLE                         LEGAL RECOVR TD
     C                   Z-ADD     *ZEROS        EERTEX                         OTHER RECOVR TD
      *
     C                   Z-ADD     *ZEROS        EERCCO                         COMP RECVR CURR
     C                   Z-ADD     *ZEROS        EERCME                         MED  RECVR CURR
     C                   Z-ADD     *ZEROS        EERCLE                         LEGAL RCVR CURR
     C                   Z-ADD     *ZEROS        EERCEX                         OTHER RCVR CURR
     C                   Z-ADD     *ZEROS        EEGPCO                         COMP GROSS CURR
     C                   Z-ADD     *ZEROS        EEGPME                         MED  GROSS CURR
     C                   Z-ADD     *ZEROS        EEGPLE                         LEGAL GROSS CUR
     C                   Z-ADD     *ZEROS        EEGPEX                         OTHER GROSS CUR
     C                   MOVEL     CMCASE        XXCASE
     C     P2KEY         CHAIN     PPA02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   SETOFF                                       313233
     C                   MOVEL     PATRDT        PAYYMM            5 0
     C*
     C* Check payment value 1 table for Recoveries Flay
     C* If flag = Y, then determine which category to report amt
     C*
     C                   MOVEL(P)  PABR2         XXKEY
     C*
     C     TBKY          CHAIN     PB201                              61
     C     *IN61         IFEQ      *OFF
     C     B2RCVR        ANDEQ     'Y'                                          RECOVERY FLAG
     C*
     C* Get the Total Recoveries to Date first
     C                   SELECT
     C* Compensation recovery
     C     PARETP        WHENEQ    'C'                                          COMP
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   ADD       PAAMT         EERTCO
     C                   END
     C     PATYPE        IFEQ      'A'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   SUB       PAAMT         EERTCO
     C                   END
     C*
     C* Medical recovery
     C     PARETP        WHENEQ    'M'                                          COMP
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   ADD       PAAMT         EERTME
     C                   END
     C     PATYPE        IFEQ      'A'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   SUB       PAAMT         EERTME
     C                   END
     C*
     C* Legal recovery
     C     PARETP        WHENEQ    'L'                                          COMP
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   ADD       PAAMT         EERTLE
     C                   END
     C     PATYPE        IFEQ      'A'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   SUB       PAAMT         EERTLE
     C                   END
     C*
     C* Expenses recovery
     C     PARETP        WHENEQ    'E'                                          COMP
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   ADD       PAAMT         EERTEX
     C                   END
     C     PATYPE        IFEQ      'A'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   SUB       PAAMT         EERTEX
     C                   END
     C*
     C                   OTHER
     C                   ENDSL                                                  PARETP = E
     C*
     C* Now Get the Current Period Recoveries to Date first
     C     PAYYMM        IFEQ      XXYYMM
     C     PATRDT        ANDLE     XXYMD
     C*
     C                   SELECT
     C* Compensation recovery
     C     PARETP        WHENEQ    'C'                                          COMP
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   ADD       PAAMT         EERCCO
     C                   END
     C     PATYPE        IFEQ      'A'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   SUB       PAAMT         EERCCO
     C                   END
     C*
     C* Medical recovery
     C     PARETP        WHENEQ    'M'                                          COMP
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   ADD       PAAMT         EERCME
     C                   END
     C     PATYPE        IFEQ      'A'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   SUB       PAAMT         EERCME
     C                   END
     C*
     C* Legal recovery
     C     PARETP        WHENEQ    'L'                                          COMP
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   ADD       PAAMT         EERCLE
     C                   END
     C     PATYPE        IFEQ      'A'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   SUB       PAAMT         EERCLE
     C                   END
     C*
     C* Expenses recovery
     C     PARETP        WHENEQ    'E'                                          COMP
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   ADD       PAAMT         EERCEX
     C                   END
     C     PATYPE        IFEQ      'A'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   SUB       PAAMT         EERCEX
     C                   END
     C*
     C                   OTHER
     C                   ENDSL                                                  PARETP = E
     C*
     C                   ENDIF                                                  B22RCVR = Y
     C                   ENDIF                                                  PATRDT = RANGE
     C*
     C*
     C     PAYYMM        IFEQ      XXYYMM
     C     PATRDT        ANDLE     XXYMD
     C*
     C     PARETP        IFEQ      'C'                                           COMP
     C                   MOVE      '1'           *IN31
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'Z'
     C     PATYPE        OREQ      'A'
     C                   ADD       PAAMT         EEPCMP
     C                   END
     C     PATYPE        IFEQ      'V'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'R'
     C                   SUB       PAAMT         EEPCMP
     C                   END
     C                   END
     C     PARETP        IFEQ      'M'                                           MEDICAL
     C                   MOVE      '1'           *IN32
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'Z'
     C     PATYPE        OREQ      'A'
     C                   ADD       PAAMT         EEPMED
     C                   END
     C     PATYPE        IFEQ      'V'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'R'
     C                   SUB       PAAMT         EEPMED
     C                   END
     C                   END
     C     PARETP        IFEQ      'L'                                           LEGAL
     C                   MOVE      '1'           *IN33
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'Z'
     C     PATYPE        OREQ      'A'
     C                   ADD       PAAMT         EEPLEG
     C                   END
     C     PATYPE        IFEQ      'V'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'R'
     C                   SUB       PAAMT         EEPLEG
     C                   END
     C                   END
     C     *IN31         IFEQ      '0'                                           OTHER
     C     *IN32         ANDEQ     '0'
     C     *IN33         ANDEQ     '0'
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'Z'
     C     PATYPE        OREQ      'A'
     C                   ADD       PAAMT         EEPOTH
     C                   END
     C     PATYPE        IFEQ      'V'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'R'
     C                   SUB       PAAMT         EEPOTH
     C                   END
     C                   END
     C**
     C                   END
     C*
     C     P2KEY         READE     PPA02                                  60
     C                   ENDDO
     C**
     C*  GET RESERVE HISTORY FILE - ACCUMULATE RESERVE CHGS THIS PERIOD
     C*
     C                   Z-ADD     *ZERO         EERCMP
     C                   Z-ADD     *ZERO         EERMED
     C                   Z-ADD     *ZERO         EERLEG
     C                   Z-ADD     *ZERO         EEROTH
     C                   MOVE      '000'         *IN(31)
     C*
     C     RHKEY         CHAIN     PRH01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL     RHEFDT        RHYYMM            5 0
     C     RHYYMM        IFEQ      XXYYMM
     C     RHEFDT        ANDLE     XXYMD
     C*
     C                   Z-ADD     1             X
     C                   DO        10            X                 2 0
     C                   MOVEL     RHR(X)        XXCTGY
     C**
     C*  GET RESERVE TYPE CODE FILE
     C*
     C                   MOVE      '0'           *IN31
     C                   MOVE      '0'           *IN32
     C                   MOVE      '0'           *IN33
     C                   MOVE      '0'           *IN34
     C     TBKEY         CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C     RTTYPE        IFEQ      'C'                                           COMP
     C                   MOVE      '1'           *IN31
     C                   END
     C     RTTYPE        IFEQ      'M'                                           MEDICAL
     C                   MOVE      '1'           *IN32
     C                   END
     C     RTTYPE        IFEQ      'L'
     C                   MOVE      '1'           *IN33                           LEGAL
     C                   END
     C  N31
     CANN32
     CANN33              MOVE      '1'           *IN34                           OTHER
     C**
     C   31              ADD       RHS(X)        EERCMP
     C   32              ADD       RHS(X)        EERMED
     C   33              ADD       RHS(X)        EERLEG
     C   34              ADD       RHS(X)        EEROTH
     C                   END
     C                   END
     C                   END
     C*
     C     RHKEY         READE     PRH01                                  60
     C                   ENDDO
     C*
     C*
     C*  ACCUMULATE PAYMENTS-TO-DATE/OUTSTND RESERVES USING CLAIMS FILE
     C*
     C                   Z-ADD     *ZERO         EEDCMP
     C                   Z-ADD     *ZERO         EEDMED
     C                   Z-ADD     *ZERO         EEDLEG
     C                   Z-ADD     *ZERO         EEDOTH
     C                   Z-ADD     *ZERO         EEOCMP
     C                   Z-ADD     *ZERO         EEOMED
     C                   Z-ADD     *ZERO         EEOLEG
     C                   Z-ADD     *ZERO         EEOOTH
     C                   Z-ADD     1             R
     C                   DO        10            R                 2 0
     C                   MOVEL     CMT(R)        XXCTGY
     C**
     C*  GET RESERVE TYPE CODE FILE
     C*
     C     TBKEY         CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C     RTTYPE        IFEQ      'C'
     C                   ADD       CMA(R)        EEDCMP
     C                   ADD       CMR(R)        EEOCMP
     C                   GOTO      ARND03
     C                   END
     C     RTTYPE        IFEQ      'M'
     C                   ADD       CMA(R)        EEDMED
     C                   ADD       CMR(R)        EEOMED
     C                   GOTO      ARND03
     C                   END
     C     RTTYPE        IFEQ      'L'
     C                   ADD       CMA(R)        EEDLEG
     C                   ADD       CMR(R)        EEOLEG
     C                   GOTO      ARND03
     C                   END
     C**
     C                   ADD       CMA(R)        EEDOTH
     C                   ADD       CMR(R)        EEOOTH
     C     ARND03        TAG
     C**
     C                   END
     C                   END
     C**
     C*  DETERMINE SORT FIELD
     C*
     C                   MOVE      *BLANKS       EESORT
     C*
     C     XXSORT        IFEQ      'C'
     C                   MOVEL     CMCASE        EESORT
     C                   END
     C*
     C     XXSORT        IFEQ      'A'
     C                   MOVEL     EECLMN        EESORT
     C                   END
     C*
     C     XXSORT        IFEQ      'S'
     C                   MOVEL     CMSS#         EESORT
     C                   END
     C*
     C     XXSORT        IFEQ      'J'
     C                   MOVEL     CMCLAS        EESORT
     C                   END
     C*
     C     XXSORT        IFEQ      '1'
     C                   MOVEL     CMCTG1        EESORT
     C                   END
     C*
     C     XXSORT        IFEQ      '2'
     C                   MOVEL     CMCTG2        EESORT
     C                   END
     C*
     C     EMMPFL        IFEQ      'M'
     C                   Z-ADD     EMMZIP        XXZIP
     C                   ELSE
     C                   Z-ADD     EMPZIP        XXZIP
     C                   END
     C     XXSORT        IFEQ      'Z'
     C                   MOVEL     XXZIP         EESORT
     C                   END
     C*
     C     XXSORT        IFEQ      'D'
     C                   MOVEL     CMACDT        EESORT
     C                   END
     C*
     C     XXSORT        IFEQ      'N'
     C                   MOVEL     CMNATR        XESOR4            4
     C                   MOVE      CMPART        XESOR4
     C                   MOVEL     XESOR4        XESOR6            6
     C                   MOVE      CMCAUS        XESOR6
     C                   MOVEL     XESOR6        EESORT
     C                   END
     C*
     C*   EXCLUDE CLAIMS THAT HAVE BEEN CLOSED MORE THAN
     C*   NUMBER OF MONTHS SPECIFIED ON PROMPT
     C*
     C     EECLDT        IFNE      *ZERO
     C     MNTH          IFNE      *ZERO
     C     EEMNTH        IFGT      MNTH
     C                   MOVE      *HIVAL        EESORT
     C                   END
     C                   END
     C                   END
     C*
     C     XXSELO        IFEQ      'N'
     C     EEOPCL        ANDEQ     ' '
     C                   MOVE      *HIVAL        EESORT
     C                   END
     C     XXSELC        IFEQ      'N'
     C     EEOPCL        ANDEQ     'C'
     C                   MOVE      *HIVAL        EESORT
     C                   END
     C*
     C*   IF AGENT SORT IS SELECTED AND AGENT NUMBER IS NOT *ZERO
     C*      THEN EXCLUDE RECORDS THAT ARE NOT FOR THE SELECTED AGENT.
     C*
     C     XXSAGT        IFEQ      'Y'
     C     XXAGT#        IFNE      0
     C     EEAGT#        ANDNE     XXAGT#
     C                   MOVE      *HIVAL        EESORT
     C                   END
     C                   END
     C*
     C*   IF TOTAL INCURRED AMOUNT HAS BEEN SELECTED VERIFY
     C*      IF THE CLAIM TOTAL INCURRED IS GT OR LT
     C*
     C                   Z-ADD     *ZERO         XXITOT
     C*
     C     XXTINC        IFGT      *ZERO
     C*
     C                   ADD       EEOCMP        XXITOT                         OUTS RSV
     C                   ADD       EEOMED        XXITOT
     C                   ADD       EEOLEG        XXITOT
     C                   ADD       EEOOTH        XXITOT
     C                   ADD       EEDCMP        XXITOT
     C                   ADD       EEDMED        XXITOT
     C                   ADD       EEDLEG        XXITOT
     C                   ADD       EEDOTH        XXITOT
     C*
     C                   SELECT
     C     QQGTLT        WHENEQ    'G'
     C     XXITOT        IFLT      XXTINC
     C                   MOVE      *HIVAL        EESORT
     C                   ENDIF                                                  XXITOT<XXTINC
     C     QQGTLT        WHENEQ    'L'
     C     XXITOT        IFGT      XXTINC
     C                   MOVE      *HIVAL        EESORT
     C                   ENDIF                                                  XXITOT>XXTINC
     C                   ENDSL
     C*
     C                   ENDIF                                                  XXTINC<>*ZERO
     C*
     C*
     C*
     C                   ADD       1             CSCNT
     C                   MOVE      SVRTCD        EERTCD
     C*** Begin Chg *** 06/29/17 *******************************
     C**********         MOVE      SVJRST        EEJRST
     C                   MOVE      CMJRST        EEJRST
     C*** End   Chg *** 06/29/17 *******************************
     C                   MOVE      CMCLEX        EEFEAD
     C                   MOVE      CMEXT         EEEXT
      *
     C* Calculate the Gross Fields for both Current and to-date fields
     C     EEPCMP        ADD       EERCCO        EEGPCO                         CURRENT
     C     EEPMED        ADD       EERCME        EEGPME                         CURRENT
     C     EEPLEG        ADD       EERCLE        EEGPLE                         CURRENT
     C     EEPOTH        ADD       EERCEX        EEGPEX                         CURRENT
      *
     C     EEDCMP        ADD       EERTCO        EEGCMP                         GROSS P-T-D
     C     EEDMED        ADD       EERTME        EEGMED                         GROSS P-T-D
     C     EEDLEG        ADD       EERTLE        EEGLEG                         GROSS P-T-D
     C     EEDOTH        ADD       EERTEX        EEGEXP                         GROSS P-T-D
      ***  BEG ADD  ***  08/31/12  *****************************************
      /free

       // Retrieve the Carrier Policy Number from file WMRPP.
       Chain keyWMRPL2 prp01;
       If %Found(WMRPL2);
          EEcap# = RPcap#;
       EndIf;

      /end-free
      ***  END ADD  ***  08/31/12  *****************************************
      *
     C                   WRITE     PEE01
     C*
     C     ENDA          TAG
     C*
     C     CMKEY1        READE     PCM01                                  60
     C                   END
     C*
     C     CSCNT         IFEQ      *ZEROS
     C*** Begin Add *** 07/27/18 *******************************************
     C     QQempC        AndEQ     'Y'
     C*** End   Add *** 07/27/18 *******************************************
     C*
     C                   CLEAR                   PEE01
     C*
     C                   MOVE      ELCO#         EECO#
     C                   MOVE      ELFND         EEFND
     C                   MOVEL     FNNAME        EEFNAM
     C                   MOVE      ELEMP#        EEEMP#
     C                   MOVE      ELDIV         EEDIV
     C                   MOVE      ELFYR         EEFYR
     C                   MOVE      *BLANKS       EEOPCL
     C                   MOVE      *BLANKS       EESORT
     C*
     C                   Z-ADD     ELEMP#        XXEMPR
     C                   Z-ADD     ELDIV         XXDIV
     C*
     C*  GET THE EMPLOYER'S REGION
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C                   MOVE      EMRGN#        EERGN#
     C                   MOVEL     EMNAME        EEENAM
     C                   MOVEL     EMSEMP        EERISK
     C*
      ***  BEG ADD  ***  08/31/12  *****************************************
      /free

       // Retrieve the Carrier Policy Number from file WMRPP.
       Chain keyWMRPL2 prp01;
       If %Found(WMRPL2);
          EEcap# = RPcap#;
       EndIf;

      /end-free
      ***  END ADD  ***  08/31/12  *****************************************
     C*  Get region description
     C     REKY          CHAIN     PRE01                              60
     C     *IN60         IFEQ      *OFF
     C     RESTAT        ANDEQ     'A'
     C                   MOVEL(P)  REDESC        EERDES
     C                   END
     C*
     C*  GET THE AGENT ASSIGNED TO THE EMPLOYER
     C*
     C     AHKEY         SETGT     PAH01
     C                   READP     PAH01                                  60
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C     AHCO#         ORNE      XXCO#
     C     AHFND         ORNE      FNFND
     C     AHEMP#        ORNE      XXEMPR
     C     AHDIV         ORNE      XXDIV
     C     AHFYR         ORNE      XXFYR3
     C                   Z-ADD     0             AHAGT#
     C                   END
     C     XXAGT#        IFNE      0
     C     AHAGT#        ANDNE     XXAGT#
     C                   GOTO      END1
     C                   END
     C                   Z-ADD     AHAGT#        EEAGT#
     C*
     C*  IF AGENT SORT IS SELECTED AND AGENT NUMBER IS NOT *ZERO
     C*  THEN EXCLUDE RECORDS THAT ARE NOT FOR THE SELECTED AGENT.
     C*
     C     XXSAGT        IFEQ      'Y'
     C     XXAGT#        IFNE      0
     C     EEAGT#        ANDNE     XXAGT#
     C                   MOVE      *HIVAL        EESORT
     C                   END
     C                   END
     C                   MOVE      *ZEROS        EECASE
     C                   MOVE      *BLANKS       EECLMN
     C                   MOVE      *ZEROS        EECLAS
     C                   MOVE      *BLANKS       EECLDS
     C                   MOVE      *BLANKS       EESS#
     C                   MOVE      *BLANKS       EECTG1
     C                   MOVE      *BLANKS       EECTG2
     C                   MOVE      *ZEROS        EEAGE
     C                   MOVE      *BLANKS       EESEX
     C                   MOVE      *ZEROS        EEACDT
     C                   MOVE      *ZEROS        EEACTM
     C                   MOVE      *ZEROS        EEDAYW
     C                   MOVE      *ZEROS        EERPDT
     C                   MOVE      *BLANKS       EENATR
     C                   MOVE      *BLANKS       EEPART
     C                   MOVE      *BLANKS       EENATR
     C                   MOVE      *BLANKS       EECAUS
     C                   MOVE      *BLANKS       EELSTM
     C                   MOVE      *ZEROS        EECLDT
     C                   MOVE      *BLANKS       EEACDS
     C                   MOVE      *ZEROS        EEPCMP
     C                   MOVE      *ZEROS        EEPMED
     C                   MOVE      *ZEROS        EEPLEG
     C                   MOVE      *ZEROS        EEPOTH
     C                   MOVE      *ZEROS        EERCMP
     C                   MOVE      *ZEROS        EERMED
     C                   MOVE      *ZEROS        EERLEG
     C                   MOVE      *ZEROS        EEROTH
     C                   MOVE      *ZEROS        EEDCMP
     C                   MOVE      *ZEROS        EEDMED
     C                   MOVE      *ZEROS        EEDLEG
     C                   MOVE      *ZEROS        EEDOTH
     C                   MOVE      *ZEROS        EEOCMP
     C                   MOVE      *ZEROS        EEOMED
     C                   MOVE      *ZEROS        EEOLEG
     C                   MOVE      *ZEROS        EEOOTH
      *
     C* Calculate the Gross Fields for both Current and to-date fields
     C                   Z-ADD     *ZEROS        EERCCO                         CURRENT COMP
     C                   Z-ADD     *ZEROS        EERCME                         CURRENT MED
     C                   Z-ADD     *ZEROS        EERCLE                         CURRENT LEGAL
     C                   Z-ADD     *ZEROS        EERCEX                         CURRENT OTHER
     C                   Z-ADD     *ZEROS        EEGPCO                         CURR COMP GROSS
     C                   Z-ADD     *ZEROS        EEGPME                         CURR MED  GROSS
     C                   Z-ADD     *ZEROS        EEGPLE                         CURR LEGAL GRSS
     C                   Z-ADD     *ZEROS        EEGPEX                         CURR OTHER GRSS
      *
     C                   Z-ADD     *ZEROS        EERTCO                         COMP  P-T-D
     C                   Z-ADD     *ZEROS        EERTME                         MED   P-T-D
     C                   Z-ADD     *ZEROS        EERTLE                         LEGAL P-T-D
     C                   Z-ADD     *ZEROS        EERTEX                         OTHER P-T-D
     C                   Z-ADD     *ZEROS        EEGCMP                         GROSS P-T-D
     C                   Z-ADD     *ZEROS        EEGMED                         GROSS P-T-D
     C                   Z-ADD     *ZEROS        EEGLEG                         GROSS P-T-D
     C                   Z-ADD     *ZEROS        EEGEXP                         GROSS P-T-D
      *
     C                   MOVE      SVRTCD        EERTCD
     C                   MOVE      SVJRST        EEJRST
     C                   WRITE     PEE01
     C                   END                                                    CSCNT = 0
     C     END1          TAG
     C                   MOVE      *ZEROS        CSCNT
     C*
     C     XQEMP#        IFEQ      *ZERO
     C     ELKEY         READE     PEL01                                  60
     C                   ELSE
     C*
     C     QQDIV         IFEQ      *BLANK
     C     ELKY2         READE     PEL01                                  60
     C                   ELSE
     C*
     C     ELKEY1        READE     PEL01                                  60
     C                   ENDIF                                                  QQDIV=*BLANKS
     C                   END
     C                   END
     C*
     C     XXFYR         IFEQ      0
     C     F1KEY         READE     PF101                                  60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   END
     C                   END
     C*
     C     XXFND         IFEQ      0
     C     FNKEY         READE     PFN01                                  60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   END
     C*
     C                   END
     C*** Begin Add *** 05/31/18 *************************************
     C* Clean up WXEEP by removing entries that were marked as ommitted
     C                   ExSr      ClnWXEEP
     C*** End   Add *** 05/31/18 *************************************
     C**
     C     END           TAG
     C                   MOVE      '1'           *INLR
     C**
     C*** Begin Add *** 05/31/18 *************************************
      *==================================================================*
      *   Clean up WXEEP by removing records that were marked as
      *   not selected.
      *==================================================================*
     CSR   CLNWXEEP      BEGSR
     C*
      /free
       Chain xxCo# pee01;
       If %Found(wxeel);
          DoU %Eof(wxeel);
             If EEsort = *HiVal;
                Delete pee01;
             EndIf;
             ReadE xxCo# pee01;
          EndDo;
       EndIf;
      /end-free
     C*
     C                   ENDSR
     C*** End   Add *** 05/31/18 *************************************
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C*
     C     COKEY         CHAIN     SMCOL                              60
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   END
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C                   MOVE      '1'           *IN22
     C*
     C                   ENDSR
     C*
