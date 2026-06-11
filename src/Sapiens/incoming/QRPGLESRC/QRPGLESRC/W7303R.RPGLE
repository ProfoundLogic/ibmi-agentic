     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7303R - Producer Performance Report
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 11/15/22 C:342 Support 104161 (UBIC)                          *
     H*                Print cancellation date for subsequent year    *
     H*                in PPR report.                                 *
     H*                                                               *
     H* 10/10/22 C:342 Support 103863 (UBIC)                          *
     H*                Fix cancellation date issue                    *
     H*                                                               *
     H* 01/17/17 C:180 Support 43856                                  *
     H*                Fix cancellation date.                         *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/22/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     F* 05/14/10  C:246  Project: 139WNI PRISS 20                     *
     F*                  Fix the Title issue on this report.          *
     F*                                                               *
     F* 01/28/10  C:249  Project: SY215                               *
     F*           C:189  Retrieve brand Name                          *
     F*                                                               *
     F* 01/08/10  C:238  Project: 139WNI                              *
     F*                  Agent Loss Ratio Report for all Groups       *
     F*                                                               *
     F* 10/20/09 C:189 PROJECT WC1726                                 *
     F*                CSB-AGT LOSS RATIO/PPR RPT SELECTION CHG       *
     H*                                                               *
     H*  09/11/06  C:132 Project: WC1420                              *
     H*                  Change to load two new fields QFPPP/PPAEFD   *
     H*                  and QFPPP/PPAGCD at different loaction in    *
     H*                  code, timing is wrong.                       *
     H*                                                               *
     H*  06/30/06  C:132 Project: WC1420                              *
     H*                  Change to load two new fields QFPPP/PPAEFD   *
     H*                  and QFPPP/PPAGCD, same fields in WFPPP.      *
     H*                                                               *
     H*  06/16/06  C:132 Project: WC1420                              *
     H*                  Change to load get agent name if blank when  *
     H*                  loading file record. Also get producer name  *
     H*                  if blank when loading WFPPP fields.          *
     H*                                                               *
     H*  06/14/06  C:132 Project: WC1420                              *
     H*                  Change to load some of the new fields from   *
     H*                  producer file in case there is no 2nd agent  *
     H*                  and no 2nd producer.                         *
     H*                                                               *
     H*  04/03/06  C:132 Project: WC1420                              *
     H*                  Load new fields added to WFPPP file.         *
     H*                                                               *
     H*  08/24/05  C:33  Project: WC1314                              *
     H*                  Add agt# and producer selections.            *
     H*                                                               *
     H*  03/31/05 C:132 PROJECT: WA099  46283                         *
     H*               - Change Company name on file output to be left *
     H*                 justified when file output is requested.      *
     H*                                                               *
     H*  11/08/04 C:132 PROJECT: WA081  44417                         *
     H*               - Change location of blank/zero out fields when *
     H*                 only File output is requested.                *
     H*                                                               *
     H*  07/15/04 C:132 PROJECT: WA081  43214                         *
     H*               - Remove Blank After on Print file for Producer *
     H*                 PNAME, so output in WFPPP file .              *
     H*                                                               *
     H*  06/16/04 C:132 PROJECT: 81WWH                                *
     H*               - ADD additional fields to WFPPP output file    *
     H*                 PPARNM - Area Name, PPCXDT - Cancel Date      *
     H*                                                               *
     H*  06/03/04 C:132 PROJECT: 81WWH Add Client 081 MODs to Base    *
     H*               - ADD an additional sort option by Agent; in    *
     H*                 addition to current sort options by Chapter,  *
     H*                 Area and County                               *
     H*               - ADD separate premium & counts for ACTIVE vs   *
     H*                 CANCELLED Policues                            *
     H*               - produce a separate Agent Summary report with  *
     H*                 one line item per agent.                      *
     H*                                                               *
     H*  05/12/04 C:62 PROJECT: WC1215                                *
     H*                ADD DATE RANGE PROCESING                       *
     H*                                                               *
     H*  04/16/02 C:90 SEQ#: WA121/31829                              *
     H*                WRITE RECORDS TO PRODUCER PERFORMANCE REPORT   *
     H*                WORKFILE - WFPPP.                              *
     H*
     H*  08/10/00 C:33 SEQ#: WA109/23913                              *
     H*                CHECK FOR INACTIVE RATE CODES.                 *
     H*
     H* 12/16/99 C:33 Y2K#: 070                                       *
     H*               EXPAND SIZE OF LOSS RATIO FIELDS.               *
     H*                                                               *
     H*  04/30/99 C:118 PROJECT: WC577                                *
     H*                 MADE ROUTING PAGE CHANGES TO LEFT ALIGN REPORT*
     H*                 TITLE, ADJUST :'S AND CENTER TITLE ON REPORT. *
     H*                                                               *
     H*  04/14/98 C:65 PROJECT WC710                                  *
     H*                FIXED PARMS FOR CALL TO EARNED PREMIUM SUBRO.  *
     H*                                                               *
     H*  12/15/97 C:65 PROJECT V3                                     *
     H*                REMOVED DEVIATION FLAG.                        *
     H*                                                               *
     H*  12/04/97 C:85 PROJECT WC611                                  *
     H*                COPIED FROM W7303R ON THE B70.                 *
     H*                (SEE THE 12/02/96 COMMENTS.)                   *
     H*                                                               *
     H*  08/27/97 C:77 PROJECT WC577                                  *
     H*               -REPORT "WRITTEN PREMIUM" AS ELANPR (PRORATED,  *
     H*                WITH EXPENCE CONSTANT) FOR ESTIMATED EMPLOYERS.*
     H*                (DELETED WDECL, RELATED *LIKE DEFN VAR.S AND   *
     H*                KEYS SINCE DON'T NEED EXPENSE CONST FILE OR    *
     H*                ITS VARIABLES)                                 *
     H*               -REMOVE CODE WHICH REFERENCES F1DVPR.(NONE)     *
     H*                                                               *
     H* 06/26/97 C:62 PROJECT WC710                                   *
     H*               COUNTY CODE NOW IS 4A, STATE + COUNTY.          *
     H*               ALTERNATE COMPANY NAME IF NOT BLANK.            *
     H*                                                               *
     H*  03/03/97 C:65 PROJECT WC643                                  *
     H*                CHANGED EXPENSE CONSTANT DEFINITION TO THE     *
     H*                FUND LEVEL.                                    *
     H*                                                               *
     H*  12/02/96 C:36 PROJECT WC611                                  *
     H*                CHANGED WRITTEN PREMIUM CALCULATION TO USE     *
     H*                NORMAL RATHER THAN STANDARD.                   *
     H*                                                               *
     H*  03/13/96 C:35 PROJECT CM012                                  *
     H*                CHANGED TO GET EXPENSE CONSTANT FROM WDECP     *
     H*                BASED ON STATE AND EFFECTIVE DATE.             *
     H*                                                               *
     H*  10/24/95 C:11 PROJECT:WC453                                  *
     H*                GET THE LAST ACTIVE AGENT AS OF THE FYR, NOT   *
     H*                AS OF THE RUN DATE.                            *
     H*                                                               *
     H*  10/17/91 TAA  PROJ #: WC075                                  *
     H*                ADDED CODE TO ONLY BILL EXPENSE CONSTANT  IF   *
     H*                EMSTRP = 'Y' AND THERE IS A SUCCESSFUL CHAIN   *
     H*                TO EMPLOYER MASTER.                            *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     FWXALL1    IF   E           K DISK
     FWXALL2    IF   E           K DISK
     F                                     RENAME(PAL01:PAL02)
     FWXALL3    IF   E           K DISK
     F                                     RENAME(PAL01:PAL03)
      *
      ***** BEG ADD ***** 06/03/04 ************************************
     FWXALL     IF   E           K DISK
     F                                     RENAME(PAL01:PAL04)
      ***** END ADD ***** 06/03/04 ************************************
     F* * * Work file built from W7301R
     F*
     FWMAAL1    IF   E           K DISK
     F* * * Agent Assignment File
     F*
     FWMAGL     IF   E           K DISK
     F* * * Agent Master File
     F*
     FWMAPL     IF   E           K DISK
     F* * * Agent Producer File
     F*
     FWMARL     IF   E           K DISK
     F* * * Agent Rate File
     F*
     FWMAHL     IF   E           K DISK
     F* * * Agent History File
     F*
     FWMCML     IF   E           K DISK
     F* * * Claims File
     F*
     FWMEML     IF   E           K DISK
     F* * * Employer File
     F*
     FWMFNL     IF   E           K DISK
     F* * * Fund File
     F*
     F*
     FSMORL     IF   E           K DISK
     F* * * Batch Routing File
     F*
     FSMCOL     IF   E           K DISK
     F* * * Company File
     F*
     FWMUPL     IF   E           K DISK
     F* * * User Parameter File
     F*
     FWDELL     IF   E           K DISK
     F* * * EMPLOYER DETAIL FILE
     F*
     FWXAPL     UF A E           K DISK
     F                                     RENAME(PAP01:PAP02)
     FWMAEL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWMCPL     IF   E           K DISK
     F*** BEGIN DEL *** 08/27/97 **************************************
     F***WDECL   IF  E           K        DISK
     F*** END DEL ***** 08/27/97 **************************************
     F*** BEGIN ADD *** 12/02/96 **************************************
     F*** BEGIN DEL *** 05/12/04 **************************************
     F**********WDF1L   IF  E           K        DISK
     F*** END   DEL *** 05/12/04 **************************************
     F* * * Fund Detail File
     F*** END   ADD *** 12/02/96 **************************************
      **** BEGIN ADD **** 04/16/02 ************************************
     FWFPPP     O    E             DISK
     F* * * PRODUCER PERFORMANCE REPORT WORKFILE
      **** END   ADD **** 04/16/02 ************************************
      **** BEGIN ADD **** 10/20/09 ************************************
     FWTCNL1    IF   E           K DISK
     F                                     RENAME(PCN01:PCN02)
      **** END   ADD **** 10/20/09 ************************************
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F                                     USROPN
     F* * * Output File
     F*
      ***** BEG ADD ***** 06/03/04 ************************************
     F* Agent Total Summary Print file
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN
      ***** END ADD ***** 06/03/04 ************************************
     F*** BEG   ADD *** 01/08/10 **************************************
     FWXGSL     IF   E           K DISK
     F                                     RENAME(PGS01:PGS00)

     D*** END   ADD *** 01/08/10 **************************************
     F*
      ***** BEG ADD ***** 06/03/04 ************************************
      ***** END ADD ***** 06/03/04 ************************************
     D*
     D*** BEG ADD *** 01/08/10 **************************************
     D GRP             S              3    DIM(999)
     D GRPX            S              4    DIM(32)
     D*** END ADD *** 01/08/10 **************************************
     D***  Begin Add  ***  08/24/05  **************************************
     D CHGNUM          C                   CONST('WA003R')
     D***  End   Add  ***  08/24/05  **************************************
     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)
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
     D                                     DIM(10)
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
     D                                     DIM(10)
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
     D  EME                    1    035  0
     D                                     DIM(5)
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
      ***** BEG ADD ***** 06/03/04 ************************************
     D                 DS
     D  EMC                    1    035  0
     D                                     DIM(5)
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
      ***** END ADD ***** 06/03/04 ************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXYMD1                 1      7  0
     D  XXTDYR                 2      3  0
     D  XXTDMO                 4      5  0
     D                 DS
     D  XXEFDT                 1      7  0
     D  XXEDYR                 2      3  0
     D  XXEDMO                 4      5  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#            5 0
      *
      ***** BEG ADD ****** 04/03/06 ***********************************
     C     AAKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHSAGT
      ***** END ADD ****** 04/03/06 ***********************************
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXYR
     C*
     C*  AGENT RATE FILE
     C*
     C     ARKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    AHRT#
     C                   KFLD                    XXYMD
     C*
     C***  BEGIN ADD  ***  08/10/00  *********************************
     C     ARKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    AHRT#
     C***  END   ADD  ***  08/10/00  *********************************
     C*
     C*  AGENT PRODUCER FILE
     C*
     C     APKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C                   KFLD                    XXPROD            5 0
     C*
     C*  AGENT PRODUCER WORK FILE
     C*
     C     APKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** BEGIN DEL *** 05/12/04 **************************************
     C**********           KFLD           XXFYR
     C*** END   DEL *** 05/12/04 **************************************
     C*
     C*  AGENT AREA FILE
     C*
     C     AEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ALAREA
     C*
     C*  TABLE FILE - FOR COUNTY CODE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  CHAPTER FILE
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ALCHAP
      **** BEGIN ADD **** 04/16/02 ************************************
     C     CPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCHAP
      **** END   ADD **** 04/16/02 ************************************
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXYR              3 0
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXYR
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** BEGIN DEL *** 05/12/04 **************************************
     C*** BEGIN ADD *** 12/02/96 **************************************
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C********** F1KEY     KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXFND
     C**********           KFLD           XXFYR
     C*** END   ADD *** 12/02/96 **************************************
     C*** END   DEL *** 05/12/04 **************************************
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* KEY LISTS FOR THE EXPENSE CONSTANT DEFINITION FILE
     C*
     C     *LIKE         DEFINE    AHDT          XXYMD
     C     *LIKE         DEFINE    AHDT          EFFDTE
     C     *LIKE         DEFINE    AHEMP#        XXEMP#
     C     *LIKE         DEFINE    AHDIV         XXDIV
     C*** BEGIN ADD *** 06/26/97 **************************************
     C     *LIKE         DEFINE    FNANAM        XXCONM
     C     *LIKE         DEFINE    ALCNTY        SVCNTY
     C*** END   ADD *** 06/26/97 **************************************
     C*
      ***** BEG ADD ***** 06/03/04 ************************************
     C     *LIKE         DEFINE    ALAGT#        SVAGT#
      *
     C     *LIKE         DEFINE    ORDIS1        XXDIS1
     C     *LIKE         DEFINE    ORDIS2        XXDIS2
     C     *LIKE         DEFINE    ORDIS3        XXDIS3
     C     *LIKE         DEFINE    ORDIS4        XXDIS4
     C     *LIKE         DEFINE    ORPNAM        XXPNAM
     C     *LIKE         DEFINE    ORFIL#        XXFIL#
     C     *LIKE         DEFINE    ORCOPY        XXCOPY
     C     *LIKE         DEFINE    ORHOLD        XXHOLD
     C     *LIKE         DEFINE    OROUTQ        XXOUTQ
     C     *LIKE         DEFINE    ORFORM        XXFORM
     C     *LIKE         DEFINE    ORSAVE        XXSAVE
      ***** END ADD ***** 06/03/04 ************************************
     C*
     C*** BEGIN ADD *** 12/02/96 **************************************
     C     *LIKE         DEFINE    ELANPR        XXANPR
     C*** END   ADD *** 12/02/96 **************************************
      **** BEGIN ADD **** 04/16/02 ************************************
     C     *LIKE         DEFINE    ALCHAP        XXCHAP
      **** END   ADD **** 04/16/02 ************************************
     C*
      ***** BEG ADD ***** 03/31/05 ************************************
     C     *LIKE         DEFINE    PPCNAM        ZZCONM
      ***** END ADD ***** 03/31/05 ************************************
      *
      ***** BEG ADD ***** 06/16/06 ************************************
     C     *LIKE         DEFINE    PPACSZ        XXACSZ
      ***** END ADD ***** 06/16/06 ************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*** BEGIN CHG *** 05/12/04 **************************************
     C**********           PARM           QQFYR   3
     C                   PARM                    QQFRDT            7
     C                   PARM                    QQTODT            7
     C*** END   CHG *** 05/12/04 **************************************
     C                   PARM                    QQDTE             8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQSORT            1
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   PARM                    QQOUTP            1
      **** END   ADD **** 04/16/02 ************************************
     C***  Begin Add  ***  08/24/05  **************************************
     C                   PARM                    QQAGT#            5
     C                   PARM                    QQPROD            5
     C***  End   Add  ***  08/24/05  **************************************
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
     C***  Begin Add  ***  08/24/05  **************************************
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C***  End   Add  ***  08/24/05  **************************************
     C*
     C*** BEGIN ADD *** 08/27/97 **************************************
     C*** DETERMINE SOR VALUE FOR REPORT SELECTION OUTPUT
     C     QQSORT        IFEQ      '1'
     C                   MOVE      *ON           *IN91
     C                   ENDIF
     C     QQSORT        IFEQ      '2'
     C                   MOVE      *ON           *IN92
     C                   ENDIF
     C     QQSORT        IFEQ      '3'
     C                   MOVE      *ON           *IN93
     C                   ENDIF
      ***** BEG ADD ***** 06/03/04 ************************************
     C     QQSORT        IFEQ      '4'
     C                   MOVE      *ON           *IN94
     C                   ENDIF
      ***** END ADD ***** 06/03/04 ************************************
      *
     C*** END ADD ***** 08/27/97 **************************************
     C*** BEG ADD *** 05/14/10 **************************************
     C                   MOVE      *ZERO         SVFND             3 0
     C                   MOVE      *OFF          SUB4ID            1
     C                   MOVE      *OFF          TOT3IN            1
     C*** END ADD ***** 05/14/10 **************************************
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C*** BEG ADD *** 01/08/10 **************************************
     C                   MOVE      '000'         QQFND
     C*** END ADD *** 01/08/10 **************************************
     C                   MOVE      QQFND         XXFND             3 0
     C*** BEGIN CHG *** 05/12/04 **************************************
     C**********           MOVE QQFYR     XXFYR   30
     C                   MOVE      QQFRDT        XXFRDT            7 0
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FRDT            8 0
     C                   MOVE      QQTODT        XXTODT            7 0
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TODT            8 0
     C*** END   CHG *** 05/12/04 **************************************
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      QQDTE         X1DATE            8 0
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C***  Begin Add  ***  08/24/05  **************************************
     C                   CLEAR                   ##AGT#
     C     Q1PRMS        IFGE      10
     C                   MOVE      QQAGT#        ##AGT#            5 0
     C                   MOVEL     ##AGT#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         RSAGT#
     C                   ENDIF
     C*
     C                   CLEAR                   ##PROD
     C     Q1PRMS        IFGE      11
     C                   MOVE      QQPROD        ##PROD            5 0
     C                   MOVEL     ##PROD        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         RSPROD
     C                   ENDIF
     C*
     C     ##AGT#        IFEQ      *ZERO
     C                   MOVEL     '*ALL'        RSAGT#            5
     C                   ENDIF
     C*
     C     ##PROD        IFEQ      *ZERO
     C                   MOVEL     '*ALL'        RSPROD            5
     C                   ENDIF
     C***  End   Add  ***  08/24/05  **************************************
      *** BEG ADD **** 01/08/10 ************************************
     C     QQFND         IFEQ      *ZEROS
     C                   EXSR      LDRTFD
     C                   ELSE
     C                   MOVEL     QQFND         RPFND
     C                   END
      *** END ADD **** 01/08/10 ************************************
     C*
     C                   EXSR      SR1ST
     C*** BEG ADD *** 01/08/10 **************************************
     C     FNKEY         SETLL     PGS00
     C     COKEY         READE     PGS00                                  50
     C*
     C     *IN50         DOWEQ     *OFF
     C     GSIEFL        IFEQ      'I'
     C                   MOVE      GSCO#         XXCO#
     C                   MOVE      GSFND         XXFND
      *** BEG ADD **** 05/14/10 ************************************
     C     XXFND         IFNE      SVFND
     C     SVFND         ANDNE     0
     C     TOT3IN        IFEQ      '1'
     C                   EXCEPT    TOTL3
     C                   MOVE      *OFF          TOT3IN
     C     QQAGT#        IFNE      *BLANKS
     C                   MOVE      *ON           *INOF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     SUB4ID        IFEQ      '1'
     C                   EXSR      SUB4
     C                   MOVE      *OFF          SUB4ID
     C                   ENDIF
     C                   MOVE      XXFND         SVFND
      *** END ADD **** 05/14/10 ************************************
     C                   EXSR      PROC
     C                   ENDIF
     C     COKEY         READE     PGS00                                  50
     C                   ENDDO
      *** BEG ADD **** 05/14/10 ************************************
     C                   EXCEPT    TOTL3
     C                   EXSR      SUB4
      *** END ADD **** 05/14/10 ************************************
     C                   MOVE      *ON           *INOB
     C                   MOVE      *ON           *INOF
     C                   EXSR      SUBL4
      /Free
       // ***** Beg Add ***** 08/26/14 *******************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          If qqOutp <> 'F';
             Except Route;
             Except Route2;
          Endif;
       Endif;
       // ***** End Add ***** 08/26/14 *******************************
      /End-free
     C*
     C                   MOVE      '1'           *INLR
     C*
     CSR   PROC          BEGSR
     C*** END ADD *** 01/08/10 **************************************
     C*  GET FUND NAME
     C     FNKEY         CHAIN     PFN01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C                   ELSE
     C                   MOVE      *BLANK        XFNAME
     C                   END
     C*** BEGIN ADD *** 06/26/97 **************************************
     C     FNANAM        IFNE      *BLANKS
     C*
     C                   MOVEL     FNANAM        XXCONM
     C*
     C                   ELSE
     C*
     C                   MOVEL     CONAME        XXCONM
     C*
     C                   ENDIF
     C*
      ***** BEG ADD ***** 03/31/05 ************************************
     C                   MOVEL(P)  XXCONM        ZZCONM
      ***** END ADD ***** 03/31/05 ************************************
     C*
     C                   MOVEL(P)  XXCONM        ##DATA
      ***** BEG ADD ***** 01/08/10 ************************************
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL(P)  FNANAM        ##DATA
     C                   ENDIF
      ***** END ADD ***** 01/08/10 ************************************
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      50            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXCONM
     C*
     C*** END   ADD *** 06/26/97 **************************************
     C*
     C*** BEGIN DEL *** 05/12/04 **************************************
     C*** BEGIN ADD *** 12/02/96 **************************************
     C*  Chain to the fund detail file to get DEVIATION APPLIED
     C*  TO FLAG
     C**********           MOVE *BLANKS   XXFVPR  1
     C********** F1KEY     CHAINPF101                61
     C********** *IN61     IFEQ '0'
     C*** BEGIN DEL ***  12/15/97  ************************************
     C******               MOVE F1DVPR    XXFVPR  1
     C*** END DEL *****  12/15/97  ************************************
     C***********          END
     C*** END   ADD *** 12/02/96 **************************************
     C*** END   DEL *** 05/12/04 **************************************
     C*
     C*  DELETE ALL RECORDS FROM AGENT PRODUCER WORK FILE (CO#,FND,FYR)
     C*
     C     APKY2         CHAIN     PAP02                              60
     C     *IN60         DOWEQ     '0'
     C*** BEGIN ADD *** 05/12/04 **************************************
     C     APPFDT        IFGE      XXFRDT
     C     APPFDT        ANDLE     XXTODT
     C*** END   ADD *** 05/12/04 **************************************
     C                   DELETE    PAP02
     C*** BEGIN ADD *** 05/12/04 **************************************
     C                   ELSE
     C                   UPDATE    PAP02
     C                   ENDIF
     C*** END   ADD *** 05/12/04 **************************************
     C     APKY2         READE     PAP02                                  60
     C                   END
     C*
     C*  DETERMINE FORM SORT SELECTION WHICH LOGICAL SHOULD BE USED
     C*  OVER AGENT LATE NOTICE WORK FILE
     C*
     C                   MOVE      *BLANKS       XXSORT            7
     C     QQSORT        IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      'CHAPTER'     XXSORT
     C                   ELSE
     C     QQSORT        IFEQ      '2'
     C                   MOVE      '1'           *IN22
     C                   MOVE      'AREA'        XXSORT
      ***** BEG DEL ***** 06/03/04 ************************************
     C*******              ELSE
     C*******              MOVE '1'       *IN23
     C*******              MOVE 'COUNTY'  XXSORT
      ***** END DEL ***** 06/03/04 ************************************
     C                   END
      *
      ***** BEG ADD ***** 06/03/04 ************************************
      ***** END ADD ***** 06/03/04 ************************************
      *
      ***** BEG ADD ***** 06/03/04 ************************************
     C     QQSORT        IFEQ      '3'
     C                   MOVE      '1'           *IN23
     C                   MOVE      'COUNTY'      XXSORT
     C                   ENDIF
     C     QQSORT        IFEQ      '4'
     C                   MOVE      '1'           *IN24
     C                   MOVE(P)   'AGENT'       XXSORT
     C                   ENDIF
      ***** END ADD ***** 06/03/04 ************************************
      *
     C                   END
     C                   MOVE      'Y'           XX1ST             1
     C*
     C***** BEG CHG ***** 01/08/10 ************************************
     C     FNKEY         SETLL     PAL01
     C     FNKEY         SETLL     PAL02
     C     FNKEY         SETLL     PAL03
     C     FNKEY         SETLL     PAL04
     C*        21          READ PAL01                    50
     C*        22          READ PAL02                    50
     C*        23          READ PAL03                    50
      ***** BEG ADD ***** 06/03/04 ************************************
     C*        24          READ PAL04                    50
      ***** END ADD ***** 06/03/04 ************************************
     C   21FNKEY         READE     PAL01                                  50
     C   22FNKEY         READE     PAL02                                  50
     C   23FNKEY         READE     PAL03                                  50
     C   24FNKEY         READE     PAL04                                  50
     C***** END CHG ***** 01/08/10 ************************************
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C***  Begin Add  ***  08/24/05  **************************************
     C                   MOVE      *ON           MEETSS            1
     C*
     C     ##AGT#        IFNE      *ZEROS
     C*
     C     ##AGT#        IFNE      ALAGT#
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ELSE
     C*
     C     ##PROD        IFNE      *ZEROS
     C     ##PROD        ANDNE     ALPROD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     MEETSS        IFEQ      *ON
     C***  End   Add  ***  08/24/05  **************************************
     C*
     C*** BEGIN ADD *** 05/12/04 **************************************
     C                   Z-ADD     ALFYR         XXFYR             3 0
     C*** END   ADD *** 05/12/04 **************************************
     C*
     C     XX1ST         IFEQ      'Y'
     C                   Z-ADD     ALCHAP        SVCHAP
     C                   Z-ADD     ALAREA        SVAREA
     C                   MOVE      ALCNTY        SVCNTY
      ***** BEG ADD ***** 06/03/04 ************************************
     C                   MOVE      ALAGT#        SVAGT#
      ***** END ADD ***** 06/03/04 ************************************
     C                   MOVE      'N'           XX1ST
     C                   END
     C*
     C                   MOVE      'N'           XXSBRK            1
     C     *IN21         IFEQ      '1'
     C     SVCHAP        ANDNE     ALCHAP
     C                   MOVE      'Y'           XXSBRK
     C                   ELSE
     C     *IN22         IFEQ      '1'
     C     SVAREA        ANDNE     ALAREA
     C                   MOVE      'Y'           XXSBRK
     C                   ELSE
     C     *IN23         IFEQ      '1'
     C     SVCNTY        ANDNE     ALCNTY
     C                   MOVE      'Y'           XXSBRK
      ***** END ADD ***** 06/03/04 ************************************
     C                   ELSE
     C     *IN24         IFEQ      '1'
     C     SVAGT#        ANDNE     ALAGT#
     C                   MOVE      'Y'           XXSBRK
     C                   ENDIF
      ***** BEG ADD ***** 06/03/04 ************************************
     C                   END
     C                   END
     C                   END
     C*
     C     XXSBRK        IFEQ      'Y'
     C                   EXSR      SUBL1
     C                   EXSR      SUBL2
     C                   EXSR      SSRT
     C                   Z-ADD     ALCHAP        SVCHAP
     C                   Z-ADD     ALAREA        SVAREA
     C                   MOVE      ALCNTY        SVCNTY
     C                   Z-ADD     ALAGT#        SVAGNT
     C                   Z-ADD     ALPROD        SVPROD
      *
      ***** BEG ADD ***** 06/03/04 ************************************
     C                   Z-ADD     ALAGT#        SVAGT#
      ***** END ADD ***** 06/03/04 ************************************
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   END
     C*
     C     SVAGNT        IFNE      ALAGT#
     C     SVAGNT        ANDNE     *ZERO
     C                   EXSR      SUBL1
     C                   EXSR      SUBL2
     C                   EXSR      SUBHD
     C*
     C                   Z-ADD     ALAGT#        SVAGNT
     C                   Z-ADD     ALCHAP        SVCHAP
     C                   Z-ADD     ALAREA        SVAREA
     C                   MOVE      ALCNTY        SVCNTY
      ***** BEG ADD ***** 06/03/04 ************************************
     C                   Z-ADD     ALAGT#        SVAGT#
      ***** END ADD ***** 06/03/04 ************************************
     C*
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   ELSE
     C     SVPROD        IFNE      ALPROD
     C     SVPROD        ANDNE     *ZERO
     C                   EXSR      SUBL1
     C                   EXSR      SUBHD
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C                   EXCEPT    HEAD2
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   END
     C                   END
     C**
     C                   Z-ADD     ALCHAP        SVCHAP            3 0
     C                   Z-ADD     ALAREA        SVAREA            3 0
     C                   MOVE      ALCNTY        SVCNTY
     C                   Z-ADD     ALAGT#        SVAGNT            5 0
     C                   Z-ADD     ALPROD        SVPROD            5 0
     C*
     C*  READ EMPLOYER FILE
     C                   Z-ADD     ALEMP#        XXEMP#
     C                   Z-ADD     ALDIV         XXDIV
     C                   Z-ADD     ALFYR         XXYR
     C*
     C*  CHAIN TO THE EMPLOYER DETAIL FILE, IF NO HIT DO NOT
     C*  INCLUDE ON REPORT
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*
     C     ELFAF         IFEQ      'A'
     C*** BEGIN CHG *** 12/02/96 **************************************
     C**********           Z-ADDELAAPR    ELANPR
     C                   Z-ADD     ELAAPR        XXANPR
     C*** END   CHG *** 12/02/96 **************************************
     C*** BEGIN ADD ***  05/05/98  ************************************
     C                   ELSE
     C                   Z-ADD     ELANPR        XXANPR
     C*** END ADD *****  05/05/98  ************************************
     C*                                                          "
     C                   END
     C*
     C     EMKEY         CHAIN     PEM01                              51
     C     *IN51         IFEQ      '0'
     C*
     C                   Z-ADD     *ZERO         EFFDTE
      ***** BEG ADD ***** 06/03/04 ************************************
     C                   Z-ADD     *ZERO         CANDTE            8 0
     C                   MOVE      'N'           XXACT             1
      ***** END ADD ***** 06/03/04 ************************************
     C                   ADD       1             EMPTOT            5 0
     C                   MOVEL     EMNAME        ENAME            24
     C                   Z-ADD     5             X                 2 0
     C     EFFDTE        DOWEQ     *ZERO
     C     X             ANDGT     *ZERO
     C     EME(X)        IFEQ      *ZERO
     C                   SUB       1             X
     C                   ELSE
     C                   Z-ADD     EME(X)        EFFDTE
      *
      ***** BEG ADD ***** 06/03/04 ************************************
      *                    *** IF VALID CANCEL DATE
     C     EMC(X)        IFGT      *ZEROS
      ***** Beg Change   01/17/17 *************************************
     C**** EMC(X)        ANDLE     ELPTDT
      ***** Beg Change   10/10/22 *************************************
     C*****EMC(X)        ANDLT     ELPTDT
      ***** Beg Del **** 11/15/22 *************************************
     C*****EMC(X)        ANDLE     ELPTDT
     C*****EMC(X)        ANDGE     ELPFDT
      ***** End Del **** 11/15/22 *************************************
      ***** End Change   10/10/22 *************************************
      ***** End Change   01/17/17 *************************************
      ***** Beg Change   11/15/22 *************************************
     C*****EMC(X)        ANDLE     XXYMD
     C     EMC(X)        IFGE      ELPFDT
      ***** End Change   11/15/22 *************************************
      *                    *** DDMMYY FORMAT
     C                   Z-ADD     EMC(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CANDTE
     C                   SETON                                        73
      ***** Beg Add **** 11/15/22 *************************************
     C                   ELSE
     C     EMC(X)        IFLT      ELPFDT
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CANDTE
     C                   SETON                                        73
     C                   ENDIF
     C                   ENDIF
      ***** End Add **** 11/15/22 *************************************
     C                   ELSE
      *                    *** SET ACTIVE
     C                   MOVE      'Y'           XXACT
     C                   Z-ADD     0             CANDTE
     C                   SETOFF                                       73
     C                   ENDIF
      ***** END ADD ***** 06/03/04 ************************************
      *
     C                   END
     C                   END
     C                   Z-ADD     EFFDTE        SRDTI
     C*** BEGIN CHG *** 05/12/04 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXFDTE  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFDTE            8 0
     C*** END   CHG *** 05/12/04 **************************************
     C*
     C*  RETRIEVE RATE FOR EMPLOYER
     C*  IF EMPLOYER ENROLLED 1 YEAR OR MORE, USE RENEW RATE.
     C*  OTHERWISE, USE NEW RATE.  USE EARLIEST EFFECTIVE DATE.
     C*  (IGNORE LAPSES IN COVERAGE.)
     C*
     C     AHKEY2        SETGT     PAH01
     C                   READP     PAH01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C     AHCO#         ANDEQ     XXCO#
     C     AHFND         ANDEQ     XXFND
     C     AHEMP#        ANDEQ     XXEMP#
     C     AHDIV         ANDEQ     XXDIV
     C     AHFYR         ANDEQ     XXYR
     C*
      ***** BEG ADD ***** 09/11/06 ***********************************
      *  Get agent effective date
      **** BEGIN CHG **** 10/20/09 ************************************
     C***************      Z-ADDAHEFDT    PPAEFD
     C                   Z-ADD     AHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PPAEFD
      **** END CHG **** 10/20/09 ************************************
      ***** END ADD ***** 09/11/06 ***********************************
     C*
     C     ARKEY         SETGT     PAR01
     C***  BEGIN CHG  ***  08/10/00  *********************************
     C**********           READPPAR01                    60
     C********** *IN60     IFEQ '0'
     C********** ARCO#     ANDEQXXCO#
     C********** ARFND     ANDEQXXFND
     C********** ARRT#     ANDEQAHRT#
     C     ARKY1         READPE    PAR01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY1         READPE    PAR01                                  60
     C                   ENDDO
     C*
     C     *IN60         IFEQ      *OFF
     C***  END   CHG  ***  08/10/00  *********************************
     C*
     C                   Z-ADD     XXYMD         XXYMD1
     C                   Z-ADD     EMEFD1        XXEFDT
     C*
     C     XXTDYR        SUB       XXEDYR        XXREM1            3 0
     C     XXREM1        IFGT      1
     C                   Z-ADD     ARRNRT        XXRATE
     C                   ELSE
     C     XXREM1        IFEQ      1
     C                   ADD       12            XXTDMO
     C     XXTDMO        SUB       XXEDMO        XXREM2            3 0
     C     XXREM2        IFGE      12
     C                   Z-ADD     ARRNRT        XXRATE            5 2
     C                   ELSE
     C                   Z-ADD     ARNWRT        XXRATE
     C                   END
     C                   ELSE
     C                   Z-ADD     ARNWRT        XXRATE
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*****************************************************************
     C*
     C                   ADD       1             L1TOT             5 0
     C*
     C*****************************************************************
     C*
     C*  READ CLAIM FILE
     C*
     C     CMKEY         SETLL     PCM01
     C     CMKEY         READE     PCM01                                  52
     C     *IN52         DOWEQ     '0'
     C*
     C     CMCO#         IFNE      EMCO#
     C     CMFND         ORNE      EMFND
     C     CMEMP#        ORNE      EMEMP#
     C     CMDIV         ORNE      EMDIV
     C                   MOVE      '1'           *IN52
     C                   END
     C                   ADD       1             EMPCLM            5 0
     C*
     C****************************************************************
     C**
     C                   Z-ADD     *ZERO         TOTPMT
     C                   Z-ADD     *ZERO         TOTRES
     C**
     C                   Z-ADD     1             R
     C                   DO        10            R                 2 0
     C**
     C**  ACCUMULATE PAYMENT AMOUNTS
     C                   ADD       CMA(R)        TOTPMT            9 2
     C**
     C**  ACCUMULATE RESERVE AMOUNTS
     C                   ADD       CMR(R)        TOTRES            9 2
     C**
     C                   END
     C                   ADD       TOTPMT        EMPINC           11 2
     C                   ADD       TOTRES        EMPINC
     C                   ADD       TOTPMT        EMPPMT           11 2
     C                   ADD       TOTRES        EMPRES           11 2
     C*
     C     CMKEY         READE     PCM01                                  52
     C                   END
     C*
     C                   ADD       EMPCLM        L1TOTC            5 0
     C                   ADD       EMPPMT        L1ITOT           11 2
     C                   ADD       EMPRES        L1ITOT
     C                   ADD       EMPPMT        L1PTOT           11 2
     C                   ADD       EMPRES        L1RTOT           11 2
     C*** BEGIN CHG *** 12/02/96 **************************************
     C**********           ADD  ELANPR    L1ANPR 112
     C                   ADD       XXANPR        L1ANPR           11 2
     C*** END   CHG *** 12/02/96 **************************************
     C**
     C*  CALCULATE PREMIUM TO DATE
     C                   EXSR      SUBPR
     C                   ADD       PRMYTD        L1PREM           11 2
     C                   Z-ADD     FREQ          L1FREQ            4 2
     C***  BEGIN CHG  ***  12/16/99  *********************************
     C**********           Z-ADDLOSS      L1LOSS  62
     C                   Z-ADD     LOSS          L1LOSS            7 2
     C***  END   CHG  ***  12/16/99  *********************************
     C*
      ***** BEG ADD ***** 06/03/04 ************************************
      *
     C     XXACT         IFEQ      'N'
      *
     C                   ADD       1             ACTOT             5 0
     C                   ADD       ELANPR        ACANPR           11 2
     C                   ADD       PRMYTD        ACPREM           11 2
     C                   ADD       EMPCLM        ACTOTC            5 0
     C                   ADD       EMPRES        ACRTOT           11 2
     C                   ADD       EMPPMT        ACPTOT           11 2
     C                   ADD       EMPPMT        ACITOT           11 2
     C                   ADD       EMPRES        ACITOT
      *
     C                   ADD       1             FCTOT             5 0
     C                   ADD       ELANPR        FCANPR           11 2
     C                   ADD       PRMYTD        FCPREM           11 2
     C                   ADD       EMPCLM        FCTOTC            5 0
     C                   ADD       EMPRES        FCRTOT           11 2
     C                   ADD       EMPPMT        FCPTOT           11 2
     C                   ADD       EMPPMT        FCITOT           11 2
     C                   ADD       EMPRES        FCITOT
      *
     C                   ADD       1             SCTOT             5 0
     C                   ADD       ELANPR        SCANPR           11 2
     C                   ADD       PRMYTD        SCPREM           11 2
     C                   ADD       EMPCLM        SCTOTC            5 0
     C                   ADD       EMPRES        SCRTOT           11 2
     C                   ADD       EMPPMT        SCPTOT           11 2
     C                   ADD       EMPPMT        SCITOT           11 2
     C                   ADD       EMPRES        SCITOT
      *
      ***** BEG ADD ***** 01/08/10 ************************************
     C                   ADD       1             RCTOT             5 0
     C                   ADD       ELANPR        RCANPR           11 2
     C                   ADD       PRMYTD        RCPREM           11 2
     C                   ADD       EMPCLM        RCTOTC            5 0
     C                   ADD       EMPRES        RCRTOT           11 2
     C                   ADD       EMPPMT        RCPTOT           11 2
     C                   ADD       EMPPMT        RCITOT           11 2
     C                   ADD       EMPRES        RCITOT
      ***** END ADD ***** 01/08/10 ************************************
      *
     C                   ELSE
      *
     C                   ADD       1             AATOT             5 0
     C                   ADD       ELANPR        AAANPR           11 2
     C                   ADD       PRMYTD        AAPREM           11 2
     C                   ADD       EMPCLM        AATOTC            5 0
     C                   ADD       EMPRES        AARTOT           11 2
     C                   ADD       EMPPMT        AAPTOT           11 2
     C                   ADD       EMPPMT        AAITOT           11 2
     C                   ADD       EMPRES        AAITOT
      *
     C                   ADD       1             FATOT             5 0
     C                   ADD       ELANPR        FAANPR           11 2
     C                   ADD       PRMYTD        FAPREM           11 2
     C                   ADD       EMPCLM        FATOTC            5 0
     C                   ADD       EMPRES        FARTOT           11 2
     C                   ADD       EMPPMT        FAPTOT           11 2
     C                   ADD       EMPPMT        FAITOT           11 2
     C                   ADD       EMPRES        FAITOT
      *
     C                   ADD       1             SATOT             5 0
     C                   ADD       ELANPR        SAANPR           11 2
     C                   ADD       PRMYTD        SAPREM           11 2
     C                   ADD       EMPCLM        SATOTC            5 0
     C                   ADD       EMPRES        SARTOT           11 2
     C                   ADD       EMPPMT        SAPTOT           11 2
     C                   ADD       EMPPMT        SAITOT           11 2
     C                   ADD       EMPRES        SAITOT
      *
      ***** BEG ADD ***** 01/08/10 ************************************
     C                   ADD       1             RATOT             5 0
     C                   ADD       ELANPR        RAANPR           11 2
     C                   ADD       PRMYTD        RAPREM           11 2
     C                   ADD       EMPCLM        RATOTC            5 0
     C                   ADD       EMPRES        RARTOT           11 2
     C                   ADD       EMPPMT        RAPTOT           11 2
     C                   ADD       EMPPMT        RAITOT           11 2
     C                   ADD       EMPRES        RAITOT
      ***** END ADD ***** 01/08/10 ************************************
      *
     C                   END
      ***** END ADD ***** 06/03/04 ************************************
     C*
     C     *INOF         IFEQ      '1'
     C                   EXSR      SUBHD
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   MOVE      '0'           *INOF
     C                   END
     C**
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'P'
      ***** BEG CHG ***** 09/11/06 ***********************************
     C***********          EXSR FILE
     C                   EXSR      SRFILE
      ***** END CHG ***** 09/11/06 ***********************************
     C                   ENDIF
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C                   EXCEPT    DETAIL
      **** BEGIN ADD **** 04/16/02 ************************************
      *
      ***** BEG ADD ***** 11/08/04 ************************************
     C                   ENDIF
      ***** END ADD ***** 11/08/04 ************************************
      *
      ***** BEG ADD ***** 06/03/04 ************************************
     C                   CLEAR                   EFFDTE
     C                   CLEAR                   CANDTE
     C                   CLEAR                   ELANPR
     C                   CLEAR                   PRMYTD
     C                   CLEAR                   EMPCLM
     C                   CLEAR                   EMPRES
     C                   CLEAR                   EMPPMT
     C                   CLEAR                   EMPINC
     C                   CLEAR                   LOSS
     C                   CLEAR                   FREQ
     C                   CLEAR                   XXRATE
      *
      * Move ENDIF before the clear fields
      ***** BEG DEL ***** 11/08/04 ************************************
      ***** END ADD ***** 06/03/04 ************************************
     C*****************    ENDIF
      **** END   ADD **** 04/16/02 ************************************
      ***** END DEL ***** 11/08/04 ************************************
     C                   END
      *
      ***** BEG ADD ***** 06/03/04 ************************************
      *
     C     *INOA         IFEQ      '1'
     C                   ADD       1             PAGES
     C                   EXCEPT    HEAD4
     C                   MOVE      '0'           *INOA
     C                   END
      ***** END ADD ***** 06/03/04 ************************************
     C*
     C***  Begin Add  ***  08/24/05  **************************************
     C                   ENDIF
     C***  End   Add  ***  08/24/05  **************************************
     C**
     C**** BEG CHG ****** 01/08/10 ************************************
     C*        21          READ PAL01                    50
     C*        22          READ PAL02                    50
     C*        23          READ PAL03                    50
      ***** BEG ADD ***** 06/03/04 ************************************
     C*        24          READ PAL04                    50
      ***** END ADD ***** 06/03/04 ************************************
     C   21FNKEY         READE     PAL01                                  50
     C   22FNKEY         READE     PAL02                                  50
     C   23FNKEY         READE     PAL03                                  50
     C   24FNKEY         READE     PAL04                                  50
     C**** END CHG ****** 01/08/10 ************************************
     C                   END
     C**
     C**** BEG DEL ****** 01/08/10 ************************************
     C*                    MOVE '1'       *INLR
     C**** END DEL ****** 01/08/10 ************************************
     C**
     C**** BEG CHG ****** 01/08/10 **TAKE OUT LR CONDITION*************
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C   OF              EXCEPT    HEAD
     C   OF              EXCEPT    HEAD2
     C                   SETOFF                                       OF
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   EXSR      SUBL1
     C                   EXSR      SUBL2
     C                   EXSR      SSRT
     C*
     C*  EXECUTE FUND TOTAL SUBROUTINES
     C*
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
      **** BEGIN ADD **** 05/14/10 ************************************
     C******               EXCPTHEAD3
      **** END   ADD **** 05/14/10 ************************************
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   EXSR      SUBL3
      *
      **** END CHG **** 01/08/10 *******LR CONDITION*****************
      **** BEG ADD **** 01/08/10 ************************************
     C                   ENDSR
      **** END   ADD **** 01/08/10 ************************************
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   END
      *
      ***** BEG ADD ***** 06/03/04 ************************************
     C     QQOUTP        IFEQ      'P'
     C                   MOVE      *ON           *IN26
     C                   END
     C     QQOUTP        IFEQ      'F'
     C                   MOVE      *ON           *IN27
     C                   END
     C     QQOUTP        IFEQ      'B'
     C                   MOVE      *ON           *IN28
     C                   END
      ***** END ADD ***** 06/03/04 ************************************
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C*** BEGIN DEL *** 05/12/04 **************************************
     C**********           MOVE XXFYR     XXFYR2  20
     C*** END   DEL *** 05/12/04 **************************************
     C*
     C     COKEY         CHAIN     SMCOL                              60
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C* * * OPEN PRINT FILE
     C                   OPEN      QSYSPRT
     C                   OPEN      QSYSPRT1
      *
     C**** BEG ADD ****** 05/14/10 ************************************
     C                   MOVE      *BLANKS       XXTITS           60
      **** END   ADD **** 05/14/10 ************************************
      ***** BEG ADD ***** 06/03/04 ************************************
      *   CHAIN TO SUMMARY DISTRIBUTION
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      *
      * Left Justify Title in Field
      *
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      ORTITL
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVEL     ORDIS1        XXDIS1
     C                   MOVEL     ORDIS2        XXDIS2
     C                   MOVEL     ORDIS3        XXDIS3
     C                   MOVEL     ORDIS4        XXDIS4
      ***** BEG CHG ***** 05/14/10 ************************************
     C**                   MOVELORTITL    XXTITL    P
     C                   MOVEL(P)  ORTITL        XXTITS
      ***** END CHG ***** 05/14/10 ************************************
     C                   MOVEL     ORPNAM        XXPNAM
     C                   MOVEL     ORFIL#        XXFIL#
     C                   Z-ADD     ORCOPY        XXCOPY
     C                   MOVEL     ORHOLD        XXHOLD
     C                   MOVEL     OROUTQ        XXOUTQ
     C                   MOVEL     ORFORM        XXFORM
     C                   MOVEL     ORSAVE        XXSAVE
      *
     C                   MOVE      *BLANKS       ORDIS1
     C                   MOVE      *BLANKS       ORDIS2
     C                   MOVE      *BLANKS       ORDIS3
     C                   MOVE      *BLANKS       ORDIS4
     C                   MOVE      *BLANKS       ORTITL
     C                   MOVE      *BLANKS       ORPNAM
     C                   MOVE      *BLANK        ORFIL#
     C                   Z-ADD     *ZEROS        ORCOPY
     C                   MOVE      *BLANKS       ORHOLD
     C                   MOVE      *BLANKS       OROUTQ
     C                   MOVE      *BLANKS       ORFORM
     C                   MOVE      *BLANKS       ORSAVE
     C                   ENDIF
      ***** END ADD ***** 06/03/04 ************************************
      *
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C***** BEGIN ADD **** 04/30/99 ***********************************
     C*  Left justify title
     C*
     C                   MOVE      *BLANKS       XXTITL           60
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      XXTITL
     C                   ENDIF
     C                   ENDIF
     C***** END ADD ****** 04/30/99 ***********************************
     C*   PRINT THE DISTRIBUTION INFORMATION
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C**** BEG ADD ****** 01/08/10 ************************************
     C                   EXSR      LDGRP1
     C                   EXCEPT    ROUT12
     C**** END ADD ****** 01/08/10 ************************************
      *
      ***** BEG ADD ***** 06/03/04 ************************************
     C                   Z-ADD     0             PAGES             3 0
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE2
      ***** End Del ***** 08/26/14 ************************************
      ***** END ADD ***** 06/03/04 ************************************
     C**** BEG ADD ****** 01/08/10 ************************************
     C                   EXSR      LDGRP2
     C                   EXCEPT    ROU112
     C**** END ADD ****** 01/08/10 ************************************
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   END
     C*
     C***** BEGIN ADD **** 04/30/99 ***********************************
     C*   CENTER TEXT IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C                   CALL      'SRCENTER'                             99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        XXTITL
     C*
     C***** END ADD ****** 04/30/99 ***********************************
     C                   MOVE      '1'           *IN11
     C*
     C                   ENDSR
     C*
     C**
     C*==================================================================*
     C*   Premium Calculations             ** Subroutine **   S U B P R  *
     C*==================================================================*
     C*
     CSR   SUBPR         BEGSR
     C**
     C**    CALL PGM TO CALCULATE EARNED PREMIUM YEAR-TO-DATE
     C**                      AND EARNED PREMIUM THIS PERIOD
     C**
     C                   MOVEL     XXCO#         SRCO#             3
     C                   MOVEL     XXFND         SRFND             3
     C                   MOVEL     XXFYR         SRFYR             3
     C*** BEGIN CHG ***  04/14/98  ************************************
     C******               MOVELXXEMP#    SREMP#  5
     C******               MOVELXXDIV     SRDIV   3
     C                   MOVEL     XXEMP#        SREMP#            9
     C                   MOVEL     XXDIV         SRDIV             5
     C*** END CHG *****  04/14/98  ************************************
     C                   MOVE      *BLANK        SRAMT            11
     C                   MOVE      *BLANK        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRPRT1'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    QQDTE
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    QQTERM
     C                   MOVE      SRAMT         PRMYTD           11 2
     C                   MOVE      SRAMT2        PRMCUR           11 2
     C*
     C**   CALCULATE LOSS RATIO
     C     PRMYTD        IFNE      *ZERO
     C     EMPINC        MULT      100           XXITOT           13 2
     C***  BEGIN CHG  ***  12/16/99  *********************************
     C********** XXITOT    DIV  PRMYTD    LOSS    62H
     C     XXITOT        DIV(H)    PRMYTD        LOSS              7 2
     C***  END   CHG  ***  12/16/99  *********************************
     C                   ELSE
     C                   Z-ADD     *ZERO         LOSS
     C                   END
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     PRMYTD        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     EMPCLM        DIV(H)    THPREM        FREQ              4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         FREQ
     C                   END
     C*
     C                   ENDSR
     C**
     C*==================================================================*
     C*   Level 1 Calculations (PRODUCER)  ** Subroutine **   S U B L 1  *
     C*==================================================================*
     C*
     CSR   SUBL1         BEGSR
     C**
     C                   ADD       L1TOT         ATOT              5 0
     C                   ADD       L1PTOT        L2PTOT           11 2
     C                   ADD       L1RTOT        L2RTOT           11 2
     C                   ADD       L1PREM        L2PREM           11 2
     C                   ADD       L1TOTC        L2TOTC            5 0
     C                   ADD       L1ANPR        L2ANPR           11 2
     C**
     C                   ADD       L1ITOT        L2ITOT           11 2
     C**
     C**   CALCULATE LOSS RATIO
     C     L1PREM        IFNE      *ZERO
     C     L1ITOT        MULT      100           XXITOT
     C***  BEGIN CHG  ***  12/16/99  *********************************
     C********** XXITOT    DIV  L1PREM    L1LOSS  62H
     C     XXITOT        DIV(H)    L1PREM        L1LOSS            7 2
     C***  END   CHG  ***  12/16/99  *********************************
     C                   ELSE
     C                   Z-ADD     *ZERO         L1LOSS
     C                   END
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     L1PREM        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L1TOTC        DIV(H)    THPREM        L1FREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L1FREQ
     C                   END
     C**
     C     *INOF         IFEQ      '1'
     C*  GET FUND NAME
     C     FNKEY         CHAIN     PFN01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C                   ELSE
     C                   MOVE      *BLANK        XFNAME
     C                   END
     C*
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C                   EXCEPT    HEAD
      **** BEGIN DEL **** 10/20/09 ************************************
     C************         EXCPTHEAD2
      **** END   DEL **** 10/20/09 ************************************
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   END
     C*
     C                   EXSR      SUBAP
     C*
     C                   SETOFF                                       OF
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
      ***** BEG CHG ***** 01/08/10 ************************************
     C     L1TOT         IFNE      0
     C                   EXCEPT    TOTL1
     C                   ENDIF
      ***** END CHG ***** 01/08/10 ************************************
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   ENDSR
     C**
     C*==================================================================*
     C*   Level 2 Calculations (AGENT)     ** Subroutine **   S U B L 2  *
     C*==================================================================*
     C*
     CSR   SUBL2         BEGSR
     C**
     C                   ADD       ATOT          STOT              5 0
     C                   ADD       L2PTOT        STPTOT           11 2
     C                   ADD       L2RTOT        STRTOT           11 2
     C                   ADD       L2PREM        STPREM           11 2
     C                   ADD       L2TOTC        STTOTC            5 0
     C                   ADD       L2ITOT        STITOT           11 2
     C                   ADD       L2ANPR        STANPR           11 2
     C*
     C**   CALCULATE LOSS RATIO
     C     L2PREM        IFNE      *ZERO
     C     L2ITOT        MULT      100           XXITOT
     C***  BEGIN CHG  ***  12/16/99  *********************************
     C********** XXITOT    DIV  L2PREM    L2LOSS  62H
     C     XXITOT        DIV(H)    L2PREM        L2LOSS            7 2
     C***  END   CHG  ***  12/16/99  *********************************
     C                   ELSE
     C                   Z-ADD     *ZERO         L2LOSS
     C                   END
      *
      ***** BEG ADD ***** 06/03/04 ************************************
      *
      *    CALCULATE ACTIVE LOSS RATIO
      *
     C     AAPREM        IFNE      *ZERO
     C     AAITOT        MULT      100           XXITOT
     C     XXITOT        DIV(H)    AAPREM        AALOSS            6 2
     C                   ELSE
     C                   Z-ADD     *ZERO         AALOSS
     C                   END
      *
      *    CALCULATE CANCELLED LOSS RATIO
      *
     C     ACPREM        IFNE      *ZERO
     C     ACITOT        MULT      100           XXITOT
     C     XXITOT        DIV(H)    ACPREM        ACLOSS            6 2
     C                   ELSE
     C                   Z-ADD     *ZERO         ACLOSS
     C                   END
      ***** END ADD ***** 06/03/04 ************************************
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     L2PREM        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L2TOTC        DIV(H)    THPREM        L2FREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L2FREQ
     C                   END
      *
      ***** BEG ADD ***** 06/03/04 ************************************
      *
      *    CALCULATE ACTIVE FREQUENCY RATIO
      *
     C     AAPREM        DIV(H)    1000          THPREM            9 2
      *
     C     THPREM        IFNE      *ZERO
     C     AATOTC        DIV(H)    THPREM        AAFREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         AAFREQ
     C                   END
      *
      *    CALCULATE CANCELLED FREQUENCY RATIO
      *
     C     ACPREM        DIV(H)    1000          THPREM            9 2
      *
     C     THPREM        IFNE      *ZERO
     C     ACTOTC        DIV(H)    THPREM        ACFREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         ACFREQ
     C                   END
      ***** END ADD ***** 06/03/04 ************************************
     C*
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C   OF              EXCEPT    HEAD
     C   OF              EXCEPT    HEAD2
     C                   SETOFF                                       OF
      ***** BEG CHG ***** 01/08/10 ************************************
     C     AATOT         IFNE      0
     C     ACTOT         ORNE      0
     C                   EXCEPT    TOTL2
     C                   END
      ***** END CHG ***** 01/08/10 ************************************
      ***** BEG ADD ***** 06/03/04 ************************************
     C   OA              EXCEPT    HEAD4
     C                   SETOFF                                       OA
      ***** BEG CHG ***** 01/10/08 ************************************
     C     AATOT         IFNE      0
     C     ACTOT         ORNE      0
     C                   EXCEPT    DET1
     C                   END
      ***** END CHG ***** 01/10/08 ************************************
      ***** END ADD ***** 06/03/04 ************************************
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
     C                   Z-ADD     0             PAGE
     C*
     C                   ENDSR
     C**
     C*==================================================================*
     C*   SORT SLECTION BREAK              ** Subroutine **   S S R T    *
     C*==================================================================*
     C*
     CSR   SSRT          BEGSR
     C**
     C                   ADD       STOT          FTOT              5 0
     C                   ADD       STPTOT        L3PTOT           11 2
     C                   ADD       STRTOT        L3RTOT           11 2
     C                   ADD       STPREM        L3PREM           11 2
     C                   ADD       STTOTC        L3TOTC            5 0
     C                   ADD       STITOT        L3ITOT           11 2
     C                   ADD       STANPR        L3ANPR           11 2
     C*
      ***** BEG ADD ***** 06/03/04 ************************************
      *
      * If sort is AGENT, already in agent order, so bypass printing
      * of Sort Totals and clear amts
      *
     C     *IN94         IFEQ      *ON
      *
     C                   Z-ADD     *ZEROS        STOT
     C                   Z-ADD     *ZEROS        STPTOT
     C                   Z-ADD     *ZEROS        STRTOT
     C                   Z-ADD     *ZEROS        STPREM
     C                   Z-ADD     *ZEROS        STTOTC
     C                   Z-ADD     *ZEROS        STITOT
     C                   Z-ADD     *ZEROS        STANPR
      *
     C                   Z-ADD     *ZEROS        SATOT
     C                   Z-ADD     *ZEROS        SAANPR
     C                   Z-ADD     *ZEROS        SAPREM
     C                   Z-ADD     *ZEROS        SATOTC
     C                   Z-ADD     *ZEROS        SARTOT
     C                   Z-ADD     *ZEROS        SAPTOT
     C                   Z-ADD     *ZEROS        SAITOT
      *
     C                   Z-ADD     *ZEROS        SCTOT
     C                   Z-ADD     *ZEROS        SCANPR
     C                   Z-ADD     *ZEROS        SCPREM
     C                   Z-ADD     *ZEROS        SCTOTC
     C                   Z-ADD     *ZEROS        SCRTOT
     C                   Z-ADD     *ZEROS        SCPTOT
     C                   Z-ADD     *ZEROS        SCITOT
      *
     C                   ELSE
      *
      ***** END ADD ***** 06/03/04 ************************************
     C*
     C**   CALCULATE LOSS RATIO
     C     STPREM        IFNE      *ZERO
     C     STITOT        MULT      100           XXITOT
     C***  BEGIN CHG  ***  12/16/99  *********************************
     C********** XXITOT    DIV  STPREM    STLOSS  62H
     C     XXITOT        DIV(H)    STPREM        STLOSS            7 2
     C***  END   CHG  ***  12/16/99  *********************************
     C                   ELSE
     C                   Z-ADD     *ZERO         STLOSS
     C                   END
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     STPREM        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     STTOTC        DIV(H)    THPREM        STFREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         STFREQ
     C                   END
      *
      ***** BEG ADD ***** 06/03/04 ************************************
      *
      *    CALCULATE ACTIVE LOSS RATIO
      *
     C     SAPREM        IFNE      *ZERO
     C     SAITOT        MULT      100           XXITOT
     C     XXITOT        DIV(H)    SAPREM        SALOSS            6 2
     C                   ELSE
     C                   Z-ADD     *ZERO         SALOSS
     C                   END
      *
      *    CALCULATE CANCELLED LOSS RATIO
      *
     C     SCPREM        IFNE      *ZERO
     C     SCITOT        MULT      100           XXITOT
     C     XXITOT        DIV(H)    SCPREM        SCLOSS            6 2
     C                   ELSE
     C                   Z-ADD     *ZERO         SCLOSS
     C                   END
      *
      *    CALCULATE ACTIVE FREQUENCY RATIO
      *
     C     SAPREM        DIV(H)    1000          THPREM            9 2
      *
     C     THPREM        IFNE      *ZERO
     C     SATOTC        DIV(H)    THPREM        SAFREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         SAFREQ
     C                   END
      *
      *    CALCULATE CANCELLED FREQUENCY RATIO
      *
     C     SCPREM        DIV(H)    1000          THPREM            9 2
      *
     C     THPREM        IFNE      *ZERO
     C     SCTOTC        DIV(H)    THPREM        SCFREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         SCFREQ
     C                   END
      *
      ***** END ADD ***** 06/03/04 ************************************
     C*
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
     C                   EXCEPT    HEAD3
     C                   SETOFF                                       OF
     C                   EXCEPT    TOTST
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
      *
      ***** BEG ADD ***** 06/03/04 ************************************
     C                   ENDIF
      ***** END ADD ***** 06/03/04 ************************************
      *
     C                   Z-ADD     0             PAGE
     C*
     C*  CHANGED TO CORRECT THE PRINTING OF HEADINGS AT OVERFLOW WHEN
     C*  PRINTING TOTALS
     C*
     C                   EXSR      SUBHD
     C                   ENDSR
     C**
     C*==================================================================*
     C*   Level 3 Calculations (FUND)      ** Subroutine **   S U B L 3  *
     C*==================================================================*
     C*
     CSR   SUBL3         BEGSR
     C**
     C                   ADD       FTOT          RTOT              5 0
     C                   ADD       L3PTOT        L4PTOT           11 2
     C                   ADD       L3RTOT        L4RTOT           11 2
     C                   ADD       L3PREM        L4PREM           11 2
     C                   ADD       L3TOTC        L4TOTC            5 0
     C                   ADD       L3ITOT        L4ITOT           11 2
     C                   ADD       L3ANPR        L4ANPR           11 2
     C*
     C*
     C**   CALCULATE LOSS RATIO
     C     L3PREM        IFNE      *ZERO
     C     L3ITOT        MULT      100           XXITOT
     C***  BEGIN CHG  ***  12/16/99  *********************************
     C********** XXITOT    DIV  L3PREM    L3LOSS  62H
     C     XXITOT        DIV(H)    L3PREM        L3LOSS            7 2
     C***  END   CHG  ***  12/16/99  *********************************
     C                   ELSE
     C                   Z-ADD     *ZERO         L3LOSS
     C                   END
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     L3PREM        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L3TOTC        DIV(H)    THPREM        L3FREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L3FREQ
     C                   END
      *
      ***** BEG ADD ***** 06/03/04 ************************************
      *
      *    CALCULATE ACTIVE LOSS RATIO
      *
     C     FAPREM        IFNE      *ZERO
     C     FAITOT        MULT      100           XXITOT
     C     XXITOT        DIV(H)    FAPREM        FALOSS            6 2
     C                   ELSE
     C                   Z-ADD     *ZERO         FALOSS
     C                   END
      *
      *    CALCULATE CANCELLED LOSS RATIO
      *
     C     FCPREM        IFNE      *ZERO
     C     FCITOT        MULT      100           XXITOT
      ***** BEG CHG ***** 01/08/10 ************************************
     C*          XXITOT    DIV  L3PREM    FCLOSS  62H
     C     XXITOT        DIV(H)    FCPREM        FCLOSS            6 2
      ***** END CHG ***** 01/08/10 ************************************
     C                   ELSE
     C                   Z-ADD     *ZERO         FCLOSS
     C                   END
      *
      *    CALCULATE ACTIVE FREQUENCY RATIO
      *
     C     FAPREM        DIV(H)    1000          THPREM            9 2
      *
     C     THPREM        IFNE      *ZERO
     C     FATOTC        DIV(H)    THPREM        FAFREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         FAFREQ
     C                   END
      *
      *    CALCULATE CANCELLED FREQUENCY RATIO
      *
     C     FCPREM        DIV(H)    1000          THPREM            9 2
      *
     C     THPREM        IFNE      *ZERO
     C     FCTOTC        DIV(H)    THPREM        FCFREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         FCFREQ
     C                   END
      *
      ***** END ADD ***** 06/03/04 ************************************
     C*
      **** BEGIN ADD **** 04/16/02 ************************************
     C     QQOUTP        IFNE      'F'
      **** END   ADD **** 04/16/02 ************************************
      *
      ***** BEG ADD ***** 06/03/04 ************************************
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD4
      ***** BEG CHG ***** 01/08/10 ************************************
     C     FTOT          IFNE      0
     C                   EXCEPT    TOTAL
     C                   ENDIF
     C                   ENDIF
     C     FTOT          IFNE      0
     C                   EXCEPT    TOTAL
     C                   ENDIF
      ***** END CHG ***** 01/08/10 ************************************
      ***** END ADD ***** 06/03/04 ************************************
      ***** BEG ADD ***** 05/14/10 ************************************
     C                   MOVE      *ON           TOT3IN
     C***                  EXCPTTOTL3
      ***** END ADD ***** 05/14/10 ************************************
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   ENDIF
      **** END   ADD **** 04/16/02 ************************************
      **** BEG DEL ****** 01/08/10 ************************************
      ***** BEG ADD ***** 05/14/10 ************************************
     C                   MOVE      *ON           SUB4ID
     C                   ENDSR
      ***** BEG ADD ***** 05/14/10 ************************************
     C     SUB4          BEGSR
     C*                    Z-ADD0         PAGE
      **** END DEL ****** 01/08/10 ************************************
      **** BEG ADD **** 01/08/10 ************************************
     C                   Z-ADD     *ZERO         FTOT
     C                   Z-ADD     *ZERO         L3PTOT
     C                   Z-ADD     *ZERO         L3RTOT
     C                   Z-ADD     *ZERO         L3PREM
     C                   Z-ADD     *ZERO         L3TOTC
     C                   Z-ADD     *ZERO         L3ITOT
     C                   Z-ADD     *ZERO         L3ANPR
     C                   Z-ADD     *ZERO         FCTOT
     C                   Z-ADD     *ZERO         FCANPR
     C                   Z-ADD     *ZERO         FCPREM
     C                   Z-ADD     *ZERO         FCTOTC
     C                   Z-ADD     *ZERO         FCRTOT
     C                   Z-ADD     *ZERO         FCPTOT
     C                   Z-ADD     *ZERO         FCITOT
     C                   Z-ADD     *ZERO         FCITOT
     C                   Z-ADD     *ZERO         FATOT
     C                   Z-ADD     *ZERO         FAANPR
     C                   Z-ADD     *ZERO         FAPREM
     C                   Z-ADD     *ZERO         FATOTC
     C                   Z-ADD     *ZERO         FARTOT
     C                   Z-ADD     *ZERO         FAPTOT
     C                   Z-ADD     *ZERO         FAITOT
     C                   Z-ADD     *ZERO         FAITOT
     C                   SETON                                        OA
      **** END ADD **** 01/08/10 ************************************
     C                   ENDSR
     C**
      **** BEG ADD **** 01/08/10 ************************************
     C*==================================================================*
     C*   Level 4 Calculations (REPORT TOT)** Subroutine **   S U B L 4  *
     C*==================================================================*
     C*
     CSR   SUBL4         BEGSR
     C*
     C**   CALCULATE LOSS RATIO
     C     L4PREM        IFNE      *ZERO
     C     L4ITOT        MULT      100           XXITOT
     C     XXITOT        DIV(H)    L4PREM        L4LOSS            7 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L4LOSS
     C                   END
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     L4PREM        DIV(H)    1000          THPREM            9 2
     C*
     C     THPREM        IFNE      *ZERO
     C     L4TOTC        DIV(H)    THPREM        L4FREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         L4FREQ
     C                   END
      *
      *    CALCULATE ACTIVE LOSS RATIO
      *
     C     RAPREM        IFNE      *ZERO
     C     RAITOT        MULT      100           XXITOT
     C     XXITOT        DIV(H)    RAPREM        RALOSS            6 2
     C                   ELSE
     C                   Z-ADD     *ZERO         RALOSS
     C                   END
      *
      *    CALCULATE CANCELLED LOSS RATIO
      *
     C     RCPREM        IFNE      *ZERO
     C     RCITOT        MULT      100           XXITOT
     C     XXITOT        DIV(H)    L4PREM        RCLOSS            6 2
     C                   ELSE
     C                   Z-ADD     *ZERO         RCLOSS
     C                   END
      *
      *    CALCULATE ACTIVE FREQUENCY RATIO
      *
     C     RAPREM        DIV(H)    1000          THPREM            9 2
      *
     C     THPREM        IFNE      *ZERO
     C     RATOTC        DIV(H)    THPREM        RAFREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         RAFREQ
     C                   END
      *
      *    CALCULATE CANCELLED FREQUENCY RATIO
      *
     C     RCPREM        DIV(H)    1000          THPREM            9 2
      *
     C     THPREM        IFNE      *ZERO
     C     RCTOTC        DIV(H)    THPREM        RCFREQ            4 2
     C                   ELSE
     C                   Z-ADD     *ZERO         RCFREQ
     C                   END
      *
     C     QQOUTP        IFNE      'F'
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD4
     C*                    EXCPTTOTAL
     C*                    EXCPTTOTL4
     C                   ENDIF
     C*                    EXCPTTOTAL
     C                   EXCEPT    TOTL4
     C                   EXCEPT    TOT41
     C                   ENDIF
     C                   Z-ADD     0             PAGE
     C                   ENDSR
     C**
     C*==================================================================*
     C*   Calculations for Headings        ** Subroutine **   S U B H D  *
     C*==================================================================*
     C*
     CSR   SUBHD         BEGSR
     C*
     C*   GET THE DESCRIPTION FOR THE SELECTED CODE
     C*
     C                   MOVE      *BLANKS       XXDESC           25
      *
      ***** BEG ADD ***** 06/03/04 ************************************
     C                   MOVE      *BLANKS       XXARAD           25
     C     AEKEY         CHAIN     PAE01                              80
     C     *IN80         IFEQ      '0'
     C                   MOVEL     AEDESC        XXARAD
     C   22              MOVEL     AEDESC        XXDESC
     C                   END
      ***** END ADD ***** 06/03/04 ************************************
      *
     C     *IN21         IFEQ      '1'
     C     CPKEY         CHAIN     PCP01                              80
     C     *IN80         IFEQ      '0'
     C                   MOVEL     CPDESC        XXDESC
     C                   END
     C                   ELSE
      ***** BEG CHG ***** 06/03/04 ************************************
     C********** *IN22     IFEQ '1'
     C********** AEKEY     CHAINPAE01                80
     C********** *IN80     IFEQ '0'
     C**********           MOVELAEDESC    XXDESC
     C**********           END
     C**********           ELSE
     C     *IN23         IFEQ      '1'
      ***** END CHG ***** 06/03/04 ************************************
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ALCNTY        XXKEY
     C     TBKEY         CHAIN     PCN01                              80
     C     *IN80         IFEQ      '0'
     C                   MOVEL     CNDESC        XXDESC
     C*** BEGIN ADD *** 06/26/97 **************************************
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       XXDESC
     C*** END   ADD *** 06/26/97 **************************************
     C                   END
     C                   END
     C                   END
     C*
     C**  GET AGENT NAME VIA AGENT MASTER FILES
     C                   Z-ADD     ALAGT#        XXAGT#
     C**
     C     AAKEY         CHAIN     PAA01                              80
     C                   Z-ADD     AAFEIN        XXFEIN
     C                   Z-ADD     AAMOD         XXMOD
     C*
      ***** BEG ADD ***** 09/11/06 ***********************************
      *  Save the Agent Cancel Date
     C                   Z-ADD     AACNDT        PPAGCD
      ***** END ADD ***** 09/11/06 ***********************************
     C**
     C     AGKEY         CHAIN     PAG01                              80
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C                   MOVEL     AGMCTY        XXLAST           56
     C                   MOVEL     AGMST         XXSTZP           13
     C                   MOVE      AGMZIP        XXSTZP
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C                   MOVEL     AGMZIP        XXST10           10
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   END
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        AGCS             40
     C*
     C                   Z-ADD     ALPROD        XXPROD
     C*
     C     APKEY         CHAIN     PAP01                              80
     C     *IN80         IFEQ      '0'
     C*
     C**    CALL PGM TO CONCATENATE PRODUCER NAME
     C**
     C                   MOVEL     APLSN         SRLNAM           56
     C                   MOVEL     APFSN         SRFNAM           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     SRLNAM        PNAME            40
      *
      ***** BEG ADD ***** 07/15/04 ************************************
      *
     C                   ELSE
     C                   CLEAR                   PNAME
      ***** END ADD ***** 07/15/04 ************************************
      *
     C                   END
     C                   ENDSR
     C**
     C*==================================================================*
     C*   WRITE TO AGT PRODUCER WORK FILE  ** Subroutine **   S U B A P  *
     C*==================================================================*
     C*
     C     SUBAP         BEGSR
     C**
     C                   Z-ADD     XXCO#         APCO#
     C                   Z-ADD     XXFND         APFND
     C                   Z-ADD     XXFYR         APFYR
     C                   Z-ADD     SVAGNT        APAGT#
     C                   Z-ADD     SVPROD        APPROD
     C                   Z-ADD     L1TOT         APCNT
     C                   Z-ADD     L1ANPR        APWRPR
     C                   Z-ADD     L1PREM        APEAPR
     C                   MOVE      XXYMD         APEPDT
     C                   Z-ADD     L1TOTC        APCLMS
     C                   Z-ADD     L1RTOT        APRSVS
     C                   Z-ADD     L1PTOT        APPAID
     C                   Z-ADD     L1ITOT        APINCR
     C                   Z-ADD     L1LOSS        APLR
     C                   Z-ADD     L1FREQ        APFR
     C                   MOVEL     AGNAME        APNAME
     C                   MOVEL     APLSN         APPLSN
     C                   MOVEL     APFSN         APPFSN
     C                   MOVEL     AGMAD1        APMAD1
     C                   MOVEL     AGMCTY        APMCTY
     C                   MOVE      AGMST         APMST
     C                   MOVEL     AGMZIP        APMZP1
     C                   MOVE      AGMZIP        APMZP2
     C                   MOVE      AGARE1        APARE1
     C                   MOVEL     AGPHN1        APPHN1
     C                   MOVE      AGPHN1        APPHN2
     C                   MOVE      AACMFL        APCMFL
     C                   MOVE      AAPRFL        APPRFL
     C                   MOVE      AAMLFL        APMLFL
     C                   MOVE      AACHAP        APCHAP
     C                   MOVE      AGCNTY        APCNTY
     C                   MOVEL     Q1USER        APUSER
     C                   MOVEL     Q1PGM         APPGM
     C                   Z-ADD     DSTIME        APTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         APDT
     C*** BEGIN ADD *** 05/12/04 **************************************
     C                   Z-ADD     ELPFDT        APPFDT
     C                   Z-ADD     ELPTDT        APPTDT
     C*** END   ADD *** 05/12/04 **************************************
     C*
     C                   WRITE     PAP02
     C*
     C                   ENDSR
     C**
      **** BEGIN ADD *** 04/16/02 *************************************
     C*==================================================================*
     C*   Write records to workfile     ** Subroutine ** S R F I L E  *
     C*==================================================================*
     C*
      ***** BEG CHG ***** 09/11/06 ***********************************
     C*********  FILE      BEGSR
     CSR   SRFILE        BEGSR
      ***** END CHG ***** 09/11/06 ***********************************
     C*
     C                   Z-ADD     XXCO#         PPCO#
     C*
      ***** BEG CHG ***** 03/31/05 ************************************
     C***************      MOVELXXCONM    PPCNAM
     C                   MOVEL(P)  ZZCONM        PPCNAM
      ***** END CHG ***** 03/31/05 ************************************
      *
     C                   Z-ADD     XXFND         PPFND
     C                   MOVEL     XFNAME        PPFNAM
      *
     C                   Z-ADD     SVAGNT        PPAGT#
     C                   MOVEL     AGNAME        PPANAM
     C                   Z-ADD     ELFYR         PPFYR
      *
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         PPPFDT
      *
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         PPPTDT
      *
     C                   Z-ADD     SVCHAP        PPACH#
     C                   Z-ADD     SVCHAP        XXCHAP
     C     CPKY1         CHAIN     PCP01                              60
     C  N60              MOVEL(P)  CPDESC        PPACHD
     C   60              MOVE      *BLANKS       PPACHD
     C                   MOVEL     AGMAD1        PPAAD1
     C                   MOVEL     AGCS          PPACSZ
      *
      ***** BEG DEL ***** 09/11/06 ***********************************
      * Get agent effective date
      ***** BEG CHG ***** 06/30/06 ***********************************
     C**********           Z-ADDAHEFDT    PPAEFD
     C**********           Z-ADDAACNDT    PPAGCD
     C     PPAEFD        IFEQ      *ZEROS
     C     AHEFDT        ANDGT     *ZEROS
      **** BEGIN CHG **** 10/20/09 ************************************
     C**********           Z-ADDAHEFDT    PPAEFD
     C                   Z-ADD     AHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PPAEFD
      **** END   CHG **** 10/20/09 ************************************
     C                   ENDIF
      ***** END CHG ***** 09/11/06 ***********************************
      ***** END ADD ***** 06/30/06 ***********************************
      *
      ***** BEG ADD ***** 06/16/06 ************************************
     C     PPANAM        IFEQ      *BLANKS
     C                   Z-ADD     SVAGNT        XXAGT#
     C     AAKEY         CHAIN     PAA01                              80
     C                   Z-ADD     AAFEIN        XXFEIN
     C                   Z-ADD     AAMOD         XXMOD
      ***** BEG ADD ***** 06/30/06 ***********************************
      ***** BEG CHG ***** 09/11/06 ***********************************
     C*************        Z-ADDAACNDT    PPAGCD
     C     AACNDT        IFGT      *ZEROS
     C     PPAGCD        ANDEQ     *ZEROS
     C                   Z-ADD     AACNDT        PPAGCD
     C                   ENDIF
      ***** END CHG ***** 09/11/06 ***********************************
      ***** END ADD ***** 06/30/06 ***********************************
     C*
     C     AGKEY         CHAIN     PAG01                              80
     C*
     C     *IN80         IFEQ      *OFF
     C                   MOVEL(P)  AGNAME        PPANAM
     C                   MOVEL     AGMAD1        PPAAD1
     C                   MOVEL(P)  AGMCTY        XXACSZ
     C                   CAT       ',':0         XXACSZ
     C     XXACSZ        CAT       AGMST:1       XXACSZ
     C                   MOVE(P)   AGMZIP        XXA9              9
     C                   MOVE(P)   XXA9          XXA5              5
     C                   MOVE(P)   XXA9          XXA4              4
     C                   MOVEL(P)  XXA9          XXAA5             5
     C     XXACSZ        CAT       XXAA5:1       XXACSZ
     C*
     C     XXA5          IFEQ      *ZERO
     C                   ELSE
     C                   MOVEL     '-'           XXA5
     C                   MOVE      XXA4          XXA5              5
     C     XXACSZ        CAT       XXA5:0        XXACSZ
     C                   ENDIF
     C                   MOVE      XXACSZ        PPACSZ
     C                   ENDIF
     C                   ENDIF
      *
     C     PPACSZ        IFEQ      *BLANKS
     C                   MOVE      XXACSZ        PPACSZ
     C                   ENDIF
      ***** END ADD ***** 06/16/06 ************************************
      *
     C                   Z-ADD     SVAREA        PPAREA
     C                   MOVEL     SVCNTY        PPCNTY
      **** BEGIN ADD **** 10/20/09 ************************************
     C                   MOVEL     SVCNTY        XXKEY
     C     TBKEY         CHAIN     PCN02                              80
      **** END   ADD **** 10/20/09 ************************************
      **** BEGIN CHG **** 10/20/09 ************************************
     C  N80              MOVEL     CNDESC        PPCTYD
     C****************     MOVELXXDESC    PPCTYD
      **** END   CHG **** 10/20/09 ************************************
     C                   Z-ADD     AGARE1        PPACOD
     C                   Z-ADD     AGPHN1        PPPHN#
      *
     C                   Z-ADD     ALEMP#        PPEMP#
     C                   Z-ADD     ALDIV         PPDIV
     C                   MOVEL     ENAME         PPENAM
     C                   Z-ADD     EMCHAP        PPECH#
     C                   Z-ADD     EMCHAP        XXCHAP
      *
     C     CPKY1         CHAIN     PCP01                              60
     C  N60              MOVEL(P)  CPDESC        PPECHD
     C   60              MOVE      *BLANKS       PPECHD
      *
     C                   Z-ADD     ALPROD        PPPROD
     C                   MOVEL     PNAME         PPPNAM
      *
      ***** BEG ADD ***** 06/16/06 ************************************
     C     PPPNAM        IFEQ      *BLANKS
     C                   Z-ADD     ALPROD        XXPROD
     C*
     C     APKEY         CHAIN     PAP01                              80
     C     *IN80         IFEQ      '0'
     C                   MOVEL(P)  APLSN         PPPNAM
     C     APFSN         IFNE      *BLANKS
     C                   CAT       ',':0         PPPNAM
     C                   CAT       APFSN:1       PPPNAM
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      ***** END ADD ***** 06/16/06 ************************************
      *
     C                   Z-ADD     EFFDTE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         PPEFDT
     C                   Z-ADD     XXANPR        PPWPRM
     C                   Z-ADD     PRMYTD        PPEPRM
     C                   Z-ADD     EMPCLM        PPCLMS
     C                   Z-ADD     EMPRES        PPRESV
     C                   Z-ADD     EMPPMT        PPPAID
     C                   Z-ADD     EMPINC        PPINCR
     C                   Z-ADD     LOSS          PPLOSR
     C                   Z-ADD     FREQ          PPFRQR
     C                   Z-ADD     XXRATE        PPRATE
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         PPDATE
     C                   Z-ADD     DSTIME        PPTIME
     C                   MOVEL     Q1PGM         PPPGM
     C                   MOVEL     Q1USER        PPUSER
      *
      ***** BEG ADD ****** 06/16/04 ***********************************
      * ADD new fields to WFPPP file - Cancel Date & Area Name
     C                   Z-ADD     CANDTE        PPCXDT
     C                   MOVEL     XXARAD        PPARNM
      ***** END ADD ****** 06/16/04 ***********************************
      *
      ***** BEG ADD ****** 04/03/06 ***********************************
      *
      * Clear All the New fields Added to WFPPP file before any adds
     C                   MOVE      *BLANKS       PPSNAM
     C                   Z-ADD     *ZEROS        PPSFIN
     C                   Z-ADD     *ZEROS        PPSMOD
     C                   Z-ADD     *ZEROS        PPSNWR
     C                   MOVE      *BLANKS       PPSDES
     C                   Z-ADD     *ZEROS        PPSRNR
     C                   Z-ADD     *ZEROS        XXPROD
      *
     C                   MOVE      *BLANKS       PPSLSN
     C                   MOVE      *BLANKS       PPSFSN
     C                   MOVE      *BLANKS       PPSPLN
     C                   MOVE      *BLANKS       PPSADR
     C                   MOVE      *BLANKS       PPSCTY
     C                   MOVE      *BLANKS       PPSST
     C                   Z-ADD     *ZEROS        PPSZIP
      *
      ****** BEG DEL ***** 06/14/06 **************************************
     C**************       Z-ADD*ZEROS    PPFIDT
     C**************       Z-ADD*ZEROS    PPMLDT
     C**************       Z-ADD*ZEROS    PPCFDT
     C**************       Z-ADD*ZEROS    PPDNDT
     C**************       MOVE *BLANKS   PPLST
     C**************       MOVE *BLANKS   PPLIC#
     C**************       Z-ADD*ZEROS    PPLEXD
     C**************       Z-ADD*ZEROS    PPLRND
     C**************       Z-ADD*ZEROS    PPACXD
      ****** END DEL ***** 06/14/06 **************************************
      *
      ****** BEG ADD ***** 06/14/06 **************************************
      * Load these fields from first producer in case there is no 2nd agt.
      * while still have available
     C                   Z-ADD     APFIDT        PPFIDT
     C                   Z-ADD     APMLDT        PPMLDT
     C                   Z-ADD     APCFDT        PPCFDT
     C                   Z-ADD     APDNDT        PPDNDT
     C                   MOVE      APRST         PPLST
     C                   MOVEL     APLIC#        PPLIC#
     C                   Z-ADD     APEXDT        PPLEXD
     C                   Z-ADD     APRWDT        PPLRND
     C                   Z-ADD     APCXDT        PPACXD
      ****** END ADD ***** 06/14/06 **************************************
      *
      * LOAD new fields Added to WFPPP file
      *
     C                   MOVE(P)   AGGRCD        PPGRCD
     C                   MOVEL(P)  AGCONT        PPCONT
     C                   MOVE(P)   AGMKRP        PPMKRP
     C                   MOVE(P)   AGMKTR        PPMKTR
      *
     C                   Z-ADD     AHRT#         PPRT#
     C                   MOVEL(P)  ARDESC        PPDESC
     C                   MOVE      ARPLAN        PPPLAN
     C                   Z-ADD     ARNWRT        PPNWRT
     C                   Z-ADD     ARRNRT        PPRNRT
     C                   Z-ADD     AHSAGT        PPSAGT
     C                   Z-ADD     AHSRT#        PPSRT#
      * Secondary Agent Info
     C     AAKY          CHAIN     PAA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  AANAME        PPSNAM
     C                   Z-ADD     AAFEIN        PPSFIN
     C                   Z-ADD     AAMOD         PPSMOD
      *
      * Secondary Agent Rates
     C     ARKEY         SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ARNWRT        PPSNWR
     C                   MOVEL(P)  ARDESC        PPSDES
     C                   Z-ADD     ARRNRT        PPSRNR
     C                   ENDIF
     C                   Z-ADD     AHSPRD        PPSPRD
      *
     C                   Z-ADD     AHSPRD        XXPROD
     C     APKEY         CHAIN     PAP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  APLSN         PPSLSN
     C                   MOVEL(P)  APFSN         PPSFSN
     C                   MOVE      ARPLAN        PPSPLN
     C                   MOVEL(P)  APMAD1        PPSADR
     C                   MOVEL(P)  APMCTY        PPSCTY
     C                   MOVE      APMST         PPSST
     C                   Z-ADD     APMZIP        PPSZIP
     C                   Z-ADD     APFIDT        PPFIDT
     C                   Z-ADD     APMLDT        PPMLDT
     C                   Z-ADD     APCFDT        PPCFDT
     C                   Z-ADD     APDNDT        PPDNDT
     C                   MOVE      APRST         PPLST
     C                   MOVEL     APLIC#        PPLIC#
     C                   Z-ADD     APEXDT        PPLEXD
     C                   Z-ADD     APRWDT        PPLRND
     C                   Z-ADD     APCXDT        PPACXD
     C                   ENDIF
      *
     C                   ENDIF
      *
      ***** END ADD ****** 04/03/06 ***********************************
      *
     C                   WRITE     PPP01
     C*
     C                   ENDSR
      **** END   ADD *** 04/16/02 *************************************
      **** BEG   ADD *** 01/08/10 *************************************
     CSR   LDRTFD        BEGSR
     C*
     C     FNKEY         SETLL     PGS00
     C     XXCO#         READE     PGS00                                  50
     C*
     C                   Z-ADD     1             I                 3 0
     C     *IN50         DOWEQ     *OFF
     C     GSIEFL        IFEQ      'I'
     C                   MOVEL     GSFND         GRP(I)
     C                   ADD       1             I
     C                   ENDIF
     C     XXCO#         READE     PGS00                                  50
     C                   ENDDO
     C                   ENDSR
     C*
     CSR   LDGRP1        BEGSR
     C                   Z-ADD     1             I
     C                   Z-ADD     1             J                 2 0
     C                   SETON                                        01
     C     I             DOWLT     1000
     C     GRP(I)        ANDGT     *BLANK
     C                   MOVEL     GRP(I)        GRPX(J)
     C                   MOVE      ','           GRPX(J)
     C     J             IFEQ      32
     C                   MOVE      ' '           GRPX(J)
     C                   MOVEA     GRPX          RPFND           128
     C                   EXCEPT    ROUT11
     C                   SETOFF                                       01
     C                   MOVE      *BLANKS       GRPX
     C                   Z-ADD     0             J
     C                   ENDIF
     C                   ADD       1             I
     C                   ADD       1             J
     C                   ENDDO
     C     J             IFLT      32
     C                   SUB       1             J
     C                   MOVE      ' '           GRPX(J)
     C                   MOVEA     GRPX          RPFND
     C                   EXCEPT    ROUT11
     C                   MOVE      *BLANKS       GRPX
     C                   ENDIF
     C                   ENDSR
     CSR   LDGRP2        BEGSR
     C                   Z-ADD     1             I
     C                   Z-ADD     1             J                 2 0
     C                   SETON                                        01
     C     I             DOWLT     1000
     C     GRP(I)        ANDGT     *BLANK
     C                   MOVEL     GRP(I)        GRPX(J)
     C                   MOVE      ','           GRPX(J)
     C     J             IFEQ      32
     C                   MOVE      ' '           GRPX(J)
     C                   MOVEA     GRPX          RPFND           128
     C                   EXCEPT    ROU111
     C                   MOVE      *BLANKS       GRPX
     C                   SETOFF                                       01
     C                   Z-ADD     0             J
     C                   ENDIF
     C                   ADD       1             I
     C                   ADD       1             J
     C                   ENDDO
     C     J             IFLT      32
     C                   SUB       1             J
     C                   MOVE      ' '           GRPX(J)
     C                   MOVEA     GRPX          RPFND
     C                   EXCEPT    ROU111
     C                   MOVE      *BLANKS       GRPX
     C                   ENDIF
     C                   ENDSR
      **** END   ADD *** 01/08/10 *************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                  15 'ROUTE TO:'
     O*********      08      ORDIS1              60
     O*********         09   UPUSNM              67
     O********* E            ROUTE          1
     O*********      08      ORDIS2              60
     O********* E            ROUTE          1
     O*********      08      ORDIS3              60
     O********* E            ROUTE          2
     O*********      08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
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
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O***** BEGIN CHG ******** 04/30/99 *******************************
     O**********               ORTITL    80
     O                       XXTITL              80
     O***** END CHG ********** 04/30/99 *******************************
      ***** Beg Del ***** 08/26/14 ************************************
     O********* E            ROUTE          2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              30
     O*********                                  36 'FILE:'
     O*********              ORFIL#              38
     O*********                                  46 'COPIES:'
     O*********              ORCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              ORHOLD              61
     O********* E            ROUTE          2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********      08      OROUTQ              30
     O*  MLS 1/18/88 ADD INDICATORS AND OUT QUEUE
     O*********         09   QQQUE               30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*** BEGIN ADD *** 08/27/97 **************************************
     O          E            ROUTE       3  2
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O***** BEGIN CHG ******** 04/30/99 *******************************
     O**********                         26 'COMPANY        :'
     O                                           21 'COMPANY  :'
     O**********               QQCO#     38
     O                       QQCO#               35
     O***** BEG CHG ******** 01/08/10 *******************************
     O          E            ROUT11         2
     O**********                         26 'FUND           :'
     O               01                          23 'GROUP      :'
     O**********               QQFND     38
     O*                        QQFND     35
     O                       RPFND             +001
     O***** END CHG ******** 01/08/10 *******************************
     O          E            ROUT12         2
     O**********                         26 'FUND YEAR      :'
     O*** BEGIN CHG *** 05/12/04 **************************************
     O**********                         23 'POLICY YEAR:'
     O**********               QQFYR     38
     O**********               QQFYR     35
     O                                           23 'DATE RANGE :'
     O                       X1FRDT            +  2 '  /  /    '
     O                                         +  1 '-'
     O                       X1TODT            +  1 '  /  /    '
     O*** END   CHG *** 05/12/04 **************************************
     O          E            ROUT12         2
     O**********                         26 'RUN DATE       :'
     O                                           23 'RUN DATE   :'
     O**********               XXDATEY   38
     O                       X1DATE        Y     35
     O***  Begin Add  ***  08/24/05  **************************************
     O          E            ROUT12         2
     O                                           23 'AGENT      :'
     O                       RSAGT#            +  2
     O          E            ROUT12         2
     O                                           23 'PRODUCER   :'
     O                       RSPROD            +  2
     O***  End   Add  ***  08/24/05  **************************************
     O          E            ROUT12         2
     O**********                         26 'SORT BY        :'
     O**********       91                38 'CHAPTER'
     O**********       92                38 'AREA'
     O**********       93                38 'COUNTY'
     O                                           23 'SORT BY    :'
     O               91                          35 'CHAPTER'
     O               92                          35 'AREA'
     O               93                          35 'COUNTY'
      ***** BEG ADD ***** 06/03/04 ************************************
     O               94                          35 'AGENT'
      ***** END ADD ***** 06/03/04 ************************************
     O*** END ADD *** 08/27/97 ****************************************
     O***** END CHG ********** 04/30/99 *******************************
      ***** BEG CHG ****** 04/03/06 ***********************************
     O************                       23 '(c) Copyright Progressi'
     O************                          've Data Solutions, Inc.,'
      ****** BEG DLT ***** 01/28/10 **********************************
     O****    E  160           ROUT12
     O***                                23 '(c) Copyright Fiserv In'
     O***                                   'surance Solutions, Inc.,'
     O***                      *YEAR   +  1
      ****** END DLT ***** 01/28/10 **********************************
      ***** END CHG ****** 04/03/06 ***********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUT12         1 59
     O                       #BRND1
     O          E            ROUT12         1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O*** BEGIN CHG *** 05/12/04 **************************************
     O********E* 106           HEAD
     O**********               DSTIME    08 '  :  :  '
     O****BEGIN*CHG *** 06/26/97 **************************************
     O**********               CONAME   100
     O**********               XXCONM   110
     O****END***CHG *** 06/26/97 **************************************
     O**********               DSDATEY  158
     O********E* 1             HEAD
     O**********               Q1PGM     10
     O******BEGIN CHG ******** 04/30/99 *******************************
     O**********               ORTITL   109
     O**********               XXTITL   115
     O******END*CHG ********** 04/30/99 *******************************
     O**********                        152 'PAGE:'
     O**********               PAGE  Z  158
     O********E* 2             HEAD
     O*** BEGIN CHG *** 05/12/04 **************************************
     O**********                         85 'POLICY YEAR'
     O**********               XXFYR Z +  1
     O**********                         77 'DATE RANGE'
     O**********               X1FRDT  +  1 '  /  /    '
     O**********                       +  1 '-'
     O**********               X1TODT  +  1 '  /  /    '
     O****END***CHG *** 05/12/04 **************************************
     O******BEGIN CHG ******** 04/30/99 *******************************
     O**********                        149 'RUN DATE:'
     O**********               XXDATEY  158
     O**********                        146 'RUN DATE:'
     O**********               X1DATEY  158
     O**********
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             121
      ***** BEG CHG ***** 06/03/04 ************************************
     O*************            DSDATEY  180
     O                       DSDATE        Y    196
      ***** END CHG ***** 06/03/04 ************************************
     O          E            HEAD           1
     O                       Q1PGM               10
      ***** BEG CHG ***** 06/03/04 ************************************
     O***********              XXTITL   126
     O***********                       174 'PAGE:'
     O***********              PAGE  Z  180
     O                       XXTITL             130
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
      ***** END CHG ***** 06/03/04 ************************************
     O          E            HEAD           2
     O                                           88 'DATE RANGE'
     O                       X1FRDT            +  1 '  /  /    '
     O                                         +  1 '-'
     O                       X1TODT            +  1 '  /  /    '
      ***** BEG CHG ***** 06/03/04 ************************************
     O***********                       168 'RUN DATE:'
     O***********              X1DATEY  180
     O                                          184 'RUN DATE:'
     O                       X1DATE        Y    196
      ***** END CHG ***** 06/03/04 ************************************
     O*** END   CHG *** 05/12/04 **************************************
     O***** END CHG ********** 04/30/99 *******************************
     O          E            HEAD           2
     O                                              'GROUP:'
     O***** BEGIN CHG ******** 04/30/99 *******************************
     O**********               XXFND Z +  1
     O**********               XFNAME  +  1
     O                       XXFND         Z   +002
     O                       XFNAME            +002
     O***** END CHG ********** 04/30/99 *******************************
     O                  21                       80 'CHAPTER:'
     O                  21   SVCHAP        Z     84
     O                  22                       80 'AREA:'
     O                  22   SVAREA        Z     84
     O                  23                       80 'COUNTY:'
     O*** BEGIN CHG *** 06/26/97 **************************************
     O**********          23   SVCNTY    84
     O**********               XXDESC   110
     O                  23   SVCNTY              86
     O                       XXDESC             112
     O*** END   CHG *** 06/26/97 **************************************
     O          E            HEAD           1
     O                                            6 'AGENT:'
     O                       SVAGNT        Z     15
     O                       AGNAME              56
     O                 N21                       71 'CHAPTER:'
     O                 N21   SVCHAP        Z     75
     O                 N22                       83 'AREA:'
     O                 N22   SVAREA        Z     87
     O                 N23                       97 'COUNTY:'
     O*** BEGIN CHG *** 06/26/97 **************************************
     O**********         N23   SVCNTY   100
     O                 N23   SVCNTY             102
     O*** END   CHG *** 06/26/97 **************************************
     O          E            HEAD           1
     O                       AGMAD1              56
     O***** BEGIN CHG ******** 04/30/99 *******************************
     O******* E  1             HEAD
     O          E            HEAD           2
     O***** END CHG ********** 04/30/99 *******************************
     O                       AGCS                56
     O                                           58 '('
     O                       AGARE1              61
     O                                           62 ')'
     O                       AGPHN1              71 '   -    '
     O*** BEGIN CHG *** 05/12/04 **************************************
     O********E* 1             HEAD
     O**********                         09 'EMPLOYER'
     O**********                         15 'DIV'
     O**********                       +  1 'NAME'
     O**********                         49 'EFFECT DT'
     O**********                         64 'WRITTEN PREM'
     O**********                         80 'EARNED PREMIUM'
     O**********                         88 'CLAIMS'
     O**********                        102 'RESERVES'
     O**********                        116 'PAID'
     O**********                        134 'INCURRED'
     O**********                        142 'L/R'
     O**********                        151 'FREQ'
     O**********                        158 'RATE'
     O          E            HEAD           1
     O                                            9 ' EMPLOYER'
     O                                         +  2 '  DIV'
     O                                         +  2 'NAME'
     O                                         + 22 ' YR'
     O                                         +  2 '  EFF DATE'
      ***** BEG ADD ***** 06/03/04 ************************************
     O                                         +  2 ' CANC DATE'
      ***** END ADD ***** 06/03/04 ************************************
     O                                         +  2 '  WRITTEN PREM'
     O                                         +  2 ' EARNED PREMIUM'
     O                                         +  2 ' CLAIMS'
     O                                         +  2 '      RESERVES'
     O                                         +  2 '           PAID'
     O                                         +  2 '       INCURRED'
     O                                         +  2 '       L/R'
     O                                         +  2 '  FREQ'
     O                                         +  2 '   RATE'
     O*** END   CHG *** 05/12/04 **************************************
     O          E            HEAD2       1  2
     O                                            9 'PRODUCER:'
     O                       ALPROD        Z     15
      *
      ***** BEG CHG ***** 07/15/04 ************************************
     O****************         PNAME  B  56
     O                       PNAME               56
      ***** END CHG ***** 07/15/04 ************************************
     O*
     O*** BEGIN CHG *** 05/12/04 **************************************
     O********E* 106           HEAD3
     O**********               DSTIME    08 '  :  :  '
     O*** BEGIN CHG *** 06/26/97 **************************************
     O**********               CONAME   100
     O**********               XXCONM   110
     O*** END   CHG *** 06/26/97 **************************************
     O**********               DSDATEY  158
     O********E* 1             HEAD3
     O**********               Q1PGM     10
     O***** BEGIN CHG ******** 04/30/99 *******************************
     O**********               ORTITL   109
     O**********               XXTITL   115
     O***** END CHG ********** 04/30/99 *******************************
     O**********                        152 'PAGE:'
     O**********               PAGE  Z  158
     O********E* 2             HEAD3
     O*** BEGIN CHG *** 05/12/04 **************************************
     O**********                         85 'POLICY YEAR'
     O**********               XXFYR Z +  1
     O**********                         77 'DATE RANGE'
     O**********               X1FRDT  +  1 '  /  /    '
     O**********                       +  1 '-'
     O**********               X1TODT  +  1 '  /  /    '
     O*** END   CHG *** 05/12/04 **************************************
     O***** BEGIN CHG ******** 04/30/99 *******************************
     O**********                        149 'RUN DATE:'
     O**********               XXDATEY  158
     O**********                        146 'RUN DATE:'
     O**********               X1DATEY  158
     O          E            HEAD3          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             121
      ***** BEG CHG ***** 06/03/04 ************************************
     O***********              DSDATEY  180
     O                       DSDATE        Y    196
      ***** END CHG ***** 06/03/04 ************************************
     O          E            HEAD3          1
     O                       Q1PGM               10
     O                       XXTITL             126
      ***** BEG CHG ***** 06/03/04 ************************************
     O***********                       174 'PAGE:'
     O***********              PAGE  Z  180
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
      ***** END CHG ***** 06/03/04 ************************************
     O          E            HEAD3          2
     O                                           88 'DATE RANGE'
     O                       X1FRDT            +  1 '  /  /    '
     O                                         +  1 '-'
     O                       X1TODT            +  1 '  /  /    '
      ***** BEG CHG ***** 06/03/04 ************************************
     O***********                       168 'RUN DATE:'
     O***********              X1DATEY  180
     O                                          184 'RUN DATE:'
     O                       X1DATE        Y    196
      ***** END CHG ***** 06/03/04 ************************************
     O*** END   CHG *** 05/12/04 **************************************
     O***** END CHG ********** 04/30/99 *******************************
     O          E            HEAD3          2
     O                                              'GROUP:'
     O***** BEGIN CHG ******** 04/30/99 *******************************
     O**********               XXFND Z +  1
     O**********               XFNAME  +  1
     O                       XXFND         Z   +  2
     O                       XFNAME            +  2
     O***** END CHG ********** 04/30/99 *******************************
     O*** BEGIN CHG *** 05/12/04 **************************************
     O********E* 2             HEAD3
     O**********                         64 'WRITTEN PREM'
     O**********                         80 'EARNED PREMIUM'
     O**********                         88 'CLAIMS'
     O**********                        102 'RESERVES'
     O**********                        116 'PAID'
     O**********                        134 'INCURRED'
     O**********                        142 'L/R'
     O**********                        151 'FREQ'
     O          E            HEAD3          2
     O                                            9 '         '
     O                                         +  2 '     '
     O                                         +  2 '    '
     O                                         + 22 '   '
     O                                         +  2 '          '
      ***** BEG ADD ***** 06/03/04 ************************************
     O                                         +  2 '          '
      ***** END ADD ***** 06/03/04 ************************************
     O                                         +  2 '  WRITTEN PREM'
     O                                         +  2 ' EARNED PREMIUM'
     O                                         +  2 ' CLAIMS'
     O                                         +  2 '      RESERVES'
     O                                         +  2 '           PAID'
     O                                         +  2 '       INCURRED'
     O                                         +  2 '       L/R'
     O                                         +  2 '  FREQ'
     O                                         +  2 '   RATE'
     O*** END   CHG *** 05/12/04 **************************************
     O*
     O*** BEGIN CHG *** 05/12/04 **************************************
     O********E* 1             DETAIL
     O**********               ALEMP#Z    9
     O**********               ALDIV Z   15
     O**********               ENAME     40
     O**********               XXFDTE B  49 '  /  /  '
     O*** BEGIN CHG *** 12/02/96 **************************************
     O**********               ELANPRJB  64
     O**********               XXANPRJB  64
     O*** END   CHG *** 12/02/96 **************************************
     O*** BEGIN DEL ***  05/05/98  ************************************
     O******                   ELANPRJB  64
     O*** END DEL *****  05/05/98  ************************************
     O**********               PRMYTDJB  80
     O**********               EMPCLMZB  87
     O**********               EMPRESJB 103
     O**********               EMPPMTJB 119
     O**********               EMPINCJB 135
     O**********               LOSS  JB 145
     O**********               FREQ  JB 152
     O**********               XXRATEJB 160
     O          E            DETAIL         1
     O                       ALEMP#        Z      9
     O                       ALDIV         Z   +  2
     O                       ENAME             +  2
     O                       ALFYR         Z   +  2
     O                       XXFDTE         B  +  2 '  /  /    '
      ***** BEG ADD ***** 06/03/04 ************************************
     O               73      CANDTE         B  +  1 '0  /  /    '
      ***** END ADD ***** 06/03/04 ************************************
     O                       XXANPR        JB  +  2
     O                       PRMYTD        JB  +  2
     O                       EMPCLM        3B  +  3
     O                       EMPRES        JB  +  2
     O                       EMPPMT        JB  +  2
     O                       EMPINC        JB  +  2
     O                       LOSS          JB  +  2
     O                       FREQ          JB  +  2
     O                       XXRATE        JB  +  2
      *
     O*** END   CHG *** 05/12/04 **************************************
      *
     O*** BEGIN CHG *** 05/12/04 **************************************
     O********E*11             TOTL1
     O**********                         30 'PRODUCER TOTALS:'
     O**********                         42 'COUNT:'
     O**********               L1TOT ZB  48
     O**********               L1ANPRJB  64
     O**********               L1PREMJB  80
     O**********               L1TOTC3B  87
     O**********               L1RTOTJB 103
     O**********               L1PTOTJB 119
     O**********               L1ITOTJB 135
     O**********               L1LOSSJB 145
     O**********               L1FREQJB 152
     O********E*13             TOTL2
     O**********                         30 'AGENT TOTALS:'
     O**********                         42 'COUNT:'
     O**********               ATOT  ZB  48
     O**********               L2ANPRJB  64
     O**********               L2PREMJB  80
     O**********               L2TOTC3B  87
     O**********               L2RTOTJB 103
     O**********               L2PTOTJB 119
     O**********               L2ITOTJB 135
     O**********               L2LOSSJB 145
     O**********               L2FREQJB 152
     O********E*13             TOTST
     O****BEGIN*CHG *** 06/26/97 **************************************
     O**********               XXSORT    18
     O**********               XXSORT    17
     O****END***CHG *** 06/26/97 **************************************
     O**********          21   SVCHAPZ   22
     O**********          22   SVAREAZ   22
     O**********          23   SVCNTY    22
     O**********                         30 'TOTALS:'
     O**********                         42 'COUNT:'
     O**********               STOT  ZB  48
     O**********               STANPRJB  64
     O**********               STPREMJB  80
     O**********               STTOTC3B  87
     O**********               STRTOTJB 103
     O**********               STPTOTJB 119
     O**********               STITOTJB 135
     O**********               STLOSSJB 145
     O**********               STFREQJB 152
     O********E*11             TOTL3
     O**********                         30 'GROUP TOTALS:'
     O**********                         42 'COUNT:'
     O**********               FTOT  ZB  48
     O**********               L3ANPRJB  64
     O**********               L3PREMJB  80
     O**********               L3TOTC3B  87
     O**********               L3RTOTJB 103
     O**********               L3PTOTJB 119
     O**********               L3ITOTJB 135
     O**********               L3LOSSJB 145
     O**********               L3FREQJB 152
     O          E            TOTL1       1  1
     O                                           30 'PRODUCER TOTALS:'
     O                                           42 'COUNT:'
     O                       L1TOT         ZB    48
      ***** BEG ADD ***** 06/03/04 ************************************
     O**************           L1ANPRJB+ 13
     O                       L1ANPR        JB  + 25
      ***** END ADD ***** 06/03/04 ************************************
     O                       L1PREM        JB  +  2
     O                       L1TOTC        3B  +  3
     O                       L1RTOT        JB  +  2
     O                       L1PTOT        JB  +  2
     O                       L1ITOT        JB  +  2
     O                       L1LOSS        JB  +  2
     O                       L1FREQ        JB  +  2
     O          E            TOTL2       1  3
     O                                           30 'AGENT TOTALS:'
     O                                           42 'COUNT:'
      ***** BEG CHG ***** 06/03/04 ************************************
     O**********               ATOT  ZB  48
     O**********               L2ANPRJB+ 13
     O**********               L2PREMJB+  2
     O**********               L2TOTC3B+  3
     O**********               L2RTOTJB+  2
     O**********               L2PTOTJB+  2
     O**********               L2ITOTJB+  2
     O**********               L2LOSSJB+  2
     O**********               L2FREQJB+  2
     O                       ATOT          Z     48
     O                       L2ANPR        J   + 25
     O                       L2PREM        J   +  2
     O                       L2TOTC        3   +  3
     O                       L2RTOT        J   +  2
     O                       L2PTOT        J   +  2
     O                       L2ITOT        J   +  2
     O                       L2LOSS        J   +  2
     O                       L2FREQ        J   +  2
      ***** END CHG ***** 06/03/04 ************************************
      *
      ***** BEG ADD ***** 06/03/04 ************************************
      *
     O          E            TOTL2       1  1
     O                                           23 'TOTAL ACTIVE:'
     O                                           42 'COUNT:'
     O                       AATOT         Z     48
     O                       AAANPR        J   +025
     O                       AAPREM        J   +002
     O                       AATOTC        3   +003
     O                       AARTOT        J   +002
     O                       AAPTOT        J   +002
     O                       AAITOT        J   +002
     O                       AALOSS        J   +003
     O                       AAFREQ        J   +002
      *
     O          E            TOTL2       1  1
     O                                           26 'TOTAL CANCELLED:'
     O                                           42 'COUNT:'
     O                       ACTOT         Z     48
     O                       ACANPR        J   +025
     O                       ACPREM        J   +002
     O                       ACTOTC        3   +003
     O                       ACRTOT        J   +002
     O                       ACPTOT        J   +002
     O                       ACITOT        J   +002
     O                       ACLOSS        J   +003
     O                       ACFREQ        J   +002
      ***** END ADD ***** 06/03/04 ************************************
     O          E            TOTST       1  3
     O                       XXSORT              17
     O                  21   SVCHAP        Z     22
     O                  22   SVAREA        Z     22
     O                  23   SVCNTY              22
     O                                           30 'TOTALS:'
     O                                           42 'COUNT:'
     O                       STOT          ZB    48
      ***** BEG CHG ***** 06/03/04 ************************************
     O**************           STANPRJB+ 13
     O                       STANPR        JB  + 25
      ***** BEG CHG ***** 06/03/04 ************************************
     O                       STPREM        JB  +  2
     O                       STTOTC        3B  +  3
     O                       STRTOT        JB  +  2
     O                       STPTOT        JB  +  2
     O                       STITOT        JB  +  2
     O                       STLOSS        JB  +  2
     O                       STFREQ        JB  +  2
      *
      ***** BEG ADD ***** 06/03/04 ************************************
      *
     O          E            TOTST          1
     O                                           32 'ACTIVE:'
     O                                           42 'COUNT:'
     O                       SATOT         ZB    48
     O                       SAANPR        JB  +025
     O                       SAPREM        JB  +002
     O                       SATOTC        3B  +003
     O                       SARTOT        JB  +002
     O                       SAPTOT        JB  +002
     O                       SAITOT        JB  +002
     O                       SALOSS        JB  +003
     O                       SAFREQ        JB  +002
      *
     O          E            TOTST          1
     O                                           35 'CANCELLED:'
     O                                           42 'COUNT:'
     O                       SCTOT         ZB    48
     O                       SCANPR        JB  +025
     O                       SCPREM        JB  +002
     O                       SCTOTC        3B  +003
     O                       SCRTOT        JB  +002
     O                       SCPTOT        JB  +002
     O                       SCITOT        JB  +002
     O                       SCLOSS        JB  +003
     O                       SCFREQ        JB  +002
      ***** END ADD ***** 06/03/04 ************************************
     O          E            TOTL3       1  1
     O                                           30 'GROUP TOTALS:'
     O                                           42 'COUNT:'
     O                       FTOT          ZB    48
      ***** BEG CHG ***** 06/03/04 ************************************
     O*************            L3ANPRJB+ 13
     O                       L3ANPR        JB  + 25
      ***** END CHG ***** 06/03/04 ************************************
     O                       L3PREM        JB  +  2
     O                       L3TOTC        3B  +  3
     O                       L3RTOT        JB  +  2
     O                       L3PTOT        JB  +  2
     O                       L3ITOT        JB  +  2
     O                       L3LOSS        JB  +  2
     O                       L3FREQ        JB  +  2
     O*** END   CHG *** 05/12/04 **************************************
      *
      ***** BEG ADD ***** 06/03/04 ************************************
      *
     O          E            TOTL3       1  1
     O                                           32 'ACTIVE:'
     O                                           42 'COUNT:'
     O                       FATOT         Z     48
     O                       FAANPR        J   +025
     O                       FAPREM        J   +002
     O                       FATOTC        3   +003
     O                       FARTOT        J   +002
     O                       FAPTOT        J   +002
     O                       FAITOT        J   +002
     O                       FALOSS        J   +003
     O                       FAFREQ        J   +002
      *
     O          E            TOTL3       1  1
     O                                           35 'CANCELLED:'
     O                                           42 'COUNT:'
     O                       FCTOT         Z     48
     O                       FCANPR        J   +025
     O                       FCPREM        J   +002
     O                       FCTOTC        3   +003
     O                       FCRTOT        J   +002
     O                       FCPTOT        J   +002
     O                       FCITOT        J   +002
     O                       FCLOSS        J   +003
     O                       FCFREQ        J   +002
      ***** END ADD ***** 06/03/04 ************************************
      ***** BEG ADD ***** 01/08/10 ************************************
     O          E            TOTL4       1  1
     O                                           30 'REPORT TOTAL:'
     O                                           42 'COUNT:'
     O                       RTOT          Z     48
     O                       L4ANPR        J   + 25
     O                       L4PREM        J   +  2
     O                       L4TOTC        3   +  3
     O                       L4RTOT        J   +  2
     O                       L4PTOT        J   +  2
     O                       L4ITOT        J   +  2
     O                       L4LOSS        J   +  2
     O                       L4FREQ        J   +  2
      *
     O          E            TOTL4       1  1
     O                                           32 'ACTIVE:'
     O                                           42 'COUNT:'
     O                       RATOT         Z     48
     O                       RAANPR        J   +025
     O                       RAPREM        J   +002
     O                       RATOTC        3   +003
     O                       RARTOT        J   +002
     O                       RAPTOT        J   +002
     O                       RAITOT        J   +002
     O                       RALOSS        J   +003
     O                       RAFREQ        J   +002
      *
     O          E            TOTL4       1  1
     O                                           35 'CANCELLED:'
     O                                           42 'COUNT:'
     O                       RCTOT         Z     48
     O                       RCANPR        J   +025
     O                       RCPREM        J   +002
     O                       RCTOTC        3   +003
     O                       RCRTOT        J   +002
     O                       RCPTOT        J   +002
     O                       RCITOT        J   +002
     O                       RCLOSS        J   +003
     O                       RCFREQ        J   +002
      ***** END ADD ***** 01/08/10 ************************************
      *
     OQSYSPRT1  E            ROUTE2         1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE2         1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE2         1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE2         1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE2         1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE2         1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            ROUTE2         1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE2         1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE2         1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE2         1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE2         1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE2         3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
      *********
     O********* E            ROUTE2         1
     O*********                                  15 'ROUTE TO:'
     O*********      08      XXDIS1              60
     O*********         09   UPUSNM              67
     O********* E            ROUTE2         1
     O*********      08      XXDIS2              60
     O********* E            ROUTE2         1
     O*********      08      XXDIS3              60
     O********* E            ROUTE2         2
     O*********      08      XXDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE2         1
     O                                           15 'ROUTE TO:'
     O               08      XXDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE2         1
     O               08      XXDIS2              60
     O          E            ROUTE2         1
     O               08      XXDIS3              60
     O          E            ROUTE2         2
     O               08      XXDIS4              60
      ***** End Add ***** 08/05/15 ************************************
      *
     O          E            ROUTE2         2
     O                                           19 'REPORT TITLE:'
      ***** BEG CHG ***** 05/14/10 ************************************
     O**                       XXTITL    80
     O                       XXTITS              80
      ***** END CHG ***** 05/14/10 ************************************
      ***** Beg Del ***** 08/26/14 ************************************
     O********* E            ROUTE2         2
     O*********                                  19 'PROGRAM NAME:'
     O*********              XXPNAM              30
     O*********                                  36 'FILE:'
     O*********              XXFIL#              38
     O*********                                  46 'COPIES:'
     O*********              XXCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              XXHOLD              61
      *
     O********* E            ROUTE2         2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********      08      XXOUTQ              30
     O*********         09   QQQUE               30
     O*********                                  37 'FORMS:'
     O*********              XXFORM              48
     O*********                                  56 'SAVE:'
     O*********              XXSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      *
     O          E            ROUTE2      3  2
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE2         2
     O                                           21 'COMPANY  :'
     O                       QQCO#               33
     O*****BEG CHG  ***  01/08/10  ********************************
     O          E            ROU111         2
     O               01                          23 'GROUP      :'
     O                       RPFND             +001
     O*                        XXFND Z +002
     O*                        XFNAME  +002
      *
     O          E            ROU112         2
     O                                           23 'DATE RANGE :'
     O                       X1FRDT            +  2 '  /  /    '
     O                                         +  1 '-'
     O                       X1TODT            +  1 '  /  /    '
      *
     O          E            ROU112         2
     O                                           23 'RUN DATE   :'
     O                       X1DATE            +000 '0  /  /    '
      *
     O          E            ROU112         2
     O                                           23 'SORT BY    :'
     O               91                          33 'CHAPTER'
     O               92                          33 'AREA'
     O               93                          33 'COUNTY'
     O               94                          33 'AGENT'
      *
     O          E            ROU112         2
     O                                           23 'TYPE OF OUTPUT:'
     O               26                          36 'PRINTED ONLY'
     O               27                          34 'FILE ONLY'
     O               28                          36 'PRINTED/FILE'
      *
     O*****END CHG  ***  01/08/10  ********************************
      *
      ***** BEG CHG ****** 04/03/06 ***********************************
     O**************                     23 '(C) COPYRIGHT PROGRESSI'
     O**************                   +000 'VE DATA SOLUTIONS. INC.,'
      ****** BEG DLT ***** 01/28/10 **********************************
     O*****   E  160           ROU112
     O***                                23 '(C) COPYRIGHT FISERV IN'
     O***                              +000 'SURANCE SOLUTIONS. INC.,'
     O***                      *YEAR   +001
      ****** END DLT ***** 01/28/10 **********************************
      ***** END CHG ****** 04/03/06 ***********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROU112         1 59
     O                       #BRND1
     O          E            ROU112         1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
      *
     O          E            HEAD4          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             100
     O                       DSDATE        Y    158
      *
     O          E            HEAD4          1
     O                       Q1PGM               10
      ***** BEG CHG ***** 05/14/10 ************************************
     O**                       XXTITL   105
     O                       XXTITS             105
      ***** END CHG ***** 05/14/10 ************************************
     O                                          152 'PAGE:'
     O                       PAGES         Z    158
      *
     O          E            HEAD4          2
     O                                           68 'DATE RANGE'
     O                       X1FRDT            +  1 '  /  /    '
     O                                         +  1 '-'
     O                       X1TODT            +  1 '  /  /    '
      *
     O          E            HEAD4          2
     O                                            6 'GROUP:'
     O                       XXFND         Z   +002
     O                       XFNAME            +002
      *
     O          E            HEAD4          2
     O                                           64 'WRITTEN PREM'
     O                                           80 'EARNED PREMIUM'
     O                                           88 'CLAIMS'
     O                                          102 'RESERVES'
     O                                          116 'PAID'
     O                                          134 'INCURRED'
     O                                          142 'L/R'
     O                                          151 'FREQ'
      *
     O          E            DET1           1
     O                                            7 'AGENT'
     O                       SVAGNT        Z     16
     O                                           25 'TOTALS:'
     O                                           37 'COUNT:'
     O                       ATOT          ZB    43
     O                       L2ANPR        JB    64
     O                       L2PREM        JB    80
     O                       L2TOTC        3B    87
     O                       L2RTOT        JB   103
     O                       L2PTOT        JB   119
     O                       L2ITOT        JB   135
     O                       L2LOSS        JB   145
     O                       L2FREQ        JB   152
      *
     O          E            DET1           1
     O                                           27 'ACTIVE:'
     O                                           37 'COUNT:'
     O                       AATOT         ZB    43
     O                       AAANPR        JB    64
     O                       AAPREM        JB    80
     O                       AATOTC        3B    87
     O                       AARTOT        JB   103
     O                       AAPTOT        JB   119
     O                       AAITOT        JB   135
     O                       AALOSS        JB   145
     O                       AAFREQ        JB   152
      *
     O          E            DET1           1
     O                                           30 'CANCELLED:'
     O                                           37 'COUNT:'
     O                       ACTOT         ZB    43
     O                       ACANPR        JB    64
     O                       ACPREM        JB    80
     O                       ACTOTC        3B    87
     O                       ACRTOT        JB   103
     O                       ACPTOT        JB   119
     O                       ACITOT        JB   135
     O                       ACLOSS        JB   145
     O                       ACFREQ        JB   152
      *
     O          E            DET1           2
     O                       AGNAME              55
     O*
     O          E            TOTAL       1  1
     O                                           25 'GROUP TOTALS:'
     O                                           37 'COUNT:'
     O                       FTOT          Z     43
     O                       L3ANPR        J     64
     O                       L3PREM        J     80
     O                       L3TOTC        3     87
     O                       L3RTOT        J    103
     O                       L3PTOT        J    119
     O                       L3ITOT        J    135
     O                       L3LOSS        J    145
     O                       L3FREQ        J    152
      *
     O          E            TOTAL          1
     O                                           27 'ACTIVE:'
     O                                           37 'COUNT:'
     O                       FATOT         Z     43
     O                       FAANPR        J     64
     O                       FAPREM        J     80
     O                       FATOTC        3     87
     O                       FARTOT        J    103
     O                       FAPTOT        J    119
     O                       FAITOT        J    135
     O                       FALOSS        J    145
     O                       FAFREQ        J    152
      *
     O          E            TOTAL          1
     O                                           30 'CANCELLED:'
     O                                           37 'COUNT:'
     O                       FCTOT         Z     43
     O                       FCANPR        J     64
     O                       FCPREM        J     80
     O                       FCTOTC        3     87
     O                       FCRTOT        J    103
     O                       FCPTOT        J    119
     O                       FCITOT        J    135
     O                       FCLOSS        J    145
     O                       FCFREQ        J    152
      ***** BEG ADD ***** 01/08/10 ************************************
     O          E            TOT41       1  1
     O                                           25 'REPORT TOTAL:'
     O                                           37 'COUNT:'
     O                       RTOT          ZB    43
     O                       L4ANPR        JB    64
     O                       L4PREM        JB    80
     O                       L4TOTC        3B    87
     O                       L4RTOT        JB   103
     O                       L4PTOT        JB   119
     O                       L4ITOT        JB   135
     O                       L4LOSS        JB   145
     O                       L4FREQ        JB   152
      *
     O          E            TOT41       1  1
     O                                           25 'ACTIVE:'
     O                                           37 'COUNT:'
     O                       RATOT         Z     43
     O                       RAANPR        J     64
     O                       RAPREM        J     80
     O                       RATOTC        3     87
     O                       RARTOT        J    103
     O                       RAPTOT        J    119
     O                       RAITOT        J    135
     O                       RALOSS        J    145
     O                       RAFREQ        J    152
      *
     O          E            TOT41       1  1
     O                                           25 'CANCELLED:'
     O                                           37 'COUNT:'
     O                       RCTOT         Z     43
     O                       RCANPR        J     64
     O                       RCPREM        J     80
     O                       RCTOTC        3     87
     O                       RCRTOT        J    103
     O                       RCPTOT        J    119
     O                       RCITOT        J    135
     O                       RCLOSS        J    145
     O                       RCFREQ        J    152
      ***** END ADD ***** 01/08/10 ************************************
      *
