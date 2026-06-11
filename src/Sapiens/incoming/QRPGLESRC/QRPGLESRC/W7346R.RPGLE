     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR  dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W7346R - AGENT-EMPLOYER LISTING
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*     Program reads the workfile produced by SRRPTR and edited  *
     H*     SRRPTR1 (WXEBP)                                           *
     H*                                                               *
     H*     This program prints a list of employers assigned to an    *
     H*     agent as of a specifed date.                              *
     H*                                                               *
     H*     Printed info include: Agent#, Name, Address, Marketing    *
     H*     representative, Emp#/Div, and Emp Name, Agent History     *
     H*     effective date, rate#, New & ReNew percentages, and       *
     H*     Producer Name.                                            *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     W7346C - OVERRIDE CL                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     NONE                                                      *
     H*                                                               *
     H*  CREATED: 01/12/93        PROGRAMMER: 11       PROJECT: WC027 *
     H*  REWRITE: 01/13/98        PROGRAMMER: 33       PROJECT: CM030 *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 11/04/14  C:291 SUPPORT# WA045 #86785                         *
     H*                 Fixed Inforce Premium on the output report.   *
     H*                 For policies that have cancel date but don't  *
     H*                 have cancellation confirmation date, the      *
     H*                 Inforce premium on the output report is not   *
     H*                 matching with the Inforce Premium in the      *
     H*                 Outfile in Business Objects.                  *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/22/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     H* 05/06/10 C:235 PROJECT: 133WIP                                *
     H*                 Agent Employee List Report-Add Agent Status   *
     H*                 date                                          *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/25/07 C:189 SUPPORT# WA133 #57933                          *
     H*          Agent# filter not working properly and printing some *
     H*          emlpoyers with Agent not selected for filteration    *
     H*                                                               *
     H*                                                               *
     H* 01/08/02 C:136 SUPPORT# WA091 #30656                          *
     H*          Pgm executes forever loop when QQCXAG = 'N' and the  *
     H*          Agent Canx Date is less than the Run Date.           *
     H*                                                               *
     H* 11/27/01  C:65  PROJECT WC1059                                *
     H*                 PULL EXPENSE CONSTANT FROM NEW FIELD.         *
     H*                                                               *
     H* 08/20/01 C:136 SUPPORT WA072 #28769                           *
     H*                Pgm was writing only one record when writing   *
     H*                to WFBBP.                                      *
     H*                                                               *
     H* 08/14/01 C:90  PROJECT: WC577                                 *
     H*                Add employer underwriter code and name, and    *
     H*                agent underwriter code and name to the output  *
     H*                file.                                          *
     H*                                                               *
     H* 04/08/01 C:61  PROJECT WC577                                  *
     H*                Add Rating State and Rate Set Code to output   *
     H*                file.                                          *
     H*                                                               *
     H* 12/21/00 C:61  PROJECT: WC577                                 *
     H*               - Add producer number and                       *
     H*                 chapter description to output file WFBBP.     *
     H*               - Add policy year and policy effective date     *
     H*                 to detail line.                               *
     H*                                                               *
     H* 04/10/00 C:118 PROJECT: WC987                                 *
     H*                Load alternate company name from fund master   *
     H*                                                               *
     H* 03/15/00 C:61  SUPPORT WA102 22212                            *
     H*                Produce final report totals when primary sort  *
     H*                is by agent.                                   *
     H*                                                               *
     H*  11/30/99 C:118 PROJECT: WC577                                *
     H*                 LOAD NEW FIELDS BBFEIN AND BBMOD.             *
     H*                                                               *
     H* 08/11/99 C:61  PROJECT WC577                                  *
     H*                Inforce Premium = 0 if policy is cancelled.    *
     H*                                                               *
     H*  10/21/98 C:33 SUPPORT: PER JJ                                *
     H*                DO NOT PRINT THE TOTAL LINE FOR AGENTS THAT DO *
     H*                NOT PRINT.                                     *
     H*                                                               *
     H*  06/04/98 C:33 SEQ#: WA033/12032                              *
     H*                GET THE AGENT MASTER RECORD IF NOT AGENT SORT. *
     H*                                                               *
     H*  05/12/98 C:33 PROJECT: CM030                                 *
     H*                IF THE REPORT IS RUN FOR ONE AGENT, THE AGENT  *
     H*                WILL SHOW REGUARDLESS OF STATUS.               *
     H*                                                               *
     H*  03/12/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*   08   BATCH MODE                                             *
     H*   09   DEMAND MODE                                            *
     H*   20   Agent Sort                                             *
     H*   21   Range of Employer Names                                *
     H*   22   Range of Employer #'s                                  *
     H*   23   Range of Agent Names                                   *
     H*   24   Range of Agent #'s                                     *
     H*   89   Employer CX                                            *
     H*   60   General Read & Chain                                   *
     H*   61   General Read & Chain                                   *
     H*   62   General Read & Chain                                   *
     H*   63   General Read & Chain                                   *
     H*   71   Effective date = 0                                     *
     H*****************************************************************
     H*                                                               *
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*   SMCOL   PCO01: CO                                   Company *
     H*   SMORL   POR01: CO/PGM                              Override *
     H*   WMUPL   PUP01: USER                               User Parm *
     H*   WMUPL1  PUP02: STAFF CODE                                   *
     H*   WMFNL   PFN01: CO/FND                          Group Master *
     H*   WMAGL   PAG02: CO/FEIN/MOD                     Agent Master *
     H*   WMAAL1  PAA01: CO/FND/AGT#                 Agent Assignment *
     H*   WMAHL   PAH02: CO/FND/EMP#/DIV/FYR/EFDT/AGT#  Agent History *
     H*   WMEML   PEM01: CO/FND/EMP#/DIV              Employer Master *
     H*   WDELL   PEL01: CO/FND/EMP#/DIV/FYR          Employer Detail *
     H*   WMAPL   PAP01: CO/FND/FEIN/MOD               Agent Producer *
     H*   WMARL   PAR01: CO/FND/RT#/EFDT                   Agent Rate *
     H*   WDECL   PEC01: CO#/ST/EFDT      Expense Constant Definition *
     H*                                                               *
     H*****************************************************************
      *
      *
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
      **** BEGIN ADD **** 08/14/01 ************************************
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP03)
      **** END   ADD **** 08/14/01 ************************************
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWMFNL     IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMAPL     IF   E           K DISK
     FWMARL     IF   E           K DISK
     F*** BEGIN DEL ***  11/27/01  ************************************
     F*****WDECL   IF  E           K        DISK
     F*** END DEL *****  11/27/01  ************************************
     FWXEBL     IF   E           K DISK
      ***** BEG ADD ******* 12/21/00 **********************************
      *
      *  Chapter Code
     FWMCPL     IF   E           K DISK
      ***** END ADD ******* 12/21/00 **********************************
     FWFBBP     O  A E             DISK
      ***** BEG CHG ******* 12/21/00 **********************************
     F*******QSYSPRT O   F     132     OF     PRINTER
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
      ***** END CHG ******* 12/21/00 **********************************
      *
      *

     D SZ              S              1    DIM(13)
      *
      *
     D                 DS
     D  AGMZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*  KEY LIST FOR THE GROUP MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE AGENT MASTER FILE
     C*
     C     AGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*  KEY LIST FOR THE AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EBAGT#
     C*
     C***  BEGIN ADD  ***  05/12/98  *********************************
     C     AAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAG#F
     C***  END   ADD  ***  05/12/98  *********************************
     C*
     C*  KEY LIST FOR THE AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    EBCO#
     C                   KFLD                    EBFND
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C                   KFLD                    EBFYR
     C                   KFLD                    X7RNDT
     C*
     C     AHKY1         KLIST
     C                   KFLD                    EBCO#
     C                   KFLD                    EBFND
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C                   KFLD                    EBFYR
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EBEMP#
     C                   KFLD                    EBDIV
     C                   KFLD                    EBFYR
     C*
     C*  KEY LIST FOR THE AGENT PRODUCER FILE
     C*
     C     APKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    AHPROD
     C*
     C*  KEY LIST FOR AGENT RATE FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    AHRT#
     C                   KFLD                    X7RNDT
     C*
     C     ARKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    AHRT#
     C*
     C*  KEY LISTS FOR THE EXPENSE CONSTANT DEFINITION FILE
     C*
     C     ECKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXPFDT
     C*
     C     ECKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
      ***** BEG ADD ******* 12/21/00 **********************************
      *
      *  Chapter Code
      *
     C     CPKY          KLIST
     C                   KFLD                    EBCO#
     C                   KFLD                    EBFND
     C                   KFLD                    EBCHAP
      *
      ***** END ADD ******* 12/21/00 **********************************
      **** BEGIN ADD **** 08/14/01 ************************************
      *
      *  User Parameter File
      *
     C     UPKY          KLIST
     C                   KFLD                    XXSTAF
      **** END   ADD **** 08/14/01 ************************************
     C*_______________________________________________________________
     C*
     C     *LIKE         DEFINE    AHCO#         XXCO#
     C     *LIKE         DEFINE    AHFND         XXFND
     C     *LIKE         DEFINE    AGFEIN        XXFEIN
     C     *LIKE         DEFINE    AGMOD         XXMOD
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    AHPROD        XXPROD
     C     *LIKE         DEFINE    AHEFDT        X7FRDT
     C     *LIKE         DEFINE    AHEFDT        X7TODT
     C     *LIKE         DEFINE    AHEFDT        X7RNDT
     C     *LIKE         DEFINE    ARNWRT        XXNEW
     C     *LIKE         DEFINE    ARRNRT        XXRNEW
     C     *LIKE         DEFINE    ELPRTD        XXEXCO
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C*** BEGIN CHG ***  11/27/01  ************************************
     C*********  *LIKE     DEFN ECST      XXST
     C     *LIKE         DEFINE    EMPST         XXST
     C*** END CHG *****  11/27/01  ************************************
     C     *LIKE         DEFINE    EBAGT#        SVAGT#
     C     *LIKE         DEFINE    BBACSZ        XXCSZ
      ***** BEG ADD ******* 12/21/00 **********************************
     C     *LIKE         DEFINE    BBPROD        XFPROD
      ***** END ADD ******* 12/21/00 **********************************
     C     *LIKE         DEFINE    BBPNAM        XFPNAM
     C     *LIKE         DEFINE    BBEMNM        XFENAM
     C     *LIKE         DEFINE    ORFIL#        XXPRT
      **** BEGIN ADD **** 08/14/01 ************************************
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
      **** END   ADD **** 08/14/01 ************************************
     C*_______________________________________________________________
     C*
      ***** BEG CHG ******* 12/21/00 **********************************
     C*************        MOVE *BLANKS   XPENAM 23        EMPLR NAME (PRINT)
     C                   MOVE      *BLANKS       XPENAM           40
      ***** END CHG ******* 12/21/00 **********************************
     C                   MOVE      *BLANKS       XFENAM
      ***** BEG CHG ******* 12/21/00 **********************************
     C*************        MOVE *BLANKS   XPPNAM 18        PRODUCER NAME (PRINT)
     C                   MOVE      *BLANKS       XPPNAM           46
      ***** END CHG ******* 12/21/00 **********************************
      ***** BEG ADD ******* 12/21/00 **********************************
     C                   Z-ADD     *ZERO         XFPROD
      ***** END ADD ******* 12/21/00 **********************************
     C                   MOVE      *BLANKS       XFPNAM
      ***** BEG CHG ******* 12/21/00 **********************************
     C*************        MOVE *BLANKS   XPANAM 18        AGENT NAME (PRINT)
     C                   MOVE      *BLANKS       XPANAM           46
      ***** END CHG ******* 12/21/00 **********************************
     C                   MOVE      *BLANK        XXPRT
     C                   MOVE      *ZEROS        XXCNT             7 0
     C                   MOVE      *ZEROS        XXECNT            7 0
     C                   MOVE      *ZEROS        XXPREM           11 2
     C                   MOVE      *ZEROS        XXTOTL           13 2
     C                   MOVE      *ZEROS        XXTOTI           13 2
      ***** BEG ADD ******* 03/15/00 **********************************
     C                   MOVE      *ZEROS        LRECNT           11 0
     C                   MOVE      *ZEROS        LRTOTL           13 2
     C                   MOVE      *ZEROS        LRTOTI           13 2
     C                   MOVE      *ALL'_'       LINE            132
      ***** END ADD ******* 03/15/00 **********************************
     C*_______________________________________________________________
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQEMSL            1
     C                   PARM                    QQRNDT            8
     C                   PARM                    QQSORT            2
     C*** BEGIN ADD *** 03/12/98 **************************************
     C                   PARM                    QQSRT2            2
     C*** END   ADD *** 03/12/98 **************************************
     C                   PARM                    QQEMNF            2
     C                   PARM                    QQEMNT            2
     C                   PARM                    QQEM#F            9
     C                   PARM                    QQEM#T            9
     C                   PARM                    QQAGNF            2
     C                   PARM                    QQAGNT            2
     C                   PARM                    QQAG#F            5
     C                   PARM                    QQAG#T            5
     C                   PARM                    QQTYPO            1
     C                   PARM                    QQCXAG            1
     C                   PARM                    QQQUE            10
     C*****BEG ADD*****05/06/10****************************************
     C                   PARM                    QQASDT            8
     C*****END ADD*****05/06/10****************************************
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
     C******BEG ADD*****05/06/10***********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '133'
     C     QQASDT        IFNE      '99999999'
     C                   MOVE      *ON           *IN55
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *OFF          *IN55
     C                   ENDIF
     C******END ADD*****05/06/10***********************************
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFRDT        X8FRDT            8 0
     C                   MOVE      QQTODT        X8TODT            8 0
     C                   MOVE      QQRNDT        X8RNDT            8 0
     C                   MOVE      QQEM#F        XXEM#F            9 0
     C                   MOVE      QQEM#T        XXEM#T            9 0
     C                   MOVE      QQAG#F        XXAG#F            5 0
     C                   MOVE      QQAG#T        XXAG#T            5 0
     C******BEG ADD*****05/06/10***********************************
     C                   MOVE      QQASDT        XXASDT            8 0
     C******END ADD*****05/06/10***********************************
     C*
     C                   Z-ADD     X8FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7FRDT
     C*
     C                   Z-ADD     X8TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7TODT
     C*
     C                   Z-ADD     X8RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7RNDT
     C*
     C                   EXSR      SR1ST
     C*
     C*===============================================================
     C*   MAIN LOOP
     C*===============================================================
     C*
     C*  Read Work file records
     C*
     C                   READ      PEB01                                  60
     C*
     C*     *** If no records in workfile
     C*
     C     *IN60         IFEQ      *ON
     C***  BEGIN ADD  ***  05/12/98  *********************************
     C*
     C*     *** If Single agent selection, get agent info
     C*
     C     XXAG#T        IFEQ      XXAG#F
     C*
     C*       ** Get Agent Assignment record
     C*
     C     AAKY1         CHAIN     PAA01                              61
     C*
     C*       ** Get Agent name and address
     C*
     C                   Z-ADD     XXAG#F        EBAGT#
     C*
     C                   EXSR      SRANAM
     C*
     C                   ENDIF
     C***  END   ADD  ***  05/12/98  *********************************
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C                   ENDIF
     C*
     C                   RETURN
     C*
     C                   ENDIF
     C*
     C*     *** While read Work File record
     C*
      *** BEG DEL ******* 08/20/01 **************************************
     C********** *LOVAL    SETLLPEB01
      *** END DEL ******* 08/20/01 **************************************
     C*
     C     *IN60         DOWEQ     *OFF
     C*
      *** BEG DEL ******* 08/20/01 **************************************
     C**********           READ PEB01                    60
      *** END DEL ******* 08/20/01 **************************************
     C*
     C   60              LEAVE
     C*
     C*     *** If NO Agent
     C*
      *** BEG CHG **** 01/08/02 *****************************************
     C***************  20      EBAGT#    IFEQ *ZERO
     C***************                     ITER
     C***************               ENDIF                           EBAGT#=*ZERO
     C     *IN20         IFEQ      *OFF
     C     EBAGT#        ORNE      *ZERO
      *** END CHG **** 01/08/02 *****************************************
     C*
     C*     *** If Agent Break, and Agent Sort
     C*
     C   20EBAGT#        IFNE      SVAGT#
     C                   EXSR      AGTBRK
     C                   ENDIF
     C***  BEGIN ADD  ***  06/04/98  *********************************
     C*
     C*     *** If Agent Break, and Employer Sort
     C*
     C  N20EBAGT#        IFNE      SVAGT#
     C*
     C*       ** Get Agent Assignment record
     C*
     C     AAKY          CHAIN     PAA01                              61
     C*
     C*       ** Get Agent name and address
     C*
     C                   EXSR      SRANAM
     C*
     C                   Z-ADD     EBAGT#        SVAGT#
     C*
     C                   ENDIF
     C***  END   ADD  ***  06/04/98  *********************************
     C*
     C*     *** If Agent CX, and Agent Sort
     C*
      *** BEG CHG **** 01/08/02 ************************************************
     C***20***** QQCXAG    IFNE 'Y'
     C********** AACNDT    ANDNE*ZEROS
     C********** AACNDT    ANDLTX7RNDT
     C**********           ITER
     C     QQCXAG        IFEQ      'Y'
     C     AACNDT        OREQ      *ZEROS
     C     AACNDT        ORGE      X7RNDT
     C                   EXSR      SRDET
      *** END CHG **** 01/08/02 ************************************************
     C                   ENDIF
      *** BEG DEL **** 01/08/02 ************************************************
     C****************     EXSR SRDET
      *** END DEL **** 01/08/02 ************************************************
      *** BEG ADD **** 01/08/02 ************************************************
     C                   ENDIF
      *** END ADD **** 01/08/02 ************************************************
      *** BEG ADD ******** 08/20/01 **********************************
     C                   READ      PEB01                                  60
      *** END ADD ******** 08/20/01 **********************************
     C                   ENDDO
     C*
     C***  BEGIN ADD  ***  10/21/98  *********************************
     C     XXECNT        IFGE      1
     C***  END   ADD  ***  10/21/98  *********************************
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C                   EXCEPT    TOTAL
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  10/21/98  *********************************
     C                   ENDIF
     C***  END   ADD  ***  10/21/98  *********************************
      ***** BEG ADD ******* 03/15/00 **********************************
      *
      * Output report totals when agent sort selected.
      *
     C   20LRECNT        IFGE      1
      *
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C                   SETON                                        OF
     C                   EXCEPT    LRTOT
     C                   ENDIF
      *
     C                   ENDIF
      *
      ***** END ADD ******* 03/15/00 **********************************
     C*_______________________________________________________________
     C*
     C*  End of Processing
     C*
     C     END           TAG
      /Free
       // ***** Beg Add ***** 08/26/14 ****************************************
       xxPrt = '0';
       Chain orKy POR01;
       If %found;
          If qqTypo = 'P' or qqTypo = 'B';
             Except Route;
          Endif;
       Endif;
       // ***** End Add ***** 08/26/14 ****************************************
      /End-free
     C*
     C                   MOVE      *ON           *INLR
     C*_______________________________________________________________
     C*
     C*===============================================================
     C*  AGTBRK - Agent Break
     C*===============================================================
     C*
     CSR   AGTBRK        BEGSR
     C*
     C*  Print Totals for agent
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
     C     XX1ST         IFEQ      'Y'
     C                   MOVE      'N'           XX1ST
     C                   ELSE
     C                   EXCEPT    TOTAL
     C                   SETON                                        OF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     *ZERO         XXECNT
     C                   Z-ADD     *ZERO         XXTOTL
     C                   Z-ADD     *ZERO         XXTOTI
     C*
     C*  Get Agent Assignment record
     C*
     C     AAKY          CHAIN     PAA01                              61
     C*
     C*  Get Agent name and address
     C*
     C                   EXSR      SRANAM
     C*
     C                   Z-ADD     EBAGT#        SVAGT#
     C*
     C     XX1STA        IFEQ      'Y'
     C     QQCXAG        ANDEQ     'N'
     C*
     C     AACNDT        IFLT      X7RNDT
     C     AACNDT        ANDNE     *ZEROS
     C                   MOVE      'Y'           XX1ST
     C                   ELSE
     C                   MOVE      'N'           XX1STA
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C*  RATEPC - SUBROUTINE TO FIND A RATE#'S NEW & RENEW %'S
     C*===============================================================
     C*
     C     RATEPC        BEGSR
     C*
     C                   Z-ADD     *ZERO         XXNEW
     C                   Z-ADD     *ZERO         XXRNEW
     C*
     C     ARKY          SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  63
     C*
     C     *IN63         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY1         READPE    PAR01                                  63
     C                   ENDDO
     C*
     C     *IN63         IFEQ      *OFF
     C                   Z-ADD     ARNWRT        XXNEW
     C                   Z-ADD     ARRNRT        XXRNEW
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C*  SR1ST - Set-up and Initialize
     C*===============================================================
     C*
     CSR   SR1ST         BEGSR
     C*
     C*  Initialize
     C*
      ***** BEG ADD ******* 12/21/00 **********************************
     C                   MOVE      *ALL'_'       LINE1           198
      ***** END ADD ******* 12/21/00 **********************************
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     *ZERO         SVAGT#
     C                   MOVE      *ON           *INOF
     C                   MOVE      'Y'           XX1ST             1
     C                   MOVE      'Y'           XX1STA            1
     C*
     C     QQSORT        IFEQ      'AA'
     C     QQSORT        OREQ      'AN'
     C                   MOVE      *ON           *IN20
     C                   ELSE
     C                   MOVE      *OFF          *IN20
     C                   ENDIF
     C*
     C     QQEMNT        COMP      *BLANKS                            2121
     C     XXEM#T        COMP      *ZEROS                             2222
     C     QQAGNT        COMP      *BLANKS                            2323
     C     XXAG#T        COMP      *ZEROS                             2424
     C*
     C*  SET UP AND INITIALIZE
     C*
     C     QQQUE         COMP      *BLANK                             090908
     C*
     C*  GET USER, COMPANY, AND GROUP INFORMATION
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF
     C                   ENDIF
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C*  GET THE DISTRIBUTION INFORMATION AND PRINT
     C*
     C                   MOVE      '0'           XXPRT
     C*
     C     ORKY          CHAIN     POR01                              60
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60
     C*
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      ORTITL
     C                   ENDIF
     C*
     C                   ENDIF
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*******     QQTYPO        IFEQ      'P'
     C*******     QQTYPO        OREQ      'B'
     C*******  N60              EXCEPT    ROUTE
     C*******                   ENDIF
      ***** End Del ***** 08/26/14 *********************************************
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
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* SRACK - Get Agent History record as of status date
     C*===============================================================
     C*
     C     SRACK         BEGSR
     C*
     C     AHKY          SETGT     PAH01
     C     AHKY1         READPE    PAH01                                  61
     C*
     C     *IN61         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY1         READPE    PAH01                                  61
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C*  SRANAM - SUBROUTINE TO GET AGENT NAME & ADDRESS
     C*===============================================================
     C*
     C     SRANAM        BEGSR
     C*
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVE      *BLANKS       XXCSZ
     C*
     C                   MOVE      *BLANKS       AGNAME
     C                   MOVE      *BLANKS       AGCONT
     C                   MOVE      *BLANKS       AGMAD1
     C*
     C*  GET AGENT MASTER RECORD
     C*
     C     AGKY1         CHAIN     PAG01                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C*  PUT TOGETHER CITY, STATE, AND ZIP CODE FOR PRINTING
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVEA     AGMST         SZ(1)
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C*
     C     XXZPM2        IFNE      *ZERO
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   ENDIF
     C*
     C                   MOVEA     SZ            XXSZ
     C                   MOVEL(P)  AGMCTY        XXLAST
     C                   MOVEL(P)  XXSZ          XXFRST
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST           56
     C                   PARM                    XXFRST           15
     C                   PARM      '1'           XXTERM            1
     C*
     C                   MOVEL     XXLAST        XXCSZ
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C*  SRDET - Write Detail Record
     C*===============================================================
     C*
     CSR   SRDET         BEGSR
     C*
     C*  Get Agent Assignment record
     C*
     C                   MOVE      *BLANKS       XPANAM
     C  N20AAKY          CHAIN     PAA01                              61
     C  N20
     CANN61              MOVEL(P)  AANAME        XPANAM
     C*
     C*  Get Employer Master record
     C*
     C     EMKY          CHAIN     PEM01                              63
     C*
     C     *IN63         IFEQ      *OFF
     C                   MOVEL(P)  EMNAME        XPENAM
     C                   MOVEL(P)  EMNAME        XFENAM
     C                   ENDIF
     C*
     C*  GET ESTIMATED ANNUAL PREMIUM FROM EMP DETAIL FILE
     C*  ESTIMATED ANNUAL PREM = EST NORMAL PREM + EXPENSE CONSTANT
     C*  XXPREM = ELENMP + XXEXCO
     C*
     C                   Z-ADD     *ZERO         XXPREM
     C                   Z-ADD     *ZERO         XXEXCO
     C                   Z-ADD     *ZERO         XXPFDT
      ***** BEG ADD ******* 08/11/99 **********************************
      *
      *  DETERMINE IF EMPLOYER IS CANCELLED.
      *
     C     EBCNDT        IFEQ      *ZERO
     C     EBCNDT        ORGT      X7RNDT
     C                   SETOFF                                       89
      *
     C                   ELSE
      *
     C                   SETON                                        89
     C                   ENDIF
      *
      ***** END ADD ******* 08/11/99 **********************************
     C*
     C*  Get Employer Detail record
     C*
     C     ELKY          CHAIN     PEL01                              63
      ***** BEG ADD ******* 08/11/99 **********************************
     C   89              Z-ADD     *ZERO         ELINMP
      ***** END ADD ******* 08/11/99 **********************************
     C*
     C     *IN63         IFEQ      *OFF
     C*
     C                   Z-ADD     ELPFDT        XXPFDT
     C*
     C     ELEMNP        IFEQ      'Y'
     C     ELPRTD        OREQ      *ZERO
     C*
     C                   Z-ADD     ELPRTD        XXEXCO
     C*
     C                   ELSE
     C*
     C*  GET CORRECT EXPENSE CONSTANT
     C*
     C*** BEGIN CHG ***  11/27/01  ************************************
     C*********  EMPST     IFNE *BLANKS
     C*********            MOVE EMPST     XXST
     C*********            ELSE
     C*********            MOVE EMMST     XXST
     C*********            ENDIF                           EMPST<>*BLANKS
     C*
     C*********  ECKY      SETGTPEC01
     C*********  ECKY1     REDPEPEC01                    62
     C*
     C*********  *IN62     IFEQ *OFF
     C*********            Z-ADDECEXCO    XXEXCO
     C*********            ENDIF                           *IN62=*OFF
     C                   Z-ADD     ELPRTF        XXEXCO
     C*** END CHG *****  11/27/01  ************************************
     C*
     C                   ENDIF
     C*
     C     ELENMP        ADD       XXEXCO        XXPREM
     C                   ADD       XXPREM        XXTOTL
     C                   ADD       ELINMP        XXTOTI
      ***** BEG ADD ******* 03/15/00 **********************************
     C                   ADD       XXPREM        LRTOTL
     C                   ADD       ELINMP        LRTOTI
      ***** END ADD ******* 03/15/00 **********************************
     C*
     C                   ENDIF
     C*
     C                   EXSR      SRACK
     C*
     C                   EXSR      SRPNAM
     C*
     C                   EXSR      RATEPC
     C*
     C                   Z-ADD     AHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXAHEF            8 0
      ***** BEG ADD ******* 12/21/00 **********************************
      *
     C                   Z-ADD     EBPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1PFDT            8 0
      *
     C     SRERR         COMP      *ON                                    71
      *
      ***** END ADD ******* 12/21/00 **********************************
     C*
     C                   ADD       1             XXCNT
     C                   ADD       1             XXECNT
      ***** BEG ADD ******* 03/15/00 **********************************
     C                   ADD       1             LRECNT           11 0
      ***** END ADD ******* 03/15/00 **********************************
      ***** BEG DEL ******* 08/11/99 **********************************
     C**********
     C***DETERMINE IF EMPLOYER IS CANCELLED.
     C**********
     C********** EBCNDT    IFEQ *ZERO
     C********** EBCNDT    ORGT X7RNDT
     C**********           SETOF                     89
     C**********           ELSE
     C**********           SETON                     89
     C**********           ENDIF                           *ZERO=EBCFDT>X7RNDT
      ***** END DEL ******* 08/11/99 **********************************
     C*
     C*  USE OUTPUT TYPE FLAG TO DETERMINE IF RECORD SHOULD BE WRITTEN
     C*  TO FILE, QSYSPRT OR BOTH.
     C*
      ***** BEG ADD ******* 04/25/07 **********************************
     C*
     C     XXAG#F        IFNE      0
     C*
     C     EBAGT#        IFGE      XXAG#F
     C     EBAGT#        ANDLE     XXAG#T
     C*
      ***** END ADD ******* 04/25/07 **********************************
     C     QQTYPO        IFEQ      'F'
     C     QQTYPO        OREQ      'B'
     C                   EXSR      WRITE
     C                   ENDIF
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   EXCEPT    DETAIL
     C*
     C                   ENDIF
      ***** BEG ADD ******* 04/25/07 **********************************
     C                   ENDIF
     C                   ELSE
     C     QQTYPO        IFEQ      'F'
     C     QQTYPO        OREQ      'B'
     C                   EXSR      WRITE
     C                   ENDIF
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   EXCEPT    DETAIL
     C*
     C                   ENDIF
     C                   ENDIF
      ***** END ADD ******* 04/25/07 **********************************
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C*  SRPNAM - SUBROUTINE TO LOAD THE PRODUCER NAME
     C*===============================================================
     C*
     C     SRPNAM        BEGSR
     C*
     C                   Z-ADD     AAFEIN        XXFEIN
     C                   Z-ADD     AAMOD         XXMOD
     C                   MOVE      *BLANKS       XPPNAM
     C                   MOVE      *BLANKS       XFPNAM
      ***** BEG ADD ******* 12/21/00 **********************************
     C                   Z-ADD     *ZERO         XFPROD
      ***** END ADD ******* 12/21/00 **********************************
     C*
     C     APKY          CHAIN     PAP01                              63
     C*
     C     *IN63         IFEQ      *OFF
     C*
     C                   MOVEL(P)  APFSN         XXFRST
     C                   MOVEL(P)  APLSN         XXLAST
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM      '1'           XXTERM
     C*
     C                   MOVEL(P)  XXLAST        XPPNAM
     C                   MOVEL(P)  XXLAST        XFPNAM
      ***** BEG ADD ******* 12/21/00 **********************************
     C                   Z-ADD     APPROD        XFPROD
      ***** END ADD ******* 12/21/00 **********************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* WRITE - SUBROUTINE TO WRITE DETAIL TO AGT-EMP WORK FILE
     C*===============================================================
     C*
     C     WRITE         BEGSR
     C*
     C                   CLEAR                   PBB01
     C*
     C                   Z-ADD     EBCO#         BBCO#
     C                   Z-ADD     EBFND         BBFND
     C                   MOVEL(P)  FNNAME        BBFNAM
     C                   Z-ADD     X7FRDT        BBFRDT
     C                   Z-ADD     X7TODT        BBTODT
     C  N89              MOVEL     'A'           BBASTS
     C   89              MOVEL     'C'           BBASTS
     C                   Z-ADD     X7RNDT        BBSTDT
     C*_______________________________________________________________
     C*
     C                   Z-ADD     EBAGT#        BBAGT#
     C                   Z-ADD     AHEFDT        BBAEDT
     C                   MOVEL(P)  AGNAME        BBANAM
     C                   MOVEL(P)  AGMAD1        BBAADR
     C                   MOVEL(P)  XXCSZ         BBACSZ
     C                   MOVEL(P)  AGHCON        BBHCON
     C                   MOVEL(P)  AGSECY        BBSECY
     C                   MOVEL(P)  AGCONT        BBCONT
      ***** BEG ADD ******* 05/06/10 **********************************
     C                   MOVEL     EBAGTL        BBAGTL
      ***** END ADD ******* 05/06/10 **********************************
     C*
     C     AGMKRP        CHAIN     PUP02                              61
     C  N61              MOVEL(P)  UPUSNM        BBMREP
     C*
     C     AGSVCR        CHAIN     PUP02                              61
     C  N61              MOVEL(P)  UPUSNM        BBSVCR
     C*
     C                   MOVEL(P)  AAGRP         BBGRP
     C                   Z-ADD     AHRT#         BBRT#
     C                   Z-ADD     XXNEW         BBNWRT
     C                   Z-ADD     XXRNEW        BBRNRT
      ***** BEG ADD ******* 12/21/00 **********************************
     C                   Z-ADD     XFPROD        BBPROD
      ***** END ADD ******* 12/21/00 **********************************
     C                   MOVEL(P)  XFPNAM        BBPNAM
     C*_______________________________________________________________
     C*
     C                   Z-ADD     EBFYR         BBFYR
     C                   Z-ADD     EBEMP#        BBEMP#
     C                   Z-ADD     EBDIV         BBDIV
     C                   MOVEL(P)  XFENAM        BBEMNM
     C                   Z-ADD     XXPFDT        BBEFDT
     C                   Z-ADD     XXPREM        BBPREM
     C                   Z-ADD     ELINMP        BBINMP
     C                   Z-ADD     EBCNDT        BBCNDT
     C                   Z-ADD     EBCHAP        BBCHAP
     C                   MOVE      *BLANK        BBSTAT
     C                   MOVEL(P)  Q1PGM         BBPGM
     C                   MOVEL(P)  Q1USER        BBUSER
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BBDT
     C*
     C                   MOVEL     DSTIME        BBTIME
     C*
     C***** BEGIN ADD **** 11/30/99 ***********************************
     C                   MOVEL     AAFEIN        BBFEIN
     C                   MOVEL     AAMOD         BBMOD
     C***** END   ADD **** 11/30/99 ***********************************
      ***** BEG ADD ******* 12/21/00 **********************************
      *
      *  Chapter description
      *
     C     CPKY          CHAIN     PCP01                              60
     C  N60              MOVEL(P)  CPDESC        BBCDES
     C   60              MOVEL(P)  *BLANKS       BBCDES
      *
      ***** END ADD ******* 12/21/00 **********************************
      ***** BEG ADD ******* 04/08/01 **********************************
      *
     C                   MOVE      EBJRST        BBJRST
      ***** Beg Chg ******* 11/04/14 **********************************
      ***  ELKY          CHAIN     PEL01                              60
      **N60              MOVE      ELRTCD        BBRTCD
     C                   MOVE      ELRTCD        BBRTCD
      ***** End Chg ******* 11/04/14 **********************************
      *
      ***** END ADD ******* 04/08/01 **********************************
      **** BEGIN ADD **** 08/14/01 ************************************
     C                   MOVE      *BLANKS       BBEUWN
     C                   MOVE      *BLANKS       BBAUWN
      *
     C                   MOVEL(P)  EMUWTR        BBEUWT
     C                   MOVEL(P)  EMUWTR        XXSTAF
     C     UPKY          CHAIN     PUP03                              60
     C  N60              MOVEL(P)  UPUSNM        BBEUWN
      *
     C                   MOVEL(P)  AAUWTR        BBAUWT
     C                   MOVEL(P)  AAUWTR        XXSTAF
     C     UPKY          CHAIN     PUP03                              60
     C  N60              MOVEL(P)  UPUSNM        BBAUWN
      **** END   ADD **** 08/14/01 ************************************
     C*
     C                   WRITE     PBB01
     C*
     C                   ENDSR
     C*_______________________________________________________________
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
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
     O*** BEG   CHG*****05/06/10 **************************************
     O*****   E  2             ROUTE
     O********* E            ROUTE          1
     O*** END   CHG*****05/06/10 **************************************
     O*********      08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O*** BEG   CHG*****05/06/10 **************************************
     O*****   E  2             ROUTE
     O          E            ROUTE          1
     O*** END   CHG*****05/06/10 **************************************
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
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
     O*********         09   QQQUE               30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O*** BEG  CHG***** 05/06/10 **************************************
     O          E            ROUTE       1  2
     O******* E 22             ROUTE
     O*** END  CHG***** 05/06/10 **************************************
     O                                           16 'Selection:'
     O          E            ROUTE          2
     O                                           26 'Company  . . . . :'
     O                       XXCO#         Z   +001
     O          E            ROUTE          2
     O                                           26 'Group  . . . . . :'
     O                       XXFND         Z   +001
     O          E            ROUTE          2
     O                                           26 'Policy Eff Range :'
     O                       X8FRDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       X8TODT            +001 '  /  /    '
     O          E            ROUTE          2
     O                                           26 'Status Selection :'
     O                       QQEMSL            +001
     O****BEG  CHG***** 05/06/10 **************************************
     O******  E  2             ROUTE
     O******                             26 'Status as of Date:'
     O******                   X8RNDT  +001 '  /  /    '
     O          E   N55      ROUTE          2
     O                                           26 'Status as of Date:'
     O                       X8RNDT            +001 '  /  /    '
     O          E    55      ROUTE          2
     O                                           26 'Emp Sts as of Dt.:'
     O                       X8RNDT            +001 '  /  /    '
     O*
     O****END  CHG***** 05/06/10 **************************************
     O****BEG  ADD***** 05/06/10 **************************************
     O*
     O          E    55      ROUTE          2
     O                                           26 'Agt Sts as of Dt.:'
     O                       XXASDT            +001 '  /  /    '
     O*
     O****END   ADD*****05/06/10 **************************************
     O***BEGIN  ADD *** 03/12/98 **************************************
     O*
     O          E            ROUTE          2
     O                                           26 'Primary Sort . . :'
     O                       QQSORT            +001
     O*
     O          E            ROUTE          2
     O                                           26 'Secondary Sort   :'
     O                       QQSRT2            +001
     O*
     O*** END   ADD *** 03/12/98 **************************************
     O*
     O*** BEGIN CHG *** 03/12/98 **************************************
     O********E* 2     20      ROUTE
     O          E            ROUTE          2
     O*** END   CHG *** 03/12/98 **************************************
     O*
     O                                           26 'Include CX Agents:'
     O                       QQCXAG            +001
     O          E            ROUTE          2
     O                                           15 'Ranges:'
     O          E            ROUTE          1
     O                                           26 'Employer Name  :'
     O               21      QQEMNF            +001
     O               21                        +010 '-'
     O               21      QQEMNT            +002
     O              N21                          31 '*ALL'
     O          E            ROUTE          1
     O                                           26 'Employer # . . :'
     O               22      XXEM#F        Z   +001
     O               22                        +001 '-'
     O               22      XXEM#T        Z   +002
     O              N22                          31 '*ALL'
     O          E            ROUTE          1
     O                                           26 'Agent Name . . :'
     O               23      QQAGNF            +001
     O               23                        +010 '-'
     O               23      QQAGNT            +002
     O              N23                          31 '*ALL'
     O*
     O*** BEGIN CHG *** 03/12/98 **************************************
     O********E* 1             ROUTE
     O          E            ROUTE          2
     O*** END   CHG *** 03/12/98 **************************************
     O*
     O                                           26 'Agent #  . . . :'
     O               24      XXAG#F        Z   +001
     O               24                        +006 '-'
     O               24      XXAG#T        Z   +002
     O              N24                          31 '*ALL'
     O*
     O*** BEGIN ADD *** 03/12/98 **************************************
     O          E            ROUTE          1
     O                                           26 'Type of Output   :'
     O                       QQTYPO            +001
     O*** END   ADD *** 03/12/98 **************************************
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                25 '(C) Copyright Progressi'
     O***                              +000 've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*_______________________________________________________________
      ***** BEG DEL ******* 12/21/00 **********************************
      *******
     O******* E  102           HEAD
     O*******                  DSTIME     8 '  :  :  '
     O*******                  CONAME    85
     O*******                  DSDATEY  132
     O******* E  1             HEAD
     O*******                  Q1PGM     10
     O*******                  ORTITL    95
     O*******                           126 'PAGE:'
     O*******                  PAGE  Z  132
     O******* E  2             HEAD
     O*******                             6 'As of:'
     O*******                  X8RNDTY +001
     O*******                            64 'Effective Date Range:'
     O*******                  X8FRDT  +001 '  /  /    '
     O*******                          +001 '-'
     O*******                  X8TODT  +001 '  /  /    '
     O*******                           130 'Status Selection:'
     O*******                  QQEMSL  +001
     O******* E  1             HEAD
     O*******                            06 'Group:'
     O*******                  XXFND Z +003
     O*******                  FNNAME  +002
     O******* E 11     20      HEAD
     O*******                            06 'Agent:'
     O*******                  EBAGT#Z +001
     O*******                  AGNAME  +002
     O*******                            89 'Marketing Rep:'
     O*******                  AGCONT  +001
     O******* E  1     20      HEAD
     O*******                  AGMAD1    54
     O******* E  1     20      HEAD
     O*******                  XXCSZ     54
     O*_______________________________________________________________
     O*******
     O******* E 11             HEAD1
     O*******                            63 'Estimated'
     O*******                            93 'Last Agent'
     O******* E 00             HEAD1
     O*******                            20 '____________________'
     O*******                          +000 '____________________'
     O*******                          +000 '____________________'
     O*******                          +000 '____________________'
     O*******                          +000 '____________________'
     O*******                          +000 '____________________'
     O*******                          +000 '____________'
     O******* E  2             HEAD1
     O*******                             9 'Employer'
     O*******                            15 'Div'
     O*******                            20 'Name'
     O*******                            43 'Chp'
     O*******                            46 'CX'
     O*******                            64 'Annual Prem'
     O*******                            82 'Inforce Prem'
     O*******                            92 'Eff Date'
     O*******                            99 'Rate#'
     O*******                           106 'New%'
     O*******                           113 'Renew%'
     O*******          20               122 'Producer'
     O*******         N20               119 'Agent'
     O*_______________________________________________________________
     O*******
     O******* E  1             DETAIL
     O*******                  EBEMP#Z    9
     O*******                  EBDIV Z   15
     O*******                  XPENAM    39
     O*******                  EBCHAP    43
     O*******          89                46 'CX'
     O*******                  XXPREMK   65
     O*******                  ELINMPK   83
     O*******                  XXAHEF    93 '  /  /    '
     O*******                  AHRT# Z   98
     O*******                  XXNEW 4  106
     O*******                  XXRNEW4  113
     O*******          20      XPPNAM   132
     O*******         N20      XPANAM   132
     O*_______________________________________________________________
     O*******
     O******* E 1              TOTAL
     O*******                            20 'Number of Employers:'
     O*******                  XXECNTK   30
     O*******                            45 'Total:'
     O*******                  XXTOTLK   65
     O*******                  XXTOTIK   83
     O*_______________________________________________________________
     O*******
     O******* E 31             NOREC
     O*******                            21 '*** NO RECORD FOUND'
     O*******                            38 'FOR THE SELECTED'
     O*******                            52 'CRITERIA. ***'
      ******BEG ADD ******* 03/15/00 **********************************
      *******
     O******* E  102           LRTOT
     O*******                  DSTIME     8 '  :  :  '
     O*******                  CONAME    85
     O*******                  DSDATEY  132
      *******
     O******* E  1             LRTOT
     O*******                  Q1PGM     10
     O*******                  ORTITL    95
     O*******                           126 'PAGE:'
     O*******                  PAGE  Z  132
      *******
     O******* E  2             LRTOT
     O*******                             6 'As of:'
     O*******                  X8RNDTY +001
     O*******                            64 'Effective Date Range:'
     O*******                  X8FRDT  +001 '  /  /    '
     O*******                          +001 '-'
     O*******                  X8TODT  +001 '  /  /    '
     O*******                           130 'Status Selection:'
     O*******                  QQEMSL  +001
      *******
     O******* E  1             LRTOT
     O*******                            06 'Group:'
     O*******                  XXFND Z +003
     O*******                  FNNAME  +002
      *******
     O******* E 11             LRTOT
     O*******                            63 'Estimated'
      *******
     O******* E 00             LRTOT
     O*******                  LINE    +000
      *******
     O******* E  2             LRTOT
     O*******                            64 'Annual Prem'
     O*******                            82 'Inforce Prem'
      *******
     O******* E 1              LRTOT
     O*******                          +000 'REPORT TOTALS'
     O******* E 1              LRTOT
     O*******                          +005 'Number of Employers:'
     O*******                  LRECNTK +001
     O*******                  LRTOTLK   65
     O*******                  LRTOTIK   83
      *_______________________________________________________________
      *******
      ******END ADD ******* 03/15/00 **********************************
      ***** END DEL ******* 12/21/00 **********************************
      ***** BEG ADD ******* 12/21/00 **********************************
      *
      *
     O          E            HEAD           1 02
     O                       DSTIME               8 '  :  :  '
     O                       CONAME             119
     O                       DSDATE        Y    198
      *
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
      *
     O          E            HEAD           2
     O*** BEG  CHG******05/06/10 **************************************
     O******                              6 'As of:'
     O******                   X8RNDTY +001
     O              N55                           6 'As of:'
     O              N55      X8RNDT        Y   +001
     O               55                          23 'Employer Sts as of Dt.:'
     O               55      X8RNDT        Y   +001
     O*** END  CHG******05/06/10 **************************************
     O                                           97 'Effective Date Range:'
     O                       X8FRDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       X8TODT            +001 '  /  /    '
     O                                          196 'Status Selection:'
     O                       QQEMSL            +001
     O****BEG   ADD*****05/06/10 **************************************
     O*
     O          E    55      HEAD           2
     O                                           23 'Agent Sts as of Dt. . :'
     O                       XXASDT            +001 '  /  /    '
     O*
     O****END   ADD*****05/06/10 **************************************
      *
     O          E            HEAD           1
     O                                           06 'Group:'
     O                       XXFND         Z   +001
     O                       FNNAME            +002
      *
     O          E    20      HEAD        1  1
     O                                           06 'Agent:'
     O                       EBAGT#        Z   +001
     O                       AGNAME            +002
     O                                           89 'Marketing Rep:'
     O                       AGCONT            +001
      *
     O          E    20      HEAD           1
     O                       AGMAD1              54
      *
     O          E    20      HEAD           1
     O                       XXCSZ               54
     O*_______________________________________________________________
     O*
     O          E            HEAD1       1  1
     O                                           61 'Policy'
     O                                           72 'Policy YR'
     O                                           94 'Estimated'
     O                                          125 'Last Agent'
      *
     O          E            HEAD1       0  0
     O                       LINE1             +000
      *
     O          E            HEAD1          2
     O                                            9 'Employer'
     O                                           15 'Div'
     O                                           20 'Name'
     O                                           60 'Year'
     O                                           71 'Eff Date'
     O                                           77 'Chp'
     O                                           80 'CX'
     O                                           95 'Annual Prem'
     O                                          113 'Inforce Prem'
     O                                          124 'Eff Date'
     O                                          131 'Rate#'
     O                                          137 'New%'
     O                                          144 'Renew%'
     O               20                         154 'Producer'
     O*** BEG   CHG *** 05/06/10 **************************************
     O******          N20               151 'Agent'
     O              N20N55                      151 'Agent'
     O*** END   CHG *** 05/06/10 **************************************
     O*** BEG   ADD *** 05/06/10 **************************************
     O               55                         166 'Commissionable Agent'
     O*** END   ADD *** 05/06/10 **************************************
      *_______________________________________________________________
      *
     O          E            DETAIL         1
     O                       EBEMP#        Z   +000
     O                       EBDIV         Z   +001
     O                       XPENAM            +001
     O                       EBFYR         Z   +001
     O              N71      X1PFDT            +002 '  /  /    '
     O                       EBCHAP            +002
     O               89                        +001 'CX'
     O                       XXPREM        K   +001
     O                       ELINMP        K   +003
     O                       XXAHEF            +001 '  /  /    '
     O                       AHRT#         Z   +002
     O                       XXNEW         4   +001
     O                       XXRNEW        4   +001
     O               20      XPPNAM             192
     O              N20      XPANAM             192
     O*_______________________________________________________________
     O*
     O          E            TOTAL       1
     O                                           20 'Number of Employers:'
     O                       XXECNT        K     30
     O                                           75 'Total:'
     O                       XXTOTL        K     96
     O                       XXTOTI        K    114
     O*_______________________________________________________________
     O*
     O          E            NOREC       3  1
     O                                           21 '*** NO RECORD FOUND'
     O                                           38 'FOR THE SELECTED'
     O                                           52 'CRITERIA. ***'
      *
     O          E            LRTOT          1 02
     O                       DSTIME               8 '  :  :  '
     O                       CONAME             119
     O                       DSDATE        Y    198
      *
     O          E            LRTOT          1
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
      *
     O          E            LRTOT          2
     O                                            6 'As of:'
     O                       X8RNDT        Y   +001
     O                                           97 'Effective Date Range:'
     O                       X8FRDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       X8TODT            +001 '  /  /    '
     O                                          196 'Status Selection:'
     O                       QQEMSL            +001
      *
     O          E            LRTOT          1
     O                                           06 'Group:'
     O                       XXFND         Z   +001
     O                       FNNAME            +002
      *
     O          E            LRTOT       1  1
     O                                           95 'Estimated'
      *
     O          E            LRTOT       0  0
     O                       LINE              +000
      *
     O          E            LRTOT          2
     O                                           96 'Annual Prem'
     O                                          114 'Inforce Prem'
      *
     O          E            LRTOT       1
     O                                         +000 'REPORT TOTALS'
     O          E            LRTOT       1
     O                                         +005 'Number of Employers:'
     O                       LRECNT        K   +001
     O                       LRTOTL        K     96
     O                       LRTOTI        K    114
      *_______________________________________________________________
      *
      ***** END ADD ******* 12/21/00 **********************************
