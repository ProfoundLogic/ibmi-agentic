     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W7851R1 - REOPEN CLAIM HISTORY REPORT
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM ALLOWS THE USER TO PRINT THE REOPEN CLAIM  **
     H**     HISTORY REPORT FOR A PARTICULAR FUND, FUND YEAR,        **
     H**     WITHING SPECIFIED DATE RANGE AND # OF MONTH AFTER       **
     H**     CLOSURE FOR SELECTED CLAIM TYPE CRITERIA.               **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W7851C1                                                 **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H** CREATED  08/04/98   C:90   PROJ:WC841                       **
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 03/28/14  C:283  Support: WC045 85189                         *
     H*                  Select a Claim only if it is re-opened       *
     H*                  before the "To Date".                        *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant info     *
     H*                                                               *
     H* 12/15/06 C:132 Project: 139WGE                                *
     H*                Use PARQDT instead of PAPODT if PARQDT not zero*
     H*                                                               *
     H* 11/17/00 C:53  PROJECT: SUPP # 24817                          *
     H*                MOVED ACC DT TO WITHIN THE DO LOOP             *
     H*                                                               *
     H* 04/10/00 C:118 PROJECT: WC987                                 *
     H*                LOAD ALTERNATE COMPANY NAME FROM FUND MASTER   *
     H*                                                               *
     H* 04/07/00 C:61  SUPPORT WA065 22288                            *
     H*              - Only present information for Group listed in   *
     H*                user parms.                                    *
     H*                                                               *
     H* 12/03/99 C:90 PROJECT: WC577                                  *
     H*               LOAD PROPER DATE FORMAT FOR RODT.               *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*     06   -      LOSTIME CLAIM TYPE                            *
     H*     07   -      NO LOSTIME CLAIM TYPE                         *
     H*     08   -      BATCH PROCESSING (FOR ROUTING PAGE)           *
     H*     09   -      DEMAND PROCESSING (FOR ROUTING PAGE)          *
     H*     11   -      CONDITION FIRST TIME THROUGH PROGRAM          *
     H*     22   -      CODITIONS FOR PAGE BREAK                      *
     H*     60   -      GENERAL CHAINING                              *
     H*     78   -      RECORDS TO PRINT                              *
     H*     79   -      NO SELECT RECORDS TO PRINT                    *
     H*                                                               *
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**                                                             **
     H**    SMCOL (PCO01): CO#                                       **
     H**    WDPAL5 (PPA01): CO#/FND/FYR/CASE/TRDT/SEQ#               **
     H**    WMUPL (PUP01): USER                                      **
     H**    WMUPL1(PUP02): STAF                                      **
     H**    WMFNL (PFN01): CO#/FND                                   **
     H**    SMORL  (POR01) ORPNAM,ORFIL#                             **
     H**    WMCML1 (PCM01): CO#/FND/FYR/CASE                         **
     H**    WMCDL  (PCD01): SS#                                      **
     H**    WMCHL (PCH01): CO#/FND/FYR/CASE/EFDT/TIME                **
     H**    WMCHL3(PCH02): CO#/FND/EFDT/FYR/CASE/TIME                **
     H**    WMPDL  (PPD01): FEIN/MOD                                 **
     H**    WFROL (PRO01): CO#/FND/FYR/CASE/EFDT/TIME                **
     H*****************************************************************
     F*
     FSMCOL     IF   E           K DISK
     F* * * Company File
     FWDPAL5    IF   E           K DISK
     F* * * Payment detail file
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     F* * * User Parameter File
     FWMFNL     IF   E           K DISK
     F* * * Fund File
     F*
     FSMORL     IF   E           K DISK
     F* * * Batch Routing File
     FWMCML1    IF   E           K DISK
     F* * * Claim Master file
     FWMCDL     IF   E           K DISK
     F* * * Claim demographic file
     FWMCHL     IF   E           K DISK
     F* * * Claim history file
     FWMCHL3    IF   E           K DISK
     F                                     RENAME(PCH01:PCH02)
     F* * * Claim history file
     FWMPDL     IF   E           K DISK
     F* * * Payee demographic file
     FWFROL     IF A E           K DISK
     F* * * Reopen claim history file
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     F* * * Output File

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CO                     1     40
     D  CONAME                 1     40
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
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** BEG ADD ****** 05/25/07 **********************************
     D XXATTX          C                   CONST('*** NO AUTHORITY ***')

     IPCH02
     I              CHCO#                       C2CO#
     I              CHFND                       C2FND
     I              CHEFDT                      C2EFDT
     I              CHFYR                       C2FYR
     I              CHCASE                      C2CASE
     I              CHTIME                      C2TIME
      *** BEG ADD ***** 03/28/14 **************************************
     I              CHOCST                      C2OCST
      *** END ADD ***** 03/28/14 **************************************
      *** END ADD ****** 05/25/07 **********************************

     C*
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKY2         KLIST
     C                   KFLD                    CMSUPR
     C*
     C*   KEY LIST FOR THE REPORT OVERRIDE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*   KEY LIST FOR CLAIM HISTORY
     C*
     C     CHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFRDT
     C*
     C     CHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     CHKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     CHKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXTIME
     C*
     C     ROKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR CLAIMANT DEMOGRAPHICS FILE.
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE PAYMENT FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXFRDT
     C*
     C*   KEY LIST FOR THE PAYEE DEMOGRAPHIC FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    PAFEIN
     C                   KFLD                    PAMOD
     C*
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CHEFDT        XXTODT
     C     *LIKE         DEFINE    CHEFDT        XXFRDT
     C     *LIKE         DEFINE    CHTIME        XXTIME
     C     *LIKE         DEFINE    CHEFDT        XXEFDT
     C     *LIKE         DEFINE    CHEFDT        C2PDT
     C     *LIKE         DEFINE    CHEFDT        CHPDT
     C     *LIKE         DEFINE    CHCASE        XXCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQMCL             3
     C                   PARM                    QQROPT            1
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
     C                   PARM                    QQRPT             1
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
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFDT         XXFROM            8 0
     C                   MOVE      QQTDT         XXTO              8 0
     C                   MOVE      QQRPT         XXRPT             1
     C*
     C                   Z-ADD     XXFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     XXTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C                   MOVE      QQMCL         XXMCL             3 0
     C                   MOVE      QQROPT        XXROP             1
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C  N11              EXSR      SR1ST
     C*
     C*  START READING CLAIM HISTORY FILE
     C*
     C     CHKY          SETLL     PCH02
     C     CHKY3         READE     PCH02                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C*  IF THERE IS A REOPEN WITHIN THE TIME PERIOD, PRINT
     C*
     C                   MOVE      '1'           *IN78
     C                   MOVE      'N'           XXPRNT            1
     C*
      *** BEG CHG ***** 03/28/14 **************************************
     C***************     CHOCST        IFEQ      'O'
      /Free
       If C2ocst = 'O' And C2efdt <= xxTODT;
      /End-Free
      *** END CHG ***** 03/28/14 **************************************
      ***** BEG DEL ******* 04/07/00 **********************************
     C**********           MOVE C2CO#     XXCO#
     C**********           MOVE C2FND     XXFND
      ***** END DEL ******* 04/07/00 **********************************
     C                   MOVE      C2FYR         XXFYR
     C                   MOVE      C2CASE        XXCASE
     C                   MOVE      C2EFDT        XXEFDT
     C                   MOVE      C2TIME        XXTIME
     C     CHKY4         CHAIN     PCH01                              60
     C     CHKY2         READPE    PCH01                                  60
     C     *IN60         IFEQ      '0'
     C     CHOCST        ANDEQ     'C'
     C**
     C     XXMCL         IFNE      *ZERO
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXMDY             8 0
     C*
     C                   Z-ADD     C2EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         C2MDY             8 0
     C*
     C                   MOVE      XXMDY         QQPDT             8 0
     C                   MOVE      C2MDY         QQMDY             8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      ' '           QQERR             1
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQPDT             8 0
     C                   PARM                    QQMDY             8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1
     C*
     C     QQERR         IFEQ      *OFF
     C                   MOVE      QQDAYS        XXDAYS            5 0
     C     XXDAYS        DIV(H)    30            XXMNTH            5 2
     C*
     C                   ELSE
     C*
     C                   MOVE      'Y'           XXPRNT
     C                   ENDIF                                                  QQERR IFEQ *OFF
     C*
     C*   INCLUDE CLAIMS THAT HAVE BEEN CLOSED FOR SAME OR MORE
     C*   THAN THE NUMBER OF MONTHS SPECIFIED ON PROMPT
     C*
     C     XXMNTH        IFGE      XXMCL
     C                   MOVE      'Y'           XXPRNT
     C                   ENDIF                                                  XXMNTH IFGE MCL
     C                   ELSE
     C                   MOVE      'Y'           XXPRNT
     C                   ENDIF                                                  XXMCL IFNE 0
     C*
     C*** BEG ADD ***** 03/28/14 **************************************

      /Free
       If CHefdt <> 0 And CHefdt <= XXTODT And
          CHefdt >= XXFRDT ;
          //Do Nothing, Process as it is coming from prevoius lines.
       Else;
          xxprnt = 'N';
       Endif;
      /End-Free
     C*** END ADD ***** 03/28/14 **************************************
     C                   ENDIF                                                  CHOCST IFEQ 'C'
     C*
     C                   ENDIF                                                  CHOCST IFEQ 'O'
     C*
     C     ROKY          CHAIN     PRO01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'N'           XXPRNT
     C                   ENDIF                                                  *IN60 = OFF
     C*
     C     XXPRNT        IFEQ      'Y'
     C*
     C*   GET CLAIMANT NAME & ACCUM TOTAL PAID & INCURRED
     C*
     C                   MOVE      *BLANKS       XXNAME           25
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C     XXROP         IFNE      'B'
     C     XXROP         IFEQ      'L'
     C     CMLTFL        ANDNE     'I'
     C     XXROP         OREQ      'N'
     C     CMLTFL        ANDEQ     'I'
     C                   GOTO      ENDA
     C                   ENDIF                                                  XXROP IFEQ L
     C                   ENDIF                                                  XXROP IFNE B
     C*
     C                   Z-ADD     CMACDT        ROACDT
     C*
     C                   XFOOT     CMA           XXPAID           11 2
     C                   XFOOT     CMR           XXRES            11 2
     C     XXPAID        ADD       XXRES         XXTOTI           11 2
     C                   Z-ADD     XXPAID        ROTOTP
     C                   Z-ADD     XXTOTI        ROTOTI
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLNAM           56
     C                   MOVEL     CDFSN         QQFNAM           15
     C                   MOVEL     ' '           QQTERM            1
     C*
     C                   CALL      'SRNAME'
     C*
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        XXNAME
     C                   MOVEL     XXNAME        ROCNAM
     C                   ENDIF                                                  *IN60 IFEQ 0
     C*
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      XXFND         ##FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      XXCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVEL     XXATTX        XXNAME
     C                   MOVEL     XXNAME        ROCNAM
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C*   GET SUPERVISOR NAME
     C*
     C                   MOVE      *BLANKS       XXSUPR           15
     C     UPKY2         CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXSUPR
     C                   MOVEL     XXSUPR        ROSUPR
     C                   ENDIF                                                  *IN60 IFEQ 0
     C*
     C                   MOVE      *BLANKS       ROTRCD
     C                   MOVE      *BLANKS       ROTRTP
     C                   MOVE      *BLANKS       RORECD
     C                   MOVE      *BLANKS       RORETP
     C                   Z-ADD     *ZEROS        ROTRDT
     C                   Z-ADD     *ZEROS        ROPODT
     C                   Z-ADD     *ZEROS        ROSFRD
     C                   Z-ADD     *ZEROS        ROSTOD
     C                   MOVE      *BLANKS       ROCHK#
     C                   MOVE      *BLANKS       ROPNAM
     C                   Z-ADD     *ZEROS        ROAMT
     C                   MOVE      XXFRDT        ROFRDT
     C                   MOVE      XXTODT        ROTODT
     C                   MOVE      XXCO#         ROCO#
     C                   MOVE      XXFND         ROFND
     C                   MOVE      XXFYR         ROFYR
     C                   MOVE      XXCASE        ROCASE
      **** BEGIN CHG **** 12/03/99 ************************************
     C******************** MOVE DSDATE    RODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RODT
      **** END   CHG **** 12/03/99 ************************************
     C                   MOVE      DSTIME        ROTIME
     C                   MOVE      Q1PGM         ROPGM
     C                   MOVE      Q1USER        ROUSER
     C*
     C     CHKY2         CHAIN     PCH01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       ROEXPL           50
     C                   MOVEL     CHEXPL        ROEXPL
     C                   Z-ADD     CHEFDT        ROEFDT
     C                   MOVE      CHOCST        ROOCST
     C*
     C                   WRITE     PRO01
     C     CHKY2         READE     PCH01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ '0'
     C*
     C*   GET PAYMENT HISTORY INFO
     C*
     C     PAKY1         SETLL     PPA01
     C     PAKY          READE     PPA01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     PATRDT        ANDLE     XXTODT
     C*
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
      ***** BEG CHG ***** 12/15/06 ***************************************
     C     PARQDT        IFNE      *ZEROS
     C                   Z-ADD     PARQDT        SRDTI
     C                   ELSE
     C                   Z-ADD     PAPODT        SRDTI
     C                   ENDIF
     C************         Z-ADDPAPODT    SRDTI
      ***** END CHG ***** 12/15/06 ***************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT            8 0
     C*
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXSFRM            8 0
     C*
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXSTO             8 0
     C*
     C                   MOVE      *BLANKS       XXPNAM           25
     C                   MOVE      PAFEIN        XXFEIN            9
     C     PAFEIN        IFEQ      0
     C     CMSS#         OREQ      XXFEIN
     C                   MOVE      XXNAME        XXPNAM
     C                   ELSE
     C     PDKY          CHAIN     PPD01                              61
     C                   MOVEL     PDNAME        XXPNAM
     C                   ENDIF                                                  PAFEIN IFEQ 0
     C*
     C                   MOVE      PATRDT        ROTRDT
      ***** BEG CHG ***** 12/15/06 ***************************************
     C     PARQDT        IFNE      *ZEROS
     C                   MOVE      PARQDT        ROPODT
     C                   ELSE
     C                   MOVE      PAPODT        ROPODT
     C                   ENDIF
     C************         MOVE PAPODT    ROPODT
      ***** END CHG ***** 12/15/06 ***************************************
     C                   MOVE      PAFRDT        ROSFRD
     C                   MOVE      PATODT        ROSTOD
     C                   MOVEL     XXPNAM        ROPNAM
     C                   MOVE      PATRCD        ROTRCD
     C                   MOVE      PATYPE        ROTRTP
     C                   MOVE      PARECD        RORECD
     C                   MOVE      PARETP        RORETP
     C                   MOVE      PACHK#        ROCHK#
     C                   Z-ADD     PAAMT         ROAMT
     C*
     C                   WRITE     PRO01
     C*
     C     PAKY          READE     PPA01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ '0'
     C*
     C     ENDA          TAG
     C                   ENDIF                                                  *IN60 IFEQ 0
     C                   ENDIF                                                  XXPRNT IFEQ Y
     C*
     C*   GET NEXT CASE
     C*
      ***** BEG DEL ******* 04/07/00 **********************************
     C************         READ PCH02                    60
      ***** END DEL ******* 04/07/00 **********************************
      ***** BEG ADD ******* 04/07/00 **********************************
     C     CHKY3         READE     PCH02                                  60
      ***** END ADD ******* 04/07/00 **********************************
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*    PRINT REOPEN RECORDS
     C*
     C     XXRPT         IFNE      'F'
     C*
     C     *LOVAL        SETLL     PRO01
     C                   READ      PRO01                                  60
     C                   Z-ADD     0             XXHCAS            7 0
     C*
     C*** BEG DEL *** 11/17/00 ****************************
     C***********          Z-ADDROACDT    SRDTI
     C***********          CALL 'SRCVTDAT'DTMDYY
     C***********          MOVELSRDTO     YYACDT  80
     C*** END DEL *** 11/17/00 ****************************
     C*
     C     *IN60         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C                   ENDIF                                                  *IN60 IFEQ 1
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*** BEG ADD *** 11/17/00 ****************************
     C                   Z-ADD     ROACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         YYACDT            8 0
     C*** END ADD *** 11/17/00 ****************************
     C*
     C                   Z-ADD     ROEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         YYEFDT            8 0
     C                   MOVE      *OFF          *IN78
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *ON           *IN78
     C                   MOVE      *OFF          *INOA
     C                   ENDIF                                                  *INOA IFEQ *ON
     C*
     C     ROCASE        IFNE      XXHCAS
     C                   MOVE      *ON           *IN78
     C                   Z-ADD     ROCASE        XXHCAS            7 0
     C                   ENDIF                                                  ROCASE NE XXHCA
     C*
     C     ROAMT         IFNE      *ZEROS
     C*
     C                   Z-ADD     ROTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         YYTRDT            8 0
     C*
     C                   Z-ADD     ROPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         YYPODT            8 0
     C*
     C                   Z-ADD     ROSFRD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         YYSFRD            8 0
     C*
     C                   Z-ADD     ROSTOD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         YYSTOD            8 0
     C*
     C                   EXCEPT    PAYH
     C                   ELSE
     C                   EXCEPT    DET
     C                   ENDIF                                                  PAAMT IFNE '0'
     C*
     C                   MOVE      *BLANKS       ROEXPL
     C                   READ      PRO01                                  60
     C                   ENDDO                                                  IN60 = 0
     C*
     C                   ENDIF                                                  XXRPT IFNE 'F'
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKy POR01;
       If %found and xxRpt <> 'F';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      *ON           *INLR
     C*
     C*
     C*
     C*=========================================================**
     C*   SET-UP / INITIALIZE          S R 1 S T                **
     C*=========================================================**
     CSR   SR1ST         BEGSR
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C                   TIME                    DSDTTM
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
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
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C     XXROP         COMP      'L'                                    06
     C     XXROP         COMP      'N'                                    07
     C*
     C     QQOUTQ        IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION.
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C     XXRPT         ANDNE     'F'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION                           D.
     C*
      ***** Beg Del ***** 08/26/14 *********************************
     C********           EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************
     C                   END
     C                   MOVE      '1'           *IN11
     C                   MOVE      *ON           *INOA
     C                   ENDSR
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************
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
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************
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
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************
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
     O**********     08      OROUTQ              30
     O**********        09   QQOUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************
     O          E            ROUTE          2 34
     O                                           29 'Group               :'
     O                       XXFND         Z     47
     O                       FNNAME             110
     O          E            ROUTE          2
     O                                           29 'Reopened Date Range :'
     O                       XXFROM        Y     53
     O                                           55 '-'
     O                       XXTO          Y     66
     O          E            ROUTE          2
     O                                           29 'Months After Closure:'
     O                       XXMCL         Z     45
     O          E            ROUTE          1
     O                                           29 'Claim Type Selection:'
     O                       XXROP               45
     O                                           65 'L = Lost Time'
     O          E            ROUTE          1
     O                                           68 'N = No Lost Time'
     O          E            ROUTE          1
     O                                           60 'B = Both'
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS, INC.'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             130
     O                       DSDATE        Y    198
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             133
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME              72
     O                                          103 'DATE RANGE:'
     O                       XXFROM        Y    112
     O                                          114 '-'
     O                       XXTO          Y    125
     O          E            HEAD           2
     O                                           04 'YEAR'
     O                                           12 'CASE#'
     O                                           28 'CLAIMANT NAME'
     O                                           67 'ACC DATE      TOTAL PAID'
     O                                           83 'TOTAL INCURRED'
     O                                          123 'USER            DATE & '
     O                                          141 'STATUS EXPLANATION'
     O          E            HEAD           1
     O                                           28 'TRANSACTION'
     O                                           38 'RESERVE'
     O                                           94 'SERVICE'
     O          E            HEAD           1
     O                                           27 'CODE TYPE'
     O                                           39 'CODE TYPE'
     O                                           60 'TRANS DATE'
     O                                           72 'POST DATE'
     O                                           87 'FROM'
     O                                           97 'TO'
     O                                          128 'CHECK#'
     O                                          139 'PAYEE'
     O                                          189 'AMOUNT'
     O          E    78      DET         1
     O          E            DET         1
     O               78      ROFYR               03
     O               78      ROCASE        Z     12
     O               78      ROCNAM              45
     O               78      YYACDT        Y     52
     O               78      ROTOTP        J     67
     O               78      ROTOTI        J     82
     O               78      ROSUPR             115
     O                       YYEFDT        Y    124
     O                       ROOCST             127
     O                       ROEXPL             180
     O          E            PAYH        1
     O                       ROTRCD              21
     O                       ROTRTP              26
     O                       RORECD              33
     O                       RORETP              38
     O                       YYTRDT              60 '  /  /    '
     O                       YYPODT              73 '  /  /    '
     O                       YYSFRD              90 '  /  /    '
     O                       YYSTOD             102 '  /  /    '
     O                       ROCHK#        Z    128
     O                       ROPNAM             174
     O                       ROAMT         J    190
     O          E            NOREC       2  1
     O                                           26 '*** NO RECORDS MEET THE'
     O                                           49 'SELECTION CRITERIA ***'
