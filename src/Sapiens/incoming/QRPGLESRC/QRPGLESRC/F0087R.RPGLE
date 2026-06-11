     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE F0087R - RECONSIDERATION BREAKDOWN                       *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    PROGRAM TO PROCESS THE WRITE OF A REFUND A/R RECORD FOR A  *
     H*    RECON EOMB AND TO CALL REFUND PRINT PROGRAM                *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    F0086R - EOMB REQUEST                                      *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    F0650R         EOMB SEARCH BY EOMB#                        *
     H*    F0651R         EOMB SEARCH BY CASE#                        *
     H*    F7087R         REFUND REQUEST PRINT                        *
     H*                                                               *
     H*  PROGRAMMER: C:20               REQ/PROJ: 12/60MAL            *
     H*  CREATED: 01/23/95                                            *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*  20    MCR0721 - INVALID EOB# ENTERED                         *
     H*  21    MCR0418 - PAYMENT AMOUNT MUST BE GREATER THAN ZERO     *
     H*  22    MCR0419 - TOTAL AMOUNTS MUST BE EQUAL TO TOTP FOR EOMB *
     H*  23    MCR0865 - MRA AMOUNT EXCEEDS MRA REDUCT ON EOMB        *
     H*  24    MCR0866 - OTHER AMOUNT EXCEEDS OTHER REDUCT ON EOMB    *
     H*  60    GENERAL CHAINING                                       *
     H*  75    CHAIN TO SUBFILE                                       *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  05/15/95   C:20   REQ/PROJ: 12/60MAL                         *
     H*                    1. WHEN CALCULATING THE REDUCTIONS ADD     *
     H*                       ALL OTHER RECON AMOUNTS FOR SAME EOMB   *
     H*  08/22/95   C:02   REQ/PROJ: 60MAP                            *
     H*                       OTHER REDUCTIONS AND NEW BRS FEE CALC   *
     H*  10/23/95   C:32   REQ/PROJ: 60MAL                            *
     H*                       ADD AMT APPLIED TO MRA REDUCTIONS AND   *
     H*                       ADD AMT APPLIED TO OTHER REDUCTIONS     *
     H*                                                               *
     H*  12/28/95   C:43   REQ/PROJ: SUPPORT CALL                     *
     H*                       DON'T WRITE ADJUSTMENT TO WDPAP IF WE   *
     H*                       HAVE NO PAYMENTS.                       *
     H*                                                               *
     H*  05/21/96   C:53   REQ/PROJ: MC060 # 3937                     *
     H*                       GET CORRECT FEIN/MOD                    *
     H*****************************************************************
     FF0087F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     F*
     FFAEOL4    UF   E           K DISK
     FFAERP     O    E           K DISK
     FFAERL1    IF   E           K DISK
     F                                     RENAME(PER01:PER02)
     FSMUPL     IF   E           K DISK
     F*** BEGIN ADD *** 04/18/95 ***********************************************
     FFAE1L     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FWDPAL8    IF   E           K DISK
     FWARPLA    IF   E           K DISK
     FWDPAL     UF A E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWMCML1    UF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F*** ADD 8/22/95 ***********************************
     FFMF1L     IF   E           K DISK
     F*** END ADD 8/22/95 ***********************************
     F*** BEGIN ADD 05/21/96 ***********************************
     FWMNRL1    IF   E           K DISK
     F*** END   ADD 05/21/96 ***********************************

     D*
     D                 DS
     D  CD                     1     20
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
     D  PA                     1     90  2
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
     D  RR                     1     90  2
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
     F*** END ADD *** 04/18/95 *************************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXPGM                  1      8
     D  XXTYPE                 4      5
     D*** BEGIN ADD *** 04/19/95 ***********************************************
     D*
     D DESC            C                   CONST('ADJUSTMENT FOR RECON-
     D                                     SIDERATION')
     D*** END ADD *** 04/19/95 *************************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE EOMB TRANSACTION FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*   KEY LIST FOR THE RECON EOMB TRANSACTION FILE
     C*
     C     ERKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    X1SEQ#
     C*
     C*** BEGIN ADD *** 05/15/95 ***********************************************
     C     ERKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*** END ADD *** 05/15/95 *************************************************
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*** BEGIN ADD *** 04/18/95 ***********************************************
     C*
     C*  KEY LIST FOR THE EOB AUXILLARY FILE
     C*
     C     E1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C* THE WCMS PAYMENT DETAIL FILE
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C                   KFLD                    XXCHK#
     C                   KFLD                    XXEOB#
     C*
     C     PAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXSEQ#
     C*
     C* KEY LIST FOR THE WCMS CLAIM MASTER FILE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C* THE WCMS REQUEST FOR PAYMENT FILE
     C*
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C                   KFLD                    XXEOB#
     C*** END ADD *** 04/18/95 *************************************************
     C*** BEGIN ADD *** 08/22/95 ***********************************************
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*** END ADD *** 08/22/95 *************************************************
     C*** BEGIN ADD 05/21/96 ***********************************
     C*
     C*   KEY LIST FOR GROUP PROVIDER MASTER
     C*
     C     NRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXGRTY            1
     C*
     C*** END   ADD 05/21/96 ***********************************
     C*
     C*------------------------*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C*------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQAMT             9
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQRTCD            1
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
     C* LIKE DEFINE SECTION
     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN                         10A
     C     *LIKE         DEFINE    EOCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    EOSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    EOFND         XXFND                          3.0N
     C     *LIKE         DEFINE    EOFYR         XXFYR                          3.0N
     C     *LIKE         DEFINE    EOCASE        XXCASE                         7.0N
     C     *LIKE         DEFINE    EOEOB#        XXEOB#                         9.0N
     C     *LIKE         DEFINE    EOSYS         XXSYS                          1A
     C*** BEGIN ADD *** 04/18/95 ***********************************************
     C     *LIKE         DEFINE    PABR2         XXBR2
     C     *LIKE         DEFINE    PABR7         XXBR7
     C     *LIKE         DEFINE    PACHK#        XXCHK#
     C     *LIKE         DEFINE    ERREFA        XXREFA
     C     *LIKE         DEFINE    ERRECA        XXRECA                         ADD 05/15/95
     C     *LIKE         DEFINE    FNWCF#        XXWCF#
     C     *LIKE         DEFINE    ERREFA        XXMRA                          ADD 10/23/95
     C     *LIKE         DEFINE    ERREFA        XXOTHR                         ADD 10/23/95
     C                   Z-ADD     *ZEROS        X1                5 0
     C*** END ADD *** 04/18/95 *************************************************
     C*** BEGIN ADD 05/21/96 ***********************************
     C     *LIKE         DEFINE    NRFEIN        XXFEIN
     C     *LIKE         DEFINE    NRMOD         XXMOD
     C*** END   ADD 05/21/96 ***********************************
     C*
     C                   MOVEL     QQPRDS        SCPRDS
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQAMT         XXAMT             9 2
     C                   MOVE      QQSS#         XXSS#             9
     C                   MOVE      QQACDT        XXACDT            8 0
     C                   MOVE      QQEOB#        XREOB#            9 0
     C                   MOVE      *ZEROS        X1SEQ#            5 0
     C                   MOVE      *ZEROS        XXSEQ#            5 0
     C                   MOVE      *BLANKS       XXSUMM            1
     C                   TIME                    DSDTTM
     C*** BEGIN ADD *** 04/18/95 ***********************************************
     C*
     C* CHAIN TO WCMC GROUP MASTER FIEL TO GET WCMS XREF FUND NUMBER
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     FNWCF#        XXWCF#
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXWCF#
     C                   END
     C*** END ADD *** 04/18/95 *************************************************
     C*
     C* CHAIN TO THE USER PARM FILE TO GET OUTQ
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPOUTQ        Q1QUE            10
     C                   END
     C*
     C*** BEGIN ADD 05/21/96 ***********************************
     C* CHAIN TO THE GROUP PROVIDER MASTER FILE
     C*
     C                   MOVE      'B'           XXGRTY
     C     NRKEY         CHAIN     PNR01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     NRFEIN        XXFEIN
     C                   Z-ADD     NRMOD         XXMOD
     C                   END
     C*** END   ADD 05/21/96 ***********************************
     C*===============================================================
     C* INITIALIZATION    AREA
     C*
     C     TOP           TAG
     C                   EXSR      INZSR
     C*
     C*===============================================================
     C*
     C     MAINLN        TAG
     C*
     C*===============================================================
     C* SCREEN EXECUTION AREA
     C*
     C     STRT          TAG
     C                   EXSR      SCREXC
     C*
     C*===============================================================
     C* HANDLE F KEYS IF PRESSED
     C*
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                      END *IN11
     C*
     C*===============================================================
     C*
     C                   MOVE      'Y'           ENTFL             1
     C                   EXSR      EDIT
     C*
     C                   EXSR      WRTRC
     C*** BEGIN DEL *** 04/18/95 ***********************************************
     C*
     C* CALL CLP PROGRAM TO CALL RPG PROGRAM TO PRINT REFUND REQUEST
     C*
     C**********           MOVE XXCO#     Q1CO#   3
     C**********           MOVE XXSUB#    Q1SUB#  5
     C**********           MOVE XXFND     Q1FND   3
     C**********           MOVE XXFYR     Q1FYR   3
     C**********           MOVE XXCASE    Q1CASE  7
     C**********           MOVE XREOB#    Q1REOB  9
     C**********           MOVE 'D'       Q1MODE  1
     C*
     C**********           CALL 'F7087C'
     C**********           PARM           Q1CO#
     C**********           PARM           Q1SUB#
     C**********           PARM           Q1FND
     C**********           PARM           Q1FYR
     C**********           PARM           Q1CASE
     C**********           PARM           Q1REOB
     C**********           PARM           Q1QUE
     C**********           PARM           Q1MODE
     C*** BEGIN ADD *** 04/18/95 ***********************************************
     C*
     C*===============================================================
     C* TERMINATION OF PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
     C*====================================================================*
     C* * *   EDIT SUBROUTINE                                    E D I T ***
     C*====================================================================*
     C     EDIT          BEGSR
     C*
     C                   MOVE      RRN2          SRRN2             5 0
     C                   Z-ADD     *ZEROS        TAMT
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXT1          TAG
     C                   ADD       1             K
     C     K             CHAIN     WDW01S                             75
     C     *IN75         IFEQ      '0'
     C*
     C     SCOEOB        IFEQ      *BLANKS
     C*** BEGIN CHG *** 10/23/95 **************************************
     C********** SCAMT     ANDEQ*ZEROS
     C     SCMRA         ANDEQ     *ZEROS
     C     SCOTHR        ANDEQ     *ZEROS
     C*** END   CHG *** 10/23/95 **************************************
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN21
     C                   UPDATE    WDW01S
     C                   GOTO      NXT1
     C                   ELSE
     C*
     C* IF ANYTHING ON LINE ENTERED THE EOB# MUST BE VALID
     C*
     C                   MOVE      SCOEOB        XXEOB#
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      STRT
     C                   ELSE
     C                   EXCEPT    PEO01E
     C                   MOVE      '0'           *IN20
     C                   END                                                    END *IN60
     C*
     C* IF EOB# NOT ZEROS THEN PAYMENT AMOUNT MUST BE ENTERED
     C*
     C     SCOEOB        IFNE      *BLANKS
     C*** BEGIN CHG *** 10/23/95 **************************************
     C********** SCAMT     IFEQ *ZEROS
     C     SCMRA         IFEQ      *ZEROS
     C     SCOTHR        ANDEQ     *ZEROS
     C*** END   CHG *** 10/23/95 **************************************
     C                   MOVE      '1'           *IN21
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      '0'           *IN21
     C                   END                                                    END SCMRA
     C                   END                                                    END SCOEOB
     C*
     C*** BEGIN ADD *** 10/23/95 **************************************
     C*  VERIFY AMOUNTS ENTERED ARE LARGE ENOUGH TO HANDLE RECON
     C*
     C                   MOVE      '0'           *IN23
     C     SCOEOB        IFNE      *BLANKS
     C     EOAMTC        SUB       EOAMT         XXMRA
     C     XXMRA         SUB       EOOTHR        XXMRA
     C     XXMRA         IFLT      SCMRA
     C                   MOVE      '1'           *IN23
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   MOVE      '0'           *IN24
     C     SCOEOB        IFNE      *BLANKS
     C     EOOTHR        IFLT      SCOTHR
     C                   MOVE      '1'           *IN24
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     K             #@$2
     C                   GOTO      STRT
     C                   END
     C                   END
     C*** END   ADD *** 10/23/95 **************************************
     C*
     C* ADD UP ALL ENTERED PAYMENT AMOUNTS TO COMPARE AGAINST TOTAL PAID
     C* FOR RECON EOMB
     C*
     C*** BEGIN CHG *** 10/23/95 **************************************
     C******************** ADD  SCAMT     TAMT    92
     C                   ADD       SCMRA         TAMT              9 2
     C                   ADD       SCOTHR        TAMT              9 2
     C*** END   CHG *** 10/23/95 **************************************
     C*
     C     *IN20         IFEQ      '0'
     C     *IN21         ANDEQ     '0'
     C                   UPDATE    WDW01S
     C                   END
     C*
     C                   GOTO      NXT1
     C                   END                                                    END SCOEOB
     C                   END                                                    END *IN75
     C*
     C* IF THIS SUBROUTINE IS CALLED BY PRESSING ENTER VERFIY TOTAL
     C* PAYMENTS ENTERED EQUALS THE TOTAL PAID FOR RECON EOMB
     C*
     C     ENTFL         IFEQ      'Y'
     C     TAMT          IFNE      XXAMT
     C     1             CHAIN     WDW01S                             75
     C     *IN75         IFEQ      '0'
     C                   MOVE      '1'           *IN22
     C                   UPDATE    WDW01S
     C                   MOVE      SRRN2         RRN2
     C                   Z-ADD     1             #@$2
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C                   MOVE      SRRN2         RRN2
     C*
     C                   ENDSR
     C/EJECT
     C*====================================================================*
     C* * *   INZSR SUBROUTINE                                    I N I T ***
     C*====================================================================*
     C     INZSR         BEGSR
     C*
     C*** CLEAR SUBFILE
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             XXRRN2            5 0
     C*
     C                   EXSR      LOAD
     C*
     C                   ENDSR
     C/EJECT
     C*====================================================================*
     C* * *   LOAD SUBROUTINE                                    L O A D ***
     C*====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN21
     C                   MOVE      '0'           *IN23                          ADD 10/23/95
     C                   MOVE      '0'           *IN24                          ADD 10/23/95
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*   Position Cursor flag - used to check if the 1st subfile RRN
     C*     of the current page needs to be saved.
     C*
     C                   MOVE      'Y'           PCFL              1
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        7             X                 5 0
     C*
     C                   MOVE      *BLANKS       SCOEOB
     C*** BEGIN CHG *** 10/23/95 **************************************
     C******************** Z-ADD*ZEROS    SCAMT
     C                   Z-ADD     *ZEROS        SCMRA
     C                   Z-ADD     *ZEROS        SCOTHR
     C*** END   CHG *** 10/23/95 **************************************
     C                   ADD       1             RRN2
     C*
     C*   Save 1st subfile RRN of the current page
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2
     C                   MOVE      'N'           PCFL
     C                   END
     C*
     C                   WRITE     WDW01S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     XXRRN2        #@$2
     C*
     C                   ENDSR
     C/EJECT
     C*===============================================================
     C     SCREXC        BEGSR
     C*===============================================================
     C*
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01C'
     C     Q2RECF        OREQ      'WDW01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     WDW01C
     C                   WRITE     WDW01
     C                   EXFMT     WDW01C
     C                   MOVE      '0'           *IN01
     C     SVRRN2        IFNE      0
     C                   Z-ADD     SVRRN2        RRN2
     C                   Z-ADD     0             SVRRN2            5 0
     C                   END
     C                   END
     C*
     C*
     C                   ENDSR
     C*
     C/EJECT
     C*====================================================================*
     C* * *   WRITE RECON FILE INFOMATION                      W R T R C ***
     C*====================================================================*
     C     WRTRC         BEGSR
     C*
     C                   MOVE      RRN2          SRRN2             5 0
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXT4          TAG
     C                   ADD       1             K
     C     K             CHAIN     WDW01S                             75
     C     *IN75         IFEQ      '0'
     C*
     C     SCOEOB        IFNE      *BLANKS
     C*
     C                   MOVE      SCOEOB        XXEOB#
     C*** BEGIN ADD *** 05/15/95 ***********************************************
     C                   Z-ADD     *ZEROS        XXRECA
     C     ERKY          CHAIN     PER02                              70
     C     *IN70         DOWEQ     '0'
     C                   ADD       ERRECA        XXRECA
     C     ERKY          READE     PER02                                  70
     C                   END
     C*** END ADD *** 05/15/95 *************************************************
     C                   MOVE      *HIVAL        X1SEQ#
     C     ERKEY         SETGT     PER02
     C                   READP     PER02                                  60
     C     *IN60         IFEQ      '0'
     C     ERCO#         ANDEQ     XXCO#
     C     ERSUB#        ANDEQ     XXSUB#
     C     ERFND         ANDEQ     XXFND
     C     ERFYR         ANDEQ     XXFYR
     C     ERCASE        ANDEQ     XXCASE
     C     EROEOB        ANDEQ     XXEOB#
     C     ERSEQ#        ADD       1             XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   END
     C*
     C                   Z-ADD     XXCO#         ERCO#
     C                   Z-ADD     XXSUB#        ERSUB#
     C                   Z-ADD     XXFND         ERFND
     C                   Z-ADD     XXFYR         ERFYR
     C                   Z-ADD     XXCASE        ERCASE
     C                   MOVE      XXSS#         ERSS#
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ERACDT
     C                   Z-ADD     XREOB#        ERREOB
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ERTRDT
     C                   MOVE      SCOEOB        EROEOB
     C                   Z-ADD     XXSEQ#        ERSEQ#
     C                   Z-ADD     *ZEROS        EROINV
     C*** BEGIN CHG *** 10/23/95 **************************************
     C*******************  Z-ADDSCAMT     ERRECA
     C                   Z-ADD     SCMRA         ERRECA
     C                   ADD       SCOTHR        ERRECA
     C*** END   CHG *** 10/23/95 **************************************
     C*
     C                   MOVE      SCOEOB        XXEOB#
     C                   Z-ADD     *ZEROS        XXRED             9 2          CHG 10/23/95
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     EOBAMT        ER92N1
     C*** BEGIN DEL 10/23/95 ******************************************
     C********** EOAMTC    SUB  EOAMT     XXRED   92
     C********** XXRED     SUB  SCAMT     XXRED
     C*** BEGIN ADD *** 05/15/95 ***********************************************
     C********** XXRED     SUB  XXRECA    XXRED
     C*** END ADD *** 05/15/95 *************************************************
     C********** EOAMTC    MULT .06       XX6PER  92H
     C********** XXRED     SUB  XX6PER    XXRED
     C********** XXRED     MULT .30       XXRED     H
     C*** END   DEL 10/23/95 ******************************************
     C*** ADD 8/22/95 *****************************************
     C*
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      F16N1         #$BRS             5 4
     C*** BEGIN CHG 10/23/95 ******************************************
     C********** EOOTHR    MULT #$BRS     XXRED     H
     C     EOOTHR        SUB       SCOTHR        XXOTHR
     C     XXOTHR        MULT(H)   #$BRS         XXRED
     C*** END   CHG 10/23/95 ******************************************
     C                   ELSE
     C                   Z-ADD     0             XXRED
     C                   ENDIF
     C*
     C*** END ADD 8/22/95 *************************************
     C*
     C     XXRED         IFLT      1.00
     C                   Z-ADD     EOBAMT        ERREFA
     C                   Z-ADD     *ZEROS        EOBAMT
     C                   ELSE                                                   ELSE XXRED LT 1
     C     EOBAMT        SUB       XXRED         ERREFA
     C                   Z-ADD     XXRED         EOBAMT
     C                   END                                                    END XXRED LT 1
     C*
     C                   Z-ADD     ERREFA        XXREFA                         ADD 04/19/95
     C*
     C* UPDATE THE EOMB FILE WITH THE NEW BILLING AMOUNT
     C*
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     DSTIME        EOTIME
     C                   UPDATE    PEO01
     C                   END                                                    END *IN60 EQ '0'
     C*
     C                   MOVE      EOSYS         ERSYS
     C*
     C                   MOVE      *BLANKS       ERFL01
     C                   MOVE      'N'           ERFL02
     C                   MOVE      *BLANKS       ERFL03
     C                   MOVE      *BLANKS       ERFL04
     C                   MOVE      *BLANKS       ERFL05
     C                   MOVE      *BLANKS       ERFL06
     C                   MOVE      *BLANKS       ERFL07
     C                   MOVE      *BLANKS       ERFL08
     C                   MOVE      *BLANKS       ERFL09
     C                   MOVE      *BLANKS       ERFL10
     C*** BEGIN CHG 10/23/95 ******************************************
     C******************** Z-ADD*ZEROS    ER92N2
     C******************** Z-ADD*ZEROS    ER92N3
     C                   Z-ADD     SCMRA         ER92N2
     C                   Z-ADD     SCOTHR        ER92N3
     C*** END   CHG 10/23/95 ******************************************
     C                   Z-ADD     *ZEROS        ER92N4
     C                   Z-ADD     *ZEROS        ER92N5
     C                   MOVE      *BLANKS       ER10A1
     C                   MOVE      *BLANKS       ER10A2
     C                   MOVE      *BLANKS       ER10A3
     C                   MOVEL     Q1USER        ERUSER
     C                   MOVEL     Q1PGM         ERPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ERDT
     C                   Z-ADD     DSTIME        ERTIME
     C                   MOVEL     Q1USER        ERIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ERIDT
     C                   WRITE     PER01
     C*** BEGIN ADD *** 04/18/95 ***********************************************
     C*
     C* WE WILL WRITE A RECOVERY IF A CHECK IS WRITTEN OR PENDED
     C*
     C                   MOVE      SCOEOB        XXEOB#
     C     E1KEY         CHAIN     PE101                              60
     C     *IN60         IFEQ      '0'
     C*
     C* GET PAYMENT VALUE 1 AND 2 FROM THE OLD CHECK THAT WAS CREATED
     C*
     C                   MOVE      *BLANKS       XXBR2
     C                   MOVE      *BLANKS       XXBR7
     C*
     C     E110A2        IFGT      *BLANKS
     C                   MOVEL     E110A2        XXCHK#
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PABR2         XXBR2
     C                   MOVE      PABR7         XXBR7
     C                   ELSE
     C                   MOVE      *BLANKS       XXBR2
     C                   MOVE      *BLANKS       XXBR7
     C                   END
     C                   ELSE                                                   ELSE E110A2 EQ BLANK
     C     RPKEY         CHAIN     PRP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RPBR2         XXBR2
     C                   MOVE      RPBR7         XXBR7
     C                   ELSE
     C                   MOVE      *BLANKS       XXBR2
     C                   MOVE      *BLANKS       XXBR7
     C                   END
     C                   END                                                    END E110A2 EQ *BLANK
     C*
     C* BECAUSE THE CHECK HAS ALREADY BEEN ISSUED WE MUST ISSUE A MEDICAL REFUND
     C* GET THE CORRECT SEQUENCE NUMBER FOR THE REFUND ADJUSTEMENT
     C*
     C*** BEGIN ADD 12/28/95   ***********************************
     C***  ARE THERE ANY PAYMENTS TO ADJUST ? ********************
     C     XXBR2         IFNE      *BLANKS
     C*** END ADD * 12/28/95   ***********************************
     C                   Z-ADD     99999         XXSEQ#
     C     PAKEY1        SETGT     PPA02
     C                   READP     PPA02                                  60
     C  N60              EXCEPT    PPA02E
     C  N60XXCO#         COMP      PACO#                              6060
     C  N60XXWCF#        COMP      PAFND                              6060
     C  N60XXFYR         COMP      PAFYR                              6060
     C  N60XXCASE        COMP      PACASE                             6060
     C   60              Z-ADD     *ZERO         PASEQ#
     C                   ADD       1             PASEQ#
     C*
     C                   Z-ADD     XXCO#         PACO#
     C                   Z-ADD     XXWCF#        PAFND
     C                   Z-ADD     XXFYR         PAFYR
     C                   Z-ADD     XXCASE        PACASE
     C*
     C* UPDATE THE CHECK NUMBER WITH ORIGNAL IF PROCESSED, OR 1 IF STILL PENDED
     C*
     C     E110A2        IFNE      *BLANKS
     C                   MOVEL     E110A2        PACHK#
     C                   ELSE
     C                   Z-ADD     1             PACHK#
     C                   END
     C*
     C                   Z-ADD     *ZEROS        PABASQ
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAPODT
     C                   Z-ADD     *ZEROS        PAWKS
     C                   Z-ADD     *ZEROS        PADAYS
     C*
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN DEL 05/21/96 ***********************************
     C******************** Z-ADDEOFEIN    PAFEIN
     C******************** Z-ADDEOMOD     PAMOD
     C*** BEGIN DEL 05/21/96 ***********************************
     C*** BEGIN ADD 05/21/96 ***********************************
     C                   Z-ADD     XXFEIN        PAFEIN
     C                   Z-ADD     XXMOD         PAMOD
     C*** BEGIN ADD 05/21/96 ***********************************
     C                   Z-ADD     EOFRDT        PAFRDT
     C                   Z-ADD     EOTODT        PATODT
     C                   ELSE
     C                   Z-ADD     *ZEROS        PAFEIN
     C                   Z-ADD     *ZEROS        PAMOD
     C                   Z-ADD     *ZEROS        PAFRDT
     C                   Z-ADD     *ZEROS        PATODT
     C                   END
     C*
     C                   Z-ADD     XXREFA        PAAMT
     C                   MOVE      XXBR2         PABR2
     C                   MOVE      XXBR7         PABR7
     C                   MOVE      'UR'          PATRCD
     C                   MOVE      'R'           PATYPE
     C                   MOVE      'ME'          PARECD
     C                   MOVE      'M'           PARETP
     C                   MOVE      *BLANKS       PADESC
     C                   MOVEL     DESC          PADESC
     C*
     C                   MOVE      *BLANKS       PAADST
     C                   MOVE      *BLANKS       PASTAT
     C                   MOVEL     Q1USER        PAUSER
     C                   MOVEL     Q1PGM         PAPGM
     C                   Z-ADD     DSTIME        PATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C                   Z-ADD     *ZEROS        PABUL1
     C                   MOVE      *BLANKS       PAPGTP
     C                   Z-ADD     XXEOB#        PAEOB#
     C                   WRITE     PPA02
     C*
     C* THE RESERVES FOR THE CLAIM SHOULD BE UPDATED
     C*
     C     CMKY1         CHAIN     PCM02                              60
     C     *IN60         IFEQ      '0'
     C*
     C* FIND THE MEDICAL RESERVE IN THE CLAIM FILE
     C*
     C                   Z-ADD     1             X1
     C     'ME'          LOOKUP    CD(X1)                                 70
     C     *IN70         IFEQ      '1'
     C*
     C* ADD THE AMOUNT TO BE REFUNDED TO THE REMAINING RESERVES
     C*
     C     RR(X1)        ADD       XXREFA        RR(X1)
     C*
     C* SUB THE AMOUNT TO BE REFUNDED TO THE PAYMENTS AGAINST RESERVES
     C*
     C     PA(X1)        SUB       XXREFA        PA(X1)
     C*
     C                   MOVEL     Q1PGM         CMPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C                   MOVEL     Q1USER        CMUSER
     C*
     C                   UPDATE    PCM02
     C*
     C                   END                                                    END *IN70 EQ '1'
     C*
     C                   END                                                    END *IN60 EQ '0'
     C*
     C*** BEGIN ADD 12/28/95
     C                   ENDIF                                                  XXBR2/7 BLK?
     C*** END   ADD 12/28/95
     C                   END                                                    END *IN60 EQ '0'
     C*** END ADD *** 04/18/95 *************************************************
     C                   END                                                    END SCOEOB
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN21
     C*
     C                   UPDATE    WDW01S
     C*
     C                   GOTO      NXT4
     C                   END                                                    END *IN75
     C                   MOVE      SRRN2         RRN2
     C*
     C                   ENDSR
     C/EJECT
      *===============================================================
     C     FKEYS         BEGSR
      *===============================================================
      *
     C     *INKA         IFEQ      '1'
     C                   END
      *
     C     *INKB         IFEQ      '1'
     C                   END
      *
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      *
     C     *INKD         IFEQ      '1'
     C                   END
      *
      *
     C     *INKE         IFEQ      '1'
     C                   END
      *
      *
     C     *INKF         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
     C*
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXT2          TAG
     C                   ADD       1             K
     C     K             CHAIN     WDW01S                             75
     C     *IN75         IFEQ      '0'
     C                   Z-ADD     9             T                 2 0
     C     1             SUBST     SCOEOB:T      XX1               1    65
     C     XX1           IFEQ      '?'
     C*
     C                   MOVE      *ZEROS        Q1EOB#                         CHG 04/28/92
     C                   CALL      'F0650R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    Q1EOB#
     C                   PARM                    QQRTCD            1
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C                   MOVE      Q1EOB#        SCOEOB
     C*
     C                   Z-ADD     K             #@$2
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN21
     C                   MOVE      '0'           *IN23                          ADD 10/23/95
     C                   MOVE      '0'           *IN24                          ADD 10/23/95
     C                   UPDATE    WDW01S
     C                   GOTO      NXT2
     C                   ELSE
     C                   GOTO      NXT2
     C                   END                                                    END EQ '?'
     C                   END                                                    END *IN75'
     C                   END                                                    END WDW01
     C*
     C*** BEGIN DEL *** 10/08/93 **************************************
     C********** Q2CPOS    DIV  256       LIN     20
     C********** LIN       SUB  12        XXPOS   30
     C********** XXPOS     IFNE #@$2
     C**********           Z-ADDXXPOS     #@$2
     C**********           END
     C*** END DEL *** 10/08/93 ****************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
      *
      *
     C     *INKG         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
     C*
     C                   Z-ADD     *ZEROS        K                 5 0
     C     NXT3          TAG
     C                   ADD       1             K
     C     K             CHAIN     WDW01S                             75
     C     *IN75         IFEQ      '0'
     C                   Z-ADD     9             T                 2 0
     C     1             SUBST     SCOEOB:T      XX1               1    65
     C     XX1           IFEQ      '?'
     C*
     C                   MOVE      *ZEROS        Q1EOB#            9 0          CHG 04/28/92
     C                   CALL      'F0651R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    Q1EOB#
     C                   PARM                    QQRTCD            1
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C                   MOVE      Q1EOB#        SCOEOB
     C*
     C                   Z-ADD     K             #@$2
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN21
     C                   MOVE      '0'           *IN23                          ADD 10/23/95
     C                   MOVE      '0'           *IN24                          ADD 10/23/95
     C                   UPDATE    WDW01S
     C                   GOTO      NXT3
     C                   ELSE
     C                   GOTO      NXT3
     C                   END                                                    END EQ '?'
     C                   END                                                    END *IN75'
     C                   END                                                    END WDW01
     C*
     C*** BEGIN DEL *** 10/08/93 **************************************
     C********** Q2CPOS    DIV  256       LIN     20
     C********** LIN       SUB  12        XXPOS   30
     C********** XXPOS     IFNE #@$2
     C**********           Z-ADDXXPOS     #@$2
     C**********           END
     C*** END DEL *** 10/08/93 ****************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
      *
      *
     C     *INKH         IFEQ      '1'
     C                   END
      *
      *
     C     *INKI         IFEQ      '1'
     C                   END
      *
      *
     C     *INKJ         IFEQ      '1'
     C                   END
      *
     C     *INKK         IFEQ      '1'
     C                   END
      *
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C****************************************
     C     *INKM         IFEQ      '1'
     C                   END
      *
     C     *INKN         IFEQ      '1'
     C                   END
      *
     C     *INKP         IFEQ      '1'
     C                   END
      *
     C     *INKQ         IFEQ      '1'
     C                   END
      *
     C     *INKR         IFEQ      '1'
     C                   END
      *
     C     *INKS         IFEQ      '1'
     C                   END
      *
     C     *INKT         IFEQ      '1'
     C                   END
      *
     C     *INKU         IFEQ      '1'
     C                   END
      *
     C     *INKV         IFEQ      '1'
     C                   END
      *
     C     *INKW         IFEQ      '1'
     C                   END
      *
     C     *INKX         IFEQ      '1'
     C                   END
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   MOVE      '0'           *IN18
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
     C*
     C                   MOVE      'N'           ENTFL             1
     C                   EXSR      EDIT
     C                   EXSR      LOAD
     C                   GOTO      MAINLN
     C                   END                                                    END ROLLUP
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      MAINLN
      *
     C                   ENDSR
     C/EJECT
     OPEO01     E            PEO01E
     OPPA02     E            PPA02E
