     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H**   PROGRAMMER: LORRI DAPPER                                  **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**   PROGRAM: F0768R                                           **
     H**   DESCRIPTION: PRINTS REQUEST LETTERS - BATCH PRINT ONLY    **
     H**        BATCH PRINT ORIGINALLY FELL UNDER F0765C/R BUT       **
     H**        USER WANTED BATCH LETTERS TO PRINT OUT IN THE ORDER  **
     H**        THAT SHE HAD ENTERED THEM.  SO KEY WAS CHANGED FROM  **
     H**        USER/CO/FND/FYR/CASE/LTR TO USER/CO/LTR              **
     H**        F0765C/R WERE KEPT AS ORIGINAL FORMAT BECAUSE THEY   **
     H**        ARE NEEDED WITH FULL KEY FOR IMMEDIATE PRINT AND     **
     H**        REPRINT                                              **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILES:                                                      **
     H** FALTL6 (OPEN QUERY):                                        **
     H**   KEY = USER/CO#/LTR#                                       **
     H**   SELECTION = LTPRDT EQ 000000  &  LTBTIM EQ "B"            **
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 06/09/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  01/10/02 C:62  PROJECT: FS234  BASE ONLY
     H*                 CHANGE "PROVIDER'S DPR# .." WITH "MEDICAL
     H*                 LICENSE#"
     H*
     H*  12/28/01 C:62  PROJECT: FS234  BASE ONLY
     H*                 ADD SECOND PROVIDER ADDRESS.
     H*
     H*  03/02/00 C:62  Support: 22023
     H*                 CHANGE LOGIC TO INCLUDE DBA.
     H*
     H*  02/21/00 C:31  Support: 21960
     H*                 Correct loop in request letters
     H*                 1. Remove chain to FALTP
     H*
     H*  12/16/99 C:56  Project: Y2K
     H*                 Correct display of dates on print
     H*  01/04/99 C:31  Project: MNT63
     H*                 Correct loop in request letters
     H*                 1. Remove chain to FALTP
     H*
     H*  03/06/98 C:31  Project: MC145
     H*                 Yr 2000, RISC Developemnt
     H*                 1. Changed size of date fields back to 6
     H*
     H** 06/11/92 KRR        - CHANGED BCL9 TO HCA1500               **
     H** 07/22/92 KRR        - ADDED SUB# TO ALL PROCESSING          **
     H* 04/13/93 C:20    -REMOVED WORD BLOCK FROM PRINTED LETTER     **
     H* 11/10/93 C:20    -MOVED PRINTING OF CASE#, LETTER# AND REQ DT *
     H*                   TO THE LEFT 10 SPACES SO AS TO FIT ON PAPER *
     H*                   FROM LASER PRINTER BETTER.                  *
     H* 01/03/94 C:20    -ADDED CODE TO COMBINE PROVIDER FIRST AND    *
     H*                   LAST NAME FOR PROPER PRINTING               *
     H*
     H* 04/26/94 C:05    Project: FSMNT
     H*                  1. Removed "BCL-10" and "BCL-11"
     H*                  2. Changed "UB-82" to "UB-92"
     H*                                                               *
     H*  06/23/94 C:20     REQ/PROJ: MCMNT                            *
     H*                    1. ADDED CODE TO CLEAR QQLAST AND QQFRST   *
     H*                       BEFORE LOADING THE NAME.  IF PRIOR NAME *
     H*                       USED MORE THAN 40 BYTES OF QQLAST THE   *
     H*                       EXTRA AFTER 40 WOULD GET CARRIED OVER TO*
     H*                       THE FOLLOWING NAMES.                    *
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H*  INDICATOR USAGE:                                            **
     H*   01      INITIAL REQUEST                                    **
     H*   02      SECOND  REQUEST                                    **
     H*   03      THIRD/FINAL REQUEST                                **
     H*   50      GENERAL CHAIN                                      **
     H*   51      GENERAL CHAIN                                      **
     H*   60      READ/DOWEQ PLT01                                   **
     H*   80      ALL COMPANIES                                      **
     H*  N80      ONE COMPANIES                                      **
     H*   81      IMMEDIATE PRINT                                    **
     H*  N81      BATCH PRINT                                        **
     H*   83      FIRST TIME USER CHANGES DON'T PRINT SUMMARY TOTALS **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     FSMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMC1L     IF   E           K DISK
     FFALTL6    UF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     FQSYSPRT1  O    F  132        PRINTER INFDS(PR1DS)
     F                                     OFLIND(*INOA)
     F*

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     D DSC             S             70    DIM(23) CTDATA PERRCD(1)             OUTPUT DESC
     D FNNM            S              1    DIM(60) ASCEND
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D                 DS
     D  XXREAS                 1     18
     D  XXRE01                 1      1
     D  XXRE02                 2      2
     D  XXRE03                 3      3
     D  XXRE04                 4      4
     D  XXRE05                 5      5
     D  XXRE06                 6      6
     D  XXRE07                 7      7
     D  XXRE08                 8      8
     D  XXRE09                 9      9
     D  XXRE10                10     10
     D  XXRE11                11     11
     D  XXRE12                12     12
     D  XXRE13                13     13
     D  XXRE14                14     14
     D  XXRE15                15     15
     D  XXRE16                16     16
     D  XXRE17                17     17
     D  XXRE18                18     18
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND
     D  CONAME                 1     40
     D                 DS
     D  XCNM                   1     40
     D                                     DIM(40) ASCEND
     D  XSNAME                 1     40
     D                 DS
     D  XSZIP                  1      9  0
     D  C1ZPP1                 1      5  0
     D  C1ZPP2                 6      9  0
     D                 DS
     D  DSPHN#                 1      7  0
     D  XCPHN1                 1      3  0
     D  XCPHN2                 4      7  0
     D*** BEG CHG *** 12/16/99 ****************************************
     D**********  DS
     D**********                              1  120DSDTTM
     D**********                              1   60DSTIME
     D**********                              7  120DSDATE
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** END CHG *** 12/16/99 ****************************************
     D***
     D                 DS
     D*** BEG CHG *** 12/16/99 ****************************************
     D**********                              1  20 XC#
     D**********                              1   50XXSUB#
     D**********                              6   6 XXHYP1
     D**********                              7   90XXFND
     D**********                             10  10 XXHYP2
     D**********                             11  120XXYR
     D**********                             13  13 XXHYP3
     D**********                             14  200XXCASE
     D*
     D  XC#                    1     21
     D  XXSUB#                 1      5  0
     D  XXHYP1                 6      6
     D  XXFND                  7      9  0
     D  XXHYP2                10     10
     D  XXYR                  11     13  0
     D  XXHYP3                14     14
     D  XXCASE                15     21  0
     D*** END CHG *** 12/16/99   **************************************
     D***
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D PR1DS           DS
     D  FILL3                  1    200
     D  FILL4                201    366
     D  XXLIN1               367    368B 0
     D  XXPAG1               369    372B 0
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE COMPANY AUXILIARY FILE
     C*
     C     C1KEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C*** BEGIN ADD *** 07/22/92 **************************************
     C                   KFLD                    XXSUB#
     C*** END ADD *** 07/22/92 ****************************************
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#             9
     C*
     C*   KEY LIST FOR THE REQUEST LETTER FILE
     C*
     C     LTKEY1        KLIST
     C                   KFLD                    XXUSER
     C                   KFLD                    XXCO#
     C*
     C     LTKEY2        KLIST
     C                   KFLD                    XXUSER
     C                   KFLD                    XXCO#
     C                   KFLD                    XXLTR#
     C*
     C*   KEY LIST FOR THE PROVIDER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C     *LIKE         DEFINE    ORTITL        X1TITL
     C*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*** BEGIN ADD *** 07/22/92 **************************************
     C                   PARM                    QQSUB#            5
     C*** END ADD *** 07/22/92 ****************************************
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQLTR#            9
     C                   PARM                    QQFLAG            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C*** BEGIN CHG *** 03/06/98 **************************************
     C********             PARM *BLANKS   SRTFMT
     C*** BEG CHG *** 12/16/99 ****************************************
     C**********           PARM '*MDY'    SRTFMT
     C                   PARM      *BLANKS       SRTFMT
     C*** BEG CHG *** 12/16/99 ****************************************
     C*** END CHG *** 03/06/98 ****************************************
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C*** BEGIN ADD *** 07/22/92 **************************************
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C*** END ADD *** 07/22/92 ****************************************
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQFYR         XXYR
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQLTR#        XXLTR#            9 0
     C                   MOVE      QQFLAG        XXFLAG            1
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C*  GET THE COMPANY NAME  FROM DATA AREA
     C*
     C     C1KEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      C1ZIP         XSZIP
     C                   MOVE      C1NAME        XSNAME
     C                   MOVE      C1AREA        XCAREA            3 0
     C                   MOVE      C1PHN#        DSPHN#
     C*
     C*** REMOVE LEADING BLANKS FROM THIS NAME
     C*
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    XCNM(Z)                            60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     XCNM(Z)       XCNAME           40
     C                   ELSE
     C                   MOVE      *BLANKS       XCNAME
     C                   END
     C*
     C                   MOVEL     C1ADR1        XCADR1           40
     C                   MOVEL     C1ADR2        XCADR2           40
     C*
     C     XCADR2        IFNE      *BLANKS
     C                   MOVE      '1'           *IN84
     C                   ELSE
     C                   MOVE      '0'           *IN84
     C                   END
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     C1ST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVEL     C1ZIP         C1ZPP1
     C                   MOVE      C1ZPP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C                   MOVE      C1ZIP         C1ZPP2
     C     C1ZPP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      C1ZPP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     C1CTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XCCSZ            40
     C                   END                                                    END 0 *IN60
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      '-'           XXHYP1            1
     C                   MOVE      '-'           XXHYP2            1
     C                   MOVE      '-'           XXHYP3            1
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*  DETERMINE IF ONE COMPANY OR ALL COMPANIES
     C*
     C     XXCO#         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN80                          ALL COMPANIES
     C                   ELSE
     C                   MOVE      '0'           *IN80                          ONE COMPANY
     C                   END
     C*
     C*  DETERMINE IF BATCH OR IMMEDIATE
     C*
     C     XXFLAG        IFEQ      'I'
     C                   MOVE      '1'           *IN81                          IMMED
     C                   ELSE
     C                   MOVE      '0'           *IN81                          BATCH
     C                   END
     C*
     C*  FIRST TIME THROUGH DON'T PRINT TOTALS ON SUMMARY REPORT
     C*
     C                   MOVE      '1'           *IN83
     C*
     C*  READ REQUEST LETTER FILE
     C*
     C*  80N81             READ PLT01                    60 ALL COMPANY
     C* N80N81   LTKEY1    CHAINPLT01                60     BATCH
     C* N80 81   LTKEY2    CHAINPLT01                60     IMMED
     C*
     C                   READ      PLT01                                  60     ALL USERS
     C*
     C     *IN60         IFEQ      '1'
     C                   GOTO      TERM
     C                   END                                                    END IFEQ '0'
     C*
     C*** GET ROUTING INFORMATION FOR REPORT
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              55
     C********** ORKEY     CHAINPOR01                60
     C     *IN55         IFEQ      '0'
     C********** *IN60     IFEQ '0'
     C                   MOVE      ORTITL        X1TITL
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********          EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *********************************************
     C                   END                                                    END IFEQ '0'
     C*
     C*  CHAIN TO USER PARAMETER FILE FOR USER NAME
     C*
     C     Q1USER        CHAIN     PUP01                              50
     C                   MOVEL     UPUSNM        XXUSN1           50
     C     LTIUSR        CHAIN     PUP01                              50
     C                   MOVEL     UPUSNM        XXUSN2           50
     C                   EXCEPT    HEAD1
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     LTIUSR        IFNE      XXUSER
     C*
     C                   MOVE      LTIUSR        XXUSER           10
     C*
     C*  CHAIN TO USER PARAMETER FILE FOR USER NAME
     C*
     C     Q1USER        CHAIN     PUP01                              50
     C                   MOVEL     UPUSNM        XXUSN1           50
     C     LTIUSR        CHAIN     PUP01                              50
     C                   MOVEL     UPUSNM        XXUSN2           50
     C*
     C*** GET ROUTING INFORMATION FOR LETTER
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      ORTITL        XXTITL
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********          EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END                                                    END IFEQ '0'
     C*
     C* IF NOT THE FIRST TIME THROUGH, PRINT USER TOTALS ON
     C* SUMMARY REPORT
     C*
     C     *IN83         IFNE      '1'
     C                   EXCEPT    TOTAL1
     C                   EXCEPT    HEAD1
     C                   ELSE
     C                   MOVE      '0'           *IN83
     C                   END
     C*
     C                   END
     C*
     C*  MOVE FIELDS TO REPORT FIELDS
     C*
     C                   Z-ADD     LTCO#         XXCO#             3 0
     C*** BEGIN ADD *** 07/22/92 **************************************
     C                   Z-ADD     LTSUB#        XXSUB#            5 0
     C*** END ADD *** 07/22/92 ****************************************
     C                   Z-ADD     LTFND         XXFND             3 0
     C                   Z-ADD     LTFYR         XXFYR             3 0
     C                   Z-ADD     LTFYR         XXYR
     C                   Z-ADD     LTCASE        XXCASE            7 0
     C                   Z-ADD     LTACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** BEGIN CHG *** 03/06/98 **************************************
     C********             MOVELSRDTO     XXACDT  80
     C*** BEG CHG *** 12/16/99 ****************************************
     C**********           MOVELSRDTO     XXACDT  60
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*** END CHG *** 12/16/99 ****************************************
     C*** END CHG *** 03/06/98 ****************************************
     C                   Z-ADD     LTLTR#        XXLTR#            9 0
     C                   Z-ADD     LTFEIN        XXFEIN            9 0
     C                   Z-ADD     LTMOD         XXMOD             3 0
     C                   Z-ADD     LTFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** BEGIN CHG *** 03/06/98 **************************************
     C********             MOVELSRDTO     XXFSDT  80
     C*** BEG CHG *** 12/16/99 ****************************************
     C**********           MOVELSRDTO     XXFSDT  60
     C                   MOVEL     SRDTO         XXFSDT            8 0
     C*** END CHG *** 12/16/99 ****************************************
     C*** END CHG *** 03/06/98 ****************************************
     C                   Z-ADD     LTTSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*** BEGIN CHG *** 03/06/98 **************************************
     C********             MOVELSRDTO     XXTSDT  80
     C*** BEG CHG *** 12/16/99 ****************************************
     C**********           MOVELSRDTO     XXTSDT  60
     C                   MOVEL     SRDTO         XXTSDT            8 0
     C*** END CHG *** 12/16/99 ****************************************
     C*** END CHG *** 03/06/98 ****************************************
     C                   Z-ADD     LTAMT         XXAMT             9 2
     C                   MOVEL     LTREAS        XXREAS           18
     C                   MOVEL     LTBL24        XXBL24           40
     C                   MOVEL     LT24D2        XX24D2           40
     C                   MOVEL     LTOTH         XXOTH            40
     C                   MOVEL     LTOTD2        XXOTD2           40
     C*
     C                   MOVE      '0'           *IN01
     C                   MOVE      '0'           *IN02
     C                   MOVE      '0'           *IN03
     C     LTFLG1        IFEQ      ' '
     C     LTFLG1        OREQ      '1'
     C                   MOVE      '1'           *IN01                          INITIAL REQ
     C                   END                                                    ENDIF '1'
     C     LTFLG1        IFEQ      '2'
     C                   MOVE      '1'           *IN02                          2ND REQ
     C                   END                                                    ENDIF '1'
     C     LTFLG1        IFEQ      '3'
     C                   MOVE      '1'           *IN03                          FINAL REQ
     C                   END                                                    ENDIF '2'
     C*
     C*  GET PATIENT NAME
     C*
     C     CMKEY         CHAIN     PCM01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      CMSS#         XXSS#             9
     C                   END                                                    ENDIF *IN50
     C     CDKEY         CHAIN     PCD01                              50
     C     *IN50         IFEQ      '0'
     C*** BEGIN ADD *** 06/23/94 ***********************************************
     C                   MOVE      *BLANKS       QQLAST
     C                   MOVE      *BLANKS       QQFRST
     C*** END ADD *** 06/23/94 *************************************************
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C**********           MOVELQQLAST    XXCLNM 40        DEL 11/10/93
     C                   MOVEL     QQLAST        XXCLNM           30            ADD 11/10/93
     C                   MOVEL     QQLAST        XZCLNM           20
     C                   ELSE                                                   ELSIF *IN50
     C                   MOVE      *BLANKS       XXCLNM
     C                   MOVE      *BLANKS       XZCLNM
     C                   END                                                    ENDIF *IN50
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      CONAME        XSNAME
     C*
     C*** REMOVE LEADING BLANKS FROM THIS NAME
     C*
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    XCNM(Z)                            51
     C     *IN51         IFEQ      '1'
     C                   MOVEA     XCNM(Z)       XXCRNM           40
     C                   MOVEA     XCNM(Z)       XZCRNM           20
     C                   ELSE
     C                   MOVE      *BLANKS       XXCRNM
     C                   MOVE      *BLANKS       XZCRNM
     C                   END                                                    ENDEQ '1'
     C                   END                                                    ENDIF *IN50
     C*** BEGIN ADD *** 12/28/01 **************************************
     C                   SETOFF                                       16
     C*** END   ADD *** 12/28/01 **************************************
     C*
     C*
     C*  GET PROVIDER NAME/ADDRESS
     C*
     C     PDKEY         CHAIN     PPD01                              50
     C     *IN50         IFEQ      '0'
     C*** BEGIN CHG *** 01/03/94 **************************************
     C*** BEGIN CHG *** 01/03/94 **************************************
     C********** PDFSN     IFEQ *BLANKS
     C**********           MOVELPDNAME    XXPRNM 40
     C**********           ELSE
     C**********           MOVE *BLANKS   QQLAST
     C**********           MOVE *BLANKS   QQFRST
     C**********           MOVELPDNAME    QQLAST 56
     C**********           MOVELPDFSN     QQFRST 15
     C**********           MOVE '1'       QQTERM  1
     C**********           CALL 'SRNAME'
     C**********           PARM           QQLAST
     C**********           PARM           QQFRST
     C**********           PARM           QQTERM
     C**********           MOVELQQLAST    XXPRNM
     C**********           END
     C*** END ADD *** 01/03/94 ****************************************
     C**********           MOVELPDADR1    XXPAD1 40
     C**********           MOVELPDCTY     XXPCTY 25
     C**********           MOVELPDST      XXPSTE  2
     C**********           MOVELPDZIP     XXPZP1  50
     C**********           MOVE PDZIP     XXPZP2  40
     C*
     C     PDDBA1        IFEQ      'Y'
     C*
     C                   MOVEL(P)  PDADR1        XXPRNM
     C                   MOVEL(P)  PDADR2        XXPAD1
     C*
     C                   ELSE
     C*
     C     PDFSN         IFEQ      *BLANKS
     C*
     C                   MOVEL(P)  PDNAME        XXPRNM
     C*
     C                   ELSE
     C*
     C     PDFSN         CAT(P)    PDNAME:1      XXPRNM
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  PDADR1        XXPAD1
     C*** BEGIN ADD *** 12/28/01 **************************************
     C                   MOVEL(P)  PDADR2        XXPAD2           40
     C                   SETON                                        16
     C*** END   ADD *** 12/28/01 **************************************
     C*
     C                   ENDIF                                                  PDDBA1 = 'Y'
     C*
     C                   MOVEL(P)  PDCTY         XXPCTY
     C                   MOVEL(P)  PDST          XXPSTE
     C                   MOVEL     PDZIP         XXPZP1
     C                   MOVE      PDZIP         XXPZP2
     C*** END   CHG *** 01/03/94 **************************************
     C                   ELSE
     C                   MOVE      *BLANKS       XXPRNM           40
     C                   MOVE      *BLANKS       XXPAD1           40
     C                   MOVE      *BLANKS       XXPCTY           25
     C                   MOVE      *BLANKS       XXPSTE            2
     C                   Z-ADD     *ZEROS        XXPZP1            5 0
     C                   Z-ADD     *ZEROS        XXPZP2            4 0
     C                   END                                                    ENDIF *IN50
     C*
     C*  PRINT LETTER
     C*
     C                   EXCEPT    DETAIL
     C*
     C*  PRINT DETAIL LINE ON REPORT
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   MOVE      *BLANKS       XXDESC           45
     C     *IN01         IFEQ      '1'
     C                   MOVEL     DSC(21)       XXDESC                         1ST REQ
     C                   END
     C     *IN02         IFEQ      '1'
     C                   MOVEL     DSC(22)       XXDESC                         2ND REQ
     C                   END
     C     *IN03         IFEQ      '1'
     C                   MOVEL     DSC(23)       XXDESC                         FINAL REQ
     C                   END
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        LTSS# = MaskData('SSN':'LTSS#':LTSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   EXCEPT    DET1
     C     1             ADD       XXCNT         XXCNT             6 0
      ***** Beg Chg *** 06/09/20 *******************************************
      /Free
        LTSS# = UnMaskData('SSN':'LTSS#':LTSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C*  UPDATE RECORD WITH PRINT DATE
     C*
     C*** BEGIN ADD *** 01/04/99 **************************************
     C********   LTKEY2    CHAINPLT01                50
     C*** END ADD *** 01/04/99 ****************************************
     C     LTFLG1        IFEQ      ' '
     C     LTFLG1        OREQ      '1'
     C                   MOVE      '1'           LTFLG1
     C                   END                                                    ENDIF '1'
     C     LTFLG1        IFEQ      '2'
     C                   MOVE      '2'           LTFLG1
     C                   END                                                    ENDIF '1'
     C     LTFLG1        IFEQ      '3'
     C                   MOVE      '3'           LTFLG1
     C                   END                                                    ENDIF '2'
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTPRDT
     C**********           MOVE Q1USER    LTUSER
     C                   MOVE      Q1PGM         LTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTDT
     C*** BEGIN ADD *** 02/21/00 **************************************
     C**N50***             UPDATPLT01
     C                   UPDATE    PLT01
     C*** END ADD *** 02/21/00 ****************************************
     C*
     C**********************************
     C*  READ NEXT RECORD              *
     C**********************************
     C*  80N81             READ PLT01                    60 ALL COMPANY
     C* N80N81   LTKEY1    READEPLT01                    60 BATCH
     C* N80 81   LTKEY2    READEPLT01                    60 IMMED
     C*
     C                   READ      PLT01                                  60     ALL USERS
     C*
     C                   END                                                    END DOWEQ 60
     C**********************************
     C*     END THE PROGRAM            *
     C**********************************
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOA
     C                   END                                                    ENDIF *INOA
     C                   EXCEPT    TOTAL1
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       xxPrt = '1';
       Chain orKey POR01;
       If %found;
          Except Route1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
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
     O**********     09      Q1USER              26
     O**********     08      LTIUSR              26
     O**********                                 28 '-'
     O**********     09      XXUSN1              79
     O**********     08      XXUSN2              79
     O**********E            ROUTE          1
     O**********       08      ORDIS2    60
     O**********E            ROUTE          1
     O******           08      ORDIS3    60
     O**********E            ROUTE          2
     O******           08      ORDIS4    60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               09      Q1USER              26
     O               08      LTIUSR              26
     O                                           28 '-'
     O               09      XXUSN1              79
     O               08      XXUSN2              79
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
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
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               58 'INC. 1993 - 2000'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ***** END CHG ****** 01/28/10 ***********************************
      *
     OQSYSPRT   E            DETAIL         1 02
     O                       XCNAME              66
     O          E            DETAIL         1
     O                       XCADR1              66
     O          E    84      DETAIL         1
     O                       XCADR2              66
     O          E            DETAIL         1
     O                       XCCSZ               66
     O          E            DETAIL         0
     O                                           27 '('
     O                       XCAREA              30
     O                                           31 ')'
     O                       XCPHN1              35
     O                                           36 '-'
     O                       XCPHN2              40
     O          E    01      DETAIL           07
     O                       DSC(21)             96
     O          E    02      DETAIL           07
     O                       DSC(22)             96
     O          E    03      DETAIL           07
     O                       DSC(23)             96
     O          E            DETAIL         1 09
     O                                           21 'PATIENT NAME :'
     O**********               XXCLNM    62
     O                       XXCLNM              52
     O**********               XC#       82
     O                       XC#                 72
     O          E            DETAIL         1
     O                                           21 'SERVICE DATES:'
     O                                           26 'FROM'
     O*** BEG CHG *** 12/16/99 ****************************************
     O**********               XXFSDT    35 '  /  /  '
     O**********                         38 'TO'
     O**********               XXTSDT    47 '  /  /  '
     O*
     O                       XXFSDT              37 '  /  /    '
     O                                           40 'TO'
     O                       XXTSDT              51 '  /  /    '
     O*** BEG CHG *** 12/16/99 ****************************************
     O          E            DETAIL         1
     O                                           21 'CHARGES      :'
     O                       XXAMT         1     35
     O          E            DETAIL         1
     O**********                         73 'LETTER #    :'
     O                                           63 'LETTER #    :'
     O**********               XXLTR#Z   83
     O                       XXLTR#        Z     73
     O          E            DETAIL         1
     O**********                         73 'REQUEST DATE:'
     O                                           63 'REQUEST DATE:'
     O**********               DSDATE    83 '  /  /  '
     O*** BEG CHG *** 12/16/99 ****************************************
     O**********               DSDATE    73 '  /  /  '
     O                       DSDATE              73 '  /  /    '
     O*** END CHG *** 12/16/99 ****************************************
     O          E            DETAIL         1
     O                       XXPRNM              47
     O          E            DETAIL         1
     O                       XXPAD1              47
     O*** BEGIN ADD *** 12/28/01 **************************************
     O          E    16      DETAIL         1
     O                       XXPAD2              47
     O*** END   ADD *** 12/28/01 **************************************
     O          E            DETAIL         1
     O                       XXPCTY              32
     O                       XXPSTE              36
     O                       XXPZP1        Z     43
     O                       XXPZP2        Z     47
     O          E            DETAIL         2 21
     O                       DSC(1)              77
     O          E            DETAIL         0
     O                                            5 '___'
     O                       DSC(2)              77
     O          E            DETAIL         1
     O                       XXRE01               4
     O          E            DETAIL         0
     O                                            5 '___'
     O***********************************14 'Block 6'
     O*************************DSC,3     87
     O                       DSC(3)              77
     O          E            DETAIL         1
     O                       XXRE02               4
     O          E            DETAIL         0
     O                                            5 '___'
     O***********************************14 'Block 8'
     O*************************DSC,4     87
     O                       DSC(4)              77
     O          E            DETAIL         1
     O                       XXRE03               4
     O          E            DETAIL         0
     O                                            5 '___'
     O***********************************15 'Block 11'
     O*************************DSC,5     87
     O                       DSC(5)              77
     O          E            DETAIL         1
     O                       XXRE04               4
     O          E            DETAIL         0
     O                                            5 '___'
     O***********************************15 'Block 14'
     O*************************DSC,6     87
     O                       DSC(6)              77
     O          E            DETAIL         1
     O                       XXRE05               4
     O          E            DETAIL         0
     O                                            5 '___'
     O***********************************15 'Block 15'
     O*************************DSC,7     87
     O                       DSC(7)              77
     O          E            DETAIL         1
     O                       XXRE06               4
     O          E            DETAIL         0
     O                                            5 '___'
     O***********************************15 'Block 19'
     O*************************DSC,8     87
     O                       DSC(8)              77
     O          E            DETAIL         1
     O                       XXRE07               4
     O          E            DETAIL         0
     O                                            5 '___'
     O***********************************15 'Block 23'
     O*************************DSC,9     87
     O                       DSC(9)              77
     O          E            DETAIL         1
     O                       XXRE08               4
     O          E            DETAIL         0
     O                                            5 '___'
     O***********************************30 'Block 24  A,B,C,D,E,F,G'
     O***********************************52 '____________________'
     O                                           27 '____________________'
     O***********************************72 '____________________'
     O                                           47 '____________________'
     O          E            DETAIL         1
     O                       XXRE09               4
     O*************************XXBL24    72
     O                       XXBL24              47
     O          E            DETAIL         0
     O***********************************52 '____________________'
     O                                           27 '____________________'
     O***********************************72 '____________________'
     O                                           47 '____________________'
     O          E            DETAIL         1
     O*************************XX24D2    72
     O                       XX24D2              47
     O          E            DETAIL         0
     O                                            5 '___'
     O***********************************15 'Block 25'
     O*************************DSC,10    87
     O                       DSC(10)             77
     O          E            DETAIL         1
     O                       XXRE10               4
     O          E            DETAIL         1
     O*************************DSC,11    87
     O                       DSC(11)             77
     O          E            DETAIL         0
     O                                            5 '___'
     O***********************************15 'Block 25'
     O*************************DSC,12    87
     O                       DSC(12)             77
     O          E            DETAIL         1
     O                       XXRE11               4
     O          E            DETAIL         0
     O                                            5 '___'
     O***********************************16 'Block 31B'
     O*************************DSC,13    87
     O*** BEGIN CHG *** 01/10/02 **************************************
     O**********               DSC,13    77
     O                                         +002 'Medical License#'
     O*** END   CHG *** 01/10/02 **************************************
     O          E            DETAIL         1
     O                       XXRE12               4
     O*** BEGIN DEL *** 01/10/02 **************************************
     O********E* 1             DETAIL
     O*************************DSC,14    87
     O**********               DSC,14    77
     O********E* 1             DETAIL
     O*************************DSC,15    87
     O**********               DSC,15    77
     O********E* 1             DETAIL
     O*************************DSC,16    87
     O**********               DSC,16    77
     O*** END   DEL *** 01/10/02 **************************************
     O          E            DETAIL         0
     O                                            5 '___'
     O***********************************15 'Block 33'
     O*************************DSC,17    87
     O                       DSC(17)             77
     O          E            DETAIL         1
     O                       XXRE13               4
     O          E            DETAIL         0
     O                                            5 '___'
     O                       DSC(18)             77
     O          E            DETAIL         1
     O                       XXRE14               4
     O          E            DETAIL         0
     O                                            5 '___'
     O                       DSC(19)             77
     O          E            DETAIL         1
     O                       XXRE15               4
     O          E            DETAIL         0
     O                                            5 '___'
     O                       DSC(20)             77
     O          E            DETAIL         1
     O                       XXRE16               4
     O          E            DETAIL         0
     O                                            5 '___'
     O                                           13 'Other:'
     O                                           35 '____________________'
     O                                           55 '____________________'
     O          E            DETAIL         1
     O                       XXRE17               4
     O                       XXOTH               55
     O          E            DETAIL         0
     O                                           35 '____________________'
     O                                           55 '____________________'
     O          E            DETAIL         1
     O                       XXOTD2              55
     O          E            DETAIL         1 49
     O                                           60 '_____________'
     O                                           75 '_______________'
     O          E            DETAIL         1
     O                                           72 'Medical Claims Analyst'
     O          E            DETAIL         0 55
     O                                           25 'IMPERATIVE TO RETURN'
     O                                           35 'THIS FORM'
     O                                           56 'WITHIN FOURTEEN DAYS'
     O          E            DETAIL         1
     O                                           25 '____________________'
     O                                           35 '__________'
     O                                           56 '_____________________'
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   Q1USER              26
     O**********        09                       28 '-'
     O**********        09   XXUSN1              79
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE1         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   Q1USER              26
     O                  09                       28 '-'
     O                  09   XXUSN1              79
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       X1TITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE1         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE1
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                58 'INC. 1993 - 2000'
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ***** END CHG ****** 01/28/10 ***********************************
     O          E            HEAD1          1 06
     O*** BEG CHG *** 12/16/99 ****************************************
     O**********               DSDATEY    8
     O                       DSDATE        Y     10
     O*** BEG CHG *** 12/16/99 ****************************************
     O                       XCNAME              86
     O                       DSTIME             132 '  :  :  '
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                       X1TITL              96
     O                                          127 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD1          2
     O                                            5 'USER:'
     O               09      Q1USER              16
     O               08      LTIUSR              16
     O                                           18 '-'
     O               09      XXUSN1              69
     O               08      XXUSN2              69
     O          E            HEAD1          2
     O                                           07 'CLAIM #'
     O                                           32 'SOC SEC #'
     O                                           42 'ACC. DATE'
     O                                           57 'CLAIMANT NAME'
     O                                           77 'LETTER #'
     O                                          102 'REQUEST DESCRIPTION'
     O          E            DET1           1
     O                       XC#                 21
     O                       LTSS#               32
     O*** BEG CHG *** 12/16/99 ****************************************
     O**********               XXACDT    42 '  /  /  '
     O                       XXACDT              43 '  /  /    '
     O*** END CHG *** 12/16/99 ****************************************
     O                       XZCLNM              64
     O                       XXLTR#        Z     78
     O                       XXDESC             128
     O          E            TOTAL1      1
     O                                           28 '# LETTERS PRINTED:'
     O                       XXCNT         ZB    35
     O**
** DESC (OUTPUT DESCRIPTIONS)
The attached bill is being returned to you for the following reason:
Proper form:  HCFA-1500 or UB-92
Patient's social security number
Employer's name
Employer's (insured) address
Date of Accident
Date first consulted
Name and address or DPR# of referring physician
Written diagnosis AND ICD-9 diagnosis code
Original provider signature
(STAMPS OR INITIALED SIGNATURES ARE NOT ACCEPTED)
Fraud and perjury statements (MUST BE SIGNED IF SEPARATE)
Provider's DPR# - must enter the nine (9) character
alpha numeric # assigned by the dept. of professional
regulations for physicians or health care providers
(e.g. ME-0000000)
Provider's Federal ID#
Itemized charges or copy of invoice from supplier
Type of medication, strength-quantity
Reports: Consultation, Operative, Narrative
*** INITIAL REQUEST ***
*** NO RESPONSE RECEIVED - SECOND REQUEST ***
*** NO RESPONSE RECEIVED - FINAL REQUEST ***
