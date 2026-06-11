     Hcopyright('fc) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F7820R - UNREVIEWED COLLISION REPORT - PRINT PROGRAM
      *****************************************************************
      **   PROGRAMMER: LORRI DAPPER                                  **
      **   CREATED:    4/20/89                                       **
      ** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
      ** REVISIONS:                                                  **
      **                                                             **
      **08/05/15 C:307 Project: SY354                                **
      **               -Adding 'Route To:'                           **
      **                                                             **
      **08/26/14 C:298 Project: SY354                                **
      **               -Implement Routing Page Changes.              **
      **                                                              *
      **  01/28/10  C:249  Project: SY215                            **
      **            C:189  Retrieve brand Name                       **
      ** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
      ** INDICATOR USAGE:                                            **
      **   INDICATOR            FUNCTION DESCRIPTION                 **
      **                                                             **
      **      OF      OVERFLOW FOR PRINTING                          **
      **                                                             **
      **      08      BATCH MODE                                     **
      **      09      DEMAND MODE                                    **
      **      50      GENERAL USAGE (CHAIN)                          **
      **      60      READ/DOWEQ OF OPEN QUERY FILE                  **GE
      **      80      ALL SUBSCRIBERS TO BE PROCESSED                **GE
      **     N80      ONE SUBSCRIBER TO BE PROCESSED                 **GE
      **      81      PROVIDER MODIFIER = 0                          **
      **      82      PROCEDURE MODIFIER = 0                         **
      **      89      NEW CASE - PRINT CASE INFO ON DETAIL LINE      **
      **      90      DETAIL LINE DONE                               **
      **      91      CASE TOTAL DONE                                **
      **      92      GROUP TOTAL DONE                               **
      **      93      SUBSCRIBER TOTAL DONE                          **
      **     N93      NO RECORDS - ERROR MESSAGE                     **
      ** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
      ** OPEN QUERY FILE:                                            **
      **  FDCLL FILE - SEL: DATE RANGE                               **
      **               SEQ: CO/SUB/FND/FYR/CASE/FEIN/MOD             **
      *****************************************************************
     FFDCLP     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMSML     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     F*

     D SB              S              5  0 DIM(99)                              SUBSCRIBER
     D AM              S             12  2 DIM(99)                              AMOUNT
     D CT              S              6  0 DIM(99)                              COUNT
     D                 DS
     D  DSSS#                  1      9
     D  SSPRE                  1      3
     D  SSMID                  4      5
     D  SSSUF                  6      9
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXCS#                  1     10
     D  XXYR                   1      2  0
     D  XXHYP1                 3      3
     D  XXCASE                 4     10  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    CLCO#         XXCO#
     C     *LIKE         DEFINE    CLSUB#        XXSUB#
     C     *LIKE         DEFINE    CLFND         XXFND
     C     *LIKE         DEFINE    CLFYR         XXFYR
     C     *LIKE         DEFINE    CLFEIN        XXFEIN
     C     *LIKE         DEFINE    CLMOD         XXMOD
     C     *LIKE         DEFINE    CLCPT4        XXCODE
     C     *LIKE         DEFINE    CLPMOD        XXPMOD
     C     *LIKE         DEFINE    CLTOTC        XXAMT
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C*
     C*   KEY LIST FOR THE UNREVIEWED COLLISION FILE
     C*
     C     CLKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   KEY LIST FOR THE OUTPUT ROUTING FILE
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
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C*
     C                   MOVE      QQFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFDT             6 0
     C*
     C                   MOVE      QQTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTDT             6 0
     C*
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      '-'           XXHYP1            1
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END                                                    ENDEQ 'D'
     C                   Z-ADD     0             X                 3 0          ARRAY INDEX
     C                   Z-ADD     0             XXACAS            9 2          CASE AMOUNT
     C                   Z-ADD     0             XXCCAS            4 0          CASE COUNT
     C*
     C*  DETERMINE IF ONE SUBSCRIBER OR ALL SUSBSCRIBERS
     C*
     C     XXSUB#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN80                          ALL SUBSCRIBERS
     C                   ELSE
     C                   MOVE      '0'           *IN80                          ONE SUBSCRIBER
     C                   END                                                    ENDEQ *ZERO
     C*
     C*  CHAIN TO USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              50
     C*
     C*** GET ROUTING INFORMATION FOR REPORT
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C********           MOVE      '0'           XXPRT
     C********     ORKEY         CHAIN     POR01                              50
     C********     *IN50         IFEQ      '0'
     C********           EXCEPT    ROUTE
     C********           END                                                    END IFEQ '0'
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C*  READ OPEN QUERY FILE
     C*
     C   80              READ      PCL01                                  60     ALL SUBSCRIBER
     C  N80CLKEY         CHAIN     PCL01                              60         ONE SUBSCRIBER
     C*
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     CLCO#         XXCO#
     C                   Z-ADD     CLSUB#        XXSUB#
     C                   Z-ADD     CLFND         XXFND
     C                   Z-ADD     CLFYR         XXFYR
     C                   Z-ADD     CLFYR         XXYR              2 0
     C                   Z-ADD     CLCASE        XXCASE
     C*
     C*  GET COMPANY NAME/SUBSCRIBER NAME/GROUP NAME
     C*
     C                   MOVE      *BLANKS       XXCONM           40
     C     COKEY         CHAIN     PCO01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     CONAME        XXCONM
     C                   END                                                    END *IN50
     C                   MOVE      *BLANKS       XXSBNM           40
     C     SMKEY         CHAIN     PSM01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM
     C                   END                                                    END *IN50
     C                   EXCEPT    HEAD
     C*
     C                   MOVE      *BLANKS       XXFNNM           40
     C     FNKEY         CHAIN     PFN01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM
     C                   END                                                    END *IN50
     C                   EXCEPT    GRPHD
     C*
     C                   ELSE                                                   ELS *IN60
     C                   GOTO      TERM
     C                   END                                                    END *IN60
     C*
     C*  GET CASE SS#/ACDT
     C*
     C                   MOVE      *BLANKS       XXSS#
     C                   Z-ADD     *ZEROS        XXACDT
     C     CMKEY         CHAIN     PCM01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      CMSS#         DSSS#
     C                   MOVE      CMSS#         XXSS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C                   END                                                    END *IN50
     C                   MOVE      '1'           *IN89                          CASE PRINT
     C*
     C     *IN60         DOWEQ     '0'
     C*                                                                AME
     C*  IF CASE CHANGES, PRINT CASE TOTALS
     C*
     C     CLCASE        IFNE      XXCASE
     C                   EXSR      SRCASE                                       CASE TOTALS
     C                   END                                                    ENDNE XXCASE
     C*
     C*  IF GROUP CHANGES, PRINT GROUP TOTALS
     C*
     C     CLFND         IFNE      XXFND
     C                   EXSR      SRGRP                                        GRP TOTALS
     C                   END                                                    ENDNE XXFND
     C*
     C*  IF SUBSCRIBER CHANGES, PRINT SUBSCRIBER TOTALS,
     C*  PAGE BREAK HEADINGS
     C*
     C     CLSUB#        IFNE      XXSUB#
     C     *IN93         IFEQ      '0'
     C                   EXCEPT    MSG                                          NO RECORDS
     C                   END                                                    END *IN93
     C                   EXSR      SRSUBS                                       SUBS TOTALS
     C                   END                                                    ENDNE XXSUB#
     C*
     C                   Z-ADD     CLFEIN        XXFEIN
     C                   Z-ADD     CLMOD         XXMOD
     C                   Z-ADD     CLRDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C                   Z-ADD     CLFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFDAT            6 0
     C                   Z-ADD     CLTDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTDAT            6 0
     C*
     C*  GET SERVICE CODE OR PROCEDURE CODE/MODIFIER
     C*
     C                   MOVE      *BLANKS       XXCODE
     C     CLSVCD        IFNE      *ZEROS
     C                   MOVE      CLSVCD        XXCODE
     C                   ELSE
     C                   MOVEL     CLCPT4        XXCODE
     C                   MOVE      CLPMOD        XXPMOD
     C                   END                                                    ENDNE *ZEROS
     C*
     C                   Z-ADD     CLTOTC        XXAMT
     C                   ADD       CLTOTC        XXACAS                          CASE AMOUNT
     C                   ADD       1             XXCCAS                          CASE COUNT
     C*
     C*** SET ANY CONTROLING INDICATOR HERE
     C*
     C     XXMOD         COMP      *ZERO                              8181
     C     XXPMOD        COMP      *ZERO                              8282
     C*
     C*  OUTPUT DETAIL LINE ITEM
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    GRPHD
     C                   MOVE      '0'           *INOF
     C                   END                                                    END *INOF
     C*
     C                   EXCEPT    DETAIL
     C                   MOVE      '0'           *IN89                          CASE PRINT
     C                   MOVE      '1'           *IN90                          DETAIL DONE
     C**********************************
     C*  READ NEXT RECORD              *
     C**********************************
     C   80              READ      PCL01                                  60     ALL SUBSCRIBER
     C  N80CLKEY         READE     PCL01                                  60     ONE SUBSCRIBER
     C*
     C                   END                                                    END DOWEQ 60
     C**********************************
     C*     END THE PROGRAM            *
     C**********************************
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C                   EXSR      SRCASE
     C                   EXSR      SRGRP
     C                   EXSR      SRSUBS
     C     *IN80         IFEQ      '1'
     C                   EXSR      SRALL
     C                   END                                                    END *IN80
     C*
     C     *IN93         IFEQ      '0'
     C                   EXCEPT    MSG
     C                   END                                                    END *IN93
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*****************************************************************--*
     C*  PERFORM CASE TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRCASE        BEGSR
     C*
     C     *IN90         IFEQ      '1'                                          DETAIL DONE
     C                   ADD       XXACAS        XXAGRP           10 2          GRP AMOUNT
     C                   ADD       XXCCAS        XXCGRP            4 0          GRP COUNT
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    GRPHD
     C                   MOVE      '0'           *INOF
     C                   END                                                    END *INOF
     C                   EXCEPT    CASTOT
     C*
     C                   MOVE      '1'           *IN91                          SRCASE DONE
     C*
     C     *INLR         IFEQ      '0'
     C     CLFND         ANDNE     XXFND
     C     XXFND         ANDNE     *ZEROS
     C                   EXSR      SRGRP
     C                   END                                                    END *INLR
     C*
     C                   END                                                    END IF *IN90
     C*
     C                   Z-ADD     CLFYR         XXFYR
     C                   Z-ADD     CLFYR         XXYR
     C                   Z-ADD     CLCASE        XXCASE
     C                   MOVE      '0'           *IN90
     C*
     C*  GET NEW CASE SS#/ACDT
     C*
     C                   MOVE      *BLANKS       XXSS#
     C                   Z-ADD     *ZEROS        XXACDT
     C     CMKEY         CHAIN     PCM01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      CMSS#         XXSS#
     C                   MOVE      CMSS#         DSSS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT
     C                   END                                                    END *IN50
     C                   MOVE      '0'           *IN89                          CASE PRINT
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM GROUP TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRGRP         BEGSR
     C*
     C     *IN91         IFEQ      '1'                                          SRCASE DONE
     C*
     C                   ADD       XXAGRP        XXASUB           11 2          SUBS AMOUNT
     C                   ADD       XXCGRP        XXCSUB            4 0          SUBS COUNT
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    GRPHD
     C                   MOVE      '0'           *INOF
     C                   END                                                    END *INOF
     C                   EXCEPT    GRPTOT
     C                   MOVE      '1'           *IN92                          SRGRP DONE
     C*
     C                   END                                                    END IF *IN91
     C*
     C     *INLR         IFEQ      '0'
     C     CLSUB#        ANDNE     XXSUB#
     C     XXSUB#        ANDNE     *ZEROS
     C                   EXSR      SRSUBS
     C                   END                                                    END *INLR
     C*
     C                   Z-ADD     CLFND         XXFND
     C                   MOVE      '0'           *IN91
     C*
     C     *INLR         IFEQ      '0'
     C*  CHAIN TO FUND MASTER FILE - GET FUND NAME
     C                   MOVE      *BLANKS       XXFNNM
     C     FNKEY         CHAIN     PFN01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM
     C                   END                                                    END *IN50
     C                   EXCEPT    GRPHD
     C                   END                                                    END *INLR
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM SUBSCRIBER TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRSUBS        BEGSR
     C*
     C     *IN92         IFEQ      '1'                                          SRGRP DONE
     C*
     C                   ADD       1             X                              ARRAY INDEX
     C                   Z-ADD     XXSUB#        SB(X)                          ALL SUSBSCRIBERS
     C                   Z-ADD     XXASUB        AM(X)                          ALL AMOUNT
     C                   Z-ADD     XXCSUB        CT(X)                          ALL COUNT
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    SUBTOT
     C*
     C                   END                                                    END IF *IN92
     C*
     C                   Z-ADD     CLSUB#        XXSUB#
     C                   MOVE      '0'           *IN92
     C                   MOVE      '1'           *IN93
     C*
     C     *INLR         IFEQ      '0'
     C                   MOVE      *BLANKS       XXSBNM
     C*  CHAIN TO SUBSCRIBER MASTER FILE - GET SUBSCRIBER NAME
     C     SMKEY         CHAIN     PSM01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM
     C                   END                                                    END *IN50
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *IN93
     C                   END                                                    END *INLR
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM ALL SUBSCRIBER TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRALL         BEGSR
     C*
     C     *IN93         IFEQ      '1'                                          SRSUBS DONE
     C*
     C                   MOVE      *BLANKS       XXSBNM
     C                   EXCEPT    HEAD
     C                   EXCEPT    ALLHD
     C*
     C                   Z-ADD     1             X
     C     SB(X)         DOWNE     *ZERO
     C     X             ANDLE     99
     C                   ADD       AM(X)         XXATOT           12 2          TOT AMOUNT
     C                   ADD       CT(X)         XXCTOT            4 0          TOT COUNT
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    ALLHD
     C                   MOVE      '0'           *INOF
     C                   END                                                    END *INOF
     C                   EXCEPT    ALLDET
     C                   ADD       1             X
     C                   END                                                    END DOWNE 0
     C*
     C                   EXCEPT    ALLTOT
     C*
     C                   END                                                    END IF *IN93
     C                   ENDSR
     C****
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
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
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
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
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
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                13 '(C) COPYRIGHT'
     O***                                30 'PROGRESSIVE DATA'
     O***                                51 'SOLUTIONS, INC. 1992'
      ****** END DLT ***** 01/28/10 **********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSDATE        Y      8
     O                       XXCONM              84
     O                       DSTIME             132 '  :  :  '
     O          E            HEAD           2
     O                       ORPNAM              10
     O                       ORTITL              94
     O                       PAGE               132
     O                                          127 'PAGE:'
     O          E            HEAD           2
     O                       XXFDT               62 '  /  /  '
     O                                           64 '-'
     O                       XXTDT               73 '  /  /  '
     O          E            HEAD           1
     O                                           10 'SUBSCRIBER'
     O                       XXSUB#              16
     O                                           18 '-'
     O                       XXSBNM              59
     O          E            GRPHD          2
     O                                            7 'GROUP #'
     O                       XXFND               16
     O                                           18 '-'
     O                       XXFNNM              59
     O          E            GRPHD          1
     O                                            7 'TRANS'
     O                                           33 'ACCIDENT'
     O                                           85 'SERVICE DATES'
     O          E            GRPHD          2
     O                                            6 'DATE'
     O                                           21 'SOC SEC #'
     O                                           31 'DATE'
     O                                           48 'FND YR  CASE'
     O                                           66 'PROVIDER  MOD'
     O                                           84 'FROM      TO'
     O                                           98 'CODE MOD'
     O                                          113 'CHARGES'
     O          E            DETAIL         1
     O                       XXTRDT               8 '  /  /  '
     O                       SSPRE               14
     O                                           15 '-'
     O                       SSMID               17
     O                                           18 '-'
     O                       SSSUF               22
     O                       XXACDT              33 '  /  /  '
     O                       XXFND               39
     O                       XXCS#               50
     O                       XXFEIN        Z     62
     O               81                          63 '-'
     O               81      XXMOD         Z     66
     O                       XXFDAT              78 '  /  /  '
     O                                           79 '-'
     O                       XXTDAT              87 '  /  /  '
     O                       XXCODE              95
     O               82                          96 '-'
     O               82      XXPMOD              98
     O                       XXAMT         2    113
     O          E            CASTOT      1  2
     O                                           52 'CASE TOTALS:'
     O                                           72 'NUMBER OF CHARGES:'
     O                       XXCCAS        ZB    78
     O                                           98 'TOTAL CHARGES:'
     O                       XXACAS        2B   113
     O          E            GRPTOT      1
     O                                           52 'GROUP TOTALS:'
     O                                           72 'NUMBER OF CHARGES:'
     O                       XXCGRP        ZB    78
     O                                           98 'TOTAL CHARGES:'
     O                       XXAGRP        2B   113
     O          E            SUBTOT         2
     O                                           60 'SUBSCRIBER'
     O                       XXSUB#              68
     O                                           75 'TOTALS'
     O          E            SUBTOT         2
     O                                           63 'NUMBER OF CHARGES:'
     O                       XXCSUB        ZB    82
     O          E            SUBTOT         2
     O                                           63 'TOTAL CHARGES:'
     O                       XXASUB        2B    82
     O          E            ALLTOT         2
     O                                           75 'ALL SUBSCRIBER TOTALS:'
     O          E            ALLHD          1
     O                                           63 'NUMBER OF'
     O                                           85 'TOTAL'
     O          E            ALLHD          2
     O                                           40 'SUBSCRIBER'
     O                                           61 'CHARGES'
     O                                           85 'AMOUNT'
     O          E            ALLDET         2
     O                       SB(X)         Z     37
     O                       CT(X)         Z     61
     O                       AM(X)         2     90
     O          E            MSG         3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
