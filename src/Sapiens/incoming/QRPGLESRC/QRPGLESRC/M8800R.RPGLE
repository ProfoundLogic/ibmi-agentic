     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8800R - AUTHORIZED USERS LIST
     H****************************************************************
     H* REVISIONS:                                                   *
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
     H****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FMMOPL     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)

     D                 DS
     D  OT                     1      6
     D                                     DIM(6)
     D  OPTY01                 1      1
     D  OPTY02                 2      2
     D  OPTY03                 3      3
     D  OPTY04                 4      4
     D  OPTY05                 5      5
     D  OPTY06                 6      6
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
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
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQUSER           10
     C                   PARM                    QQQUE            10            MLS 1/20/88
     C**
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQUSER        XXUSER           10
     C                   TIME                    DSDTTM
     C                   MOVE      '0'           *INOF
     C*                                                    MLS 1/20/88
     C*   SET UP AND INITIALIZE                                 "
     C*                                                         "
     C     QQQUE         IFNE      ' '                                               "
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE                                                        "
     C                   SETON                                        08        BATCH MODE
     C                   END                                                         "
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     COKEY         CHAIN     SMCOL                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********          MOVE      '0'           XXPRT
     C*********     ORKEY         CHAIN     POR01                              60
     C*********     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C*********          EXCEPT    ROUTE
     C*********          END
      ***** End Del ***** 08/26/14 *********************************************
     C                   EXCEPT    HEAD
     C*
     C*
     C*  GET THE MENU OPTION LIST
     C*
     C                   READ      MMOPL                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL     OPEXCL        XXEXCL          119
     C                   MOVEL     OPTY01        XXTY01           01
     C                   MOVEL     OPTY02        XXTY02           01
     C                   MOVEL     OPTY03        XXTY03           01
     C                   MOVEL     OPTY04        XXTY04           01
     C                   MOVEL     OPTY05        XXTY05           01
     C*
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C*
     C*  CHECK TO SEE IF THE USER IS AUTHORIZED TO READ THE REPORT
     C*
     C                   EXSR      SRAUT
     C*
     C  N55              EXCEPT    DET
     C*
     C                   READ      MMOPL                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*   SUBROUTINE TO CHECK AUTHORITY
     C*
     C     SRAUT         BEGSR
     C*
     C                   MOVE      '0'           *IN55
     C*
     C*  IF THE RECORD IS A MENU RECORD, DO NOT DISPLAY
     C*
     C     OPOPTN        COMP      *BLANK                                 55
     C*
     C*  IF THE RECORD IS NOT ACTIVE DO NOT DISPLAY
     C*
     C  N55OPSTAT        COMP      'I'                                    55
     C*
     C*  IF THE SECURITY TYPE FROM THE USER FILE IS NOT
     C*  "*" THEN CHECK THE TYPE, AND AUTHORITY
     C*
     C     *IN55         CABEQ     '1'           SRAUTE
     C*
     C     UPTYPE        IFNE      '*'
     C     UPTYPE        ANDNE     '#'
     C     UPTYPE        ANDNE     '$'
     C     UPTYPE        ANDNE     '&'
     C     UPTYPE        ANDNE     '%'
     C*
     C*  LOOK FOR THE SECURITY TYPE, FROM THE OPTION
     C*
     C     UPTYPE        IFNE      *BLANK
     C     UPTYPE        LOOKUP    OT                                     60
     C  N60              MOVE      '1'           *IN55
     C   60              DO
     C*
     C*  THE VALUE FROM THE USER FILE MUST BE GREATER OR EQUAL
     C*
     C     UPLVL         IFLT      OPMINS
     C                   MOVE      '1'           *IN55
     C                   END
     C     UPLVL         IFLT      OPMMSC
     C                   MOVE      '1'           *IN55
     C                   END
     C*
     C                   END
     C                   ELSE
     C                   MOVE      '1'           *IN55
     C                   END
     C                   ELSE
     C*
     C*  CHECK THE SPECIAL TYPE CODES
     C*
     C                   MOVE      *BLANK        XXSPEC            1
     C     UPTYPE        IFNE      '*'
     C*
     C     1             DO        6             I2                5 0
     C     OT(I2)        IFNE      *BLANK
     C*
     C     UPTYPE        IFEQ      '#'
     C     OT(I2)        IFGE      'A'
     C     OT(I2)        ANDLE     'M'
     C     OT(I2)        OREQ      '#'
     C                   MOVE      '#'           XXSPEC
     C                   END
     C                   END
     C*
     C     UPTYPE        IFEQ      '$'
     C     OT(I2)        IFGE      'N'
     C     OT(I2)        ANDLE     'Z'
     C     OT(I2)        OREQ      '$'
     C                   MOVE      '$'           XXSPEC
     C                   END
     C                   END
     C*
     C     UPTYPE        IFEQ      '&'
     C     OT(I2)        IFGE      'A'
     C     OT(I2)        ANDLE     'Z'
     C     OT(I2)        OREQ      '&'
     C     OT(I2)        OREQ      '#'
     C     OT(I2)        OREQ      '$'
     C                   MOVE      '&'           XXSPEC
     C                   END
     C                   END
     C*
     C     UPTYPE        IFEQ      '%'
     C     OT(I2)        IFGE      '0'
     C     OT(I2)        ANDLE     '9'
     C                   MOVE      '%'           XXSPEC
     C                   END
     C                   END
     C*
     C                   END
     C                   END
     C*
     C     XXSPEC        IFEQ      ' '
     C                   MOVE      '1'           *IN55
     C                   END
     C*
     C     UPLVL         IFLT      OPMINS
     C                   MOVE      '1'           *IN55
     C                   END
     C     UPLVL         IFLT      OPMMSC
     C                   MOVE      '1'           *IN55
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C     SRAUTE        ENDSR
     C*
     C**
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
     O*  MLS 1/20/88  ADD INDICATORS FOR DISTRIBUTION INFORMATION
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
     O*  MLS 1/20/88  ADD INDICATORS FOR DISTRIBUTION INFORMATION
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
     O*  MLS 1/20/88 ADD INDICATORS AND OUT QUEUE
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                56 '.1990'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             115
     O                       DSDATE        Y    196
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             126
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
     O          E            HEAD           2
     O                                           10 'USER:'
     O                       XXUSER              21
     O                                           25 'SEC:'
     O                       UPTYPE              27
     O                                           35 'LVL:'
     O                       UPLVL               39
     O          E            HEAD           1
     O                                           06 'MENU'
     O                                           27 'SEC'
     O                                           31 'LVL'
     O                                           36 'SEC'
     O          E            HEAD           2
     O                                           06 'NAME'
     O                                           17 'OPTION'
     O                                           21 'APP'
     O                                           27 'REQ'
     O                                           31 'REQ'
     O                                           37 'TYPE'
     O                                           49 'DESCRIPTION'
     O                                           91 'EXECUTED CLP'
     O          E            DET            1
     O                       OPMNAM              10
     O                       OPOPTN              15
     O                       OPMYN               17
     O                       OPAPP               21
     O                       OPYNO               23
     O                       OPMMSC        Z     27
     O                       OPMINS        Z     31
     O                       XXTY01              33
     O                       XXTY02              34
     O                       XXTY03              35
     O                       XXTY04              36
     O                       XXTY05              37
     O                       OPMNTT              78
     O                       XXEXCL             198
     O          E            DET            1
     O                       OPTX01              20
     O                       OPTX02              41
     O                       OPTX03              63
     O                       OPTX04              84
     O                       OPTX05             105
     O                       OPTX06             126
     O          E            DET            2
     O                       OPTX07              20
     O                       OPTX08              41
     O                       OPTX09              63
     O                       OPTX10              84
     O                       OPTX11             105
     O                       OPTX12             126
