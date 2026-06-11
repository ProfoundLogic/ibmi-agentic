      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD007H - Print affected reserves before and after update
      *****************************************************************
      *
      * THIS pgm DOES NOT UPDATE
      *
      *****************************************************************
      * DESCRIPTION:  This pgm prints the affected reserves
      *               before and after update.
      *
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FBADPV4    IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     D X               S              3  0 INZ
     D TOTCNT          S              5  0 INZ
     D TOTAMT          S                   LIKE(CMAR01) INZ
     D TOTAMT2         S                   LIKE(CMAR01) INZ
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  RC                     1     20
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
     D  RA                     1     90  2
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
     C     *ENTRY        PLIST
     C                   PARM                    BEFAFT            1

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     CMKY          KLIST
     C                   KFLD                    P4CO
     C                   KFLD                    P4FND
     C                   KFLD                    P4FYR
     C                   KFLD                    P4CASE

     C     BEFAFT        COMP      'A'                                    59
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   EXCEPT    HEAD

     C                   READ      BADPV4
     C                   DOW       NOT %EOF(BADPV4)
     C     CMKY          CHAIN     WMCML1
     C                   IF        %FOUND(WMCML1)
     C                   Z-ADD     1             X
     C     P4RECD        LOOKUP    RC(X)                                  70
     C                   IF        *IN70
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   EVAL      TOTCNT = TOTCNT + 1
     C                   ENDIF
     C                   ENDIF
     C                   READ      BADPV4
     C                   ENDDO
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    TOT1
     C                   EVAL      *INLR = *ON
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O              N59                          59 'Before Update -'
     O               59                          59 'After  Update -'
     O                                           75 'Claims reserves'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           11 'WTFIXD007H'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            3 'FND'
     O                                           10 'YEAR'
     O                                           20 'CLAIM #'
     O                                           27 'CODE'
     O                                           41 'REM RSV'
     O                                           60 'PAYMT AGAINST RSV'
     O          E            DET            1
     O                       CMFND         4      3
     O                       CMFYR         4      9
     O                       CMCASE        4     20
     O                       RC(X)               26
     O                       RR(X)         K     41
     O                       RA(X)         K     56
     O          E            TOT1        2
     O                       TOTCNT        K     26
     O**                     TOTAMT        K     41
     O**                     TOTAMT2       K     56
