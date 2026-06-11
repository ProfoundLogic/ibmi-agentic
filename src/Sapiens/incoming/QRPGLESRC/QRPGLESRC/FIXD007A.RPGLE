      *  (C) 2012 StoneRiver Inc. All rights reserved.
      *   StoneRiver is a trademark of StoneRiver, Inc.
      *
      *****************************************************************
     H/TITLE FIXD007A - Check claim reserves without reserve code
      *****************************************************************
      *
      * THIS pgm DOES NOT UPDATE
      *
      * DESCRIPTION:  This pgm lists claims with reserves that
      *               have no reserve codes.
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
      *
     FBADPV3    IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     D IJ              S              3  0 INZ
     D TOTAMT          S                   LIKE(CMAR01) INZ
     D TOTCNT          S              5  0 INZ
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C**************************************************************************
     C*
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     CMKY          KLIST
     C                   KFLD                    P3CO
     C                   KFLD                    P3FND
     C                   KFLD                    P3FYR
     C                   KFLD                    P3CASE



     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     0             Z                 3 0
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   EXCEPT    HEAD
     C                   EXSR      CHK1
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    TOT1
     C                   EVAL      *INLR = *ON

     C**************************************************************************
     C*  Check reserves without reserve codes for claims in FILE 1
     C**************************************************************************
     C     CHK1          BEGSR
     C                   READ      BADPV3

     C                   DOW       NOT %EOF(BADPV3 )
     C     CMKY          CHAIN     PCM01                              50
     C     *IN50         IFEQ      '0'
      *
      *  Check for reserve amounts without reserve codes
      *
     C                   DO        10            IJ
     C                   IF        RC(IJ) = *BLANKS and RR(IJ)<> *ZEROS
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   EVAL      TOTCNT = TOTCNT + 1
     C                   ADD       RR(IJ)        TOTAMT
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   READ      BADPV3
     C                   ENDDO
     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           75 'Claims without rsv code'
     O                       DSDATE        Y    131
     O          E            HEAD           2
     O                                           11 'WTFIXD007A'
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           2
     O                                            3 'FND'
     O                                           10 'YEAR'
     O                                           20 'CLAIM #'
     O                                           27 'CODE'
     O                                           40 'RSV AMOUT '
     O          E            DET            1
     O                       CMFND         4      3
     O                       CMFYR         4      9
     O                       CMCASE        4     20
     O                       RC(IJ)              26
     O                       RR(IJ)        K     41
     O          E            TOT1        2
     O                                           10 'TOTAL'
     O                       TOTCNT        Z     26
     O                       TOTAMT        K     41
