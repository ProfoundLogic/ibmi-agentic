     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     UF   E           K DISK
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXPFDT                 1      7  0
     D  YY                     1      3  0
     D  MM                     4      5  0
     D  DD                     6      7  0
     D  YYMM                   1      5  0

     C*
     C* GROUP MASTER KEY
     C*
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C* GROUP DETAIL KEY
     C*
     C     F1KY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C*
     C* EMPLOYER MASTER KEY
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUPD             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      *OFF          *IN66
     C     QQUPD         IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *ON           *INOF
     C*
     C* READ THROUGH ALL EMPLOYERS.
     C*
     C                   READ      PEL01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     FNKY          CHAIN     PFN01                              30
     C*
     C     *IN30         IFEQ      *OFF
     C     FNCGFL        ANDNE     'Y'
     C*
     C     EMKY          CHAIN     PEM01                              40
     C*
     C* ONLY PROCESS NON-CANCELLED EMPLOYERS.
     C*
     C     *IN40         IFEQ      *OFF
     C     EMCAD1        ANDEQ     *ZERO
     C*
     C                   Z-ADD     ELBSCD        SVBSCD            3 0
     C*
     C*  CONVERT THESE BILLING SCHEDULE CODES BY ADDING THE MONTHS
     C*  OF THE POLICY TO CALCULATE THE CORRECT CODE.
     C*
     C     ELBSCD        IFEQ      100
     C     ELBSCD        OREQ      150
     C     ELBSCD        OREQ      200
     C     ELBSCD        OREQ      250
     C*
     C     F1KY          CHAIN     PF101                              50
     C     *IN50         IFEQ      *OFF
     C*
     C     F1STPO        ADD       10000         XXDATE            7 0
     C*
     C     XXDATE        IFEQ      F1ENPO
     C*
     C                   Z-ADD     F1STPO        XXPFDT
     C                   Z-ADD     MM            SVMM              2 0
     C                   Z-ADD     YY            SVYY              3 0
     C*
     C                   Z-ADD     ELPFDT        XXPFDT
     C*
     C     YY            IFGT      SVYY
     C                   ADD       12            MM
     C                   ENDIF
     C*
     C                   SUB       SVMM          MM
     C*
     C                   MULT      2             MM
     C*
     C                   ADD       MM            ELBSCD
     C*
     C     DD            IFGT      15
     C                   ADD       1             ELBSCD
     C                   ENDIF
     C*
     C     ELBSCD        IFNE      SVBSCD
     C*
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C* UPDATE EMPLOYER DETAIL AND RESET FILE.
     C*
     C   66              UPDATE    PEL01
     C*
     C                   Z-ADD     F1STPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXSTPO            8 0
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1PFDT            8 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT            8 0
     C*
     C   OF              EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   EXCEPT    DET
     C*
     C                   ENDIF                                                  ELBSCD IFNE
     C*
     C                   ENDIF                                                  XXDATE IFEQ
     C*
     C                   ENDIF                                                  *IN50 IFEQ
     C*
     C                   ENDIF                                                  ELBSCD IFEQ
     C*
     C                   ENDIF                                                  *IN40 IFEQ
     C*
     C                   ENDIF                                                  *IN30 IFEQ
     C*
     C                   READ      PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      *ON           *INLR
     C*
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           54 'UPDATE SHORT TERM BSCDS'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                       Q1PGM               10
     O               66                          55 '** UPDATE MODE ACTIVE **'
     O                                           74 'PAGE:'
     O                       PAGE          Z     80
     O          E            HEAD           2
     O                                            5 'FND'
     O                                           17 'EMPLOYER'
     O                                           25 'DIV'
     O                                           31 'FYR'
     O                                           44 'GRP EFF DT'
     O                                           57 'POL EFF DT'
     O                                           70 'POL EXP DT'
     O                                           75 'OLD'
     O                                           80 'NEW'
     O          E            DET            1
     O                       ELFND         4      5
     O                       ELEMP#        4     17
     O                       ELDIV         4     25
     O                       ELFYR         4     31
     O                       XXSTPO              44 '  /  /    '
     O                       X1PFDT              57 '  /  /    '
     O                       XXPTDT              70 '  /  /    '
     O                       SVBSCD              75
     O                       ELBSCD              80
