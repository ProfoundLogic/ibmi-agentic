     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FSMKDL     UF A E           K DISK
     F*

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D****************************************************************
     D*   CONSTANTS
     D***************************************************************
     D DESC1           C                   CONST('WORK WITH WCMC GRO-
     D                                     UPS')
     D DESC2           C                   CONST('WORK WITH EMPLOYER-
     D                                     S')

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     *LIKE         DEFINE    KDPGMN        KKPGMN
     C*
     C     KDKY          KLIST
     C                   KFLD                    KKPGMN
     C*
     C                   MOVEL(P)  'F0060R'      KKPGMN
     C*
     C     KDKY          CHAIN     PKD01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  KKPGMN        KDPGMN
     C                   MOVEL(P)  'Co#'         KDHD01
     C                   MOVEL(P)  'Sub#'        KDHD02
     C                   MOVEL(P)  'Fnd'         KDHD03
     C                   MOVE      '3'           KDHL01
     C                   MOVE      '4'           KDHL02
     C                   MOVE      '3'           KDHL03
     C                   MOVE      '1'           KDDS01
     C                   MOVE      '4'           KDDS02
     C                   MOVE      '9'           KDDS03
     C                   MOVE      '3'           KDDL01
     C                   MOVE      '5'           KDDL02
     C                   MOVE      '3'           KDDL03
     C                   MOVE      '0'           KDDD01
     C                   MOVE      '0'           KDDD02
     C                   MOVE      '0'           KDDD03
     C                   MOVE      'A'           KDSTAT
     C                   MOVE      Q1USER        KDUSER
     C                   MOVE      Q1PGM         KDPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KDDT
     C                   MOVEL     DSTIME        KDTIME
     C                   MOVE      Q1USER        KDIUSR
     C                   MOVE      Q1PGM         KDIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KDIDT
     C                   MOVEL     DSTIME        KDITIM
     C                   MOVEL(P)  'FN'          KDTYPE
     C                   MOVEL(P)  DESC1         KDDESC
     C*
     C                   WRITE     PKD01
     C*
     C                   ENDIF
     C****************************************************************
     C                   MOVEL(P)  'F0140R'      KKPGMN
     C*
     C     KDKY          CHAIN     PKD01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL(P)  KKPGMN        KDPGMN
     C                   MOVEL(P)  'Co#'         KDHD01
     C                   MOVEL(P)  'Sub#'        KDHD02
     C                   MOVEL(P)  'Fnd'         KDHD03
     C                   MOVEL(P)  'Emp'         KDHD04
     C                   MOVEL(P)  'Div'         KDHD05
     C                   MOVE      '3'           KDHL01
     C                   MOVE      '4'           KDHL02
     C                   MOVE      '3'           KDHL03
     C                   MOVE      '3'           KDHL04
     C                   MOVE      '3'           KDHL05
     C                   MOVE      '1'           KDDS01
     C                   MOVE      '4'           KDDS02
     C                   MOVE      '9'           KDDS03
     C                   MOVE      '12'          KDDS04
     C                   MOVE      '21'          KDDS05
     C                   MOVE      '3'           KDDL01
     C                   MOVE      '5'           KDDL02
     C                   MOVE      '3'           KDDL03
     C                   MOVE      '9'           KDDL04
     C                   MOVE      '5'           KDDL05
     C                   MOVE      '0'           KDDD01
     C                   MOVE      '0'           KDDD02
     C                   MOVE      '0'           KDDD03
     C                   MOVE      '0'           KDDD04
     C                   MOVE      '0'           KDDD05
     C                   MOVE      'A'           KDSTAT
     C                   MOVE      Q1USER        KDUSER
     C                   MOVE      Q1PGM         KDPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KDDT
     C                   MOVEL     DSTIME        KDTIME
     C                   MOVE      Q1USER        KDIUSR
     C                   MOVE      Q1PGM         KDIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KDIDT
     C                   MOVEL     DSTIME        KDITIM
     C                   MOVEL(P)  'EM'          KDTYPE
     C                   MOVEL(P)  DESC2         KDDESC
     C*
     C                   WRITE     PKD01
     C*
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
