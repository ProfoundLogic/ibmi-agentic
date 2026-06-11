     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***************************************************************
     H*   THIS PROGRAM WILL CONVERT THE BLANK STATUS TO ACTIVE
     H*   STATUS IN THE COMPANY FILES
     H*
     H*   CREATED BY C:94
     H***************************************************************
     FSMCOP     UF   E             DISK
     F                                     RENAME(PCO01:PFRCDF)

     D*
     D**SYSTEM DATE AND TIME
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   READ      PFRCDF                                 99
     C     *IN99         DOWNE     '1'
     C     COSTAT        IFEQ      *BLANK
     C                   MOVE      'A'           COSTAT
     C                   MOVEL     Q1PGM         COPGM
     C                   MOVEL(P)  Q1USER        COUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        COTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C                   UPDATE    PFRCDF
     C                   ENDIF
     C                   READ      PFRCDF                                 99
     C                   ENDDO
     C                   MOVE      '1'           *INLR
