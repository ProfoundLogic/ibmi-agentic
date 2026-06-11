     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDELP3 - CORRECT SCHED DB/CR FACTOR
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWDELP     UF   E             DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C     *LIKE         DEFINE    ELDBCR        XXDBCR
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   READ      PEL01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      'N'           XXERR             1
     C*
     C                   EXSR      CHKSDC
     C*
     C     XXERR         IFEQ      'Y'
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C                   MOVEL(P)  Q1PGM         ELPGM
     C                   UPDATE    PEL01
     C                   ENDIF
     C*
     C                   READ      PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*****************************************************************
     C*
     C* CHKSDC - CHECK SCHEDULE DEBIT/CREDIT FACTOR
     C*
     C     CHKSDC        BEGSR
     C*
     C     ELDBCR        IFLT      .75
     C     ELDBCR        ANDNE     *ZERO
     C     ELDBCR        ORGT      1.25
     C*
     C                   SELECT
     C*
     C     ELDBCR        WHENGT    *ZERO
     C     1             SUB       ELDBCR        XXDBCR
     C                   Z-ADD     XXDBCR        ELDBCR
     C                   MOVE      'Y'           XXERR
     C*
     C     ELDBCR        WHENLT    *ZERO
     C     1             SUB       ELDBCR        XXDBCR
     C                   Z-ADD     XXDBCR        ELDBCR
     C                   MOVE      'Y'           XXERR
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  ELDBCR
     C*
     C                   ENDSR                                                  CHKSBJ SUBR
     C*
     C*****************************************************************
