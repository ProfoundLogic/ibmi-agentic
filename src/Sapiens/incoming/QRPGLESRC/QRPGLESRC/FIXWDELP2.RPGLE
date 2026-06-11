     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDELP81 - CORRECT SUBJ AND STD PREM FOR DB/CR
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
     C                   MOVE      *BLANK        XXUP              1
     C*
     C                   EXSR      CHKSBJ
     C*
     C     XXUP          IFEQ      'Y'
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
     C* CHKSUB - CHECK SUBJECT PREMIUM SUBROUTINE
     C*
     C     CHKSBJ        BEGSR
     C*
     C     ELESBJ        IFEQ      *ZERO
     C     ELSTPR        ANDNE     *ZERO
     C                   Z-ADD     ELMAPR        ELESBJ
     C                   ADD       ELEIN1        ELESBJ
     C                   ADD       ELEIN2        ELESBJ
     C                   SUB       ELEDCT        ELESBJ
     C                   MOVE      'Y'           XXUP
     C                   ENDIF                                                  ELESBJ IFEQ
     C*
     C     ELASBJ        IFEQ      *ZERO
     C     ELASPR        ANDNE     *ZERO
     C                   Z-ADD     ELAMPR        ELASBJ
     C                   ADD       ELAIN1        ELASBJ
     C                   ADD       ELAIN2        ELASBJ
     C                   SUB       ELADCT        ELASBJ
     C                   MOVE      'Y'           XXUP
     C                   ENDIF                                                  ELASBJ IFEQ
     C*
     C                   ENDSR                                                  CHKSBJ SUBR
     C*
     C*****************************************************************
