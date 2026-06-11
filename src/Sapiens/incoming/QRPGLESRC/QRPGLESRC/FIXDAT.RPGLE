     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      ***************************************************************
      *                                                            **
      * PROGRAM:     FIXDAT                                        **
      * DESCRIPTION: Reformat the dates in SMARP and IMDNP         **
      * CREATED BY:  C:128  PROJ:         DATE:  04/06/04          **
     H***************************************************************
     FIMDNP     UF   E             DISK
      *
     FSMARP     UF   E             DISK
      *

     D                 DS                  INZ
     D  CHKDAT                 1      8  0
     D  POS1                   1      1  0
     D  POS2                   2      2  0
     D  MMDD                   1      4  0
     D  YR4                    5      8  0
      *
     D                 DS                  INZ
     D  INDATE                 1      6  0
     D  INYY                   1      2  0
     D  INMD                   3      6  0
      *
     D                 DS                  INZ
     D  OUTDAT                 1      7  0
     D  CENT                   1      1  0
     D  OUTYY                  2      3  0
     D  OUTMD                  4      7  0
     D                 DS                  INZ
     D  OUTDN                  1      8  0
     D  DNYYYY                 1      4  0
     D  DNMMDD                 5      8  0
      *

     C*
     C     DT6POS        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YMD'        SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C*** Read the Darts additional remarks file.
     C                   READ      PAR01                                  60
B01  C     *IN60         DOWEQ     *OFF
      *=== ARCDAT =====================================================
     C                   MOVE      ARCDAT        INDATE
     C                   MOVE      ARCDAT        CHKDAT
      *
B02  C     POS2          IFGT      2
     C                   EXSR      REVDAT
E02  C                   ENDIF
      *
     C                   EXSR      FIXDAT
      *
     C                   MOVE      OUTDAT        ARCDAT
      *
      *=== ARTRDT =====================================================
     C                   MOVE      ARTRDT        INDATE
     C                   MOVE      ARTRDT        CHKDAT
      *
B02  C     POS2          IFGT      2
     C                   EXSR      REVDAT
E02  C                   ENDIF
      *
     C                   EXSR      FIXDAT
      *
     C                   MOVE      OUTDAT        ARTRDT
      *
      *=== ARDT =======================================================
     C                   MOVE      ARDT          INDATE
     C                   MOVE      ARDT          CHKDAT
      *
B02  C     POS2          IFGT      2
     C                   EXSR      REVDAT
E02  C                   ENDIF
      *
     C                   EXSR      FIXDAT
      *
     C                   MOVE      OUTDAT        ARDT
      *
      *=== ARIDT ======================================================
     C                   MOVE      ARIDT         INDATE
     C                   MOVE      ARIDT         CHKDAT
      *
B02  C     POS2          IFGT      2
     C                   EXSR      REVDAT
E02  C                   ENDIF
      *
     C                   EXSR      FIXDAT
      *
     C                   MOVE      OUTDAT        ARIDT
     C                   UPDATE    PAR01
      *
     C                   READ      PAR01                                  60
E01  C                   ENDDO                                                  ENDDOW *IN60
      *
     C*** Read the Image Doc. Notes file.
     C                   READ      PDN01                                  61
B01  C     *IN61         DOWEQ     *OFF
      *
      *=== DNCDAT =====================================================
     C                   MOVE      DNCDAT        CHKDAT
      *
B02  C     POS1          IFEQ      *ZERO
     C                   MOVE      MMDD          DNMMDD
     C                   MOVE      YR4           DNYYYY
E02  C                   ENDIF
      *
     C                   MOVE      OUTDN         DNCDAT
     C                   UPDATE    PDN01
      *
     C                   READ      PDN01                                  61
E01  C                   ENDDO                                                  ENDDOW *IN60
     C*
     C*** End the program
     C*
     C                   MOVE      *ON           *INLR
     C*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C     FIXDAT        BEGSR
     C                   Z-ADD     INDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DT6POS
     C                   MOVEL     SRDTO         OUTDAT
     C                   ENDSR
     C*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     C     REVDAT        BEGSR
     C                   MOVE      YR4           INYY
     C                   MOVE      MMDD          INMD
     C                   ENDSR
     C*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
