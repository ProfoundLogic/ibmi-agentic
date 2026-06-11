     H*  (C) 2011 STONERIVER, INC. ALL RIGHTS RESERVED.               *
     H*  STONERIVER IS A TRADEMARK OF STONERIVER, INC.                *
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H*                                                               *
     H* F80466R1 - EOB Export to SIS Build Member Name               *
     H*****************************************************************
     H*                                                               *
     H*  Called from: F8066C1 - EOB Export to SIS Override CL         *
     H*                                                               *
     H*****************************************************************
     H*  Created on 12/02/2011 by C:277 for Project 114MCV            *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*****************************************************************
     F*EOB EXPORT TRANSACTION FILE BY
     F*
     FFOZ9L2    IF   E           K DISK                                         <Z9>
     D*
     D                 DS
     D KKMBR#                  1     10
     D YYJUL                   2      8  0
     D B                       1      1
     D YYYEAR                  2      5  0
     D YYDAYS                  6      8  0
     D SEQ#                    9     10  0
     D                 DS
     D DSDTTM                  1     14  0
     D DSTIME                  1      6  0
     D DSDATE                  7     14  0
     D*
     D                 DS
     D Z9MBR                   1     10
     D YYJUL2                  2      8  0
     D XXSEQ#                  9     10  0
     C*
     C
     C     *ENTRY        Plist
     C                   PARM                    @@MBR            10
     C                   PARM                    @@SEQ             2 0
     C*
     C     Z9KY1         KLIST
     C                   KFLD                    KKMBR#
     C*
     C     DTJUL         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDYY'       SRFFMT            8
     C                   PARM      '*JUL'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   EXSR      SRMBRSEQ
     C* Assign the member name and member seq
     C
     C                   MOVEL     KKMBR#        @@MBR
     C                   EVAL      @@SEQ = SEQ#
     C*
     C* Program ends here
     C*
     C                   EVAL      *INLR = *ON
     C                   RETURN
     C
     C**************************************************************************
     C*  SRMBRSEQ - Retrives the Member name and member sequence
     C**************************************************************************
     C     SRMBRSEQ      BEGSR
     C                   Z-ADD     *ZEROS        YYJUL2
     C                   Z-ADD     *ZEROS        XXSEQ#
     C                   MOVEL(P)  'B'           B
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTJUL
     C                   MOVEL     SRDTO         YYJULI            5 0
     C*
     C                   Z-ADD     DSDATE        YYYEAR
     C                   Z-ADD     YYJULI        YYDAYS
     C                   Z-ADD     99            SEQ#
     C*
     C     Z9KY1         SETGT     Z9P01
     C                   READP     Z9P01                                  99
     C     *IN99         IFEQ      *OFF
     C     YYJUL         ANDEQ     YYJUL2
     C                   IF        XXSEQ# =  99
     C                   EVAL      SEQ# = 0
     C                   ELSE
     C*
     C                   EVAL      SEQ# = XXSEQ# + 1
     C                   ENDIF
     C                   ELSE
     C*
     C                   Z-ADD     0             SEQ#
     C                   ENDIF
     C*
     C                   ENDSR
