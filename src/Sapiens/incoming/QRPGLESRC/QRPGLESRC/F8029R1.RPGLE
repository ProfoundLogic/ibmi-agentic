    Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
    H StoneRiver is a trademark of StoneRiver, Inc.')
    H option(*nodebugio:*srcstmt)
    H dftactgrp(*no) actgrp(*caller)
     H*
     H* Title F8029R1 - Corvel Upload Process
     H*
     H*****************************************************************
     H* Created on 10/21/2016 by c:212 for Project 89MBT              *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFXU0L     IF   E           K DISK
     F                                     RENAME(QWHFDML:DBRCDF)
     FFDOCP     IF   E             DISK
     FFPRVP     O    E             DISK    usropn
     D*
     D OC              S              1    DIM(500)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  MBR1                   1     10
     D  LTRL1                  1      1    INZ('P')
     D  YYYEAR                 2      5  0
     D  YYDAYS                 6      8  0
     D  DATE1                  2      8  0
     D  SEQ1                   9     10  0
     D*
     D                 DS
     D  MBR2                   1     10
     D  LTRL2                  1      1    INZ('P')
     D  DATE2                  2      8  0
     D  SEQ2                   9     10  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTLJUL        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*JUL'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     U0KY          KLIST
     C                   KFLD                    KKNAME
     C*
     C     *LIKE         DEFINE    MLNAME        KKNAME
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C     *IN99         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   EXSR      ASSMEM
     C*
     C                   CLEAR                   POC01
     C     1             SETLL     POC01
     C                   READ      POC01                                  99
     C                   IF        NOT *IN99 OR @@CALL<>'P'
     C                   IF        @@CALL='P'
     C
      * Filter out characters that can not be displayed on the AS400
     C                   CLEAR                   OC
     C                   MOVEA     OCDCID        OC
      *
     C     1             DO        500           X                 3 0
     C     OC(X)         IFLT      X'40'
      *
     C     OC(X)         IFNE      X'08'
     C     OC(X)         ANDNE     X'09'
     C                   MOVE      X'40'         OC(X)
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     OC(X)         IFGT      X'F9'
     C                   MOVE      X'40'         OC(X)
     C                   ENDIF
     C                   ENDDO
     C                   MOVEA     OC            OCDCID
     C*
     c                   open      fprvp
     c                   clear                   ppr01
     C                   eval      prdata='H' + ocdcid
     c                   write     ppr01
     c                   close     fprvp
     c                   eval      docid=ocdcid
     c                   else
     c                   eval      docid=*blanks
     c                   endif
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   CALL      'F8039C'
     C                   PARM      MBR1          MEMBER           10
     C                   PARM                    DOCID           500
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C*****************************************************************
     C* ASSMEM - ASSIGN FILE MEMBER
     C*****************************************************************
     C     ASSMEM        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTLJUL
     C                   MOVEL     SRDTO         DATE5             5 0
     C                   MOVE      DATE5         YYDAYS
     C                   MOVE      DSDATE        YYYEAR
     C*
     C                   Z-ADD     99            SEQ1
     C*
     C                   MOVEL(P)  MBR1          KKNAME
     C     U0KY          SETGT     DBRCDF
     C                   READP     DBRCDF                                 99
     C     *IN99         IFEQ      *OFF
     C     MLNAME        ANDNE     'FPRVP'
     C                   MOVEL     MLNAME        MBR2
     C     DATE1         IFEQ      DATE2
     C     SEQ2          ADD       1             SEQ1
     C                   ELSE
     C                   Z-ADD     0             SEQ1
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     0             SEQ1
     C                   ENDIF
     C*
     C                   ENDSR
