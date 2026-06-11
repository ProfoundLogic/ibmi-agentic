     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*                                                               *
     H/TITLE FIXWDXSP - COPY OF DEFAULT RECORDS TO ALL WDELP RECORDS.
     H*****************************************************************
     H*  DESCRIPTION: COPY DEFAULT WDXSP RECORDS TO ALL WDELP RECORDS.*
     H*                                                               *
     H*  PROJECT:  114WAK  DATE: 02/13/04  PGMR: 65                   *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/18/12 C:136 SUPPORT: WA093 80072                          *
     H*                 ADD PARMS THAT WILL RUN THE PROCESS BY        *
     H*                 CO/FND/FYR. THE ORIGINAL PROGRAM RAN          *
     H*                 OVER ALL POLICIES. IF CO# IS PASSED IN,       *
     H*                 IT RUNS FOR ONLY CO#.  IF CO#/FND IS          *
     H*                 PASSED IN, IT RUNS FOR ALL POLICIES IN        *
     H*                 CO#/FND. IF CO#/FND/FYR IS PASSED IN, IT      *
     H*                 RUNS FOR ALL POLICIES IN THAT CO#/FND/FYR.    *
     H*                                                               *
     H*****************************************************************
     FWDELL1    IF   E           K DISK
     FWDXSL     IF A E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *   EXCLUDED STATES FILE
      *

     C     ELKY1         KLIST
     C                   KFLD                    KKCO#             3 0
      *
     C     ELKY2         KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKFND             3 0
      *
     C     ELKY3         KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKFND             3 0
     C                   KFLD                    KKFYR             3 0
      *
     C     XSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    X1EMP#            9 0
     C                   KFLD                    X1FYR             3 0
     C*
     C     XSKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELFYR
     C                   KFLD                    XSST
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C                   PARM                    SRDTI             8 0
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQFYR             3 0
      *
     C     Q1PRMS        IFGE      1
     C                   Z-ADD     QQCO#         KKCO#
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      2
     C                   Z-ADD     QQFND         KKFND
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      3
     C                   Z-ADD     QQFYR         KKFYR
     C                   ENDIF
      *
     C                   Z-ADD     0             X1EMP#
     C                   Z-ADD     0             X1FYR
     C*
     C* READ EACH EMPLOYER
     C*
     C                   SELECT
     C     Q1PRMS        WHENGE    3
     C     ELKY3         SETLL     PEL01
     C     ELKY3         READE     PEL01                                  50
     C     Q1PRMS        WHENGE    2
     C     ELKY2         SETLL     PEL01
     C     ELKY2         READE     PEL01                                  50
     C     Q1PRMS        WHENGE    1
     C     ELKY1         SETLL     PEL01
     C     ELKY1         READE     PEL01                                  50
     C                   OTHER
     C                   READ      PEL01                                  50
     C                   ENDSL
     C*
     C     *IN50         DOWEQ     *OFF
     C     XSKY          CHAIN     PXS01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     XSKY1         CHAIN     PXS01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     ELEMP#        XSEMP#
     C                   Z-ADD     ELFYR         XSFYR
     C                   MOVEL     Q1USER        XSIUSR
     C                   MOVEL     Q1USER        XSUSER
     C                   MOVEL     Q1PGM         XSIPGM
     C                   MOVEL     Q1PGM         XSPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XSIDT
     C                   MOVEL     SRDTO         XSDT
     C                   MOVEL     DSTIME        XSITIM
     C                   MOVEL     DSTIME        XSTIME
     C                   WRITE     PXS01
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XSKY          READE     PXS01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* READ NEXT POLICY
     C*
     C                   SELECT
     C     Q1PRMS        WHENGE    3
     C     ELKY3         READE     PEL01                                  50
     C     Q1PRMS        WHENGE    2
     C     ELKY2         READE     PEL01                                  50
     C     Q1PRMS        WHENGE    1
     C     ELKY1         READE     PEL01                                  50
     C                   OTHER
     C                   READ      PEL01                                  50
     C                   ENDSL
     C*
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
