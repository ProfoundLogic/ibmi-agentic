     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*                                                              **
     H* PROGRAM:     FIXSMDLP                                        **
     H* DESCRIPTION: DARTS Records Transfer Program From WACTL Old   **
     H* to New.                                                      **
     H* CALLED FROM: SC900C                                          **
     H*  07/14/95 C:38  - Proj:SA100OJECT: 9/DR007                   **
     H*                                                              **
     H* CREATED BY:  Edwin Atzenhoffer    DATE:  07/14/95            **
     H*****************************************************************
     H* REVISIONS:                                                   **
     H*                                                              **
     H*                                                              **
     H*                                                              **
     H*****************************************************************
     H* INDICATOR USAGE:                                             **
     H*****************************************************************
     H* FILES:                                                       **
     H*****************************************************************
     H DECEDIT('0,') FORMSALIGN DFTNAME(FXSMDL) DATEDIT(*DMY/)
     F*==BEGIN========================================================
     F*
     FSMDLL     UF   E           K DISK
     FWACTL     IF   E           K DISK
     F*
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     F*==END  ========================================================

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  OLDKEY                 1     40
     D  X1CO#                  1      3  0
     D  X1FND                  4      6  0
     D  X1FYR                  7      9  0
     D  X1CASE                10     16  0
     D  X1FIL                 17     40
     D                 DS
     D  NEWKEY                 1     40
     D  X2CO#                  1      3  0
     D  X2FND                  4      6  0
     D  X2FYR                  7      9  0
     D  X2CASE                10     16  0
     D  X2FIL                 17     40
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*================================================================
     C*
     C*   KEY LIST FOR THE DARTS OPTICAL FILE
     C*
     C     DLKY          KLIST
     C                   KFLD                    OLDKEY
     C*
     C*          *LIKE     DEFN CTCO#     X1CO#
     C*          *LIKE     DEFN CTCO#     X2CO#
     C*          *LIKE     DEFN CTFND     X1FND
     C*          *LIKE     DEFN CTFND     X2FND
     C*          *LIKE     DEFN CTFYR     X1FYR
     C*          *LIKE     DEFN CTFYR     X2FYR
     C*          *LIKE     DEFN CTCASE    X1CASE
     C*          *LIKE     DEFN CTCASE    X2CASE
     C*
     C                   MOVE      *BLANKS       X1FIL
     C                   MOVE      *BLANKS       X2FIL
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C                   READ      PCT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CTFYR         IFNE      CTXFYR
     C     CTCASE        ANDEQ     CTXCAS
     C*
     C                   Z-ADD     CTCO#         X1CO#
     C                   Z-ADD     CTCO#         X2CO#
     C                   Z-ADD     CTFND         X1FND
     C                   Z-ADD     CTFND         X2FND
     C                   Z-ADD     CTXFYR        X1FYR
     C                   Z-ADD     CTXCAS        X1CASE
     C                   Z-ADD     CTCASE        X2CASE
     C*
     C     DLKY          CHAIN     PDL01                              61
     C     *IN61         DOWEQ     '0'
     C                   MOVE      DLKEY         NEWKEY
     C                   Z-ADD     CTFYR         X2FYR
     C                   MOVE      NEWKEY        DLKEY
     C                   UPDATE    PDL01
     C                   EXCEPT    DET
     C*
     C     DLKY          READE     PDL01                                  61
     C                   END
     C                   END
     C*
     C                   READ      PCT01                                  60
     C                   END
     C*
     C*** END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     O*****************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       DSDATE        Y     60
     O          E            HEAD           1
     O                       Q1PGM               10
     O                                           54 'PAGE:'
     O                       PAGE          Z     60
     O          E            HEAD           2
     O                                           27 'DARTS OPTICAL'
     O                                           46 'RECORDS CORRECTION'
     O          E            DET            1
     O                       OLDKEY              40
     O                       NEWKEY              80
