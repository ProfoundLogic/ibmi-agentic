     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* DELETE A SINGLE TABLE ENTRY
     H**
     H* CREATED: 10/13/99   MIKE FLOWER
     H**
     H* LAST CHANGED FOR UPDATE TAPE: 10/25/99
     H*****************************************************************
     H*
     FSMCOP     IF   E           K DISK
     FSTGTL     UF   E           K DISK

     D*
     D************        ITID    1  15  4   IDES   40   TBL ID, DESC
     D************        TKEY    1  50 10   TDES   40   KEY, DESC
     D************        TTID    1  50  4               TBL ID
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
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
     C     GIKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C*
     C     GTKEY1        KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKTBID            4
     C                   KFLD                    KKKEY            10
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*** GET COMPANY #
     C                   READ      PCO01                                  90
     C     *IN90         DOWEQ     *OFF
     C                   Z-ADD     COCO#         KKCO#
     C*
     C*** PROCESS FTGTP
     C                   DO        1             X                 2 0
     C     X             IFEQ      1
     C                   MOVEL(P)  'DLTY'        KKTBID
     C                   MOVEL(P)  'E'           KKKEY
     C                   ENDIF
     C*
     C     GTKEY1        CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PGT01
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   READ      PCO01                                  90
     C                   ENDDO                                                  DO *IN90
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
