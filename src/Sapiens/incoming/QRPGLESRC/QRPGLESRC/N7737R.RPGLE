     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N7737R - CASH RECEIPT/ADJ JOURNAL - LOAD A/R CODES
     FFTABL     IF   E           K DISK

     D CD              S              2    DIM(30) ASCEND                       A/R CODES
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR TABLE FILE
     C*
     C     TBKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQCODE           60
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C*
     C                   MOVE      *BLANK        CD
     C                   Z-ADD     1             X                 2 0
     C     TBKY1         CHAIN     PAC01                              60        ADD 12/17/93
     C     *IN60         DOWEQ     '0'
     C     ACSTAT        IFNE      'I'
     C                   MOVEL     ACKEY         XXCODE            2
     C                   MOVE      XXCODE        CD(X)
     C                   ADD       1             X
     C                   END
     C     X             IFEQ      31
     C                   MOVE      '1'           *IN60
     C                   ELSE
     C     TBKY1         READE     PAC01                                  60    ADD 12/17/93
     C                   END
     C                   END
     C*
     C                   MOVEA     CD            QQCODE
     C*
     C                   RETURN
