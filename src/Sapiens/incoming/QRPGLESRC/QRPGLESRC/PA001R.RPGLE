     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@FND
     C                   PARM                    @@CONM
     C                   PARM                    @@FNAM
     C*
     C     COKEY         KLIST                                                  COMPANY
     C                   KFLD                    UPCO#
     C*
     C     UPKEY         KLIST                                                  USER PROFILE
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    UPCO#         @@CO#
     C     *LIKE         DEFINE    UPCO#         @@FND
     C     *LIKE         DEFINE    CONAME        @@CONM
     C     *LIKE         DEFINE    CONAME        @@FNAM
     C                   MOVE      *BLANKS       @@CONM
     C                   MOVE      *BLANKS       @@FNAM
     C*
     C     UPKEY         CHAIN     PUP01                              99
B01  C     *IN99         IFEQ      *OFF
     C     COKEY         CHAIN     PCO01                              99
B02  C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        @@CONM
E02  C                   ENDIF
E01  C                   ENDIF
     C*
     C                   MOVE      UPCO#         @@CO#
     C                   MOVE      *ZEROS        @@FND
     C                   MOVEL     *BLANKS       @@FNAM
     C*
     C                   RETURN
