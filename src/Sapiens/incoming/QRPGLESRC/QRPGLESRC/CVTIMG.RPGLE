     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FIMDOP     UF   E             DISK
     FIMDTL     IF   E           K DISK
     FIMDOP1    O    E             DISK
     F*

     D                 DS
     D  OLKEY                  1     40
     D  OLCO#                  1      3  0
     D  OLFND                  4      6  0
     D  OLEMP#                 7     11  0
     D  OLDIV                 12     14  0
     D  OBLANK                15     40
     D                 DS
     D  NWKEY                  1     40
     D  NWCO#                  1      3  0
     D  NWFND                  4      6  0
     D  NWEMP#                 7     15  0
     D  NWDIV                 16     20  0
     D  NBLANK                21     40

     C*
     C     DTKY          KLIST
     C                   KFLD                    DODTYP
     C*
     C     IEKY          KLIST
     C                   KFLD                    DODCID
     C*
     C     BMKY          KLIST
     C                   KFLD                    DOBCHI
     C*
     C                   MOVE      *ZEROS        NWKEY
     C                   MOVE      *ZEROS        OLKEY
     C                   MOVE      *BLANKS       OBLANK
     C                   MOVE      *BLANKS       NBLANK
     C     442           SETLL     PDO01
     C                   READ      PDO01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C*
     C*** SET FLAGS *****
     C*
     C*** IF THIS IS AN EMPLOYER BASED RECORD.
     C     DOIDFX        IFEQ      'Y'
     C     DTKY          CHAIN     PDT01                              61
     C     *IN61         IFEQ      *OFF
     C     DTIDXP        ANDEQ     'ISII01R '
     C*** CHANGE THE DOKEY ********************************************
     C                   MOVEL     DODKEY        OLKEY
     C                   MOVEL     OLKEY         ##TEST           14
     C                   TESTN                   ##TEST               67
     C     *IN67         IFEQ      *ON
     C*
     C                   MOVE      OLCO#         NWCO#
     C                   MOVE      OLFND         NWFND
     C                   MOVE      OLEMP#        NWEMP#
     C                   MOVE      OLDIV         NWDIV
     C*
     C                   MOVEL     NWKEY         DODKEY
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C***
     C***** IF INDEXED THEN CREATE THE DOP1 RECORD HERE
     C***
     C                   WRITE     PDO011
     C*
     C                   ENDIF
     C*
     C****
     C*
     C                   UPDATE    PDO01
     C*
     C*
     C                   READ      PDO01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
