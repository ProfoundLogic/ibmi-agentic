     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FWDELL     IF   E           K DISK
     F                                     INFDS(ELFDB)

     D ELFDB           DS
     D  ELRRN                397    400B 0
     D                 DS
     D  Q1FACT                 1    143
     D  Q1LMPC                 1      5  3
     D  Q1TOT1                 6     16  0
     D  Q1DSCB                17     27  9
     D  Q1TOT2                28     38  0
     D  Q1EXPM                39     43  4
     D  Q1TOT3                44     54  0
     D  Q1DSCA                55     65  9
     D  Q1TOT4                66     76  0
     D  Q1TERR                77     87  0
     D  Q1WAIV                88     98  0
     D  Q1REIN                99    109  0
     D  Q1NETF               110    121  9
     D  Q1NETP               122    132  0
     D  Q1NWAV               133    143  0

     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQRPMY            6
     C                   PARM                    QQTODT            8
     C                   PARM                    QQMAPR           11
     C                   PARM                    QQADLD           11
     C                   PARM                    QQPAYR           11
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQEMP#        XXEMP#            9 0
     C                   MOVE      QQDIV         XXDIV             5 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C                   MOVE      ELRRN         @@RRN
     C                   MOVE      QQRPMY        @@RPMY
     C                   MOVE      QQTODT        @@TODT
     C                   MOVE      QQMAPR        @@MAPR
     C                   MOVE      QQADLD        @@ADLD
     C                   MOVE      QQPAYR        @@PAYR
     C*
     C                   CALL      'SRPREM'
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RPMY            6 0
     C                   PARM                    @@TODT            8 0
     C                   PARM                    @@MAPR           11 0
     C                   PARM                    @@ADLD           11 0
     C                   PARM                    @@PAYR           11 0
     C                   PARM                    @@PREM           11 0
     C                   PARM                    @@TAX            11 0
     C                   PARM                    @@PPRM           11 0
     C                   PARM                    @@PTAX           11 0
     C                   PARM                    @@FACT          143
     C*
     C                   MOVE      @@FACT        Q1FACT
     C*
     C                   MOVE      *ON           *INLR
