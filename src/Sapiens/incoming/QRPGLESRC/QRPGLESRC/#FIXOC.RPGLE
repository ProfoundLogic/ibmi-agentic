     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     FSMOCL     UF   E           K DISK
      *

     D                 DS
     D  OCDCID                 1     12
     D  ##DLS                  1      3
     D  ##DC#                  4      8  0
     D  ##PRID                 9      9
     D  ##SD#                 10     12  0

     I*

     C     OCKY          KLIST
     C                   KFLD                    QQOSYS            1
     C                   KFLD                    QQLIBN           10
     C*
     C                   MOVE      'G'           QQOSYS
     C                   MOVEL     'OPTMLB01'    QQLIBN
     C*
      *
     C     OCKY          CHAIN     POC01                              62
      *
     C     *IN62         IFEQ      '0'
      *
     C                   ADD       1             OCC#DC
     C                   ADD       1             ##DC#
     C                   UPDATE    POC01
     C                   END
      *
     C                   MOVE      '1'           *INLR
