     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*** THIS PROGRAM WILL UPDATE ANY EXISTING IMAGE SYSTEM RCDS
     H*** WITH OPTICAL INFO WHEN A IMG IS WRITTEN TO OPTICAL !!!
     FIMDOL     IF   E           K DISK
     FIMDOL1    UF   E           K DISK
     F                                     RENAME(PDO01:PDO02)

     C*
     C     DOKY          KLIST
     C                   KFLD                    QQKEY
     C                   KFLD                    XXSEQ#
     C*
     C     DOKY2         KLIST
     C                   KFLD                    DODCID
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12
     C                   PARM                    QQOSYS            1
     C                   PARM                    QQOFLD            8
     C                   PARM                    QQOSBD           12
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ#            5 0
     C                   PARM                    QQSYS             1
     C                   PARM                    QQLIBN           10
     C                   PARM                    QQVOL1           10
     C*
     C                   MOVE      QQSEQ#        XXSEQ#            5 0
     C*
     C     DOKY          CHAIN     PDO01                              60
     C  N60DOKY2         CHAIN     PDO02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     QQOSYS        DOOSYS
     C                   MOVEL     QQOFLD        DOOFLD
     C                   MOVEL     QQOSBD        DOOSFL
     C                   MOVEL     QQDCID        DOODCI
     C                   MOVEL     QQLIBN        DOLIBN
     C                   MOVEL     QQVOL1        DOVOL1
     C                   MOVEL     'Y'           DOOPTF
     C                   UPDATE    PDO02
     C                   ENDIF
     C*
     C                   MOVEL     *ON           *INLR
     C*
