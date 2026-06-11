     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FIMDOP     UF   E             DISK
     FSMDLL     IF   E           K DISK
     F*

     C*
     C     DLKY          KLIST
     C                   KFLD                    DODKEY
     C                   KFLD                    DODSQ#
     C*
     C     1             SETLL     PDO01
     C                   READ      PDO01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C*** GET THE DL RECORD TO UPDATE THE VOL1 FIELD
     C*
     C*
     C*
     C*
     C     DODKEY        IFNE      *BLANKS
     C     DLKY          CHAIN     PDL01                              64
     C     *IN64         IFEQ      *OFF
     C                   MOVEL     DLVOL1        DOVOL1
     C                   MOVEL     DLDCID        DOODCI
     C                   MOVEL     DLOSYS        DOOSYS
     C                   MOVEL     DLOFLD        DOOFLD
     C                   MOVEL     DLOSBD        DOOSFL
     C*
     C                   UPDATE    PDO01
     C                   ENDIF
     C                   ENDIF
     C*
     C                   READ      PDO01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
