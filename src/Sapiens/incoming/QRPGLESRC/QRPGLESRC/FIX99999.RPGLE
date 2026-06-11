     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      /TITLE FIX99999 -Resequence  SMARP if sequence number is 99999
      *****************************************************************
      * Description:  This program reads all DARTS records in SMARP  *
      *   if the sequence number is 99999 get the previous seq# and   *
      *   add one to it. Change the 99999 seq# to the new number.     *
      *                                                               *
      *****************************************************************
     FSMARL     UF   E           K DISK
      *
      *=====================================================================
      *
      *  Key list for the DARTS Additional Remarks file.
      *

     C     ARKY          KLIST
     C                   KFLD                    SAVKEY
      *
     C     ARKY9         KLIST
     C                   KFLD                    ARKEY
     C                   KFLD                    ##SEQ#
      *
     C**************************************************************************
      *  Begin main loop.
      *
     C                   Z-ADD     99999         ##SEQ#            5 0
      *
     C                   READ      PAR01                                  50
b1   C     *IN50         DOWEQ     '0'
      *
      *  If the sequence number is 99999 process this record
      *
b2   C     ARSEQ#        IFEQ      99999
     C                   MOVE      ARKEY         SAVKEY           40
      ****
     C     ARKY          READPE    PAR01                                  60
b3   C     *IN60         IFEQ      *OFF
      *
b4   C     ARSEQ#        IFGE      *ZERO
     C     ARSEQ#        ADD       1             NEWSEQ            5 0
e4   C                   ENDIF
e3   C                   ENDIF
      *
     C     ARKY9         CHAIN     PAR01                              61
b4   C     *IN61         IFEQ      *OFF
     C                   Z-ADD     NEWSEQ        ARSEQ#
     C                   UPDATE    PAR01
e4   C                   ENDIF
e2   C                   ENDIF
      *
     C                   READ      PAR01                                  50
e1   C                   ENDDO
     C****
     C*
     C                   MOVE      *ON           *INLR
