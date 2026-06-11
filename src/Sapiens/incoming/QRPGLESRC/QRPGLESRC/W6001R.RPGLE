     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE UPDATE DAILY BATCH COMMUNICATION RECORD
     FWZDBP     UF   E             DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    XXTAG            10
     C                   PARM                    XXRTRT            1
     C                   PARM                    XXTDT             6
     C                   PARM                    XXTTM             6
     C                   PARM                    QQSLR             1
     C*
     C*  GET THE RECORD FOR UPDATE
     C*
     C     1             CHAIN     PDB01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      XXTDT         DBTDT
     C*
     C                   MOVE      XXTTM         DBTTM
     C                   MOVE      XXTAG         DBTAG
     C                   MOVE      ' '           DBRTRT
     C*
     C*  UPDATE THE RECORD
     C*
     C                   UPDATE    PDB01
     C*
     C                   END
     C*
     C*  TEST FOR CLOSE OF FILES
     C*
     C     QQSLR         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   ELSE
     C                   RETURN
     C                   END
