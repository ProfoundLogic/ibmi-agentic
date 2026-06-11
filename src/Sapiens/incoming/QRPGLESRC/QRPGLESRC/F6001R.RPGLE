     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDATE DAILY BATCH COMMUNICATION RECORD
     FSZDBP     UF   E             DISK

     C     *ENTRY        PLIST
     C                   PARM                    XXTAG            10
     C                   PARM                    XXRTRT            1
     C*** BEGIN CHG *** 01/25/99 **************************************
     C**********           PARM           XXTDT   6
     C                   PARM                    XXTDT             8
     C*** END   CHG *** 01/25/99 **************************************
     C                   PARM                    XXTTM             6
     C                   PARM                    QQSLR             1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  GET THE RECORD FOR UPDATE
     C*
     C     1             CHAIN     PDB01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     QQSLR         IFEQ      *BLANKS
     C     QQSLR         OREQ      '1'
     C                   MOVE      XXTDT         DBTDT
     C*
     C                   MOVE      XXTTM         DBTTM
     C                   MOVE      XXTAG         DBTAG
     C                   MOVE      ' '           DBRTRT
     C                   END
     C*
     C* IF THE DAILY BATCH HAS COMPLETED(I.E. QQSLR = 1) THEN UPDATE
     C* DBFL11(DAILY BATCH ACTIVE FLAG) TO BE BLANKS
     C*
     C     QQSLR         IFEQ      '1'
     C                   MOVE      'C'           DBFL11
     C                   END
     C*
     C* IF THE DAILY BATCH HAS STARTED UPDATE THE PROCESSING FLAG TO
     C* A 'P'
     C*
     C     QQSLR         IFEQ      '2'
     C                   MOVE      'P'           DBFL11
     C                   END
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
