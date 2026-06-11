     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *TITLE UPDATE RETURN PREMIUM BATCH COMMUNICATION RECORD
      *****************************************************************
      *  CALLED BY: W6530C                                            *
      *                                                               *
      *  04/02/03 C:143 PROJECT: WC1033                               *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      *                                                               *
      *****************************************************************
     FWZD6P     UF   E             DISK
      *
      *

     C     *ENTRY        PLIST
     C                   PARM                    XXTAG            10
     C                   PARM                    XXRTRT            1
     C                   PARM                    QQTDT             6
     C                   PARM                    XXTTM             6
     C                   PARM                    QQSLR             1
      *
      *  GET THE RECORD FOR UPDATE
      *
     C     1             CHAIN     PD601                              60
     C     *IN60         IFEQ      '0'
      *
     C                   MOVE      QQTDT         D6TDT
     C                   MOVE      XXTTM         D6TTM
     C                   MOVE      XXTAG         D6TAG
     C                   MOVE      ' '           D6RTRT
      *
      *  UPDATE THE RECORD
      *
     C                   UPDATE    PD601
      *
     C                   END
      *
      *  TEST FOR CLOSE OF FILES
      *
     C     QQSLR         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   ELSE
     C                   RETURN
     C                   END
