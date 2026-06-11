     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE UPDATE GROUP BATCH COMMUNICATION RECORD
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*      THIS PROGRAM UPDATES THE GROUP BATCH COMMUNICATION FILE  *
     H*      DURING THE BATCH PROCESS. IT RECORDS EACH STEP OF THE    *
     H*      BATCH AND IF AN ERROR OCCURS, THE BATCH PROGRAM WILL BE  *
     H*      ABLE TO START WHERE THE ERROR OCCURRED.                  *
     H*                                                               *
     H* 09/08/94  C:15  PROJECT: WC361                                *
     H*                                                               *
     H*****************************************************************
     FWZD3P     UF   E             DISK
     F*

     C******************************
     C*  PROGRAM ENTRY PARAMETERS
     C******************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    XXTAG            10
     C                   PARM                    XXRTRT            1
     C                   PARM                    XXTDT             8
     C                   PARM                    XXTTM             6
     C                   PARM                    QQSLR             1
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*  GET THE RECORD FOR UPDATE
     C*
     C     1             CHAIN     PD301                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      XXTDT         X1TDT             8 0
     C*
     C                   Z-ADD     X1TDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         D3TDT
     C*
     C                   MOVE      XXTTM         D3TTM
     C                   MOVE      XXTAG         D3TAG
     C                   MOVE      ' '           D3RTRT
     C*
     C*  UPDATE THE RECORD
     C*
     C                   UPDATE    PD301
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
