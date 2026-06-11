     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
      * PRINT THE PAGE SEGMENTS AS A SINGLE DOCUMENT
     H***************************************************************
     H*  REVISIONS:                                                **
     H*                                                            **
     H*  10/25/00 C:128 PROJECT DR081 #24777                       **
     H*                 Added date and time to the file write in   **
     H*                 order to time stamp the record.            **
     H*****************************************************************
     F*
     FIS204P    O    E             PRINTER OFLIND(*IN01)
     FIMTTP     O    E             DISK
     F*
      *** 10/25/00 Start add *****************************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
      *** 10/25/00 End   add *****************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    #PAGES            5 0
     C                   PARM                    JOBNBR            6
     C                   PARM                    JOBNAM           10
     C                   PARM                    USRNAM           10
     C     'TS'          CAT       JOBNBR        TSLIB            10
     C                   MOVEL     'IMGPRT'      FILNAM            8
     C                   MOVEL     '001'         FILNBR            3
     C*
     C*
     C*
     C                   DO        #PAGES        X                 5 0
     C*
     C                   MOVE      X             $X                5
     C                   MOVE      $X            $X1               1
     C                   MOVE      $X            $X2               2
     C                   MOVE      $X            $X3               3
     C                   MOVE      $X            $X4               4
     C                   MOVE      $X            $X5               5
     C*
     C     X             IFLT      10
     C     'P'           CAT       $X1           DOCID
     C                   ENDIF
     C*
     C*
     C     X             IFLT      100
     C     X             ANDGT     9
     C     'P'           CAT       $X2           DOCID
     C                   ENDIF
     C*
     C*
     C     X             IFLT      1000
     C     X             ANDGT     99
     C     'P'           CAT       $X3           DOCID
     C                   ENDIF
     C*
     C*
     C     X             IFLT      10000
     C     X             ANDGT     999
     C     'P'           CAT       $X4           DOCID
     C                   ENDIF
     C*
     C*
     C     X             IFLT      100000
     C     X             ANDGT     9999
     C     'P'           CAT       $X5           DOCID
     C                   ENDIF
     C*
     C*
     C*
     C*
     C                   WRITE     PAGEx
     C                   WRITE     PAGE1A
     C*
     C                   ENDDO
     C*
     C                   MOVEL     'IS204R'      TTIPGM
      *** 10/25/00 Start add *****************************************
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TTIDT
     C                   MOVEL     DSTIME        TTITIM
      *** 10/25/00 End   add *****************************************
     C*
     C                   WRITE     PTT01
     C*
     C*
     C                   MOVE      '1'           *INLR
