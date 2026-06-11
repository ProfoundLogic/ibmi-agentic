     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W3040R - COMMISSION CHECK PRINTING DRIVER
     H*****************************************************************
     H*  CALLED FROM:                                                 *
     H*      W3030C                                                   *
     H*  CALLS TO:                                                    *
     H*      W3050C                                                   *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 09/07/05 C:53  Project: 129WLV/WC1340
     H*                ADD FLAG FOR USING THE CURRENT DATE
     H*
     H*  12/01/97 C:30 Project: WC500                                 *
     H*           Access Bank Info to determine if Group "Print Hard- *
     H*           Copy" flag is 'Y'.  If not, skip Check Processing   *
     H*           for Group.                                          *
     H*           Change DATE PARM from 6 to 8 chars.                 *
     H*                                                               *
     H*  05/29/96 C:33 PROJECT WC420                                  *
     H*                IF THIS REPORT IS CALLED ON DEMAND IT SHOULD   *
     H*                BE ROUTED TO THE USER PARM OUTQ INSTEAD OF THE *
     H*                OVERRIDE OUTQ.                                 *
     H*****************************************************************
     FWAC2L     UF   E           K DISK
     FSMCOL     IF   E           K DISK
      *** BEGIN ADD *** 12/01/97 **************************************
      *___________________________________________________________________
      *  Bank Information file
     FWMBIL1    IF   E           K DISK
      *___________________________________________________________________
      *** END   ADD *** 12/01/97 **************************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    C2DT          XXDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
      *** BEGIN CHG *** 12/01/97 ***********************************************
     C*********************PARM***********QQDATE  6
     C                   PARM                    QQDATE            8
      *** BEGIN CHG *** 12/01/97 ***********************************************
     C                   PARM                    QQFND             3
     C                   PARM                    QQQUE            10
      *** BEG ADD *** 09/07/05 **************************************  *********
     C                   PARM                    QQCURD            1
      *** END ADD *** 09/07/05 **************************************  *********
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
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE CHECK PROCESSING FILE
     C*
     C     C2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    XXFND
     C*
      *** BEGIN ADD *** 12/01/97 **************************************
      *
      *   Bank Information file
      *
     C     BIKY          KLIST
     C                   KFLD                    C2CO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    XXFND
     C                   KFLD                    C2ACCT
     C                   KFLD                    XXEFDT
      *
     C     BIKY1         KLIST
     C                   KFLD                    C2CO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    XXFND
     C                   KFLD                    C2ACCT
      *
     C     *LIKE         DEFINE    BIFND         XXFND
     C     *LIKE         DEFINE    BIAPPL        XXAPPL
     C     *LIKE         DEFINE    BIEFDT        XXEFDT
      *
      *** END   ADD *** 12/01/97 **************************************
      *
     C                   MOVE      QQCO#         XXCO#             3 0
      *** BEGIN CHG *** 12/01/97 ***********************************************
     C*********************MOVE QQDATE    XXDATE  60
      *
     C                   MOVE      QQDATE        XXDATE            8 0
      *** END   CHG *** 12/01/97 ***********************************************
      *
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   MOVE      QQFND         XXFND
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*   START READING THE CHECK PROCESSING FILE
     C*
     C     C2KEY         CHAIN     PC201                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   MAKE SURE PROCESSING FLAGS AREN'T FLIPPED
     C*
     C     C2STA0        IFEQ      ' '
     C     C2STA1        ANDEQ     ' '
      *
      *** BEGIN ADD *** 12/01/97 ***********************************************
      *
      *  For the Fund Encountered, Access Bank Information record and determine
      *  if Check Processing CLP should be called.
      *
     C                   Z-ADD     C2FND         XXFND
     C                   MOVE      'AG'          XXAPPL
     C                   Z-ADD     XXDT          XXEFDT
      *
     C     BIKY          SETLL     PBI01
     C     BIKY1         READE     PBI01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     BISTAT        ANDEQ     'I'
     C     BIKY1         READE     PBI01                                  60
     C                   ENDDO
      *
     C     *IN60         IFEQ      *OFF
     C     BIHDCY        IFEQ      'Y'
     C***                  MOVELBIFTYP    QQFTYP
     C***                  MOVELBIOUTQ    QQOUTQ
     C***                  MOVELBICPYQ    QQCPYQ
      *
      *** END   ADD *** 12/01/97 ***********************************************
     C*
     C*   CALL THE CL TO APPLY THE OVERRIDE
     C*
     C                   CALL      'W3050C'
     C                   PARM                    QQDATE
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQQUE
      *** BEG ADD *** 09/07/05 **************************************  *********
     C                   PARM                    QQCURD
      *** END ADD *** 09/07/05 **************************************  *********
     C*
     C                   MOVE      'Y'           C2STA0
     C                   UPDATE    PC201
     C                   END
      *
      *** BEGIN ADD *** 12/01/97 ***********************************************
     C                   ENDIF                                                  BIHDCY
     C                   ENDIF                                                  *IN60
      *** END   ADD *** 12/01/97 ***********************************************
     C*
     C     C2KEY         READE     PC201                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
