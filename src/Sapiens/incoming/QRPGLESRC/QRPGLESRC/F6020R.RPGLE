     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F6020R - UPDATE FEE SCHEDULE RCDS WITH CHK#'S
     H**************************************************************************
     H* PROGRAM DESCRIPTION:                                                   *
     H* UPDATE CHK# WITH EOB# TO SHOW PROCESSED - DBP                          *
     H*                                                                        *
     H**************************************************************************
     H* REVISIONS:                                                             *
     H*
     H*  03/09/99  C:94  PROJECT: MC152
     H*                  CHANGE FDCAL2 TO FDCALL AND CHANGE KEYLIST
     H*
     H**************************************************************************
     FSMCOL     IF   E           K DISK
     F*** BEG CHG *** 03/09/99 ****************************************
     FFDCALL    UF   E           K DISK
     F**********FDCAL2  UF  E           K        DISK
     F*** END CHG *** 03/09/99 ****************************************
     FFAEOL1    UF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE CASE DETAIL FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C**********           KFLD           EOTRCD           DEL 03/09/99
     C                   KFLD                    EOEOB#
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         XXCO#             3 0
     C*
     C*   CHAIN OUT TO COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*   GET FIRST RECORD AND BEGIN LOOPING
     C*
     C     *LOVAL        SETLL     PEO01
     C                   READ      PEO01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     EOEOB#        EOCHK#
     C*
     C                   MOVE      Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   Z-ADD     DSTIME        EOTIME
     C*
     C                   UPDATE    PEO01
     C*
     C     CAKEY         CHAIN     PCA01                              50
     C     *IN50         DOWEQ     '0'
     C                   Z-ADD     EOEOB#        CACHK#
     C*
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   UPDATE    PCA01
     C*
     C     CAKEY         READE     PCA01                                  50
     C                   END
     C*
     C*   GET NEXT RECORD
     C*
     C                   READ      PEO01                                  60
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C                   MOVE      '1'           *INLR
