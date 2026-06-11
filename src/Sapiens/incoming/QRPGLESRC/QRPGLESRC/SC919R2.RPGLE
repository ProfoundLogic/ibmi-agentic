     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE SC919R2  TRANSFER A SINGLE INDEX (TAG ALONG FROM DARTS)
     H*****************************************************************
     H* THIS PROGRAM HANDLES THE CHANGING OF THE INDEX VALUES WHEN    *
     H*   A DARTS TRANSER IS PERFORMED                                *
     H*                                                               *
     H*    CALLS TO:                                                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H***                                                             *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     F*
      * Document Index Control Record
     FIMDOLA    UF A E           K DISK
     FIMDOL1    IF   E           K DISK
     FIMDOLC    UF   E           K DISK
     F                                     RENAME(PDO01:PDO0A)
     F*
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    DOLDT         XXDT
     C*
     C     DTYMD         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDYY'       SRFFMT            8
     C                   PARM      '*YMD'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     DOTKY         KLIST
     C                   KFLD                    QQTKEY
     C                   KFLD                    QQTSQ#
     C     DOKY          KLIST
     C                   KFLD                    QQFKEY
     C                   KFLD                    QQFSQ#
     C*
     C     DOK           KLIST
     C                   KFLD                    QQDCID
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQFKEY           40
     C                   PARM                    QQFSQ#            5 0
     C                   PARM                    QQTKEY           40
     C                   PARM                    QQTSQ#            5 0
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQINUS           10
      *
      * GET TODAY'S DATE
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C*
     C*** IF NOT A COPY DO THE FOLLOWING *****************
     C     QQRTCD        IFNE      'C'
     C*
     C     DOKY          CHAIN     PDO0A                              63
     C     DOKY          CHAIN     PDO011                             60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     QQTKEY        DODKEY
     C                   Z-ADD     QQTSQ#        DODSQ#
     C  N63              UPDATE    PDO0A
     C  N60              UPDATE    PDO011
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C                   ELSE
     C*
     C                   MOVEL     '1'           QQRTCD
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*** THIS IS A COPY NOT A TRANSFER **** WE NEED TO HANDLE THIS????
     C*** WRITE ANOTHER RECORD??? IF SO HOW WILL THIS AFFECT THE REST
     C*** OF THE PROCESSING???
     C*
     C     DOK           CHAIN     PDO01                              60           DCID FOR CHAIN
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     DOTKY         CHAIN     PDO011                             61
     C*
     C     *IN61         IFEQ      *ON
     C*
     C                   MOVEL     QQTKEY        DODKEY
     C                   Z-ADD     QQTSQ#        DODSQ#
     C                   Z-ADD     XXDT          DOLDT
     C                   MOVE      QQINUS        DOLUSR
     C                   MOVE      Q1PGM         DOLPGM
     C                   MOVE      DSTIME        DOLTIM
     C                   WRITE     PDO011
     C                   MOVEL     *BLANKS       QQRTCD
     C                   ENDIF                                                  *in61 end
     C*
     C                   ENDIF                                                  *in60 end
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
