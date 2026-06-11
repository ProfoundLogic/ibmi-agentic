     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE IS900R   TRANSFER A SINGLE INDEX (TAG ALONG FROM DARTS)
     H*****************************************************************
     H* THIS PROGRAM HANDLES THE CHANGING OF THE INDEX VALUES WHEN    *
     H*   A DARTS TRANSER IS PERFORMED                                *
     H*                                                               *
     H*    CALLS TO:                                                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H***  9/12/02 - 02                                               *
     H**** MUST CONSIDER BOTH IMDOP AND IMDOP1 FOR UPDATES
     H**** CHAIN/UPDATE BOTH OR WHICH EVER IS FOUND...
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
     F*DEL 9/12/02 ***********IMDOLA  UF  E           K        DISK
     F*=== ADD 09/12/02 ======================
     FIMDOLA    UF   E           K DISK
     FIMDOLC    UF   E           K DISK
     **=== END ADD 09/12/02 ===================
     F*
     F**** DEL 9/12/02** PDO011                            KRENAMEPDO01
     F*

     C     DOKY          KLIST
     C                   KFLD                    QQFKEY
     C                   KFLD                    QQFSQ#
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
     C*
     **== ADD 09/12/02 ===================================
     C*
     C     DOKY          CHAIN     PDO01                              60
     C     DOKY          CHAIN     PDO011                             61
     C*
     **=== END ADD 09/12/02 ==============================
     **DEL NXT LINE 09/12/02 ==============================
     C********** DOKY      CHAINPDO01                60
     C*
     C     *IN60         IFEQ      *OFF
     **=== ADD NXT LINE 09/12/02 ========================
     C     *IN61         OREQ      *OFF
     C*
     C                   MOVEL     QQTKEY        DODKEY
     C                   Z-ADD     QQTSQ#        DODSQ#
     C*******DEL**9/12/02* UPDATPDO01
     **=== ADD 9/12/02 === BEGIN =============================
     C  N60              UPDATE    PDO01
     C  N61              UPDATE    PDO011
     **=== END ADD 9/12/02 ===========================
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C                   ELSE
     C*
     C                   MOVEL     '1'           QQRTCD
     C*
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
