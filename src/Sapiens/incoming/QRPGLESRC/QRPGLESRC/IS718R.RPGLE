     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS718R   Get Image Info for requested document
     H*****************************************************************
     H*                                                               *
     H*    CALLS TO:                                                 **
     H*                                                              **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H* 61  GENERAL CHAIN INDICATOR                                   *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     F*
      * Work station ID file
     FIMWSL2    IF   E           K DISK
     F*

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*** WORKSTATION FILE
     C*
     C     WSKY1         KLIST
     C                   KFLD                    QQJBNM
     C*
     C*
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQJBNM           10
     C                   PARM                    Q1IDRV            1
     C                   PARM                    Q1VQUE           10
     C                   PARM                    Q1VQLB           10
     C                   PARM                    QQRTCD            1
     C*
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C*
      *================================================================
     C*
     C*
     C     WSKY1         CHAIN     PWS01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     WSVQUE        Q1VQUE
     C                   MOVEL     WSVQLB        Q1VQLB
     C                   MOVEL     WSIDRV        Q1IDRV            1
     C                   ELSE
     C                   MOVEL     *BLANKS       Q1VQUE
     C                   MOVEL     *BLANKS       Q1VQLB
     C                   MOVEL     *BLANKS       Q1IDRV            1
     C                   MOVEL     '1'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C*
     C     TERM          TAG
     C                   RETURN
     C     EXIT          TAG
     C                   MOVE      '1'           *INLR
     C*
