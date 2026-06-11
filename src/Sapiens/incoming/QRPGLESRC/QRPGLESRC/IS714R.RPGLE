     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS714R   Get Info for requested SCAN
     H*****************************************************************
     H*                                                               *
     H* * * GET THE WS RCD AND DETERMINE THE SND/RCV QUEUES,  IF THIS *
     H* * * STATION CAN SCAN, ETC.                                    *
     H*                                                               *
     H*    CALLS TO:                                                  *
     H*                                                               *
     H*                                                               *
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
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQJBNM           10
     C                   PARM                    Q1IDRV            1
     C                   PARM                    Q1SSND           10
     C                   PARM                    Q1SSLB           10
     C                   PARM                    Q1SRCV           10
     C                   PARM                    Q1SRLB           10
     C                   PARM                    Q1SCAP            1
     C                   PARM                    Q1WSID            2
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
     C                   MOVEL     WSSSND        Q1SSND
     C                   MOVEL     WSSSLB        Q1SSLB
     C                   MOVEL     WSSRCV        Q1SRCV
     C                   MOVEL     WSSRLB        Q1SRLB
     C                   MOVEL     WSIDRV        Q1IDRV
     C                   MOVEL     WSSCAP        Q1SCAP
     C                   MOVEL     WSWSID        Q1WSID
     C                   ELSE
     C                   MOVEL     *BLANKS       Q1SSND
     C                   MOVEL     *BLANKS       Q1SSLB
     C                   MOVEL     *BLANKS       Q1SRCV
     C                   MOVEL     *BLANKS       Q1SRLB
     C                   MOVEL     *BLANKS       Q1IDRV
     C                   MOVEL     *BLANKS       Q1SCAP
     C                   MOVEL     *BLANKS       Q1WSID
     C                   MOVEL     '1'           QQRTCD
     C*** ERROR - THIS WRKSTN NOT CONFIGURED FOR IMAGING
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C     TERM          TAG
     C                   RETURN
     C     EXIT          TAG
     C                   MOVE      '1'           *INLR
     C*
