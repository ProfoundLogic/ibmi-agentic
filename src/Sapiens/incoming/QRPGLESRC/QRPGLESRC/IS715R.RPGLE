     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS715R   Get DOCUMENT TYPE INFORMATION
     H*****************************************************************
     H*                                                               *
     H* * * GET THE DT RCD AND RETURN THE INFO TO CALLING PROGRAM     *
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
      * Document Type File  - DOC TYPE KEY
     FIMDTL     IF   E           K DISK
     F*
     F*
      * Document Type File  - DARTS RMCD KEY
     FIMDTL2    IF   E           K DISK
     F                                     RENAME(PDT01:PDT02)
     F*

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*** DOCUMENT TYPE FILE
     C*
     C     DTKY1         KLIST
     C                   KFLD                    QQDOCT
     C*
     C*
     C*
     C*** DOCUMENT TYPE FILE
     C*
     C     DTKY2         KLIST
     C                   KFLD                    QQRMCD
     C*
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDOCT            8              IMG DOC TYPE
     C                   PARM                    QQRMCD            5              DARTS RMK CD
     C                   PARM                    QQDLIM            3              DELIMITER *DEC
     C                   PARM                    QQDESC           40              DESCRIPTION
     C                   PARM                    QQDQUE           10              DFT IDX QUEUE
     C                   PARM                    QQ#DIB            5              # DOCS IN BTCH
     C                   PARM                    QQSECV            3              SECURITY VALUE
     C                   PARM                    QQIDXP           10              INDX PROGRAM
     C                   PARM                    QQBSTS            2              NXT BATCH STS
     C                   PARM                    QQRTCD            1
     C*
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C*
      *================================================================
     C*
      * * * USE EITHER THE DOC TYPE OR THE DARTS RMK CODE TO GET THE
      * * * DOCUMENT TYPE RECORD.
     C*
     C     QQDOCT        IFNE      *BLANKS
     C     DTKY1         CHAIN     PDT01                              60
     C                   ELSE
     C     DTKY2         CHAIN     PDT02                              60
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DTDOCT        QQDOCT            8              IMG DOC TYPE
     C                   MOVEL     DTRMCD        QQRMCD            5              DARTS RMK CD
     C                   MOVE      DTDLIM        QQDLIM            3              DELIMITER *DEC
     C                   MOVEL     DTDESC        QQDESC           40              DESCRIPTION
     C                   MOVEL     DTDQUE        QQDQUE           10              DFT IDX QUEUE
     C                   MOVE      DT#DIB        QQ#DIB            5              # DOCS IN BTCH
     C                   MOVEL     DTSECV        QQSECV            3              SECURITY VALUE
     C                   MOVEL     DTIDXP        QQIDXP           10              INDX PROGRAM
     C                   MOVEL     DTBSTS        QQBSTS            2              NEXT BCH STS
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       QQDOCT            8              IMG DOC TYPE
     C                   MOVE      *BLANKS       QQRMCD            5              DARTS RMK CD
     C                   MOVE      *ZEROS        QQDLIM            3              DELIMITER *DEC
     C                   MOVEL     *BLANKS       QQDESC           40              DESCRIPTION
     C                   MOVEL     *BLANKS       QQDQUE           10              DFT IDX QUEUE
     C                   MOVE      *ZEROS        QQ#DIB            5              # DOCS IN BTCH
     C                   MOVEL     *BLANKS       QQSECV            3              SECURITY VALUE
     C                   MOVEL     *BLANKS       QQIDXP           10              INDX PROGRAM
     C                   MOVEL     *BLANKS       QQBSTS            2              NEXT BCH STS
     C*
     C                   MOVE      '1'           QQRTCD
      * * * ERROR - DOCUMENT TYPE RCD NOT FOUND FOR INPUT KEY
     C                   ENDIF
      *
     C*
     C     TERM          TAG
     C                   RETURN
     C     EXIT          TAG
     C                   MOVE      '1'           *INLR
     C*
