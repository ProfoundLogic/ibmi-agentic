     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SRMCHK - CHK FOR MULTIPLE DIVISIONS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**      THIS PROGRAM IS CALLED BY VARIOUS BILLING PROGRAMS TO   *
     H**      DETERMINE IF THE MULTIPLE DIVISIONS EXIST FOR AN        *
     H**      EMPLOYER.                                               *
     H**                                                              *
     H**      RETURN CODE:      'Y' MULT DIV EXIST                    *
     H**                                                              *
     H**                        'N' NO MULTIPLE DIV'S.                *
     H**                                                              *
     H**                                                              *
     H* CALLED FROM:                                                  *
     H*   W8001R  - ORIGINAL BILL PROMPT                              *
     H*   W7710R  - AUDITED  BILL PROMPT                              *
     H*   W0090R  - REVISED  BILL PROMPT                              *
     H* CALLS TO:                                                     *
     H*                                                               *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    02/05/98            PROJECT: WC705                *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWDELL1    IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQMDIV            1
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C*
     C*    SET UP AND INITALIZE
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      'N'           QQMDIV
     C*
     C*  GET DETAIL FILE FOR THIS EMPLR, DIV = 0
     C*
     C                   Z-ADD     0             XXDIV
     C     ELKY1         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  SEE IF ANOTHER DIVISION EXISTS
     C*
     C     ELKY1         READE     PEL01                                  60
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVE      'Y'           QQMDIV
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   MOVE      '1'           *INLR
     C*
