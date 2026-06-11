     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7413R1 - Create Listing from CPYSPLF DATA member
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PRINTS A LISTING FROM A DATA FILE COPIED   **
     H**     FROM  A SPOOL FILE.  THIS IS DONE TO RESOLVE THE        **
     H**     OUTPUT INTO A DOCUMENT.                                 **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W7413C1- COPY POLICY PAGE TO DOCUMENT (VIA MRGDOC CMD)  **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H**   PROGRAMMER: C:24                                          **
     H**   CREATED:    7/13/94      PROJECT: WC313                   **
     H**                                                             **
     H*****************************************************************
     H** REVISIONS:                                                  **
     H**                                                             **
     H** 11/15/07 C:33  Seq#: WA146/61137                            **
     H**                Dots will be added for all DTM clients.      **
     H**                 CLTID changes will be removed without       **
     H**                 comments.                                   **
     H**                                                             **
     H** 09/24/07 C:33  Project: SY151  Priss #8                     **
     H**                Add dots for CLT099.                         **
     H**                                                             **
     H** 06/26/07 C:33  Add dots for PDSDEV.                         **
     H**                                                             **
     H** 05/21/07 C:33  Project: 102WEE                              **
     H**                Add dots for CLT102.                         **
     H**                                                             **
     H** 05/05/07 C:33  Project: 89IAI                               **
     H**                Add dots for CLT089.                         **
     H**                                                             **
     H** 04/02/07 C:33  Project: 90WEY  PRISS 12                     **
     H**                Add dots for CLT090.                         **
     H**                                                             **
     H** 03/09/07 C:33  Project: 114WMF                              **
     H**                Add dots for CLT114.                         **
     H**                                                             **
     H** 02/01/07 C:33  Seq#: WA129/56711                            **
     H**                Add dots for CLT129.                         **
     H**                                                             **
     H** 11/16/06 C:33  Seq#: WA046/55870                            **
     H**                Add dots for CLT046.                         **
     H**                                                             **
     H** 11/07/06 C:33  Seq#: WA045/54583                            **
     H**                Add dots for CLT045.                         **
     H**                                                             **
     H** 10/21/06 C:33  Project: 33IGD                               **
     H**                Add dots for CLT033.                         **
     H**                                                             **
     H** 03/06/06 C:33  Seq#: WA133/51411                            **
     H**                Add dots for CLT133.                         **
     H**                                                             **
     H** 12/23/05 C:33  Seq#: WA132/50518                            **
     H**                Add dots for CLT132.                         **
     H**                                                             **
     H** 12/09/05 C:65  Seq#: WA136/50243                            **
     H**                ADD DOTS FOR CLIENT 136 AS WELL.             **
     H**                                                             **
     H** 07/01/05 C:33  Seq#: WA126/47885                            **
     H**                Use RTVCLTID to determine if client should   **
     H**                 have dots.  This is due to different clients**
     H**                 with different versions of DTM.             **
     H**                                                             **
     H** 07/15/03 C:33  Project: SY059                               **
     H**                Add '.' for DTM formatting.                  **
     H*****************************************************************
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H*****************************************************************
     FSPLCPY1   IP   E             DISK
     FQSYSPRT1  O    F   85        PRINTER
     F                                     INFDS(FIDS01)

     D FIDS01        E DS                  EXTNAME(SZQ4P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  SPLFLD                 1    202
     D  XXSKP                  1      3
     D  XXSPC                  4      4
     D  DATA                   5     89

     C                   MOVE      XXSKP         SKIPB             3 0
     C                   MOVE      XXSPC         SPACEB            1 0
     C*
     C     SKIPB         CASNE     *ZERO         SKIPSR
     C     SPACEB        CASNE     *ZERO         SPACSR
     C                   END
     C                   EXCEPT    DATAL
      *------------------------------------------------------------------*
     C     SKIPSR        BEGSR
     C*
     C     Q4LIN#        IFLT      SKIPB
     C*
     C     SKIPB         SUB       Q4LIN#        X                 5 0
     C                   DO        X
     C                   EXCEPT    SPACE1
     C                   END
     C*
     C                   ELSE
     C*
     C                   EXCEPT    NEWPAG
     C     SKIPB         SUB       1             X
     C*
     C                   DO        X
     C                   EXCEPT    SPACE1
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
      *------------------------------------------------------------------*
     C     SPACSR        BEGSR
     C*
     C                   DO        SPACEB
     C*
     C                   EXCEPT    SPACE1
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C***  Begin Add  ***  07/15/04  **************************************
     C*====================================================================
     C* Initialize
     C*====================================================================
     C     *INZSR        BEGSR
     C*
     C***  Begin Add  ***  07/01/05  **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C***  End   Add  ***  07/01/05  **************************************
     C*
     C***  Begin Add  ***  06/26/07  **************************************
     C                   CALL      'SACHKPDS'
     C                   PARM                    PDS               1
     C***  End   Add  ***  06/26/07  **************************************
     C*
     C                   CALL      'DTMCHK'
     C                   PARM                    DTM               1
     C*
     C                   CLEAR                   DTMDOT
     C*
     C     DTM           IFEQ      'Y'
     C                   MOVE      '.'           DTMDOT            1
     C                   ENDIF
     C*
     C                   ENDSR
     C***  End   Add  ***  07/15/04  **************************************
     C*
     OQSYSPRT1  E            DATAL          0
     O                       DATA                85
     O*
     O          E            SPACE1      1
     C***  Begin Add  ***  07/15/04  **************************************
     O                       DTMDOT              85
     C***  End   Add  ***  07/15/04  **************************************
     O*
     O          E            NEWPAG           01
     O*
