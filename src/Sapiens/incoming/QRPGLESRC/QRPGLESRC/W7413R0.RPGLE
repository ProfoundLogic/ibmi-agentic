     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W7413R0 - Create Listing from CPYSPLF DATA member
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
     H** 07/01/05 C:33  Seq#: WA126/47885                            **
     H**                Use RTVCLTID to determine if client should   **
     H**                 have dots.  This is due to different clients**
     H**                 with different versions of DTM.             **
     H**                                                             **
     H** 05/25/05 C:33  Seq#: WA135/47248                            **
     H**                Add processing for pages that print right to **
     H**                 the last line.                              **
     H**                                                             **
     H** 07/15/04 C:33  Project: SY059                               **
     H**                Add '.' for DTM formatting.                  **
     H*****************************************************************
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H*****************************************************************
     FSPLCPY0   IP   E             DISK
     F***  Begin Chg  ***  05/25/05  **************************************
     F**********QSYSPRT O   F     102            PRINTER
     FQSYSPRT   O    F  105        PRINTER OFLIND(*INOF)
     F***  End   Chg  ***  05/25/05  **************************************
     F                                     INFDS(FIDS01)

     D FIDS01        E DS                  EXTNAME(SZQ4P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  SPLFLD                 1    202
     D  XXSKP                  1      3
     D  XXSPC                  4      4
     D  DATA                   5    106

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
     C***  Begin Add  ***  05/25/05  **************************************
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    NEWPAG
     C                   ENDIF
     C***  End   Add  ***  05/25/05  **************************************
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
     C                   CALL      'DTMCHK'
     C                   PARM                    DTM               1
     C*
     C                   CLEAR                   DTMDOT
     C*
     C     DTM           IFEQ      'Y'
     C***  Begin Add  ***  07/01/05  **************************************
     C     CLTID         IFEQ      '126'
     C***  End   Add  ***  07/01/05  **************************************
     C                   MOVE      '.'           DTMDOT            1
     C***  Begin Add  ***  07/01/05  **************************************
     C                   ENDIF
     C***  End   Add  ***  07/01/05  **************************************
     C                   ENDIF
     C*
     C                   ENDSR
     C***  End   Add  ***  07/15/04  **************************************
     C*
     OQSYSPRT   E            DATAL          0
     O                       DATA               102
     O*
     O          E            SPACE1      1
     C***  Begin Add  ***  07/15/04  **************************************
     O                       DTMDOT             105
     C***  End   Add  ***  07/15/04  **************************************
     O*
     O          E            NEWPAG           01
     O*
