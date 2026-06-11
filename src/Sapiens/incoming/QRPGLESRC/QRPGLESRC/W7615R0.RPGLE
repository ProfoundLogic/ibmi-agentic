     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W7615R0 - Create Listing from CPYSPLF DATA member
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PRINTS A LISTING FROM A DATA FILE COPIED   **
     H**     FROM  A SPOOL FILE.  THIS IS DONE TO RESOLVE THE        **
     H**     OUTPUT INTO A DOCUMENT.                                 **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W7615C- COPY BILL TO DOCUMENT (VIA MRGDOC CMD)          **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H**   PROGRAMMER: C:65                                          **
     H**   CREATED:   11/21/96      PROJECT: CM023                   **
     H**                                                             **
     H*****************************************************************
     H** REVISIONS:                                                  **
     H**                                                             **
     H** 10/20/04  C:33  Project: SY059                              **
     H**                 Correct alignment in resolved document.     **
     H*****************************************************************
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H*****************************************************************
     FSPLCPY0   IP   E             DISK
     FW7615P    O    F  102        PRINTER
     F                                     INFDS(FIDS01)

     D FIDS01        E DS                  EXTNAME(SZQ4P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  SPLFLD                 1    202
     D  XXSKP                  1      3
     D  XXSPC                  4      4
     D  DATA                   5    106

     C*
     C                   MOVE      XXSKP         SKIPB             3 0
     C                   MOVE      XXSPC         SPACEB            1 0
     C     *IN99         IFEQ      *OFF
     C                   MOVE      *ZERO         Q4LIN#
     C                   MOVE      *ON           *IN99
     C***  Begin Add  ***  10/20/04  **************************************
     C     DTM           IFEQ      'Y'
     C     SKIPB         ANDGT     0
     C                   SUB       1             SKIPB
     C                   ENDIF
     C***  End   Add  ***  10/20/04  **************************************
     C                   END
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
     C***
     C     60            SUB       Q4LIN#        X                 5 0
     C                   DO        X
     C                   EXCEPT    SPACE1
     C                   END
     C*
     C*
     C                   EXCEPT    NEWPAG
     C*
     C***  Begin Add  ***  10/20/04  **************************************
     C     DTM           IFNE      'Y'
     C***  End   Add  ***  10/20/04  **************************************
     C     SKIPB         SUB       1             X
     C***  Begin Add  ***  10/20/04  **************************************
     C                   ENDIF
     C***  End   Add  ***  10/20/04  **************************************
     C*
     C                   DO        X
     C                   EXCEPT    SPACE1
     C                   END
     C*
     C                   ENDIF
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
     C***  Begin Add  ***  10/20/04  **************************************
     C*====================================================================
     C* Initialization
     C*====================================================================
     C     *INZSR        BEGSR
     C*
     C                   CALL      'DTMCHK'
     C                   PARM                    DTM               1
     C*
     C                   ENDSR
     C***  End   Add  ***  10/20/04  **************************************
     C*
     OW7615P    E            DATAL          0
     O                       DATA               102
     O*
     O          E            SPACE1      1
     O                                          102 '.'
     O*
     O          E            NEWPAG           01
     O*
