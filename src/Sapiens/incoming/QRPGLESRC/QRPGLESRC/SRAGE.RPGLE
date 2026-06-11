     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H**********************************************************
     H* PROGRAM: SRAGE
     H* DESCRIPTION: CALCULATE AGE
     H**********************************************************
     H*
     H*  REVISIONS
     H*
     H*  09/09/98  C:94  PROJECT: MC098
     H*                  MADE PROGRAM YR2000 COMPLIANT
     H*                  CORRECTED MISCALCULATING AGE
     H**********************************************************

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDTMM                 7      8  0
     D  DSDTDD                 9     10  0
     D  DSDTCY                11     14  0
     D*
     D*** BEG ADD *** 09/09/98 *********************************
     D                 DS
     D  PPDOB                  1      8  0
     D  PPDTMM                 1      2  0
     D  PPDTDD                 3      4  0
     D  PPDTCY                 5      8  0

     I*** END ADD *** 09/09/98 *********************************
     I*
     I*** BEG DEL *** 09/09/98 *********************************
     I*****       DS
     I*****                                   1   80PPDOB
     I*****                                   1   20PPDTCC
     I*****                                   3   40PPDTMM
     I*****                                   5   60PPDTDD
     I*****                                   7   80PPDTYY
     I*** END DEL *** 09/09/98 *********************************

     C*
     C*** BEG ADD *** 09/09/98 *********************************
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*** END ADD *** 09/09/98 *********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDOB             8
     C                   PARM                    QQAGE             3
     C*
     C                   TIME                    DSDTTM
     C*** BEG ADD *** 09/09/98 ************************************
     C*
     C                   MOVE      QQDOB         YYDOB             8 0
     C                   Z-ADD     YYDOB         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         PPDOB
     C*
     C     DSDTCY        SUB       PPDTCY        XXAGE             3 0
     C*** END ADD *** 09/09/98 ************************************
     C*** BEG DEL *** 09/09/98 ************************************
     C*****                MOVE QQDOB     PPDOB
     C*****
     C*****                MOVEL'19'      XXDTCY
     C*****                MOVE PPDTYY    XXDTCY  40
     C*****
     C*****      DSDTCY    SUB  XXDTCY    XXAGE   30
     C*** END DEL *** 09/09/98 ************************************
     C*
     C     XXAGE         IFGT      0
     C*
     C                   SELECT
     C     PPDTMM        WHENGT    DSDTMM
     C                   SUB       1             XXAGE
     C     PPDTMM        WHENEQ    DSDTMM
     C     PPDTDD        IFGT      DSDTDD
     C                   SUB       1             XXAGE
     C                   ENDIF
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C                   MOVE      XXAGE         QQAGE
     C*
     C                   RETURN
     C*
