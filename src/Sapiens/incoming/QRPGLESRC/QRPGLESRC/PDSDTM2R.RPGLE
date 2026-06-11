     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
*****F** RETRIEVE IP ADDRESS
*****F***** ADD 10/15/07 *****************************
*****F*       ADD HEX CONVERT FOR X'00' VALUES TO BLANKS
    *F*
     FSAIPP     IF   E           K DISK
     F*

     D*
     D*** ADD 10/15/07 **************************************
     D HX              S              1    DIM(15)                              HEX CNVT ARRY
     **** END ADD 10/15/07 **************************************
      *

     C     *ENTRY        PLIST
     C                   PARM                    INUSR            10
     C                   PARM                    OUTIP            15
     C                   PARM                    RTNCD             1
     ***
     C*
     C     *DTAARA       DEFINE    PDSDTMDA      DTMEXT            1
     C*
     C                   IN        *DTAARA                              83      ERROR IF 83
     C*
     C                   MOVE      *BLANKS       RTNCD
     C     *IN83         IFEQ      *OFF
     C     DTMEXT        ANDEQ     'Y'
     C*
     C     INUSR         CHAIN     PIP01                              66
     C     *IN66         IFEQ      *OFF
     C                   MOVEL     IPIPAD        OUTIP
     C                   MOVE      *BLANKS       RTNCD
     C                   ELSE
     C                   MOVE      *BLANKS       OUTIP
     C                   MOVE      '1'           RTNCD
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       OUTIP
     C                   MOVE      '*'           RTNCD
     C                   ENDIF
     **** ADD 10/15/07 **************************************
    ****
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     OUTIP         HX(1)
     C                   EXSR      SRHEX
     C                   MOVEA     HX(1)         OUTIP
    ****
     **** END ADD 10/15/07 **************************************
     C*
     C                   RETURN
     C                   MOVE      *ON           *INLR
     **** ADD 10/15/07 **************************************
      *================================================================
      * SRHEX - Check Alpha input field for Invald Hex characters
      *================================================================
     C     SRHEX         BEGSR
      *
     C                   DO        15            K                 3 0
      *
     C     HX(K)         IFEQ      X'00'
     C                   MOVE      X'40'         HX(K)
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDSR                                                  SROUT
      *================================================================
     **** END ADD 10/15/07 **************************************
