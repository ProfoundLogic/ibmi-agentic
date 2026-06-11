     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*  PROGRAM SUBROUTINE SRNAME - BUILD A COMBINED NAME.
     H*
     H*  THIS SUB-PROGRAM WILL ACCEPT THE FIRST AND LAST NAME
     H*  FIELDS AND BUILD A COMBINED NAME IN THE LAST NAME FIELD
     H*  LAST,FIRST
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/26/93 C:09 PROJECT WC251                                   *
     H*               CLEAR OUT DATA STRUCTURED FIELDS BEFORE USING   *
     H*               SINCE THE RETRN STATEMENT IS BEING USED AND     *
     H*               OLD DATA MAY STILL EXIST IN THESE FIELDS.       *
     H*                                                               *
     H*****************************************************************

     D                 DS
     D  SCNAM                  1     56
     D  SC                     1     56
     D                                     DIM(56)
     D                 DS
     D  SLNAM                  1     56
     D  SL                     1     56
     D                                     DIM(56)
     D                 DS
     D  SFNAM                  1     15
     D  SF                     1     15
     D                                     DIM(15)

     C     *ENTRY        PLIST
     C                   PARM                    SRLNAM           56
     C                   PARM                    SRFNAM           15
     C                   PARM                    SRTERM            1
     C*
     C*** BEGIN ADD ***** 07/26/93 ***************************************
     C                   MOVE      *BLANKS       SCNAM
     C                   MOVE      *BLANKS       SLNAM
     C                   MOVE      *BLANKS       SFNAM
     C*** END ADD ******* 07/26/93 ***************************************
     C                   MOVEL     SRFNAM        SCNAM
     C                   MOVE      SRLNAM        SLNAM
     C*
     C*** FIND THE LAST NON-BLANK FIELD OF THE 1ST NAME.
     C*
     C                   SETOFF                                       60
     C                   Z-ADD     15            SX                5 0
     C                   DO        15
     C     SC(SX)        COMP      ' '                                6060
     C  N60              SUB       1             SX
     C  N60              END
     C*
     C*
     C*
     C     SX            IFNE      0
     C     SLNAM         ANDNE     *BLANK
     C*
     C                   ADD       2             SX
     C                   MOVEA     SLNAM         SC(SX)                         SAJ 5-12-88
     C*
     C                   END
     C     SX            IFEQ      0                                            SAJ 5-12-88
     C                   Z-ADD     1             SX                             |
     C                   MOVEA     SLNAM         SC(SX)                         |
     C                   END                                                    |
     C*
     C                   MOVE      SFNAM         SRFNAM
     C                   MOVE      SCNAM         SRLNAM
     C*
     C     SRTERM        IFEQ      '1'
     C                   RETURN
     C                   ELSE
     C                   MOVE      '1'           *INLR
     C                   END
