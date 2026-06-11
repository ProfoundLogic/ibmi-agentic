     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - FIND OUT IF EMPLOYER IS CANCELLED, AND IF IT IS THEN GET  WCMC
     H* CANCELLATION DATE.  IF NOT FOUND PASS BACK AN F AS A RTN CD.
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: WA111R                                            *
     H*    CREATED: 10/08/98  C:94                                    *
     H*    DESCRIPTION: GET CANCELLED EMPLOYER CANCELATION DATE       *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       60      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     FWMEML     IF   E           K DISK

     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*** EFFECTIVE DATE
     D*
     D                 DS
     D  EFDT                   1     35  0
     D                                     DIM(5)                               EFFECTIVE DT
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*
     D                 DS
     D  CADT                   1     35  0
     D                                     DIM(5)                               CANCELED DT
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0

     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*  MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     *ENTRY        PLIST
      * INPUT FROM CALLER
     C                   PARM                    KKCO#                           I
     C                   PARM                    KKFND                           I
     C                   PARM                    KKEMP#                          I
     C                   PARM                    KKDIV                           I
      * RETURN TO CALLER
     C                   PARM                    XYCDT                           O
     C                   PARM                    XXRTCD            1             I/O
      *
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C     *LIKE         DEFINE    EMCAD1        XXCDT
      *
      * MAINLINE
     C*
     C     EMKY          CHAIN     PEM01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     5             I                 1 0
     C*
     C     EFDT(I)       DOWEQ     *ZEROS
     C     I             ANDGT     1
     C                   SUB       1             I
     C                   ENDDO
     C*
     C                   MOVE      *BLANKS       XXCDT
     C*
     C     EFDT(I)       IFLT      CADT(I)
     C                   MOVE      'Y'           XXRTCD
     C                   Z-ADD     CADT(I)       XXCDT
     C                   ENDIF
     C*
     C                   Z-ADD     XXCDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XYCDT             8
     C*
     C                   ENDIF
     C*
      * PROGRAM TERMINATION
     C                   MOVE      '1'           *INLR
      /EJECT
