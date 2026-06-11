     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - WHEN PASSED NATURE, PART, AND CAUSE WILL PASS BACK THE    WCMC
     H* APPOPRIATE DESCRIPTIONS
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: WA108R                                            *
     H*    CREATED: 09/28/98  C:94                                    *
     H*    DESCRIPTION: GET DESCRIPTION OF NATURE, PART, AND CAUSE    *
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
     FWTABL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  NATURE CODE TABLE FILE
     C*
     C     TBLKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXKEY            10
     C*
     C     *ENTRY        PLIST
      * INPUT FROM CALLER
     C                   PARM                    QQCO#             3 0           I
     C                   PARM                    QQNA              2             I
     C                   PARM                    QQPA              2             I
     C                   PARM                    QQCA              2             I
      * RETURN TO CALLER
     C                   PARM                    QQNADS                          O
     C                   PARM                    QQPADS                          O
     C                   PARM                    QQCADS                          O
     C                   PARM                    QQRTCD            1             I/O
      *
     C     *LIKE         DEFINE    NADESC        QQNADS
     C     *LIKE         DEFINE    PADESC        QQPADS
     C     *LIKE         DEFINE    CADESC        QQCADS
      * PROGRAM INITIALIZATION
     C                   EXSR      INIT
      * MAINLINE
     C                   DO
     C     QQNA          IFNE      *BLANK
     C                   EXSR      NATURE
     C                   ENDIF
     C     QQPA          IFNE      *BLANK
     C                   EXSR      PART
     C                   ENDIF
     C     QQCA          IFNE      *BLANK
     C                   EXSR      CAUSE
     C                   ENDIF
     C                   ENDDO
      * PROGRAM TERMINATION
     C                   MOVE      '1'           *INLR
      /EJECT
     C     NATURE        BEGSR
     C                   MOVEL(P)  QQNA          XXKEY
     C     TBLKY         CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      NADESC        QQNADS
     C                   ELSE
     C                   CLEAR                   QQNADS
     C                   MOVE      'X'           QQRTCD                         X=NO HIT
     C                   ENDIF                                                  PNA01=*IN60=0
      *
     C                   ENDSR
      /EJECT
     C     PART          BEGSR
     C                   MOVEL(P)  QQPA          XXKEY
     C     TBLKY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PADESC        QQPADS
     C                   ELSE
     C                   CLEAR                   QQPADS
     C                   MOVE      'X'           QQRTCD                         X=NO HIT
     C                   ENDIF                                                  PNA01=*IN60=0
     C                   ENDSR
      /EJECT
     C     CAUSE         BEGSR
     C                   MOVEL(P)  QQCA          XXKEY
     C     TBLKY         CHAIN     PCA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CADESC        QQCADS
     C                   ELSE
     C                   CLEAR                   QQCADS
     C                   MOVE      'X'           QQRTCD                         X=NO HIT
     C                   ENDIF                                                  PNA01=*IN60=0
     C                   ENDSR
      /EJECT
     C     INIT          BEGSR
     C                   CLEAR                   QQNADS
     C                   CLEAR                   QQPADS
     C                   CLEAR                   QQCADS
     C                   CLEAR                   PNA01
     C                   CLEAR                   PPA01
     C                   CLEAR                   PCA01
     C                   ENDSR
