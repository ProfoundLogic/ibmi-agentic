     H**********************************************************************
     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H*PROGRAM: FA746R
    H*DESCRIPTION: API - CHECK FOR INVALID LIC# PREFIX
     H**********************************************************************
     H*CREATED: 03/20/07 C: 182 PROJECT: FS297
     H**********************************************************************
     H*                                                                    *
    H*  REVISIONS:                                                        *
    H*                                                                    *
    H*   03/11/10 C:227 Project: FL083                                    *
    H*                  Add service date to entry parameters              *
     H*                                                                    *
     H*   03/20/08 C:198                                                   *
    H*                  Project: FS297                                    *
    H*                  MDS to reject selected prefixes for specialties   *
    H*                  not associate with WC - Scope change              *
     H*                                                                    *
     H**********************************************************************
     F*
     FFTLCL1    IF   E           K DISK
     F*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D LIC             DS             5
     D LIC1                           1    Overlay(lic:1)
     D LIC2                           1    Overlay(lic:2)
     D LIC3                           1    Overlay(lic:3)
     D LIC4                           1    Overlay(lic:4)
     D LIC5                           1    Overlay(lic:5)
     D*
     D LICA            S              2
     D LICP            S              3
     D LICQ            S              4
     D LICR            S              5
     D LICW            S              3
     D LICX            S              4
     D LICY            S              5
     D*
HITXXD @@st            s              2
  |  D @@lic#          s             15
HITXXD @@rtcd          s              1
HITXXD @@kklipr        s                   like(lclipr)
     D*** Beg Add ************ 03/20/08 ***********************************
HITXXD @@btyp          s                   like(lcbtyp)
     D*** End Add ************ 03/20/08 ***********************************
     D*** BEG ADD *** 03/11/10 *********************************************
HITXXD @@sdte          s                   like(lcefdt)
     D*** END ADD *** 03/11/10 *********************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
HITXXC     *entry        plist
  |  C                   parm                    @@st
HITXXC                   parm                    @@lic#
     C                   parm                    @@rtcd
     C*** Beg Add ************ 03/20/08 ***********************************
     C                   parm                    @@btyp
     C*** End Add ************ 03/20/08 ***********************************
     C*** BEG ADD *** 03/11/10 *********************************************
     C                   parm                    @@sdte
     C*** END ADD *** 03/11/10 *********************************************
     C*
     C*** Beg Chg ************ 03/20/08 ***********************************
HITXXC*****prlky         klist
  |  C*****              kfld                    @@st
HITXXC*****              kfld                    @@kklipr
     C*
HITXXC     prlky         klist
  |  C                   kfld                    @@st
HITXXC                   kfld                    @@btyp
HITXXC                   kfld                    @@kklipr
     C*** End Chg ************ 03/20/08 ***********************************
     C*
     C*=====================================================================
     C*MAIN PROGRAM
     C*=====================================================================
     C*
     C                   MOVEL     @@LIC#        LIC
     C*
     C*1st & 2nd position are always alpha
     C*
     C     LIC1          CAT       LIC2          LICA
     C                   MOVEL(P)  LICA          @@KKLIPR
     C*
     C* CHECK FOR ALPHA VALUE IN 3RD POSOTION
     C*
     C     LIC3          IFGE      'A'
     C     LIC3          ANDLE     'Z'
     C*
     C     LICA          CAT       LIC3          LICP
     C                   MOVEL(P)  LICP          @@KKLIPR
     C                   ELSE
     C                   GOTO      ENDCHK
     C                   ENDIF
     C*
     C* CHECK FOR ALPHA VALUE IN 4TH POSOTION
     C*
     C     LIC4          IFGE      'A'
     C     LIC4          ANDLE     'Z'
     C
     C     LICP          CAT       LIC4          LICQ
     C                   MOVEL(P)  LICQ          @@KKLIPR
     C                   ELSE
     C                   GOTO      ENDCHK
     C                   ENDIF
     C*
     C* CHECK FOR ALPHA VALUE IN 5TH POSOTION
     C*
     C     LIC5          IFGE      'A'
     C     LIC5          ANDLE     'Z'
     C
     C     LICQ          CAT       LIC5          LICR
     C                   MOVEL(P)  LICR          @@KKLIPR
     C                   ELSE
     C                   GOTO      ENDCHK
     C                   ENDIF
     C*
     C     ENDCHK        TAG
     C*
     C*** Beg Chg ************ 03/20/08 ***********************************
     C*****PRLKY         CHAIN     PLC01                              60
     C     PRLKY         SETLL     PLC01                              60
     C     PRLKY         READE     PLC01                                  60
     C*** End Chg ************ 03/20/08 ***********************************
     C*
     C     *IN60         IFEQ      '0'
     C     LCSTAT        ANDNE     'I'
     C*
     C*** BEG ADD *** 03/11/10 *********************************************
     C                   IF        %PARMS > 4
     C                   IF        @@SDTE >= LCEFDT and
     C                             @@SDTE <= LCCNDT and
     C                             @@SDTE > 0
     C                   MOVE      'N'           @@RTCD
     C                   ELSE
     C                   MOVE      'Y'           @@RTCD
     C                   ENDIF
     C                   ELSE
     C*** END ADD *** 03/11/10 *********************************************
     C                   MOVE      'N'           @@RTCD
     C*** BEG ADD *** 03/11/10 *********************************************
     C                   ENDIF
     C*** END ADD *** 03/11/10 *********************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      'Y'           @@RTCD
     C*
     C                   ENDIF
     C*
     C                   EVAL      *INLR=*ON
