     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDEHP12 - CORRECT INFORCE PREMIUM ON MULTIPLE PEO GROUPS.
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  SET INFORCE PREMIUM DISCOUNT BASED ON WRITTEN DISCOUNT FOR   *
     H*  MULTIPLE STYLE PEO GROUPS WHEN THE INFORCE DISCOUNT IS       *
     H*  LESS THAN THE WRITTEN DISCOUNT AND THE INFORCE PREMIUM IS    *
     H*  EQUAL OR GREATER THAN THE WRITTEN PREMIUM.                   *
     H*                                                               *
     H*  PROGRAMMER:  C:65    SUPPORT# 53349   DATE:  04/26/07        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWDEHL     UF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWMRSL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     EHKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
      *
     C     RSKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    EHRTCD
      *
     C     RRKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXST
     C                   KFLD                    EHPFDT
      *
     C     RRKY1         KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    XXST
      *
     C                   READ      PFN01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     FNPEOF        IFEQ      'M'
      *
     C     EHKY          CHAIN     PEH01                              61
      *
     C     *IN61         DOWEQ     *OFF
      *
     C     EHWDC3        IFNE      *ZERO
     C     EHWDC3        ANDGT     EHIDC3
     C     EHISPR        ANDGE     EHWSPR
     C     EHWSPR        ANDNE     *ZERO
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST              2
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C*
     C     EHWDC3        DIV(H)    EHWSPR        XXFACT            9 8
     C     XXFACT        MULT(H)   EHISPR        XXRND            11 0
     C                   Z-ADD     XXRND         EHIDC3
     C*
     C     EHISPR        SUB       EHIDC3        EHINMP
     C*
     C     RRDV1L        IFEQ      'S'
     C     RRDV1L        OREQ      'D'
     C                   SUB       EHIDC2        EHINMP
     C                   ENDIF
     C*
     C     RRDV2L        IFEQ      'S'
     C     RRDV2L        OREQ      'D'
     C                   SUB       EHIDV2        EHINMP
     C                   ENDIF
     C*
     C     EHWVFL        IFEQ      'S'
     C     EHWVFL        OREQ      'D'
     C                   ADD       EHIWAV        EHINMP
     C                   ENDIF
     C*
     C     RRASFL        IFEQ      'S'
     C     RRASFL        OREQ      'D'
     C                   ADD       EHIADL        EHINMP
     C                   ENDIF
     C*
     C                   ADD       EHIRP1        EHINMP
     C                   ADD       EHIRP2        EHINMP
     C                   ADD       EHIRP3        EHINMP
     C*
     C     EHINMP        ADD       EHITER        EHIAPR
     C                   ADD       EHIDTR        EHIAPR
     C                   ADD       EHPRTF        EHIAPR
     C*
     C                   MOVEL(P)  Q1USER        EHUSER
     C                   MOVEL(P)  Q1PGM         EHPGM
     C*
     C                   UPDATE    PEH01
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  EHWDC3 IFNE
      *
     C     EHKY          READE     PEH01                                  61
     C                   ENDDO
     C*
     C                   ENDIF                                                  FNPEOF IFEQ
      *
     C                   READ      PFN01                                  60
     C                   ENDDO
      *
     C                   MOVE      *ON           *INLR
