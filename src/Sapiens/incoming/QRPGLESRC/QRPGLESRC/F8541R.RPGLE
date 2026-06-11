     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*                                                               *
     H*  F8541R - Update Medical Bills with CS Check Info             *
     H*****************************************************************
     H*                                                               *
     H*  Called from:                                                 *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 01/02/2007 by c:62 for project 144MAF             *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     F*
     F* Transfer files
     F*
     FFZCIP     UF   E             DISK
     FFMCML14   IF   E           K DISK
     FFAEAL1    UF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D***PY SCOPYWW01I
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  CITRID                 1     12
     D  EOB#                   1      9
     D  RCN                   10     12
     D*****************************************************************
     D* Program constants.
     D*****************************************************************
     D COMAPI          C                   CONST('FA007R')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    CISRC
     C*
     C     EAKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EAEOB#        KKEOB#
     C     *LIKE         DEFINE    EARCN         KKRCN
     C*****************************************************************
     C*
     C* EXECUTE MAIN SUBROUTINE
     C*
     C                   CALL      COMAPI        COMPL                  99
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C                   READ      PCI01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     CISRC         IFNE      *BLANKS
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      EOB#          KKEOB#
     C                   MOVE      RCN           KKRCN
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      CIWCPD        EAWCPD
     C                   MOVE      CIWCPC        EAWCPC
     C                   Z-ADD     XXDATE        EADT
     C                   Z-ADD     DSTIME        EATIME
     C                   MOVEL(P)  Q1PGM         EAPGM
     C                   UPDATE    PEA01
     C                   MOVEL(P)  'Y'           CIPRFL
     C                   Z-ADD     XXDATE        CIPRDT
     C                   Z-ADD     DSTIME        CIPRTM
     C                   ELSE
     C                   MOVEL(P)  'BE'          CIPRFL
     C                   Z-ADD     XXDATE        CIPRDT
     C                   Z-ADD     DSTIME        CIPRTM
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL(P)  'CE'          CIPRFL
     C                   Z-ADD     XXDATE        CIPRDT
     C                   Z-ADD     DSTIME        CIPRTM
     C                   ENDIF
     C                   ELSE
     C                   MOVEL(P)  'CE'          CIPRFL
     C                   Z-ADD     XXDATE        CIPRDT
     C                   Z-ADD     DSTIME        CIPRTM
     C                   ENDIF
     C*
     C                   UPDATE    PCI01
     C                   READ      PCI01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
