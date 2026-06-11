     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* TITLE F8362R01 - TWCC ADL INFORMATION
     H*
     H*****************************************************************
     H* CREATED ON 09/19/03 BY C:62 FOR PROJECT TX021                 *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FF8362F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FFZBCP     IF   E             DISK
     FFTGTL     IF   E           K DISK

     D*
     D COMAPI          C                   CONST('FA007R')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@LIC#           12
     C                   PARM                    @@RRN             9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*
     C                   CALL      COMAPI        COMPL
     C                   MOVEL(P)  @@LIC#        SCLIC#
     C*
     C     @@RRN         CHAIN     PBC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      BCID#         SCID#
     C                   MOVE      BCCID#        SCCID#
     C                   MOVE      BCCTYP        SCCTYP
     C*
     C                   MOVEL(P)  'CRTP'        KKTBID
     C                   MOVEL(P)  SCCTYP        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCTYPD
     C                   ELSE
     C                   CLEAR                   SCTYPD
     C                   ENDIF
     C*
     C                   MOVE      BCCSTS        SCCSTS
     C*
     C                   MOVEL(P)  'CSTS'        KKTBID
     C                   MOVEL(P)  SCCSTS        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCSTSD
     C                   ELSE
     C                   CLEAR                   SCSTSD
     C                   ENDIF
     C*
     C                   MOVE      BCCEFD        SCCEFD
     C                   MOVE      BCCDDT        SCCDDT
     C                   MOVEL     BCUDAT        SCUDAT
     C                   ELSE
     C                   CLEAR                   SCCID#
     C                   CLEAR                   SCID#
     C                   CLEAR                   SCCTYP
     C                   CLEAR                   SCTYPD
     C                   CLEAR                   SCCSTS
     C                   CLEAR                   SCSTSD
     C                   CLEAR                   SCCEFD
     C                   CLEAR                   SCCDDT
     C                   CLEAR                   SCUDAT
     C                   ENDIF
     C*
     C     *IN90         DOUEQ     *OFF
     C     *INKL         OREQ      *ON
     C*
     C                   SETOFF                                       90
     C                   EXFMT     WDW01
     C*
     C                   ENDDO
     C*
     C* END THE PROGRAM
     C*
     C                   SETON                                        LR
