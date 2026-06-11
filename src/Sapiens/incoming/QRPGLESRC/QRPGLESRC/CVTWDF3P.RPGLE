     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CVTWDF3P - CREATES WDF3P RECORDS FROM THE COMPANY FILE
     H*****************************************************************
     H* CREATED ON 12/04/2008 BY C:63 FOR SUPPORT 63706
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 08/25/10 C:257 PROJECT: WC1790                                *
     H*                F3SFEI and F3SZP not in use.                   *
     H*                                                               *
     H* 04/19/10 C:235 PROJECT: 25WRG                                 *
     H*                CLAIMS GENERAL RETROFIT                        *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMIXL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWDF3L     UF A E           K DISK

     D*
     D                 DS
     D  XXPHON                 1     10
     D  XXPH1                  1      3
     D  XXPH2                  4     10
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDY(YY)
     C                   PARM      '*CYMD'       SRTFMT            8            YYMD
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     FNKEY         KLIST
     C                   KFLD                    COCO#
     C*
     C     F2KEY         KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C*
     C     IXKEY         KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNPST
     C*
     C     F3KEY         KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    STKEY
     C*
     H***BEG ADD****04/19/10*******************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     H***END ADD****04/19/10*******************************************
     C*
     C                   MOVE      *BLANKS       STKEY             2
     C*
     C                   READ      PCO01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     FNKEY         CHAIN     PFN01                              98
     C     *IN98         DOWEQ     *OFF
     C*
     C     F2KEY         CHAIN     PF201                              90
     C     IXKEY         CHAIN     PIX01                              90
     C*
     C                   CLEAR                   PF301
     C     F3KEY         CHAIN     PF301                              97
     C*
     C                   Z-ADD     FNCO#         F3CO#
     C                   Z-ADD     FNFND         F3FND
     C                   MOVEL     '  '          F3KST
     C                   MOVEL(P)  CONAME        F3NAME
     C                   MOVEL(P)  COADR1        F3ADR1
     C                   MOVEL(P)  COADR2        F3ADR2
     C                   MOVEL(P)  COCTY         F3CTY
     C                   MOVEL(P)  COST          F3ST
     C                   Z-ADD     COZIP         F3ZIP
     C                   Z-ADD     COFEIN        F3FEIN
     C                   MOVE      COAREA        XXPH1
     C                   MOVE      COPHN#        XXPH2
     C                   MOVEL(P)  XXPHON        F3PH#
     C                   MOVE      COFARA        XXPH1
     C                   MOVE      COFAX#        XXPH2
     C                   MOVEL(P)  XXPHON        F3FAX
     C                   MOVEL(P)  IXCNNM        F3INFO
     C                   MOVEL(P)  FNNAME        F3INAM
     C                   MOVEL(P)  F2FEIN        F3IFEI
     C                   MOVEL(P)  COMZIP        F3AZP
     C***BEG ADD****08/25/10*******************************************
     C******               MOVELIXSEIN    F3SFEI    P
     C******               MOVELIXSZP     F3SZP     P
     C                   MOVEL(P)  *ZEROS        F3SFEI
     C                   MOVEL(P)  *ZEROS        F3SZP
     C***END ADD****08/25/10*******************************************
     H***BEG ADD****04/19/10*******************************************
     C     CLTID         IFEQ      '025'
     C                   MOVEL(P)  IXIFEI        F3IFEI
     C                   MOVEL(P)  IXINAM        F3INAM
     C                   ENDIF
     H***END ADD****04/19/10*******************************************
     C*
     C     *IN97         IFEQ      *ON
     C                   WRITE     PF301
     C                   ELSE
     C                   UPDATE    PF301
     C                   ENDIF
     C*
     C     FNKEY         READE     PFN01                                  98
     C                   ENDDO
     C*
     C                   READ      PCO01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
