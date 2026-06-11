     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE CVTWDF3PST - CREATES WDF3P RECORDS FROM HARDCODE FOR STATE
     H*****************************************************************
     H* CREATED ON 12/04/2008 BY C:63 FOR SUPPORT 63706
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMIXL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWDF3L     UF A E           K DISK

     D*
     D ST              S              2    DIM(20) CTDATA PERRCD(1)
     D*
     D                 DS
     D  XXPHON                 1     10
     D  XXPH1                  1      3
     D  XXPH2                  4     10
     D*
     D XXNAME          C                   CONST('Guarantee Insurance -
     D                                     Company')
     D XXADR1          C                   CONST('43 Skyline Drive')
     D XXADR2          C                   CONST('Ste 2020')
     D XXCTY           C                   CONST('Lake Mary')
     D XXST            C                   CONST('FL')
     D XXZIP           C                   CONST('327467124')
     D XXAZP           C                   CONST('327958470')
     D XXPHN#          C                   CONST('          ')
     D XXFAX#          C                   CONST('          ')
     D XXINFO          C                   CONST('                    ')
     D XXINAM          C                   CONST('                    ')
     D XXIFEI          C                   CONST('         ')
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
     C                   KFLD                    KKST              2
     C*
     C     F3KEY         KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    STKEY
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
     C*
     C                   Z-ADD     1             X                 2 0
     C                   DO        20            X
     C*
     C     ST(X)         IFEQ      *BLANKS
     C                   LEAVE
     C                   ENDIF
     C*
     C                   MOVEA     ST(X)         KKST
     C     IXKEY         CHAIN     PIX01                              90
     C*
     C                   CLEAR                   PF301
     C*
     C                   MOVE      KKST          STKEY             2
     C     F3KEY         CHAIN     PF301                              97
     C*
     C                   Z-ADD     FNCO#         F3CO#
     C                   Z-ADD     FNFND         F3FND
     C                   MOVEL     KKST          F3KST
     C                   MOVEL(P)  XXNAME        F3NAME
     C                   MOVEL(P)  XXADR1        F3ADR1
     C                   MOVEL(P)  XXADR2        F3ADR2
     C                   MOVEL(P)  XXCTY         F3CTY
     C                   MOVEL(P)  XXST          F3ST
     C                   MOVE      XXZIP         F3ZIP
     C                   Z-ADD     *ZEROS        F3FEIN
     C                   MOVEL(P)  XXPHN#        F3PH#
     C                   MOVEL(P)  XXFAX#        F3FAX
     C                   MOVEL(P)  XXINFO        F3INFO
     C                   MOVEL(P)  XXINAM        F3INAM
     C                   MOVEL(P)  XXIFEI        F3IFEI
     C                   MOVEL(P)  XXAZP         F3AZP
     C*
     C     *IN97         IFEQ      *ON
     C                   WRITE     PF301
     C                   ELSE
     C                   UPDATE    PF301
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     FNKEY         READE     PFN01                                  98
     C                   ENDDO
     C*
     C                   READ      PCO01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
**  ARRAY ST - STATES
FL
AL
VA
WV
GA















