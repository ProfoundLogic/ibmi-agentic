     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W0070RLU - CONFIRMATION PROGRAM FOR LUBA WC 1007
     H* ***************************************************************
     H* Description:                                                  *
     H*                                                               *
     H* Called from: W0070R                                           *
     H* Calls to:    W8070CLUO7- OVERRIDE PROGRAM FOR FORM            *
     H*              LUBA-WC-1007                                     *
     H*                                                               *
     H*****************************************************************
     H* Created on 01/14/2008 by C:190 for Project 133WET             *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     FW0070FLU  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMCMP     IF   E             DISK
     F                                     INFDS(DBFDS)
      *

     D XLUBB           C                   CONST('W8070CLU07')
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*================================================================
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  ENTRY PARM LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         QQKC              1
     C                   PARM      *INKL         QQKL              1
     C                   PARM                    QQMODE            1
     C     XXRRN         PARM                    QQRRN             9 0
     C     XXFROI        PARM                    QQFROI            2
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
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  Define and initialize work fields
     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    DBRRN         XXRRN
     C     *LIKE         DEFINE    QQFROI        XXFROI
      *
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C*  Retrieve the claim record.
     C*
     C     XXRRN         CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C*  Retrieve the company name
     C*
     C                   Z-ADD     CMCO#         KKCO#
     C     COKY          CHAIN     PCO01                              99
     C                   MOVEL(P)  CONAME        SRDATA
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDATA           80
     C                   PARM      40            SRLENG            3 0
     C                   MOVEL     SRDATA        SCCONM
      *
     C                   MOVE      CMFND         XXFND             3
     C                   MOVE      CMFYR         XXFYR             3
     C                   MOVE      CMCASE        XXCASE            7
     C                   MOVEL     XXFND         SCCASE
     C     SCCASE        CAT(P)    '-':0         SCCASE
     C     SCCASE        CAT(P)    XXFYR:0       SCCASE
     C     SCCASE        CAT(P)    '-':0         SCCASE
     C     SCCASE        CAT(P)    XXCASE:0      SCCASE
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*****
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      *OFF
     C*****
     C*  PROCESS REQUEST SCREEN 1
     C*
     C*
     C*  If ENTER is pressed, call the corresponding NOI print program.
     C*
     C                   MOVE      CMCO#         @@CO#
     C                   MOVE      CMFND         @@FND
     C                   MOVE      CMFYR         @@FYR
     C                   MOVE      CMCASE        @@CASE
     C                   MOVE      *BLANKS       @@FROI
     C                   MOVE      *BLANKS       @@JRST
     C                   MOVE      *ZEROS        @@DATE
     C                   CALL      XLUBB                                99
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FROI            2
     C                   PARM                    @@JRST            2
     C                   PARM                    @@DATE            8
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
      *
     C     *IN99         IFEQ      *ON
     C                   EXFMT     SCR02
     C                   ENDIF
      *
     C                   ENDIF                                                  F12=*ON/*OFF
     C                   ENDIF                                                  CLAIM RECORD?
     C*
     C*  END OF PROGRAM
     C*
     C                   MOVE      *ON           *INLR
     C*
