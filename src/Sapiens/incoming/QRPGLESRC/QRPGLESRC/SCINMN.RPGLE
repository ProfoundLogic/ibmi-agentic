     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SCINMN - Visual Darts Sub-Menu Creation
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program creates a entry on the MAIN menu, creates  **
     H**     the sub-menu V-DARTS and adds entries on that.          **
     H**                                                             **
     H**                                                             **
     H**   Programmer: C:128                                         **
     H**   Created:   12/11/00      Project: DR123                   **
     H*****************************************************************
     H** REVISIONS:
     H*
     H*
     H*****************************************************************
     H**
     H*****************************************************************
     F*
     FMMOPL     UF A E           K DISK
      *

     D CMD             S             71    DIM(8) CTDATA PERRCD(1)              Ext. CMD + Parms
     D DSC             S             40    DIM(8) CTDATA PERRCD(1)              Description
     D OPT             S             40    DIM(8) CTDATA PERRCD(1)              Option
      *
      *================================================================
      *  Constants
      *================================================================
     D PDS             C                   CONST('PDSPGMR')
     D PNAM            C                   CONST('SCINMN')
      *
      *** Default Date and Time
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
      *

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
      *
     C                   MOVEL     'V-DARTS'     MNAM             10
      *
     C     MNAM          CHAIN     MMOPL                              60
     C     *IN60         IFEQ      *ON
      *
      *          Move defaults to data base fields
     C                   MOVEL     SRDTO         OPDT                           Todays date
     C                   MOVE      DSTIME        OPTIME                         Time now
     C                   MOVEL     'A'           OPSTAT
     C                   MOVEL     'N'           OPYNO
     C                   MOVEL     'LIV'         OPAPP
     C                   MOVE      *ZERO         OPMMSC
     C                   MOVE      *ZERO         OPMINS
     C                   MOVEL     PDS           OPUSER
     C                   MOVEL     PNAM          OPPGM
      *
      *       X=1    Managed Care Main Menu Option to V-Darts
      *       X=2    V-Darts sub-menu
      *       X=3/7  Options on V-Darts sub-menu
      *       X=8    Return to Managed Care Main Menu
      *
     C                   DO        8             X                 2 0
     C                   SELECT
      *
     C     X             WHENEQ    1
     C                   MOVEL     'MAIN'        OPMNAM
     C                   MOVEL     'Y'           OPMYN
     C     X             WHENEQ    2
     C                   MOVEL     'V-DARTS'     OPMNAM
     C                   MOVEL     'Y'           OPMYN
     C     X             WHENEQ    8
     C                   MOVEL     'V-DARTS'     OPMNAM
     C                   MOVEL     'Y'           OPMYN
     C                   OTHER
     C                   MOVEL     'V-DARTS'     OPMNAM
     C                   MOVEL     'N'           OPMYN
     C                   ENDSL
      *
     C                   MOVE      *BLANKS       OPOPTN
     C                   MOVEL     *BLANKS       OPEXCL
     C                   MOVEL     *BLANKS       OPMNTT
     C                   MOVEL     OPT(X)        OPOPTN
     C                   MOVEL     CMD(X)        OPEXCL
     C                   MOVEL     DSC(X)        OPMNTT
      *
      *          Write Menu records
     C                   WRITE     POP01
     C                   ENDDO
     C                   ENDIF                                                  *IN60 END
      *
     C                   MOVE      *ON           *INLR
**   External command with parms
V-DARTS
V-DARTS
IDSPLCK                ('Y' ' ')
IDSPLCK                (' ' ' ')
IDSPLCK                (' ' 'Y')
SC092R                 (' ' ' ' ' ' X'000000000F')
IS530R                 ('                                        ' ' ')
MAIN
**   Menu option description
VISUAL DARTS MENU
VISUAL DARTS MAIN MENU
WORK WITH OBJ LOCKS/VDARTS (ALL/RESET)
WORK WITH OBJ LOCKS/VDARTS (ALL/NO REST)
WORK WITH OBJ LOCKS/VDARTS (SELF)
WORK WITH KEY TYPES
WORKSTATION ID SETUP
MANAGED CARE MAIN MENU
**   Menu options
98VD

A010
A020
A030
A040
A050
A060
