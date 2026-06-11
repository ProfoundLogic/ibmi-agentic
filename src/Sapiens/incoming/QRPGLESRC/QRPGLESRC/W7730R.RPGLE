      * (C) Copyright Fiserv, Inc., 1997 - 2007
      *
      * W7730R - Prompt for MEMBER PROFILE Report
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 07/23/07  C: 191  PROJECT: 90WFE                              *
      *               - prompt for SS# when executing Employer Exp rep*
      *                 from the Member Profile prompt.               *
      *                                                               *
      * 10/14/03  C: 132    PROJECT: WC1083 SEQ: 37882                *
      *               - Add Display of calling format on Search F10   *
      *                 after return from Search subroutine           *
      *                                                               *
      * 04/24/03 C:142 36936                                          *
      *             - On F13, redisplay screen 1.                     *
      *                                                               *
      * 12/09/02 C:142 WC1086                                         *
      *             - Added summarize by division option.             *
      *                                                               *
      * 10/11/02 C:61 SUPPORT wa045 34444                             *
      *             - Sort entries not required if single employer    *
      *               entered.                                        *
      *                                                               *
      *****************************************************************
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *  SCR01:                                                       *
      *  ERROR:                                                       *
      *  21 - (WCR0538)                                               *
      *  22 - (WCR0517)                                               *
      *  23 - (WCR0957)                                               *
      *  24 - (WCR0615)                                               *
      *                                                               *
      *  POSITION CURSOR:                                             *
      *  31 - SCEMP# POSITION                                         *
      *                                                               *
      *****************************************************************
      *                                                               *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW7730F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMEMP     IF   E             DISK
      *
     FWDF1L     IF   E           K DISK
      *
     FSMCOL     IF   E           K DISK
      *
     FWMUPL     IF   E           K DISK
      *
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUPL1)
      *
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
      *
     FWMCPL     IF   E           K DISK
      *
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
      *
     FWMAAL1    IF   E           K DISK
     F                                     INFDS(DBFDSA)
      *
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PAA02)
      *
     FWMAPP     IF   E             DISK
     F                                     RENAME(PAP01:PAP02)
      *
     FSTSTL1    IF   E           K DISK
      *
     D DBFDS           DS
     D  DBRRN                397    400B 0
      *
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DATA                   1    194
     D  SCFFND                 1      3  0
     D Error           S              1n
     D H10             S             10
     D                 DS
     D H7                      1      7
     D Datusa          S               D   DATFMT(*USA)
     D Dateto          S               D   DATFMT(*USA)
     D Datefr          S               D   DATFMT(*USA)
     D qqoutq          S             10
     D Done_Scr1       S              1n
     D Done_Scr2       S              1n
     D Done_Scr3       S              1n
      *** BEG ADD  **** 10/14/03 *******************************************
     D Done_Srch       S              1n
      *** END ADD  **** 10/14/03 *******************************************
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
      *
      *  USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *   KEY LIST FOR FUND MASTER FILE
      *
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND             3 0
      *
      *   KEY LIST FOR THE FUND DETAIL
      *
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
      *
      *   KEY LIST FOR THE EMPLOYER FILE
      *
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
     C     Wmcpl_Key     Klist
     C                   Kfld                    Upco#
     C                   Kfld                    Upfnd
     C                   Kfld                    Scchap
      *
     C     Agent_Key     Klist
     C                   Kfld                    Upco#
     C                   Kfld                    Upfnd
     C                   Kfld                    Scagnt
      *
     C     CPKY          KLIST
     C                   Kfld                    Upco#
     C                   KFLD                    SCFND
     C                   KFLD                    SCCHAP
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      /Eject
      *
      *** Begin Add *** 04/24/03 *******************************************
     C                   Eval      Done_Scr1 = *Off
     C                   Eval      Done_Scr2 = *Off
     C                   Eval      Done_Scr3 = *Off
      *
     C                   Dou       *In03 = *On or
     C                             *In12 = *On or
     C                             Done_Scr1
     C                   Exsr      Dsp_Scr01
     C                   Select
     C                   When      *In03
     C                   Leave
     C                   When      *In12
     C                   Leave
      *** BEG ADD  **** 10/14/03 *******************************************
     C                   when      Done_Srch = *on
     C                   eval      Done_Srch = *off
     c                   iter
      *** END ADD  **** 10/14/03 *******************************************
     C                   Endsl
      *
     C                   Dou       Done_Scr2
     C                   Exsr      Dsp_Scr02
     C                   Select
     C                   When      *In03
     C                   Eval      Done_Scr1 = *On
     C                   Eval      Done_Scr2 = *On
     C                   Eval      Done_Scr3 = *On
     C                   When      *In12
     C                   Eval      Done_Scr1 = *Off
     C                   Eval      Done_Scr2 = *On
     C                   Eval      Done_Scr3 = *On
     C                   Eval      *In12 = *Off
     C                   Leave
      *** BEG ADD  **** 10/14/03 *******************************************
     C                   when      Done_Srch = *on
     C                   eval      Done_Srch = *off
     c                   iter
      *** END ADD  **** 10/14/03 *******************************************
     C                   When      Not Error
     C                   Eval      Done_Scr1 = *On
     C                   Eval      Done_Scr2 = *On
     C                   Eval      Done_Scr3 = *Off
     C                   Endsl
      *
     C                   If        Not Done_Scr3
     C                   Dou       Done_Scr3
     C                   Exsr      Dsp_Scr03
     C                   Select
     C                   When      *In03
     C                   Eval      Done_Scr1 = *On
     C                   Eval      Done_Scr2 = *On
     C                   Eval      Done_Scr3 = *On
     C                   When      *In12
     C                   Eval      Done_Scr1 = *Off
     C                   Eval      Done_Scr2 = *Off
     C                   Eval      Done_Scr3 = *On
     C                   When      Not Error
     C                   Eval      Done_Scr1 = *On
     C                   Eval      Done_Scr2 = *On
     C                   Eval      Done_Scr3 = *On
     C                   Endsl
     C                   Enddo
     C                   Endif
     C                   Enddo
      *
     C                   Enddo
      *** End   Add *** 04/24/03 *******************************************
      *** Begin Del *** 04/24/03 *******************************************
      *************
     C*************      If        *In03 = *Off and
     C*************                *In12 = *Off and
     C*************                Error = *Off
     C*************      Exsr      Dsp_Scr01
     C*************      Endif
      *************
     C*************      If        *In03 = *Off and
     C*************                *In12 = *Off and
     C*************                Error = *Off
     C*************      Exsr      Dsp_Scr02
     C*************      Endif
      *************
     C*************      If        *In03 = *Off and
     C*************                *In12 = *Off and
     C*************                Error = *Off
     C*************      Exsr      Dsp_Scr03
     C*************      Endif
      *** End  14sp *** 04/24/03 *******************************************
      *
      *  END THE PROGRAM
      *
     C                   MOVE      *On           *INLR
      *
      /Eject
      ********************************************************************
      *   Process Screen 1                                               *
      ********************************************************************
      *
     C     Dsp_Scr01     Begsr
      *
     C                   Dou       *In03 = *On or
     C                             *In12 = *On or
     C                             Error = *Off
      *
     C                   Exfmt     Scr01
     C                   Eval      Error = *Off
      *
     C                   Select
     C                   When      *In03 = *On or
     C                             *In12 = *On
     C                   Leave
      *
      *  F10 - SEARCH
      *
     C                   When      *IN10 = *On
     C                   Exsr      Search
      *** BEG ADD  **** 10/14/03 *******************************************
     C                   eval      Done_SCR1 = *OFF
     C                   eval      Done_Srch = *ON
      *** END ADD  **** 10/14/03 *******************************************
     C                   Iter
      *
     C                   When      *IN19 = *On
     C                   Exsr      Help
     C                   Iter
      *
      *   PROCESS CHANGE OF USER PARAMETERS
      *
     C                   When      *IN13 = *On
     C                   Exsr      Chgprms
      *** Begin Add *** 04/24/03 *******************************************
     C                   Eval      Error = *On
      *** End   Add *** 04/23/03 *******************************************
     C                   Iter
      *
     C                   Other
     C                   Eval      Error = *Off
     C                   Exsr      Validate1
      *
     C                   If        Error
     C                   Iter
     C                   Endif
      *
     C                   Endsl
     C                   Enddo
      *
     C                   Endsr
      *
      /Eject
      ********************************************************************
      *   Process Screen 2                                               *
      ********************************************************************
      *
     C     Dsp_Scr02     Begsr
      *
     C                   Dou       *In03 = *On or
     C                             *In12 = *On or
     C                             Error = *Off
      *
     C                   Exfmt     Scr02
      *
     C                   Eval      *In40 = *Off
     C                   Eval      *In41 = *Off
     C                   Eval      *In42 = *Off
     C                   Eval      *In43 = *Off
     C                   Eval      *In47 = *Off
     C                   Eval      *In48 = *Off
     C                   Eval      *In49 = *Off
     C                   Eval      *In53 = *Off
     C                   Eval      *In54 = *Off
     C                   Eval      *In55 = *Off
     C                   Eval      *In56 = *Off
     C                   Eval      Error = *Off
      *
     C                   Select
     C                   When      *In03 = *On or
     C                             *In12 = *On
     C                   Leave
      *
     C                   When      *IN10 = *On
     C                   Exsr      Search
     C                   Eval      *In12 = *OFF
      *** BEG ADD  **** 10/14/03 *******************************************
     C                   Eval      Done_SCR2 = *OFF
     C                   Eval      Done_Srch = *ON
      *** END ADD  **** 10/14/03 *******************************************
     C                   Iter
      *
     C                   When      *IN19 = *On
     C                   Exsr      Help
     C                   Iter
      *
     C                   Other
     C                   Eval      Error = *Off
     C                   Exsr      Validate2
      *
     C                   If        Error
     C                   Iter
     C                   Endif
      *
     C                   Endsl
      *
     C                   Enddo
      *
     C                   Endsr
      *
      /Eject
      ********************************************************************
      *   Process Screen 3                                               *
      ********************************************************************
      *
     C     Dsp_Scr03     Begsr
      *
     C                   Dou       *In03 = *On or
     C                             *In12 = *On or
     C                             Error = *Off
      *
     C                   Exfmt     Scr03
      *
     C                   Eval      *In40 = *Off
     C                   Eval      *In41 = *Off
     C                   Eval      *In42 = *Off
     C                   Eval      *In43 = *Off
     C                   Eval      Error = *Off
      *
     C                   Select
     C                   When      *In03 = *On or
     C                             *In12 = *On
     C                   Leave
      *
     C                   When      *IN19 = *On
     C                   Exsr      Help
     C                   Iter
      *
     C                   Other
     C                   Eval      Error = *Off
     C                   Exsr      Validate3
      *
     C                   If        Error
     C                   Iter
     C                   Else
     C                   Exsr      Submit_Job
     C                   Leave
     C                   Endif
      *
     C                   Endsl
      *
     C                   Enddo
     C                   Endsr
      /Eject
      ********************************************************************
      *   Submit Job                                                     *
      ********************************************************************
      *
     C     Submit_Job    Begsr
      *
      *
      *   CALL THE CLP TO SUBMIT THE LIST.
      *
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
      *
     C                   MOVE      SCCFYR        QQCFYR            3
     C                   MOVE      SCFYR1        QQFYR1            3
     C                   MOVE      SCFYR2        QQFYR2            3
     C                   MOVE      SCFYR3        QQFYR3            3
     C                   MOVE      SCFYR4        QQFYR4            3
      *
     C                   MOVE      SCDTE         QQDTE             8
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
      *
     C                   If        Scfrdt <> *Zero
     C                   Move      Scfrdt        QQfrdt            8
     C                   Endif
     C                   If        Sctodt <> *Zero
     C                   Move      Sctodt        QQtodt            8
     C                   Endif
      *
     C                   If        Sccfyr = *Zero
      *
     C                   If        Scfrdt <> *Zero and
     C                             Sctodt <> *Zero
     C                   Move      'X'           QQstat            1
     C                   Else
     C                   Move      'B'           QQstat
     C                   Move      '01011950'    QQfrdt
     C                   Move      '12312099'    QQtodt
     C                   Endif
     C                   Else
      *
     C                   Move      'E'           QQstat
     C                   Movel     Sccfyr        H7
     C                   Move      0101          H7
     C     *Ymd0         Move      H7            Datusa
     C     *Usa0         Move      Datusa        QQfrdt
     C                   Movel     Sccfyr        H7
     c                   Move      1231          H7
     C     *Ymd0         Move      H7            Datusa
     C     *Usa0         Move      Datusa        QQtodt
     C                   Endif
      *
     C                   Select
     C                   When      Scpsrt = '1'
     C                   Move      'EX'          QQpsrt            2
     C                   When      Scpsrt = '2'
     C                   Move      'AN'          QQpsrt
     C                   When      Scpsrt = '3'
     C                   Move      'EC'          QQpsrt
     C                   When      Scpsrt = '4'
     C                   Move      'UW'          QQpsrt
     C                   When      Scpsrt = '5'
     C                   Move      'ST'          QQpsrt
     C                   When      Scpsrt = ' '
     C                   Move      'EN'          QQpsrt
     C                   Endsl
      *
     C                   Select
     C                   When      Scssrt = '1'
     C                   Move      'EX'          QQssrt            2
     C                   When      Scssrt = '2'
     C                   Move      'AN'          QQssrt
     C                   When      Scssrt = '3'
     C                   Move      'EC'          QQssrt
     C                   When      Scssrt = '4'
     C                   Move      'UW'          QQssrt
     C                   When      Scssrt = '5'
     C                   Move      'ST'          QQssrt
     C                   When      Scssrt = ' '
     C                   Move      '  '          QQssrt
     C                   Endsl
      *
     C                   MOVE      Scagnt        QQagnt            5
     C                   MOVE      Scchap        QQchap            3
     C                   MOVE      Scundw        QQundw            3
     C                   MOVE      Scprst        QQprst            2
     C                   MOVE      Scexpr        QQexpr            1
     C                   MOVE      Sctypo        QQtypo            1
      *
     C                   CALL      'W7730C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQCFYR
     C                   PARM                    QQFYR1
     C                   PARM                    QQFYR2
     C                   PARM                    QQFYR3
     C                   PARM                    QQFYR4
     C                   PARM                    QQDTE
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQfrdt
     C                   PARM                    QQtodt
     C                   PARM                    QQexpr
     C                   PARM                    QQtypo
     C                   PARM                    QQagnt
     C                   PARM                    QQchap
     C                   PARM                    QQundw
     C                   PARM                    QQprst
     C                   PARM                    QQpsrt
     C                   PARM                    QQssrt
     C                   PARM                    QQstat
     C                   PARM                    qqoutq
     C                   PARM      'D'           MODE              1
      *** Begin Add *** 12/09/02 *******************************************
     C                   PARM      Scsyn         QQsyn             1
      *** End   Add *** 12/09/02 *******************************************
      *** BEG ADD ****** 07/23/07 *******************************************
     C                   PARM      scprss        QQPRSS            1
      *** END ADD ****** 07/23/07 *******************************************
      *
     C                   Endsr
      *
      /Eject
      ********************************************************************
      *   Validate Fields on Screen 1                                    *
      ********************************************************************
      *
     C     Validate1     Begsr
      *
      *  VALIDATE CURRENT FUND YEAR
      *
     C                   If        SCCFYR <> *Zeros
     C                   MOVE      SCCFYR        XXFYR             3 0
     C     F1KEY         CHAIN     PF101
      *
     C                   If        Not %Found
     C                   Eval      Error = *On
     C                   Eval      *In26 = *On
     C                   Eval      *In45 = *On
     C                   Endif
     C                   Endif
      *
      *  VALIDATE THE FUND YEARS FOR THE FUND
      *
     C                   MOVE      SCFYR1        XXFYR             3 0
      *
     C                   If        Xxfyr <> *Zero
     C     F1key         Chain     PF101
      *
     C                   If        Not %Found
     C                   Eval      Error = *On
     C                   Eval      *In20 = *On
     C                   Endif
      *
     C                   Else
     C*                  Eval      *In29 = *On
     C                   Endif
      *
     C                   MOVE      SCFYR2        XXFYR             3 0
      *
     C     XXFYR         IFNE      *ZERO
     C     F1KEY         CHAIN     PF101
      *
     C                   If        not %Found
     C                   Eval      Error = *On
     C                   Eval      *In21 = *On
     C                   Endif
      *
     C                   Endif
      *
     C                   MOVE      SCFYR3        XXFYR             3 0
      *
     C     XXFYR         IFNE      *ZERO
     C     F1KEY         CHAIN     PF101
      *
     C                   If        Not %Found
     C                   Eval      Error = *On
     C                   Eval      *In22 = *On
     C                   Endif
      *
     C                   Endif
      *
     C                   MOVE      SCFYR4        XXFYR             3 0
      *
     C     XXFYR         IFNE      *ZERO
     C     F1KEY         CHAIN     PF101
      *
     C                   If        Not %Found
     C                   Eval      Error = *On
     C                   Eval      *In23 = *On
     C                   Endif
      *
     C                   Endif
      *
      *  VALIDATE DATE
      *
     C                   Z-ADD     SCDTE         SRDTI
      *
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
      *
     C                   If        SRERR = '1'
     C                   Eval      Error = *On
     C                   Eval      *In24 = *On
     C                   Endif
      *
      *  VALIDATE EMPLOYER IF ENTERED
      *
     C     SCEMP#        IFNE      *ZERO
      *
     C     EMKEY         CHAIN     PEM02
      *
     C                   If        not %Found
     C                   Eval      Error = *On
     C                   Eval      *In25 = *On
     C                   Eval      *In45 = *On
     C                   Endif
      *
     C                   Endif
      *
      *  Validate From Date
      *
     C                   If        Scfrdt <> *Zero
     C                   Z-add     Scfrdt        Srdti
      *
     C                   Call      'SRCVTDAT'    DATCHK
     C                   Movel     Srdto         Scfrdt
      *
     C                   If        Srerr = '1'
     C                   Eval      Error = *On
     C                   Eval      *In27 = *On
     C                   Else
     C     *Usa          Move      Scfrdt        Datefr
     C                   Endif
      *
     C                   Endif
      *
      *  Validate To Date
      *
     C                   If        Sctodt <> *Zero
     C                   Z-add     Sctodt        Srdti
      *
     C                   Call      'SRCVTDAT'    Datchk
     C                   Movel     Srdto         Sctodt
      *
     C                   If        Srerr = '1'
     C                   Eval      Error = *On
     C                   Eval      *In28 = *On
     C                   Else
     C     *Usa          Move      Sctodt        Dateto
     C                   Endif
      *
     C                   Endif
      *
     C                   Eval      QQexpr = Scexpr
     C                   Eval      QQtypo = Sctypo
      *
      *  Date range
      *
     C                   If        (Scfrdt <> *Zero  or
     C                             Sctodt <> *Zero)and
     C                             (Dateto < Datefr)
     C                   Eval      *In36 = *On
     C                   Eval      *In46 = *On
     C                   Eval      Error = *On
     C                   Endif
      *
      *  Cannot enter Year and date range.
      *
     C                   If        (Sccfyr <> *Zero) and
     C                             (Scfrdt <> *Zero or
     C                             Sctodt <> *Zero)
     C                   Eval      *In35 = *On
     C                   Eval      *In44 = *On
     C                   Eval      *In45 = *On
     C                   Eval      *In46 = *On
     C                   Eval      Error = *On
     C                   Endif
      *
      *  Must enter policy year if employer.
      *
     C                   If        Scemp# <> *Zero and
     C                             Sccfyr = *Zero
     C                   Eval      *In34 = *On
     C                   Eval      *In44 = *On
     C                   Eval      Error = *On
     C                   Endif
      *
      *  Must enter some fields.
      *
     C                   If        Scemp# = *Zero and
     C                             Sccfyr = *Zero and
     C                             Scfrdt = *Zero and
     C                             Sctodt = *Zero
     C                   Eval      *In43 = *On
     C                   Eval      *In44 = *On
     C                   Eval      *In45 = *On
     C                   Eval      *In46 = *On
     C                   Eval      Error = *On
     C                   Endif
      *
      *** BEG ADD ****** 07/23/07 *******************************************
     c*
     c* Print SS# must be Y or blank
     c*
     c                   if        not Error
     c                   if        scprss <> 'Y' and scprss <> *blanks
     c                   seton                                        57
     C                   Eval      Error = *On
     c                   endif
     c                   endif
      *** END ADD ****** 07/23/07 *******************************************
      *
     C                   Endsr
      *
      /Eject
      ********************************************************************
      *   Validate Fields on Screen 2                                    *
      ********************************************************************
      *
     C     Validate2     Begsr
      *
     C                   Eval      Error = *Off
      *
      *  Validate Agent Number
      *
     C                   If        Scagnt > *Zero
     C     Agent_Key     Chain     Wmaal1
      *
     C                   If        Not %Found
     C                   Eval      Error = *On
     C                   Eval      *In40 = *On
     C                   Endif
     C                   Endif
      *
      *  Validate Chapter
      *
     C                   If        Scchap > *Zero
     C     Wmcpl_Key     Chain     Wmcpl
     C                   If        Not %Found
     C                   Eval      Error = *On
     C                   Eval      *In41 = *On
     C                   Endif
     C                   Endif
      *
      *  Validate Underwriter
      *
     C                   If        Scundw <> *Blank
     C     Scundw        Chain     Smupl1
     C                   IF        Not %Found
     C                   Eval      Error = *On
     C                   Eval      *In42 = *On
     C                   Endif
     C                   Endif
      *
      *  Validate Premium Rating State
      *
     C                   If        Scprst <> *Blank
     C                   Movel(p)  Scprst        H10
     C     H10           Chain     Ststl1
     C                   If        Not %Found
     C                   Eval      Error = *On
     C                   Eval      *In43 = *On
     C                   Endif
     C                   Endif
      *
      *  Validate Primary Sort
      *
     C                   If        Scemp# <>  0  or
     C                             Scsyn  = 'Y'
     C                   If        Scpsrt <> ' '
     C                   Eval      Error = *On
     C                   Eval      *In55 = *On
     C                   Endif
      *
     C                   Else
      *
     C                   If        Scpsrt <> '1' and
     C                             Scpsrt <> '2' and
     C                             Scpsrt <> '3' and
     C                             Scpsrt <> '4' and
     C                             Scpsrt <> '5' and
     C                             Scpsrt <> ' '
     C                   Eval      Error = *On
     C                   Eval      *In47 = *On
     C                   Endif
     C                   Endif
      *
      *  Validate Secondary Sort
      *
     C                   If        (Scemp# <>  0  or
     C                             Scsyn  = 'Y')
     C                   If        Scssrt <> ' '
     C                   Eval      Error = *On
     C                   Eval      *In56 = *On
     C                   Endif
      *
     C                   Else
      *
     C                   If        Scssrt <> '1' and
     C                             Scssrt <> '2' and
     C                             Scssrt <> '3' and
     C                             Scssrt <> '4' and
     C                             Scssrt <> '5' and
     C                             Scssrt <> ' '
     C                   Eval      Error = *On
     C                   Eval      *In48 = *On
     C                   Endif
     C                   Endif
      *
      *  Must Y or N for Summarization
      *
     C                   If        Scsyn <> 'Y' and
     C                             Scsyn <> 'N'
     C                   Eval      *In53 = *On
     C                   Eval      Error = *On
     C                   Endif
      *
      *  Must be N if another sort has been selected
      *
     C                   If        Scsyn  <> 'N' and
     C                             Scpsrt <> ' ' and
     C                             Scssrt <> ' '
     C                   Eval      *In54 = *On
     C                   Eval      Error = *On
     C                   Endif
      *
     C                   Endsr
      *
      /Eject
      ********************************************************************
      *   Validate Fields on Screen 3                                    *
      ********************************************************************
      *
     C     Validate3     Begsr
      *
     C                   Eval      Error = *Off
     C                   Eval      *In40 = *Off
     C                   Eval      *In41 = *Off
     C                   Eval      *In42 = *Off
     C                   Eval      *In43 = *Off
     C                   Eval      *In47 = *Off
     C                   Eval      *In48 = *Off
     C                   Eval      *In49 = *Off
     C                   Eval      *In53 = *Off
     C                   Eval      *In54 = *Off
      *
      *  Validate file selection
      *
     C                   If        Sctypo <> 'F' and
     C                             Sctypo <> 'P' and
     C                             Sctypo <> 'B'
     C                   Eval      Error = *On
     C                   Eval      *In49 = *On
     C                   Endif
      *
     C                   Endsr
      *
      /Eject
      ********************************************************************
      *   Search Fields                                                *
      ********************************************************************
      *
     C     Search        Begsr
      *
      *  F10 - TABLE FILE SEARCH - EMPLOYER NUMBER OR DIVISION
      *
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
      *
     C                   MOVE      *ZEROS        @@RRN
      *
     C                   CALL      'W0050R'
     C     *IN03         PARM                    @@KC              1
     C     *IN12         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
      *
     C     @@RRN         Ifne      *ZEROS
     C     @@RRN         CHAIN     PEM01
      *
     C                   If        %Found
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   ENDIF                                                  *IN60=OFF
      *
     C                   ENDIF                                                  *INKL=OFF,@@RRN
      *
     C                   IF        *In12 = *On or
     C                             @@RRN = *ZEROS  or
     C                             Not %Found(Wmeml)
     C                   Eval      *In31 = *On                                  SCEMP# POSITION
     C                   ENDIF                                                  *INKL=ON,@@RRN
      *
     C                   ENDIF                                                  Q2RECF=SCR01
      *
      *  Chapter Search
      *
     C     HFLD          IFEQ      'SCCHAP'
     C                   MOVE      *ZEROS        @@RRN
     C                   EVAL      *IN51 = *ON
      *
     C                   CALL      'W0720R'
     C     *IN03         PARM                    @@KC
     C     *IN12         PARM                    @@KL
     C                   PARM      '2'           @@CALL
     C                   PARM                    @@RRN
     C                   PARM      DBRRN         @@RRN2
     C                   PARM                    ##CHAP            3 0
      *
     C     ##CHAP        IFNE      *ZEROS
     C                   Z-ADD     ##CHAP        SCCHAP
     C     CPKY          CHAIN     PCP01
     C                   If        %Found
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
      *
      *  UNDERWRITER CODE
      *
     C                   If        Hfld = 'SCUNDW'
     C                   EVAL      *IN52 = *ON
      *
     C                   CALL      'S0005R'
     C                   PARM      '2'           @@CALL
     C                   PARM      *ZEROS        @@RRN
     C                   PARM      3             ##View            1 0
      *
     C     @@RRN         IFNE      *ZEROS
     C     @@RRN         CHAIN     Smupp
      *
     C                   If        %Found
     C                   MOVEL     Upstaf        SCUNDW
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *  Agent Number
      *
     C     Hfld          Ifeq      'SCAGNT'
     C                   EVAL      *IN50 = *ON
      *
     C                   CALL      'W0040R'
     C     *IN03         PARM                    @@KC
     C     *IN12         PARM                    @@KL
     C                   Parm      '2'           @@CALL
     C                   Parm      DBRRN         @@RRN
      *
     C     @@RRN         Ifne      *Zeros
     C     @@RRN         Chain     Wmaap
     C                   If        %Found
     C                   Z-add     Aaagt#        Scagnt
     C                   Endif
     C                   Endif
      *
     C                   Endif
      *
     C                   Endsr
      *
      /Eject
      ********************************************************************
      *   Process Help Key                                               *
      ********************************************************************
      *
     C     Help          Begsr
      *
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
      *
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
      *
     C                   Endsr
      *
      /Eject
      ********************************************************************
      *   Change User parameters                                         *
      ********************************************************************
      *
     C     Chgprms       Begsr
      *
     C                   CALL      'W0006R'
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *On
     C                   MOVE      *ZERO         UPCO#
     C                   Endif
     C                   eval      qqoutq = UPOUTQ
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCCFYR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
      *
      *  GET THE COMPANY RECORD
      *
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
      *  GET THE FUND RECORD
      *
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
      *
     C                   TIME                    DSDTTM
      *
     C                   Endsr
      *
      /Eject
      ********************************************************************
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      ********************************************************************
      *
     C     *Inzsr        BEGSR
      *
     C                   TIME                    DSDTTM
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *On
     C                   MOVE      *ZERO         UPCO#
     C                   Endif
      *
     C                   eval      qqoutq = UPOUTQ
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCCFYR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
      *
      *  GET THE COMPANY RECORD
      *
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
      *  GET THE FUND RECORD
      *
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
      *
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
     C                   MOVE      *On           *IN11                          1ST TIME INDIC.
      *
     C                   ENDSR
      *
