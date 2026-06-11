      * (C) Copyright Progressive Data Solutions, Inc., 1997 - 2003   *
      *                                                               *
      *****************************************************************
      *      W7940R - Prompt for NEW/RENEWAL BUSINESS REPORT          *
      *      CREATED 01/17/03 C:142   PROJECT: WC1029                 *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 07/07/04  C:132 Project: WA099  Seq#: 43117                   *
      *               - Change screen fields to REF from CHGINPDFT so *
      *                  will not blank out when use F10 Search on    *
      *                  group fields.                                *
      *                                                               *
      * 06/28/04  C:132 Project: wa099  Seq#: 43027                   *
      *               - Indent the "If AN or AA" under secondary Sort *
      *                  on screen two. No code changes in program    *
      *                                                               *
      * 04/28/04  C:132 Project: 114WHQ                               *
      *               - Add secondary selection:                      *
      *                  "Sort by Agent#/Name when Primary sort is    *
      *                   Underwriter Code.                           *
      *               - Totals by Agent within Underwriter Code       *
      *                                                               *
      * 10/13/03  C:61  Project: WC1017                               *
      *               - Add selection:                                *
      *                  "Renewal Quote / Live Policy / Both".        *
      *               - Edit comments                                 *
      *                                                               *
      *****************************************************************
      *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW7940F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
     FWMFNL     IF   E           K DISK
      *
     FSMCOL     IF   E           K DISK
      *
     FWMUPL     IF   E           K DISK
      *
      *
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DATA                   1    194
     D  SCFFND                 1      3  0
     D Error           S              1n
     D Datusa          S               D   DATFMT(*USA)
     D Dateto          S               D   DATFMT(*USA)
     D Datefr          S               D   DATFMT(*USA)
     D Fund            S              3  0
     D qqoutq          S             10
      *
      *
      *
      *=================================================================
      *  KLIST
      *=================================================================
      *
      *   KEY LIST FOR FUND MASTER FILE
      *
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND             3 0
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
     C     Wmfnl_Key     KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    Fund
      *
      *
      *
      *=================================================================
      *  PLIST
      *=================================================================
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
      *
     C     Fnpl          Plist
     C                   Parm      *BLANK        ##KC              1
     C                   Parm      *BLANK        ##KL              1
     C                   Parm      '2'           ##CALL            1
     C                   Parm                    ##CO#             3
     C                   Parm      *BLANKS       ##FND             3
     C                   Parm      *BLANKS       ##FYR             3
     C                   Parm      1             ##VIEW            1 0
      *
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
      *
      *
      *
      **********************************************************************
      * PROCESS SCREEN 01                                                  *
      **********************************************************************
      *
     C                   Dou       *In03 = *On or
     C                             *In12 = *On
      *
     C                   Exfmt     Scr01
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
     C                   Iter
      *
     C                   Other
     C                   Eval      Error = *Off
     C                   Exsr      Validate1
      *
     C                   If        Error
     C                   Eval      Error = *Off
     C                   Iter
     C                   Endif
      *
     C                   Endsl
      *
      *
      *
      **********************************************************************
      * PROCESS SCREEN 02                                                  *
      **********************************************************************
      *
     C                   Dou       *In03 = *On or
     C                             *In12 = *On
      *
     C                   Exfmt     Scr02
      *
     C                   Select
     C                   When      *In03 = *On or
     C                             *In12 = *On
     C                   Leave
      *
     C                   When      *IN10 = *On
     C                   Exsr      Search
     C                   Eval      *In12 = *OFF
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
     C                   Else
     C                   Exsr      Submit_Job
     C                   Leave
     C                   Endif
      *
     C                   Endsl
      *
     C                   Enddo
      *
     C                   If        *In03 = *Off and
     C                             *in12 = *Off
     C                   Leave
     C                   Endif
      *
     C                   Eval      *In12 = *Off
     C                   Enddo
      *
      *  END THE PROGRAM
      *
     C                   MOVE      *On           *INLR
      *
      *
      *
      ********************************************************************
      *   Submit Job                                                     *
      ********************************************************************
      *
     C     Submit_Job    Begsr
      *
      *   CALL THE CLP TO SUBMIT THE LIST.
      *
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCGRPFR       QQFND1            3
     C                   MOVE      SCGRPTO       QQFND2            3
      *
     C                   CLEAR                   QQFYR             3
      *
     C                   CLEAR                   QQDTE             8
     C                   CLEAR                   QQEMP#            9
     C                   CLEAR                   QQDIV             5
      *
     C                   If        Scfrdt <> *Zero
     C                   Move      Scfrdt        QQfrdt            8
     C                   Endif
      *
     C                   If        Sctodt <> *Zero
     C                   Move      Sctodt        QQtodt            8
     C                   Endif
      *
     C                   Move      Scpsrt        QQpsrt            2
     C                   Move      Scssrt        QQssrt            2
      *
     C                   Clear                   QQagnt            5
     C                   Clear                   QQchap            3
     C                   Clear                   QQundw            3
     C                   Clear                   QQprst            2
     C                   Clear                   QQexpr            1
      ****** beg del ******** 10/13/03 ********************************
     C**********         Move      Scpfb         QQtypo            1
      ****** end del ******** 10/13/03 ********************************
      *
     C                   Call      'W7940C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND1
     C                   PARM                    QQFND2
     C                   PARM                    QQFYR
     C                   PARM                    QQDTE
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQfrdt
     C                   PARM                    QQtodt
     C                   PARM                    QQexpr
      ****** beg del ******** 10/13/03 ********************************
     C**********         PARM                    QQtypo
      ****** end del ******** 10/13/03 ********************************
     C                   PARM                    QQagnt
     C                   PARM                    QQprst
     C                   PARM                    QQpsrt
     C                   PARM                    QQssrt
     C                   PARM      Scdtsl        QQstat            1
     C                   PARM                    QQoutq
     C                   PARM      'D'           Mode              1
     C                   PARM                    Scnrb             1
     C                   PARM                    Scapb             1
     C                   PARM                    Scdtsl            1
     C                   PARM                    Scexpc            1
     C                   PARM                    Scpsa             1
     C                   PARM                    Scpfb             1
      ****** beg add ******** 10/13/03 ********************************
     C                   PARM                    Scrlb             1
      ****** end add ******** 10/13/03 ********************************
      *
      ***** BEG ADD ***** 04/28/04 ************************************
     C                   PARM                    Scssa             1            Secondary sort agt #
      ***** END ADD ***** 04/28/04 ************************************
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
     C                   Do
      *
     C                   If        Scgrpfr > *Zero or
     C                             Scgrpto > *Zero
     C
     C                   If        Scgrpfr > *Zero
     C                   Eval      Fund = Scgrpfr
     C     Wmfnl_Key     Chain     Wmfnl
      *
     C                   If        Not %Found
     C                   Eval      Error = *On
     C                   Eval      *In30 = *On
     C                   Leave
     C                   Endif
      *
     C                   Endif
      *
     C                   If        Scgrpto > *Zero
     C                   Eval      Fund = Scgrpto
     C     Wmfnl_Key     Chain     Wmfnl
      *
     C                   If        Not %Found
     C                   Eval      Error = *On
     C                   Eval      *In31 = *On
     C                   Leave
     C                   Endif
      *
     C                   Endif
      *
     C                   If        Scgrpto < Scgrpfr
     C                   Eval      Error = *On
     C                   Eval      *In32 = *On
     C                   Leave
     C                   Endif
      *
     C                   Endif
      *
      *  Validate new/renewal/both
      *
     C                   If        Scnrb <> 'N' and
     C                             Scnrb <> 'R' and
     C                             Scnrb <> 'B'
     C                   Eval      Error = *On
     C                   Eval      *In36 = *On
     C                   Leave
     C                   Endif
      *
      *  Validate active/pending/both
      *
     C                   If        Scapb <> 'A' and
     C                             Scapb <> 'P' and
     C                             Scapb <> 'B'
     C                   Eval      Error = *On
     C                   Eval      *In37 = *On
     C                   Leave
     C                   Endif
      ****** beg add ******** 10/13/03 ********************************
      *
      *  Validate Renewal Quote / Live Policy / Both
      *
     C                   If        Scrlb <> 'R' and
     C                             Scrlb <> 'L' and
     C                             Scrlb <> 'B'
     C                   Eval      Error = *On
     C                   Eval      *In71 = *On
     C                   Leave
     C                   Endif
      ****** end add ******** 10/13/03 ********************************
      *
      *  Validate From Date
      *
     C                   Z-add     Scfrdt        Srdti
      *
     C                   Call      'SRCVTDAT'    DATCHK
     C                   Movel     Srdto         Scfrdt
      *
     C                   If        Srerr = '1'
     C                   Eval      Error = *On
     C                   Eval      *In27 = *On
     C                   Leave
     C                   Else
     C     *Usa          Move      Scfrdt        Datefr
     C                   Endif
      *
     C                   If        Scfrdt = *Zero
     C                   Eval      Error = *On
     C                   Eval      *In27 = *On
     C                   Leave
     C                   Endif
      *
      *  Validate To Date
      *
     C                   Z-add     Sctodt        Srdti
      *
     C                   Call      'SRCVTDAT'    Datchk
     C                   Movel     Srdto         Sctodt
      *
     C                   If        Srerr = '1'
     C                   Eval      Error = *On
     C                   Eval      *In28 = *On
     C                   Leave
     C                   Else
     C     *Usa          Move      Sctodt        Dateto
     C                   Endif
      *
     C                   If        Sctodt = *Zero
     C                   Eval      Error = *On
     C                   Eval      *In28 = *On
     C                   Leave
     C                   Endif
      *
      *  Date range
      *
     C                   If        (Scfrdt <> *Zero  or
     C                             Sctodt <> *Zero)and
     C                             (Dateto < Datefr)
     C                   Eval      *In35 = *On
     C                   Eval      Error = *On
     C                   Leave
     C                   Endif
      *
      *  Date type Issues or Effective
      *
     C                   If        Scdtsl <> 'E' and
     C                             Scdtsl <> 'I'
     C                   Eval      Error = *On
     C                   Eval      *In44 = *On
     C                   Leave
     C                   Endif
      *
     C                   If        Scdtsl = 'I' and
     C                             Scapb  <> 'A'
     C                   Eval      Error = *On
     C                   Eval      *In50 = *On
     C                   Leave
     C                   Endif
      *
      *  Validate expense constant Y/N
      *
     C                   If        Scexpc <> 'Y' and
     C                             Scexpc <> 'N'
     C                   Eval      Error = *On
     C                   Eval      *In38 = *On
     C                   Leave
     C                   Endif
      *
     C                   Enddo
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
     C                   Do
      *
      *  Validate Primary Sort
      *
     C                   If        Scpsrt <> 'AN' and
     C                             Scpsrt <> 'AA' and
     C                             Scpsrt <> 'EN' and
     C                             Scpsrt <> 'EA' and
     C                             Scpsrt <> 'UW' and
     C                             Scpsrt <> 'DT'
     C                   Eval      Error = *On
     C                   Eval      *In40 = *On
     C                   Leave
     C                   Endif
      *
      *  Validate primary/secondary
      *
     C                   If        Scpsrt = 'AN' or
     C                             Scpsrt = 'AA'
     C                   If        Scpsa <> 'P' and
     C                             Scpsa <> 'S'
     C                   Eval      Error = *On
     C                   Eval      *In41 = *On
     C                   Leave
     C                   Endif
     C                   Endif
      *                                                               *
      ***** BEG ADD ***** 04/28/04 ************************************
      *
      *  Validate Primary Sort
      *
     C                   If        Scpsrt <> 'AN' AND
     C                             Scpsrt <> 'AA'
     C                   If        Scpsa  = 'P' OR
     C                             Scpsa  = 'S'
     C                   Eval      Error = *On
     C                   Eval      *In47 = *On
     C                   Leave
     C                   Endif
     C                   Endif
      ***** END ADD ***** 04/28/04 ************************************
      *
      *  Validate Secondary Sort
      *
      ***** BEG DEL ***** 04/28/04 ************************************
     C**************     If        Scssrt <> 'EN' and
     C**************               Scssrt <> 'EA' and
     C**************               Scssrt <> 'UW' and
     C**************               Scssrt <> 'DT' and
     C**************               Scssrt <> '  '
      ***** END DEL ***** 04/28/04 ************************************
      *
      ***** BEG ADD ***** 04/28/04 ************************************
     C                   If        Scssrt <> 'EN' and
     C                             Scssrt <> 'EA' and
     C                             Scssrt <> 'UW' and
     C                             Scssrt <> 'DT' and
     C                             Scssrt <> 'AN' and
     C                             Scssrt <> 'AA' and
     C                             Scssrt <> '  '
      ***** END ADD ***** 04/28/04 ************************************
     C                   Eval      Error = *On
     C                   Eval      *In42 = *On
     C                   Endif
      *                                                               *
      ***** BEG ADD ***** 04/28/04 ************************************
      *
      *  Validate primary/secondary agent if secondary sort AN or AA
      *
     C                   If        Scssrt = 'AN' or
     C                             Scssrt = 'AA'
     C                   If        Scssa <> 'P' and
     C                             Scssa <> 'S'
     C                   Eval      Error = *On
     C                   Eval      *In45 = *On
     C                   Leave
     C                   Endif
     C                   Endif
      *
     C                   If        Scssrt <> 'AN' AND
     C                             Scssrt <> 'AA'
     C                   If        Scssa  = 'P' OR
     C                             Scssa  = 'S'
     C                   Eval      Error = *On
     C                   Eval      *In46 = *On
     C                   Leave
     C                   Endif
     C                   Endif
      ***** END ADD ***** 04/28/04 ************************************
      *
      *  Validate file selection
      *
     C                   If        Scpfb <> 'F' and
     C                             Scpfb <> 'P' and
     C                             Scpfb <> 'B'
     C                   Eval      Error = *On
     C                   Eval      *In43 = *On
     C                   Leave
     C                   Endif
      *
     C                   Enddo
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
      * FROM Group Search
      *
     C     HFLD          Ifeq      'SCGRPFR'
     C                   Move      UPCO#         ##CO#
     C                   Call      'W0010R'      FNPL
     C                   Move      ##FND         SCGRPFR
     C                   Endif
      *
      * TO Group Search
      *
     C     Hfld          Ifeq      'SCGRPTO'
     C                   Move      UPCO#         ##CO#
     C                   Call      'W0010R'      FNPL
     C                   Move      ##fnd         SCGRPTO
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
     C                   MOVE      XXSCR#        MBRNAM           10
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
      *
     C     *IN60         IFEQ      *On
     C                   MOVE      *ZERO         UPCO#
     C                   Endif
      *
     C                   Eval      Qqoutq = UPOUTQ
     C                   TIME                    DSDTTM
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
     C                   TIME                    DSDTTM
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
