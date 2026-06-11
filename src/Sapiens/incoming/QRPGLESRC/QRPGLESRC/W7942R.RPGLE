     H* (C) Copyright Progressive Data Solutions, Inc., 2005          *
      *                                                               *
      * W7942R - Prompt for Renewal Tracking Reports                  *
      * ============================================================= *
      * PROGRAM DESCRIPTION:                                          *
      *                                                               *
      * This program prompts for parmeters to be submited for the     *
      * Policy Expiration Date Report.                                *
      *                                                               *
      * CALLED FROM:                                                  *
      *   MENU                                                        *
      * CALLS TO:                                                     *
      *   W7943C - Calls W7943R                                       *
      *                                                               *
      * CREATED: 08/21/03     C: 132    PROJECT: 129WAM               *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      *  03/16/05 C:53 PROJECT: 129WJW/WC1277
      *           MOVE TO BASE.
      *                                                               *
      *****************************************************************
      *                                                               *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW7942F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Fund master file
     FWMFNL     IF   E           K DISK
     F                                     infds(FNfds)
      *
      *  User Parm file
     FWMUPL     IF   E           K DISK
      *                                    RENAME(PUP01:PUPL1)
     FSMCOL     IF   E           K DISK
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D Fids01        E DS                  extname(szq2p)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D Datusa          S               D   DATFMT(*USA)
     D Fund            S              3  0
     D #1outq          S             10
     D #1prds          S             40    INZ
     D #1co#           S              3    inz
     D #1ptdt          S              8    inz
     D #1pfdt          S              8    inz
     D Srdfld          S             80
     D Srdlen          S              3  0
     D Srdti           S              8  0
     D Srdto           S             10
     D Srdate          S             10
     D Srerr           S              1
     D Srffmt          S              8
     D Srtfmt          S              8
     D @@kc            s              1
     D @@kl            s              1
      *
     D Error           S              1N
      *
     D Title           C                   CONST('Renewal Tracking Reports')
      *
     D fnfds           ds
     D  fnrrn                397    400b 0
      *
      *
      *
      *=================================================================
      * KLIST
      *=================================================================
      *
      *  COMPANY
      *
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
      *
      *  USER PARAMETER
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
    C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
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
     C                   Parm      *Blanks       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr
      *
      *
      *
      * ================================================================== *
      * PROCESS SCREEN 01                                                  *
      * ================================================================== *
      *
     C                   Dou       *In03 = *On or
     C                             *In12 = *On
      *
     C                   Exfmt     Scr01
      *
     C                   Select
      *
     C                   When      *In03 = *On or
     C                             *In12 = *On
     C                   Leave
      *
      *  Search
      *
     C                   When      *Inkj = *On
     C                   Exsr      kjdo
     c                   Iter
      *
      *  Change of User Parameters
      *
     C                   When      *IN13 = *On
     C                   Exsr      Chgprms
     C                   Iter
      *
     C                   Other
     C                   Eval      Error = *Off
     C                   Exsr      ERRCK1
      *
     C                   If        Error
     C                   Iter
      *
     C                   Else
      *
     C                   Exsr      Submit_Job
     C                   Leave
     C                   Endif
      *
     C                   Endsl
      *
     C                   Enddo
      *
      *  END THE PROGRAM
     C                   Move      *On           *Inlr
      *
      *
      *
      * ================================================================== *
      *   Submit Job                                                     *
      * ================================================================== *
      *
     C     Submit_Job    Begsr
      *
      *   CALL THE CLP TO SUBMIT THE LIST.
      *
     C                   Move      Upco#         #1co#
      *
     C                   If        Scpfdt <> *Zero
     C                   Move      Scpfdt        #1pfdt
     C                   Endif
      *
     C                   If        Scptdt <> *Zero
     C                   Move      Scptdt        #1ptdt
     C                   Endif
      *
     C                   Call      'W7942C'
     C                   Parm                    #1co#             3
     C                   Parm                    #1pfdt            8
     C                   Parm                    #1ptdt            8
     C                   Parm                    #1outq           10
     C
     C                   Move      *on           *inkc
     C
     C                   Endsr
      /Eject
      * ================================================================ *
      *   Validate Fields on Screen 1                                    *
      * ================================================================ *
      *
     C     ERRCK1        Begsr
      *
      *  Validate From Date
      *
     C                   If        Scpfdt = *Zeros
     C                             and Error = *Off
     C                   Eval      *In21 = *On
     C                   Eval      Error = *On
     C                   Endif
      *
     C                   If        Scpfdt > *Zeros
     C                             and Error = *Off
     C     *Usa          Test(de)                Scpfdt
     C                   If        %Error
     C     *Mdy          Test(de)                Scpfdt
      *
     C                   If        %Error
     C                   Eval      *In22 = *On
     C                   Eval      Error = *On
     C                   Endif
     C                   Endif
     C                   Endif
      *
      *  Validate To Date
      *
     C                   If        Scptdt > *Zeros
     C                             and Error = *Off
     C     *Usa          Test(de)                Scptdt
      *
     C                   If        %Error
     C     *Mdy          Test(de)                Scptdt
      *
     C                   If        %Error
     C                   Eval      *In23 = *On
     C                   Eval      Error = *On
     C                   Endif
      *
     C                   Endif
      *
     C                   Endif
      *
      *  If to Date zero is error
     C                   If        Scptdt = *Zeros
     C                             and Error = *Off
     C                   Eval      *In25 = *On
     C                   Eval      Error = *On
     C                   Endif
      *
      *  Edit dates for in ascending order
      *
     C                   If        Error = *Off
     C                   z-add     scpfdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scpfdt
      *
     C                   z-add     scpfdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxpfdt            7 0
      *
     C                   z-add     scptdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scptdt
      *
     C                   z-add     scptdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxptdt            7 0
      *
     C                   If            (xxpfdt >= xxptdt)
     C                   Eval      Error = *On
     C                   Eval      *In24 = *On
     C                   Endif
      *
     C                   Endif
      *
     C                   Endsr
      *
      *
      * ================================================================ *
      *   Change User parameters                                         *
      * ================================================================ *
      *
     C     Chgprms       Begsr
      *
     C                   Call      'W0006R'
      *
      *  GET THE USER PARAMETER FILE
      *
     C     Upkey         Chain     Pup01                              60
      *
     C                   If        *In60
     C                   Move      *Zero         Upco#
     C                   Endif
      *
     C                   Eval      #1outq = Upoutq
     C                   Time                    Dsdttm
      *
      *  GET THE COMPANY RECORD
      *
     C     Cokey         Chain     Pco01                              60
     C                   Movel(P)  Coname        Srdfld
     C                   Call      'SRCENTER'
     C                   Parm                    Srdfld           80
     C                   Parm      40            Srdlen            3 0
     C                   Movel     Srdfld        Scconm
      *
     C                   Time                    Dsdttm
     C                   Movel(p)  Fnname        Scconm
      *
     C                   Endsr
      *
      *
      *
      * ================================================================ *
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      * ================================================================ *
      *
     C     *Inzsr        Begsr
      *
     C                   Time                    Dsdttm
      *
      *  Get the User Parameter file
      *
     C     Upkey         Chain     Pup01                              60
     C     *In60         Ifeq      *On
     C                   Move      *Zero         Upco#
     C                   Endif
     C                   Eval      #1outq = Upoutq
     C                   Time                    Dsdttm
      *
      *  Get the Company Record
      *
     C     Cokey         Chain     Pco01                              60
     C                   Movel(p)  Coname        Srdfld
     C                   Call      'SRCENTER'
     C                   Parm                    Srdfld           80
     C                   Parm      40            Srdlen            3 0
     C                   Movel     Srdfld        Scconm
      *
      *  Get the Fund Record
      *
     C                   Move      Q1user        Scuser
     C                   Movel     Title         #1prds
     C                   Move      #1prds        Scprds
      *
      * Center Screen Prompt in display field
      *
     C                   Movel(p)  Scprds        ##data
     C                   Call      'SRCENTER'                           99
     C                   Parm                    ##data           80
     C                   Parm      40            ##leng            3 0
     C                   Movel(p)  ##data        Scprds
      *
     C                   Endsr
      *
      *
      *
      * ================================================================ *
      *  KJDO
      * ================================================================ *
     C     KJDO          Begsr
      *
     C                   Endsr
      *
      * ================================================================ *
