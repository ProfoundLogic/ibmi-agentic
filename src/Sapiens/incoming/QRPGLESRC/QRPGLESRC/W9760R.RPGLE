     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *****************************************************************
      *                                                               *
      *      W7940R - Prompt for  (P/F/B):EL BUSINESS REPORT          *
      *****************************************************************
      *                                                               *
      *  REVISIONS:                                                   *
      *                                                               *
      *    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
      *    -------------------------------------------------------    *
      *****************************************************************
      *                                                               *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW9760F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FWMFNL     IF   E           K DISK
      *
     FSMCOL     IF   E           K DISK
      *
     FWMUPL     IF   E           K DISK
      *
     FQFEUL     IF   E           K DISK
      *
     FQFE2L     IF   E           K DISK
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D Asof            DS
     D  Scmm                   1      2  0
     D  Sccc                   3      4  0
     D  Scyy                   5      6  0
     D  Scccyy                 3      6  0
      *
     D Asof2           DS
     D  Scc2                   1      1  0
     D  Scyy2                  2      3  0
     D  Scmm2                  4      5  0
      *
     D Wkdate          DS
     D  Wkmth                  1      2  0
     D  Wkday                  3      4  0
     D  Wkyr                   5      8  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D Error           S              1n
     D Datusa          S               D   DATFMT(*USA)
     D Dateto          S               D   DATFMT(*USA)
     D Datefr          S               D   DATFMT(*USA)
     D Fund            S              3  0
     D qqoutq          S             10
      *
      /Eject
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
     C     Qfeul_Key     KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    CYYMM             5 0
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
      /Eject
      **********************************************************************
      * PROCESS SCREEN 01                                                  *
      **********************************************************************
      *
     C                   Dou       *In03 = *On
      *
     C                   Exfmt     Scr01
      *
     C                   Select
     C                   When      *In03 = *On
     C                   Leave
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
     C                   Else
     C                   Exsr      Submit_Job
     C                   Leave
     C                   Endif
      *
     C                   Endsl
      *
     C                   Enddo
      *
      *  END THE PROGRAM
      *
     C                   MOVE      *On           *INLR
      /Eject
      ********************************************************************
      *   Submit Job                                                     *
      ********************************************************************
      *
     C     Submit_Job    Begsr
      *
      *   CALL THE CLP TO SUBMIT THE LIST.
      *
     C                   Move      Upco#         QQco#             3
     C                   Move      Scfnd         QQfnd             3
      *
     C                   Movel     '0101'        QQfrdt            8
     C                   Move      Scccyy        QQfrdt
      *
     C                   Movel     Scasof        Wkmth
     C                   Move      Scasof        Wkyr
     C     *Usa0         Move      Wkdate        Datusa
     C     Datusa        Adddur    1:*Months     Datusa
     C     Datusa        Subdur    1:*Days       Datusa
     C     *Usa0         Move      Datusa        QQtodt            8
     C                   Move      Scasof        QQasof
      *
     C                   Call      'W9760C'
     C                   Parm                    QQco#
     C                   Parm                    QQfnd
     C                   Parm                    QQfrdt            8
     C                   Parm                    QQtodt            8
     C                   Parm                    QQasof            6
     C                   Parm      Scsort        QQsort            1
     C                   Parm      Scpfb         QQtypo            1
     C                   Parm                    QQoutq
     C                   Parm      'D'           Mode              1
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
     C                   If        Scfnd > *Zero
     C                   Eval      Fund = Scfnd
     C     Wmfnl_Key     Chain     Wmfnl
      *
     C                   If        Not %Found
     C                   Eval      Error = *On
     C                   Eval      *In30 = *On
     C                   Endif
      *
     C                   Endif
      *
      *  Validate From Date
      *
     C                   Move      Scasof        Asof
      *
     C                   If        Scmm < 01   or
     C                             Scmm > 12   or
     C                             Scccyy  < 1960 or
     C                             Scccyy  > 2060
     C                   Eval      Error = *On
     C                   Eval      *In31 = *On
     C                   Endif
      *
      *  Date type Issues or Effective
      *
     C                   If        Scsort <> '1' and
     C                             Scsort <> '2' and
     C                             Scsort <> '3'
     C                   Eval      Error = *On
     C                   Eval      *In32 = *On
     C                   Endif
      *
      *  Validate file selection
      *
     C                   If        Scpfb <> 'F' and
     C                             Scpfb <> 'P' and
     C                             Scpfb <> 'B'
     C                   Eval      Error = *On
     C                   Eval      *In33 = *On
     C                   Endif
      *
      *  Validate selected month is in files Qfeup and Qfe2p
      *
     C                   If        Sccc > 19
     C                   Eval      Scc2 = 1
     C                   Else
     C                   Eval      Scc2 = 0
     C                   Endif
      *
     C                   Move      Scmm          Scmm2
     C                   Move      Scyy          Scyy2
     C                   Move      Asof2         CYYMM
      *
     C     Qfeul_Key     Chain     Qfeul
     C                   If        Not %Found(Qfeul)
     C                   Eval      Error = *On
     C                   Eval      *In34 = *On
     C                   Endif
      *
     C     Qfeul_Key     Chain     Qfe2l
     C                   If        Not %Found(Qfe2l)
     C                   Eval      Error = *On
     C                   Eval      *In34 = *On
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
     C                   Endsr
      *
      /Eject
      ********************************************************************
      *   Process Help Key                                               *
      ********************************************************************
      *
     C     Help          Begsr
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
     C     *Inzsr        Begsr
      *
     C                   Time                    Dsdttm
     C                   Eval      Wkday = 01
      *
      *  GET THE USER PARAMETER FILE
      *
     C     Upkey         Chain     Pup01
     C                   If        Not %Found
     C                   Move      *Zero         Upco#
     C                   Endif
      *
     C                   Eval      Qqoutq = Upoutq
     C                   Time                    Dsdttm
      *
      *  GET THE COMPANY RECORD
      *
     C     Cokey         Chain     Pco01
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
      *  GET THE FUND RECORD
      *
     C     FNKEY         CHAIN     PFN01
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
     C                   ENDSR
      *
