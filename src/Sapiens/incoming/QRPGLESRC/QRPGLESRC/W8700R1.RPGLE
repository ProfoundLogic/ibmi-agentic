     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *****************************************************************
      *  DESCRIPTION:  Prompt Screen for Date to Submit               *
      *                Quote Transfer                                 *
      *                                                               *
      *  CALLS TO: W8700C1                                            *
      *                                                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*  20  INVALID FROM DATE                                        *
      *****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *  04/01/13 C:282 PROJECT: 147WAK                               *
      *                 To enhance quote transfer process to process  *
      *                 a single employer.                            *
      *                                                               *
      *  04/06/05 C:132 PROJECT: BASECD                               *
      *                 Install in WCMS BASE                          *
      *                                                               *
      *  06/25/03 C:136 PROJECT: 129WBM                               *
      *                 Add parm for user's outq.                     *
      *                                                               *
      *  10/05/04 C:09  Project: 132WBY                               *
      *                 Implement quote transfer on demand.           *
      * ------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
      * ------------------------------------------------------------- *
      *****************************************************************
      /eject
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW8700F1   cf   e             Workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Company file used to get company name
     FSMCOL     If   E           K Disk
      *
      *  User file used to get company number.
     FWMUPL     If   E           K Disk
      *
      /eject
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D Srdfld          S             80
     D Srdlen          S              3  0
     D Today           S               D   DATFMT(*USA)
     D Datusa          S               D   DATFMT(*USA)
     D DatMdy          S               D   DATFMT(*MDY)
     D Timestmp        S             12  0
      *
     D ##Data          S             80
     D ##Leng          S              3  0
     D Prmco           S              3
     D PrmDate         S              8
     D PrmOutq         S             10
      ***  Beg Add  ***  04/01/13  **************************************
     d charFnd         s              3
     d charEmp#        s              9
      ***  End Add  ***  04/01/13  **************************************
      *
     D Optnm           C                   'Request a Quote Transfer'
      *
     *---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     *
     D psds          ESDS                  extname(szq1p)
      ***  Beg Add  ***  04/01/13  **************************************
      *Entry
     dW8700R1          pr                  ExtPgm('W8700R1')
     d @@Fnd                          3
     d @@Emp#                         9
     dW8700R1          pi
     d @@Fnd                          3
     d @@Emp#                         9
      ***  End Add  ***  04/01/13  **************************************

     *
     ****************************************************************
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             800
     C                   PARM      *BLANK        SRFFMT            8
     C                   PARM      *BLANK        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANK        SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     *
     C     C1Plist       Plist
     C                   Parm                    PrmCo
     C                   Parm                    PrmDate
     C                   Parm                    PrmOutq
      ***  Beg Add  ***  04/01/13  **************************************
     C                   Parm                    charFnd
     C                   Parm                    charEmp#
      ***  End Add  ***  04/01/13  **************************************
     *
     C*PUIMOD
     C*                  Move      Udate         Today
     C                   EVAL      Today=%Date()
      /eject
      **********************************************************************
      *   M A I N L I N E   R O U T I N E                                  *
      **********************************************************************
B01  C                   Dou       *In03 = *On
      *
     C                   Exfmt     Scr01
      *
     C                   If        *In03 = *On
     C                   Leave
     C                   Endif
      *
     C                   Eval      *In20 = *off
      *
     C                   Z-ADD     Scrundt       SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCrundt
     C                   MOVEL     SRDTO         prmdate
      *
     C                   IF        SRERR = *ON
     C                   MOVE      *ON           *IN20
     C                   ITER
     C                   ENDIF
      *
      ***  Beg Add  ***  04/01/13  **************************************
      /Free
       charFnd = @@Fnd;
       charEmp#= @@Emp#;
      /end-Free
      ***  Beg End  ***  04/01/13  **************************************
     C                   Call      'W8700C1'     C1Plist
     C                   Leave
      *
     C                   Enddo
     *
     C                   Eval      *Inlr = *On
      /eject
     **********************************************************************
     *                                                                *****
     *  *INZSR   INITIAL SUBROUTINE
     *                                                                *****
     **********************************************************************
      *
     C     *Inzsr        Begsr
      *
     C                   Time                    Timestmp
     C     *Mdy          Move      Timestmp      Today
     C     *Usa          Move      Today         ScRunDt
      *
     C     Upky          Chain     Pup01                              93
      *
     C                   If        *in93 = *on
     C                   Move      *Zero         Upco#
     C                   Endif
      *
     C                   Move      UpCo#         PrmCo
     c                   eval      prmOutq = UPOUTQ
      *
      *  Get the company record
      *
     C     Coky          Chain     SMCOL                              94
      *
     C                   If        *in94 = *off
     C                   Movel(P)  Coname        Srdfld
     C                   CALL      'SRCENTER'
     C                   Parm                    Srdfld
     C                   Parm      40            Srdlen
     C                   Movel     Srdfld        Scconm
      *
     C                   Endif
      *
     C                   Movel     Optnm         ##Data
     C                   call      'SRCENTER'
     C                   Parm                    ##Data
     C                   Parm      40            ##Leng
     C                   Movel     ##Data        Scprds
      *
      *================================================================
      *  Klist definitions
      *================================================================
      *
      *
      *  Company file key
     C
     C     Coky          Klist
     C                   Kfld                    Upco#
      *
      *  User parameter file
      *
     C     Upky          Klist
     C                   Kfld                    Q1user
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
      *
     C                   Endsr
