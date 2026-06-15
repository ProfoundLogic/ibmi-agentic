     H COPYRIGHT('(C) Copyright Midrange Computer Associates, Inc., 2014')
     H DFTACTGRP(*NO) ACTGRP('HDSOPT') BNDDIR('HYR3504')
      **************************************************************************
      * APPLICATION: Shipping                                                  *
      *       TITLE: Shipping BOL Powder Weight Summary                        *
      *          BY: William Earl (MCA)                                        *
      *        DATE: September 2014                                            *
      *                                                                        *
      *   REV DATE   BY   CHANGE                                               *
      *  ----------  ---  ---------------------------------------------------  *
      *  XX/XX/XXXX  XXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *
      *                                                                        *
      **************************************************************************

     FOEBOLH    IF   E           K DISK
     FHDCUST    IF   E           K DISK
     FHDCTRY    IF   E           K DISK
     FHYLSSCC3  IF   E           K DISK
     FHYLELD1   IF   E           K DISK
     FHYR3504PF O    F   96        PRINTER OFLIND(*INOF) USROPN                 8.5" @ 12 CPI

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR3504         PR                  EXTPGM('HYR3504')
     D  BOL#                          8P 0 CONST
     D  OUTQ                         10A   CONST

     D HSYSBM          PR                  EXTPROC('HSYSBM')                    Retrieve env ovr
     D  Appl                          2A   CONST
     D  User                         10A   CONST
     D  WrkStn                       10A   CONST
     D  Pgm                          10A   CONST
     D  PrintFile                    10A   CONST
     D  ToPrintFile                  10A   CONST
     D  JobName                      10A   CONST
     D  JobDesc                      10A   CONST
     D  JobQueue                     10A   CONST
     D  OutputQueue                  10A   CONST
     D  OvrCmdPart1                 250A
     D  OvrCmtPart2                 250A

     D QCMDEXC         PR                  EXTPGM('QCMDEXC')                    Exec system command
     D  out_Command                 512A   CONST
     D  out_CmdLen                   15P 5 CONST

      **************************************************************************
      * Define *Entry parameter list.                                          *
      **************************************************************************
     D HYR3504         PI
     D  in_BOL#                       8P 0 CONST
     D  in_OUTQ                      10A   CONST

      **************************************************************************
      * Define program status data structure.                                  *
      **************************************************************************
     D ps_            SDS
     D  ps_PgmName             1     10
     D  ps_JobName           244    253
     D  ps_UserID            254    263

      **************************************************************************
      * Define data structures.                                                *
      **************************************************************************
     D                 DS
     D td_TimeDate             1     14  0
     D td_Time                 1      6  0
     D td_Date                 7     14  0

      **************************************************************************
      * Define work fields.                                                    *
      **************************************************************************
     D wk_BOL#         S                   LIKE(SDBOL#)
     D wk_OUTQ         S             10A
     D wk_SysMDYY      S              8P 0
     D wk_SysTime      S              6P 0
     D wk_TotPwdrWgt   S              7P 2
     D wk_TotAmmoCtn   S              5P 0
     D wk_BOL#X        S              8A
     D wk_ShipTo#X     S              8A
     D wk_DropShip#X   S              8A
     D wk_Addr         S             45A
     D wk_CustCSZ      S             45A
     D wk_CustCtry     S                   LIKE(CNCDES)
     D wk_AmmoCDTxt    S             20A
     D wk_Ovr1         S            250A
     D wk_Ovr2         S            250A
     D wk_Ovr          S            500A
     D wk_Ptr1         S              3P 0
     D wk_Ptr2         S              3P 0
     D sv_SRL#         S                   LIKE(SCSRL#) INZ(*Hival)

      **************************************************************************
      * Define constants.                                                      *
      **************************************************************************

     C/EJECT
      **************************************************************************
      * Main routine.                                                          *
      **************************************************************************

     C                   EVAL      wk_BOL# = in_BOL#
     C                   EVAL      wk_OUTQ = in_OUTQ

     C     wk_BOL#       CHAIN     OEBOLH
     C                   IF        not %found(OEBOLH)
     C                   EVAL      SDSHTO = *Zero
     C                   EVAL      SDDSHP = *Zero
     C                   EVAL      SDNAME = *Blanks
     C                   EVAL      SDCITY = *Blanks
     C                   EVAL      SDST = *Blanks
     C                   EVAL      SDZIP = *Blanks
     C                   EVAL      SDCTRY = *Blanks
     C                   ENDIF                                                  not %found(OEBOLH)

     C                   IF        SDDSHP <> *Zero
     C     SDSHTO        CHAIN     HDCUST
     C                   IF        not %found(HDCUST)
     C                   EVAL      CMCNA1 = *Blanks
     C                   EVAL      CMCCTY = *Blanks
     C                   EVAL      CMST = *Blanks
     C                   EVAL      CMZIP = *Blanks
     C                   EVAL      CMCTRY = *Blanks
     C                   ENDIF                                                  not %found(HDCUST)
     C                   IF        CMST = *Blanks
     C                   EVAL      wk_CustCSZ = CMCCTY + '  ' + CMZIP
     C                   ELSE
     C                   EVAL      wk_CustCSZ = %trimr(CMCCTY) + ', ' + CMST +
     C                               '  ' + CMZIP
     C                   ENDIF                                                  SDST=*Blanks
     C                   IF        CMCTRY <> 'US'
     C     CMCTRY        CHAIN     HDCTRY
     C                   IF        not %found(HDCTRY)
     C                   EVAL      CNCDES = CMCTRY
     C                   ENDIF                                                  not %found(HDCTRY)
     C                   EVAL      wk_CustCtry = CNCDES
     C                   ELSE
     C                   EVAL      wk_CustCtry = *Blanks
     C                   ENDIF                                                  SDCTRY<>'US'
     C                   EVAL      *IN81 = *ON
     C                   ELSE
     C                   EVAL      *IN81 = *OFF
     C                   ENDIF                                                  SDDSHP<>*Zero

     C     wk_BOL#       SETLL     HYLSSCC3
     C                   DOU       %eof(HYLSSCC3)
     C     wk_BOL#       READE     HYLSSCC3
     C                   IF        %eof(HYLSSCC3)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSSCC3)
     C                   IF        SCSRL# = sv_SRL#
     C                   ITER
     C                   ENDIF                                                  SCSRL#=sv_SRL#
     C                   EVAL      sv_SRL# = SCSRL#
     C     SCSRL#        CHAIN     HYLELD1
     C                   IF        %found(HYLELD1)
     C                   EVAL      wk_TotPwdrWgt = wk_TotPwdrWgt + EDPWGT
     C                   EVAL      wk_TotAmmoCtn = wk_TotAmmoCtn + 1
     C                   ENDIF                                                  %found(HYLELD1)
     C                   ENDDO                                                  %eof(HYLSSCC3)

     C                   EXSR      $Print

     C                   EVAL      *INLR = *ON
     C                   RETURN

     C/EJECT
      **************************************************************************
      * Print the report.                                                      *
      **************************************************************************
     CSR   $Print        BEGSR

     C                   EXSR      $OpenPrtF
     C                   IF        %error
     C                   LEAVESR
     C                   ENDIF                                                  %error

     C                   EVAL      *INOF = *ON

     C                   EVAL      wk_BOL#X = %triml(%editc(wk_BOL#:'Z'))

     C                   EVAL      wk_ShipTo#X =
     C                               '#' + %triml(%editc(SDSHTO:'Z'))

     C                   IF        SDDSHP <> *Zero
     C                   EVAL      wk_DropShip#X =
     C                               '#' + %triml(%editc(SDDSHP:'Z'))
     C                   EVAL      *IN81 = *ON
     C                   ELSE
     C                   EVAL      wk_DropShip#X = *Blanks
     C                   EVAL      *IN81 = *OFF
     C                   ENDIF                                                  SDDSHP<>*Zero

     C                   EXCEPT    Prt01a
     C                   IF        SDDSHP <> *Zero  and  wk_CustCtry <> *Blanks
     C                   EXCEPT    Prt01b
     C                   ENDIF                                                  SDDSHP<>*Zero and
     C                   IF        SDDSHP <> *Zero
     C                   EXCEPT    Prt01c
     C                   ENDIF                                                  SDDSHP<>*Zero
     C                   IF        SDADR1 <> *Blanks
     C                   EVAL      wk_Addr = SDADR1
     C                   EXCEPT    Prt01d
     C                   ENDIF                                                  SDADR1<>*Blanks
     C                   IF        SDADR2 <> *Blanks
     C                   EVAL      wk_Addr = SDADR2
     C                   EXCEPT    Prt01d
     C                   ENDIF                                                  SDADR2<>*Blanks
     C                   IF        SDADR3 <> *Blanks
     C                   EVAL      wk_Addr = SDADR3
     C                   EXCEPT    Prt01d
     C                   ENDIF                                                  SDADR3<>*Blanks
     C                   IF        SDST = *Blanks
     C                   EVAL      wk_Addr = SDCITY + '  ' + SDZIP
     C                   ELSE
     C                   EVAL      wk_Addr = %trimr(SDCITY) + ', ' + SDST +
     C                               '  ' + SDZIP
     C                   ENDIF                                                  SDST=*Blanks
     C                   EXCEPT    Prt01d
     C                   IF        SDCTRY <> 'US'
     C     SDCTRY        CHAIN     HDCTRY
     C                   IF        not %found(HDCTRY)
     C                   EVAL      CNCDES = SDCTRY
     C                   ENDIF                                                  not %found(HDCTRY)
     C                   EVAL      wk_Addr = CNCDES
     C                   EXCEPT    Prt01d
     C                   ENDIF                                                  SDCTRY<>'US'
     C                   EVAL      wk_Addr = *Blanks
     C                   EXCEPT    Prt01d
     C                   EXCEPT    Prt01d

     C                   EVAL      wk_AmmoCDTxt = 'NEW ' +
     C                               %triml(%editc(wk_TotPwdrWgt:'L'))

     C                   EXCEPT    Prt02

     CSR                 ENDSR

     C/EJECT
      **************************************************************************
      * Open print file.                                                       *
      **************************************************************************
     CSR   $OpenPrtF     BegSR

     C                   IF        wk_OutQ = '*ENV'

     C                   CALLP(E)  HSYSBM ('OE':ps_UserID:ps_JobName:'HOEBOL':
     C                               'HYR3504PF':'HOEBOLPF':*Blanks:*Blanks:
     C                               *Blanks:*Blanks:wk_Ovr1:wk_Ovr2)

     C                   EVAL      wk_Ovr = wk_Ovr1 + wk_Ovr2

     C                   EVAL      wk_Ptr1 = %scan(' TOFILE(':wk_Ovr)
     C                   IF        wk_Ptr1 > *Zero
     C                   EVAL      wk_Ptr2 = %scan(') ':wk_Ovr:wk_Ptr1)
     C                   EVAL      wk_Ovr = %subst(wk_Ovr:1:wk_Ptr1+7) +
     C                               'QSYSPRT' + %subst(wk_Ovr:wk_Ptr2)
     C                   ENDIF                                                  wk_Ptr1>*Zero

     C                   EVAL      wk_Ptr1 = %scan(' DEVTYPE(':wk_Ovr)
     C                   IF        wk_Ptr1 > *Zero
     C                   EVAL      wk_Ptr2 = %scan(') ':wk_Ovr:wk_Ptr1)
     C                   EVAL      wk_Ovr = %subst(wk_Ovr:1:wk_Ptr1-1) +
     C                               %subst(wk_Ovr:wk_Ptr2+1)
     C                   ENDIF                                                  wk_Ptr1>*Zero
     C                   EVAL      wk_Ovr = %trimr(wk_Ovr) +
     C                               ' DEVTYPE(*SCS)'

     C                   EVAL      wk_Ptr1 = %scan(' PAGESIZE(':wk_Ovr)
     C                   IF        wk_Ptr1 > *Zero
     C                   EVAL      wk_Ptr2 = %scan(') ':wk_Ovr:wk_Ptr1)
     C                   EVAL      wk_Ovr = %subst(wk_Ovr:1:wk_Ptr1-1) +
     C                               %subst(wk_Ovr:wk_Ptr2+1)
     C                   ENDIF                                                  wk_Ptr1>*Zero
     C                   EVAL      wk_Ovr = %trimr(wk_Ovr) +
     C                               ' PAGESIZE(66 96)'

     C                   EVAL      wk_Ptr1 = %scan(' LPI(':wk_Ovr)
     C                   IF        wk_Ptr1 > *Zero
     C                   EVAL      wk_Ptr2 = %scan(') ':wk_Ovr:wk_Ptr1)
     C                   EVAL      wk_Ovr = %subst(wk_Ovr:1:wk_Ptr1-1) +
     C                               %subst(wk_Ovr:wk_Ptr2+1)
     C                   ENDIF                                                  wk_Ptr1>*Zero
     C                   EVAL      wk_Ovr = %trimr(wk_Ovr) +
     C                               ' LPI(6)'

     C                   EVAL      wk_Ptr1 = %scan(' CPI(':wk_Ovr)
     C                   IF        wk_Ptr1 > *Zero
     C                   EVAL      wk_Ptr2 = %scan(') ':wk_Ovr:wk_Ptr1)
     C                   EVAL      wk_Ovr = %subst(wk_Ovr:1:wk_Ptr1-1) +
     C                               %subst(wk_Ovr:wk_Ptr2+1)
     C                   ENDIF                                                  wk_Ptr1>*Zero
     C                   EVAL      wk_Ovr = %trimr(wk_Ovr) +
     C                               ' CPI(12)'

     C                   EVAL      wk_Ptr1 = %scan(' OVRFLW(':wk_Ovr)
     C                   IF        wk_Ptr1 > *Zero
     C                   EVAL      wk_Ptr2 = %scan(') ':wk_Ovr:wk_Ptr1)
     C                   EVAL      wk_Ovr = %subst(wk_Ovr:1:wk_Ptr1-1) +
     C                               %subst(wk_Ovr:wk_Ptr2+1)
     C                   ENDIF                                                  wk_Ptr1>*Zero
     C                   EVAL      wk_Ovr = %trimr(wk_Ovr) +
     C                               ' OVRFLW(60)'

     C                   EVAL      wk_Ptr1 = %scan(' FRONTOVL(':wk_Ovr)
     C                   IF        wk_Ptr1 > *Zero
     C                   EVAL      wk_Ptr2 = %scan(') ':wk_Ovr:wk_Ptr1)
     C                   EVAL      wk_Ovr = %subst(wk_Ovr:1:wk_Ptr1-1) +
     C                               %subst(wk_Ovr:wk_Ptr2+1)
     C                   ENDIF                                                  wk_Ptr1>*Zero

     C                   EVAL      wk_Ptr1 = %scan(' USRDTA(':wk_Ovr)
     C                   IF        wk_Ptr1 > *Zero
     C                   EVAL      wk_Ptr2 = %scan(') ':wk_Ovr:wk_Ptr1)
     C                   EVAL      wk_Ovr = %subst(wk_Ovr:1:wk_Ptr1-1) +
     C                               %subst(wk_Ovr:wk_Ptr2+1)
     C                   ENDIF                                                  wk_Ptr1>*Zero
     C                   EVAL      wk_Ovr = %trimr(wk_Ovr) +
     C                               ' USRDTA(' + %trimr(ps_PgmName) + ')'

     C                   EVAL      wk_Ptr1 = %scan(' SPLFNAME(':wk_Ovr)
     C                   IF        wk_Ptr1 > *Zero
     C                   EVAL      wk_Ptr2 = %scan(') ':wk_Ovr:wk_Ptr1)
     C                   EVAL      wk_Ovr = %subst(wk_Ovr:1:wk_Ptr1-1) +
     C                               %subst(wk_Ovr:wk_Ptr2+1)
     C                   ENDIF                                                  wk_Ptr1>*Zero
     C                   EVAL      wk_Ovr = %trimr(wk_Ovr) +
     C                               ' SPLFNAME(HYR3504PF)'

     C                   CALLP(E)  QCMDEXC (wk_Ovr:500)

     C                   ENDIF                                                  wk_OutQ='*ENV'

     C                   OPEN(E)   HYR3504PF

     CSR                 EndSR

     C/EJECT
      **************************************************************************
      * Program initialization.                                                *
      **************************************************************************
     CSR   *InzSR        BEGSR

      *
      * Retrieve system date and time for report heading.
      *
     C                   EVAL      wk_SysMDYY = %dec(%date:*USA)
     C                   EVAL      wk_SysTime = %dec(%time:*HMS)

     CSR                 ENDSR

     O/EJECT
     OHYR3504PF H    OF                     2 02
     O                       wk_SysMDYY          10 '  /  /    '
     O                       wk_SysTime          20 '0 :  :  '
     O                                           57 'BOL POWDER WEIGHT SUMMARY'
     O                                           64 'REPORT'
     O                                           79 'HYR3504'
     O                                           91 'PAGE'
     O                       PAGE          Z     95
     O          EF           Prt01a         2
     O                                            8 'BOL no.:'
     O                       wk_BOL#X            18                             8A
     O          E            Prt01a         1
     O                                            8 'Ship to:'
     O                       CMCNA1              37                             26A
     O                       wk_ShipTo#X         47                             #9999999
     O          E    81      Prt01a         1
     O                       wk_CustCSZ          56                             45A
     O          E    81      Prt01b         1
     O                       wk_CustCtry         41                             30A
     O          E    81      Prt01c      1  1
     O                                           10 'Drop ship:'
     O                       SDNAME              41                             30A
     O                       wk_DropShip#X       51                             #9999999
     O          E    81      Prt01d         1
     O                       wk_Addr             56                             45A
     O          E            Prt02          3
     O                                           25 'Total BOL ammo cartons:'
     O                       wk_TotAmmoCtn J     32                             xx,xxx-
     O                                           62 'Total powder weight:'
     O                       wk_TotPwdrWgt J     72                             xx,xxx.xx-
     O          E            Prt02          0
     O                                           22 'SHIPPING INSTRUCTIONS:'
     O          E            Prt02          2
     O                                           22 'SHIPPING INSTRUCTIONS:'
     O          E            Prt02          0
     O                       wk_AmmoCDTxt        92                             20A
     O          E            Prt02          2
     O                                           27 '1) Write the following on'
     O                                           47 'the BOL in the ammo'
     O                                           70 'commodity description:'
     O                       wk_AmmoCDTxt        92                             20A
     O          E            Prt02          2
     O                                           27 '2) Affix a CHEMTREC phone'
     O                                           51 'label to the top of the'
     O                                           72 '1st page of the BOL.'

     O/EJECT
