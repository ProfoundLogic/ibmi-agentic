      * (C) Copyright Progressive Data Solutions, Inc., 2001
      *
     H/TITLE W8126R4 - Build A/R Extracts
     H option(*nodebugio:*srcstmt)
      *****************************************************************
      * Created: 10/24/01  C:61  Project WC970                        *
      *****************************************************************
      * Revisions:                                                    *
      *                                                               *
      * 04/27/06 C:136 SUPPORT: WA109 51891                           *
      *                ADD PROCESSING FOR THE NEW VOID A/R TYPE       *
      *                                                               *
      * 10/24/02  C:142  PROJECT: WC970 Supp:34460                    *
      *                  Add Agdefr to defr so will balance for Clrdn *
      *****************************************************************
     H DATFMT(*USA)
      *
      *  A/R Detail Work File  (co/fnd/emp#/fyr/st)
     FWXX1L1    IF   E           K DISK
      *
      *  Employer Master
     FWMEML     IF   E           K DISK
      *
      *  Employer Policy Year
     FWDELL1    IF   E           K DISK
     *
     * Employer Detail File
     FWdell2    IF   E           K Disk
     F                                     Rename(Pel01:Wdelr2)
     F                                     Prefix(L2:2)
      *
      * Carrier/State Reporting Master
     FWMRPL4    IF   E           K DISK
      *
      *  Group Carrier Info
     FWMGCL     IF   E           K DISK
      *
      *  A/R Transaction Type Code Table
     FWTTTL1    IF   E           K DISK
     *
     * Rate Set Code Master File
     FWmrsl     IF   E           K Disk
      *
      *  County Code Table
     FWTCNL1    IF   E           K DISK
      *
      *  Policy Billing Transactions
     FBILLP     O    E             DISK
      *
      *  Policy Cash Transactions
     FCASHP     O    E             DISK
      *
      *
      *
     D/copy sprdate
     D Editzip         c                    '     -    '
     D Editdate        c                    '  /  /    '
     D Today           s               D   DATFMT(*USA)
     D Datusa          S               D   DATFMT(*USA)
     D LastDay         S               D   DATFMT(*USA)
     D day             s              3  0
     D date            s               d
     D xxtdt           s              7s 0
     D Hold20          S             20  2
     D First           S              1n
      *
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *
      *=================================================================
      *  KLIST
      *=================================================================
      *
      *  Group Carrier Info
      *
     C     GCKY          KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    X1FND
      *
      * Table
      *
     C     TBKY1         KLIST
     C                   KFLD                    x1co#
     C                   KFLD                    XXKEY            10
      *
      *  Employer Master
      *
     C     EMKY          KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    X1FND
     C                   KFLD                    X1EMP#
      *
      *  Employer Policy Year
      *
     C     elky          KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    X1FND
     C                   KFLD                    X1FYR
     C                   KFLD                    X1EMP#
     C                   KFLD                    SVDIV
      *
     C     elky1         KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    X1FND
     C                   KFLD                    X1FYR
     C                   KFLD                    X1EMP#
     C                   KFLD                    X1DIV
      *
      *  Carrier/State Reporting Master
      *
     C     RPKY          KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    X1FND
     C                   KFLD                    X1FYR
     C                   KFLD                    X1EMP#
     C                   KFLD                    SVDIV
      *
     C     Wdell2_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    X1Fnd
     C                   Kfld                    X1Emp#
      *
     C     Wmrsl_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Kyrtcd
      *
      *=================================================================
      *  *LIKE
      *=================================================================
     C     *LIKE         DEFINE    X1CO#         SVCO#
     C     *LIKE         DEFINE    X1FND         SVFND
     C     *LIKE         DEFINE    X1EMP#        SVEMP#
     C     *LIKE         DEFINE    X1FYR         SVFYR
     C     *LIKE         DEFINE    X1DIV         SVDIV
     C     *LIKE         DEFINE    ELPFDT        SVPFDT
     C     *LIKE         DEFINE    RPCAP#        SVCAP#
     C     *LIKE         DEFINE    X1CO#         KKCO#
     C     *Like         Define    ElFyr         FirstFyr
     C     *Like         Define    Elpfdt        FirstEfd
     C     *Like         Define    ElFyr         Fyear
     C     *Like         Define    RsJrst        Risk_St
     C     *Like         Define    Elrtcd        Kyrtcd
     C     *Like         Define    X1co#         Prmco#
      *
      /eject
      *=================================================================
      *  PLIST
      *=================================================================
      /eject
      *=================================================================
      * M A I N L I N E   R O U T I N E
      *=================================================================
      *
     C                   Dou       %Eof(Wxx1l1)
      *
     C                   Read      Wxx1l1
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   Eval      Fyear = X1fyr
     C                   Eval      Risk_St = X1st
      *
     C                   Exsr      Get_Srd
     C                   Exsr      Get_Pol
     C                   Exsr      Eff_Date_St
      *
      * Write to Output files
      *
     C                   EXSR      WRPOLM
     C                   EXSR      WRBILL
     C                   EXSR      WRCASH
      *
     C                   ENDDO
      *
      * Write Gross Collected
      *
     C                   CALL      'W8126R6'
     C                   PARM      Prmco         ##CO#             3
     C                   PARM      @@TODT        ##TODT            8
      *
     C                   MOVE      *ON           *INLR
      *
      *
      /eject
      *==============================================================
      * Get State Reporting Division
      *==============================================================
     C     Get_Srd       BEGSR
      *
      *  Get Employer State Reporting Division
      *
     C                   Z-ADD     *ZERO         SVDIV
     C     EMKY          CHAIN     PEM01                              61
      *
     C     *IN61         DOWEQ     *OFF
      *
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         SVDIV
     C                   LEAVE
     C                   ENDIF
      *
     C     EMKY          READE     PEM01                                  61
     C                   ENDDO                                                  EMKY
      *
     C                   ENDSR
      *
      /eject
      *==============================================================
      * Get Policy Number
      *==============================================================
     C     Get_Pol       BEGSR
      *
      ****** Befin Delete 08/24/02 *****************************************
     C*                  CLEAR                   SVPFDT
     C*    ELKY          CHAIN     PEL01
      *
     C*                  If        %Found
     C*                  Z-ADD     ELPFDT        SVPFDT
      ****** Befin Delete 08/24/02 *****************************************
      *
     C     RPKY          CHAIN     PRP01
      *
     C                   IF        %Found and
     C                             RPCAP# <> *BLANKS
     C                   MOVEL(P)  RPCAP#        SVCAP#
      *
     C                   ELSE
      *
     C                   MOVEL(P)  X1FND         SVCAP#
     C                   MOVEL(P)  X1EMP#        XXTMP            10
     C                   CAT(P)    XXTMP:0       SVCAP#
      **** BEGIN CHG **** 04/19/02 *****************************************************************
     C****************** MOVEL(P)  SVFYR         XXTMP            10
     C                   MOVEL(P)  X1FYR         XXTMP            10
      **** END   CHG **** 04/19/02 *****************************************************************
     C                   CAT(P)    XXTMP:0       SVCAP#
     C                   ENDIF                                                  RPKY
      *
     C*                  ENDIF                                                  ELKY
      *
     C                   ENDSR
      *
      /eject
     **********************************************************************
     *  Get Earliest Effective Date for State /Year
     **********************************************************************
      *
     C     Eff_Date_St   Begsr
      *
     C                   Clear                   FirstFyr
     C                   Clear                   FirstEfd
      *
     C     Wdell2_Key    Setll     Wdell2
      *
     C                   Dou       %eof(Wdell2)
      *
     C     Wdell2_Key    Reade     Wdell2
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        L2Fyr <> Fyear
     C                   Iter
     C                   Endif
      *
     C                   Eval      Kyrtcd = L2rtcd
     C     Wmrsl_Key     Chain     Wmrsl
      *
     C                   If        %Found
      *
     C                   If        RsJrst = Risk_St
      *
     C                   If        L2pfdt < FirstEfd or
     C                             FirstEfd = *Zero
     C                   Eval      FirstEfd = L2Pfdt
     C                   Endif
      *
     C                   Endif
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr
      /eject
      *==============================================================
      * WRPOLM - Write to Policy Master File
      *==============================================================
     C     WRPOLM        BEGSR
      *
     C                   MOVEL(P)  X1FND         ##POL
     C                   MOVEL(P)  X1EMP#        XXTMP            10
     C                   CAT(P)    XXTMP:0       ##POL
     C                   MOVEL(P)  SVFYR         XXTMP            10
     C                   CAT(P)    XXTMP:0       ##POL
      *
     C                   Eval      ##Div = X1Div
      *
     C                   CALL      'W8126R2'
     C                   PARM      Prmco         ##CO#             3
     C                   PARM      @@TODT        ##TODT            8
     C                   PARM                    ##POL            15
     C                   PARM                    ##Div             5 0
      *
     C                   ENDSR                                                  WRPOLM
      *
      *
      *
      /eject
      *==============================================================
      * WRBILL - Write to Policy Billing Transaction File
      *==============================================================
     C     WRBILL        BEGSR
      *
     c     x1arty        ifeq      'B'
     c     x1arty        oreq      'A'
     c     x1arty        oreq      'D'
     c     x1arty        oreq      'F'
     c     x1arty        oreq      'H'
      *
     C                   CLEAR                   PLL01
      *
     C     GCKY          CHAIN     PGC01                              60
     C  N60              MOVE      GCPGID        LLPGID
      *
     C                   MOVEL(P)  SVCAP#        LLCAP#
     C                   MOVE      '11'          LLBTYP
      *
     C     X1DATE        IFNE      *ZERO
     C     *YMD          MOVE      X1DATE        DATUSA
     C                   MOVE      DATUSA        LLBIDT
     C                   ENDIF
      *
     C                   eval      Hold20 = x1amt
     C                   eval      llamt = %editc(Hold20:'P')
     C                   eval      llamt# = Hold20
      *
     C     X1PLAN        IFEQ      'A'
     C     x1amt         SUB       X1CAMT        xxtmp2           20 2
     C                   eval      llbamt = %editc(xxtmp2:'P')
     C                   eval      llbamt# = xxtmp2
      *
     c                   else
      *
     C                   eval      Hold20 = x1amt
     C                   eval      llbamt = %editc(Hold20:'P')
     C                   eval      llbamt# = Hold20
     C                   ENDIF
      *
      *
     C     X1PLAN        IFEQ      'D'
     C                   z-add     *zero         xxtmp2
     C                   z-add     *zero         Hold20
     C                   eval      llcamt = %editc(Hold20:'P')
     C                   eval      llcamt# = Hold20
      *
     c                   else
      *
     C                   eval      Hold20 = x1camt
     C                   eval      llcamt = %editc(Hold20:'P')
     C                   eval      llcamt# = Hold20
     C                   Endif
      *
     C     X1TRDT        IFNE      *ZERO
     C     *YMD          MOVE      X1TRDT        DATUSA
     C                   MOVE      DATUSA        LLTRDT
     C                   ENDIF
      *
      *  Accounting Date - Month end of processed month
      *
     C     *USA0         MOVE      @@todt        DATUSA
     C     datUSA        adddur    1:*m          date
     C                   extrct    date:*d       day
     C                   subdur    day:*d        date
     C                   MOVE      date          llacdt
      *
     C     Firstefd      IFNE      *ZERO
     C     *YMD          MOVE      Firstefd      DATUSA
     C                   MOVE      DATUSA        LLPFDT
     C                   ENDIF
      *
     C                   MOVEL(P)  x1st          LLJRST
     C                   Eval      Llfyr  = X1fyr
     C                   Eval      Llfnd  = X1fnd
     C                   Eval      Llemp# = X1emp#
     C                   WRITE     PLL01
     c                   endif
      *
     C                   ENDSR                                                  WRBILL
      *
      *
      *
      /eject
      *==============================================================
      * WRCASH - Write to Policy Cash Transaction File
      *==============================================================
     C     WRCASH        BEGSR
      *
     c     x1arty        ifeq      'P'
     c     x1arty        oreq      'N'
     c     x1arty        oreq      'R'
     c     x1arty        oreq      'V'
     c     x1arty        oreq      'Q'
     c     x1arty        oreq      'I'
      *** beg add ***** 04/27/06 *************************************
     c     x1arty        oreq      'T'
      *** end add ***** 04/27/06 *************************************
      *
     C                   CLEAR                   PSH01
      *
     c     x1arty        ifeq      'N'
     c     x1arty        oreq      'R'
     c     x1arty        oreq      'V'
     c                   eval      x1amt = x1amt * -1
     c                   endif
      *
     C     GCKY          CHAIN     PGC01                              60
     C  N60              MOVE      GCPGID        SHPGID
      *
     C                   MOVEL(P)  SVCAP#        SHCAP#
     C                   MOVE      X1CHK#        SHCHK#
      *
     C     X1DATE        IFNE      *ZERO
     C     *YMD          MOVE      X1DATE        DATUSA
     C                   MOVE      DATUSA        SHDATE
     C                   ENDIF
      *
     C                   MOVE      '11'          SHBTYP
      *
     C                   eval      Hold20 = x1amt
     C                   eval      Shpamt = %editc(Hold20:'P')
     C                   Eval      Shpamt# = Hold20
      *
     C     X1PLAN        IFEQ      'A'
     C                   eval      xxtmp2 = x1amt - x1camt
     C                   eval      SHamt = %editc(xxtmp2:'P')
     C                   Eval      Shamt# = Xxtmp2
      *
     c                   else
      *
     C                   eval      Hold20 = x1amt
     C                   eval      Shamt = %editc(Hold20:'P')
     C                   eval      Shamt# = Hold20
     C                   Endif
      *
     C     X1PLAN        IFEQ      'D'
     C                   z-add     *zero         Hold20
     C                   eval      shcamt = %editc(Hold20:'P')
     C                   eval      shcamt# = Hold20
      *
     c                   else
      *
     C                   eval      Hold20 = x1camt
     C                   eval      shcamt = %editc(Hold20:'P')
     C                   eval      shcamt# = Hold20
     C                   Endif
      *
     C     X1TRDT        IFNE      *ZERO
     C     *YMD          MOVE      X1TRDT        DATUSA
     C                   MOVE      DATUSA        SHTRDT
     C                   ENDIF
      *
      *  Accounting Date - Month end of transaction date
      *
     C     *YMD          MOVE      X1TRDT        DATUSA
     C     datUSA        adddur    1:*m          date
     C                   extrct    date:*d       day
     C                   subdur    day:*d        date
     C                   MOVE      date          SHacdt
      *
     C     Firstefd      IFNE      *ZERO
     C     *YMD          MOVE      Firstefd      DATUSA
     C                   MOVE      DATUSA        SHPFDT
     C                   ENDIF
      *
     C                   MOVEL(P)  x1st          SHJRST
     C                   Eval      Shfyr  = X1fyr
     C                   Eval      Shfnd  = X1fnd
     C                   Eval      Shemp# = X1emp#
     C                   WRITE     PSH01
     c                   endif
      *
     C                   ENDSR                                                  WRCASH
      *
      /eject
     C     *Inzsr        Begsr                                                  WRCASH
      *
     C     *Entry        Plist
     C                   Parm                    Prmco             3
     C                   Parm                    @@todt            8
      *
     C                   Move      Prmco         Prmco#
      *
     C                   move      Prmco#        kkco#
     C                   time                    dsdttm
     C                   Move      Udate         Today
      *
     C                   eval      xxtdt = dtcymd(numDate(@@todt))
      *
      * Determine if commisssion is paid on expense constant
      *
     C                   MOVE(P)   *BLANK        XXCOMM            1
     C     kkco#         CHAIN     PTT01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     TTCTYP        IFEQ      'E'
     C     TTTYPE        ANDEQ     'P'
     C     TTECFL        ANDNE     'Y'
     C                   MOVE(P)   'Y'           XXCOMM            1
     C                   ENDIF
      *
     C     kkco#         READE     PTT01                                  60
     C                   ENDDO                                                  TTKEY
      *
     C                   Eval      First = *On
      *
     C                   Endsr                                                  WRCASH
      *
