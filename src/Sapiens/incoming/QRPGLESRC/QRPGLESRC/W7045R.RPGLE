    HCOPYRIGHT('(C) Copyright Fiserv, Inc., 2006')
    H*********************************************************************
    H/TITLE Transpac Download - Load
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DEBUG(*YES)
    H*********************************************************************
    H*  CALLED FROM: W7045C                                              *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*  CREATED: 04/18/07      C:183     PROJECT: 135WIB                 *
    H*********************************************************************
    H*  REVISIONS                                                        *
    H*                                                                   *
    H* 08/08/07 C:53  Support: 59384                                     *
     H*                Correct waiver file issue                          *
     H*                                                                   *
    H*********************************************************************
      *
     *********************************************************************
    *  File Specification                                               *
     *********************************************************************
      *
     FWMCML     IF   E           K DISK
      *
     FWMEML     IF   E           K DISK
      *
     FWDELL     IF   E           K DISK
      *
     FWDPAL     IF   E           K DISK    RENAME(PPA01:PPA02)
      *
     FWMRHL     IF   E           K DISK
      *
     FSDCML     IF   E           K DISK    RENAME(PCM01:PCM02)
     F                                     PREFIX('X_')
      *
     FSMARL     IF   E           K DISK
      *
     FWTABL     IF   E           K DISK
      *
     FWTOCL     IF   E           K DISK
      *
     FWTGTL     IF   E           K DISK
      *
     FWMALL     IF   E           K DISK
      *
     FWMCDL     IF   E           K DISK
      *
     FWMLLL     IF   E           K DISK
      *
     FWMMUL     IF   E           K DISK
      *
     FWMREL     IF   E           K DISK
      *
     FWDRPL     IF   E           K DISK
      *
     FWMPDL     IF   E           K DISK
      *
     FWMFNL     IF   E           K DISK
      *
     FWXTAP1    UF A E             DISK
     FWXTAP2    UF A E             DISK
     FWXTAP3    UF A E             DISK
     FWXTAP4    UF A E             DISK
     FWXTAP5    UF A E             DISK
     FWXTAP6    UF A E             DISK
     FWXTAP7    UF A E             DISK
     FWXTAP8    UF A E             DISK
      *
     *********************************************************************
    *  Definition Specification                                         *
     *********************************************************************
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D                 DS                  INZ
     D  RR                     1     90  2
     D                                     DIM(10)                              CM RMAIN RSV
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS                  INZ
     D  AR                     1     90  2
     D                                     DIM(10)                              CM PAYMENTS
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS                  INZ
     D  RT                     1     20
     D                                     DIM(10)                              CM RSV TYPE
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS                  INZ
     D  SCTP                   1     60
     D                                     DIM(4)                               SCREEN RSV DESC
     D  SCTP01                 1     15
     D  SCTP02                16     30
     D  SCTP03                31     45
     D  SCTP04                46     60
     D                 DS                  INZ
     D  SCRR                   1     45  2
     D                                     DIM(5)                               SCREEN REMAIN RSV
     D  T3RRSC                 1      9  2
     D  T3RRSM                10     18  2
     D  T3RRSE                19     27  2
     D  T3RRSL                28     36  2
     D  T3RRSR                37     45  2
     D                 DS                  INZ
     D  SCAR                   1     45  2
     D                                     DIM(5)                               SCREEN PAYMENTS
     D  SCAR01                 1      9  2
     D  SCAR02                10     18  2
     D  SCAR03                19     27  2
     D  SCAR04                28     36  2
     D  SCAR05                37     45  2
     D                 DS                  INZ
     D  SCTI                   1     45  2
     D                                     DIM(5)                               SCREEN TOT INCURR
     D  T3TINC                 1      9  2
     D  T3TINM                10     18  2
     D  T3TINE                19     27  2
     D  T3TINL                28     36  2
     D  T3TINR                37     45  2
      *
     D                 DS                  INZ
     DCLMKEY                   1     40
     DCMCO#                    1      3  0
     DCMFND                    4      6  0
     DCMFYR                    7      9  0
     DCMCASE                  10     16  0
      *
     D@@FRDT7          S              7  0
     D@@TODT7          S              7  0
     D@KEY             S             10
     D@PAAMT1          S             11  2
     D@PAAMT2          S             11  2
     D@SUMAMT          S             11  2
     D@TAP1CNT         S              9  0
     D@TAP2CNT         S              9  0
     D@TAP3CNT         S              9  0
     D@TAP4CNT         S              9  0
     D@TAP5CNT         S              9  0
     D@TAP6CNT         S              9  0
     D@TAP7CNT         S              9  0
     D@WDPALFND        S              1    INZ('N')
     D@SDCMPFND        S              1    INZ('N')
     D@WMRHPFND        S              1    INZ('N')
     D@CT              S              4    INZ('  CT')
     DPOLICY#          S             17
     DCLAIMID          S             17
     D@CO#             S              3  0
     D@FND             S              3  0
     D@FFND            S              3  0
     D@TFND            S              3  0
      *
     D SRDTI           S              8  0
     D SRDTO           S             10
     D SRERR           S              1
     D SRFFMT          S              8
     D SRTFMT          S              8
      *
     *********************************************************************
    *  Copybooks for Prototypes                                         *
     *********************************************************************
      *
     D/COPY SPRDATE
      *
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     *********************************************************************
    *  Parameter List                                                   *
     *********************************************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FFND            3
     C                   PARM                    @@TFND            3
     C                   PARM                    @@FRDT            8
     C                   PARM                    @@TODT            8
      *
     *********************************************************************
    *  Key Lists                                                        *
     *********************************************************************
      *
     C     CMLKY         KLIST
     C                   KFLD                    @CO#
     C                   KFLD                    @FND
      *
     C     PALKY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
      *
     C     RPLKY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    PACHK#
      *
     C     LLLKY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
      *
     C     RHLKY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
      *
     C     EMLKY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
     C     ELLKY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
      *
     C     ABLKY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    @KEY
      *
     C     OCLKY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMOCCD
      *
     C     ALLKY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMCTG1
      *
     C     GTLKY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    @KKTBID           4
     C                   KFLD                    @KKKEY           10
      *
     C     RTLKY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    T4RECD
      *
     C     MULKY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
      *
     C     RELKY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    EMRGN#
     C*
     C     TBKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    XXKEY            10
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
     C     FNLKY         KLIST
     C                   KFLD                    @CO#
     C                   KFLD                    @FND
      *
     *********************************************************************
    *  PLIST                                                            *
     *********************************************************************
      *
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTYYMD1       PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     *********************************************************************
     *  Main Line of Code                                                *
     *********************************************************************
      *
     C                   EVAL      @CO# = *ZEROS
     C                   EVAL      @FFND = *ZEROS
     C                   EVAL      @TFND = *ZEROS
     C                   MOVE      @@CO#         @CO#
     C                   MOVE      @@FFND        @FFND
     C                   MOVE      @@TFND        @TFND
     C                   EVAL      @FND = @FFND
      *
     C                   MOVE      @@FRDT        @@@FRDT           8 0
     C                   MOVE      @@TODT        @@@TODT           8 0
     C                   EVAL      @@FRDT7 = DTCYMD(@@@FRDT)
     C                   EVAL      @@TODT7 = DTCYMD(@@@TODT)
      *
     C                   DOW       @FND <= @TFND
      *
      * Check for Test Group
      *
     C     FNLKY         CHAIN     WMFNL
      *
     C                   IF        %FOUND(WMFNL) AND FNTEST <> 'Y'
      *
     C     CMLKY         SETLL     WMCML
     C     CMLKY         READE     WMCML
      *
     C                   DOW       NOT %EOF(WMCML)
      *
     C                   EXSR      CHKWDPAL
     C                   EXSR      CHKSDCMP
     C                   EXSR      CHKWMRHP
      *
     C                   IF        (CMIDT >= @@FRDT7 AND CMIDT <= @@TODT7) OR
     C                             @WDPALFND = 'Y' OR @SDCMPFND = 'Y' OR
     C                             @WMRHPFND = 'Y'
      *
     C                   EVAL      CLAIMID = %EDITC(CMFND:'X') +
     C                             %EDITC(CMFYR:'X') + %EDITC(CMCASE:'X')
      *
     C                   EVAL      POLICY# = %EDITC(CMFND:'X') +
     C                             %EDITC(CMEMP#:'X') + %EDITC(CMDIV:'X')
      *
     C                   EXSR      CLRTAP                                       Loss File
      *
     C                   EXSR      WXTAP1P                                      Loss File
     C                   EXSR      WXTAP2P                                      Claimant File
     C                   EXSR      WXTAP3P                                      Feature File
     C                   EXSR      WXTAP4P                                      Transaction File
     C                   EXSR      WXTAP5P                                      Darts File
     C                   EXSR      WXTAP6P                                      Darts File
     C                   EXSR      WXTAP7P                                      Waiver File
      *
     C                   ENDIF
      *
     C     CMLKY         READE     WMCML
      *
     C                   ENDDO
      *
     C                   ENDIF
      *
     C                   IF        @FND < 999
      *
     C                   EVAL      @FND = @FND + 1
      *
     C                   ELSE
      *
     C                   LEAVE
      *
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   EXSR      WXTAP8P
      *
     C                   EVAL      *INLR = *ON
      *
     *====================================================================
    *  WXTAP1P - Populate WXTAP1                                        *
     *====================================================================
      *
     C     WXTAP1P       BEGSR
      *
     C                   EVAL      T1POL# = POLICY#
     C                   EVAL      T1CLM# = CLAIMID
      *
     C                   EVAL      SRDTI = CMACDT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T1ACDT
     C                   ENDIF
      *
     C                   EVAL      T1OFF# = CMOFF#
      *
     C     EMLKY         CHAIN     WMEML
     C                   IF        %FOUND(WMEML)
     C*
     C     EMNAME        CAT(P)    EMNAM2        T1NAME
     C     ELLKY         CHAIN     WDELL
     C                   IF        %FOUND(WDELL)
      *
     C                   EVAL      SRDTI = ELPFDT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T1PFDT
     C                   ENDIF
      *
     C                   EVAL      SRDTI = ELPTDT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T1PTDT
     C                   ENDIF
      *
     C                   ENDIF
     C     EMNAME        CAT(P)    EMNAM2        T1DNAM
     C     EMFDNM        CAT(P)    EMFDN2        T1FDNM
      *
     C                   IF        EMMPFL = 'P'
      *
     C                   EVAL      T1PAD1 = EMPAD1
     C                   EVAL      T1PAD2 = EMPAD2
     C                   EVAL      T1PCTY = EMPCTY
     C                   EVAL      T1PST  = EMPST
     C                   EVAL      T1ZIP  = %EDITC(EMPZIP:'X')
      *
     C                   ELSEIF    EMMPFL = 'M'
     C                   EVAL      T1PAD1 = EMMAD1
     C                   EVAL      T1PAD2 = EMMAD2
     C                   EVAL      T1PCTY = EMMCTY
     C                   EVAL      T1PST  = EMMST
     C                   EVAL      T1ZIP  = %EDITC(EMMZIP:'X')
      *
     C                   ENDIF
      *
     C                   EVAL      T1SIC  = EMSIC
      *
     C                   EVAL      @KKTBID = 'SIC'
     C                   EVAL      @KKKEY = EMSIC
     C     GTLKY         CHAIN     WTGTL
     C                   IF        %FOUND(WTGTL)
     C                   EVAL      T1SDES = GTDESC
     C                   ENDIF
      *
     C                   EVAL      T1PHN# = %EDITC(EMAREA:'X')+
     C                                      %EDITC(EMPHN#:'X')
      *
     C     RELKY         CHAIN     WMREL
     C                   IF        %FOUND(WMREL)
     C                   EVAL      T1RDESC = REDESC
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   EVAL      T1STAT = CMSTAT
      *
     C                   EVAL      SRDTI = CMDTRP
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T1DTRP
     C                   ENDIF
      *
     C                   EVAL      SRDTI = CMRCVD
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T1RCVD
     C                   ENDIF
      *
     C                   EVAL      T1CLEX = CMCLEX
     C                   EVAL      T1ACDS = CMACDS + CMACD2 + CMACD3
     C                   EVAL      T1ACNM = CMACNM
     C                   EVAL      T1ACTY = CMACTY
     C                   EVAL      T1AST  = CMAST
      *
     C                   EVAL      T1NATR = CMNATR
     C                   EVAL      @KEY = CMNATR
     C     ABLKY         CHAIN     PNA01                              68
     C                   IF        *IN68 = '0'
     C                   EVAL      T1NDESC = NADESC
     C                   ENDIF
      *
     C                   EVAL      T1CAUS = CMCAUS
     C                   EVAL      @KEY = CMCAUS
      *
     C     ABLKY         CHAIN     PCA01                              68
     C                   IF        *IN68 = '0'
     C                   EVAL      T1CDESC = CADESC
     C                   ENDIF
      *
     C                   EVAL      T1PART = CMPART
     C                   EVAL      @KEY = CMPART
      *
     C     ABLKY         CHAIN     PPA01                              68
     C                   IF        *IN68 = '0'
     C                   EVAL      T1BDESC = PADESC
     C                   ENDIF
      *
     C                   EVAL      T1NCCI = CMNCCI
     C                   EVAL      @KKTBID = CMJRST + 'NC'
     C                   EVAL      @KKKEY = CMNCCI
      *
     C     GTLKY         CHAIN     WTGTL
     C                   IF        %FOUND(WTGTL)
     C                   EVAL      T1NDES  = GTDESC
     C                   ENDIF
      *
     C                   EVAL      T1OCCD = CMOCCD
      *
     C     OCLKY         CHAIN     WTOCL
     C                   IF        %FOUND(WTOCL)
     C                   EVAL      T1ODESC = OCDESC
     C                   ENDIF
      *
     C                   EVAL      T1SUBR = CMSUBR
      *
     C                   EVAL      @PAAMT1 = 0
     C                   EVAL      @PAAMT2 = 0
     C                   EVAL      T1RAMT1 = 0
     C                   EVAL      T1RAMT2 = 0
     C     PALKY         CHAIN     WDPAL                              67
     C                   DOW       *IN67 = *OFF
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   EVAL      PAAMT = PAAMT * -1
     C                   ENDIF
     C*
     C                   EVAL      @KEY = PABR2
     C     ABLKY         CHAIN     PB201                              68
     C                   IF        *IN68 = '0' AND B2STAT = 'A'
     C                   IF        B2MINR = '081'
     C                   EVAL      @PAAMT1 = @PAAMT1 + PAAMT
     C                   ELSEIF    B2MINR = '085'
     C                   EVAL      @PAAMT2 = @PAAMT2 + PAAMT
     C                   ENDIF
     C                   ENDIF
     C     PALKY         READE     WDPAL                                  67
     C                   ENDDO
      *
     C                   EVAL      T1RAMT1 = @PAAMT1
     C                   EVAL      T1RAMT2 = @PAAMT2
     C                   EVAL      T1JRST = CMJRST
     C                   EVAL      T1ACL# = CMACL#
      *
     C     ALLKY         CHAIN     WMALL
      *
     C                   IF        %FOUND(WMALL)
     C                   EVAL      T1LNAME = ALNAME
     C                   ENDIF
      *
     C     LLLKY         CHAIN     WMLLL
      *
     C                   IF        %FOUND(WMLLL)
      *
     C                   EVAL      T1PLAN = LLPLAN
      *
     C                   IF        LLLITG = 'Y'
     C                   EVAL      T1LITG = LLLITG
     C                   ELSEIF    LLLITG = ' '
     C                   EVAL      T1LITG = 'N'
     C                   ELSE
     C                   EVAL      T1LITG = 'N'
     C                   ENDIF
      *
     C                   IF        LLLUMP = 'Y'
      *
     C                   EVAL      SRDTI = LLSTDT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T1STDT
     C                   ENDIF
     C*                  ELSE
     C*                  EVAL      T1STDT = *ZEROS
     C                   ENDIF
      *
     C                   EVAL      T1TPAA = LLTPAA
     C                   EVAL      T1TPAM = LLTPAM
      ****** Beg Add   05/22/07 ***************************************
     C                   EVAL      XXFEIN = LLTPAA
     C                   EVAL      XXMOD = LLTPAM
     C     PDKY          CHAIN     WMPDL
     C                   IF        %FOUND(WMPDL)
     C                   EVAL      T1TPAN = PDNAME
     C                   IF        PDDBA1 = 'Y'
     C                   EVAL      T1TAD1 = PDNAME
     C                   EVAL      T1TAD2 = PDADR1
     C                   ELSE
     C                   EVAL      T1TAD1 = PDADR1
     C                   EVAL      T1TAD2 = PDADR2
     C                   ENDIF
     C                   EVAL      T1TCTY = PDCTY
     C                   EVAL      T1TST = PDST
     C                   EVAL      T1TZIP = %EDITC(PDZIP:'X')
     C                   ELSE
     C                   EVAL      T1TPAN = LLTPAN
     C                   EVAL      T1TAD1 = LLTAD1
     C                   EVAL      T1TAD2 = LLTAD2
     C                   EVAL      T1TCTY = LLTCTY
     C                   EVAL      T1TST = LLTST
     C                   EVAL      T1TZIP = %EDITC(LLTZIP:'X')
     C                   ENDIF
      ****** End Add   05/22/07 ***************************************
      *
     C                   EVAL      T1TPAR = %EDITC(LLTPAR:'X')
     C                   EVAL      T1TPAP = %EDITC(LLTPAP:'X')
      *
     C                   EVAL      SRDTI = LLTPDT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T1TPDT
     C                   ENDIF
      *
     C                   EVAL      T1TPAE = %EDITC(LLTPAE:'X')
     C                   EVAL      T1TPAF = %EDITC(LLTPAF:'X')
      *
     C                   EVAL      SRDTI = LLLWDT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T2LWDT
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   WRITE     PT101
      *
     C                   EVAL      @TAP1CNT = @TAP1CNT + 1
      *
     C                   ENDSR
      *
     *====================================================================
    *  WXTAP2P - Populate WXTAP2                                        *
     *====================================================================
      *
     C     WXTAP2P       BEGSR
      *
     C                   EVAL      T2CLM# = CLAIMID
     C                   EVAL      T2SS# = CMSS#
     C                   EVAL      T2ACDT = T1ACDT
      *
     C     CMSS#         CHAIN     WMCDL
     C                   IF        %FOUND(WMCDL)
     C                   EVAL      T2SEX = CDSEX
     C                   EVAL      T2CSS# = CDSS#
     C                   EVAL      T2CFSN = CDFSN
     C                   EVAL      T2CMID = CDMID
     C                   EVAL      T2CLSN = CDLSN
     C                   EVAL      T2ADR1 = CDADR1
     C                   EVAL      T2ADR2 = CDADR2
     C                   EVAL      T2CTY = CDCTY
     C                   EVAL      T2ST = CDST
     C                   EVAL      T2CZIP = %EDITC(CDZIP:'X')
     C                   EVAL      T2HPHN=%EDITC(CDAREA:'X')+%EDITC(CDPHN#:'X')
     C                   EVAL      T2WPH#=%EDITC(CDWARA:'X')+%EDITC(CDWPH#:'X')
      *
     C                   EVAL      SRDTI = CDDTOB
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T2DTOB
     C                   ENDIF
      *
     C                   EVAL      T2LITG = T1LITG
     C                   EVAL      T2PLAN = T1PLAN
      *
     C                   WRITE     PT201
      *
     C                   EVAL      @TAP2CNT = @TAP2CNT + 1
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
     *====================================================================
    *  WXTAP3P - Populate WXTAP3                                        *
     *====================================================================
      *
     C     WXTAP3P       BEGSR
      *
     C                   EVAL      T3CLM# = CLAIMID
     C                   EVAL      T3SS# = CMSS#
     C                   EVAL      T3LITG = T1LITG
     C                   EVAL      T3LWDT= T2LWDT
      *
     C                   EVAL      T3COVT = CMCOVT
      *
     C                   EVAL      @KKTBID = '  CT'
     C                   EVAL      @KKKEY = CMCOVT
      *
     C     GTLKY         CHAIN     WTGTL
     C                   IF        %FOUND(WTGTL)
     C                   EVAL      T3CDES = GTDESC
     C                   ENDIF
      *
     C                   EVAL      SCRR = *ZEROS
     C                   EVAL      SCAR = *ZEROS
     C                   EVAL      SCTI = *ZEROS
      *
     C                   DO        10            N                 3 0
      *
     C                   MOVEL     RT(N)         XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     RTTYPE        IFEQ      'C'
     C                   EVAL      T3RCDC = RT(N)                                 REMAIN RSV
     C                   ADD       RR(N)         SCRR(1)                          REMAIN RSV
     C                   ADD       AR(N)         SCAR(1)                          PAYMENTS
     C                   ADD       AR(N)         SCTI(1)                          TOT INCURRED
     C                   ADD       RR(N)         SCTI(1)
     C                   ENDIF
     C     RTTYPE        IFEQ      'M'
     C                   IF        RTCTGY = 'RE'
     C                   EVAL      T3RCDR = RT(N)                                 REMAIN RSV
     C                   ADD       RR(N)         SCRR(5)                          REMAIN RSV
     C                   ADD       AR(N)         SCAR(5)                          PAYMENTS
     C                   ADD       AR(N)         SCTI(5)                          TOT INCURRED
     C                   ADD       RR(N)         SCTI(5)
     C                   ELSE
     C                   EVAL      T3RCDM = RT(N)                                 REMAIN RSV
     C                   ADD       RR(N)         SCRR(2)                          REMAIN RSV
     C                   ADD       AR(N)         SCAR(2)                          PAYMENTS
     C                   ADD       AR(N)         SCTI(2)                          TOT INCURRED
     C                   ADD       RR(N)         SCTI(2)
     C                   ENDIF
     C                   ENDIF
     C     RTTYPE        IFEQ      'E'
     C                   EVAL      T3RCDE = RT(N)                                 REMAIN RSV
     C                   ADD       RR(N)         SCRR(3)                          REMAIN RSV
     C                   ADD       AR(N)         SCAR(3)                          PAYMENTS
     C                   ADD       AR(N)         SCTI(3)                          TOT INCURRED
     C                   ADD       RR(N)         SCTI(3)
     C                   ENDIF
     C     RTTYPE        IFEQ      'L'
     C                   EVAL      T3RCDL = RT(N)                                 REMAIN RSV
     C                   ADD       RR(N)         SCRR(4)                          REMAIN RSV
     C                   ADD       AR(N)         SCAR(4)                          PAYMENTS
     C                   ADD       AR(N)         SCTI(4)                          TOT INCURRED
     C                   ADD       RR(N)         SCTI(4)
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
      *
      * Total Claim Incurred
      *
     C                   XFOOT     RR            @RRTO            11 2
     C                   XFOOT     AR            @ARTO            11 2
     C     @RRTO         ADD       @ARTO         T3TCIN
      *
     C                   WRITE     PT301
      *
     C                   EVAL      @TAP3CNT = @TAP3CNT + 1
      *
     C                   ENDSR
      *
     *====================================================================
    *  WXTAP4P - Populate WXTAP4                                        *
     *====================================================================
      *
     C     WXTAP4P       BEGSR
      *
     C                   EVAL      T4CLM# = CLAIMID
     C                   EVAL      T4SS# = CMSS#
     C                   EVAL      T4ACDT = T1ACDT
      *
     C     PALKY         CHAIN     WDPAL                              64
     C                   DOW       *IN64 = *OFF
     C                   IF        (PATRDT >= @@FRDT7 AND
     C                              PATRDT <= @@TODT7)
     C                   EVAL      T4RECD = PARECD
     C                   EVAL      T4TRCD = PATRCD
     C                   EVAL      T4BR2  = PABR2
     C                   EVAL      T4CHK# = PACHK#
      *
     C                   EVAL      SRDTI = PATRDT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T4TRDT
     C                   ENDIF
      *
     C                   EVAL      SRDTI = PAFRDT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T4FRDT
     C                   ENDIF
      *
     C                   EVAL      SRDTI = PATODT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T4TODT
     C                   ENDIF
      *
     C                   EVAL      SRDTI = PAPODT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T4PODT
     C                   ENDIF
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   EVAL      PAAMT = PAAMT * -1
     C                   ENDIF
      *
     C                   EVAL      T4AMT = PAAMT
      *
     C                   EVAL      @KEY = PARECD
     C     ABLKY         CHAIN     PRT01                              68
     C                   IF        *IN68 = '0'
     C                   EVAL      T4DESC = RTDESC
     C                   ENDIF
      *
     C                   EVAL      @KEY = PATRCD
     C     ABLKY         CHAIN     PPC01                              68
     C                   IF        *IN68 = '0'
     C                   EVAL      T4PDES = PCDESC
     C                   ENDIF
      *
     C                   EVAL      @KEY = PABR2
     C     ABLKY         CHAIN     PB201                              68
     C                   IF        *IN68 = '0'
     C                   EVAL      T4BDES = B2DESC
     C                   ENDIF
      *
     C     RPLKY         CHAIN     WDRPL                              68
     C                   IF        %FOUND(WDRPL)
     C                   EVAL      T4PNAM = RPPNAM
     C                   EVAL      T4PTNM = RPPNAM
     C                   EVAL      T4ADR1 = RPADR1
     C                   EVAL      T4ADR2 = RPADR2
     C                   EVAL      T4PCTY = RPCTY
     C                   EVAL      T4PST  = RPST
     C                   EVAL      T4PZIP = %EDITC(RPZIP:'X')
     C                   ENDIF
      *
     C                   WRITE     PT401
      *
     C                   EVAL      @TAP4CNT = @TAP4CNT + 1
     C                   EVAL      @SUMAMT = @SUMAMT + T4AMT
      *
     C                   ENDIF
      *
     C     PALKY         READE     WDPAL                                  64
      *
     C                   ENDDO
      *
     C                   ENDSR
      *
     *====================================================================
    *  WXTAP5P - Populate WXTAP5                                        *
     *====================================================================
      *
     C     WXTAP5P       BEGSR
      *
     C                   EVAL      T5CLM# = CLAIMID
     C                   EVAL      T5SS# = CMSS#
     C                   EVAL      T5ACDT = T1ACDT
      *
     C     CLMKEY        CHAIN     SDCML                              65
     C                   DOW       *IN65 = *OFF
     C                   IF        (X_CMCDAT >= @@FRDT7 AND
     C                              X_CMCDAT <= @@TODT7)
     C                   EVAL      T5SEQ# = X_CMSEQ#
     C                   EVAL      T5RMCD = X_CMRMCD
     C                   EVAL      SRDTI = X_CMIDT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T5IDT
     C                   ENDIF
     C                   EVAL      T5COMM = X_CMCOMM
     C                   EVAL      T5IUSR = X_CMIUSR
      *
     C                   EVAL      SRDTI = CMRCVD
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T5RCVD
     C                   ENDIF
      *
     C                   EVAL      T6RMCD = X_CMRMCD
      *
     C                   WRITE     PT501
      *
     C                   EVAL      @TAP5CNT = @TAP5CNT + 1
      *
     C                   ENDIF
      *
     C     CLMKEY        READE     SDCML                                  65
      *
     C                   ENDDO
      *
     C                   ENDSR
      *
     *====================================================================
    *  WXTAP6P - Populate WXTAP6                                        *
     *====================================================================
      *
     C     WXTAP6P       BEGSR
      *
     C                   EVAL      T6CLM# = CLAIMID
     C                   EVAL      T6SS# = CMSS#
     C                   EVAL      T6ACDT = T1ACDT
      *
     C     CLMKEY        CHAIN     SMARL                              66
     C                   DOW       *IN66 = *OFF
     C                   IF        (ARCDAT >= @@FRDT7 AND
     C                              ARCDAT <= @@TODT7)
     C                   EVAL      T6SEQ# = ARSEQ#
     C                   EVAL      T6ASEQ# = ARASQ#
      *
     C                   EVAL      SRDTI = ARCDAT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T6IDT
     C                   ENDIF
      *
     C                   EVAL      SRDTI = ARIDT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T6AIDT
     C                   ENDIF
      *
     C                   EVAL      T6CM75 = ARCM75
     C                   EVAL      T6IUSR = ARIUSR
      *
     C                   WRITE     PT601
      *
     C                   EVAL      @TAP6CNT = @TAP6CNT + 1
      *
     C                   ENDIF
      *
     C     CLMKEY        READE     SMARL                                  66
      *
     C                   ENDDO
      *
     C                   ENDSR
      *
     *====================================================================
    *  WXTAP7P - Populate WXTAP7                                        *
     *====================================================================
      *
     C     WXTAP7P       BEGSR
      *
     C                   EVAL      T7POL# = POLICY#
     C                   EVAL      T7CLM# = CLAIMID
     C                   EVAL      T7SS# = CMSS#
     C                   EVAL      T7ACDT = T1ACDT
     C                   EVAL      T7NAME = T1NAME
     C                   EVAL      T7PFDT = T1PFDT
     C                   EVAL      T7PTDT = T1PTDT
     C                   EVAL      T7CSTA = CMSTAT
     C                   EVAL      T7DTRP = T1DTRP
      *
     C                   EVAL      SRDTI = CMRCVD
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T7RCVD
     C                   ENDIF
      *
     C     MULKY         CHAIN     WMMUL                              70
     C                   DOW       *IN70 = *OFF
      *
      *** beg chg *** 08/08/07 *****************************
     C*****************  IF        MUFORM = 'WC000313       '
     C*****************            AND MUEEDT >= CMACDT
     C                   IF        MUFORM = 'WC000313       '
     C                             AND MUEEDT <= CMACDT
      *** end chg *** 08/08/07 *****************************
      *
     C                   EVAL      T7END# = MUEND#
     C                   EVAL      T7FORM = MUFORM
     C                   EVAL      SRDTI = MUEEDT
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T7EEDT
     C                   ENDIF
      *
     C                   WRITE     PT701
      *
     C                   EVAL      @TAP7CNT = @TAP7CNT + 1
     C                   ENDIF
      *
     C     MULKY         READE     WMMUL                                  70
      *
     C                   ENDDO
      *
     C                   ENDSR
      *
     *====================================================================
    *  WXTAP8P - Populate WXTAP8                                        *
     *====================================================================
      *
     C     WXTAP8P       BEGSR
      *
     C                   EVAL      T8TLSR = @TAP1CNT
     C                   EVAL      T8TCLR = @TAP2CNT
     C                   EVAL      T8TCNR = @TAP5CNT
     C                   EVAL      T8TCNR1 = @TAP6CNT
     C                   EVAL      T8TFTR = @TAP3CNT
     C                   EVAL      T8TTRR = @TAP4CNT
     C                   EVAL      T8TOTR = @TAP1CNT + @TAP2CNT + @TAP3CNT +
     C                                      @TAP5CNT + @TAP6CNT + @TAP7CNT
     C                   EVAL      T8TTAM = @SUMAMT
      *
     C                   EVAL      SRDTI = *DATE
     C                   CALL      'SRCVTDAT'    DTYYMD1
     C                   IF        SRERR = *OFF
     C                   MOVEL(P)  SRDTO         T8DTCO
     C                   ENDIF
      *
     C                   WRITE     PT801
      *
     C                   ENDSR
      *
     *====================================================================
    *  CHKWDPAL - Check in range record in WDPAL                        *
     *====================================================================
      *
     C     CHKWDPAL      BEGSR
      *
     C                   EVAL      @WDPALFND = 'N'
     C     PALKY         CHAIN     WDPAL                              61
     C                   DOW       *IN61 = *OFF
     C                   IF        (PATRDT >= @@FRDT7 AND
     C                              PATRDT <= @@TODT7)
     C                   EVAL      @WDPALFND = 'Y'
     C                   LEAVE
     C                   ENDIF
     C     PALKY         READE     WDPAL                                  61
     C                   ENDDO
     C                   ENDSR
      *
     *====================================================================
    *  CHKSDCMP - Check in range record in SDCMP                        *
     *====================================================================
      *
     C     CHKSDCMP      BEGSR
      *
     C                   EVAL      @SDCMPFND = 'N'
     C     CLMKEY        CHAIN     SDCML                              62
     C                   DOW       *IN62 = *OFF
     C                   IF        (X_CMCDAT >= @@FRDT7 AND
     C                              X_CMCDAT <= @@TODT7)
     C                   EVAL      @SDCMPFND = 'Y'
     C                   LEAVE
     C                   ENDIF
     C     CLMKEY        READE     SDCML                                  62
     C                   ENDDO
     C                   ENDSR
      *
     *====================================================================
    *  CHKWMRHP - Check in range record in WMRHP                        *
     *====================================================================
      *
     C     CHKWMRHP      BEGSR
      *
     C                   EVAL      @WMRHPFND = 'N'
     C     RHLKY         CHAIN     WMRHL                              63
     C                   DOW       *IN63 = *OFF
     C                   IF        (RHEFDT >= @@FRDT7 AND
     C                              RHEFDT <= @@TODT7)
     C                   EVAL      @WMRHPFND = 'Y'
     C                   LEAVE
     C                   ENDIF
     C     RHLKY         READE     WMRHL                                  63
     C                   ENDDO
     C                   ENDSR
      *
     *====================================================================
    *  CLRTAP - Clear the RF for WXTAP1-8                               *
     *====================================================================
      *
     C     CLRTAP        BEGSR
      *
     C                   CLEAR                   PT101
     C                   CLEAR                   PT201
     C                   CLEAR                   PT301
     C                   CLEAR                   PT401
     C                   CLEAR                   PT501
     C                   CLEAR                   PT601
     C                   CLEAR                   PT701
      *
     C                   ENDSR
