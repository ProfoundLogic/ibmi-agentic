    HCOPYRIGHT('(C) Copyright Fiserv, Inc., 2007')
    H*********************************************************************
    H/TITLE Upload State Reporting Submission Program
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DEBUG(*YES)
     H DFTACTGRP(*NO) ACTGRP(*caller)
    H*********************************************************************
    H*  CALLED FROM: F8046C                                              *
    H*                                                                   *
    H*  CALLS      :                                                     *
    H*                                                                   *
    H*  CREATED: 12/04/07      C:183     PROJECT: MC315                  *
    H*********************************************************************
    H*  REVISIONS                                                        *
    H*                                                                   *
    H*********************************************************************
      *
     *********************************************************************
    *  File Specification                                               *
     *********************************************************************
      *
     FFAEAP     UF   E             DISK
      *
     FFAEOL4    IF   E           K DISK
      *
     FFAU1L2    UF   E           K DISK    PREFIX(X_)
     F                                     RENAME(PU101:X_PU101)
      *
     FFAU2L4    UF   E           K DISK    PREFIX(X_)
     F                                     RENAME(PU201:X_PU201)
      *
     FWMCML1    IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FFMCML1    IF   E           K DISK    PREFIX(X_) rename(pcm01:pcm02)
     FFMFNL     IF   E           K DISK
      *
     FWMEML     IF   E           K DISK
      *
     FFDDPL     IF   E           K DISK
      *
     FFOU0P     UF A E             DISK
      *
     FFOU1P     UF A E             DISK
      *
     FFOU2P     UF A E             DISK
      *
     FFOU2L     IF   E           K DISK    PREFIX(Y_)
     F                                     RENAME(PU201:Y_PU201)
      *
     *********************************************************************
    *  Definition Specification                                         *
     *********************************************************************
      *
     DDATE             S               D   DATFMT(*USA)
     DTODT             S               D   DATFMT(*YMD)
     D SRDTI           S              8  0
     D SRDTO           S             10
     D SRERR           S              1
     D SRFFMT          S              8
     D SRTFMT          S              8
      *
     DFOU1PDS        E DS                  EXTNAME(FOU1P)
      *
     DFOU2PDS          DS
     DY_U2TYPE                 1      1
     DY_U2TRID                 2     16
     DY_U2LIN#                17     24  0
     DY_U2SVDT                25     32  0
     DY_U2RVCD                33     35
     DY_U2CODE                36     43
     DY_U2CDSC                44     73
     DY_U2UNTS                74     80  0
     DY_U2MOD                 81     83
     DY_U2POS                 84     85
     DY_U2CHRG                86     96
     DY_U2MRA                 97    107
     DY_U2NDIS               108    118
     DY_U2PAID               119    129
     DY_U2NDC                130    147
     DY_U2RX#                148    160
     DY_U2DXR                161    164
     DY_U2EOB                165    168
     DY_U2EOBD               169    198
     DY_U2RLIN               199    206  0
     DY_U2REAS               207    236
     DY_U2FIL1               237    436
     DY_U2FIL2               437    500
     DY_U2MOD2               501    503
     DY_U2MOD3               504    506
     DY_U2MOD4               507    509
     DY_U2TH#                510    511
     DY_U2NWRF               512    512
     DY_U2CERT               513    513
     DY_U2LIC#               514    526
     DY_U2NDCD               527    606
     DY_U2RXDY               607    611  0
     DY_U2DFEE               612    622
     DY_U2RXDI               623    623
     DY_U2BCDT               624    625
     DY_U2BNDC               626    643
     DY_U2BLFC               644    644
     DY_U2DUCD               645    646
     DY_U2BPRC               647    654
     DY_U2BMD1               655    657
     DY_U2EOB1               658    661
     DY_U2SAG1               662    663
     DY_U2EOD1               664    803
     DY_U2EOB2               804    807
     DY_U2SAG2               808    809
     DY_U2EOD2               810    949
     DY_U2EOB3               950    953
     DY_U2SAG3               954    955
     DY_U2EOD3               956   1095
     DY_U2EOB4              1096   1099
     DY_U2SAG4              1100   1101
     DY_U2EOD4              1102   1241
     DY_U2OTH2              1242   1252
     DY_U2RXPT              1253   1253
     DY_U2PRIN              1254   1254
     DY_U2RNPR              1255   1289
     DY_U2RNPI              1290   1304
     DY_U2FILL              1305   2000
      *
     *********************************************************************
    *  Parameter List                                                   *
     *********************************************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    DOCID            12
      *
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     *********************************************************************
    *  Key Lists                                                        *
     *********************************************************************
      *
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
      *
     C     FAU1KY        KLIST
     C                   KFLD                    EAEOB#
     C                   KFLD                    EARCN
      *
     C     F2KY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    FNWCF#
      *
     C     FNKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
      *
     C     CMKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    FNWCF#
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
      *
     C     MCCMKY        KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
      *
     C     EMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
     C     DPKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    X_U2EOB#
     C                   KFLD                    X_U2DSEQ
      *
     *********************************************************************
     *  Main Line of Code                                                *
     *********************************************************************
      *
      ********************************************************************
      * Subroutine to populate State Reporting Workfiles                 *
      ****************************************************************** *
     C                   EXSR      STRPWF
      *
      ********************************************************************
      * Subroutine to populate State Reporting Outfile                   *
      ****************************************************************** *
     C                   EXSR      STRPOF
      *
      ********************************************************************
      * Pass the value of Document ID back to F8046C. The convention is  *
      * 'SR071207.txt', in which 'SR' is a literal followed by today's   *
      * date in 6 digit format and then a .txt.                          *
      *******************************************************************
      *
     C                   MOVE      *DATE         TODT
     C                   MOVE      TODT          @TODT             6 0
      *
     C                   EVAL      DOCID = 'SR' + %EDITC(@TODT:'X') +
     C                             '.txt'
      *
     C                   EVAL      *INLR = *ON
      *
     *====================================================================
    *  STRPWF - Populate State Reporting Work Files                     *
     *====================================================================
      *
     C     STRPWF        BEGSR
      *
     C                   READ      FAEAP                                  89
      *
     C                   DOW       *IN89 = *OFF
      *
      * Find out corresponding Network Code. Should be 'DCRX'to proceed   *
      *
     C     FNKY          CHAIN     PFN01                              90
     C                   IF        *IN90 = *OFF
     C     EOKY          CHAIN     FAEOL4                             90
     C                   ENDIF
      *
     C                   IF        *IN90 = *OFF AND EONTWK = 'DCRX'
      *
      * Chain FAU1L2 to find and populate corresponding FOU1P fields. The *
      * U1EDI field should be blank to indicate that it hasnt been picked *
      * as yet.                                                           *
      *
     C     FAU1KY        CHAIN     FAU1L2                             91
      *
     C                   IF        *IN91 = *OFF AND X_U1EDI = *BLANKS
      *
      *************************************************************************************
      * Processing of FOU1P
      *************************************************************************************
      *
      * Clear existing values in the fields of FOU1P                       *
      *
     C                   CLEAR                   PU101
      *
     C                   EVAL      U1TYPE = X_U1TYPE
     C                   EVAL      U1TRID = X_U1TRID
     C                   EVAL      U1CRDT = X_U1CRDT
     C                   EVAL      U1TRDT = X_U1TRDT
     C                   EVAL      U1CLM# = X_U1CLM#
     C                   if        u1clm#=' '
     c                   move      eafnd         group             3
     c                   move      eafyr         year              3
     c                   move      eacase        case#             7
     C                   eval      u1clm#=group+year+case#
     C                   endif
     C                   EVAL      U1SS#  = X_U1SS#
     C                   EVAL      U1DOI  = X_U1DOI
     C                   EVAL      U1CLSN = X_U1CLSN
     C                   EVAL      U1CFSN = X_U1CFSN
     C                   EVAL      U1NTWK = X_U1NTWK
     C                   EVAL      U1ICD1 = X_U1ICD1
     C                   EVAL      U1ICD2 = X_U1ICD2
     C                   EVAL      U1ICD3 = X_U1ICD3
     C                   EVAL      U1ICD4 = X_U1ICD4
     C                   EVAL      U1BTYP = X_U1BTYP
     C                   EVAL      U1BLTY = X_U1BLTY
     C                   EVAL      U1PCON = X_U1PCON
     C                   EVAL      U1ADDT = X_U1ADDT
     C                   EVAL      U1CFDT = X_U1CFDT
     C                   EVAL      U1CTDT = X_U1CTDT
     C                   EVAL      U1MCAR = X_U1MCAR
     C                   EVAL      U1SPCD = X_U1SPCD
     C                   EVAL      U1PHAR = X_U1PHAR
     C                   EVAL      U1POTX = X_U1POTX
     C                   EVAL      U1LIC# = X_U1LIC#
     C                   EVAL      U1COST = X_U1COST
     C                   EVAL      U1FRDT = X_U1FRDT
     C                   EVAL      U1TODT = X_U1TODT
     C                   EVAL      U1CHRG = X_U1CHRG
     C                   EVAL      U1MRA  = X_U1MRA
     C                   EVAL      U1NDIS = X_U1NDIS
      *
      *********** U1PAID needs to be populated with U1CHRG ***************
      *
     C                   EVAL      U1PAID = X_U1PAID
     C                   if        u1paid='00000000000' and
     C                             u1mra ='00000000000' and
     C                             u1ndis='00000000000'
     C                   eval      u1paid=u1chrg
     C                   endif
     C                   EVAL      U1CSDT = X_U1CSDT
     C                   EVAL      U1FEIN = X_U1FEIN
     C                   EVAL      U1PNAM = X_U1PNAM
     C                   EVAL      U1PFSN = X_U1PFSN
     C                   EVAL      U1ADR1 = X_U1ADR1
     C                   EVAL      U1ADR2 = X_U1ADR2
     C                   EVAL      U1CTY  = X_U1CTY
     C                   EVAL      U1ST   = X_U1ST
     C                   EVAL      U1ZIP  =  X_U1ZIP
     C                   EVAL      U1ZIPA =  X_U1ZIPA
     C                   EVAL      U1RCNF =  X_U1RCNF
     C                   EVAL      U1SVTY =  X_U1SVTY
     C                   EVAL      U1ADHR =  X_U1ADHR
     C                   EVAL      U1DCDT =  X_U1DCDT
     C                   EVAL      U1DCHR =  X_U1DCHR
     C                   EVAL      U1ADTY =  X_U1ADTY
     C                   EVAL      U1TSS  =  X_U1TSS
     C                   EVAL      U1PCM  =  X_U1PCM
     C                   EVAL      U1PRC1 =  X_U1PRC1
     C                   EVAL      U1PRC2 =  X_U1PRC2
     C                   EVAL      U1PPLN =  X_U1PPLN
     C                   EVAL      U1IDOC =  X_U1IDOC
     C                   EVAL      U1ACCT =  X_U1ACCT
     C                   EVAL      U1DRG# =  X_U1DRG#
     C                   EVAL      U1CONT =  X_U1CONT
     C                   EVAL      U1PPDT =  X_U1PPDT
     C                   EVAL      U1ADDG =  X_U1ADDG
     C                   EVAL      U1FFEI =  X_U1FFEI
     C                   EVAL      U1FNAM = X_U1FNAM
     C                   EVAL      U1FZIP = X_U1FZIP
     C                   EVAL      U1BLIC = X_U1BLIC
     C                   EVAL      U1BTYC = X_U1BTYC
     C                   EVAL      U1RNAM = X_U1RNAM
     C                   EVAL      U1RFEI = X_U1RFEI
     C                   EVAL      U1RLIC = X_U1RLIC
     C                   EVAL      U1RSLC = X_U1RSLC
     C                   EVAL      U1BLID = X_U1BLID
     C                   EVAL      U1PACD = X_U1PACD
     C                   EVAL      U1BADJ = X_U1BADJ
     C                   EVAL      U1BAGP = X_U1BAGP
     C                   EVAL      U1PCDT = X_U1PCDT
     C                   EVAL      U1BACD = X_U1BACD
     C                   EVAL      U1PROV = X_U1PROV
     C                   EVAL      U1OTH2 = X_U1OTH2
     C                   EVAL      U1RNST = X_U1RNST
     C                   EVAL      U1OLIC = X_U1OLIC
     C                   EVAL      U1ICD5 = X_U1ICD5
     C                   EVAL      U1ICD6 = X_U1ICD6
     C                   EVAL      U1ICD7 =  X_U1ICD7
     C                   EVAL      U1ICD8 =  X_U1ICD8
     C                   EVAL      U1PRC3 =  X_U1PRC3
     C                   EVAL      U1PRC4 =  X_U1PRC4
     C                   EVAL      U1PRC5 =  X_U1PRC5
     C                   EVAL      U1CAU1 =  X_U1CAU1
     C                   EVAL      U1CAU2 =  X_U1CAU2
     C                   EVAL      U1CAU3 =  X_U1CAU3
     C                   EVAL      U1SCHD =  X_U1SCHD
     C                   EVAL      U1PEND =  X_U1PEND
     C                   EVAL      U1RFNM =  X_U1RFNM
     C                   EVAL      U1PXFE =  X_U1PXFE
     C                   EVAL      U1PXNM =  X_U1PXNM
     C                   EVAL      U1PXAD =  X_U1PXAD
     C                   EVAL      U1PXCT =  X_U1PXCT
     C                   EVAL      U1PXST =  X_U1PXST
     C                   EVAL      U1PXZP =  X_U1PXZP
     C                   EVAL      U1NDSC =  X_U1NDSC
      *
      * Chain WMCML1 to find and populate Jurisdiction State              *
      *
     C                   EVAL      U1JUST = *BLANKS
     C     CMKY          CHAIN     WMCML1                             92
     C                   IF        *IN92 = *OFF
     C                   EVAL      U1JUST = CMJRST
     C                   ENDIF                                                  *IN92 = *OFF
      *
      * Change of *CYMD format EAIDT to *YYMD format U1DTRV               *
      *
     C                   MOVE      EAIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         U1DTRV
      *
     C                   EVAL      U1PURP = 'D'
     C                   EVAL      U1MOP  = 'CHK'
     C                   EVAL      U1PMDT = EAWCPD
     C                   EVAL      U1CHK# = EAWCPC
     C                   EVAL      U1PTNM = EAPNAM
      *
      * Concatenation of Address, City, State and Zip fields for U1PTAD   *
      *
     C                   EVAL      U1PTAD = %TRIM(EAADR1) + ', ' +
     C                             %TRIM(EACTY) + ', ' + EAST +
     C                             ', ' + %EDITC(EAZIP:'X')
      *
     C                   EVAL      U1PTFN = EAFEIN
     C                   EVAL      U1PTLC = *BLANKS
      *
      * Populate Employer based fields - Name, FEIN and Address           *
      *
     C                   EVAL      U1EMNM = *BLANKS
     C                   EVAL      U1EMID = *ZEROS
     C                   EVAL      U1EMAD = *BLANKS
     C     EMKY          CHAIN     WMEML                              93
     C                   IF        *IN93 = *OFF
     C                   EVAL      U1EMNM = EMNAME
     C                   EVAL      U1EMID = EMFEIN
     C                   EVAL      U1EMAD = %TRIM(EMMAD1) + ', ' +
     C                             %TRIM(EMMCTY) + ', ' + EMMST  +
     C                             ', ' + %EDITC(EMMZIP:'X')
     C                   ENDIF                                                  *IN93 = *OFF
      *
      * Payment Status should be 1 for EAPAID <> 0 and 4 for EAPAID = 0   *
      *
     C                   IF        EAPAID <> 0
     C                   EVAL      U1PMTS = 1
     C                   ELSE
     C                   EVAL      U1PMTS = 4
     C                   ENDIF                                                  EAPAID <> 0
      *
      *
      * Chain FMCML1 to find and Populate State Claim Number             *
      *
     C                   EVAL      U1SCL# = *BLANKS
     C     MCCMKY        CHAIN     FMCML1                             92
     C                   IF        *IN92 = *OFF
     C                   EVAL      U1SCL# = X_CMSCL#
     C                   ENDIF                                                  *IN92 = *OFF
      *
      * Chain SMCOL to find and Populate Carrier Fein                    *
      *
     C                   EVAL      U1CFEI = *ZEROS
     C     F2KY          CHAIN     PF201                              92
     C                   IF        *IN92 = *OFF
     C                   EVAL      U1CFEI = F2FEIN
     C                   ENDIF                                                  *IN92 = *OFF
      *
     C                   WRITE     PU101
      *
      *************************************************************************************
      * Processing of FOU2P
      *************************************************************************************
      *
      * Clear existing values in FOU2P fields.                            *
      *
     C                   CLEAR                   PU201
      *
      *************************************************************************************
      * CHAIN FAU2L4 on U1TRID and populate FOU2P fields                                  *
      *************************************************************************************
      *
      * Chain FAU2L4 on TRID and populate corresponding FOU2P fields      *
      *
     C     U1TRID        CHAIN     FAU2L4                             96
      *
     C                   DOW       *IN96 = *OFF
      *
     C                   EVAL      U2TYPE    = X_U2TYPE
     C                   EVAL      U2TRID    = X_U2TRID
     C                   EVAL      U2LIN#    = X_U2LIN#
     C                   EVAL      U2SVDT    = X_U2SVDT
     C                   EVAL      U2RVCD    = X_U2RVCD
     C                   EVAL      U2CODE    = X_U2CODE
     C                   EVAL      U2CDSC    = X_U2CDSC
     C                   EVAL      U2UNTS    = X_U2UNTS
     C                   EVAL      U2MOD     = X_U2MOD
     C                   EVAL      U2POS     = X_U2POS
     C                   EVAL      U2CHRG    = X_U2CHRG
     C                   EVAL      U2MRA     = X_U2MRA
     C                   EVAL      U2NDIS    = X_U2NDIS
      *
      *********** U2PAID needs to be populated with U2CHRG ***************
      *
     C                   EVAL      U2PAID    = X_U2PAID
     C                   if        u2paid='00000000000' and
     C                             u2mra ='00000000000' and
     C                             u2ndis='00000000000'
     C                   eval      u2paid=u2chrg
     C                   endif
     C                   EVAL      U2NDC     = X_U2NDC
     C                   EVAL      U2RX#     = X_U2RX#
     C                   EVAL      U2DXR     = X_U2DXR
     C                   EVAL      U2EOB     = X_U2EOB
     C                   EVAL      U2EOBD    = X_U2EOBD
     C                   EVAL      U2RLIN    = X_U2RLIN
     C                   EVAL      U2REAS    = X_U2REAS
     C                   EVAL      U2FIL1    = X_U2FIL1
     C                   EVAL      U2FIL2    = X_U2FIL2
     C*                  EVAL      U2EOB#    = X_U2EOB#
     C*                  EVAL      U2DSEQ    = X_U2DSEQ
     C*                  EVAL      U2PRFL    = X_U2PRFL
     C                   EVAL      U2MOD2    = X_U2MOD2
     C                   EVAL      U2MOD3    = X_U2MOD3
     C                   EVAL      U2MOD4    = X_U2MOD4
     C                   EVAL      U2TH#     = X_U2TOO#
     C                   EVAL      U2NWRF    = X_U2NWRF
     C                   EVAL      U2CERT    = X_U2CERT
     C                   EVAL      U2LIC#    = X_U2LIC#
     C                   EVAL      U2NDCD    = X_U2NDCD
     C                   EVAL      U2RXDY    = X_U2RXDY
     C                   EVAL      U2DFEE    = X_U2DFEE
     C                   EVAL      U2RXDI    = X_U2RXDI
     C                   EVAL      U2BCDT   =  X_U2BCDT
     C                   EVAL      U2BNDC   =  X_U2BNDC
     C                   EVAL      U2BLFC   =  X_U2BLFC
     C                   EVAL      U2DUCD   =  X_U2DUCD
     C                   EVAL      U2BPRC   =  X_U2BPRC
     C                   EVAL      U2BMD1   =  X_U2BMD1
     C*                  EVAL      U2EOB1   =  X_U2EOB1
     C                   EVAL      U2SAG1   =  X_U2SAG1
     C*                  EVAL      U2EOD1   =  X_U2EOD1
     C                   EVAL      U2EOB2   =  X_U2EOB2
     C                   EVAL      U2SAG2   =  X_U2SAG2
     C                   EVAL      U2EOD2   =  X_U2EOD2
     C                   EVAL      U2EOB3   =  X_U2EOB3
     C                   EVAL      U2SAG3   =  X_U2SAG3
     C                   EVAL      U2EOD3   =  X_U2EOD3
     C                   EVAL      U2EOB4   =  X_U2EOB4
     C                   EVAL      U2SAG4   =  X_U2SAG4
     C                   EVAL      U2EOD4   =  X_U2EOD4
     C                   EVAL      U2OTH2   =  X_U2OTH2
      *
      * Chain FDDPL to populate U2RXPT and U2PRIN                         *
      *
     C                   EVAL      U2RXPT =  *BLANKS
     C                   EVAL      U2PRIN =  *BLANKS
     C     DPKY          CHAIN     FDDPL                              97
     C                   IF        *IN97 = *OFF
     C                   IF        (U1ST = 'FL' AND U1BTYP = 'P')
     C                             OR U1ST <> 'FL'
     C                   EVAL      U2RXPT =  DPRXPT
     C                   IF        (U1ST = 'FL' AND U1BTYP = 'P'
     C                             AND U2RXPT = 'S') OR U1ST <> 'FL'
     C                   EVAL      U2PRIN =  DPPRIN
     C                   ENDIF                                                  U1ST = 'FL'
     C                   ENDIF                                                  AND U2RXPT = 'S'
     C                   ENDIF                                                  *IN97 = *OFF
      *
     C                   EVAL      U2RNPR =  *BLANKS
     C                   EVAL      U2RNPI =  *BLANKS
      *
      * U2EOB1 and U2EOD1 should be populated on State basis              *
      *
     C                   EVAL      U2EOB1 = *BLANKS
     C                   EVAL      U2EOD1 = *BLANKS
      *
     C                   IF        EAPAID <> 0
      *
     C                   SELECT
      *
     C                   WHEN      U1JUST = 'TX'
     C                   EVAL      U2EOB1 = 'C'
     C                   EVAL      U2EOD1 = 'NEGOTIATED CONTRACT'
      *
     C                   WHEN      U1JUST = 'FL'
     C                   EVAL      U2EOB1 = '398'
     C                   EVAL      U2EOD1 = 'NEGOTIATED DISCOUNT APPLIED'
      *
     C                   WHEN      U1JUST = 'CA'
     C                   EVAL      U2EOB1 = '45'
     C                   EVAL      U2EOD1 = 'Charges exceed your contracted' +
     C                             ' / legislated fee arrangment.'
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  EAPAID <> 0
      *
     C                   WRITE     PU201
      *
     C     U1TRID        READE     FAU2L4                                 96
      *
     C                   ENDDO
      *
      * U1EDI field in FAU1P should be updated to 'S'.                    *
      *
     C                   EVAL      X_U1EDI = 'S'
     C                   UPDATE    X_PU101
     C                   EVAL      EAEDI='S'
     C                   move      *date         date
     C     *cymd         move      date          eaesdt
     C                   UPDATE    PEA01
      *
     C                   ENDIF                                                  *IN91 = *OFF
      *
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C                   READ      FAEAP                                  89
      *
     C                   ENDDO
      *
     C                   ENDSR                                                  STRPWF
      *
     *====================================================================
    *  STRPOF - Populate State Reporting Out file                       *
     *====================================================================
      *
     C     STRPOF        BEGSR
      *
      * Start reading FOU1P from the start and populate FOU0P flat record *
      *
     C     *START        SETLL     FOU1P
     C                   READ      FOU1P                                  94
      *
     C                   DOW       *IN94 = *OFF
      *
     C                   EVAL      U0DATA = FOU1PDS
     C                   EVAL      U0PRFL = 'Y'
     C                   WRITE     PU001
      *
      * Chain FOU2L for U1TRID and populate corresponding records in FOU0P*
      *
     C     U1TRID        CHAIN     FOU2L                              95
      *
     C                   DOW       *IN95 = *OFF
      *
     C                   EVAL      U0DATA = FOU2PDS
     C                   EVAL      U0PRFL = 'Y'
     C                   WRITE     PU001
      *
     C     U1TRID        READE     FOU2L                                  95
      *
     C                   ENDDO
      *
     C                   READ      FOU1P                                  94
      *
     C                   ENDDO
      *
     C                   ENDSR
