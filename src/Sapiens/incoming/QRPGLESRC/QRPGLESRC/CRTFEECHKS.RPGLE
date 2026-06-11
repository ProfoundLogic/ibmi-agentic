     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CRTFEECHKS - Create MCMC Fe Checks
     H*****************************************************************
     H* Created by:  C:62     Support: 59129      09/21/2007          *
     H*****************************************************************
     H* Entry parms:  Fee Type-B=BRS and N=Network and Blank=Both     *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     FFAEAPSEL  IF   E             DISK
     F                                     RENAME(FAEAPSEL:PEA02)
     FFAEAL1    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FFAEOL3    IF   E           K DISK
     FFAESL     IF   E           K DISK
     FWMCIL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWARPL     IF A E           K DISK
     FWTPSL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)

     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  F1NW                   1     50
     D                                     DIM(10)
     D  F1NW01                 1      5
     D  F1NW02                 6     10
     D  F1NW03                11     15
     D  F1NW04                16     20
     D  F1NW05                21     25
     D  F1NW06                26     30
     D  F1NW07                31     35
     D  F1NW08                36     40
     D  F1NW09                41     45
     D  F1NW10                46     50
     D                 DS
     D  F1NF                   1     10
     D                                     DIM(10)
     D  F1NF01                 1      1
     D  F1NF02                 2      2
     D  F1NF03                 3      3
     D  F1NF04                 4      4
     D  F1NF05                 5      5
     D  F1NF06                 6      6
     D  F1NF07                 7      7
     D  F1NF08                 8      8
     D  F1NF09                 9      9
     D  F1NF10                10     10
     D                 DS
     D  F1NI                   1     90  0
     D                                     DIM(10)
     D  F1NI01                 1      9  0
     D  F1NI02                10     18  0
     D  F1NI03                19     27  0
     D  F1NI04                28     36  0
     D  F1NI05                37     45  0
     D  F1NI06                46     54  0
     D  F1NI07                55     63  0
     D  F1NI08                64     72  0
     D  F1NI09                73     81  0
     D  F1NI10                82     90  0
     D                 DS
     D  F1NM                   1     30  0
     D                                     DIM(10)
     D  F1NM01                 1      3  0
     D  F1NM02                 4      6  0
     D  F1NM03                 7      9  0
     D  F1NM04                10     12  0
     D  F1NM05                13     15  0
     D  F1NM06                16     18  0
     D  F1NM07                19     21  0
     D  F1NM08                22     24  0
     D  F1NM09                25     27  0
     D  F1NM10                28     30  0
     D                 DS
     D  F1N$                   1     90  2
     D                                     DIM(10)
     D  F1N$01                 1      9  2
     D  F1N$02                10     18  2
     D  F1N$03                19     27  2
     D  F1N$04                28     36  2
     D  F1N$05                37     45  2
     D  F1N$06                46     54  2
     D  F1N$07                55     63  2
     D  F1N$08                64     72  2
     D  F1N$09                73     81  2
     D  F1N$10                82     90  2
     D                 DS
     D  F1NT                   1     10
     D                                     DIM(10)
     D  F1NT01                 1      1
     D  F1NT02                 2      2
     D  F1NT03                 3      3
     D  F1NT04                 4      4
     D  F1NT05                 5      5
     D  F1NT06                 6      6
     D  F1NT07                 7      7
     D  F1NT08                 8      8
     D  F1NT09                 9      9
     D  F1NT10                10     10
     D*
     D                 DS
     D  DATA                   1     50
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*================================================================
     C* Parameter Lists
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@FTYP            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANK        SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDCY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANK        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C* Key Lists
     C*================================================================
     C*
     C*  KEY LIST FOR HEADER BILL AMOUNT FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    EARCN
     C*
     C     ESKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    XXIDT
     C*
     C     ESKY2         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C*  Key list for the WCMC Group Master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C*
     C*  Key list for the EOMB file
     C*
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAEOB#
     C*
     C*  Key list for the WCMC Check Issuance file
     C*
     C     CIKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    XXPTYP
     C                   KFLD                    XXSYS
     C*
     C*  Key list for the Claim Master file
     C*
     C     CMKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C*
     C*  Key list for the Request Payment file
     C*
     C     RPKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C*
     C*  Key list for the Provider file
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  Key list for the WCMS table files
     C*
     C     TBKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    XXKEY
     C*================================================================
     C* Like Defines
     C*================================================================
     C     *LIKE         DEFINE    EOFEIN        XXFEIN
     C     *LIKE         DEFINE    EOMOD         XXMOD
     C     *LIKE         DEFINE    CIPTYP        XXPTYP
     C     *LIKE         DEFINE    CISYS         XXSYS
     C     *LIKE         DEFINE    PCKEY         XXKEY
     C     *LIKE         DEFINE    ESIDT         XXIDT
     C*================================================================
     C* Program Cycle
     C*================================================================
     C     @@FTYP        IFEQ      'B'
     C     @@FTYP        OREQ      'N'
     C     @@FTYP        OREQ      ' '
     C*
     C     1             SETLL     PEA02
     C                   READ      PEA02                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C*  Process the initialization subroutine
     C*
     C                   EXSR      SRINIT
     C*
     C                   DO
     C*
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C*  Determine Next sequence number for payment requests
     C*
     C     RPKY          SETGT     PRP01
     C     RPKY          READPE(N) PRP01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             RPSEQ#
     C                   ENDIF
     C*
     C*  Process the subroutines which create the various payments.
     C*
     C     @@FTYP        IFNE      'N'
     C     EASYS         IFEQ      'P'
     C     EASYS         OREQ      'H'
     C     EASYS         OREQ      'U'
     C     EASYS         OREQ      'M'
     C                   EXSR      SRBRS
     C                   ENDIF
     C                   ENDIF
     C*
     C     @@FTYP        IFNE      'B'
     C     EASYS         IFEQ      'P'
     C     EASYS         OREQ      'H'
     C*                    EXSR SRNTWK
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   READ      PEA02                                  99
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C*  END THE PROGRAM
     C*
     C                   MOVE      *ON           *INLR
     C******************************************************************
     C*  SRINIT - This subroutine is used to define and initialize
     C*    program variables.
     C******************************************************************
     C     SRINIT        BEGSR
     C*
     C                   Z-ADD     1             X                 3 0
     C                   TIME                    DSDTTM
     C*
     C*  Retrieve the WCMS group #.
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   ENDIF
     C*
     C*  Retrieve the Group Auxiliary record.
     C*
     C     FNKY          CHAIN     PF101                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEA     *BLANKS       F1NW
     C                   MOVEA     *BLANKS       F1NF
     C                   MOVEA     *ZEROS        F1NI
     C                   MOVEA     *ZEROS        F1NM
     C                   MOVEA     *ZEROS        F1N$
     C                   MOVEA     *BLANKS       F1NT
     C                   ENDIF
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLT#            3
     C*
     C                   ENDSR
     C******************************************************************
     C*  SRBRS - This subroutine creates the bill review service pmt.
     C******************************************************************
     C     SRBRS         BEGSR
     C*
     C                   DO
     C*
     C*  Retrieve the BRS WCMC check issuance record.
     C*
     C                   MOVE      'B'           XXPTYP
     C                   MOVE      EOSYS         XXSYS
     C     CIKY          CHAIN     PCI01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  Create the payment request record.
     C*
     C                   Z-ADD     EACO#         RPCO#
     C                   Z-ADD     EAFND         RPFND
     C                   Z-ADD     EAFYR         RPFYR
     C                   Z-ADD     EACASE        RPCASE
     C                   ADD       1             RPSEQ#
     C*
     C                   Z-ADD     EOTRDT        RPTRDT
     C                   MOVE      CIFEIN        RPFEIN
     C                   MOVE      CIMOD         RPMOD
     C*
     C                   Z-ADD     EABAMT        RPAMT
     C*
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CIP1CV        RPBR2
     C                   ELSE
     C                   MOVE      CIBR2         RPBR2
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVE      CIBR7L        RPBR7
     C                   ELSE
     C                   MOVE      CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVE      CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C                   Z-ADD     0             RPWKS
     C                   Z-ADD     0             RPDAYS
     C*
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C     EATRCD        IFNE      *BLANKS
     C                   MOVE      EATRCD        RPTRCD
     C                   ELSE
     C     CMCVFL        IFNE      'Y'
     C                   MOVEL(P)  CITRCD        RPTRCD
     C                   ELSE
     C                   MOVEL(P)  CITRCV        RPTRCD
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CITRCV        RPTRCD
     C                   ELSE
     C                   MOVE      CITRCD        RPTRCD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PCRECD        RPRECD
     C                   MOVE      PCTRTP        RPTYPE
     C                   MOVEL     PCDESC        RPDESC
     C                   ELSE
     C                   MOVE      *BLANKS       RPRECD
     C                   MOVE      *BLANKS       RPTYPE
     C                   MOVE      *BLANKS       RPDESC
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       RPRMDS
     C                   MOVE      *BLANKS       RPRMK1
     C                   MOVE      *BLANKS       RPRMK2
     C                   MOVE      *BLANKS       RPRMK3
     C*
     C                   MOVEL     CIFEIN        XXFEIN
     C                   MOVEL     CIMOD         XXMOD
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     PDDBA1        IFNE      'Y'
     C                   MOVE      PDNAME        RPPNAM
     C                   MOVE      PDADR1        RPADR1
     C                   MOVE      PDADR2        RPADR2
     C                   ELSE
     C                   MOVE      PDADR1        RPPNAM
     C                   MOVE      PDADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   ENDIF
     C*
     C                   MOVE      PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
     C                   MOVE      *BLANKS       RPADR3
     C                   MOVE      *BLANKS       RPCTRY
     C     PDADR3        IFNE      *BLANK
     C                   MOVEL(P)  PDCTRY        RPCTRY
     C                   MOVEL(P)  PDADR3        RPADR3
     C                   END
     C*
     C                   Z-ADD     EOEOB#        RPEOB#
     C                   MOVE      'P'           RPSTAT
     C                   Z-ADD     0             RPBUL1
     C                   Z-ADD     0             RPCHK#
     C                   MOVE      'G'           RPPGTP
     C                   MOVE      *BLANK        RPOFF#
     C*
     C                   MOVE      EAUSER        RPUSER
     C                   MOVEL     *BLANKS       RPCKCN
     C                   MOVE      Q1PGM         RPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C*
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C     EORCN         IFEQ      *ZEROS
     C                   Z-ADD     EOCRDT        RPRCVD
     C                   ELSE
     C                   Z-ADD     *HIVAL        XXIDT
     C     ESKY          SETGT     PES01
     C     ESKY2         READPE    PES01                                  99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     ESRRDT        RPRCVD
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     EOTRDT        RPRCVD
     C                   ENDIF
     C*
     C                   Z-ADD     0             RPDUE
      *
      *  Call API to determine the Bank "Code".
      *
     C                   MOVE      RPCO#         ##CO#
     C                   MOVE      RPFND         ##FND
     C                   MOVE      RPFYR         ##FYR
     C                   MOVE      RPCASE        ##CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
      *
     C                   CALL      'WA023R1'
     C                   PARM      '2'           ##MODE            1            Mode
     C                   PARM                    ##CO#             3            Company
     C                   PARM                    ##FND             3            Group #
     C                   PARM                    ##FYR             3            Year
     C                   PARM                    ##CASE            7            Case
     C                   PARM      'WC'          ##APPL            2            Applic Code
     C                   PARM      DATA          ##DATA           50            Selection criteria
     C                   PARM                    ##CKTP           10            Bank Acct Code
     C                   PARM                    ##RTCD            1            Return Code
      *
     C                   MOVE      ##CKTP        RPCKTP                         Bank Acct# Code
     C                   MOVE      *BLANK        RPACCT
     C*
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
     C     EACKST        IFEQ      *BLANKS
     C                   MOVEL     CISTS         RPSTS
     C                   ELSE
     C     EACKST        IFEQ      'PY'
     C                   MOVEL     CISTS         RPSTS
     C                   ELSE
     C                   MOVE      EACKST        RPSTS
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   MOVE      CISTS         RPSTS
     C                   ENDIF
     C*
     C* CHECK TO SEE IF THIS BILL SHOULD BE AUTO-RELEASED
     C*
     C     @@CLT#        IFEQ      '132'
     C     RPAMT         IFGT      250
     C     CMDNY         OREQ      'Y'
     C     CMSTAT        OREQ      'C'
     C                   MOVEL(P)  'PR'          RPSTS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPSTS         XXKEY
     C     TBKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PSSRT         RPSORT
     C                   ELSE
     C                   MOVE      *BLANKS       RPSORT
     C                   ENDIF
     C*
     C                   MOVE      'N'           RPSENT
     C                   Z-ADD     *ZERO         RPDSEQ
     C                   MOVEL     EAUSER        RPRQST
     C                   MOVE      *BLANKS       RPLTCD
     C                   MOVE      'B'           RPNTWK
     C                   Z-ADD     EORCN         RPEOBS
     C                   MOVE      *BLANKS       RPFNAL
     C                   Z-ADD     RPDT          RPIDT
     C                   Z-ADD     DSTIME        RPITM
     C                   MOVEL     RPUSER        RPIUSR
     C                   MOVEL     Q1PGM         RPIPGM
     C                   Z-ADD     0             RPRDT
     C                   Z-ADD     0             RPRTM
     C                   MOVE      *BLANKS       RPRUSR
     C                   MOVE      *BLANKS       RPRPGM
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     RPDT          RPRDT
     C                   Z-ADD     DSTIME        RPRTM
     C                   MOVEL     RPUSER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
     C                   ENDIF
     C*
     C     RPAMT         IFGT      0
     C     EAPBRC        ANDNE     'N'
     C                   WRITE     PRP01
     C                   ENDIF                                                  RPAMT GT 0
     C                   ENDIF                                                  CIKEY
     C                   ENDDO
     C*
     C                   ENDSR
     C******************************************************************
     C*  SRNTWK - This subroutine creates the network payment.
     C******************************************************************
     C     SRNTWK        BEGSR
     C*
     C*  Create the payment request record.
     C*
     C                   Z-ADD     EACO#         RPCO#
     C                   Z-ADD     EAFND         RPFND
     C                   Z-ADD     EAFYR         RPFYR
     C                   Z-ADD     EACASE        RPCASE
     C                   ADD       1             RPSEQ#
     C                   Z-ADD     EOTRDT        RPTRDT
     C*
     C                   MOVEL     EONTWK        XXNWCD            5
     C*
     C     XXNWCD        LOOKUP    F1NW(X)                                61
     C  N61              GOTO      NXTC
     C*
     C* IF IN-HOUSE NETWORK:
     C*
B01  C     F1NF(X)       IFEQ      'I'
     C*
     C                   MOVE      'B'           XXPTYP
     C                   MOVE      'N'           XXSYS
     C*
     C* RETRIEVE WCMC CHECK ISSUANCE INFORMATION:
     C*
     C     CIKY          CHAIN     PCI01                              60
     C   60              GOTO      NXTC
     C*
     C                   Z-ADD     CIFEIN        RPFEIN
     C                   Z-ADD     CIMOD         RPMOD
     C*
     C* RETRIEVE BILLING AMOUNT:
     C*
     C                   Z-ADD     EANAMT        RPAMT
     C*
     C* RETRIEVE BILLING AMOUNT:
     C*
S01  C                   ELSE
     C*
     C* RETRIEVE WCMC CHECK ISSUANCE INFORMATION:
     C*
     C                   MOVEL     'N'           XXPTYP
     C                   MOVEL     EOSYS         XXSYS
     C*
     C     CIKY          CHAIN     PCI01                              60
     C   60              GOTO      NXTC
     C*
     C* ESTABLISH PAYEE FEIN/MOD
     C*
     C                   Z-ADD     F1NI(X)       RPFEIN
     C                   Z-ADD     F1NM(X)       RPMOD
     C*
     C* CALCULATE OUTSIDE NETWORK FEE:
     C* IF OUTSIDE NETWORK, DISCOUNT CALCULATED OUTSIDE, THEN USE FEE
     C* STORED IN FAEAP/EANAMT
     C*
B02  C     F1NF(X)       IFEQ      'Y'
     C*
     C                   Z-ADD     EANAMT        RPAMT
     C*
S02  C                   ELSE
     C*
     C* IF OUTSIDE NETWORK, DISCOUNT CALCULATED IN-HOUSE, THEN
     C* CALCULATE FEE AND STORE IN EANAMT, CALCULATION BASED ON "TYP"
     C* IN GROUP SETUP:
     C*      S = % OF NETWORK SAVINGS (WHERE "FEE AMOUNT" = X)
     C*      B = FLAT RATE(WHERE "FEE AMOUNT" = FLAT RATE)
     C*      N = N/A (NO BILL RATE)
     C*
B03  C     F1NT(X)       IFEQ      'B'
     C*
     C                   Z-ADD     F1N$(X)       RPAMT
     C*
S03  C                   ELSE
     C*
B04  C     F1NT(X)       IFEQ      'S'
     C*
     C     F1N$(X)       DIV       100           XXPERC            9 2
     C     XXPERC        MULT      EANDIS        RPAMT
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        RPAMT
     C*
E04  C                   ENDIF
E03  C                   ENDIF
E02  C                   ENDIF
E01  C                   ENDIF
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CIP1CV        RPBR2
     C                   ELSE
     C                   MOVE      CIBR2         RPBR2
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVE      CIBR7L        RPBR7
     C                   ELSE
     C                   MOVE      CIBR7N        RPBR7
     C                   ENDIF
     C*
     C                   MOVE      CMNCCI        RPNCCI
     C                   Z-ADD     EOFRDT        RPFRDT
     C                   Z-ADD     EOTODT        RPTODT
     C                   Z-ADD     0             RPWKS
     C                   Z-ADD     0             RPDAYS
     C     EATRCD        IFNE      *BLANKS
     C                   MOVE      EATRCD        RPTRCD
     C                   ELSE
     C     CMCVFL        IFNE      'Y'
     C                   MOVEL(P)  CITRCD        RPTRCD
     C                   ELSE
     C                   MOVEL(P)  CITRCV        RPTRCD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PCRECD        RPRECD
     C                   MOVE      PCTRTP        RPTYPE
     C                   MOVEL     PCDESC        RPDESC
     C                   ELSE
     C                   MOVE      *BLANKS       RPRECD
     C                   MOVE      *BLANKS       RPTYPE
     C                   MOVE      *BLANKS       RPDESC
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      RTTYPE        RPRETP
     C                   ELSE
     C                   MOVE      *BLANKS       RPRETP
     C                   ENDIF
     C*
     C                   MOVEL(P)  EACNTL        RPRMDS
     C                   MOVE      *BLANKS       RPRMK1
     C                   MOVE      *BLANKS       RPRMK2
     C                   MOVE      *BLANKS       RPRMK3
     C*
     C                   Z-ADD     RPFEIN        XXFEIN
     C                   Z-ADD     RPMOD         XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C   60              GOTO      NXTC
     C     PDDBA1        IFNE      'Y'
     C                   MOVE      PDNAME        RPPNAM
     C                   MOVE      PDADR1        RPADR1
     C                   MOVE      PDADR2        RPADR2
     C                   ELSE
     C                   MOVE      PDADR1        RPPNAM
     C                   MOVE      PDADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   ENDIF
     C                   MOVE      PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
     C                   MOVE      *BLANKS       RPADR3
     C                   MOVE      *BLANKS       RPCTRY
     C     PDADR3        IFNE      *BLANK
     C                   MOVEL(P)  PDCTRY        RPCTRY
     C                   MOVEL(P)  PDADR3        RPADR3
     C                   END
     C*
     C                   Z-ADD     EOEOB#        RPEOB#
     C                   MOVE      'P'           RPSTAT
     C                   Z-ADD     0             RPBUL1
     C                   Z-ADD     0             RPCHK#
     C                   MOVE      'G'           RPPGTP
     C                   MOVE      *BLANK        RPOFF#
     C*
     C                   MOVE      EAIUSR        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C*
     C                   Z-ADD     EOCRDT        RPRCVD
     C                   Z-ADD     0             RPDUE
      *
      *  Call API to determine the Bank "Code".
      *
     C                   MOVE      RPCO#         ##CO#
     C                   MOVE      RPFND         ##FND
     C                   MOVE      RPFYR         ##FYR
     C                   MOVE      RPCASE        ##CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
      *
     C                   CALL      'WA023R1'
     C                   PARM      '2'           ##MODE            1            Mode
     C                   PARM                    ##CO#             3            Company
     C                   PARM                    ##FND             3            Group #
     C                   PARM                    ##FYR             3            Year
     C                   PARM                    ##CASE            7            Case
     C                   PARM      'WC'          ##APPL            2            Applic Code
     C                   PARM      DATA          ##DATA           50            Selection criteria
     C                   PARM                    ##CKTP           10            Bank Acct Code
     C                   PARM                    ##RTCD            1            Return Code
      *
     C                   MOVE      ##CKTP        RPCKTP                         Bank Acct# Code
     C                   MOVE      *BLANK        RPACCT
     C     EACKST        IFEQ      *BLANKS
     C                   MOVEL     CISTS         RPSTS
     C                   ELSE
     C     EACKST        IFEQ      'PY'
     C                   MOVEL     CISTS         RPSTS
     C                   ELSE
     C                   MOVE      EACKST        RPSTS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  RPSTS         XXKEY
     C     TBKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PSSRT         RPSORT
     C                   ELSE
     C                   MOVE      *BLANKS       RPSORT
     C                   ENDIF
     C*
     C                   MOVE      'N'           RPSENT
     C                   Z-ADD     *ZERO         RPDSEQ
     C                   MOVE      EAIUSR        RPRQST
     C                   MOVE      *BLANKS       RPLTCD
     C     F1NF(X)       IFEQ      'I'
     C                   MOVE      'I'           RPNTWK
     C                   ELSE
     C                   MOVE      *BLANKS       RPNTWK
     C                   ENDIF
     C                   Z-ADD     EARCN         RPEOBS
     C                   MOVE      *BLANKS       RPFNAL
     C                   MOVEL     *BLANKS       RPCKCN
     C                   Z-ADD     RPDT          RPIDT
     C                   Z-ADD     DSTIME        RPITM
     C                   MOVEL     RPUSER        RPIUSR
     C                   MOVEL     Q1PGM         RPIPGM
     C                   Z-ADD     0             RPRDT
     C                   Z-ADD     0             RPRTM
     C                   MOVE      *BLANKS       RPRUSR
     C                   MOVE      *BLANKS       RPRPGM
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     RPDT          RPRDT
     C                   Z-ADD     DSTIME        RPRTM
     C                   MOVEL     RPUSER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
     C                   ENDIF
     C*
     C     RPAMT         IFGT      0
     C     EAPNWC        ANDNE     'N'
     C     EAPNWC        ANDNE     '3'
     C     EAPNWC        ANDNE     '4'
     C                   WRITE     PRP01
     C                   ENDIF
     C*
     C     NXTC          TAG
     C*
     C                   ENDSR
