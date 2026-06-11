     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
      *****************************************************************
     H/TITLE FIXD005A - Retrieve PV1 and other payment attributes
      *****************************************************************
      *
      * DESCRIPTION:  This pgm retrieves the pv1 and other payment
      *               attributes for passed in payment/payment request.
      *
      *
      * CREATED:      06/14/12  C:180    SUPPORT 79581
      ********************************************************************
      * REVISIONS:
      *
     FFMFNL     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
     FWMCIL     IF   E           K DISK
     FFAEAL1    IF   E           K DISK
     FFAEOL3    IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWARPL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
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
      *

     D @BR             S              2    DIM(4)
     D @TC             S              2    DIM(4)
     D @TT             S              1    DIM(4)
     D @RC             S              2    DIM(4)
     D @RT             S              1    DIM(4)
     D @AM             S              9  2 DIM(4)
     D @DS             S             30    DIM(4)
      *
     D DESC2           C                   CONST('Pool Department')
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

     I*
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5            EASUB
     C                   PARM                    @@GRP#            3            EAFND
     C                   PARM                    @@FYR             3            EAFYR
     C                   PARM                    @@CASE            7            EACASE
     C                   PARM                    @@EOB#            9            EAEOB
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@NAMT            9 2
     C                   PARM                    @BR
     C                   PARM                    @TC
     C                   PARM                    @TT
     C                   PARM                    @RC
     C                   PARM                    @RT
     C                   PARM                    @AM
     C                   PARM                    @DS
      *
     C     EOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXEOB#
      *
     C     CIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPTYP
     C                   KFLD                    XXSYS
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP#
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXRCN             3 0
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP#
      *
     C     FNKYWC        KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOFND
      *
     C     ELKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  Process the initialization subroutine
     C*
     C                   EXSR      SRINIT
     C*
     C*  Retrieve the EOMB information
     C*
     C     EOKY          CHAIN     PEO01                              60
     C   60              GOTO      TERM
     C                   MOVE      EORCN         XXRCN
     C*
     C     EAKY          CHAIN     PEA01                              60
     C   60              GOTO      TERM
     C*
     C*  Retrieve the Claim record
     C*
     C     CMKY          CHAIN     PCM01                              60
     C   60              GOTO      TERM
     C*
     C*  Determine Next sequence number for payment requests
     C*
     C     RPKY          SETGT     PRP01
     C     RPKY          READPE    PRP01                                  60
     C   60              Z-ADD     0             RPSEQ#
     C*
     C*  Process the subroutines which create the various payments.
     C*
     C     @@PCIB        IFEQ      'Y'
     C     EASYS         IFEQ      'P'
     C     EASYS         OREQ      'H'
     C     EASYS         OREQ      'N'
     C     EASYS         OREQ      'V'
     C*
     C                   EXSR      SRPROV
     C                   ENDIF
     C                   ENDIF
     C     EASYS         IFEQ      'P'
     C     EASYS         OREQ      'H'
     C     EASYS         OREQ      'U'
     C     EASYS         OREQ      'M'
     C                   EXSR      SRBRS
     C                   ENDIF
     C     EASYS         IFEQ      'P'
     C     EASYS         OREQ      'H'
     C     EASYS         OREQ      'N'
     C                   EXSR      SRNTWK
     C                   ENDIF
     C     EASYS         IFEQ      'P'
     C     EASYS         OREQ      'H'
     C     EASYS         OREQ      'N'
     C     EASYS         OREQ      'V'
     C     FNKYWC        CHAIN     PFN02                              60
     C     *IN60         IFEQ      *OFF
     C     FNCGFL        IFNE      'Y'
     C     @@CLT#        OREQ      '129'
     C     FNFND         ANDEQ     1
     C     @@CLT#        OREQ      '129'
     C     FNFND         ANDEQ     50
     C                   EXSR      SRPGPC
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C******************************************************************
     C*  SRPROV - This subroutine creates the provider payment.
     C******************************************************************
     C     SRPROV        BEGSR
     C*
     C*  Retrieve the provider WCMC check issuance record.
     C*
     C                   MOVE      'P'           XXPTYP
     C                   MOVE      EOSYS         XXSYS
     C     CIKY          CHAIN     PCI01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  Create the payment request record.
     C*
     C                   Z-ADD     XXCO#         RPCO#
     C                   Z-ADD     XXFND         RPFND
     C                   Z-ADD     EOFYR         RPFYR
     C                   Z-ADD     EOCASE        RPCASE
     C                   ADD       1             RPSEQ#
     C*
     C                   Z-ADD     EOTRDT        RPTRDT
     C                   MOVE      EOFEIN        RPFEIN
     C                   MOVE      EOMOD         RPMOD
     C                   MOVE      EAPAID        RPAMT
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CIP1CV        RPBR2
     C                   ELSE
     C     EOBTYP        IFEQ      'X'
     C                   MOVE      CIXBR2        RPBR2
     C                   ELSE
     C                   MOVE      CIBR2         RPBR2
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      EOTRCD        RPTRCD
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
     C                   ENDIF
     C                   Z-ADD     EOEOB#        RPEOB#
      *
     C     RPAMT         IFGT      0
     C                   EXSR      EFTSLT
     C                   ENDIF                                                  RPAMT GT 0
      **
     C                   ENDSR
     C******************************************************************
     C*  SRBRS - This subroutine creates the bill review service pmt.
     C******************************************************************
     C     SRBRS         BEGSR
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
     C                   Z-ADD     XXCO#         RPCO#
     C                   Z-ADD     XXFND         RPFND
     C                   Z-ADD     EOFYR         RPFYR
     C                   Z-ADD     EOCASE        RPCASE
     C                   ADD       1             RPSEQ#
     C*
     C                   Z-ADD     EOTRDT        RPTRDT
     C                   MOVE      CIFEIN        RPFEIN
     C                   MOVE      CIMOD         RPMOD
     C*
     C     EAKY          CHAIN     PEA01                              60
     C     *IN60         IFEQ      *OFF
     C     @@CLT#        IFEQ      '129'
     C     EARCN         ANDGT     0
     C                   GOTO      NXTB
     C                   ENDIF
     C                   Z-ADD     EABAMT        RPAMT
     C                   ELSE
     C   60              GOTO      NXTB
     C                   ENDIF
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
      *
     C     EOSYS         IFEQ      'P'
     C     EOSYS         OREQ      'H'
      *
     C     EATRCD        IFNE      *BLANKS
     C     @@CLT#        ANDNE     '146'
     C                   MOVE      EATRCD        RPTRCD
     C                   ELSE
     C     CMCVFL        IFNE      'Y'
     C                   MOVEL(P)  CITRCD        RPTRCD
     C                   ELSE
     C                   MOVEL(P)  CITRCV        RPTRCD
     C                   ENDIF
     C                   ENDIF
      *
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
     C                   ENDIF
     C                   Z-ADD     EOEOB#        RPEOB#
      *
     C     RPAMT         IFGT      0
     C     EAPBRC        ANDNE     'N'
     C                   EXSR      EFTSLT
     C                   ENDIF                                                  RPAMT GT 0
     C*
     C     NXTB          TAG
     C                   ENDSR
     C******************************************************************
     C*  SRNTWK - This subroutine creates the network payment.
     C******************************************************************
     C     SRNTWK        BEGSR
     C*
     C*  Create the payment request record.
     C*
     C                   Z-ADD     XXCO#         RPCO#
     C                   Z-ADD     XXFND         RPFND
     C                   Z-ADD     EOFYR         RPFYR
     C                   Z-ADD     EOCASE        RPCASE
     C                   ADD       1             RPSEQ#
     C                   Z-ADD     EOTRDT        RPTRDT
     C*
     C                   MOVEL     EONTWK        XXNWCD            5
     C*
     C                   Z-ADD     1             X
     C     XXNWCD        LOOKUP    F1NW(X)                                61
     C*
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
     C*
     C   60              GOTO      NXTC
     C*
     C                   Z-ADD     CIFEIN        RPFEIN
     C                   Z-ADD     CIMOD         RPMOD
     C*
     C* RETRIEVE BILLING AMOUNT:
     C*
     C*
     C   60              GOTO      NXTC
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
     C*
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
     C*
E03  C                   ENDIF
     C*
     C* UPDATE FAEAP/EANAMT WITH CALCULATE NETWORK FEE:
     C*
     C     EARCN         IFEQ      0
     C     RPAMT         ANDLT     0
     C                   Z-ADD     0             @@NAMT
     C                   Z-ADD     0             RPAMT
     C                   ELSE
     C                   Z-ADD     RPAMT         @@NAMT
     C                   ENDIF
     C*
E02  C                   ENDIF
     C*
E01  C                   ENDIF
      *
     C     ELKY1         CHAIN     PEL01                              60
     C                   MOVE      *OFF          @ICC              1
      *
     C                   SELECT
      *
      * Identify Cost-containment PV1
      *
     C     CMJRST        WHENEQ    'CA'
     C     ELPFDT        ANDGE     1100701
     C     *IN60         ANDEQ     *OFF
      *
     C                   MOVE      *BLANKS       XXKEY
      *
     C     CMCVFL        IFEQ      'Y'
     C     CICABC        ANDGT     *BLANKS
     C                   MOVEL(P)  CICABC        XXKEY
     C                   ELSE
     C     CICAB2        IFGT      *BLANKS
     C                   MOVEL(P)  CICAB2        XXKEY
     C                   ENDIF                                                  CICAB2
     C                   ENDIF                                                  CMCVFL = 'Y'
      *
      *  Determine if cost containment PV1 feature is turned on.
      *  Otherwise, use old logic.
      *
     C     TBKY          CHAIN     PB201                              60
     C     *IN60         IFEQ      *OFF
     C     B2CCW         ANDEQ     'Y'
     C     XXKEY         ANDGT     *BLANKS
     C                   MOVEL     XXKEY         RPBR2
     C                   MOVE      *ON           @ICC
     C                   ELSE
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CIP1CV        RPBR2
     C                   ELSE
     C                   MOVE      CIBR2         RPBR2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   OTHER
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CIP1CV        RPBR2
     C                   ELSE
     C                   MOVE      CIBR2         RPBR2
     C                   ENDIF
     C                   ENDSL
      *
     C     EATRCD        IFNE      *BLANKS
     C     @@CLT#        ANDNE     '146'
     C                   MOVE      EATRCD        RPTRCD
     C                   ELSE
      *
     C                   SELECT
      *
      * Cost-containment PV1
      *
     C     @ICC          WHENEQ    *ON
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CICATC        RPTRCD
     C                   ELSE
     C                   MOVE      CICATR        RPTRCD
     C                   ENDIF
      *
     C                   OTHER
     C     CMCVFL        IFNE      'Y'
     C                   MOVEL(P)  CITRCD        RPTRCD
     C                   ELSE
     C                   MOVEL(P)  CITRCV        RPTRCD
     C                   ENDIF
     C                   ENDSL
     C                   ENDIF                                                  EATRCD      '
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
      *
     C     @@CLT#        IFEQ      '139'
     C     RPAMT         ANDNE     0
     C     EAPNWC        ANDNE     'N'
     C     EAPNWC        ANDNE     '3'
     C     EAPNWC        ANDNE     '4'
     C**                   WRITEPRP01   (CHECK REQUEST)
     C                   EXSR      EFTSLT
     C                   ENDIF
      *
     C     NXTC          TAG
     C                   ENDSR
     C******************************************************************
     C*  SRPGPC - This subroutine creates the Public Goods Pool Check
     C******************************************************************
     C     SRPGPC        BEGSR
     C*
     C     EOAST         IFEQ      'NY'
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'P'           XXPTYP
     C                   MOVEL     EOSYS         XXSYS
     C     CIKY          CHAIN     PCI01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     150329043     XXFEIN
     C                   Z-ADD     1             XXMOD
     C                   MOVEL(P)  DESC2         RPRMDS
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     EAINPE        ADD       EAOUTE        RPAMT
     C                   ADD       EAASCE        RPAMT
     C                   ADD       EACLNE        RPAMT
     C*
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE(P)   CIP1CV        RPBR2
     C                   ELSE
     C                   MOVE(P)   CIBR2         RPBR2
     C                   ENDIF
     C*
     C     EATRCD        IFNE      *BLANKS
     C     @@CLT#        ANDNE     '146'
     C                   MOVE      EATRCD        RPTRCD
     C                   ELSE
     C     CMCVFL        IFNE      'Y'
     C                   MOVEL(P)  CITRCD        RPTRCD
     C                   ELSE
     C                   MOVEL(P)  CITRCV        RPTRCD
     C                   ENDIF
     C                   ENDIF
      *
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
     C                   ENDIF
      *
     C     RPAMT         IFGT      *ZEROS
     C                   EXSR      EFTSLT
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C******************************************************************
     C*  EFTSLT - Load pv1, amount, and payment attributes
     C******************************************************************
     C     EFTSLT        BEGSR
     C                   ADD       1             J
     C                   MOVE      RPBR2         @BR(J)
     C                   MOVE      RPTRCD        @TC(J)
     C                   MOVE      RPTYPE        @TT(J)
     C                   MOVE      RPRECD        @RC(J)
     C                   MOVE      RPRETP        @RT(J)
     C                   MOVE      RPAMT         @AM(J)
     C                   MOVE      RPDESC        @DS(J)
     C                   ENDSR
     C******************************************************************
     C*  SRINIT - This subroutine is used to define and initialize
     C*    program variables.
     C******************************************************************
     C     SRINIT        BEGSR
      *
     C                   Z-ADD     1             X                 3 0
     C                   Z-ADD     0             J                 3 0
     C     *LIKE         DEFINE    RPCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOFEIN        XXFEIN
     C     *LIKE         DEFINE    CIPTYP        XXPTYP
     C     *LIKE         DEFINE    CISYS         XXSYS
     C     *LIKE         DEFINE    EOFND         XXGRP#
     C     *LIKE         DEFINE    EOMOD         XXMOD
     C     *LIKE         DEFINE    PCKEY         XXKEY
     C     *LIKE         DEFINE    EANAMT        XXAMT
     C*
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@SUB#        XXSUB#
     C                   MOVE      @@GRP#        XXGRP#
     C                   MOVE      @@EOB#        XXEOB#
     C                   MOVE      @@NAMT        XXAMT
     C                   MOVE      'N'           @@RTCD
      ***** ADDED FROM W7000R->W7304R *** BEGIN ********************
     C                   MOVE      @@CO#         ##CO#
     C                   MOVE      @@GRP#        ##WCF#
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##WCF#            3
     C                   PARM      *BLANKS       ##SUB#            5
     C                   PARM      *BLANKS       ##FND             3
     C*
     C     ##RTCD        IFEQ      ' '
     C                   MOVE      ##SUB#        XXSUB#
     C                   MOVE      ##FND         XXGRP#
     C                   ENDIF
      ***** ADDED FROM W7000R->W7304R *** END   ********************
     C                   Z-ADD     0             XXFND
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      FNWCF#        XXFND
     C                   ENDIF
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLT#            3
      *
     C*
     C*  Retrieve the Group Auxiliary record.
     C*
     C     FNKY          CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEA     *BLANKS       F1NW
     C                   MOVEA     *BLANKS       F1NF
     C                   MOVEA     *ZEROS        F1NI
     C                   MOVEA     *ZEROS        F1NM
     C                   MOVEA     *ZEROS        F1N$
     C                   MOVEA     *BLANKS       F1NT
     C                   ENDIF
      *
     C                   CALL      'SA001R'
01   C                   PARM                    @@WCIF            1            WC INST
02   C                   PARM                    @@MCIF            1            MC INST
03   C                   PARM                    @@WCIN            1            WC INTERFACE
04   C                   PARM                    @@AR              1            MC AR INST
05   C                   PARM                    @@NTWK            1            NETWORK INST
06   C                   PARM                    @@UR              1            PRECERT INST
07   C                   PARM                    @@MED             1            MEDMGMT INST
08   C                   PARM                    @@EDI             1            EDI INST
09   C                   PARM                    @@WCDK            1            WC DARTS KEY
10   C                   PARM                    @@RHAB            1            REHAB INST
11   C                   PARM                    @@MSS             1            MS SUPPORT
12   C                   PARM                    @@FSCH            1            FEE SCHEDULE
13   C                   PARM                    @@PRVB            1            PROVIDER BILLS
14   C                   PARM      ' '           @@NPWC            1
15   C                   PARM      ' '           @@OS              1
16   C                   PARM      ' '           @@ARLS            1
17   C                   PARM      ' '           @@CCOD            1
18   C                   PARM      ' '           @@CRVW            1
19   C                   PARM      ' '           @@PROM            1
20   C                   PARM      ' '           @@MCRV            1
21   C                   PARM      ' '           @@FREQ            1
22   C                   PARM      ' '           @@CRAC            1
23   C                   PARM      ' '           @@PREO            1
24   C                   PARM      ' '           @@PARN            1
25   C                   PARM      ' '           @@MGRI            1
26   C                   PARM      ' '           @@KCIF            1
27   C                   PARM      ' '           @@CURS            1
28   C                   PARM      ' '           @@PCIB            1
     C                   ENDSR
