     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N93N3R - TYPE N3 BILLING TYPE FORMULA TO CALC BILL AMT
     H**********************************************************************
     H** N3 Billing Calculation:                                     **
     H**  1.  With "N" System type, look up Group's Billing code     **
     H**  2.  Read the EOMB Transaction File (FAEOP)                 **
     H**  2A. Read EOMB Auxiliary File (FAE1P) for Network Code      **
     H**      (NWCD).                                                **
     H**  2B. Read Fund Auxiliary File (FMF1P) for match between     **
     H**      NWCD and F1NM01-10 (NFxx).                             **
     H**  2C. Once match is found determine if this is In-house/     **
     H**      Company Network (NFxx = I).                            **
     H**           Yes ==> Proceed with calculations.                **
     H**           No ===> Do not do calculations.                   **
     H**  3.  Read the Detail Charge Transaction File (FDCAP).       **
     H**  4.  Determine the Effective/termination dates of           **
     H**      Providers contract.                                    **
     H**  5.  Determine if the detail charge's service date falls    **
     H**      within the contract dates.                             **
     H**  6.  If yes, Accumulate the Network Discount Amount         **to
     H**      (CANDIS) in Total Network Discount Amount (XXDISC)     **
     H**  7.  If yes, Accumulate the FS Reductions Amount            **
     H**                CAMRA into Total FS Reductions Amount (XXMRA)**
     H**  8.  XXDISC + XXMRA = Total Network Savings Amount (XXTOT)  **
     H**  9.  XXTOT * Network Bill % (BDPC01) = Total Bill Amt       **
     H**      (QQBAMT).                                              **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   11/11/99      PROJECT: 48MEA                   **
     H**              (Adopted from N93N2R)                          **
     H*****************************************************************
     H** REVISIONS:
     H*  12/13/99  C:62  PROJECT: 48MDZ
     H*                  GET RID OF THE 'I' CONDITION.
     H*                                                               *
     H*****************************************************************
     H**
     FFAEOLG    IF   E           K DISK
     FFAEAL1    IF   E           K DISK
     FFDCALL    IF   E           K DISK
     FFDCAL13   IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FNMBDLB    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMS4L2    IF   E           K DISK
     FFMS4L3    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     FFAE1L     IF   E           K DISK
     FFMF1L     IF   E           K DISK

     D*** FILE ARRAYS
     D*****************************************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  FSY                    1     10
     D                                     DIM(10)                              FILE SYSTEM CODE
     D  FNSY01                 1      1
     D  FNSY02                 2      2
     D  FNSY03                 3      3
     D  FNSY04                 4      4
     D  FNSY05                 5      5
     D  FNSY06                 6      6
     D  FNSY07                 7      7
     D  FNSY08                 8      8
     D  FNSY09                 9      9
     D  FNSY10                10     10
     D                 DS
     D  FBR                    1     30
     D                                     DIM(10)                              FILE BILL/RATE CD
     D  FNBR01                 1      3
     D  FNBR02                 4      6
     D  FNBR03                 7      9
     D  FNBR04                10     12
     D  FNBR05                13     15
     D  FNBR06                16     18
     D  FNBR07                19     21
     D  FNBR08                22     24
     D  FNBR09                25     27
     D  FNBR10                28     30
     D*
     D                 DS
     D  NWC                    1     50
     D                                     DIM(10)                              NETWORK CODE
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
     D*
     D                 DS
     D  NWF                    1     10
     D                                     DIM(10)                              NETWORK FLAG
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXGRP#
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C                   Z-ADD     *ZEROS        Y                 2 0
     C                   Z-ADD     *ZEROS        Z                 2 0
     C                   MOVE      *BLANKS       XXNWCD            5
     C*** KEY LIST FOR EOB FILE FAEOL3
     C*
     C*
     C     EOKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQGRP#
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C*
     C     EAKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQGRP#
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    QQRCN
     C*
     C*
     C*** KEY LIST FOR BD FILE NMBDLB
     C*
     C     BDKEY         KLIST
     C                   KFLD                    XXCODE
     C                   KFLD                    XXDATE            7 0
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQGRP#
     C*
     C*
     C*** KEY LIST FOR CASE DETAIL FILE FDCAL2.
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C                   KFLD                    EOEOB#
     C*
     C     CAKEY1        KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    EARCN                          ADD 09/30/99
     C*
     C*
     C*   KEY LIST FOR THE PROVIDER NETWORK CONTRACT FILE.
     C*
     C     S4KEY         KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C*
     C*  EOMB AUXILIARY FILE
     C*
     C     E1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*  FUND AUXILIARY FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXGRP#
     C*
     C****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    ZZCO#             3
     C                   PARM                    ZZSUB#            5
     C                   PARM                    ZZGRP#            3
     C                   PARM                    ZZEOB#            9
     C                   PARM                    ZZBAMT            9
     C                   PARM                    ZZFYR             3
     C                   PARM                    ZZCASE            7
     C                   PARM                    ZZRCN             3
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
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C****************************************************************
     C*
     C* INITIALIZATION SECTION
     C     *LIKE         DEFINE    EOEOB#        QQEOB#
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*
     C*
     C                   MOVE      ZZCO#         QQCO#             3 0
     C                   MOVE      ZZSUB#        QQSUB#            5 0
     C                   MOVE      ZZGRP#        QQGRP#            3 0
     C                   MOVE      ZZEOB#        QQEOB#
     C                   MOVE      ZZBAMT        QQBAMT            9 2
     C                   MOVE      ZZFYR         QQFYR             3 0
     C                   MOVE      ZZCASE        QQCASE            7 0
     C                   MOVE      ZZRCN         QQRCN             3 0
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*** GET CURRENT 'N3' TYPE RECORD FOR GROUP
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             X                 5 0
     C     'N'           LOOKUP    FSY(X)                                 50
     C     *IN50         IFEQ      '1'
     C*
     C                   MOVEL     FBR(X)        XXCODE            3
     C                   ELSE
     C                   GOTO      TERM
     C                   END
     C                   ELSE
     C                   GOTO      TERM
     C                   END
     C*
     C     BDKEY         SETGT     PBD01
     C     BDREAD        TAG
     C                   READP     PBD01                                  60
     C*
     C     *IN60         IFEQ      '0'
     C     XXCODE        ANDEQ     BDCODE
     C     BDSTAT        CABEQ     'I'           BDREAD
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C     EAKEY         CHAIN     PEA01                              61
     C*
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     EACO#         XXCO#
     C                   Z-ADD     EASUB#        XXSUB#
     C                   Z-ADD     EAFND         XXGRP#
     C                   Z-ADD     EAFYR         XXFYR
     C                   Z-ADD     EACASE        XXCASE
     C                   Z-ADD     EAEOB#        XXEOB#
     C     EOKEY         CHAIN     PEO01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      EONTWK        XXNWCD
     C*
     C     F1KY          CHAIN     PF101                              51
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     1             Z
     C     XXNWCD        LOOKUP    NWC(Z)                                 52
     C     *IN52         IFEQ      '1'
     C*
     C*** BEGIN DEL *** 12/13/99 *************************************
     C********** NWF,Z     ANDEQ'I'
     C*** END   DEL *** 12/13/99 *************************************
     C*
     C*** GET CASE DETAIL RECORDS TO ACCUMULATE NETWORK DISCOUNT AMT.
     C*
     C     CAKEY1        CHAIN     PCA02                              52
     C     *IN52         DOWEQ     '0'
     C*
     C*  GET THE EFFECTIVE AND TERMINATION DATE FOR PROVIDER
     C     S4KEY         CHAIN     PS401                              63
     C     *IN63         IFEQ      '0'
     C                   Z-ADD     S4BGDT        XXFROM            7 0
     C*
     C     S4ENDT        IFEQ      *ZEROS
     C*
     C                   Z-ADD     9999999       XXTO
     C*
     C                   ELSE
     C*
     C                   Z-ADD     S4ENDT        XXTO              7 0
     C*
     C                   ENDIF
     C*
     C                   ELSE                                                   ELSE '0' *IN63
     C     S4KEY         CHAIN     PS402                              63
     C     *IN63         IFEQ      '0'
     C                   Z-ADD     S4BGDT        XXFROM
     C*
     C     S4ENDT        IFEQ      *ZEROS
     C*
     C                   Z-ADD     9999999       XXTO
     C*
     C                   ELSE
     C*
     C                   Z-ADD     S4ENDT        XXTO
     C*
     C                   ENDIF
     C                   END                                                    END '0' *IN63
     C                   END                                                    END '0' *IN63
     C*
     C*** FOR EACH DETAIL RECORD READ, CHECK THAT THE SERVICE DATE IS
     C*** BETWEEN THE EFFECTIVE/TERMINATION DATE OF PROVIDERS CONTRACT.
     C*
     C     CAFDAT        IFGE      XXFROM
     C     CATDAT        ANDLE     XXTO
     C*** TOTAL ONLY RECORDS WITH NETWORK DISCOUNT > 0. THESE WILL BE
     C*** THE RECORDS THAT WERE IN THE CORRECT DATE RANGE AS DETERMINED
     C*** BY THE DISCOUNT PGMS - N94D1R THROUGH N94D5R.
     C                   ADD       CANDIS        XXDISC            9 2
     C                   ADD       CAMRA         XXMRA             9 2
     C                   END                                                    END GE/LE XX
     C*
     C     CAKEY1        READE     PCA02                                  52
     C*
     C                   END                                                    *IN62 = 0
     C*
     C*** ADD THE MRA REDUCTION AMT TO THE NETWORK DISCOUNT AMT
     C*
     C     XXDISC        ADD       XXMRA         XXTOT             9 2
     C*
     C*
     C*** NOW MULTIPLY THE TOTAL NETWORK DISCOUNT IN THE VALID CASE
     C*** DETAIL RECORDS BY THE CORRECT PERCENTAGE.
     C*
     C     XXTOT         MULT      BDPC01        QQBAMT            9 2
     C*
     C* If Calculated Billing Amount is less than the Minimum Bill
     C* amount, then load the Mimimum Bill Amount.
     C*
     C     QQBAMT        IFLT      BDAM01
     C                   Z-ADD     BDAM01        QQBAMT
     C                   ENDIF                                                  QQBAMT<BDPC01
     C*
     C                   ENDIF                                                  *IN52
     C                   ENDIF                                                  *IN51
     C                   ENDIF                                                  *IN50
     C                   END                                                    END EO CHAIN
     C*
     C                   ELSE                                                   ##PRVB = Y
     C*
     C     EOKEY         CHAIN     PEO01                              61
     C*
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     EOCO#         XXCO#
     C                   Z-ADD     EOSUB#        XXSUB#
     C                   Z-ADD     EOFND         XXGRP#
     C                   Z-ADD     EOFYR         XXFYR
     C                   Z-ADD     EOCASE        XXCASE
     C                   Z-ADD     EOEOB#        XXEOB#
     C     E1KY          CHAIN     PE101                              50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     1             Y
     C     5             SUBST     E120A1:Y      XXNWCD
     C*
     C     F1KY          CHAIN     PF101                              51
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     1             Z
     C     XXNWCD        LOOKUP    NWC(Z)                                 52
     C     *IN52         IFEQ      '1'
     C*
     C     NWF(Z)        ANDEQ     'I'
     C*
     C*** GET CASE DETAIL RECORDS TO ACCUMULATE NETWORK DISCOUNT AMT.
     C*
     C     CAKEY         CHAIN     PCA01                              62
     C     *IN62         DOWEQ     '0'
     C*
     C*  GET THE EFFECTIVE AND TERMINATION DATE FOR PROVIDER
     C*
     C     S4KEY         CHAIN     PS401                              63
     C     *IN63         IFEQ      '0'
     C                   Z-ADD     S4BGDT        XXFROM            7 0
     C*
     C     S4ENDT        IFEQ      *ZEROS
     C*
     C                   Z-ADD     9999999       XXTO
     C*
     C                   ELSE
     C*
     C                   Z-ADD     S4ENDT        XXTO              7 0
     C*
     C                   ENDIF
     C*
     C                   ELSE                                                   ELSE '0' *IN63
     C     S4KEY         CHAIN     PS402                              63
     C     *IN63         IFEQ      '0'
     C                   Z-ADD     S4BGDT        XXFROM
     C*
     C     S4ENDT        IFEQ      *ZEROS
     C*
     C                   Z-ADD     9999999       XXTO
     C*
     C                   ELSE
     C*
     C                   Z-ADD     S4ENDT        XXTO
     C*
     C                   ENDIF
     C                   END                                                    END '0' *IN63
     C                   END                                                    END '0' *IN63
     C*
     C*** FOR EACH DETAIL RECORD READ, CHECK THAT THE SERVICE DATE IS
     C*** BETWEEN THE EFFECTIVE/TERMINATION DATE OF PROVIDERS CONTRACT.
     C*
     C     CAFDAT        IFGE      XXFROM
     C     CATDAT        ANDLE     XXTO
     C*** TOTAL ONLY RECORDS WITH NETWORK DISCOUNT > 0. THESE WILL BE
     C*** THE RECORDS THAT WERE IN THE CORRECT DATE RANGE AS DETERMINED
     C*** BY THE DISCOUNT PGMS - N94D1R THROUGH N94D5R.
     C                   ADD       CANDIS        XXDISC            9 2
     C                   ADD       CAMRA         XXMRA             9 2
     C                   END                                                    END GE/LE XX
     C*
     C     CAKEY         READE     PCA01                                  62
     C*
     C                   END                                                    *IN62 = 0
     C*
     C*** ADD THE MRA REDUCTION AMT TO THE NETWORK DISCOUNT AMT
     C*
     C     XXDISC        ADD       XXMRA         XXTOT             9 2
     C*
     C*
     C*** NOW MULTIPLY THE TOTAL NETWORK DISCOUNT IN THE VALID CASE
     C*** DETAIL RECORDS BY THE CORRECT PERCENTAGE.
     C*
     C     XXTOT         MULT      BDPC01        QQBAMT            9 2
     C*
     C* If Calculated Billing Amount is less than the Minimum Bill
     C* amount, then load the Mimimum Bill Amount.
     C*
     C     QQBAMT        IFLT      BDAM01
     C                   Z-ADD     BDAM01        QQBAMT
     C                   ENDIF                                                  QQBAMT<BDPC01
     C*
     C*
     C                   ENDIF                                                  *IN52
     C                   ENDIF                                                  *IN51
     C                   ENDIF                                                  *IN50
     C                   END                                                    END EO CHAIN
     C*
     C                   ENDIF                                                  ##PRVB = Y
     C                   END                                                    END BD VALID
     C*
     C     TERM          TAG
     C                   MOVE      QQBAMT        ZZBAMT
     C                   MOVE      '1'           *INLR
     C*
     C*
