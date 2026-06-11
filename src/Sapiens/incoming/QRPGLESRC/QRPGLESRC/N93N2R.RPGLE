     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H**********************************************************************
     H**********************************************************************
     H/TITLE N93N2R - TYPE N2 BILLING TYPE FORMULA TO CALC BILL AMT
     H**********************************************************************
     H* N2 Billing Calculation:
     H*  1.  With "N" System type, look up Group's Billing code
     H*  2.  Read the EOMB Transaction File (FAEOP)
     H*  2A. Read EOMB Auxiliary File (FAE1P) for Network Code (NWCD).
     H*  2B. Read Fund Auxiliary File (FMF1P) for match between NWCD
     H*      and F1NM01-10 (NFxx).
     H*  2C. Once match is found determine if this is In-house/Company
     H*      Network (NFxx = I).
     H*           Yes ==> Proceed with calculations.
     H*           No ===> Do not do calculations.
     H*  3.  Read the Detail Charge Transaction File (FDCAP)
     H*  4.  Determine the Effective/termination dates of Provider's contract
     H*  5.  Determine if the detail charge's service date falls within
     H*             the contract dates.
     H*  6.  If yes, Accumulate the Network Discount Amount (CANDIS) into
     H*                Total Network Discount Amount (XXDISC)
     H*  7.  If yes, Accumulate the Other Savings Amount (CAOTHR) into
     H*                Total Other Savings Amount (XXOTHR)
     H*  8.  XXDISC + XXOTHR = Total Network Savings Amount (XXTOT)
     H*  9.  XXTOT * Network Bill % (BDPC01) = Total Bill Amt (QQBAMT)
     H**********************************************************************
     H* REVISIONS :                                                   *
     H*                                                               *
     H*  04/16/08  C:62  PROJECT: MC025 63063                         *
     H*                  CORRECT LOOP INDICATORS                      *
     H*                                                               *
     H*  09/30/99  C:62  PROJECT: MC212                               *
     H*                  ADD RCN TO EAKY AND ADD FDCAL13              *
     H*                                                               *
     H*  03/09/99  C:94  PROJECT: MC152
     H*                  CHANGE FDCAL2 TO FDCALL AND CHANGE KLIST
     H*
     H*  01/14/99  C:94  PROJECT: 25MKL
     H*                  CHANGE PROGRAM TO USE FAEAP INSTEAD OF FEAOP
     H*                  AND FAE1P
     H*
     H* 02/19/98 C:62  Project: MC159                                 *
     H*                1) BACKED OUT PROJECT 87MBE (02/06/97)         *
     H*                                                               *
     H* 02/18/98 C:62  Project: MC159                                 *
     H*                1) CHANGED NMPHL & NMHSL WITH FMS4L2/L3.       *
     H*                2) IF XXTO EQ *ZEROS, THEN MAKE IT 9999999.    *
     H*                                                               *
     H* 02/06/97 C:56 PROJECT: 87MBE                                  *
     H*                  REPLACE COMPARISON OF EOMB's NETWORK CODE    *
     H*                  AGAINST GROUP NETWORK CODES WITH COMPARISON  *
     H*                  AGAINST EMPLOYER'S NETWORK CODE.             *
     H*                                                               *
     H* 06/15/95 C:26 PROJECT: AR012                                  *
     H*            1. CALCULATE 'N' BILLING ONLY IF NETWORK FLAG = I  *
     H*            2. ADD BILLING CALCULATION COMMENT                 *
     H*                                                               *
     H* 10/04/89 LJD  - HAD TO CHANGE PROCESSING FOR CALCULATING      *
     H* PROJ #: I0034   BILL AMOUNT.  IT WAS CHECKING TO SEE IF       *
     H*                 CANDIS GT ZERO (ASSUMING THAT IF IT WAS THE   *
     H*                 DISCOUNT PGM HAD ALREADY VERIFIED THAT IT FELL*
     H*                 WITHIN THE CONTRACT DATES). HOWEVER, THIS     *
     H*                 NETWORK BILLING PROGRAM (N2) LOOKS AT BOTH    *
     H*                 CANDIS AND CAOTHR.  AND CANDIS MAY BE EQUAL   *
     H*                 TO ZERO BUT CAOTHR ISNT.  THEREFORE CAOTHR    *
     H*                 WOULD BE NEEDED BUT COULDN'T BE ACCESSED.     *
     H* SOLUTION:     - TAKE OUT CHECK FOR CANDIS GT ZERO             *
     H*               - ADD CODE TO VALIDATE DETAIL CHARGE IS WITHIN  *
     H*                 CONTRACT DATES                                *
     H*                                                               *
     H* 7/19/89 CONNIE TERRY  - ADD CODE TO VERIFY THE SERVICE DATE   *
     H*         FALLS WITHIN THE PROVIDERS CONTRACT EFFECTIVE AND     *
     H*         AND TERMINATION DATES.  EACH CASE DETAIL RECORD MUST  *
     H*         BE CHECKED AGAINST THE PROVIDER DATES WHEN CALCULATING*
     H*         THE BILL AMT, SO THAT ONLY RECORDS IN THE CORRECT DATE*
     H*         RANGE ARE USED.  BECAUSE THE DATE RANGE IS CHECKED IN *
     H*         THE DISCOUNT PGMS, ANY RECORD IN THE VALID RANGE WILL *
     H*         HAVE A DISCOUNT AMT > ZERO.  PROJECT : AD1            *
     H*                                                               *
     F*****************************************************************
     F*** BEG CHG *** 01/14/99 ***************************************
     FFAEOLG    IF   E           K DISK
     F**********FAEOL3  IF  E           K        DISK
     F*** END CHG *** 01/14/99 ***************************************
     F*** BEG ADD *** 01/14/99 ***************************************
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 01/14/99 ***************************************
     F*** ADD NEXT LINE *** 7/18/89
     F*** BEG CHG *** 03/09/99 **************************************
     FFDCALL    IF   E           K DISK
     F***********FDCAL2  IF  E           K        DISK
     F*** END CHG *** 03/09/99 **************************************
     F*** BEGIN ADD *** 09/30/99 *************************************
     FFDCAL13   IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     F*** END   ADD *** 09/30/99 *************************************
     FNMBDLB    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F*** BEGIN CHG *** 02/18/98 ************************************
     F********** NMPHL   IF  E           K        DISK
     F********** NMHSL   IF  E           K        DISK
     FFMS4L2    IF   E           K DISK
     FFMS4L3    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     F*** END   CHG *** 02/18/98 ************************************
     F*** BEGIN ADD *** 06/15/95 **************************************
     FFAE1L     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     F***  END  ADD *** 06/15/95 **************************************
     F*** BEGIN DEL *** 02/19/98 **************************************
     F*** BEGIN ADD *** 02/06/97 **************************************
     F********** FMCML1  IF  E           K        DISK
     F********** FMEML   IF  E           K        DISK
     F*** END ADD *** 02/06/97 ****************************************
     F*** END   DEL *** 02/19/98 **************************************

     D*****************************************************************
     D*** FILE ARRAYS
     D*** BEGIN ADD *** 06/15/95 **************************************
     D*** BEGIN DEL *** 02/06/97 **************************************
     D**********          NWC        10  5               NETWORK CODE
     D*** END DEL *** 02/06/97 ****************************************
     D*** BEGIN ADD *** 02/19/98 **************************************
     D*** END   ADD *** 02/19/98 **************************************
     D***  END  ADD *** 06/15/95 **************************************
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
     D*** BEGIN ADD *** 06/15/95 **************************************
     D*
     D*** BEGIN DEL *** 02/06/97 **************************************
     D**********  DS
     D**********                              1  50 NWC
     D**********                              1   5 F1NW01
     D**********                              6  10 F1NW02
     D**********                             11  15 F1NW03
     D**********                             16  20 F1NW04
     D**********                             21  25 F1NW05
     D**********                             26  30 F1NW06
     D**********                             31  35 F1NW07
     D**********                             36  40 F1NW08
     D**********                             41  45 F1NW09
     D**********                             46  50 F1NW10
     D*** END DEL *** 02/06/97 ****************************************
     D*** BEGIN ADD *** 02/19/98 **************************************
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
     D*** END   ADD *** 02/19/98 **************************************
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
     D***  END  ADD *** 06/15/95 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C*** BEGIN ADD *** 06/15/95 **************************************
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXGRP#
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C*** BEGIN DEL *** 02/19/98 **************************************
     C*** BEGIN ADD *** 02/06/97 **************************************
     C********** *LIKE     DEFN EMEMP#    XXEMP#
     C********** *LIKE     DEFN EMDIV     XXDIV
     C*** END ADD *** 02/06/97 ****************************************
     C*** END   DEL *** 02/19/98 **************************************
     C                   Z-ADD     *ZEROS        Y                 2 0
     C                   Z-ADD     *ZEROS        Z                 2 0
     C                   MOVE      *BLANKS       XXNWCD            5
     C***  END  ADD *** 06/15/95 **************************************
     C*** KEY LIST FOR EOB FILE FAEOL3
     C*
     C*** BEG CHG *** 01/14/99 ***************************************
     C*
     C     EOKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQGRP#
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C********** EOKEY     KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           QQSUB#
     C**********           KFLD           QQEOB#
     C*** END CHG *** 01/14/99 ***************************************
     C*** BEG ADD *** 01/14/99 ***************************************
     C*
     C     EAKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQGRP#
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*** BEGIN ADD *** 09/30/99 **************************************
     C                   KFLD                    QQRCN
     C*** END   ADD *** 09/30/99 **************************************
     C*
     C*** END ADD *** 01/14/99 ***************************************
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
     C*** BEGIN OF ADD  *** 7/18/89 *** CONNIE
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
     C***********          KFLD           EOTRCD  2        DEL 03/09/99
     C                   KFLD                    EOEOB#
     C*
     C*** END OF ADD  *** 7/18/89  *** CONNIE
     C*** BEG ADD *** 01/14/99 ***************************************
     C     CAKEY1        KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C***********          KFLD           EAFEIN           DEL 09/30/99
     C***********          KFLD           EAMOD            DEL 09/30/99
     C***********          KFLD           EATRCD           DEL 03/09/99
     C                   KFLD                    EAEOB#
     C                   KFLD                    EARCN                          ADD 09/30/99
     C*
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C*** BEGIN DEL *** 02/18/98 ************************************
     C*** BEGIN ADD *** 10/04/89 *************************************
     C*   KEY LIST FOR THE HOSPITAL PROVIDER FILE
     C*
     C********** HSKEY     KLIST
     C**********           KFLD           CAFEIN
     C**********           KFLD           CAMOD
     C*
     C*   KEY LIST FOR THE PHYSICIAN PROVIDER FILE
     C*
     C********** PHKEY     KLIST
     C**********           KFLD           CAFEIN
     C**********           KFLD           CAMOD
     C*** END ADD ***** 10/04/89 *************************************
     C*** END   DEL *** 02/18/98 ************************************
     C*** BEGIN ADD *** 02/18/98 ************************************
     C*
     C*   KEY LIST FOR THE PROVIDER NETWORK CONTRACT FILE.
     C*
     C     S4KEY         KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C*** END   ADD *** 02/18/98 ************************************
     C*** BEGIN ADD *** 06/15/95 **************************************
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
     C***  END  ADD *** 06/15/95 **************************************
     C*
     C*** BEGIN DEL *** 02/19/98 **************************************
     C*** BEGIN ADD *** 02/06/97 **************************************
     C*
     C*   CLAIM MASTER FILE
     C*
     C********** CMKEY     KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXSUB#
     C**********           KFLD           XXGRP#
     C**********           KFLD           XXFYR
     C**********           KFLD           XXCASE
     C*
     C*   EMPLOYER MASTER FILE
     C*
     C********** EMKEY     KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXSUB#
     C**********           KFLD           XXGRP#
     C**********           KFLD           XXEMP#
     C**********           KFLD           XXDIV
     C*
     C*** END ADD *** 02/06/97 ****************************************
     C*** END DEL *** 02/19/98 ****************************************
     C*
     C****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    ZZCO#             3
     C                   PARM                    ZZSUB#            5
     C                   PARM                    ZZGRP#            3
     C                   PARM                    ZZEOB#            9
     C                   PARM                    ZZBAMT            9
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   PARM                    ZZFYR             3
     C                   PARM                    ZZCASE            7
     C                   PARM                    ZZRCN             3
     C*** END ADD *** 01/14/99 ***************************************
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
     C*** BEG ADD *** 01/14/99 ***************************************
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
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   MOVE      ZZCO#         QQCO#             3 0
     C                   MOVE      ZZSUB#        QQSUB#            5 0
     C                   MOVE      ZZGRP#        QQGRP#            3 0
     C                   MOVE      ZZEOB#        QQEOB#
     C                   MOVE      ZZBAMT        QQBAMT            9 2
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      ZZFYR         QQFYR             3 0
     C                   MOVE      ZZCASE        QQCASE            7 0
     C                   MOVE      ZZRCN         QQRCN             3 0
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*** GET CURRENT 'N2' TYPE RECORD FOR GROUP
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
     C*** BEG CHG *** 01/14/99 ***************************************
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
     C     NWF(Z)        ANDEQ     'I'
     C*
     C*** GET CASE DETAIL RECORDS TO ACCUMULATE NETWORK DISCOUNT AMT.
     C*
     C*** BEGIN CHG *** 09/30/99 *************************************
     C********** CAKEY1    CHAINPCA01                62
     C*** BEGIN CHG *** 04/16/08 *************************************
     C********** CAKEY1    CHAINPCA02                52
     C     CAKEY1        CHAIN     PCA02                              62
     C*** END   CHG *** 04/16/08 *************************************
     C*** END   CHG *** 09/30/99 *************************************
     C     *IN62         DOWEQ     '0'
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
     C                   ADD       CAOTHR        XXOTHR            9 2
     C                   ADD       CAOTH2        XXOTH2            9 2
     C                   END                                                    END GE/LE XX
     C*
     C*** BEGIN CHG *** 09/30/99 *************************************
     C********** CAKEY1    READEPCA01                    62
     C*** BEGIN CHG *** 04/16/08 *************************************
     C********** CAKEY1    READEPCA02                    52
     C     CAKEY1        READE     PCA02                                  62
     C*** END   CHG *** 04/16/08 *************************************
     C*** END   CHG *** 09/30/99 *************************************
     C*
     C                   END                                                    *IN62 = 0
     C*
     C*** ADD THE OTHER REDUCTION AMT TO THE NETWORK DISCOUNT AMT
     C*
     C     XXDISC        ADD       XXOTHR        XXTOT             9 2
     C     XXTOT         ADD       XXOTH2        XXTOT
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
     C*** BEGIN ADD *** 06/15/95 **************************************
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
     C*** BEGIN DEL *** 02/06/97 **************************************
     C********** XXNWCD    LOKUPNWC,Z                    52
     C********** *IN52     IFEQ '1'
     C*** END DEL *** 02/06/97 ****************************************
     C*** BEGIN ADD *** 02/19/98 **************************************
     C     XXNWCD        LOOKUP    NWC(Z)                                 52
     C     *IN52         IFEQ      '1'
     C*** END   ADD *** 02/19/98 **************************************
     C*
     C*** BEGIN DEL *** 02/19/98 **************************************
     C*** BEGIN ADD *** 02/06/97 **************************************
     C********** CMKEY     CHAINPCM01                52
     C********** *IN52     IFEQ '0'
     C**********           MOVE CMEMP#    XXEMP#
     C**********           MOVE CMDIV     XXDIV
     C*
     C********** EMKEY     CHAINPEM01                53
     C********** *IN53     IFEQ '0'
     C*
     C********** XXNWCD    IFEQ EMNWCD
     C*** END ADD *** 02/06/97 ****************************************
     C*** END   DEL *** 02/19/98 **************************************
     C     NWF(Z)        ANDEQ     'I'
     C***  END  ADD *** 06/15/95 **************************************
     C*
     C*** BEGIN OF ADD  *** 7/18/89  *** CONNIE
     C*
     C*** GET CASE DETAIL RECORDS TO ACCUMULATE NETWORK DISCOUNT AMT.
     C*
     C     CAKEY         CHAIN     PCA01                              62
     C     *IN62         DOWEQ     '0'
     C*
     C*** BEGIN ADD *** 10/04/89 *************************************
     C*  GET THE EFFECTIVE AND TERMINATION DATE FOR PROVIDER
     C*
     C*** BEGIN CHG *** 02/18/98 ************************************
     C********** HSKEY     CHAINPHS01                63
     C     S4KEY         CHAIN     PS401                              63
     C     *IN63         IFEQ      '0'
     C***** BEGIN CHANGE ********** 12/01/97 **************************
     C***********          Z-ADDHSCEDT    XXFROM  60
     C***********          Z-ADDHSTMDT    XXTO    60
     C***********          Z-ADDHSCEDT    XXFROM  70
     C***********          Z-ADDHSTMDT    XXTO    70
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
     C***** END   CHANGE ********** 12/01/97 **************************
     C*** END   CHG *** 02/18/98 ************************************
     C*
     C                   ELSE                                                   ELSE '0' *IN63
     C*** BEGIN CHG *** 02/18/98 ************************************
     C********** PHKEY     CHAINPPH01                63
     C     S4KEY         CHAIN     PS402                              63
     C     *IN63         IFEQ      '0'
     C**********           Z-ADDPHNEDT    XXFROM
     C**********           Z-ADDPHNXDT    XXTO
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
     C*** END   CHG *** 02/18/98 ************************************
     C                   END                                                    END '0' *IN63
     C                   END                                                    END '0' *IN63
     C*
     C*** FOR EACH DETAIL RECORD READ, CHECK THAT THE SERVICE DATE IS
     C*** BETWEEN THE EFFECTIVE/TERMINATION DATE OF PROVIDERS CONTRACT.
     C*
     C     CAFDAT        IFGE      XXFROM
     C     CATDAT        ANDLE     XXTO
     C*** END ADD ***** 10/04/89 *************************************
     C*** BEGIN DEL *** 10/04/89 *************************************
     C*** TOTAL ONLY RECORDS WITH NETWORK DISCOUNT > 0. THESE WILL BE
     C*** THE RECORDS THAT WERE IN THE CORRECT DATE RANGE AS DETERMINED
     C*** BY THE DISCOUNT PGMS - N94D1R THROUGH N94D5R.
     C********** CANDIS    IFGT *ZEROS                      10/04/89 DEL
     C*** END DEL ***** 10/04/89 *************************************
     C                   ADD       CANDIS        XXDISC            9 2
     C                   ADD       CAOTHR        XXOTHR            9 2
     C                   END                                                    END GE/LE XX
     C*
     C     CAKEY         READE     PCA01                                  62
     C*
     C                   END                                                    *IN62 = 0
     C*
     C*** ADD THE OTHER REDUCTION AMT TO THE NETWORK DISCOUNT AMT
     C*
     C     XXDISC        ADD       XXOTHR        XXTOT             9 2
     C*
     C*
     C*** NOW MULTIPLY THE TOTAL NETWORK DISCOUNT IN THE VALID CASE
     C*** DETAIL RECORDS BY THE CORRECT PERCENTAGE.
     C*
     C     XXTOT         MULT      BDPC01        QQBAMT            9 2
     C*** BEGIN ADD *** 06/15/95 **************************************
     C*
     C* If Calculated Billing Amount is less than the Minimum Bill
     C* amount, then load the Mimimum Bill Amount.
     C*
     C     QQBAMT        IFLT      BDAM01
     C                   Z-ADD     BDAM01        QQBAMT
     C                   ENDIF                                                  QQBAMT<BDPC01
     C***  END  ADD *** 06/15/95 **************************************
     C*
     C*** END OF ADD  *** 7/18/89  *** CONNIE
     C*
     C*** DELETE NEXT 2 LINES  *** 7/18/89  *** CONNIE
     C*
     C********** EONDIS    ADD  EOOTHR    XXAMT   92
     C********** XXAMT     MULT BDPC01    QQBAMT  92
     C*
     C*** BEGIN ADD *** 06/15/95 **************************************
     C*** BEGIN DEL *** 02/19/98 **************************************
     C*** BEGIN ADD *** 02/06/97 **************************************
     C**********           ENDIF                           XXNWCD IFEQ
     C**********           ENDIF                           *IN53
     C*** END ADD *** 02/06/97 ****************************************
     C*** END   DEL *** 02/19/98 **************************************
     C                   ENDIF                                                  *IN52
     C                   ENDIF                                                  *IN51
     C                   ENDIF                                                  *IN50
     C***  END  ADD *** 06/15/95 **************************************
     C                   END                                                    END EO CHAIN
     C*
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/14/99 ***************************************
     C                   END                                                    END BD VALID
     C*
     C     TERM          TAG
     C                   MOVE      QQBAMT        ZZBAMT
     C                   MOVE      '1'           *INLR
     C*
     C*
