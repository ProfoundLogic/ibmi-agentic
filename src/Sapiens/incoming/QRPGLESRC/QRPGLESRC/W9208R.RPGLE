     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE  W9208R - J. D. EDWARDS A/R INTERFACE PROGAM
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*           THIS PROGRAM WILL TRANSFER ON A DAILY BASIS RECORDS *
     H*    FROM THE A/R TRANSACTION (WMEAP) FILE TO THE J.D. EDWARDS  *
     H*    FRONT FILE (F57901).                                       *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W9208C                                                      *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: DAVID MONCRIEF                                    *
     H* CREATED: 10/02/91               PROJECT: MD022                *
     H*****************************************************************
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*   50  GENERAL CHAINING                                        *
     H*   60  PRIMARY READ ON A/R TRANSACTION FILE                    *
     H*   LR  LAST RECORD INDICATOR                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* SPECIAL FILE USAGE:                                           *
     H*                                                               *
     H*    WMEAL  - QUERIED FOR RECORDS WITH POSTING OR TRANSACTION   *
     H*              DATE = TO PARM DATE.                             *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/20/11 C:136 SUPPORT: WA025 78415                          *
     H*                 CLIENT REQUESTED CHG IN HOW DOCUMENT TYPE     *
     H*                 (Q1BPTY) IS POPULATED.                        *
     H*                                                               *
     H*  08/23/10 C:65  PROJECT 25WQI/SUPP#73846                      *
     H*                 CHANGE THE FYR PORTION AGAIN TO BE BASED ON   *
     H*                 9/1 EACH YEAR.                                *
     H*                                                               *
     H*  08/16/10 C:65  PROJECT 25WQI/SUPP#73846                      *
     H*                 PREVENT PAYMENTS FROM TRANSFERRING OVER.      *
     H*                 CHANGE THE FYR PORTION OF Q1MCU AND Q1MCU2    *
     H*                 FIELDS TO PULL BASED ON BATCH DATE RATHER     *
     H*                 FROM A/R FYR FIELD (LIKE W9209R DOWNLOAD).    *
     H*                                                               *
     H*  03/03/10 C:191 PROJECT 25WQI PRISS #28                       **
     H*                 DATA ERROR
     H*                                                               *
     H*  11/17/09 C:191 PROJECT 25WQI                                 **
     H*                 Write output to new record format F5503B11    *
     H*                                                               *
     H*  12/08/04 C:09 PROJECT 25WNY                                  **
     H*                Set up ASO Cash transactions to hit cost       *
     H*                center 4, account 1212.                        *
     H*                                                               *
     H*  05/26/00 C:61 PROJECT 25WMQ                                  **
     H*                Set up deposit transactions (payments) to hit  *
     H*                cost center 4, account 2185.                   *
     H*                                                               *
     H*  01/20/00 C:15 Project: 25WJK                                 *
     H*                Chg code to load Q1AN8 with pos 2-6 of EMSEMP. *
     H*                                                               *
     H*  11/10/98 C:61 PROJECT 25WJK                                  *
     H*                Convert for V3.  Clean comments from program.  *
     H*                                                               *
     H*  01/21/98 C:86 PROJECT# 25WIX                                 *
     H*            1.  IF RECORD TYPE = 2 AND (OFFSET >=4000 & <=8999,*
     H*                Q1PO = '5'FYR ELSE IF OFFSET =1230 OR 1201,    *
     H*                Q1PO = '4' ELSE Q1PO = '5').                   *
     H*            2.  Q1CO = BLANKS.                                 *
     H*            3.  IF OBJECT ACCT  >=4000 & <=8999 Q1MCU = '5'FYR,*
     H*                ELSE IF OBJECT ACCT = 1230 OR 1201 Q1CMU = '4' *
     H*                ELSE Q1CMU = '5'.                              *
     H*                                                               *
     H*  08/07/95 C:33 PROJECT# 25WBA                                 *
     H*                REPLACE HARDCODING OF FND 100 WITH CASH FLOW   *
     H*                FUND FLAG (WDFQP/F1CFFL)                       *
     H*                                                               *
     H*  01/13/94 C:09 PER REQUEST FROM PAM M., MODIFY CODE THAT IS   *
     H*                WRITING AUDIT ADJUSTMENT TYPE ('D') RECORDS    *
     H*                TO NO LONGER RETRIEVE THE CURRENT FUND YEAR    *
     H*                BUT USE THE FUND YEAR OF THE AUDIT RECORD      *
     H*                (EAFYR) INSTEAD.                               *
     H*                                                               *
     H* 11/04/93  C04  PROJECT MD022                                  *
     H*                MOVED AUDIT DATE COMPARISON TO BE WITHIN THE   *
     H*                CANCEL CONDITION.  WE DO NOT WANT TO TRANSFER  *
     H*                FUTURE D TYPE BECAUSE WE ARE NOT TRANSFERRING  *
     H*                FUTURE B AND A TYPES.  BUT WE DO NOT WANT THIS *
     H*                CONDITION ON ACTIVE ACCOUNTS.                  *
     H*                                                               *
     H* 02/08/93  R.HINDS  TASB                                       *
     H*                UNCOMMENTED THE CODE IN SRWRIT THAT MOVES THE  *
     H*                INVOICE NUMBER TO THE DOCUMENT NUMBER          *
     H*                                                               *
     H* 04/13/92  R.HINDS  TASB                                       *
     H*                ADDED CODE TO IGNORE FUND NUMBER 999 WHICH     *
     H*                IA COMMONLY USED FOR TESTING PURPOSES        D *
     H*                                                               *
     H* 03/31/92  J.SCHAIBLE TASB                                     *
     H*                CHANGED CODE TO CREATE JOURNAL AND AP RECORDS  *
     H*                INSTEAD OF AR AND AP RECORDS. THE CHANGES MADE *
     H*                ARE AS FOLLOWS:                                *
     H*                    1. FOR 02 RECORDS MOVE THE BATCH NUMBER    *
     H*                    2. FOR QI@DTY = 03, MOVE Q1AN8 TO Q1PO     *
     H*                    3. REMOVE ALL AMOUNT REVERSAL CODE THIS    *
     H*                       WILL BE HANDLED IN THE NEXT PROGRAM     *
     H*                    4. REMOVE CALCULATION OF POSTING DATE      *
     H*                                                               *
     H* 01/23/92  R.HINDS  TASB                                       *
     H*                ADDED CODE TO GENERATE AN INVOICE NUMBER IF    *
     H*                IT IS 0. IF ARCODE IS CH, MAKE INVOICE # BASED *
     H*                ON TRDATE. IF ARCODE IS UN, UC OR AP, GET AN   *
     H*                INVOICE NUMBER FROM WMFNL. UPDATE THE A/R      *
     H*                RECORD W/ THE NEW INVOICE NUMBER               *
     H*                                                               *
     H* 12/19/91  JJJ  PROJECT MD022                                  *
     H*                VARIOUS CHANGES PER PAM MYERS REQUEST DATED    *
     H*                12/19.                                         *
     H*                                                               *
     H* 11/07/91  DVM  - ADDED CODE TO ONLY LOAD FUND YEAR TO THE     *
     H* PROJECT MD022     COST CENTER IF THE G/L OBJECT ACCOUNT OR    *
     H*                   G/L OFFSET IS EQUAL TO '4100'.              *
     H*                  ALSO COMMENTED OUT CODE THAT LOAD A VALUE    *
     H*                   TO THE CHECK DATE.                          *
     H*                                                               *
     H* PLEASE ADD TO TOP                                             *
     H*****************************************************************
      *
      *
     F*** BEGIN CHG ****** 11/17/09 ************************************
     F************WMEAL   UF  E           K        DISK
     FWMEAL     IF   E           K DISK
     FWMA#L     IF   E           K DISK
     F                                     RENAME(PEA01:PA#01)
     FWMJEL     IF   E           K DISK
     F*************WMFNL   UF  E           K        DISK
     FWMFNL     IF   E           K DISK
     F***********WDF1L   IF  E           K        DISK
     F***********            PF101                             KRENAMEPF102
     F*** END   CHG ****** 11/17/09 ************************************
     F*** BEGIN ADD ***  08/16/10  *************************************
     F*** BEGIN DEL ****** 08/23/10 ************************************
     F*****WDF1L1  IF  E           K        DISK
     F*** END DEL ******** 08/23/10 ************************************
     F*** END ADD *****  08/16/10  *************************************
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F*** BEGIN CHG ****** 11/17/09 ************************************
     F***************F57901  O   E                    DISK
     FF5503B11  O    E             DISK
      *
     F*** END   CHG ****** 11/17/09 ************************************
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** BEGIN DEL ****** 11/17/09 ************************************
      *  GENERAL LEDGER DATE
     D**********  DS
     D**********                              1   70X@GLDT
     D**********                              2   30Q1DGY
     D**********                              4   50Q1DGM
     D**********                              6   70Q1DGD
      *
      *  DATE ENTERED
      *
     D**********  DS
     D**********                              1   70X@DTE
     D**********                              2   30Q1DEY
     D**********                              4   50Q1DEM
     D**********                              6   70Q1DED
      *
      *  INVOICE DATE
      *
     D**********  DS
     D**********                              1   70X@INDT
     D**********                              2   30Q1DIY
     D**********                              4   50Q1DIM
     D**********                              6   70Q1DID
     D*** END   DEL ****** 11/17/09 ************************************
      *
      **** BEGIN ADD ****** 11/17/09 ************************************
      *  ORDER ID
      *
     D                 DS
     D  XXBORI                 1     50
     D  EAFND                  1      3  0
     D  EAEMP#                 4     12  0
     D  EADIV                 13     17  0
     D  EAFYR                 18     20  0
     D  EAARCD                21     22
     D  XXPODT                23     30  0
     D  XXDT                  31     38  0
     D  EATIME                39     44  0
      *  DATE FIELD WITH SEPARATORS
      *
     D                 DS
     D  XXDT10                 1     10
     D  XXDTMM                 1      2
     D  XXSEP1                 3      3
     D  XXDTDD                 4      5
     D  XXSEP2                 6      6
     D  XXDTYY                 7     10
      *
      *  DATE FIELD
      *
     D                 DS
     D  XXDT8                  1      8
     D  XXDTM8                 1      2
     D  XXDTD8                 3      4
     D  XXDTY8                 5      8
      **** END   ADD ****** 11/17/09 ************************************
     D*** BEGIN ADD ***  08/23/10  *************************************
     D                 DS
     D  X1DATE                 1      7  0
     D  XXDTY                  1      3  0
     D  XXDTM                  4      5  0
     D  XXDTD                  6      7  0

     I*** END ADD *****  08/23/10  *************************************
      *
      ****************************************************************
      *    LIKE DEFINES
      ****************************************************************
      *

     C     *LIKE         DEFINE    EACO#         XXCO#
     C     *LIKE         DEFINE    EAFND         XXFND
     C     *LIKE         DEFINE    EAFYR         XXFYR
     C     *LIKE         DEFINE    EAEMP#        XXEMP#
     C     *LIKE         DEFINE    EADIV         XXDIV
     C     *LIKE         DEFINE    EATRDT        XXEFDT
     C     *LIKE         DEFINE    EATRDT        XXDATE
     C*** BEGIN DEL****  08/23/10  *************************************
     C*********  *LIKE     DEFN EATRDT    X1DATE
     C*** END DEL *****  08/23/10  *************************************
      *
      ****************************************************************
      *    KEY LISTING
      ****************************************************************
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C*** BEGIN ADD ***  08/16/10  *************************************
     C*** BEGIN DEL ****** 08/23/10 ************************************
      *
     C*****      F1KY      KLIST
     C*****                KFLD           XXCO#
     C*****                KFLD           XXFND
     C*****                KFLD           X1DATE
      *
     C*****      F1KY1     KLIST
     C*****                KFLD           XXCO#
     C*****                KFLD           XXFND
     C*** END DEL ******** 08/23/10 ************************************
     C*** END ADD *****  08/16/10  *************************************
      *
     C     F1KY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ELFYR
      *
     C     F1KY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      *
     C     BSKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      *
     C     TTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      ****************************************************************
      *    ENTRY PARMS
      ****************************************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQRNDT            8
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      ****************************************************************
      *    SET UP AND INITIALIZE
      ****************************************************************
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DATE
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQRNDT        XXTMP1            8 0
     C                   Z-ADD     XXTMP1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
      *
     C                   MOVE      'N'           XXTRAN            1            TRANSFER REC.
     C*** BEGIN DEL ****** 11/17/09 ************************************
     C**************       MOVE 'N'       XXSELF  1        TRANSFER REC.
     C*** BEGIN DEL ****** 11/17/09 ************************************
     C                   MOVE      'Y'           #FSTIM            1            FIRST TIME FLAG
     C*** BEGIN ADD ****** 11/17/09 ************************************
     C                   Z-ADD     000           XXFND
     C                   MOVE      ' '           XXHLDF            1
      *
      * CHAIN TO JDE CONTROL FILE
     C     XXCO#         CHAIN     PJE01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     JESTCD        Q1BSTS
     C                   MOVEL     JEITYP        Q1BINT
     C                   ENDIF                                                  *IN50='0'
     C*** END   ADD ****** 11/17/09 ************************************
      *
      ****************************************************************
      *    START READING A/R TRANSACTION FILE
      ****************************************************************
      *
     C*** BEGIN ADD ****** 11/17/09 ************************************
     C* Process WMEAP and when done process Holding file (WMA#P)
     C     XXHLDF        IFEQ      ' '
     C*** END   ADD ****** 11/17/09 ************************************
     C                   READ      PEA01                                  60
     C*** BEGIN ADD ****** 11/17/09 ************************************
     C   60              MOVE      'Y'           XXHLDF
     C                   ENDIF                                                  XXHLDF=' '
     C     XXHLDF        IFEQ      'Y'
     C                   READ      PA#01                                  60
     C                   ENDIF                                                  XXHLDF='Y'
     C*** END   ADD ****** 11/17/09 ************************************
      *
     C     *IN60         DOWEQ     '0'
      *
      *  CHAIN TO EMPLOYER DETAIL FILE TO GET BILLING TYPE
      *
     C*** BEGIN CHG ****** 11/17/09 ************************************
     C*************        Z-ADDEACO#     XXCO#
     C*************        Z-ADDEAFND     XXFND
     C* Get Fund record whenever fund changes
     C     EACO#         IFNE      XXCO#
     C     EAFND         ORNE      XXFND
     C                   Z-ADD     EACO#         XXCO#
     C                   Z-ADD     EAFND         XXFND
     C     FNKY          CHAIN     WMFNL                              65
     C  N65              MOVE      FNTEST        XXTEST            1
     C                   ENDIF                                                  EAFND NE XFND
     C*** END   CHG ****** 11/17/09 ************************************
      *
     C                   Z-ADD     EAFYR         XXFYR
     C                   Z-ADD     EAEMP#        XXEMP#
     C                   Z-ADD     EADIV         XXDIV
     C     ELKY1         CHAIN     PEL01                              50
     C     *IN50         IFEQ      '0'
      *
     C*** BEGIN DEL ****** 11/17/09 ************************************
     C* Specific processing for S/R policies is being removed
      ************ RETRIEVE BILLING SCHEDULE CODE TO DETERMINE IF
      ************ SELF-REPORTING
      **********
     C**********           MOVE 'N'       XXSELF
     C**********           MOVE *BLANKS   XXKEY
     C**********           MOVELELBSCD    XXKEY
     C********** BSKY1     CHAINPBS01                50
     C********** *IN50     IFEQ '0'
     C********** BSSEFL    ANDEQ'S'
     C**********           MOVE 'Y'       XXSELF
     C**********           END
     C*** END   DEL ****** 11/17/09 ************************************
      *
      * DETERMINE IF THE RECORD SHOULD BE TRANSFERRED. IF THE CONDITION
      *   FOR TRANSFER ARE MET THEN XXTRAN WILL EQUAL 'Y'
      *
     C                   MOVE      'N'           XXTRAN
      *
     C*** BEGIN DEL ****** 11/17/09 ************************************
     C********** XXSELF    IFEQ 'Y'                        SELF-REPORTING
      **********
     C********** EAARTY    IFEQ 'H'
     C********** EAARTY    OREQ 'A'
     C********** EAPODT    IFEQ XXDATE
     C**********           MOVE 'Y'       XXTRAN
     C**********           END
     C**********           ELSE
     C********** EATRDT    IFEQ XXDATE
     C**********           MOVE 'Y'       XXTRAN
     C**********           END
     C**********           END                             EAARTY = H OR A
      **********
     C**********           ELSE                            XXSELF = 'N'
     C*** END   DEL ****** 11/17/09 ************************************
      *
     C*** BEGIN ADD ****** 11/17/09 ************************************
     C* Only process billing A/R records (EAARTY = 'B', 'A', 'D', 'F', or 'H')
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'F'
     C     EAARTY        OREQ      'H'
     C                   MOVE      'Y'           XXTRAN
     C*** BEGIN DEL ***  08/16/10  *************************************
     C*********            ENDIF
     C*** END DEL *****  08/16/10  *************************************
     C*** END   ADD ****** 11/17/09 ************************************
     C*
     C*** BEGIN CHG ****** 11/17/09 ************************************
     C********** EATRDT    IFGT EAPODT
     C********** EATRDT    IFEQ XXDATE
     C**********           MOVE 'Y'       XXTRAN
     C**********           END
     C**********           ELSE
     C********** EAPODT    IFEQ XXDATE
     C**********           MOVE 'Y'       XXTRAN
     C**********           END
     C**********           END                             TRDT > PODT
     C*
     C* Only check to include the record will be the transaction date EATRDT equ
     C* and the maintenance date EADT equal to the current date for WMA#P
     C     XXHLDF        IFEQ      ' '
     C     EATRDT        ANDEQ     XXDATE
     C     XXHLDF        OREQ      'Y'
     C     EADT          ANDEQ     XXDATE
     C                   MOVE      'Y'           XXTRAN
     C                   ELSE
     C                   MOVE      'N'           XXTRAN
     C                   ENDIF
      *
     C* Negate amount for holding file processing
     C     XXHLDF        IFEQ      'Y'
     C                   MULT      -1            EAAMT
     C                   ENDIF
     C*** END   CHG ****** 11/17/09 ************************************
     C*
      *
     C*** BEGIN ADD ***  08/16/10  *************************************
     C                   ENDIF
     C*** END ADD *****  08/16/10  *************************************
     C*** BEGIN DEL ****** 11/17/09 ************************************
     C**********           END                             XXSELF = 'Y'
      *
     C* Do not check policy cancellation date
      ***********  DO NOT TRANSFER RECORD ACCOUNT IS CANCELLED AND RECORD TYPE
      ***********   IS 'B' OR 'A'.  IF RECORD TYPE IS 'D' THEN ONLY TRANSFER IF
      ***********   POSTING DATE = TRANSACTION DATE.
      **********
     C********** F1KY3     CHAINPF102                60
     C********** EAARTY    IFEQ 'A'
     C********** EAARTY    OREQ 'B'
     C********** EAPODT    IFGT ELPTDT
     C********** F1CFFL    ANDNE'Y'
     C**********           MOVE 'N'       XXTRAN
     C**********           END
     C**********           END
      **********
      **********
     C********** EAARTY    IFEQ 'D'
     C********** EAPODT    ANDNEEATRDT
     C********** F1KY2     CHAINPF102                60
     C********** *IN60     IFEQ '0'
     C********** ELPTDT    ANDLTF1ENPO
     C**********           MOVE 'N'       XXTRAN
     C**********           END
     C**********           END
     C*** END   DEL ****** 11/17/09 ************************************
      *
      *  IF TRANSFER FLAG = 'Y' THEN EXCUTE SUBROUTINE TO
      *   WRITE RECORD TO INTERFACE FILE
      *
     C*** BEGIN CHG ****** 11/17/09 ************************************
      ************** CHECK ALSO FOR FUND = 999                 4-13-92  RJH
      * Check for FNTEST = 'Y' instead of EAFND = 999
      *
     C     XXTRAN        IFEQ      'Y'
     C********   EAFND     ANDNE999
     C     XXTEST        ANDNE     'Y'
     C*** END   CHG ****** 11/17/09 ************************************
     C                   EXSR      SRINZ
     C                   EXSR      SRWRIT
     C                   END
      *
     C                   END                                                    ELKY1 = 0
      *
      *   READ NEXT RECORD
      *
     C*** BEGIN ADD ****** 11/17/09 ************************************
     C* Process WMEAP and when done process Holding file (WMA#P)
     C     XXHLDF        IFEQ      ' '
     C*** END   ADD ****** 11/17/09 ************************************
     C                   READ      PEA01                                  60
     C*** BEGIN ADD ****** 11/17/09 ************************************
     C   60              MOVE      'Y'           XXHLDF
     C                   ENDIF                                                  XXHLDF=' '
     C     XXHLDF        IFEQ      'Y'
     C                   READ      PA#01                                  60
     C                   ENDIF                                                  XXHLDF='Y'
     C*** END   ADD ****** 11/17/09 ************************************
     C                   END                                                    *IN60 = 0
      *
     C                   MOVE      '1'           *INLR
      *
      ************************************************************************
      *  SRINZ - INITIALIZE INTERFACE FILE FIELDS                            *
      ************************************************************************
      *
     C     SRINZ         BEGSR
      *
     C*** BEGIN CHG ****** 11/17/09 ************************************
     C                   MOVE      *BLANKS       Q1@TY             2
     C**********           MOVE *BLANKS   Q1DCT   2
     C**********           MOVE *ZEROS    Q1DOC   80
     C**********           MOVE *BLANKS   Q1PO    8
     C**********           MOVE *ZEROS    Q1AN8   80
     C**********           MOVE *BLANKS   Q1CO    3
     C**********           MOVE *BLANKS   Q1MCU  12
     C**********           MOVE *BLANKS   Q1MCU2 12
     C**********           MOVEL'5'       Q1MCU2
     C**********           MOVE *BLANKS   Q1OBJ   4
     C**********           MOVE *BLANKS   Q1SUB   8
     C**********           MOVE *BLANKS   Q1SBL   8
     C**********           MOVE *ZEROS    Q1AA   150
     C**********           MOVE *ZEROS    Q1AG   150
     C**********           MOVE *BLANKS   Q1RMK  30
     C**********           MOVE *BLANKS   Q1EXA  30
     C**********           MOVE *BLANKS   Q1EXR  30
     C**********           MOVE *ZEROS    X@GLDT           G/L DATE
     C**********           MOVE *ZEROS    X@DTE            DATE ENTERED
     C**********           MOVE *BLANKS   Q1DCTM  2
     C**********           MOVE *ZEROS    Q1DOCM  80
     C**********           MOVE *ZEROS    X@INDT           INVOICE DATE
     C**********           MOVE *BLANKS   Q1GLC   4
     C**********           MOVE 'PDS'     Q1WR01  3
     C**********           MOVE 'O'       Q1PTC   1
     C**********           MOVE *BLANKS   Q1ICUT  1
     C**********           MOVE *ZEROS    Q1ICU   80
      *
     C                   MOVE      *BLANKS       Q1BSTS
     C                   MOVE      *BLANKS       Q1BINT
     C*** BEGIN CHG ****** 03/03/10 ************************************
     C**********           MOVE *ZEROS    Q1BPTY
     C                   MOVE      *BLANKS       Q1BPTY
     C*** END   CHG ****** 03/03/10 ************************************
     C                   MOVE      *ZEROS        Q1AN8
     C                   MOVE      *ZEROS        Q1BORI
     C                   Z-ADD     1             Q1LIN
     C                   MOVE      *BLANKS       Q1DCT
     C                   MOVE      *ZEROS        Q1DOC
     C                   MOVE      '001'         Q1SFX
     C                   MOVE      *BLANKS       Q1CO
     C                   MOVE      *BLANKS       Q1MCU
     C                   MOVE      *BLANKS       Q1MCU2
     C                   MOVE      *BLANKS       Q1OBJ
     C                   MOVE      *BLANKS       Q1SUB
     C                   MOVE      *BLANKS       Q1SBL
     C                   MOVE      *BLANKS       Q1SBLT
     C                   MOVE      *ZEROS        Q1AA
     C                   MOVE      *ZEROS        Q1AG
     C                   MOVE      *BLANKS       Q1EXA
     C                   MOVE      *BLANKS       Q1EXR
     C                   MOVE      *BLANKS       Q1PTC
     C                   MOVE      *ZEROS        Q1GLDT
     C                   MOVE      *ZEROS        Q1DTEN
     C                   MOVE      *ZEROS        Q1CKAM
     C*** BEGIN CHG ****** 03/03/10 ************************************
     C*********            MOVE *ZEROS    Q1DMTJ
     C                   MOVE      *BLANKS       Q1DMTJ
     C*** END   CHG ****** 03/03/10 ************************************
     C                   MOVE      *BLANKS       Q1CKNU
     C                   MOVE      *ZEROS        Q1PAAP
     C                   MOVE      *ZEROS        Q1AAP
     C                   MOVE      *BLANKS       Q1GLBA
     C                   MOVE      *BLANKS       Q1GLC
     C                   MOVE      *ZEROS        Q1DI
     C                   MOVE      *ZEROS        Q1DDJ
     C*** BEGIN ADD ****** 03/03/10 ************************************
     C**********           MOVE 'AR'      Q1JDMD
     C                   MOVEL     'AR'          Q1JDMD
     C*** END   ADD ****** 03/03/10 ************************************
     C                   MOVE      *BLANKS       Q1LNTY
     C                   MOVE      *ZEROS        Q1UORG
     C                   MOVE      *BLANKS       Q1LITM
     C                   MOVE      *BLANKS       Q1DSC1
     C                   MOVE      *BLANKS       Q1DSC2
     C                   MOVE      *BLANKS       Q1PMCT
     C* DATE SEPARATOR
     C                   MOVE      '/'           XXSEP1
     C                   MOVE      '/'           XXSEP2
     C*** END   CHG ****** 11/17/09 ************************************
      *
     C                   ENDSR
      *
      ************************************************************************
      *  SRWRIT- WRITE RECORD TO INTERFACE FILE                              *
      ************************************************************************
      *
     C     SRWRIT        BEGSR
      *
     C*** BEGIN ADD ****** 11/17/09 ************************************
      * CHAIN TO JDE CONTROL FILE
     C     XXCO#         CHAIN     PJE01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     JESTCD        Q1BSTS
     C                   MOVEL     JEITYP        Q1BINT
     C                   ENDIF                                                  *IN50='0'
      *
      *** BEG CHG ***** 12/20/11 ***************************************
     C***********          SELEC
     C***********FNPFFL    WHEQ 'C'
     C***********          MOVEL'WCASO '  Q1BPTY
     C***********ELAMAG    WHNE 0
     C***********          MOVEL'WCAD  '  Q1BPTY
     C***********          OTHER
     C***********          MOVEL'WCPOOL'  Q1BPTY
     C***********          ENDSL
     C                   SELECT
     C     ELQTYP        WHENEQ    'CO'
     C                   MOVEL     'WCASO '      Q1BPTY
     C     ELQTYP        WHENEQ    'RD'
     C                   MOVEL     'WCAD  '      Q1BPTY
     C     ELQTYP        WHENEQ    'RP'
     C                   MOVEL     'WCPOOL'      Q1BPTY
     C                   OTHER
     C                   MOVEL     'WCOTHER'     Q1BPTY
     C                   ENDSL
      *** END CHG ***** 12/20/11 ***************************************
      *
     C                   MOVE      EANINV        Q1EXA
     C*** END   ADD ****** 11/17/09 ************************************
      *
      *  CHAIN TO EMPLOYER MASTER FILE TO GET DISTRICT
      *
     C     EMKY          CHAIN     PEM01                              50
     C*** BEGIN DLT *** 01/20/00 **************************************
     C**N50*************** MOVELEMSEMP    Q1AN8
     C*** END DLT ***** 01/20/00 **************************************
     C*** BEGIN ADD *** 01/20/00 **************************************
     C                   Z-ADD     0             Q1AN8
     C     *IN50         IFEQ      '0'
     C     6             SUBST     EMSEMP:2      XX6A              6
     C                   MOVE      XX6A          Q1AN8
     C                   END
     C*** END ADD ***** 01/20/00 **************************************
      *
     C*** BEGIN CHG ****** 11/17/09 ************************************
     C* BUILD ORDERID - CONCATENATION OF EAFND, EAEMP#, EADIV, EAFYR, EAARCD
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT
     C*
     C                   Z-ADD     EADT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   MOVEL     XXBORI        Q1BORI
     C* LOAD GL DATE WITH SEPARATOR
     C                   MOVE      XXPODT        XXDT8
     C                   EXSR      SRLDDT
     C                   MOVE      XXDT10        Q1GLDT
     C                   MOVE      Q1GLDT        Q1DI
     C                   MOVE      Q1GLDT        Q1DDJ
     C*
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXWKDT            8 0
     C*
     C                   MOVE      XXWKDT        XXDT8
     C                   EXSR      SRLDDT
     C                   MOVE      XXDT10        Q1DTEN
     C*** END   CHG ****** 11/17/09 ************************************
      *
      *  CHAIN TO A/R TABLE FILE
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TTKY          CHAIN     PTT01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      TT1@TY        Q1@TY
     C                   MOVE      TT1DCT        Q1DCT
     C                   MOVE      TT1OBJ        Q1OBJ
     C                   MOVE      TT1GLC        Q1GLC
     C                   END
      *
      *
     C*** BEGIN DEL ****** 11/17/09 ************************************
     C********** Q1@TY     IFEQ '02'
     C**********           Z-ADDEABUL1    Q1ICU
     C**********           END
     C********** Q1@TY     IFEQ '03'
     C**********           MOVE Q1AN8     Q1PO
     C**********           END
     C*** END   DEL ****** 11/17/09 ************************************
     C*** BEGIN ADD ****** 08/16/10 ************************************
      *
      * DETERMINE CURRENT FUND YEAR
     C*** BEGIN CHG ****** 08/23/10 ************************************
     C********             Z-ADDEAFYR     XNFYR   30
     C********   F1KY      SETGTPF101
     C********   F1KY1     REDPEPF101                    60
     C********   *IN60     IFEQ '0'
     C********             Z-ADDF1FYR     XNFYR
     C********             ENDIF                           *IN60='0'
     C     XXDTM         IFGE      9
     C     XXDTM         ANDLE     12
     C                   Z-ADD     XXDTY         XNFYR             3 0
     C                   ELSE
     C     XXDTY         SUB       1             XNFYR
     C                   ENDIF
     C*** END CHG ******** 08/23/10 ************************************
     C*** END ADD ******** 08/16/10 ************************************
      *
      *  LOAD COST CENTER: IF RECORD TYPE NE TO 'D' THEN COMBINE
      *    '5' + EAFYR, ELSE FIND CURRENT FUND YEAR AND COMBINE
      *    WITH '5'.
      *
     C                   MOVE      *ZEROS        XXGLCC            4 0
     C                   MOVEL     '5'           Q1MCU
     C     TT1OBJ        IFGE      '4000'
     C     TT1OBJ        ANDLE     '8999'
     C*** BEGIN CHG ****** 08/16/10 ************************************
     C*********            Z-ADDEAFYR     XXGLCC
     C                   Z-ADD     XNFYR         XXGLCC
     C*** END CHG ******** 08/16/10 ************************************
     C                   MOVEL     '5'           XXGLCC
     C                   MOVEL     XXGLCC        Q1MCU
     C                   ELSE
     C     TT1OBJ        IFEQ      '1230'
     C     TT1OBJ        OREQ      '1201'
      ***** BEG ADD ******* 05/26/00 **********************************
     C     TT1OBJ        OREQ      '2185'
      ***** END ADD ******* 05/26/00 **********************************
      ***** BEG ADD ******* 12/08/04 **********************************
     C     TT1OBJ        OREQ      '1212'
      ***** END ADD ******* 12/08/04 **********************************
     C                   MOVEL     '4'           Q1MCU
     C                   ENDIF                                                  =1230 OR 1201
     C                   ENDIF                                                  >=4000/<=8999
     C*** BEGIN CHG ****** 11/17/09 ************************************
 A   C     Q1@TY         IFEQ      '02'
 A   C**********           MOVEL'5'       Q1PO             DEFAULT
     C                   MOVEL     '5'           Q1MCU2                         DEFAULT
     C     TT1GLC        IFGE      '4000'
     C     TT1GLC        ANDLE     '8999'
     C*** BEGIN CHG ****** 08/16/10 ************************************
     C*********            Z-ADDEAFYR     XXGLCC
     C                   Z-ADD     XNFYR         XXGLCC
     C*** END CHG ******** 08/16/10 ************************************
     C                   MOVEL     '5'           XXGLCC
     C**********           MOVELXXGLCC    Q1PO
     C                   MOVEL     XXGLCC        Q1MCU2
 A   C                   ELSE
 A   C     TT1GLC        IFEQ      '1230'
 A   C     TT1GLC        OREQ      '1201'
 A   C**********           MOVEL'4'       Q1PO
     C                   MOVEL     '4'           Q1MCU2
 A   C                   ENDIF                                                  =1230 OR 1201
     C                   ENDIF                                                  >=4000 & <=8999
 A   C                   ENDIF                                                  Q1@TY = 2
      **********
     C**********           MOVELEADESC    Q1RMK
     C                   MOVEL     EADESC        Q1EXR
     C**********           Z-ADDEAPODT    X@GLDT
     C**********           Z-ADDX1DATE    X@DTE
     C*** END   CHG ****** 11/17/09 ************************************
      *
      *  ADD A/R AMOUNT
      *
     C                   MOVE      EAAMT         Q1AA
      *
     C                   MOVE      Q1AA          Q1AG
      *
      *  LOAD INV# IF NOT ZERO.
      *
     C*** BEGIN DEL ****** 03/03/10 ************************************
     C********   EANINV    IFNE *ZEROS
     C********             Z-ADDEANINV    Q1DOC
     C********             END                             EANINV NE 0
     C*** END   DEL ****** 03/03/10 ************************************
      *
     C*** BEGIN DEL ****** 11/17/09 ************************************
     C**********           Z-ADDEAPODT    X@INDT           INVOICE DATE
     C*** END   DEL ****** 11/17/09 ************************************
      *
      *  WRITE RECORD TO INTERFACE FILE
      *
     C*** BEGIN CHG ****** 11/17/09 ************************************
     C**********           WRITEI57901
     C                   WRITE     I5503B
     C*** END   CHG ****** 11/17/09 ************************************
      *
     C                   ENDSR
      *
     C*** BEGIN ADD ****** 11/17/09 ************************************
      ************************************************************************
      *  SRLDDT- LOAD 8,0 DATE TO 10A DATE WITH SEPARATOR                    *
      ************************************************************************
     C     SRLDDT        BEGSR
     C                   MOVE      XXDTM8        XXDTMM
     C                   MOVE      XXDTD8        XXDTDD
     C                   MOVE      XXDTY8        XXDTYY
     C                   ENDSR
     C*** END   ADD ****** 11/17/09 ************************************
     C*** BEGIN DEL ****** 11/17/09 ************************************
      ************************************************************************
      *  SRGEN#- GENERATE AN INVOICE NUMBER IF NECESSARY                     *
      *          UPDATE A/R FILE WITH THE NEW INVOICE NUMBER
      ************************************************************************
      **********
     C********** SRGEN#    BEGSR
      **********
     C********** EAARCD    IFEQ 'CH'
     C********** #FSTIM    IFEQ 'Y'                         START SEQUENCE
     C**********           MOVE 'N'       #FSTIM  1        FOR INV# GENERA
     C********** EATRDT    MULT 100       #TRMSS 100
     C**********           Z-ADD40        #SS     20       BUMP ABOVE WHAT
     C**********           END                             USERS USED THIS
      **********                                           TRANS DATE
     C**********           ADD  1         #SS
     C********** #TRMSS    ADD  #SS       EANINV
      **********
     C**********           ELSE                            ELSE IS UN,UC
      **********                                           OR AP
     C********** FNKY      CHAINWMFNL                65
     C**********           ADD  1         FNNINV
     C**********           UPDATPFN01
     C********** FNNINV    SUB  1         EANINV
     C**********           END
      **********
     C**********           UPDATPEA01
      **********
     C**********           ENDSR
     C*** END   DEL ****** 11/17/09 ************************************
