     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N93H2R - TYPE H2 BILLING TYPE FORMULA TO CALC BILL AMT
     H**********************************************************************
     H* H2 Billing Calculation:
     H*  1.  With "H" System type, look up Group's Billing code
     H*  2.  Read the EOMB Transaction File (FAEOP)
     H*  3.  Read the Detail Charge Transaction File (FDCAP)
     H*  4.  Add up the number of detail charges (#LIN)
     H*  5.  Z-add 1st line $ amt (BDAM01) = WWAMT
     H*  5.  #LIN * #lines $ amt (BDAM02) = VVAMT
     H*  5.  VVAMT + WWAMT = Total Bill Amt (QQBAMT/ZZBAMT)
     H*  6.  Total Bill Amt < (BDAM03) then Total Bill Amt = (BDAM03)
     H**********************************************************************
     H*  REVISIONS
     H*
     H*  05/05/03  C:62  PROJECT: 129MBJ
     H*                  Exclude apportionment line items.
     H*
     H*  06/01/01  C:62  PROJECT: AR035
     H*                  Add logic for minimum bill amount.
     H*
     H*  09/30/99  C:62  PROJECT: MC212
     H*                  ADD FDCAL13
     H*
     H*  07/27/99  C:62  PROJECT: MC201
     H*                  CORRECT THE KEY THAT IS USED.
     H*
     H*  03/09/99  C:94  PROJECT: MC152
     H*                  CHANGE FDCAL2 TO FDCALL AND CHANGE KLIST
     H*
     H*  01/13/99  C:94  PROJECT: 25MKL
     H*                  CHANGE PROGRAM TO USE FAEAP INSTEAD OF FAEOP
     H****************************************************************
     F*** BEG CHG *** 01/13/99 ***************************************
     FFAEOLG    IF   E           K DISK
     F**********FAEOL3  IF  E           K        DISK
     F*** END CHG *** 01/13/99 ***************************************
     F*** BEG CHG *** 03/09/99 ***************************************
     FFDCALL    IF   E           K DISK
     F**********FDCAL2  IF  E           K        DISK
     F*** END CHG *** 03/09/99 ***************************************
     F*** BEGIN ADD *** 09/30/99 *************************************
     FFDCAL13   IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     F*** END   ADD *** 09/30/99 *************************************
     FNMBDLB    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F*** BEG ADD *** 01/13/99 ***************************************
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 01/13/99 ***************************************

     D*** FILE ARRAYS
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C*** BEG ADD *** 01/13/99 ***************************************
     C     EAKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQGRP#
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    QQRCN
     C*** END ADD *** 01/13/99 ***************************************
     C*
     C*** BEG CHG *** 01/13/99 ***************************************
     C*** KEY LIST FOR EOB FILE FAEOLG
     C     EOKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQGRP#
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C********** EOKEY     KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           QQSUB#
     C**********           KFLD           QQEOB#
     C*** END CHG *** 01/13/99 ***************************************
     C*
     C*** KEY LIST FOR BD FILE NMBDLB
     C*
     C     BDKEY         KLIST
     C                   KFLD                    XXCODE
     C                   KFLD                    XXDATE
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQGRP#
      *
     C*
     C*  CASE DETAIL FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C***********          KFLD           EOTRCD           DEL 03/09/99
     C                   KFLD                    EOEOB#
     C*
     C*** BEG ADD *** 01/13/99 ***************************************
     C*
     C     CAKEY1        KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C**********           KFLD           EAFEIN           DEL 09/30/99
     C**********           KFLD           EAMOD            DEL 09/30/99
     C**********           KFLD           EATRCD           DEL 03/09/99
     C                   KFLD                    EAEOB#
     C                   KFLD                    EARCN                          ADD 09/30/99
     C*
     C*** END ADD *** 01/13/99 ***************************************
     C     *LIKE         DEFINE    FNDT          XXDATE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    ZZCO#             3
     C                   PARM                    ZZSUB#            5
     C                   PARM                    ZZGRP#            3
     C                   PARM                    ZZEOB#            9
     C                   PARM                    ZZBAMT            9
     C*** BEG ADD *** 01/13/99 ***************************************
     C                   PARM                    ZZFYR             3
     C                   PARM                    ZZCASE            7
     C                   PARM                    ZZRCN             3
     C*** END ADD *** 01/13/99 ***************************************
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
     C*
     C*  INITALIZATION SECTION
     C     *LIKE         DEFINE    EOEOB#        QQEOB#
     C*
     C*** BEG ADD *** 01/13/99 ***************************************
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
     C*** END ADD *** 01/13/99 ***************************************
     C                   MOVE      ZZCO#         QQCO#             3 0
     C                   MOVE      ZZSUB#        QQSUB#            5 0
     C                   MOVE      ZZGRP#        QQGRP#            3 0
     C                   MOVE      ZZEOB#        QQEOB#
     C                   MOVE      ZZBAMT        QQBAMT            9 2
     C*** BEG ADD *** 01/13/99 ***************************************
     C                   MOVE      ZZFYR         QQFYR             3 0
     C                   MOVE      ZZCASE        QQCASE            7 0
     C                   MOVE      ZZRCN         QQRCN             3 0
     C*** END ADD *** 01/13/99 ***************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*** GET CURRENT 'H2' TYPE RECORD FOR GROUP
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             X                 5 0
     C     'H'           LOOKUP    FSY(X)                                 50
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
     C*** BEG CHG *** 01/13/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*  GET THE CASE DETAIL CLAIM RECORD.
     C*
     C                   Z-ADD     0             #LIN
     C*** BEGIN CHG *** 09/30/99 *************************************
     C********** CAKEY1    CHAINPCA01                52
     C     CAKEY1        CHAIN     PCA02                              52
     C*** END   CHG *** 09/30/99 *************************************
     C*
     C*  READ ALL THE CASE DETAIL RECORDS IN ORDER TO ACCUMULATE TOTAL
     C*
     C     *IN52         DOWEQ     '0'
     C*
     C*** BEGIN ADD *** 05/05/03 **************************************
     C*
     C* Do not count apportionment lines
     C*
     C     CACODE        IFNE      '000APORT'
     C*** END   ADD *** 05/05/03 **************************************
     C                   ADD       1             #LIN              5 0
     C*** BEGIN ADD *** 05/05/03 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/05/03 **************************************
     C*
     C*  READ THE NEXT CASE DETAIL RECORD FOR THIS CLAIM.
     C*
     C*** BEGIN CHG *** 07/27/99 **************************************
     C********** CAKEY     READEPCA01                    52
     C*** BEGIN CHG *** 09/30/99 *************************************
     C********** CAKEY1    READEPCA01                    52
     C     CAKEY1        READE     PCA02                                  52
     C*** END   CHG *** 09/30/99 *************************************
     C*** END   CHG *** 07/27/99 **************************************
     C*
     C                   END                                                    END DOWEQ 52
     C                   END                                                    END EO CHAIN
     C*
     C                   ELSE                                                   ##PRVB = Y
     C*
     C     EOKEY         CHAIN     PEO01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*  GET THE CASE DETAIL CLAIM RECORD.
     C*
     C                   Z-ADD     0             #LIN
     C     CAKEY         CHAIN     PCA01                              52
     C*
     C*  READ ALL THE CASE DETAIL RECORDS IN ORDER TO ACCUMULATE TOTAL
     C*
     C     *IN52         DOWEQ     '0'
     C*
     C                   ADD       1             #LIN              5 0
     C*
     C*  READ THE NEXT CASE DETAIL RECORD FOR THIS CLAIM.
     C*
     C     CAKEY         READE     PCA01                                  52
     C*
     C                   END                                                    END DOWEQ 52
     C                   END                                                    END EO CHAIN
     C*
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/13/99 ***************************************
     C*
     C                   END                                                    END BD VALID
     C*
     C     #LIN          IFLE      0
     C                   Z-ADD     1             #LIN
     C                   END
     C*
     C                   Z-ADD     BDAM01        WWAMT             9 2
     C                   SUB       1             #LIN
     C*
     C     #LIN          IFGT      *ZERO
     C     #LIN          MULT      BDAM02        VVAMT             9 2
     C                   ELSE
     C                   Z-ADD     *ZERO         VVAMT
     C                   END
     C     WWAMT         ADD       VVAMT         XXAMT             9 2
     C*** BEGIN ADD *** 06/01/01 **************************************
     C     XXAMT         IFLT      BDAM03
     C                   MOVE      BDAM03        QQBAMT
     C                   ELSE
     C*** END   ADD *** 06/01/01 **************************************
     C                   MOVE      XXAMT         QQBAMT
     C*** BEGIN ADD *** 06/01/01 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/01/01 **************************************
     C*
     C*
     C     TERM          TAG
     C                   MOVE      QQBAMT        ZZBAMT
     C                   MOVE      '1'           *INLR
     C*
     C*
