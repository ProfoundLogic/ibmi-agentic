     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N93P3R - TYPE P3 BILLING TYPE FORMULA TO CALC BILL AMT
     H*************************************************************
     H*  P3 Billing Calculation:
     H*  1.  With "P" System type, look up Group's Billing code
     H*  2.  Read the EOMB Transaction File (FAEOP)
     H*  3.  Amt Charged (EOAMTC) - Amt Paid (EOAMT) = MRA Savings (XXDISA)
     H*  4.  XXDISA * MRA Savings % (BDPC01)  = MRA Bill Amt (XMDUE)
     H*  5.  If XMDUE is greater than the maximum bill amt (BDAM01)
     H*                then pay BDAM01
     H*  6.  Other Savings (EOOTHR) * Other Savings % (BDPC02) =
     H*                Other Bill Amt (XODUE)
     H*  7.  XMDUE + XODUE = Total Bill Amt (QQBAMT)
     H*  8.  If QQBAMT is less than the minimum bill amt (BDAM02)
     H*                then pay BDAM02 if BDCOM2(pos 1) = *blank
     H*                else pay *zeros if BDCOM2(pos 1) = 'X'.
     H*************************************************************
     H* Revisions:
     H*
     H*  08/07/00  C:62  PROJECT: 65MAL
     H*                  ADD LOGIC FOR BDCOM2(pos 1)(see instruction
     H*                  8 above.)
     H*
     H*  10/26/99  C:62  PROJECT: ARMNT
     H*                  CHG CALC OF MRA
     H*
     H*  09/30/99  C:62  PROJECT: MC212
     H*                  ADD RCN TO EAKEY
     H*
     H*  01/14/99  C:94  PROJECT: 25MKL
     H*                  CHANGE PROGRAM TO USE FAEAP INSTEAD OF
     H*                  FAEOP
     H*
     H* 08/31/93 C:05   Fixed calculation for MRA Savings to subtract out
     H*                 Other Savings first...it was including Other Savings
     H*                 in MRA calculation, then adding Other Savings calc
     H**********************************************************************
     F*** BEG CHG *** 01/14/99 ***************************************
     FFAEOLG    IF   E           K DISK
     F**********FAEOL3  IF  E           K        DISK
     F*** END CHG *** 01/14/99 ***************************************
     F*** BEG ADD *** 01/14/99 ***************************************
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 01/14/99 ***************************************
     FNMBDLB    IF   E           K DISK
     FFMFNL     IF   E           K DISK

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

     C*** KEY LIST FOR EOB FILE FAEOL3
     C*
     C*** BEG CHG *** 01/14/99 ***************************************
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
     C                   KFLD                    XXDATE
     C*
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQGRP#
     C*
     C     *LIKE         DEFINE    FNDT          XXDATE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    ZZCO#             3
     C                   PARM                    ZZSUB#            5
     C                   PARM                    ZZGRP#            3
     C                   PARM                    ZZEOB#            9            CHG 11/05/92
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
     C*
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
     C                   MOVE      ZZCO#         QQCO#             3 0
     C                   MOVE      ZZSUB#        QQSUB#            5 0
     C                   MOVE      ZZGRP#        QQGRP#            3 0
     C                   MOVE      ZZEOB#        QQEOB#            9 0          CHG 11/05/92
     C                   MOVE      ZZBAMT        QQBAMT            9 2
     C*** BEG ADD *** 01/14/99 ***************************************
     C                   MOVE      ZZFYR         QQFYR             3 0
     C                   MOVE      ZZCASE        QQCASE            7 0
     C                   MOVE      ZZRCN         QQRCN             3 0
     C*** END ADD *** 01/14/99 ***************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*** GET CURRENT 'P3' TYPE RECORD FOR GROUP
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             X                 5 0
     C     'P'           LOOKUP    FSY(X)                                 50
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
     C*** BEG CHG *** 01/14/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*** BEGIN DEL *** 08/07/00 **************************************
     C********** EACHRG    SUB  EAPAID    XXDISA  92
     C*** BEGIN ADD *** 10/26/99 **************************************
     C********** XXDISA    SUB  EANDIS    XXDISA
     C*** END   ADD *** 10/26/99 **************************************
     C********** XXDISA    SUB  EAOTHR    XXDISA
     C********** XXDISA    SUB  EAOTH2    XXDISA
     C*** END   DEL *** 08/07/00 **************************************
     C*** BEGIN CHG *** 08/07/00 **************************************
     C********** XXDISA    MULT BDPC01    XMDUE   92
     C     EAMRA         MULT      BDPC01        XMDUE             9 2
     C*** END   CHG *** 08/07/00 **************************************
     C     XMDUE         IFGT      BDAM01
     C                   Z-ADD     BDAM01        XMDUE                          MAXIMUM
     C                   END
     C     EAOTHR        ADD       EAOTH2        XXOTHT            9 2
     C     XXOTHT        MULT      BDPC02        XODUE             9 2
     C*
     C     XODUE         ADD       XMDUE         QQBAMT
     C*** BEGIN ADD *** 08/07/00 **************************************
     C                   MOVEL     BDCOM2        XXPOS1            1
     C*** END   ADD *** 08/07/00 **************************************
     C*
     C     QQBAMT        IFLT      BDAM02
     C*** BEGIN ADD *** 08/07/00 **************************************
     C     XXPOS1        IFEQ      'X'
     C*
     C                   Z-ADD     *ZEROS        QQBAMT
     C*
     C                   ELSE
     C*** END   ADD *** 08/07/00 **************************************
     C                   Z-ADD     BDAM02        QQBAMT                         MINIMUM
     C*** BEGIN ADD *** 08/07/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 08/07/00 **************************************
     C                   END
     C*
     C                   ELSE
     C                   GOTO      TERM
     C*
     C                   END                                                    END EO CHAIN
     C*
     C                   ELSE                                                   ##PRVB = Y
     C*
     C     EOKEY         CHAIN     PEO01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*** BEGIN CHG *** 10/26/99 **************************************
     C********** EOCHRG    SUB  EOPAID    XXDISA  92       CHG 01/14/99
     C     EOAMTC        SUB       EOAMT         XXDISA            9 2          CHG 01/14/99
     C*** END   CHG *** 10/26/99 **************************************
     C     XXDISA        SUB       EOOTHR        XXDISA                         ADD 08/31/93
     C     XXDISA        MULT      BDPC01        XMDUE             9 2
     C     XMDUE         IFGT      BDAM01
     C                   Z-ADD     BDAM01        XMDUE                          MAXIMUM
     C                   END
     C     EOOTHR        MULT      BDPC02        XODUE             9 2
     C*
     C     XODUE         ADD       XMDUE         QQBAMT
     C*** BEGIN ADD *** 08/07/00 **************************************
     C                   MOVEL     BDCOM2        XXPOS1
     C*** END   ADD *** 08/07/00 **************************************
     C*
     C     QQBAMT        IFLT      BDAM02
     C*** BEGIN ADD *** 08/07/00 **************************************
     C     XXPOS1        IFEQ      'X'
     C*
     C                   Z-ADD     *ZEROS        QQBAMT
     C*
     C                   ELSE
     C*** END   ADD *** 08/07/00 **************************************
     C                   Z-ADD     BDAM02        QQBAMT                         MINIMUM
     C*** BEGIN ADD *** 08/07/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 08/07/00 **************************************
     C                   END
     C*
     C                   ELSE
     C                   GOTO      TERM
     C*
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
