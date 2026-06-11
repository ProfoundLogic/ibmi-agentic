     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N93U1R - TYPE U1 BILLING TYPE FORMULA TO CALC BILL AMT
     H*************************************************************
     H*  U1 Billing Calculation:
     H*  1.  With "U" System type, look up Group's Billing code
     H*  2.  Z-add Bill $ amt (BDAM01) = Total Bill Amt (QQBAMT)
     H*************************************************************
     H*  REVISIONS
     H*
     H* 05/27/08 C:191 PROJECT: 25MOS WO2                             *
     H*                AUTOMATE PRE-AUTH PROCESS                      *
     H*                                                               *
     H* 02/28/08 C:191 PROJECT: 25MOS                                 *
     H*                AUTOMATE PRE-AUTH PROCESS                      *
     H*                                                               *
     H*  09/30/99  C:62  PROJECT: MC212
     H*                  ADD RCN TO EAKEY
     H*
     H*  01/14/99  C:94  PROJECT: 25MKL
     H*                  CHANGE PROGRAM TO USE FAEAP INSTEAD OF
     H*                  FAEOP
     H*************************************************************
     FNMBDLB    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F*** BEG CHG *** 01/14/99 ***************************************
     FFAEOLG    IF   E           K DISK
     F**********FAEOL3  IF  E           K        DISK
     F*** END CHG *** 01/14/99 ***************************************
     FUMPCL1    IF   E           K DISK
     F*** BEG ADD *** 01/14/99 ***************************************
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 01/14/99 ***************************************

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
     C*
     C*  EOMB FILE
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
     C*  PRE-CERT FILE
     C*
     C     PCKEY         KLIST
     C                   KFLD                    EOCERT
     C                   KFLD                    EOEXT
     C*
     C     *LIKE         DEFINE    FNDT          XXDATE
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
     C*
     C* INITIALIZATION SECTION
     C************LIKE     DEFN EOEOB#    QQEOB#           DEL 11/05/92
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
     C*** GET CURRENT 'U1' TYPE RECORD FOR GROUP
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             X                 5 0
     C     'U'           LOOKUP    FSY(X)                                 50
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
     C*** BEG CHG *** XX/XX/XX ***************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '0'
     C     PCKEY         CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C     PCSTAT        IFEQ      'O'
     C                   MOVE      BDAM01        QQBAMT
     C                   END
     C     PCSTAT        IFEQ      'R'
     C                   MOVE      BDAM02        QQBAMT
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   ENDIF                                                  EAKEY
     C*
     C                   ELSE                                                   ##PRVB = Y
     C*
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '0'
     C     PCKEY         CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C     PCSTAT        IFEQ      'O'
     C                   MOVE      BDAM01        QQBAMT
     C                   END
     C     PCSTAT        IFEQ      'R'
     C                   MOVE      BDAM02        QQBAMT
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/14/99 ***************************************
     C                   END                                                    END BD VALID
     C*
     C     TERM          TAG
     C                   MOVE      QQBAMT        ZZBAMT
     C                   MOVE      '1'           *INLR
     C*
