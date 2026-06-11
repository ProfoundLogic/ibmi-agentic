     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4301R - CLAIMANT STAFF CODE ASSIGN/CHANGE CALCULATION
     H*                 AND PRINT PROGRAM
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 04/19/10 C:235 Project: 25WRG                                 *
     H*                CLAIMS GENERAL RETROFIT                        *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 08/30/06 C:132 Project: 132WGI                                *
     H*              - Added selections for employer alpha name       *
     H*                range. Add edits for new range.                *
     H*              - Add new employer alpha selections to QQDATA    *
     H*                                                               *
     H*  08/22/03 C:15  Project: WC1138                               *
     H*                 Added code to process claim status selection. *
     H*                                                               *
     H*  06/12/03 C:15 Support: WC065/37505                           *
     H*                Added code to transfer diaries when the Assign *
     H*                staff codes option is used.                    *
     H*                                                               *
     H*  10/22/01 C:15 Support: WC114/29410                           *
     H*                Fixed issue with claim name subroutine.        *
     H*                                                               *
     H*  07/21/00 C:15 Support: WC065/23621                           *
     H*                Chgd the lost time flag processing.            *
     H*                                                               *
     H*  05/23/00 C:15 Support: WC044/22957                           *
     H*                Added code to call FA030R which will update    *
     H*                the staff codes in the WCMC claim file.        *
     H*                                                               *
     H*  12/13/99 C:15 Support: WC044/20951                           *
     H*                Changed pgm to more efficient.                 *
     H*                                                               *
     H*  08/16/99 C:15 Support: WC091/19297                           *
     H*                Changed pgm to not load the claim SS# into a   *
     H*                numeric field since the SS# is an alphanumeric *
     H*                field.                                         *
     H*                                                               *
     H*  10/14/98 C:98 PROJECT: 48WNW                                 *
     H*               CHANGE ALPHA LOOKUP TO BE UPPER CASE ONLY       *
     H*                                                               *
     H*    06/07/98  C:58  PROJ:BASE FIX                              *
     H*         Correct legend on Routing page.                       *
     H*                                                               *
     H*    11/24/97  C:79  PROJ:BASE FIX                              *
     H*         -ADD LOST TIME FIELD SELECTION                        *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  08  SUBMITTED IN BATCH MODE                                  *
     H*  09  SUBMITTED IN DEMAND MODE                                 *
     H*  20  COUNTY CODES SELECTED                                    *
     H*  30  CLAIMANT NAME RANGE SELECTED                             *
     H*  40  EMPLOYER NUMBER RANGE SELECTED                           *
     H*  50  RECORD SELECTED FOR STAFF CODE MAINTENANCE               *
     H*  60  GENERAL CHAIN                                            *
     H*  61  GENERAL CHAIN                                            *
     H*  70  STAFF CODES WERE SELECTED IN CHANGE MODE (QQMODE="C")    *
     H*  71  XXTADJ FIELD WAS SELECTED (ADJUSTER                    ) *
     H*  72  XXTSPV FIELD WAS SELECTED (SUPERVISOR                  ) *
     H*  73  XXTAMN FIELD WAS SELECTED (ACCOUNT MANAGER             ) *
     H*  74  XXTUMN FIELD WAS SELECTED (CLAIM MANAGER               ) *
     H*  75  XXTNSE FIELD WAS SELECTED (CASE NURSE                  ) *
     H*  76  XXTFAD FIELD WAS SELECTED (OUTSIDE ADJUSTER            ) *
     H*  77  XXTSDF FIELD WAS SELECTED (SDF ADJUSTER                ) *
     H*  78  XXTESP FIELD WAS SELECTED (EXCESS ADJUSTER             ) *
     H*  80  FUNDS WERE SELECTED                                      *
     H*  97  ONE DETAIL LINE HAS BEEN PRINTED                         *
     H*  98  FIRST TIME THROUGH SRPRNT SO PRINT A HEADER              *
     H*****************************************************************
     F*** BEGIN CHG *** 12/13/99 **************************************
     F******WMCML2  UF  E           K        DISK
     FWMCMLL    UF   E           K DISK
     F*** END CHG ***** 12/13/99 **************************************
     F                                     RENAME(PCM01:PCM02)
     FWMCML     UF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     F*** BEGIN DLT *** 12/13/99 **************************************
     F******WMCDL1  IF  E           K        DISK
     F******      PCD01                             KRENAMEPCD02
     F*** END DLT ***** 12/13/99 **************************************
     FWMCXL     IF   E           K DISK
      *
      **** BEG ADD ***** 08/30/06 *************************************
     FWMEML     IF   E           K DISK
      **** END ADD ***** 08/30/06 *************************************
      *
     FWADTL     O  A E           K DISK
     FQSYSPRT   O    F  132        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOA)
      *
      **** BEG ADD ****** 04/19/10 ************************************
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSTCTL1    IF   E           K DISK
      *
      *  DARTS ARRAY
      *

     D DRT             S             21    DIM(4) CTDATA PERRCD(1)
      **** END ADD ****** 04/19/10 ************************************
      *
     D SPVSOR          C                   CONST('SUPERVISOR          ')
     D ACTMGR          C                   CONST('ACCOUNT MANAGER     ')
     D CLMMGR          C                   CONST('CLAIM MANAGER       ')
     D CASNSE          C                   CONST('CASE NURSE          ')
     D OUTADJ          C                   CONST('OUTSIDE ADJUSTER    ')
     D SDFADJ          C                   CONST('SDF ADJUSTER        ')
     D EXCADJ          C                   CONST('EXCESS ADJUSTER     ')
     D*** BEGIN ADD *** 10/14/98 ***********************************************
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*** END   ADD *** 10/14/98 ***********************************************
     D*** BEGIN ADD *** 04/19/10 ***********************************************
     D DATEDS          DS                  INZ
     D  CYMD                   1      8  0
     D  CC1                    1      2  0
     D  CY1                    1      4
     D  YY1                    3      4  0
     D  YMD                    3      8  0
     D  MMDD                   5      8  0
     D  MM                     5      6
     D  DD                     7      8
     D  CC2                    9     10  0
     D  CY2                    9     12
     D  YY2                   11     12  0
     D  MDCY                   5     12  0
     D*** END   ADD *** 04/19/10 ***********************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PRTDS           DS
     D  XXLINE               367    368B 0
     D                 DS                  INZ
     D  CNTY                   1     20
     D                                     DIM(5)
     D  XXCN1                  1      4
     D  XXCN2                  5      8
     D  XXCN3                  9     12
     D  XXCN4                 13     16
     D  XXCN5                 17     20
     D                 DS
     D  XXCNTY                 1      4
     D  XXST                   1      2
     D  XXCC                   3      4
     D                 DS
     D  DSFD                   1     30  0
     D                                     DIM(10)
     D  XXFD01                 1      3  0
     D  XXFD02                 4      6  0
     D  XXFD03                 7      9  0
     D  XXFD04                10     12  0
     D  XXFD05                13     15  0
     D  XXFD06                16     18  0
     D  XXFD07                19     21  0
     D  XXFD08                22     24  0
     D  XXFD09                25     27  0
     D  XXFD10                28     30  0
     D*** BEGIN ADD *** 08/16/99 **************************************
     D*** BEGIN ADD *** 04/19/10 **************************************
     D*
      *  DARTS
     D                 DS                  INZ
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS                  INZ
     D  QCCOM1                 1    195
     D  QCFEAD                 1      3
     D  QCUSNM                 5     35
     D  QCDRT1                36     56
     D  QCCNAM                58     87
     D  QCCTY                 89    113
     D  QCDRT2               116    129
     D  QCMO                 131    132
     D  QCSLS1               133    133
     D  QCDY                 134    135
     D  QCSLS2               136    136
     D  QCYR                 137    138
     D                 DS                  INZ
     D  QCCOM2                 1     61
     D  QCDRT3                 1     10
     D  QCUSER                12     61
     D*** END   ADD *** 04/19/10 **************************************
     D                 DS
     D  ZZSS#                  1      9
     D  XXSS1                  1      3
     D  XXSS2                  4      5
     D  XXSS3                  6      9
     D*** END ADD ***** 08/16/99 **************************************
      *
      **** BEG ADD ***** 08/30/06 *************************************
     D                 DS
     D  EMNAME                 1     40
     D  XXNAME                 1      1
      **** END ADD ***** 08/30/06 *************************************
      *
     D*** Begin add *** 08/22/03 **************************************
     D                 DS                  INZ
      **** BEG CHG ***** 08/30/06 *************************************
     D**********************                  1  98 DATA
     D  DATA                   1    100
      **** END CHG ***** 08/30/06 *************************************
     D  QQCN1                  1      4
     D  QQCN2                  5      8
     D  QQCN3                  9     12
     D  QQCN4                 13     16
     D  QQCN5                 17     20
     D  QQFADJ                21     23
     D  QQTADJ                24     26
     D  QQFSPV                27     29
     D  QQTSPV                30     32
     D  QQFAMN                33     35
     D  QQTAMN                36     38
     D  QQFUMN                39     41
     D  QQTUMN                42     44
     D  QQFNSE                45     47
     D  QQTNSE                48     50
     D  QQFFAD                51     53
     D  QQTFAD                54     56
     D  QQFSDF                57     59
     D  QQTSDF                60     62
     D  QQFESP                63     65
     D  QQTESP                66     68
     D  QQFD01                69     71
     D  QQFD02                72     74
     D  QQFD03                75     77
     D  QQFD04                78     80
     D  QQFD05                81     83
     D  QQFD06                84     86
     D  QQFD07                87     89
     D  QQFD08                90     92
     D  QQFD09                93     95
     D  QQFD10                96     98
      **** BEG ADD ***** 08/30/06 *************************************
     D  QQFENM                99     99
     D  QQTENM               100    100
      **** END ADD ***** 08/30/06 *************************************
      *
     D*** End add ***** 08/22/03 **************************************
      *
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  CLAIM MASTER FILE
      *

     C     CMKY          KLIST
     C                   KFLD                    XXCO#                          C:04 12/16
     C                   KFLD                    XXFND
      *
      *  CLAIM MASTER FILE
      *
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#                          C:04 12/16
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
      *
      *  CLAIM MASTER FILE
      *
     C     CMKY2         KLIST
     C                   KFLD                    XXCO#                          C:04 12/16
     C                   KFLD                    XXFND
     C*** BEGIN CHG *** 12/13/99 **************************************
     C******************** KFLD           XXSS#
     C                   KFLD                    XXLSN            30
     C*** END CHG ***** 12/13/99 **************************************
     C*** BEGIN ADD**** 04/19/10 **************************************
     C*  USER PARAMETER FILE
     C*
     C     UPKY1         KLIST
     C                   KFLD                    XXCLEX
     C*** END ADD ***** 04/19/10 **************************************
      *
      *  DIARY MASTER FILE
      *
     C     DDKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
      *
      *  CLAIMANT DEMOGRAPHICS FILE
      *
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
      *
     C*** BEGIN DLT *** 12/13/99 **************************************
      *  CLAIMANT DEMOGRAPHICS FILE
      **********
     C********** CDKY1     KLIST
     C**********           KFLD           XXLSN  30
     C*** END DLT ***** 12/13/99 **************************************
      *
      *  CLAIMS DARTS INTAKE
      *
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *   KEY LIST FOR THE OVERRIDE FILE.
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
      *
      **** BEG ADD ***** 08/30/06 *************************************
      *
      *  Employer Master file key
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      **** END ADD ***** 08/30/06 *************************************
      *
      *  LIKE DEFINES
      *
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMEMP#        XXFEMP
     C     *LIKE         DEFINE    CMEMP#        XXTEMP
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    FNNAME        XXFNAM
     C     *LIKE         DEFINE    CMCLEX        XXFCDE
     C     *LIKE         DEFINE    CMCLEX        XXTCDE
     C     *LIKE         DEFINE    CXADJ         XXADJ
     C     *LIKE         DEFINE    DTDT          XXCYMD
     C*** BEGIN ADD *** 10/14/98 ***********************************************
     C     *LIKE         DEFINE    YYLET         XXLET
     C*** END   ADD *** 10/14/98 ***********************************************
      *
      **** BEG ADD ***** 08/30/06 *************************************
     C     *LIKE         DEFINE    CMDIV         XXDIV
      **** END ADD ***** 08/30/06 *************************************
      **** BEG ADD ***** 04/19/10 *************************************
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMFYR         XXFYR
      **** END ADD ***** 04/19/10 *************************************
      *
      *  INITIALIZE PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFLET            1
     C                   PARM                    QQTLET            1
     C                   PARM                    QQFEMP            9
     C                   PARM                    QQTEMP            9
     C*** Begin add *** 08/22/03 **************************************
     C**********           PARM           QQCN1   4
     C**********           PARM           QQCN2   4
     C**********           PARM           QQCN3   4
     C**********           PARM           QQCN4   4
     C**********           PARM           QQCN5   4
     C**********           PARM           QQFADJ  3
     C**********           PARM           QQTADJ  3
     C**********           PARM           QQFSPV  3
     C**********           PARM           QQTSPV  3
     C**********           PARM           QQFAMN  3
     C**********           PARM           QQTAMN  3
     C**********           PARM           QQFUMN  3
     C**********           PARM           QQTUMN  3
     C**********           PARM           QQFNSE  3
     C**********           PARM           QQTNSE  3
     C**********           PARM           QQFFAD  3
     C**********           PARM           QQTFAD  3
     C**********           PARM           QQFSDF  3
     C**********           PARM           QQTSDF  3
     C**********           PARM           QQFESP  3
     C**********           PARM           QQTESP  3
     C**********           PARM           QQFD01  3
     C**********           PARM           QQFD02  3
     C**********           PARM           QQFD03  3
     C**********           PARM           QQFD04  3
     C**********           PARM           QQFD05  3
     C**********           PARM           QQFD06  3
     C**********           PARM           QQFD07  3
     C**********           PARM           QQFD08  3
     C**********           PARM           QQFD09  3
     C**********           PARM           QQFD10  3
     C*** End dlt ***** 08/22/03 **************************************
     C*** Begin add *** 08/22/03 **************************************
      **** BEG CHG ***** 08/30/06 *************************************
     C*************        PARM           QQDATA 98
     C                   PARM                    QQDATA          100
      **** END CHG ***** 08/30/06 *************************************
     C*** End add ***** 08/22/03 **************************************
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQFLAG            1
     *** BEG ADD *** 11/24/97 *****************************************
     C                   PARM                    QQLTFL            1
     *** END ADD *** 11/24/97 *****************************************
     C*** Begin add *** 08/22/03 **************************************
     C                   PARM                    QQOCST            1
     C*** End add ***** 08/22/03 **************************************
     **
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     **
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDY'        SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C*** Begin add *** 08/22/03 **************************************
     C                   MOVE      QQDATA        DATA
     C*** End add ***** 08/22/03 **************************************
     C*** BEG ADD ***** 04/19/10 **************************************
     C     DATCHK        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*          DA700R - API to write a DARTS Remark PLIST
     C*          ------------------------------------------------------
     C     DARTPL        PLIST
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C*** END ADD ***** 04/19/10 **************************************
      *
      *  LOAD COUNTY CODES TO WORK FIELDS
      *
     C                   MOVE      QQCN1         XXCN1             4
     C                   MOVE      QQCN2         XXCN2             4
     C                   MOVE      QQCN3         XXCN3             4
     C                   MOVE      QQCN4         XXCN4             4
     C                   MOVE      QQCN5         XXCN5             4
      *
      *  LOAD STAFF CODES TO WORK FIELDS
      *
     C                   MOVE      QQFADJ        XXFADJ            3
     C                   MOVE      QQTADJ        XXTADJ            3
     C                   MOVE      QQFSPV        XXFSPV            3
     C                   MOVE      QQTSPV        XXTSPV            3
     C                   MOVE      QQFAMN        XXFAMN            3
     C                   MOVE      QQTAMN        XXTAMN            3
     C                   MOVE      QQFUMN        XXFUMN            3
     C                   MOVE      QQTUMN        XXTUMN            3
     C                   MOVE      QQFNSE        XXFNSE            3
     C                   MOVE      QQTNSE        XXTNSE            3
     C                   MOVE      QQFFAD        XXFFAD            3
     C                   MOVE      QQTFAD        XXTFAD            3
     C                   MOVE      QQFSDF        XXFSDF            3
     C                   MOVE      QQTSDF        XXTSDF            3
     C                   MOVE      QQFESP        XXFESP            3
     C                   MOVE      QQTESP        XXTESP            3
     C                   MOVE      QQFLET        XXFLET            1
     C                   MOVE      QQTLET        XXTLET            1
      **** BEG ADD ***** 08/30/06 *************************************
     C                   MOVE      QQFENM        XXFENM            1
     C                   MOVE      QQTENM        XXTENM            1
      **** END ADD ***** 08/30/06 *************************************
      *
      *  LOAD FUNDS TO WORK FIELDS
      *
     C                   MOVE      QQFD01        XXFD01
     C                   MOVE      QQFD02        XXFD02
     C                   MOVE      QQFD03        XXFD03
     C                   MOVE      QQFD04        XXFD04
     C                   MOVE      QQFD05        XXFD05
     C                   MOVE      QQFD06        XXFD06
     C                   MOVE      QQFD07        XXFD07
     C                   MOVE      QQFD08        XXFD08
     C                   MOVE      QQFD09        XXFD09
     C                   MOVE      QQFD10        XXFD10
      *
      *  LOAD MISCELLANEOUS PARMS TO WORK FIELDS
      *
     C                   MOVE      QQFLAG        XXFLAG            1
     C                   MOVE      QQFLET        XXFLET            1
     C                   MOVE      QQTLET        XXTLET            1
     C                   MOVE      QQFEMP        XXFEMP
     C                   MOVE      QQTEMP        XXTEMP
     C                   MOVE      QQCO#         XXCO#
     *** BEG ADD *** 11/24/97 *****************************************
     C                   MOVE      QQLTFL        XXLTFL            1
     *** END ADD *** 11/24/97 *****************************************
     C*** Begin add *** 08/22/03 **************************************
     C                   MOVE      QQOCST        XXOCST            1
     C*** End add ***** 08/22/03 **************************************
      *
     C                   TIME                    DSDTTM
      *
      *  CONVERT THE DATE TO MONTH/DAY/CENTURY-YEAR
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         XXDATE            8 0
      *
      *  CONVERT THE DATE TO CENTURY-YEAR/MONTH/DAY FOR DIARY UPDATE
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCYMD
      *****************************************************************
      *   SET UP INDICATORS FOR ROUTING PAGE FIELDS AND HEADINGS
      *
      *  DETERMINE WHETHER BATCH OR DEMAND MODE
      *
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
      *
      *  ASSIGN OR CHANGE MODE
      *
     C     XXFLAG        COMP      'C'                                    70
      *
      *  CLAIMANT RANGE ENTERED
      *
     C     XXTLET        IFNE      *BLANKS
     C                   MOVE      *ON           *IN30
     C                   ENDIF
      *
      *  EMPLOYER NUMBER RANGE ENTERED
      *
     C     XXTEMP        IFNE      *ZERO
     C                   MOVE      *ON           *IN40
     C                   ENDIF
      *
      *  IF NO EMPLOYER RANGE ENTERED THEN SET "FROM" AND "TO" VALUES
      *
     C     *IN40         IFEQ      *OFF
     C                   MOVE      *HIVAL        XXTEMP
     C                   MOVE      *LOVAL        XXFEMP
     C                   ENDIF
      *
      **** BEG ADD ***** 08/30/06 *************************************
      *
      *  Employer Name range Entered
      *
     C     XXTENM        IFNE      *ZERO
     C     XXTENM        ANDNE     *BLANK
     C                   MOVE      *ON           *IN41
     C                   ENDIF
      *
      *  If NO Employer Name Range entered then set "FROM" and "TO" Values
      *
     C     *IN41         IFEQ      *OFF
     C                   MOVE      *HIVAL        XXTENM
     C                   MOVE      *LOVAL        XXFENM
     C                   ENDIF
      **** END ADD ***** 08/30/06 *************************************
      *
      *  COUNTY CODES ENTERED
      *
     C                   MOVEA     CNTY          CNTY1            20
      *
     C     CNTY1         IFNE      *BLANKS
      *
     C                   MOVE      *ON           *IN20
     C     XXCN1         COMP      *BLANKS                            21
     C     XXCN2         COMP      *BLANKS                            22
     C     XXCN3         COMP      *BLANKS                            23
     C     XXCN4         COMP      *BLANKS                            24
     C     XXCN5         COMP      *BLANKS                            25
      *
     C                   ENDIF
      *
      *  PARTICULAR FUNDS OR ALL SELECTED
      *
     C                   DO        10            Y                 2 0
      *
     C     DSFD(Y)       IFNE      *ZERO
      *
     C                   MOVE      *ON           *IN80
     C                   LEAVE
      *
     C                   ENDIF
      *
     C                   ENDDO
      *
     C     *IN80         IFEQ      *ON
      *
     C     XXFD01        COMP      *ZERO                              51
     C     XXFD02        COMP      *ZERO                              52
     C     XXFD03        COMP      *ZERO                              53
     C     XXFD04        COMP      *ZERO                              54
     C     XXFD05        COMP      *ZERO                              55
     C     XXFD06        COMP      *ZERO                              56
     C     XXFD07        COMP      *ZERO                              81
     C     XXFD08        COMP      *ZERO                              82
     C     XXFD09        COMP      *ZERO                              83
     C     XXFD10        COMP      *ZERO                              84
      *
     C                   ENDIF
      *
      *  STAFF CODES SELECTED CHANGE MODE
      *
     C     *IN70         IFEQ      *ON
      *
     C     XXTADJ        COMP      *BLANKS                            71
     C     XXTSPV        COMP      *BLANKS                            72
     C     XXTAMN        COMP      *BLANKS                            73
     C     XXTUMN        COMP      *BLANKS                            74
     C     XXTNSE        COMP      *BLANKS                            75
     C     XXTFAD        COMP      *BLANKS                            76
     C     XXTSDF        COMP      *BLANKS                            77
     C     XXTESP        COMP      *BLANKS                            78
      *
      *  STAFF CODES SELECTED ASSIGN MODE
      *
     C                   ELSE
      *
     C     XXFADJ        COMP      *BLANKS                            71
     C     XXFSPV        COMP      *BLANKS                            72
     C     XXFAMN        COMP      *BLANKS                            73
     C     XXFUMN        COMP      *BLANKS                            74
     C     XXFNSE        COMP      *BLANKS                            75
     C     XXFFAD        COMP      *BLANKS                            76
     C     XXFSDF        COMP      *BLANKS                            77
     C     XXFESP        COMP      *BLANKS                            78
      *
     C                   ENDIF                                                  *IN70=ON
      *
      *****************************************************************
      *  GET THE FLEXIBLE ADJUSTER HEADING FROM DARTS INTAKE
      *
     C     CXKY          CHAIN     PCX01                              60
      *
     C     CXADJ         IFEQ      *BLANK
     C                   MOVEL     'ADJUSTER'    XXADJ
      *
     C                   ELSE
     C                   MOVEL     CXADJ         XXADJ
      *
     C                   ENDIF                                                  CXADJ NE BLANK
      *
      *****************************************************************
      *     GET THE DISTRIBUTION INFORMATION
      *
      *  IF DEMAND MODE ROUTE TO USER NAME IN USER PARAMETER FILE
      *
     C     *IN09         IFEQ      *ON
     C     Q1USER        CHAIN     PUP01                              60
     C                   ENDIF
      *
     C                   MOVE      '0'           XXPRT
      *
     C     ORKY          CHAIN     POR01                              60
      *
      *****************************************************************
      *  CENTER THE REPORT HEADINGS
      *
      *  CENTER THE REPORT TITLE
      *
     C     *IN60         IFEQ      *OFF
     C     ORSTAT        ANDEQ     'A'
      *
     C                   MOVEL(P)  ORTITL        ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
      *
     C                   MOVEL     ##DATA        XXTITL           60
      *
     C                   ENDIF                                                  *IN60=OFF
      *
      *  CENTER THE COMPANY NAME
      *
     C     COKY          CHAIN     PCO01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL(P)  CONAME        ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
      *
     C                   MOVEL     ##DATA        XXCONM           40
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
     C                   ENDIF                                                  *IN60=OFF
      *
      *  PRINT ROUTING PAGE AND SET INDICATOR FOR 1ST TIME THROUGH
      *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   MOVE      *OFF          *IN98
      *
      *****************************************************************
      *     BEGIN PROCESSING SELECTED FUNDS FOR CLAIMS MASTER
      *****************************************************************
     C     *IN80         IFEQ      *ON
      *
      *  PROCESS SELECTED FUNDS
      *
     C     1             DO        10            X                 2 0
      *
     C     DSFD(X)       IFNE      *ZERO
      *
     C                   MOVE      DSFD(X)       XXFND
      *
      *****************************************************************
      *  PROCESS STAFF CODES
      *
      *  CLAIMANT NAME RANGE SELECTED
      *
     C     *IN30         IFEQ      *ON
      *
     C                   EXSR      SRCLNM
      *
     C                   ELSE                                                   *IN30 = ON
      *
      **** BEG ADD ***** 08/30/06 *************************************
      * Employer Name Range Selected
     C     *IN41         IFEQ      *ON
     C                   EXSR      SREMNM
     C                   ELSE                                                   *IN30 = ON
      **** END ADD ***** 08/30/06 *************************************
      *
      *  EMPLOYER NUMBER RANGE SELECTED
      *
     C                   EXSR      SREMP#
      **** BEG ADD ***** 08/30/06 *************************************
     C                   ENDIF                                                  *IN41 = ON
      **** END ADD ***** 08/30/06 *************************************
      *
     C                   ENDIF                                                  *IN30 = ON
      *
     C                   ENDIF                                                  DSFD,X  IFNE 0
      *
     C                   ENDDO                                                  1 DO 10 X    0
      *
     C                   ELSE
      *
      *  ALL FUNDS WERE SELECTED
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      FNFND         XXFND
      *
      *  UPDATE CLAIMS MASTER
      *****************************************************************
      *  PROCESS STAFF CODES
      *
      *  CLAIMANT NAME RANGE SELECTED
      *
     C     *IN30         IFEQ      *ON
      *
     C                   EXSR      SRCLNM
      *
     C                   ELSE                                                   *IN30 = ON
      *
      **** BEG ADD ***** 08/30/06 *************************************
      * Employer Name Range Selected
     C     *IN41         IFEQ      *ON
     C                   EXSR      SREMNM
     C                   ELSE                                                   *IN30 = ON
      **** END ADD ***** 08/30/06 *************************************
      *
      *  EMPLOYER NUMBER RANGE SELECTED
      *
     C                   EXSR      SREMP#
      **** BEG ADD ***** 08/30/06 *************************************
     C                   ENDIF                                                  *IN41 = ON
      **** END ADD ***** 08/30/06 *************************************
      *
     C                   ENDIF                                                  *IN30 = ON
      *
     C     FNKY          READE     PFN01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ 0
      *
     C                   ENDIF                                                  DSFD  NE 0  0
      *
      *  IF NO RECORDS WERE SELECTED PRINT THAT INFORMATION
      *
     C     *IN98         IFEQ      *OFF
     C                   EXCEPT    HEAD                                                     0
     C                   EXCEPT    NOREC                                                    0
     C                   ENDIF                                                              0
      *
      *****************************************************************
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
          Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
      *
     C*** BEGIN ADD *** 05/23/00 **************************************
     C*===============================================================*
     C*  MCUPD - This subroutine determines if WCMC is installed and
     C*    linked, then calls FA030R to update the staff codes in the
     C*    WCMC claim file.
     C*===============================================================*
     C     MCUPD         BEGSR
     C*
     C*     WCMC Installed determination
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1
     C                   PARM                    ##FBIF            1
     C                   PARM                    ##FL01            1
     C*
     C     ##FBIF        IFEQ      'Y'
     C     ##FL01        ANDEQ     'Y'
     C*
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C*
     C                   CALL      'FA030R'
     C                   PARM      *BLANK        ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      CMNRSE        ##CMNU            3            Nurse
     C                   PARM      *BLANKS       ##CMSU            3            Unavailable
     C                   PARM      CMCLEX        ##CLAD            3            Claim examiner
     C                   PARM      CMSUPR        ##CLSU            3            Supervisor
     C                   PARM      CMFEAD        ##CLFA            3            FEAD
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD ***** 05/23/00 **************************************
      *
      **** BEG ADD ***** 08/30/06 *************************************
      *===============================================================*
      *   SUBROUTINE SREMNM - Select by Employer Name Range
      *===============================================================*
     C     SREMNM        BEGSR
      *
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C     XXNAME        ANDGE     XXFENM
     C     XXNAME        ANDLE     XXTENM
      *
     C                   MOVE      *OFF          *IN50
      *
     C     *IN20         IFEQ      *ON
      *
      *  COUNTY CODES WERE SELECTED
      *
     C                   EXSR      SRCNTY
      *
      *  COUNTY CODES WERE NOT SELECTED
      *
     C                   ELSE
      *
     C     CDKY          CHAIN     PCD01                              61
     C                   EXSR      SRCNTY
     C                   MOVE      *ON           *IN50
      *
     C                   ENDIF                                                  *IN20 EQ ON LET
      *
      *  SELECT BASED ON LOST TIME FLAG
      *
     C                   EXSR      SRLTFL
     C                   EXSR      SROCST
      *
      *  ASSIGN SELECTED STAFF CODES
      *
     C     *IN50         IFEQ      *ON
      *
     C                   MOVE      *BLANK        XXCNTY
     C                   MOVEL     CDCNTY        XXCC
     C                   MOVEL     CMJRST        XXST
      *
     C                   EXSR      SRASSN
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C     CMKY          READE     PCM01                                  60
      *
     C                   ENDDO
      *
     C                   ENDSR                                                  SREMNM
      *
      **** END ADD ***** 08/30/06 *************************************
      *===============================================================*
      *   SUBROUTINE SREMP# - SELECT BY EMPOYER NUMBER RANGE
      *===============================================================*
     C     SREMP#        BEGSR
      *
     C                   MOVE      XXFEMP        XXEMP#
      *
     C     *LOVAL        SETLL     PCM01
     C     CMKY1         SETLL     PCM01
      *
     C     CMKY          READE     PCM01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CMEMP#        ANDLE     XXTEMP
      *
      *
     C                   MOVE      *OFF          *IN50
      *
     C     *IN20         IFEQ      *ON
      *
      *  COUNTY CODES WERE SELECTED
      *
     C                   EXSR      SRCNTY
      *
      *  COUNTY CODES WERE NOT SELECTED
      *
     C                   ELSE
      *
     C     CDKY          CHAIN     PCD01                              61
     C                   EXSR      SRCNTY
     C                   MOVE      *ON           *IN50
      *
     C                   ENDIF                                                  *IN20 EQ ON LET
      *** BEG ADD *** 11/24/97 ****************************************
      *
      *  SELECT BASED ON LOST TIME FLAG
      *
     C                   EXSR      SRLTFL
      *** END ADD *** 11/24/97 ****************************************
     C*** Begin add *** 08/22/03 **************************************
     C                   EXSR      SROCST
     C*** End add ***** 08/22/03 **************************************
      *
      *  ASSIGN SELECTED STAFF CODES
      *
     C     *IN50         IFEQ      *ON
      *
     C                   MOVE      *BLANK        XXCNTY
     C                   MOVEL     CDCNTY        XXCC
     C                   MOVEL     CMJRST        XXST
      *
     C                   EXSR      SRASSN
      *
     C                   ENDIF
      *
     C     CMKY          READE     PCM01                                  60
      *
     C                   ENDDO
      *
     C                   ENDSR
      *===============================================================*
      *   SUBROUTINE SRCLNM - UPDATE BY CLAIMANT NAME
      *===============================================================*
     C     SRCLNM        BEGSR
      *
     C                   MOVEL     XXFLET        XXLSN
      *
      *  GET LAST NAME
      *
     C*** BEGIN DLT *** 12/13/99 **************************************
     C********** CDKY1     SETLLPCD02                      BY LAST NM
     C**********           READ PCD02                    60
     C**********
     C*** BEGIN CHG *** 10/14/98 ***********************************************
     C******************** MOVELCDLSN     XXLET   1
     C**********           MOVELCDLSN     YYLET   1
     C********** LO:UP     XLATEYYLET     XXLET
     C*** END   CHG *** 10/14/98 ***********************************************
     C*** END DLT ***** 12/13/99 **************************************
     C*** BEGIN ADD *** 12/13/99 **************************************
     C     CMKY2         SETLL     PCM02                                        BY LAST NM
     C     CMKY          READE     PCM02                                  60
     C*
     C                   MOVEL     CMLSN         YYLET             1
     C     LO:UP         XLATE     YYLET         XXLET
     C*** END ADD ***** 12/13/99 **************************************
      *
     C     *IN60         DOWEQ     *OFF
     C     XXLET         ANDLE     XXTLET
      *
     C                   MOVE      *OFF          *IN50
      *
     C*** BEGIN DLT *** 12/13/99 **************************************
     C**********           MOVE CDSS#     XXSS#
      **********
      *  READ THE CLAIMS MASTER FILE FOR ALL MATCHES ON SS#
      **********
     C********** CMKY2     CHAINPCM02                61
      **********
     C********** *IN61     DOWEQ*OFF
     C*** END DLT ***** 12/13/99 **************************************
     C*** BEGIN ADD *** 12/13/99 **************************************
     C*  Retrieve the claimant demographics record
     C*
     C     CDKY          CHAIN     PCD01                              61
     C     *IN61         IFEQ      *OFF
     C*** END ADD ***** 12/13/99 **************************************
      *
      *  COUNTY CODES WERE SELECTED
      *
     C     *IN20         IFEQ      *ON
      *
     C                   EXSR      SRCNTY
      *
      *  COUNTY CODES WERE NOT SELECTED
      *
     C                   ELSE
     C                   MOVE      *ON           *IN50
      *
     C                   ENDIF                                                  *IN20 EQ ON
      *** BEG ADD *** 11/24/97 ****************************************
      *
      *  SELECT BASED ON LOST TIME FLAG
      *
     C                   EXSR      SRLTFL
      *** END ADD *** 11/24/97 ****************************************
     C*** Begin add *** 08/22/03 **************************************
     C                   EXSR      SROCST
     C*** End add ***** 08/22/03 **************************************
      *
      *  ASSIGN SELECTED STAFF CODES
      *
     C     *IN50         IFEQ      *ON
      *
     C                   MOVE      *BLANK        XXCNTY
     C                   MOVEL     CDCNTY        XXCC
     C                   MOVEL     CMJRST        XXST
      *
     C                   EXSR      SRASSN
      *
     C                   ENDIF                                                  *IN50=ON
      *
     C*** BEGIN ADD *** 12/13/99 **************************************
     C                   ENDIF                                                  *IN61=OFF
      *
     C     CMKY          READE     PCM02                                  60
      *
     C*** Begin chg *** 10/22/01 **************************************
     C*******************  MOVELCDLSN     YYLET   1
     C                   MOVEL     CMLSN         YYLET             1
     C*** End chg ***** 10/22/01 **************************************
     C     LO:UP         XLATE     YYLET         XXLET
      *
     C                   ENDDO                                                  DOWEQ IN60 OFF
     C*** END ADD ***** 12/13/99 **************************************
      *
     C*** BEGIN DLT *** 12/13/99 **************************************
     C********** CMKY2     READEPCM02                    61
     C**********           ENDDO                           DOWEQ IN61 OFF
      **********
     C**********           READ PCD02                    60
     C*** BEGIN CHG *** 10/14/98 ***********************************************
     C******************** MOVELCDLSN     XXLET   1
     C**********           MOVELCDLSN     YYLET   1
     C********** LO:UP     XLATEYYLET     XXLET
     C*** END   CHG *** 10/14/98 ***********************************************
     C**********           ENDDO                           *IN60=OFF
     C*** END DLT ***** 12/13/99 **************************************
      *
     C                   ENDSR
      *===============================================================*
      *   SUBROUTINE SRCNTY - PROCESS COUNTY CODES
      *===============================================================*
     C     SRCNTY        BEGSR
      *
      *  CALLED TO FIND A COUNTY CODE MATCH
      *
     C                   Z-ADD     1             Y
      *
     C     Y             DOWLE     5
      *
     C     CNTY(Y)       IFNE      *BLANKS
      *
     C                   MOVE      CNTY(Y)       XXCNTY
      *
     C     *IN30         IFEQ      *ON
      *
      *  CALLED FROM CLAIMANT SUBROUTINE
      *
     C     CDCNTY        IFEQ      XXCC
     C     CMJRST        ANDEQ     XXST
      *
     C                   MOVE      *ON           *IN50
     C                   LEAVE
      *
     C                   ENDIF                                                  CDCNTY = XXCC
      *
      *  CALLED FROM EMPLOYER SUBROUTINE
      *
     C                   ELSE                                                   CDCNTY=XXCC
      *
     C     CMJRST        IFEQ      XXST
      *
     C                   MOVE      CMSS#         XXSS#
      *
     C     CDKY          CHAIN     PCD01                              61
      *
     C     *IN61         IFEQ      *OFF
     C     CDCNTY        ANDEQ     XXCC
      *
     C                   MOVE      *ON           *IN50
     C                   LEAVE
      *
     C                   ENDIF                                                  *IN61 = OFF
      *
     C                   ENDIF                                                  CMJRST = XXST
      *
     C                   ENDIF                                                  *IN30 = 0
      *
     C                   ENDIF                                                  CNTY,Y NE BLANKS
      *
     C                   ADD       1             Y
      *
     C                   ENDDO                                                  DOW Y LE 5
      *
     C                   ENDSR
      *===============================================================*
      *   SUBROUTINE SRASSN - STAFF CODE ASSIGNMENT FROM THIS TO THAT
      *===============================================================*
     C     SRASSN        BEGSR
      *
     C                   MOVE      *OFF          *IN97
     C                   MOVE      'N'           UPD               1
      *****************************************************************
      *  ADJUSTER
      *
      *  CHANGE STAFF CODE
      *
     C     *IN71         IFEQ      *ON
     C     XXFADJ        ANDEQ     CMCLEX
     C     XXFLAG        ANDEQ     'C'
      *
     C                   MOVE      'Y'           UPD
     C                   MOVE      XXTADJ        CMCLEX
     C                   MOVEL(P)  XXADJ         XXSTNM           20
     C                   MOVEL     XXFADJ        XXFCDE
     C                   MOVEL     XXTADJ        XXTCDE
     C                   MOVE      'C'           DTCTGY
      *
      *  PRINT HEADER AND DETAIL LINES
      *
     C                   EXSR      SRDIAR
     C                   EXSR      SRPRNT
      *
     C*** BEG   ADD **** 04/19/10 **********************************************
      ****  When adjuster changes write DARTS diary items
     C                   EXSR      SRRITE
     C*** END   ADD **** 04/19/10 **********************************************
     C                   ENDIF
      *
      *  ASSIGN STAFF CODE
      *
     C     XXFLAG        IFEQ      'A'
     C     *IN71         ANDEQ     *ON
      *
     C                   MOVE      'Y'           UPD
     C*** Begin add *** 06/12/03 **************************************
     C                   MOVEL     CMCLEX        XXFCDE
     C                   MOVEL     XXFADJ        XXTCDE
     C                   MOVE      'C'           DTCTGY
     C*** End add ***** 06/12/03 **************************************
     C                   MOVE      XXFADJ        CMCLEX
     C                   MOVEL     XXADJ         XXSTNM           20
     C*** Begin dlt *** 06/12/03 **************************************
     C**********           MOVELXXFADJ    XXFCDE
     C*** End dlt ***** 06/12/03 **************************************
      *
      *  PRINT HEADER AND DETAIL LINES
      *
     C*** Begin add *** 06/12/03 **************************************
     C                   EXSR      SRDIAR
     C*** End add ***** 06/12/03 **************************************
     C                   EXSR      SRPRNT
     C*** BEG   ADD **** 04/19/10 **********************************************
      ****  When adjuster changes write DARTS diary items
     C                   EXSR      SRRITE
     C*** END   ADD **** 04/19/10 **********************************************
      *
     C                   ENDIF
      *****************************************************************
      *  SUPERVISOR
      *
      *  CHANGE STAFF CODE
      *
     C     *IN72         IFEQ      *ON
     C     XXFSPV        ANDEQ     CMSUPR
     C     XXFLAG        ANDEQ     'C'
      *
     C                   MOVE      'Y'           UPD
     C                   MOVE      XXTSPV        CMSUPR
     C                   MOVEL     SPVSOR        XXSTNM           20
     C                   MOVEL     XXFSPV        XXFCDE
     C                   MOVEL     XXTSPV        XXTCDE
     C                   MOVE      'S'           DTCTGY
      *
      *  PRINT HEADER AND DETAIL LINES
      *
     C                   EXSR      SRDIAR
     C                   EXSR      SRPRNT
      *
     C                   ENDIF
      *
      *  ASSIGN STAFF CODE
      *
     C     *IN72         IFEQ      *ON
     C     XXFLAG        ANDEQ     'A'
      *
     C                   MOVE      'Y'           UPD
     C*** Begin add *** 06/12/03 **************************************
     C                   MOVEL     CMSUPR        XXFCDE
     C                   MOVEL     XXFSPV        XXTCDE
     C                   MOVE      'S'           DTCTGY
     C*** End add ***** 06/12/03 **************************************
     C                   MOVE      XXFSPV        CMSUPR
     C                   MOVEL     SPVSOR        XXSTNM           20
     C*** Begin dlt *** 06/12/03 **************************************
     C**********           MOVELXXFSPV    XXFCDE
     C*** End dlt ***** 06/12/03 **************************************
      *
      *  PRINT HEADER AND DETAIL LINES
      *
     C*** Begin add *** 06/12/03 **************************************
     C                   EXSR      SRDIAR
     C*** End add ***** 06/12/03 **************************************
     C                   EXSR      SRPRNT
      *
     C                   ENDIF
      *****************************************************************
      *  ACCOUNT MANAGER
      *
      *  CHANGE STAFF CODE
      *
     C     *IN73         IFEQ      *ON
     C     XXFAMN        ANDEQ     CMAMAN
     C     XXFLAG        ANDEQ     'C'
      *
     C                   MOVE      'Y'           UPD
     C                   MOVE      XXTAMN        CMAMAN
     C                   MOVEL     ACTMGR        XXSTNM           20
     C                   MOVEL     XXFAMN        XXFCDE
     C                   MOVEL     XXTAMN        XXTCDE
     C                   MOVE      'A'           DTCTGY
      *
      *  PRINT HEADER AND DETAIL LINES
      *
     C                   EXSR      SRDIAR
     C                   EXSR      SRPRNT
      *
     C                   ENDIF
      *
      *  ASSIGN STAFF CODE
      *
     C     *IN73         IFEQ      *ON
     C     XXFLAG        ANDEQ     'A'
      *
     C                   MOVE      'Y'           UPD
     C*** Begin add *** 06/12/03 **************************************
     C                   MOVEL     CMAMAN        XXFCDE
     C                   MOVEL     XXFAMN        XXTCDE
     C                   MOVE      'A'           DTCTGY
     C*** End add ***** 06/12/03 **************************************
     C                   MOVE      XXFAMN        CMAMAN
     C                   MOVEL     ACTMGR        XXSTNM           20
     C*** Begin dlt *** 06/12/03 **************************************
     C**********           MOVELXXFAMN    XXFCDE
     C*** End dlt ***** 06/12/03 **************************************
      *
      *  PRINT HEADER AND DETAIL LINES
      *
     C*** Begin add *** 06/12/03 **************************************
     C                   EXSR      SRDIAR
     C*** End add ***** 06/12/03 **************************************
     C                   EXSR      SRPRNT
      *
     C                   ENDIF
      *****************************************************************
      *  CLAIM MANAGER
      *
      *  CHANGE STAFF CODE
      *
     C     *IN74         IFEQ      *ON
     C     XXFUMN        ANDEQ     CMUMAN
     C     XXFLAG        ANDEQ     'C'
      *
     C                   MOVE      'Y'           UPD
     C                   MOVE      XXTUMN        CMUMAN
     C                   MOVEL     CLMMGR        XXSTNM           20
     C                   MOVEL     XXFUMN        XXFCDE
     C                   MOVEL     XXTUMN        XXTCDE
     C                   MOVE      'U'           DTCTGY
      *
      *  PRINT HEADER AND DETAIL LINES
      *
     C                   EXSR      SRDIAR
     C                   EXSR      SRPRNT
      *
     C                   ENDIF
      *
      *  ASSIGN STAFF CODE
      *
     C     *IN74         IFEQ      *ON
     C     XXFLAG        ANDEQ     'A'
      *
     C                   MOVE      'Y'           UPD
     C*** Begin add *** 06/12/03 **************************************
     C                   MOVEL     CMUMAN        XXFCDE
     C                   MOVEL     XXFUMN        XXTCDE
     C                   MOVE      'U'           DTCTGY
     C*** End add ***** 06/12/03 **************************************
     C                   MOVE      XXFUMN        CMUMAN
     C                   MOVEL     CLMMGR        XXSTNM           20
     C*** Begin dlt *** 06/12/03 **************************************
     C**********           MOVELXXFUMN    XXFCDE
     C*** End dlt ***** 06/12/03 **************************************
      *
      *  PRINT HEADER AND DETAIL LINES
      *
     C*** Begin add *** 06/12/03 **************************************
     C                   EXSR      SRDIAR
     C*** End add ***** 06/12/03 **************************************
     C                   EXSR      SRPRNT
      *
     C                   ENDIF
      *****************************************************************
      *  CASE NURSE
      *
      *  CHANGE STAFF CODE
      *
     C     *IN75         IFEQ      *ON
     C     XXFNSE        ANDEQ     CMNRSE
     C     XXFLAG        ANDEQ     'C'
      *
     C                   MOVE      'Y'           UPD
     C                   MOVE      XXTNSE        CMNRSE
     C                   MOVEL     CASNSE        XXSTNM           20
     C                   MOVEL     XXFNSE        XXFCDE
     C                   MOVEL     XXTNSE        XXTCDE
     C                   MOVE      'N'           DTCTGY
      *
      *  PRINT HEADER AND DETAIL LINES
      *
     C                   EXSR      SRDIAR
     C                   EXSR      SRPRNT
      *
     C                   ENDIF
      *
      *  ASSIGN STAFF CODE
      *
     C     *IN75         IFEQ      *ON
     C     XXFLAG        ANDEQ     'A'
      *
     C                   MOVE      'Y'           UPD
     C*** Begin add *** 06/12/03 **************************************
     C                   MOVEL     CMNRSE        XXFCDE
     C                   MOVEL     XXFNSE        XXTCDE
     C                   MOVE      'N'           DTCTGY
     C*** End add ***** 06/12/03 **************************************
     C                   MOVE      XXFNSE        CMNRSE
     C                   MOVEL     CASNSE        XXSTNM           20
     C*** Begin dlt *** 06/12/03 **************************************
     C**********           MOVELXXFNSE    XXFCDE
     C*** End dlt ***** 06/12/03 **************************************
      *
      *  PRINT HEADER AND DETAIL LINES
      *
     C*** Begin add *** 06/12/03 **************************************
     C                   EXSR      SRDIAR
     C*** End add ***** 06/12/03 **************************************
     C                   EXSR      SRPRNT
      *
     C                   ENDIF
      *****************************************************************
      *  OUTSIDE ADJUSTER
      *
      *  CHANGE STAFF CODE
      *
     C     *IN76         IFEQ      *ON
     C     XXFFAD        ANDEQ     CMFEAD
     C     XXFLAG        ANDEQ     'C'
      *
     C                   MOVE      'Y'           UPD
     C                   MOVE      XXTFAD        CMFEAD
     C                   MOVEL     OUTADJ        XXSTNM           20
     C                   MOVEL     XXFFAD        XXFCDE
     C                   MOVEL     XXTFAD        XXTCDE
     C                   MOVE      'F'           DTCTGY
      *
      *  PRINT HEADER AND DETAIL LINES
      *
     C                   EXSR      SRDIAR
     C                   EXSR      SRPRNT
      *
     C                   ENDIF
      *
      *  ASSIGN STAFF CODE
      *
     C     *IN76         IFEQ      *ON
     C     XXFLAG        ANDEQ     'A'
      *
     C                   MOVE      'Y'           UPD
     C*** Begin add *** 06/12/03 **************************************
     C                   MOVEL     CMFEAD        XXFCDE
     C                   MOVEL     XXFFAD        XXTCDE
     C                   MOVE      'F'           DTCTGY
     C*** End add ***** 06/12/03 **************************************
     C                   MOVE      XXFFAD        CMFEAD
     C                   MOVEL     OUTADJ        XXSTNM           20
     C*** Begin dlt *** 06/12/03 **************************************
     C**********           MOVELXXFFAD    XXFCDE
     C*** End dlt ***** 06/12/03 **************************************
      *
      *  PRINT HEADER AND DETAIL LINES
      *
     C*** Begin add *** 06/12/03 **************************************
     C                   EXSR      SRDIAR
     C*** End add ***** 06/12/03 **************************************
     C                   EXSR      SRPRNT
      *
     C                   ENDIF
      *****************************************************************
      *  SDF ADJUSTER
      *
      *  CHANGE STAFF CODE
      *
     C     *IN77         IFEQ      *ON
     C     XXFSDF        ANDEQ     CMSDFA
     C     XXFLAG        ANDEQ     'C'
      *
     C                   MOVE      'Y'           UPD
     C                   MOVE      XXTSDF        CMSDFA
     C                   MOVEL     SDFADJ        XXSTNM           20
     C                   MOVEL     XXFSDF        XXFCDE
     C                   MOVEL     XXTSDF        XXTCDE
     C                   MOVE      'D'           DTCTGY
      *
      *  PRINT HEADER AND DETAIL LINES
      *
     C                   EXSR      SRDIAR
     C                   EXSR      SRPRNT
      *
     C                   ENDIF
      *****************************************************************
      *  EXCESS ADJUSTER
      *
      *  CHANGE STAFF CODE
      *
     C     *IN78         IFEQ      *ON
     C     XXFESP        ANDEQ     CMESUP
     C     XXFLAG        ANDEQ     'C'
      *
     C                   MOVE      'Y'           UPD
     C                   MOVE      XXTESP        CMESUP
     C                   MOVEL     EXCADJ        XXSTNM           20
     C                   MOVEL     XXFESP        XXFCDE
     C                   MOVEL     XXTESP        XXTCDE
     C                   MOVE      'E'           DTCTGY
      *
      *  PRINT HEADER AND DETAIL LINES
      *
     C                   EXSR      SRDIAR
     C                   EXSR      SRPRNT
      *
     C                   ENDIF
      *****************************************************************
      *  UPDATE CLAIMS MASTER
      *
     C     UPD           IFEQ      'Y'
      *
      *  UPDATE THE USER, PGM, DATE, AND TIME FIELDS OF CLAIMS MASTER
      *
     C                   MOVE      Q1PGM         CMPGM
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      XXCYMD        CMDT
     C                   MOVE      DSTIME        CMTIME
      *
      *  SKIP A LINE BETWEEN CASE #S
      *
     C                   EXCEPT    DET2
      *
      *  BY CLAIMANT NAME RANGE WITH OR WITHOUT COUNTY CODES
      *
     C     *IN30         IFEQ      *ON
     C     *IN40         ANDEQ     *OFF
     C     *IN20         ANDEQ     *OFF
      *
     C     *IN30         OREQ      *ON
     C     *IN20         ANDEQ     *ON
     C     *IN40         ANDEQ     *OFF
      *
     C                   UPDATE    PCM02
     C*** BEGIN ADD *** 05/23/00 **************************************
     C                   EXSR      MCUPD
     C*** END ADD ***** 05/23/00 **************************************
      *
     C                   ELSE
      *
     C                   UPDATE    PCM01
     C*** BEGIN ADD *** 05/23/00 **************************************
     C                   EXSR      MCUPD
     C*** END ADD ***** 05/23/00 **************************************
      *
     C                   ENDIF                                                  *IN30 = ON
      *
     C                   ENDIF                                                  UPD = Y
      *
     C                   ENDSR
      *
      *===============================================================*
      *  SUBROUTINE TO PRINT HEADER AND DETAIL LINES                  *
      *===============================================================*
      *
     C     SRPRNT        BEGSR
      *
      *  CHECK FOR END OF PAGE OR FIRST TIME THROUGH
      *
     C*** BEGIN CHG *** 08/16/99 **************************************
     C******************** MOVE CMSS#     ZZSS#   90
     C                   MOVE      CMSS#         ZZSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMOD            10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'ZZSS#'       QQNAME           25
     C     ZZSS#         PARM      ZZSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C*** END CHG ***** 08/16/99 **************************************
      *
     C     XXLINE        IFGT      60
     C     *IN98         OREQ      *OFF
      *
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   MOVE      *ON           *IN98
     C                   MOVE      *ON           *IN97
      *
     C                   ELSE
      *
     C     *IN97         IFEQ      *OFF
     C                   EXCEPT    DET
     C                   MOVE      *ON           *IN97
     C                   ENDIF
      *
     C                   ENDIF                                                  XXLINE IFGT 60
      *
      *  PRINT THE "FROM" AND/OR "TO" STAFF CODE CHANGES
      *
     C                   EXCEPT    DET1
      *
     C                   ENDSR                                                              60
      *===============================================================*
      *  SUBROUTINE TO ADD A RECORD TO THE DIARY TRANSFER FILE        *
      *===============================================================*
      *
     C     SRDIAR        BEGSR
      *
      *  CHANGE MODE ONLY
      *
     C                   MOVE      CMCO#         DTCO#
     C                   MOVE      CMFND         DTFND
     C                   MOVE      CMFYR         DTFYR
     C                   MOVE      CMCASE        DTCASE
     C                   MOVE      XXFCDE        DTOSTF
     C                   MOVE      XXTCDE        DTNSTF
     C                   MOVE      Q1USER        DTUSER
     C                   MOVE      Q1PGM         DTPGM
     C                   MOVE      XXCYMD        DTDT
     C                   MOVE      DSTIME        DTTIME
      *
     C                   WRITE     PDT01
      *
     C                   ENDSR
      *************************************************************************
      *
     *** BEG ADD *** 11/24/97 *****************************************
      *===============================================================*
      *   SUBROUTINE SRLTFL - SELECTS BASED ON LOST TIME FLAG
      *===============================================================*
     C     SRLTFL        BEGSR
      *
      *  IF LOST TIME NE BOTH THEN SELECT BASED ON CMLTFL
      *
     C     XXLTFL        IFNE      'B'
      *
     C                   SELECT
      *
     C     CMLTFL        WHENEQ    'I'
     C*** BEGIN CHG *** 07/21/00 **************************************
     C********** XXLTFL    ANDEQ'N'
     C     XXLTFL        ANDEQ     'M'
     C*** END CHG ***** 07/21/00 **************************************
      *
     C                   MOVE      *OFF          *IN50
      *
     C     CMLTFL        WHENNE    'I'
     C*** BEGIN CHG *** 07/21/00 **************************************
     C********** XXLTFL    ANDEQ'L'
     C     XXLTFL        ANDEQ     'I'
     C*** END CHG ***** 07/21/00 **************************************
      *
     C                   MOVE      *OFF          *IN50
      *
     C                   ENDSL                                                  XXLTFL NE B
      *
     C                   ENDIF                                                  XXLTFL NE B
      *
     C                   ENDSR
      *===============================================================*
     *** END ADD *** 11/24/97 *****************************************
     *** BEG ADD *** 04/19/10 *****************************************
      *===============================================================*
     C*  SRRITE - SUBROUTINE TO WRITE DARTS
      *===============================================================*
     C     SRRITE        BEGSR
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     XXFND         QCFND
     C                   Z-ADD     CMFYR         QCFYR
     C                   Z-ADD     CMCASE        QCCASE
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL     *BLANKS       Q#RMCD
     C                   MOVEL     *BLANKS       Q#COMM
     C                   MOVEL     *BLANKS       Q#DCID
     C                   MOVEL     *BLANKS       Q#DIFL
     C                   MOVE      *BLANK        Q#FL01
     C                   MOVE      *BLANK        Q#FL02
     C                   MOVEL     *BLANKS       Q#FOLD
     C                   MOVE      *BLANKS       QCCOM1
     C                   MOVEL     DRT(1)        QCDRT1
     C                   MOVE      *BLANKS       QCCNAM           30
     C                   MOVEL     CDLSN         QQLNAM           56
     C                   MOVEL     CDFSN         QQFNAM           15
     C                   MOVEL     ' '           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        QCCNAM
     C                   MOVEL     CDCTY         QCCTY
     C                   MOVEL     DRT(2)        QCDRT2
     C                   MOVE      '/'           QCSLS1
     C                   MOVE      '/'           QCSLS2
     C                   CLEAR                   DATEDS
     C                   Z-ADD     CMACDT        CYMD
     C                   MOVE      CY1           QCYR
     C                   MOVE      MM            QCMO
     C                   MOVE      DD            QCDY
     C                   MOVEL     DRT(3)        QCDRT3
     C     CXKY          CHAIN     PCX01                              60
     C     XXFADJ        IFNE      *BLANKS
     C     CXRFFA        ANDNE     *BLANKS
     C                   MOVE      XXFADJ        XXCLEX            3
     C     UPKY1         CHAIN     PUP02                              60
     C                   MOVE      XXCLEX        QCFEAD
     C  N60              MOVEL     UPUSNM        QCUSNM
     C                   MOVEL(P)  CXRFFA        Q#RMCD            5
     C                   MOVEL(P)  QCCOM1        Q#COMM          256
     C                   MOVE      QCCOM2        Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C     CXRFFA        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CTDEPG        Q#DCID           12
     C                   MOVEL(P)  CTFL01        Q#DIFL            1
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVE(P)   'Y'           Q#FL01            1
     C                   ENDIF                                                  DCID => blanks
     C     CTDPRG        IFGT      0
     C                   MOVE(P)   'Y'           Q#FL02            1
     C                   ENDIF                                                  DPRG > 0
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  Q#KEY         Q#FOLD           80
     C                   ENDIF                                                  DCID <> blanks
     C                   ENDIF                                                  *IN60 = Off
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANKS       Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANKS       Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   CALL      'DA700R'      DARTPL
     C                   ENDIF                                                  FADJ <> blanks
     C*
     C                   MOVEL     *BLANKS       Q#RMCD
     C                   MOVEL     *BLANKS       Q#DCID
     C                   MOVEL     *BLANKS       Q#DIFL
     C                   MOVE      *BLANK        Q#FL01
     C                   MOVE      *BLANK        Q#FL02
     C                   MOVEL     *BLANKS       Q#FOLD
     C     XXTADJ        IFNE      *BLANKS
     C     CXRTFA        ANDNE     *BLANKS
     C                   MOVE      *BLANKS       QCFEAD
     C                   MOVE      *BLANKS       QCUSNM
     C                   MOVE      XXTADJ        XXCLEX
     C     UPKY1         CHAIN     PUP02                              60
     C                   MOVE      XXCLEX        QCFEAD
     C  N60              MOVEL     UPUSNM        QCUSNM
     C                   MOVE      CXRTFA        Q#RMCD
     C                   MOVEL(P)  QCCOM1        Q#COMM
     C                   MOVE      QCCOM2        Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C     CXRTFA        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CTDEPG        Q#DCID
     C                   MOVEL(P)  CTFL01        Q#DIFL
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVE      'Y'           Q#FL01
     C                   ENDIF                                                  DCID <> blanks
     C     CTDPRG        IFGT      0
     C                   MOVE      'Y'           Q#FL02
     C                   ENDIF                                                  DPRG > 0
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL     Q#KEY         Q#FOLD
     C                   ENDIF                                                  DCID <> blanks
     C                   ENDIF                                                  *IN60 = Off
     C                   CALL      'DA700R'      DARTPL
     C                   ENDIF                                                  TADJ <> blanks
     C*** END ADD *** 04/26/00 ****************************************
     C*
     C                   ENDSR
     *** END ADD *** 04/19/10 *****************************************
     C*** Begin add *** 08/22/03 **************************************
      *===============================================================*
      *   SUBROUTINE SROCST - Selects based on claim status
      *===============================================================*
     C     SROCST        BEGSR
      *
      *  If claim status is not 'B', select based on CMSTAT.
      *
     C     XXOCST        IFNE      'B'
      *
     C                   SELECT
      *
     C     XXOCST        WHENEQ    'O'
     C     CMSTAT        ANDNE     'O'
      *
     C                   MOVE      *OFF          *IN50
      *
     C     XXOCST        WHENEQ    'C'
     C     CMSTAT        ANDNE     'C'
      *
     C                   MOVE      *OFF          *IN50
      *
     C                   ENDSL
      *
     C                   ENDIF
      *
     C                   ENDSR
      *===============================================================*
     C*** End add ***** 08/22/03 **************************************
      *
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  9 'XXXXXXXX'
     O**********                                 21 'XXXXXX'
     O**********                                 35 'XX      XX'
     O**********                                 46 'XXXXXXXXXX'
     O**********                                 57 'XXXXXXXXXX'
     O**********                                 68 'XXX     XX'
     O**********                                 78 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XXXXXXXXX'
     O**********                                 22 'XXXXXXXX'
     O**********                                 35 'XX      XX'
     O**********                                 46 'XXXXXXXXXX'
     O**********                                 57 'XXXXXXXXXX'
     O**********                                 68 'XXXX    XX'
     O**********                                 79 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 11 'XX      XX'
     O**********                                 23 'XX      XX'
     O**********                                 35 'XX      XX'
     O**********                                 42 'XX'
     O**********                                 53 'XX'
     O**********                                 68 'XXXX    XX'
     O**********                                 80 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 11 'XX      XX'
     O**********                                 23 'XX      XX'
     O**********                                 35 'XX      XX'
     O**********                                 42 'XX'
     O**********                                 53 'XX'
     O**********                                 68 'XX XX   XX'
     O**********                                 72 'XX'
     O**********E            ROUTE          1
     O**********                                 10 'XXXXXXXXX'
     O**********                                 23 'XX      XX'
     O**********                                 35 'XX      XX'
     O**********                                 42 'XX'
     O**********                                 53 'XX'
     O**********                                 68 'XX XX   XX'
     O**********                                 72 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXX'
     O**********                                 23 'XX      XX'
     O**********                                 35 'XX      XX'
     O**********                                 42 'XX'
     O**********                                 53 'XX'
     O**********                                 68 'XX  XX  XX'
     O**********                                 72 'XX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXX'
     O**********                                 23 'XX      XX'
     O**********                                 35 'XX      XX'
     O**********                                 42 'XX'
     O**********                                 53 'XX'
     O**********                                 68 'XX  XX  XX'
     O**********                                 80 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XXXXX'
     O**********                                 23 'XX      XX'
     O**********                                 35 'XX      XX'
     O**********                                 42 'XX'
     O**********                                 53 'XX'
     O**********                                 68 'XX   XX XX'
     O**********                                 80 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  7 'XX XXX'
     O**********                                 23 'XX      XX'
     O**********                                 35 'XX      XX'
     O**********                                 42 'XX'
     O**********                                 53 'XX'
     O**********                                 68 'XX   XX XX'
     O**********                                 80 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX  XXX'
     O**********                                 23 'XX      XX'
     O**********                                 35 'XX      XX'
     O**********                                 42 'XX'
     O**********                                 53 'XX'
     O**********                                 68 'XX    XXXX'
     O**********                                 80 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  9 'XX   XXX'
     O**********                                 22 'XXXXXXXX'
     O**********                                 34 'XXXXXXXX'
     O**********                                 42 'XX'
     O**********                                 57 'XXXXXXXXXX'
     O**********                                 68 'XX    XXXX'
     O**********                                 79 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                 10 'XX    XXX'
     O**********                                 21 'XXXXXX'
     O**********                                 33 'XXXXXX'
     O**********                                 42 'XX'
     O**********                                 57 'XXXXXXXXXX'
     O**********                                 68 'XX     XXX'
     O**********                                 78 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** End Del ***** 08/26/14 ************************************
     O**********E            ROUTE          1
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      **
      **   CHANGE CRITERIA
      **
     O          E    30      ROUTE          0
     O                                           21 'CLAIMANT NAME RANGE:'
     O                       XXFLET              23
     O                                           25 '-'
     O                       XXTLET              27
      **** BEG ADD ***** 08/30/06 *************************************
     O          E    41      ROUTE          0
     O                                           21 'EMP NAME RANGE . . :'
     O                       XXFENM              23
     O                                           25 '-'
     O                       XXTENM              27
      **** END ADD ***** 08/30/06 *************************************
     O          E    40      ROUTE       2  0
     O                                           21 'EMP NUMBER RANGE . :'
     O                       XXFEMP        Z     31
     O                                           33 '-'
     O                       XXTEMP        Z     44
     O          E    20      ROUTE       2  0
     O                                           21 'COUNTY CODES       :'
     O                       XXCN1               26
     O                       XXCN2             +001
     O                       XXCN3             +001
     O                       XXCN4             +001
     O                       XXCN5             +001
     O*** Begin add *** 08/22/03 **************************************
     O          E            ROUTE       2  0
     O                                           21 'CLAIM STATUS       :'
     O                       XXOCST              23
     O                                           41 '(O=OPEN, C=CLOSED'
     O                                           50 ', B=BOTH)'
     O*** End add ***** 08/22/03 **************************************
     O          E            ROUTE       2  0
     O                                           21 'LOST TIME          :'
     O                       XXLTFL              23
     O*** BEGIN CHANGE *** 06/07/98 ***********************************
     O**********                         41 '(L=LOST TIME,N=NO'
     O**********                         59 'LOST TIME,B=BOTH)'
     O                                           41 '(I=LOST TIME,M=NO'
     O                                           59 'LOST TIME,B=BOTH)'
     O*** END CHANGE ***** 06/07/98 ***********************************
     O          E            ROUTE       2  0
     O                                           21 'GROUPS SELECTED    :'
     O          E   N80      ROUTE          1
     O                                           25 '*ALL'
     O          E    51      ROUTE          1
     O                       XXFD01              25
     O          E    52      ROUTE          1
     O                       XXFD02              25
     O          E    53      ROUTE          1
     O                       XXFD03              25
     O          E    54      ROUTE          1
     O                       XXFD04              25
     O          E    55      ROUTE          1
     O                       XXFD05              25
     O          E    56      ROUTE          1
     O                       XXFD06              25
     O          E    81      ROUTE          1
     O                       XXFD07              25
     O          E    82      ROUTE          1
     O                       XXFD08              25
     O          E    83      ROUTE          1
     O                       XXFD09              25
     O          E    84      ROUTE          1
     O                       XXFD10              25
     O          E            ROUTE       1  0
     O                                           12 'STAFF CODES'
     O          E            ROUTE          0
     O                                           12 '___________'
     O          E            ROUTE          0
     O               70                          31 'FROM    TO'
     O              N70                          28 'ASSIGN'
     O          E            ROUTE          2
     O               70                          31 '____   ___'
     O              N70                          28 '______'
     O          E    71      ROUTE          2
     O                                           21 'ADJUSTER           :'
     O                       XXFADJ              25
     O               70 71                       27 '-'
     O               70 71   XXTADJ              31
     O          E    72      ROUTE          2
     O                                           21 'SUPERVISOR         :'
     O                       XXFSPV              25
     O               70 72                       27 '-'
     O               70 72   XXTSPV              31
     O          E    73      ROUTE          2
     O                                           21 'ACCOUNT MANAGER    :'
     O                       XXFAMN              25
     O               70 73                       27 '-'
     O               70 73   XXTAMN              31
     O          E    74      ROUTE          2
     O                                           21 'CLAIM MANAGER      :'
     O                       XXFUMN              25
     O               70 74                       27 '-'
     O               70 74   XXTUMN              31
     O          E    75      ROUTE          2
     O                                           21 'CASE NURSE         :'
     O                       XXFNSE              25
     O               70 75                       27 '-'
     O               70 75   XXTNSE              31
     O          E    76      ROUTE          2
     O                                           21 'OUTSIDE ADJUSTER   :'
     O                       XXFFAD              25
     O               70 76                       27 '-'
     O               70 76   XXTFAD              31
     O          E    77      ROUTE          2
     O                                           21 'SDF ADJUSTOR       :'
     O                       XXFSDF              25
     O                                           27 '-'
     O                       XXTSDF              31
     O          E    78      ROUTE          2
     O                                           21 'EXCESS ADJUSTER    :'
     O                       XXFESP              25
     O                                           27 '-'
     O                       XXTESP              31
     **
      **** BEG ADD ***** 08/30/06 *************************************
     O                                           10 'RUN DATE:'
     O                       DSDTTM            +001 '  :  :  &  /  /  '
      **** END ADD ***** 08/30/06 *************************************
      ****** BEG DEL ***** 01/28/10 **********************************
     O****    E   62           ROUTE
     O***                                14 '(C) COPYRIGHT'
      ****** END DEL ***** 01/28/10 **********************************
      **** BEG ADD ***** 08/30/06 *************************************
     O***************                  +001 'PROGRESSIVE DATA'
      ****** BEG DEL ***** 01/28/10 **********************************
     O****                             +001 'FISERV INSURANCE'
      **** END ADD ***** 08/30/06 *************************************
     O****                             +001 'SOLUTIONS, INC.,'
      ****** END DEL ***** 01/28/10 **********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ***** END ADD ****** 01/28/10 ***********************************
      ****************************************************************
      * PAGE HEADINGS
      ****************************************************************
      *
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM              86
     O                       XXDATE        Y    132
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
      *
      *  NEW PAGE HEADER(GROUP/EMP#/CLMT NAME/SS#/CASE/STAFF CHANGES)
      *
     O          E            HEAD        2  1
     O              N30                          16 'EMPLOYER'
     O               30                          48 'EMPLOYER'
     O                                           78 'CASE'
     O                                           86 'COUNTY'
     O          E            HEAD           0
     O                                            5 'GROUP'
     O              N30                          16 'NUMBER'
     O              N30                          23 'DIV'
     O               30                          20 'CLAIMANT NAME'
     O              N30                          38 'CLAIMANT NAME'
     O               30                          48 'NUMBER'
     O               30                          55 'DIV'
     O                                           68 'SS NUMBER'
     O                                           78 'NUMBER'
     O                                           84 'CODE'
     O                                          102 'STAFF CODE NAME'
     O               70                         120 'FROM     TO'
     O              N70                         116 'ASSIGN'
     O          E            HEAD        0  2
     O                                           24 '________________________'
     O                                           48 '________________________'
     O                                           72 '________________________'
     O                                           96 '________________________'
     O              N70                         117 '____________________'
     O               70                         120 '_______________________'
      *
      *  PRINT DETAIL LINES
      *
     O          E            DET            0
     O                       XXFND         Z      5
     O              N30      CMEMP#        Z     16
     O              N30      CMDIV         Z     23
     O               30      CDLSN               37
     O              N30      CDLSN               55
     O               30      CMEMP#        Z     48
     O               30      CMDIV         Z     55
     O*** BEGIN DLT *** 08/16/99 **************************************
     O************************ ZZSS#     68 '0   -  -    '
     O*** END DLT ***** 08/16/99 **************************************
     O*** BEGIN ADD *** 08/16/99 **************************************
     O                       XXSS1               60
     O                                           61 '-'
     O                       XXSS2               63
     O                                           64 '-'
     O                       XXSS3               68
     O*** END ADD ***** 08/16/99 **************************************
     O                       CMCASE        Z     78
     O                       XXCNTY              84
      *
     O          E            DET1           1
     O                       XXSTNM             107
     O*** Begin chg *** 06/12/03 **************************************
     O************************ XXFCDE   113
     O              N70      XXTCDE             113
     O               70      XXFCDE             113
     O*** End chg ***** 06/12/03 **************************************
     O               70      XXTCDE             120
      *
     O          E            NOREC
     O                                         +005 '.....NO RECORDS MEET'
     O                                         +001 'THE SELECTION CRITERIA'
     O                                         +000 '.....'
      *
     O          E            DET2           1
      **** BEGIN ADD **** 04/19/10 ************************************
**  TABLE DRT
CLAIMANT NAME & CITY:
ACCIDENT DATE:
USER NAME:
OLD CLAIMANT NAME:
