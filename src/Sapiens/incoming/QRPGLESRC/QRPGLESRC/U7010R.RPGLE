     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE U7010R - PRE-CERTIFICATION REPORT - PRT PGM
     H*
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*  08    Batch Mode                                             *
     H*  09    Demand Mode                                            *
     H*  60    General Chain/Usage                                    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*  08/05/13 C:277 PROJECT: MC329 Phase II                       *
     H*                 Print ICD10 if ICD10 is active else Print ICD9*
     H*                                                               *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H*  03/14/00 C:62 PROJECT: MCMNT                                 *
     H*                CHANGE DATE FORMATS TO 8.0                     *
     H*                                                               *
     H* 02/23/96 C:32 REQ/PROJ: 171/MC131                             *
     H*               USE LPAS INSTEAD OF PASG (LOS PAS GROUP)        *
     H*               INCREASED SIZE FROM 4 DIGITS TO 6 CHARS         *
     H*                                                               *
     H* 02/24/94 C:20 Project: URMNT                                  *
     H*               Revise key to ICD9 Code File to use PCLOS1(TYPE FOR
     H*               ICD9 CODE 1 NOT PCLOST FOR LENGTH OF STAY DIAG OR OPER
     H*                                                               *
     H* 02/16/94 C:05 Project: URMNT                                  *
     H*               Revise key to ICD9 Code File to use LOS Type (D vs. O)
     H*                                                               *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FUXPCL     IF   E           K DISK
     FUXPCL1    IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FUXPCL2    IF   E           K DISK
     F                                     RENAME(PPC01:PPC03)
     FUMLSL     IF   E           K DISK
     FUMLSL1    IF   E           K DISK
     F                                     RENAME(PLS01:PLS02)
     FUMPCL     IF   E           K DISK
     F                                     RENAME(PPC01:PPC04)
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     F*
     F*** Beg Add *** 08/05/13 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ***************************************

     D TYAD            S              1    DIM(5) CTDATA PERRCD(1) ASCEND       TYPE/DESC OF SERVICE
     D DSAD            S             10    DIM(5) ALT(TYAD)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  XXLPAS                 1     11
     D  XXRGN                  1      1  0
     D  XXGRP                  2     11
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** Beg Add *** 08/05/13 ***************************************
     D xxICD           s              7
     D kkicd10         s                   like(iccode)
     D kkLost          s                   like(DIlost)
     D*** End Add *** 08/05/13 ***************************************

     I*
     IPPC04
     I              PCCO#                       P4CO#
     I              PCSUB#                      P4SUB#
     I              PCFND                       P4FND
     I              PCFYR                       P4FYR
     I              PCCASE                      P4CASE
     I              PCLIAB                      P4LIAB
     I              PCSTAT                      P4STAT
     I              PCADDT                      P4ADDT
     I              PCRVDT                      P4RVDT
     I              PCCERT                      P4CERT
     I              PCEXT                       P4EXT
     I              PCFEIN                      P4FEIN
     I              PCMOD                       P4MOD
     I              PCICD1                      P4ICD1
     I              PCICD2                      P4ICD2
     I              PCICD3                      P4ICD3
     I              PCICD4                      P4ICD4
     I              PCICD5                      P4ICD5
     I              PCLOS1                      P4LOS1
     I              PCLOS2                      P4LOS2
     I              PCLOS3                      P4LOS3
     I              PCLOS4                      P4LOS4
     I              PCLOS5                      P4LOS5
     I              PCCPT1                      P4CPT1
     I              PCCPT2                      P4CPT2
     I              PCCPT3                      P4CPT3
     I              PCCPT4                      P4CPT4
     I              PCCPT5                      P4CPT5
     I              PCCPT6                      P4CPT6
     I              PCCPT7                      P4CPT7
     I              PCCPT8                      P4CPT8
     I              PCSPD1                      P4SPD1
     I              PCSPD2                      P4SPD2
     I              PCSPD3                      P4SPD3
     I              PCSPD4                      P4SPD4
     I              PCSPD5                      P4SPD5
     I              PCSPD6                      P4SPD6
     I              PCSPD7                      P4SPD7
     I              PCSPD8                      P4SPD8
     I              PCEPD1                      P4EPD1
     I              PCEPD2                      P4EPD2
     I              PCEPD3                      P4EPD3
     I              PCEPD4                      P4EPD4
     I              PCEPD5                      P4EPD5
     I              PCEPD6                      P4EPD6
     I              PCEPD7                      P4EPD7
     I              PCEPD8                      P4EPD8
     I              PCA#V1                      P4A#V1
     I              PCA#V2                      P4A#V2
     I              PCA#V3                      P4A#V3
     I              PCA#V4                      P4A#V4
     I              PCA#V5                      P4A#V5
     I              PCA#V6                      P4A#V6
     I              PCA#V7                      P4A#V7
     I              PCA#V8                      P4A#V8
     I              PCTOS1                      P4TOS1
     I              PCTOS2                      P4TOS2
     I              PCTOS3                      P4TOS3
     I              PCTOS4                      P4TOS4
     I              PCTOS5                      P4TOS5
     I              PCTOS6                      P4TOS6
     I              PCTOS7                      P4TOS7
     I              PCTOS8                      P4TOS8
     I              PCINTR                      P4INTR
     I              PCPASG                      P4PASG
     I              PCREQD                      P4REQD
     I              PCCERD                      P4CERD
     I              PCRECD                      P4RECD
     I              PCPOS                       P4POS

     C*
     C* COMPANY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C* OUTPUT ROUTINF FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C* USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  LENGTH OF STAY FILE
     C*
     C     LSKEY         KLIST
     C                   KFLD                    XXRGN
     C                   KFLD                    XXGRP
     C                   KFLD                    XXCAT
     C                   KFLD                    XXAGE
     C                   KFLD                    XXEFDT
     C*
     C* ICD9 DIAGNOSIS CODE FILE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXICD9
     C                   KFLD                    XXLOST                         ADD 02/16/93
     C*
     C* PRE-CERT MASTER FILE
     C*
     C     PCKY          KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCCERT
     C                   KFLD                    PCEXT
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C                   kfld                    kklost
     C*** End Add *** 08/05/13 ***************************************
     C*
     C     *LIKE         DEFINE    LSDT          XXEFDT
     C     *LIKE         DEFINE    LSDT          X1FRDT
     C     *LIKE         DEFINE    LSDT          X1TODT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQFRDT            7
     C                   PARM                    QQTODT            7
     C                   PARM                    QQSTAT            1
     C                   PARM                    QQSORT            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    PCCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PCSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    PCFND         XXFND                          3.0N
     C     *LIKE         DEFINE    PCCAT         XXCAT                          1.0N
     C     *LIKE         DEFINE    PCAGE         XXAGE                          1.0N
     C     *LIKE         DEFINE    PCLOST        XXLOST                         1A
     C*
     C                   Z-ADD     *ZEROS        XXEMP#            9 0          5.0N
     C                   Z-ADD     *ZEROS        XX50P             5 0          5.0N
     C                   Z-ADD     *ZEROS        XX75P             5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTCER            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTTCD            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTTAC            5 0          5.0N
     C                   Z-ADD     *ZEROS        TOTSAV            5 0          5.0N
     C                   Z-ADD     *ZEROS        X1FRDT                         7.0N
     C                   Z-ADD     *ZEROS        X1TODT                         7.0N
     C                   MOVE      *BLANKS       XXSORT            1            1A
     C                   MOVE      *BLANKS       XXSTAT            1            1A
     C                   MOVE      *BLANKS       XXCNAM           15            40A
     C                   MOVE      *BLANKS       XXNAME           15            40A
     C                   MOVE      *BLANKS       XXDSC1           15            30A
     C                   MOVE      *BLANKS       XXDSAD           10            20A
     C                   MOVE      *BLANKS       XXICD9            5            5A
     C                   MOVE      *BLANKS       XXPRT             1            1A
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQFRDT        X1FRDT
     C                   MOVE      QQTODT        X1TODT
     C                   MOVE      QQSTAT        XXSTAT
     C                   MOVE      QQSORT        XXSORT
     C*
     C                   TIME                    DSDTTM
     C*
     C* SET-UP AND INITIALIZE
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C* CHECK THE SORT SELECTION
     C*
     C                   MOVE      '0'           *IN15
     C                   MOVE      '0'           *IN16
     C                   MOVE      '0'           *IN17
     C                   MOVE      '0'           *IN18
     C                   MOVE      '0'           *IN19
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN21
     C                   MOVE      '0'           *IN22
     C                   MOVE      '0'           *IN23
     C                   MOVE      '0'           *IN24
     C*
     C     XXSORT        IFEQ      'D'
     C                   MOVE      '1'           *IN15
     C                   END
     C     XXSORT        IFEQ      'P'
     C                   MOVE      '1'           *IN16
     C                   END
     C     XXSORT        IFEQ      'C'
     C                   MOVE      '1'           *IN17
     C                   END
     C     XXSUB#        IFNE      *ZEROS
     C                   MOVE      '1'           *IN18
     C                   END
     C     XXFND         IFNE      *ZEROS
     C                   MOVE      '1'           *IN19
     C                   END
     C     XXEMP#        IFNE      *ZEROS
     C                   MOVE      '1'           *IN20
     C                   END
     C     XXSTAT        IFEQ      'O'
     C                   MOVE      '1'           *IN22
     C                   END
     C     XXSTAT        IFEQ      'A'
     C                   MOVE      '1'           *IN23
     C                   END
     C     XXSTAT        IFEQ      'C'
     C                   MOVE      '1'           *IN24
     C                   END
     C*
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT            8 0
     C*
     C* GET THE USER PARAMETERS FILE.
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C* CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C* GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C* PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C*********  N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C* PRINT HEADINGS
     C*
     C                   EXCEPT    HEAD
     C*
     C* PROCESS THE CORRECT LOGICAL
     C*
     C   15
     CANN16
     CANN17              READ      PPC01                                  60
     C  N15
     CAN 16
     CANN17              READ      PPC02                                  60
     C  N15
     CANN16
     CAN 17              READ      PPC03                                  60
     C*
     C* IF NO RECORDS PRINT ERROR MESSAGE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END                                                    END *IN60
     C*
     C* START LOOP
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C* MOVE PROV. NAME AND CLAIMANT NAME INTO SHORTER PRINTABLE FIELDS
     C*
     C                   MOVE      *BLANKS       XXCNAM
     C                   MOVEL     PCCNAM        XXCNAM
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVEL     PCNAME        XXNAME
     C*
     C* CONVERT ACDT/ADDT TO MM/DD/YY FORMAT FOR PRINTING
     C*
     C                   Z-ADD     *ZEROS        XXADDT
     C                   Z-ADD     *ZEROS        XXACDT
     C*
     C                   Z-ADD     PCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXADDT            8 0
     C*
     C                   Z-ADD     PCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C* CHAIN TO PRE-CERT MASTER FILE TO GET INFORMATION
     C*
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
                  XXdsc1 = *Blanks;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C     PCKY          CHAIN     PPC04                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     P4LOS1        XXLOST
     C                   END
     C*
     C* GET THE DIAGNOSIS CODE DESCRIPTION
     C*
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If PCicdInd = '1' Or PCicdInd = 'Y';
         kkICd10 = %trim(PCICD1001);
         kklost = P4LOS1;
         Chain fmicd10l1key pic01;
            If %found(fmicd10l1);
            Dow not %eof(fmicd10l1);
               If ICstat <> 'I';
                  XXdsc1 = ICdsc1;
                  Leave;
               Endif;
            Reade fmicd10l1key pic01;
            Enddo;
            Else;
                  XXdsc1 = *Blanks;
            Endif;
         Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVEL     PCICD1        XXICD9
     C     DIKEY         SETGT     PDI01
     C     NXTDC3        TAG
     C                   READP     PDI01                                  60
     C     *IN60         IFEQ      '0'
     C     DISTAT        IFNE      'I'
     C                   MOVEL     DIDSC1        XXDSC1
     C                   ELSE                                                   ELSE *IN60=1
     C                   GOTO      NXTDC3
     C                   END                                                    END DISTAT
     C                   END                                                    END *IN60
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
          Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C* GET THE TYPE OF SERVICE DESCRIPTION
     C*
     C                   Z-ADD     1             X                 5 0
     C     PCTYAD        LOOKUP    TYAD(X)                                60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     DSAD(X)       XXDSAD
     C                   ELSE
     C                   MOVE      *BLANKS       XXDSAD
     C                   END                                                    END *IN60
     C*
     C* GET THE LOS PERCENTAGES
     C* CHAIN TO APPROPRIATE FILE DEPENDING ON PCLOST 'O' OR 'D'
     C*
     C                   MOVEL(P)  PCLPAS        XXLPAS                         ADD 02/23/96
     C                   Z-ADD     PCAGE         XXAGE
     C                   Z-ADD     PCCAT         XXCAT
     C                   Z-ADD     *ZEROS        XX50P
     C                   Z-ADD     *ZEROS        XX75P
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     XXLPAS        IFNE      *BLANKS
     C     PCLOST        IFEQ      'D'
     C     LSKEY         SETGT     PLS01
     C                   READP     PLS01                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      LS50P         XX50P
     C                   MOVE      LS75P         XX75P
     C                   ELSE
     C                   Z-ADD     *ZEROS        XX50P
     C                   Z-ADD     *ZEROS        XX75P
     C                   END
     C                   END
     C*
     C     PCLOST        IFEQ      'O'
     C     LSKEY         SETGT     PLS02
     C                   READP     PLS02                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      LS50P         XX50P
     C                   MOVE      LS75P         XX75P
     C                   ELSE
     C                   Z-ADD     *ZEROS        XX50P
     C                   Z-ADD     *ZEROS        XX75P
     C                   END
     C                   END
     C                   ELSE
     C                   Z-ADD     *ZEROS        XX50P
     C                   Z-ADD     *ZEROS        XX75P
     C                   END
     C*
     C* ACCUMULATE THE TOTALS
     C*
     C                   ADD       PCRECD        TOTCER
     C                   ADD       PCTCED        TOTTCD
     C                   ADD       PCTACD        TOTTAC
     C                   ADD       PCTSAV        TOTSAV
     C*
     C* PRINT THE DETAIL LINE
     C*
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMOD            10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'PCSS#'       QQNAME           25
     C     PCSS#         PARM      PCSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    DET
     C*
     C* GET THE NEXT PRE-CERT WORKFILE RECORD
     C*
     C   15
     CANN16
     CANN17              READ      PPC01                                  60
     C  N15
     CAN 16
     CANN17              READ      PPC02                                  60
     C  N15
     CANN16
     CAN 17              READ      PPC03                                  60
     C*
     C                   END                                                    END DOW 60
     C*
     C* PRINT HEADINGS
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    TOT
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
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
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
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
     O          E            ROUTE          2 33
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           26 'COMPANY        :'
     O                       XXCO#               31
     O          E            ROUTE          2
     O                                           26 'SUBSCRIBER     :'
     O               18      XXSUB#              33
     O              N18                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'GROUP          :'
     O               19      XXFND               31
     O              N19                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'EMPLOYER       :'
     O               20      XXEMP#              37
     O              N20                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'ADMIT DT RANGE :'
     O                       XXFRDT              38 '  /  /    '
     O                                           41 'TO'
     O                       XXTODT              52 '  /  /    '
     O          E            ROUTE          2
     O                                           26 'STATUS         :'
     O               22N23N24                    32 'OPEN'
     O               23N22N24                    31 'ALL'
     O               24N22N23                    34 'CLOSED'
     O          E            ROUTE          2
     O                                           26 'SORT SELECTION :'
     O                       XXSORT              38
     O               15N16N17                    45 'PRIMARY DIAGNOSIS'
     O               16N15N17                    40 'PROVIDER/MOD'
     O               17N15N16                    41 'CLAIMANT NAME'
     O          E            ROUTE          2
     O                                           26 'RUN DATE       :'
     O                       DSDATE              38 '  /  /    '
     O                                         +000 '(C)'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              97
     O                       DSDATE        Y    195
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          189 'PAGE:'
     O                       PAGE          Z    195
     O          E            HEAD           1
     O                                          124 'TOTAL'
     O          E            HEAD           1
     O                                           55 'ACCIDENT'
     O                                           99 'ADMIT'
     O                                          120 'REC.'
     O                                          127 'REC.'
     O                                          133 '50%'
     O                                          139 '75%'
     O                                          146 'ACTUAL'
     O                                          152 'SAVED'
     O                                          159 'HOME'
     O                                          167 'P/A'
     O                                          183 'PT/'
     O                                          190 '2nd'
     O          E            HEAD           1
     O                                           06 'CERT #'
     O                                           30 'CLAIMANT NAME'
     O                                           44 'SOC. SEC.#'
     O                                           53 'DATE'
     O                                           70 'PROVIDER NAME'
     O                                           83 'DIAGNOSIS'
     O                                           99 'DATE'
     O                                          107 'TYPE'
     O                                          120 'DAYS'
     O                                          127 'DAYS'
     O                                          133 'TILE'
     O                                          139 'TILE'
     O                                          145 'DAYS'
     O                                          152 'DAYS'
     O                                          159 'HLTH'
     O                                          169 'REFER'
     O                                          175 'LIAB'
     O                                          184 'CHIRO'
     O                                          194 'OPINION'
     O                                          198 'IME'
     O          E            DET            1
     O                       PCCERT              15
     O                       XXCNAM              32
     O                       PCSS#               43
     O                       XXACDT              55 '  /  /    '
     O                       XXNAME              72
     O                       XXDSC1              89
     O                       XXADDT             101 '  /  /    '
     O                       XXDSAD             113
     O                       PCRECD        Z    120
     O                       PCTCED        Z    127
     O                       XX50P         Z    134
     O                       XX75P         Z    140
     O                       PCTACD        Z    145
     O                       PCTSAV        Z    152
     O                       PCHMHL             158
     O                       PCPARF             166
     O                       PCLIAB             174
     O                       PCPTCH             181
     O                       PC2OPN             190
     O                       PCIME              198
     O          E            TOT         1
     O                                           95 'TOTALS:'
     O                       TOTCER        Z    116
     O                       TOTTCD        Z    123
     O                       TOTTAC        Z    141
     O                       TOTSAV        Z    148
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
     O*
**  TYPE OF SERVICE/DESCRIPTION OF SERVICE
CCHIR
MMED
PPHY THER
SSUR
YPSY
