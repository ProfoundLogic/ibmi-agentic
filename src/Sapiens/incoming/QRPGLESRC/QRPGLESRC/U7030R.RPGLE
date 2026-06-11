     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE U7030R - REVIEW DATE DIARY REPORT - PRT PGM
     F*
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
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H*  07/05/94   C:20    REQ/PROJ: URMNT                           *
     H*                     1. MODIFICATION TO KEEP AND USE UPOUTQ    *
     H*                        FROM THE USER SUBMITTING THE JOB.  IT  *
     H*                        GETTING REPLACED WITH THE INTERVIEWERS *
     H*                        THAT WAS BEING DISPLAYED ON THE REPORT *
     H*                        WHEN WE CHAIN TO GET THEN INTERVIEWER  *
     H*                        NAME.                                  *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FUMPCL1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXSS#                  1      9
     D  SSPRE                  1      3
     D  SSMID                  4      5
     D  SSSUF                  6      9
     D                 DS
     D  FNNM                   1     60
     D                                     DIM(60) ASCEND
     D  FNNAME                 1     60
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** Beg Add *** 08/05/13 ***************************************
     D xxICD1          s              7
     D*** End Add *** 08/05/13 ***************************************

     IPUP02
     I              UPUSR                       U2USR
     I              UPCO#                       U2CO#
     I              UPIPGM                      U2IPGM
     I              UPLVL                       U2LVL
     I              UPTYPE                      U2TYPE
     I              UPUSNM                      U2USNM
     I              UPTITL                      U2TITL
     I              UPSTAF                      U2STAF
     I              UPOUTQ                      U2OUTQ
     I              UPPRDT                      U2PRDT
     I              UPSTAT                      U2STAT
     I              UPUSER                      U2USER
     I              UPPGM                       U2PGM
     I              UPTIME                      U2TIME
     I              UPDT                        U2DT
     I              UPIUSR                      U2IUSR
     I              UPIDT                       U2IDT
     I              UPFND                       U2FND
     I              UPFYR                       U2FYR
     I              UPEMP#                      U2EMP#
     I              UPRAL                       U2RAL
     I              UPDIV                       U2DIV
     I              UPALIC                      U2ALIC
     I              UPSUB#                      U2SUB#
     I              UPRGN                       U2RGN
     I              UPOFC#                      U2OFC#
     I              UPEXT#                      U2EXT#
     I              UPLOB                       U2LOB
     I              UPPAL                       U2PAL
     I              UPDT1                       U2DT1
     I              UPDT2                       U2DT2
     I              UPDT3                       U2DT3
     I              UPDT4                       U2DT4
     I              UPDT5                       U2DT5
     I              UPFL1                       U2FL1
     I              UPFL2                       U2FL2
     I              UPFL3                       U2FL3
     I              UPFL4                       U2FL4
     I              UPFL5                       U2FL5
     I              UPNU1                       U2NU1
     I              UPNU2                       U2NU2
     I              UPNU3                       U2NU3
     I              UPNU4                       U2NU4
     I              UPNU5                       U2NU5
     I              UPAL1                       U2AL1
     I              UPAL2                       U2AL2
     I              UPAL3                       U2AL3
     I              UPAL4                       U2AL4
     I              UPAL5                       U2AL5

     C*
     C* COMPNAY MASTER FILE
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
     C     UPKY1         KLIST
     C                   KFLD                    PCINTR
     C*
     C     CMKEY         KLIST
     C                   KFLD                    PCCO#
     C                   KFLD                    PCSUB#
     C                   KFLD                    PCFND
     C                   KFLD                    PCFYR
     C                   KFLD                    PCCASE
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   KEY LIST FOR THE PROVIDER
     C*
     C     PDKEY         KLIST
     C                   KFLD                    PCFEIN
     C                   KFLD                    PCMOD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQINTR            3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
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
     C     *LIKE         DEFINE    PCINTR        XXINTR                         3A
     C*
     C                   Z-ADD     *ZEROS        XXTCNT            5 0          5.0N
     C                   MOVE      *BLANKS       XXPRT             1            1A
     C                   MOVE      *BLANKS       XXCNAM           25            25A
     C                   MOVE      *BLANKS       XXFNNM           40            40A
     C                   MOVE      *BLANKS       XXSBNM           40            40A
     C                   MOVE      *BLANKS       XXPRNM           40            40A
     C                   MOVE      *BLANKS       XXUSNM           40            40A
     C                   MOVE      '-'           X1HYPH            1
     C                   MOVE      '-'           X2HYPH            1
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQINTR        XXINTR
     C                   MOVE      QQFRDT        X1FRDT            8 0
     C*
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   MOVE      QQTODT        X1TODT            8 0
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
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
     C                   MOVE      '0'           *IN18
     C                   MOVE      '0'           *IN19
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN21
     C*
     C     XXSUB#        IFNE      *ZEROS
     C                   MOVE      '1'           *IN18
     C                   END
     C     XXFND         IFNE      *ZEROS
     C                   MOVE      '1'           *IN19
     C                   END
     C     XXINTR        IFNE      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   END
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
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C* PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********  N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C* GET THE SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C* GET THE FUND NAME
     C*
     C                   MOVEL     *BLANKS       XZFNNM           40
     C                   MOVEL     *BLANKS       XXFNNM
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    FNNM(Z)                            60
     C                   MOVEA     FNNM(Z)       XZFNNM                         FOR REPORT
     C                   END
     C*
     C* GET THE INTERVIEWER NAME
     C*
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     U2USNM        XXUSNM                         ADD 07/05/94
     C                   ELSE
     C                   MOVE      *BLANKS       XXUSNM
     C                   END
     C*
     C* PRINT THE HEADING NOW IF SUB#,FND AND INTR WERE SELECTED
     C*
     C     *IN18         IFEQ      '1'
     C     *IN19         ANDEQ     '1'
     C     *IN20         ANDEQ     '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C*
     C                   END                                                    END 18 & 19
     C*
     C* PROCESS THE CORRECT LOGICAL
     C*
     C                   READ      PPC01                                  60
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
     C* CHECK TO SEE IF SUBSCRIBER NUMBER CHANGES
     C*
     C     PCSUB#        IFNE      XXSUB#
     C*
     C* IF NOT 1ST TIME THRU EXCPT TOTALS
     C*
     C     XXTCNT        IFGT      0
     C                   EXCEPT    TOT
     C                   Z-ADD     *ZEROS        XXTCNT
     C                   END
     C*
     C                   Z-ADD     PCSUB#        XXSUB#
     C                   Z-ADD     PCFND         XXFND
     C                   MOVE      *BLANKS       XXINTR
     C                   MOVE      PCINTR        XXINTR
     C*
     C* GET THE SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C* GET THE FUND NAME
     C*
     C                   MOVEL     *BLANKS       XZFNNM           40
     C                   MOVEL     *BLANKS       XXFNNM
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    FNNM(Z)                            60
     C                   MOVEA     FNNM(Z)       XZFNNM                         FOR REPORT
     C                   END
     C*
     C                   EXCEPT    HEAD
     C*
     C* GET THE INTERVIEWER NAME
     C*
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXUSNM                         ADD 07/05/94
     C                   ELSE
     C                   MOVE      *BLANKS       XXUSNM
     C                   END
     C*
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   END
     C*
     C* CHECK TO SEE IF FUND NUMBER CHANGES
     C*
     C     PCFND         IFNE      XXFND
     C*
     C* IF NOT 1ST TIME THRU EXCPT TOTALS
     C*
     C     XXTCNT        IFGT      0
     C                   EXCEPT    TOT
     C                   Z-ADD     *ZEROS        XXTCNT
     C                   END
     C*
     C                   Z-ADD     PCFND         XXFND
     C                   MOVE      *BLANKS       XXINTR
     C                   MOVE      PCINTR        XXINTR
     C*
     C* GET THE FUND NAME
     C*
     C                   MOVEL     *BLANKS       XZFNNM           40
     C                   MOVEL     *BLANKS       XXFNNM
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM
     C                   Z-ADD     1             Z                 5 0
     C     *BLANK        LOOKUP    FNNM(Z)                            60
     C                   MOVEA     FNNM(Z)       XZFNNM                         FOR REPORT
     C                   END
     C*
     C                   EXCEPT    HEAD
     C*
     C* GET THE INTERVIEWER NAME
     C*
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXUSNM                         ADD 07/05/94
     C                   ELSE
     C                   MOVE      *BLANKS       XXUSNM
     C                   END
     C*
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   END
     C*
     C* CHECK TO SEE IF THE INTERVIEWER HAS CHANGED
     C*
     C     PCINTR        IFNE      XXINTR
     C*
     C* IF NOT 1ST TIME THRU EXCPT TOTALS
     C*
     C     XXTCNT        IFGT      0
     C                   EXCEPT    TOT
     C                   Z-ADD     *ZEROS        XXTCNT
     C                   END
     C*
     C                   MOVE      *BLANKS       XXINTR
     C                   MOVE      PCINTR        XXINTR
     C*
     C* GET THE INTERVIEWER NAME
     C*
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     U2USNM        XXUSNM                         ADD 07/05/94
     C                   ELSE
     C                   MOVE      *BLANKS       XXUSNM
     C                   END
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   END
     C*
     C* GET THE CLAIMANT NAME FROM THE PRE-CERT RECORD
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMOD            10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C     CMSS#         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCNAM
     C                   END                                                    END CDFSN
     C                   END                                                    END *IN60
     C                   END                                                    END *IN60
     C*
     C* CONVERT PRE-CERT RECORD DATES TO CORRECT FORMAT FOR PRINTING
     C*
     C*
     C                   Z-ADD     PCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXADDT            6 0
     C*
     C                   Z-ADD     PCRVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXRVDT            6 0
     C*
     C                   Z-ADD     PCEPD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEPD1            6 0
     C*
     C* GET THE PROVIDER NAME
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       X1LAST           56
     C                   MOVE      *BLANKS       X1FRST           15
     C                   MOVEL     PDNAME        X1LAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         X1FRST           15
     C                   MOVE      '1'           X1TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    X1LAST
     C                   PARM                    X1FRST
     C                   PARM                    X1TERM
     C                   MOVEL     X1LAST        XXPRNM
     C                   ELSE
     C                   MOVEL     PDNAME        XXPRNM
     C                   END                                                    END CDFSN
     C                   ELSE
     C                   MOVE      *BLANKS       XXPRNM
     C                   END                                                    END *IN60
     C
     C*
     C*** Beg Add *** 08/05/13 ****************************************
     C* GET THE DIAGNOSIS CODE
      /Free
         If PCICDIND = 'Y';
             XXicd1 = %trimR(PCICD1001);
         Else;
             XXicd1 = PCICD1;
         EndIf;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C* ACCUMULATE THE TOTAL COUNT FOR THE INTERVIEWER
     C*
     C                   ADD       1             XXTCNT
     C*
     C* PRINT THE DETAIL LINE
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    DET
     C*
     C* EXCPT DETAIL FOR MORE PROC CODES IF PRESSENT
     C*
     C     PCCPT2        IFNE      *BLANKS
     C*
     C                   Z-ADD     PCEPD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEPD2            6 0
     C*
     C                   EXCEPT    DET1
     C                   END
     C*
     C     PCCPT3        IFNE      *BLANKS
     C*
     C                   Z-ADD     PCEPD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEPD3            6 0
     C*
     C                   EXCEPT    DET2
     C                   END
     C*
     C     PCCPT4        IFNE      *BLANKS
     C*
     C                   Z-ADD     PCEPD4        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEPD4            6 0
     C*
     C                   EXCEPT    DET3
     C                   END
     C*
     C     PCCPT5        IFNE      *BLANKS
     C*
     C                   Z-ADD     PCEPD5        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEPD5            6 0
     C*
     C                   EXCEPT    DET4
     C                   END
     C*
     C     PCCPT6        IFNE      *BLANKS
     C*
     C                   Z-ADD     PCEPD6        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEPD6            6 0
     C*
     C                   EXCEPT    DET5
     C                   END
     C*
     C     PCCPT7        IFNE      *BLANKS
     C*
     C                   Z-ADD     PCEPD7        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEPD7            6 0
     C*
     C                   EXCEPT    DET6
     C                   END
     C*
     C     PCCPT8        IFNE      *BLANKS
     C*
     C                   Z-ADD     PCEPD8        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEPD8            6 0
     C*
     C                   EXCEPT    DET7
     C                   END
     C*
     C* GET THE NEXT PRE-CERT RECORD
     C*
     C                   READ      PPC01                                  60
     C*
     C                   END                                                    END DOW 60
     C*
     C* PRINT THE TOTALS FOR THE LAST INTERVIEWER
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    TOT
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
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
     O                                           26 'INTERVIEWER    :'
     O               20      XXINTR              31
     O              N20                          32 '*ALL'
     O          E            ROUTE          2
     O                                           26 'REVIEW DT RANGE:'
     O                       XXFRDT              36 '  /  /  '
     O                                           39 'TO'
     O                       XXTODT              48 '  /  /  '
     O          E            ROUTE          2
     O                                           26 'RUN DATE       :'
     O                       DSDATE              36 '  /  /  '
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 1997      '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              97
     O                       DSDATE        Y    167
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          163 'PAGE:'
     O                       PAGE          Z    167
     O          E            HEAD           2
     O                                           62 'REVIEW DATE RANGE:'
     O                       XXFRDT              71 '  /  /  '
     O                                           75 'TO'
     O                       XXTODT              84 '  /  /  '
     O          E            HEAD           1
     O                                           12 'SUBSCRIBER.:'
     O                       XXSUB#              19
     O                                           21 '-'
     O                       XXSBNM              62
     O          E            HEAD           1
     O                                           12 'GROUP......:'
     O                       XXFND               19
     O                                           21 '-'
     O                       XZFNNM              62
     O          E            HEAD1          2
     O                                           12 'INTERVIEWER:'
     O                       XXINTR              19
     O                                           21 '-'
     O                       XXUSNM              62
     O          E            HEAD2          1
     O                                           47 'ACCIDENT'
     O                                           77 'ADMIT'
     O                                           88 'REVIEW'
     O*** Beg Chg *** 08/05/13 ****************************************
     O*****                                     137 'PRIM'
     O                                          135 'PRIM'
     O*** End Chg *** 08/05/13 ****************************************
     O                                          143 'REC'
     O                                          150 'PROC'
     O                                          155 '#'
     O                                          166 'END OF'
     O          E            HEAD2          1
     O                                           13 'CLAIMANT NAME'
     O                                           37 'SOC. SEC.#'
     O                                           45 'DATE'
     O                                           62 'PRE-CERT#'
     O                                           69 'EXT'
     O                                           76 'DATE'
     O                                           86 'DATE'
     O                                          104 'PROVIDER NAME'
     O*** Beg Chg *** 08/05/13 ****************************************
     O*****                                     137 'DIAG'
     O                                          135 'DIAG'
     O*** End Chg *** 08/05/13 ****************************************
     O                                          144 'DAYS'
     O                                          150 'CODE'
     O                                          157 'VIST'
     O                                          167 'AUTH DOS'
     O          E            DET         1  1
     O                       XXCNAM              25
     O                       XXSS#               37
     O                       XXACDT              47 '  /  /  '
     O                       PCCERT              64
     O                       PCEXT         Z     69
     O                       XXADDT              79 '  /  /  '
     O                       XXRVDT              89 '  /  /  '
     O                       XXPRNM             131
     O*** Beg Chg *** 08/05/13 ****************************************
     O*****                  PCICD1             138
     O                       xxICD1             138
     O*** End Chg *** 08/05/13 ****************************************
     O                       PCRECD        Z    143
     O                       PCCPT1             151
     O                       PCA#V1        Z    156
     O                       XXEPD1             167 '  /  /  '
     O          E            DET1           1
     O                       PCCPT2             151
     O                       PCA#V2        Z    156
     O                       XXEPD2             167 '  /  /  '
     O          E            DET2           1
     O                       PCCPT3             151
     O                       PCA#V3        Z    156
     O                       XXEPD3             167 '  /  /  '
     O          E            DET3           1
     O                       PCCPT4             151
     O                       PCA#V4        Z    156
     O                       XXEPD4             167 '  /  /  '
     O          E            DET4           1
     O                       PCCPT5             151
     O                       PCA#V5        Z    156
     O                       XXEPD5             167 '  /  /  '
     O          E            DET5           1
     O                       PCCPT6             151
     O                       PCA#V6        Z    156
     O                       XXEPD6             167 '  /  /  '
     O          E            DET6           1
     O                       PCCPT7             151
     O                       PCA#V7        Z    156
     O                       XXEPD7             167 '  /  /  '
     O          E            DET7           1
     O                       PCCPT8             151
     O                       PCA#V8        Z    156
     O                       XXEPD8             167 '  /  /  '
     O          E            TOT         2
     O                                           62 'INTERVIEWER TOTAL PRE/R'
     O                                           79 'E-CERTIFICATIONS:'
     O                       XXTCNT        Z     89
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
     O*
