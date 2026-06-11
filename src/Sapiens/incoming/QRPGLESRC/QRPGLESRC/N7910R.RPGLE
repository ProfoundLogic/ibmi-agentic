     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H* PROGRAM: N7910R - PARTICIPATING PROVIDER NETWORK REPORT      **
     H*****************************************************************
     H*  REVISIONS:                                                  **
     H*                                                               *
     H* 06/09/20 C:212 Project : SY420                                *
     H*                Mask SSN                                       *
     H*                                                              **
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H*  01/11/99  C:94  PROJECT: MC152
     H*                  CHANGE PROGRAM TO USE FAEAP INSTEAD OF
     H*                  FAHBP, AND NATRP
     H*
     H*  01/10/89 LJD    REVISED OUTPUT - WAS GETTING NEW NAME       **
     H*  PROJ #: I0045   OF FUND AND SUBSCRIBER BEFORE OUTPUTTING    **
     H*                  TOTAL FOR OLD (THEREFORE WAS DISPLAYING     **
     H*                  WRONG NAME)                                 **
     H*  02/07/90 LJD    REVISED CALCS OF XXMRED (MRA REDUCTIONS)    **
     H*                  BECAUSE IT WAS NOT TAKING INTO CONSIDERATION**
     H*                  THAT A NETWORK DISCOUNT HAD ALREADY BEEN    **
     H*                  APPLIED TO THE AMOUNT PAID - THEREFORE,     **
     H*                  MRA WAS APPEARING WHEN IT WAS NOT PRESENT   **
     H*  03/01/90 LJD    REVISED CALCS OF XXMRED (MRA REDUCTIONS):   **
     H*                  EOAMTC - EOAMT - EOOTHR = XXMRED            **
     H*  12/10/92 C:20   REVISED OUTPUT TO PRINT FEE RATHER THAN     **
     H*                  INVOICE AMOUNT                              **
     H*****************************************************************
     H*  INDICATORS:                                                 **
     H*  08      BATCH MODE                                          **
     H*  09      DEMAND MODE                                         **
     H*  10      DETAIL REPORT                                       **
     H*  11      SUMMARY REPORT                                      **
     H*  15      CHG IN FUND = DETERMINE WHAT IS PRINTED (OUTPUT SPEC)*
     H*  16      CHG IN SUB  = DETERMINE WHAT IS PRINTED (OUTPUT SPEC)*
     H*  17      CHG IN PROV = DETERMINE WHAT IS PRINTED (OUTPUT SPEC)*
     H*  60      GENERAL CHAIN                                       **
     H*  65      GENERAL CHAIN (CHAIN TO PHB01/PTR01)                **
     H*  90      PRIMARY CHAIN/DOWEQ (PEO01)                         **
     H*  95      GENERAL CHAIN (CHAIN TO PCM01)                      **
     H*  96      GENERAL CHAIN (CHAIN TO PCD01)                      **
     H*****************************************************************
     F* * * ROUTING FILE
     FSMORL     IF   E           K DISK
     F* * * COMPANY FILE
     FSMCOL     IF   E           K DISK
     F* * * FUND FILE
     FFMFNL     IF   E           K DISK
     F* * * USER PARAMETER FILE
     FSMUPL     IF   E           K DISK
     F* * * HBA FILE
     FFAHBL     IF   E           K DISK
     F* * * EOB WORK FILE
     FTAEOL     IF   E           K DISK
     F* * * CLAIM FILE
     FFMCML1    IF   E           K DISK
     F* * * DEMOGRAPHIC FILE
     FFMCDL     IF   E           K DISK
     F* * * SUBSCRIBER FILE
     FFMSML     IF   E           K DISK
     F* * * NETWORK TRANSACTION FILE
     FNATRL     IF   E           K DISK
     F*** BEG ADD *** 01/11/99 ***************************************
     F* * * BILL HEADER AMOUNT FILE
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 01/11/99 ***************************************
     F* * * PRINTER FILES
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOF)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************

     D TOTALS          DS                  OCCURS(8)
     D  TLAMTC                 1      5P 2
     D  TLMRED                 6     10P 2
     D  TLOTHR                11     15P 2
     D  TLNDIS                16     20P 2
     D  TLSAV                 21     25P 2
     D  TLSMUS                26     30P 2
     D PRTDS1          DS
     D  FIL11                  1    200
     D  FIL12                201    366
     D  XXLIN1               367    368B 0
     D  XXPAG1               369    372B 0
     D PRTDS2          DS
     D  FIL21                  1    200
     D  FIL22                201    366
     D  XXLIN2               367    368B 0
     D  XXPAG2               369    372B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  XF#                    1     30
     D  XFSUB                  1      5  0
     D  XFHYP1                 6      6
     D  XFFND                  7      9  0
     D  XFHYP2                10     10
     D  XFYR                  11     12  0
     D  XFHYP3                13     13
     D  XFCASE                14     20  0
     D  XFHYP4                21     21
     D  XFEOB#                22     30  0
     D                 DS
     D  XXSS#                  1      9
     D  XXPRE                  1      3
     D  XXMID                  4      5
     D  XXSUF                  6      9

     I*
     I*

     C*
     C*   KEY LIST FOR EOB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE CLAIMAMT DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*   KEY LIST FOR THE HBA FILE
     C*
     C     HBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*   KEY LIST FOR THE NETWORK TRANSACTION FILE
     C*
     C     TRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*** BEG ADD *** 01/11/99 ***************************************
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*** END ADD *** 01/11/99 ***************************************
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    UPDT          XXFROM
     C     *LIKE         DEFINE    UPDT          XXTO
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQRTYP            1
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
     C                   TIME                    DSDTTM
     C*
     C*** BEG ADD *** 01/11/99 ***************************************
     C*
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
     C*** END ADD *** 01/11/99 ***************************************
     C                   Z-ADD     0             X                 3 0
     C                   DO        8             X
     C     X             OCCUR     TOTALS
     C                   Z-ADD     0             TLAMTC
     C                   Z-ADD     0             TLMRED
     C                   Z-ADD     0             TLOTHR
     C                   Z-ADD     0             TLNDIS
     C                   Z-ADD     0             TLSAV
     C                   Z-ADD     0             TLSMUS
     C                   END
     C*
     C* MOVE ENTRY PARAMETERS INTO REPORT SELECTION FIELDS
     C     QQSUB#        IFEQ      *ZEROS
     C                   MOVEL     'ALL'         RSSUB#            5
     C                   ELSE
     C                   MOVE      QQSUB#        RSSUB#
     C                   END
     C     QQFND         IFEQ      *ZEROS
     C                   MOVE      'ALL'         RSFND             3
     C                   ELSE
     C                   MOVE      QQFND         RSFND
     C                   END
     C                   MOVE      QQRTYP        RSRTYP            1
     C*
     C                   MOVE      QQFRDT        X1FRDT            8 0
     C*
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   Z-ADD     XXFRDT        RSFRDT            6 0
     C*
     C                   MOVE      QQTODT        X1TODT            8 0
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C                   Z-ADD     XXTODT        RSTODT            6 0
     C*
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFROM
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTO
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      '-'           XFHYP1
     C                   MOVE      '-'           XFHYP2
     C                   MOVE      '-'           XFHYP3
     C                   MOVE      '-'           XFHYP4
     C*
     C*   SET INDICATORS TO DETERMINE WHICH REPORTS TO PRINT
     C*   SUMMARY/DETAIL
     C*
     C     QQRTYP        IFEQ      'B'
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           *IN11
     C                   ELSE
     C     QQRTYP        IFEQ      'D'
     C                   MOVE      '1'           *IN10
     C                   ELSE
     C     QQRTYP        IFEQ      'S'
     C                   MOVE      '1'           *IN11
     C                   END
     C                   END
     C                   END
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              30
     C     *IN30         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR QSYSPRT
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      ORTITL        XXTITL           60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C********   10              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR QSYSPRT1
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      ORTITL        YYTITL           60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C************   11              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C                   END
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CONAME        XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   END
     C*
     C*   READ EOB FILE
     C*
     C     EOKEY         SETLL     PEO01
     C                   READ      PEO01                                  90
     C*
     C     *IN90         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C   11              EXCEPT    HEAD1
     C   10              EXCEPT    ERRMSG
     C   11              EXCEPT    ERRMS1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C                   MOVE      EOCO#         XXCO#
     C                   MOVE      EOSUB#        XXSUB#
     C                   MOVE      EOFND         XXFND
     C                   MOVE      EOFYR         XXFYR
     C                   MOVE      EOCASE        XXCASE
     C                   MOVE      EOEOB#        XXEOB#
     C*
     C                   Z-ADD     EOSUB#        XFSUB
     C                   Z-ADD     EOFND         XFFND
     C                   Z-ADD     EOFYR         XFYR                           FILE NUMBER
     C                   Z-ADD     EOCASE        XFCASE
     C                   Z-ADD     EOEOB#        XFEOB#
     C*
     C                   Z-ADD     EOSUB#        LSTSUB            5 0
     C                   Z-ADD     EOFND         LSTFND            3 0
     C                   Z-ADD     EOFEIN        LSTFEN            9 0
     C                   Z-ADD     EOMOD         LSTMOD            3 0
     C                   MOVEL     EOPNAM        LSTPNM           40
     C*
     C*  CHAIN TO SUBSCRIBER FILE TO GET SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C*  CHAIN TO FUND FILE TO GET FUND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C*   PRINT THE HEADINGS
     C*
     C   10              EXCEPT    HEAD
     C   11              EXCEPT    HEAD1
     C   10              EXCEPT    PROHD
     C*
     C     *IN90         DOWEQ     '0'
     C*
     C     EOAMTC        SUB       EOAMT         XXMRED            9 2
     C     XXMRED        SUB       EOOTHR        XXMRED
     C*********  XXMRED    SUB  EONDIS    XXMRED           03/01/90 ADD
     C*
     C                   Z-ADD     XXMRED        XXTSAV            9 2
     C                   ADD       EOOTHR        XXTSAV
     C                   ADD       EONDIS        XXTSAV
     C*
     C*  GET REVIEWER AMOUNT
     C*
     C     EOSYS         IFEQ      'P'
     C                   Z-ADD     EOBAMT        XISMUS            9 2
     C                   END                                                    SYS = P
     C*
     C     EOSYS         IFEQ      'H'
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              65
     C     *IN65         IFEQ      *OFF
     C                   Z-ADD     EABAMT        XISMUS
     C                   ENDIF
     C                   ELSE                                                   ##PRVB = Y
     C     HBKEY         CHAIN     PHB01                              65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     HB1AAM        XISMUS            9 2
     C                   END                                                    *IN65
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C                   END                                                    SYS = H
     C*
     C     EOSYS         IFEQ      'N'
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              65
     C     *IN65         IFEQ      *OFF
     C                   Z-ADD     EANAMT        XISMUS
     C                   ENDIF
     C                   ELSE                                                   ##PRVB = Y
     C     TRKEY         CHAIN     PTR01                              65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     TRBAMT        XISMUS
     C                   END                                                    *IN65
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C                   END                                                    SYS = N
     C*
     C     EOTRDT        IFGE      XXFROM
     C     EOTRDT        ANDLE     XXTO
     C*
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C*
     C*  ADD TO DATE RANGE TOTALS
     C*
     C                   DO        4             X
     C     X             OCCUR     TOTALS
     C                   ADD       EOAMTC        TLAMTC
     C                   ADD       XXMRED        TLMRED
     C                   ADD       EOOTHR        TLOTHR
     C                   ADD       EONDIS        TLNDIS
     C                   ADD       XXTSAV        TLSAV
     C                   ADD       XISMUS        TLSMUS
     C                   END                                                    DO X
     C*
     C*   GET SOCIAL SECURITY NUMBER, CLAIMANT NAME, AND ACCIDENT DATE
     C*
     C     CMKEY         CHAIN     PCM01                              95
     C     *IN95         IFEQ      '0'
     C                   MOVEL     CMSS#         XXSS#             9
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              96
     C     *IN96         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXNAME           35
     C                   ELSE                                                   ELSEIF *IN96
     C                   MOVE      *BLANKS       XXNAME
     C                   END                                                    END IF *IN96
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       xxSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   ELSE                                                   ELSEIF *IN95
     C                   MOVE      *BLANKS       XXNAME
     C                   Z-ADD     *ZERO         XXACDT
     C                   END                                                    END IF *IN95
     C*
     C     *INOA         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRSV            6 0
     C*
     C   10              EXCEPT    DET
     C*
     C*
     C                   END                                                    IN DATE
     C*                                                    RANGE?
     C*
     C*  ADD TO YEAR TO DATE TOTALS
     C*
     C     5             DO        8             X
     C     X             OCCUR     TOTALS
     C                   ADD       EOAMTC        TLAMTC
     C                   ADD       XXMRED        TLMRED
     C                   ADD       EOOTHR        TLOTHR
     C                   ADD       EONDIS        TLNDIS
     C                   ADD       XXTSAV        TLSAV
     C                   ADD       XISMUS        TLSMUS
     C                   END                                                    DO X
     C*
     C                   READ      PEO01                                  90
     C*
     C*********  EOTRDT    IFGE XXFROM
     C*********  EOTRDT    ANDLEXXTO
     C*
     C*** CHECK FOR LEVEL BREAKS
     C                   Z-ADD     *ZEROS        XISMUS
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVE      *BLANKS       XXNAME
     C                   Z-ADD     *ZERO         XXACDT
     C                   MOVE      EOCO#         XXCO#
     C                   MOVE      EOSUB#        XXSUB#
     C                   MOVE      EOFND         XXFND
     C                   MOVE      EOFYR         XXFYR
     C                   MOVE      EOCASE        XXCASE
     C                   MOVE      EOEOB#        XXEOB#
     C*
     C     EOSUB#        IFNE      LSTSUB
     C                   MOVE      '1'           *IN15
     C                   EXSR      SRPRO
     C                   EXSR      SRFND
     C                   EXSR      SRSUB
     C                   ELSE
     C     EOFND         IFNE      LSTFND
     C                   MOVE      '1'           *IN16
     C                   EXSR      SRPRO
     C                   EXSR      SRFND
     C                   ELSE
     C     EOFEIN        IFNE      LSTFEN
     C     EOMOD         ORNE      LSTMOD
     C                   MOVE      '1'           *IN17
     C                   EXSR      SRPRO
     C                   END
     C                   END
     C                   END
     C*
     C                   Z-ADD     EOSUB#        LSTSUB            5 0
     C                   Z-ADD     EOFND         LSTFND            3 0
     C                   Z-ADD     EOFEIN        LSTFEN            9 0
     C                   Z-ADD     EOMOD         LSTMOD            3 0
     C                   MOVEL     EOPNAM        LSTPNM           40
     C*
     C*
     C                   Z-ADD     EOSUB#        XFSUB
     C                   Z-ADD     EOFND         XFFND
     C                   Z-ADD     EOFYR         XFYR                           FILE NUMBER
     C                   Z-ADD     EOCASE        XFCASE
     C                   Z-ADD     EOEOB#        XFEOB#
     C*
     C*********            END                             IN DATE
     C*                                                    RANGE?
     C*
     C                   END                                                     END *IN90
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   MOVE      '1'           *IN15
     C                   MOVE      '1'           *IN18
     C                   EXSR      SRPRO
     C                   EXSR      SRFND
     C                   EXSR      SRSUB
     C                   Z-ADD     4             X
     C     X             OCCUR     TOTALS
     C                   EXCEPT    FINTOT
     C                   Z-ADD     8             X
     C     X             OCCUR     TOTALS
     C                   EXCEPT    FINYTD
     C*
     C* END OF PROGRAM
     C     TERM          TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found and (qqrtyp = 'B' or qqrtyp = 'D');
          Except Route;
       Endif;
       xxprt = '1';
       Chain Orkey Por01;
       If %found and (qqrtyp = 'B' or qqrtyp = 'S');
          Except Route1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C**********************************************************
     C     SRCLR         BEGSR
     C**********************************************************
     C     X             OCCUR     TOTALS
     C                   Z-ADD     0             TLAMTC
     C                   Z-ADD     0             TLMRED
     C                   Z-ADD     0             TLOTHR
     C                   Z-ADD     0             TLNDIS
     C                   Z-ADD     0             TLSAV
     C                   Z-ADD     0             TLSMUS
     C                   ENDSR
     C**********************************************************
     C     SRSUB         BEGSR
     C**********************************************************
     C     *INOA         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C*** 01/10/89 *** SUB NAME SECTION MOVED FROM HERE **************
     C                   Z-ADD     3             X
     C     X             OCCUR     TOTALS
     C                   EXCEPT    SUBTOT
     C                   EXSR      SRCLR
     C                   Z-ADD     7             X
     C     X             OCCUR     TOTALS
     C                   EXCEPT    SUBYTD
     C*
     C*** BEGIN MOV *** 01/10/89 *************************************
     C*  CHAIN TO SUBSCRIBER FILE TO GET SUBSCRIBER NAME
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*** END MOV ***** 01/10/89 *************************************
     C*
     C                   MOVE      '0'           *IN15
     C   10              EXCEPT    HEAD
     C   10              EXCEPT    PROHD
     C                   EXSR      SRCLR
     C                   ENDSR
     C*
     C**********************************************************
     C     SRFND         BEGSR
     C**********************************************************
     C*
     C     *INOA         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C*** 01/10/89 *** FND NAME SECTION MOVED FROM HERE **************
     C                   Z-ADD     2             X
     C     X             OCCUR     TOTALS
     C                   EXCEPT    GRPTOT
     C                   EXSR      SRCLR
     C                   Z-ADD     6             X
     C     X             OCCUR     TOTALS
     C                   EXCEPT    GRPYTD
     C*
     C*** BEGIN MOV *** 01/10/89 *************************************
     C*  CHAIN TO FUND FILE TO GET FUND NAME
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*** END MOV ***** 01/10/89 *************************************
     C*
     C                   MOVE      '0'           *IN16
     C   10              EXCEPT    HEAD
     C   10              EXCEPT    PROHD
     C                   EXSR      SRCLR
     C                   ENDSR
     C*
     C**********************************************************
     C     SRPRO         BEGSR
     C**********************************************************
     C                   Z-ADD     1             X
     C     X             OCCUR     TOTALS
     C   10              EXCEPT    PROTOT
     C   11              EXCEPT    DET1
     C                   EXSR      SRCLR
     C                   Z-ADD     5             X
     C     X             OCCUR     TOTALS
     C                   EXCEPT    PROYTD
     C                   MOVE      '0'           *IN17
     C*
     C     *INOA         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXSR      SRCLR
     C   10              EXCEPT    PROHD
     C                   ENDSR
     C**********************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
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
     O**********        09   Q1USER              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   Q1USER              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
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
      ***** End Del ***** 08/26/14 *********************************************
     O***** REPORT SELECTIONS ***** BEGIN
     O          E            ROUTE          2 33
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           26 'SUBSCRIBER :'
     O                       RSSUB#              35
     O          E            ROUTE          2
     O                                           26 'GROUP      :'
     O                       RSFND               33
     O          E            ROUTE          2
     O                                           26 'FROM DATE  :'
     O                       RSFRDT              38 '  /  /  '
     O          E            ROUTE          2
     O                                           26 'TO DATE    :'
     O                       RSTODT              38 '  /  /  '
     O          E            ROUTE          2
     O                                           26 'REPORT TYPE:'
     O                       RSRTYP              31
     O***** REPORT SELECTIONS ***** END
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 1997'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*****************************************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             119
     O                       DSDATE        Y    198
     O**
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL             129
     O                                          194 'PAGE:'
     O                       PAGE          Z    198
     O**
     O          E            HEAD           1
     O                                           99 'PPO PARTICIPATING'
     O                                          116 'PROVIDER NETWORK'
     O**
     O          E            HEAD           1
     O                                          100 'PERIODIC REPORT'
     O                                          111 'OF SAVINGS'
     O*
     O          E            HEAD           1
     O                                           97 'CLAIMS PROCESSED FROM'
     O                       XXFRDT        Y    106
     O                                          110 'TO:'
     O                       XXTODT        Y    119
     O*
     O          E            HEAD           2
     O                       XXSBNM             119
     O*
     O          E   N15N18   HEAD           2
     O                                            5 'GROUP'
     O                       XXFND               10
     O                       XXFNNM              53
     O*
     O          E            HEAD           1
     O              N15N18                        7 'DATE OF'
     O              N15N18                       25 'CLAIMANT'
     O                                           93 'SUBMITTED'
     O                                          107 'MRA'
     O                                          134 'ADDITIONAL'
     O                                          147 'NETWORK'
     O                                          166 'TOTAL'
     O                                          193 'REVIEWER'
     O*
     O          E            HEAD           2
     O              N15N18                        7 'SERVICE'
     O              N15N18                       23 'SS#'
     O              N15N18                       50 'CLAIMANT NAME'
     O                                           91 'CHARGES'
     O                                          114 'REDUCTIONS'
     O                                          134 'REVIEWER REDS'
     O                                          153 'DISCOUNT REDS'
     O                                          168 'SAVINGS'
     O                                          193 'FEE'
     O          E   N15N16N90PROHD          2
     O                                            9 'PROVIDER:'
     O                       EOFEIN              19
     O                                           20 '-'
     O                       EOMOD               23
     O                       EOPNAM              64
     O*
     O          E            DET            1
     O                       XXFRSV               8 '  /  /  '
     O                       XXPRE               20
     O                                           21 '-'
     O                       XXMID               23
     O                                           24 '-'
     O                       XXSUF               28
     O                       XXNAME              77
     O                       EOAMTC        J     96
     O                       XXMRED        J    116
     O                       EOOTHR        J    136
     O                       EONDIS        J    152
     O                       XXTSAV        J    173
     O                       XISMUS        J    194
     O          E            PROTOT      1  1
     O                                           58 'PROVIDER:'
     O                       LSTFEN              68
     O                                           69 '-'
     O                       LSTMOD              72
     O                                           80 'TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E    10      PROYTD         2
     O                                           80 'PROVIDER YTD TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E    10      GRPTOT         1
     O                                           68 'GROUP'
     O                       LSTFND              72
     O                                           80 'TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E    10      GRPYTD         2
     O                                           80 'GROUP YTD TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E    10      SUBTOT         1
     O                                           66 'SUBSCRIBER'
     O                       LSTSUB              72
     O                                           80 'TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E    10      SUBYTD         2
     O                                           80 'SUBSCRIBER YTD TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E    10      FINTOT         1
     O                                           80 'FINAL TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E    10      FINYTD         2
     O                                           80 'FINAL YTD TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
     O**
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   Q1USER              67
     O**********E            ROUTE1         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE1         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   Q1USER              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       YYTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE1         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O***** REPORT SELECTIONS ***** BEGIN
     O          E            ROUTE1         2 33
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE1         2
     O                                           26 'SUBSCRIBER :'
     O                       RSSUB#              35
     O          E            ROUTE1         2
     O                                           26 'GROUP      :'
     O                       RSFND               33
     O          E            ROUTE1         2
     O                                           26 'FROM DATE  :'
     O                       RSFRDT              38 '  /  /  '
     O          E            ROUTE1         2
     O                                           26 'TO DATE    :'
     O                       RSTODT              38 '  /  /  '
     O          E            ROUTE1         2
     O                                           26 'REPORT TYPE:'
     O                       RSRTYP              31
     O***** REPORT SELECTIONS ***** END
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE1
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 1997'
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*****************************************************************
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             119
     O                       DSDATE        Y    198
     O**
     O          E            HEAD1          1
     O                       Q1PGM               10
     O                       YYTITL             129
     O                                          194 'PAGE:'
     O                       PAGE1         Z    198
     O**
     O          E            HEAD1          1
     O                                           99 'REVIEWER PARTICIPATING'
     O                                          116 'PROVIDER NETWORK'
     O**
     O          E            HEAD1          1
     O                                          100 'PERIODIC REPORT'
     O                                          111 'OF SAVINGS'
     O*
     O          E            HEAD1          3
     O                                           97 'CLAIMS PROCESSED FROM'
     O                       XXFRDT        Y    106
     O                                          110 'TO:'
     O                       XXTODT        Y    119
     O*
     O          E            HEAD1          1
     O                                           93 'SUBMITTED'
     O                                          107 'MRA'
     O                                          134 'ADDITIONAL'
     O                                          147 'NETWORK'
     O                                          166 'TOTAL'
     O                                          193 'REVIEWER'
     O*
     O          E            HEAD1          2
     O                                           91 'CHARGES'
     O                                          114 'REDUCTIONS'
     O                                          134 'REVIEWER REDS'
     O                                          153 'DISCOUNT REDS'
     O                                          168 'SAVINGS'
     O                                          193 'FEE'
     O*
     O          E            DET1           1
     O                                            9 'PROVIDER:'
     O                       LSTFEN              19
     O                                           20 '-'
     O                       LSTMOD              23
     O                       LSTPNM              65
     O                                           73 'TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E    11      PROYTD         2
     O                                           73 'PROVIDER YTD TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E    11      GRPTOT         1
     O                                           22 'GROUP'
     O                       LSTFND              26
     O                       XXFNNM              68
     O                                           76 'TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E    11      GRPYTD         2
     O                                           76 'GROUP YTD TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E    11      SUBTOT         1
     O                                           23 'SUBSCRIBER'
     O                       LSTSUB              29
     O                       XXSBNM              71
     O                                           79 'TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E    11      SUBYTD         2
     O                                           79 'SUBSCRIBER YTD TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E    11      FINTOT         1
     O                                           79 'FINAL TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E    11      FINYTD         2
     O                                           79 'FINAL YTD TOTALS:'
     O                       TLAMTC        J     96
     O                       TLMRED        J    116
     O                       TLOTHR        J    136
     O                       TLNDIS        J    152
     O                       TLSAV         J    173
     O                       TLSMUS        J    194
     O          E            ERRMS1      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
     O**
