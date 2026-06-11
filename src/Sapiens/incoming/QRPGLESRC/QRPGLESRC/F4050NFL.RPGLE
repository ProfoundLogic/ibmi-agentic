     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4050RFL                                          *
     H*    DESCRIPTION: ADJUDICATION (01/01/01 RULES)                 *
     H*   This program is called from F0070R (Claim Adjudication)     *
     H*   and will perform the actual adjudication of the detail      *
     H*   claim records written by F0070R.                            *
     H*                                                               *
     H*  This Sub-program is intended to be used to calculate the     *
     H*  amount that is due a provider of services, by the carrier    *
     H*  of the Workers' Compensation insurance.                      *
     H*                                                               *
     H*  It should be used for any service provided on or after       *
     H*  01/01/01.                                                    *
     H*****************************************************************
     H*  CREATED: 5/7/99    C:79   PROJECT: FL019                     *
     H*****************************************************************
     H*  Specialty Codes used in this program:                        *
     H*       NONE                                                    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     FFDCAL13   UF   E           K DISK
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0F)
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSVL1FL  IF   E           K DISK
     FFTABLFL   IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FNMHSL     IF   E           K DISK
      *
     FFDMGL1    UF A E           K DISK
     FFDMGL5    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDEL4    UF A E           K DISK
     FFAEOL4    IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFDDUL1    IF   E           K DISK
     F                                     RENAME(PDU01:PDU02)
     F*

     D*
     D QQS             S              5    DIM(999)                             DETAIL SEQ #
     D QQC             S              2    DIM(999)                             ERROR MESSAGES
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3  0
     D  XXHCPC                 4      8
     D  XXUNIT                 9     11  0
     D*
     D*  DATE/TIME DATA STRUCTURE.
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  FSY                    1     10
     D                                     DIM(10)                              GROUP SYS CODE
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

     I*
     I*  RENAME THE DUPLICATE FIELD NAMES.
     I*
     IPDU02
     I              DUCO#                       D1CO#
     I              DUSUB#                      D1SUB#
     I              DUFND                       D1FND
     I              DUFYR                       D1FYR
     I              DUCASE                      D1CASE
     I              DUSS#                       D1SS#
     I              DUACDT                      D1ACDT
     I              DUEOB#                      D1EOB#
     I              DUDSEQ                      D1DSEQ
     I              DURVCD                      D1RVCD
     I              DUDESC                      D1DESC
     I              DURATE                      D1RATE
     I              DUUNTS                      D1UNTS
     I              DUNOCH                      D1NOCH
     I              DUSTAT                      D1STAT
     I              DUUSER                      D1USER
     I              DUPGM                       D1PGM
     I              DUTIME                      D1TIME
     I              DUDT                        D1DT
     I              DUIUSR                      D1IUSR
     I              DUIPGM                      D1IPGM
     I              DUITIM                      D1ITIM
     I              DUIDT                       D1IDT
     I              DUPOS                       D1POS
     IPCA0F
     I              CACO#                       XFCO#
     I              CAFND                       XFFND
     I              CAFYR                       XFFYR
     I              CACASE                      XFCASE
     I              CACSCD                      XFCSCD
     I              CAFDAT                      XFFDAT
     I              CATDAT                      XFTDAT
     I              CARDAT                      XFRDAT
     I              CAIPOP                      XFIPOP
     I              CAFEIN                      XFFEIN
     I              CAMOD                       XFMOD
     I              CAPRTY                      XFPRTY
     I              CATRCD                      XFTRCD
     I              CASVCD                      XFSVCD
     I              CAPMOD                      XFPMOD
     I              CACPT4                      XFCPT4
     I              CAICD9                      XFICD9
     I              CATOTA                      XFTOTA
     I              CAEOMB                      XFEOMB
     I              CADSEQ                      XFDSEQ
     I              CADELF                      XFDELF
     I              CAFLAG                      XFFLAG
     I              CACHK#                      XFCHK#
     I              CAEOB#                      XFEOB#
     I              CAPART                      XFPART
     I              CAP1                        XFP1
     I              CASS#                       XFSS#
     I              CAACDT                      XFACDT
     I              CASTAT                      XFSTAT
     I              CAUSER                      XFUSER
     I              CAPGM                       XFPGM
     I              CATIME                      XFTIME
     I              CADT                        XFDT
     I              CAIUSR                      XFIUSR
     I              CAIDT                       XFIDT
     I              CADXR                       XFDXR
     I              CADCS#                      XFDCS#
     I              CALTR#                      XFLTR#
     I              CARCFL                      XFRCFL
     I              CASUB#                      XFSUB#
     I              CAOTHR                      XFOTHR
     I              CANDIS                      XFNDIS
     I              CARDTP                      XFRDTP
     I              CABRSV                      XFBRSV
     I              CABRPM                      XFBRPM
     I              CABRPC                      XFBRPC
     I              CABRAM                      XFBRAM
     I              CABRUN                      XFBRUN
     I              CABRTC                      XFBRTC
     I              CAZIPA                      XFZIPA
     I              CAAST                       XFAST
     I              CADIFF                      XFDIFF
     I              CAIPGM                      XFIPGM
     I              CACHRG                      XFTOTC
     I              CAMRA                       XFTOTD
     I              CAOTH2                      XFOTH2
     I              CAPAID                      XFTOTP
     I              CARCN                       XFRCN
     I              CALIN#                      XFLIN#
     I              CARULE                      XFRULE
     I              CAOT1C                      XFOT1C
     I              CAOT2C                      XFOT2C
     I              CAMRAA                      XFMRAA
     I              CAITIM                      XFITIM
     I              CAMNAA                      XFMNAA

     C*
     C*  CLAIM DETAIL FILE (FDCAL13)
     C     CAKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C*  CLAIM DETAIL FILE (FDCALF)
     C     CAKYF         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    KKDSEQ
     C*
     C*  EOB MASTER FILE (FAEOL4)
     C     EOKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C*  EOMB CODE TABLE (FTEBPFL)
     C     EBKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    EBKEY
     C*
     C*  EOMB CODE FILE MASTER (FDDEL)
     C     DEKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    QQDSEQ
     C                   KFLD                    QQESRC
     C*
     C*  EOMB CODE FILE MASTER (FDDEL4)
     C     DEKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    QQDSEQ
     C*
     C*  HOSPITAL DETAIL BILL FILE (FDDUL)
     C     DUKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXDSEQ
     C*
     C*  HOSPITAL DETAIL BILL FILE (FDDUL1)
     C     DUKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    DURVCD
     C                   KFLD                    DURATE
     C                   KFLD                    DUUNTS
     C*
     C* Adjudication Message File (PMG01)
     C*
     C     MGKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXDSEQ
     C*
     C* Adjudication Message File (PMG02)
     C*
     C     MGKY2         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXDSEQ
     C                   KFLD                    XXMSG#
     C                   KFLD                    XXRCN
      *
     C*
     C*  CLAIM MASTER FILE
     C     CMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
     C*  Provider Code File
     C     PDKY          KLIST
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C*
     C*  SERV CODE FILE
     C     SVKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXCMSV
     C                   KFLD                    CAFDAT
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C     TCSKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY
     C*
     C*  USER PARAMETER FILE
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  DUMMY KEY USED FOR RELEASE OF ALL CA RECORDS
     C*
     C     DUMMY         KLIST
     C                   KFLD                    QQCO#
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C*
     C*  HOSPITAL PROVIDER FILE
     C*
     C     HSKEY         KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9 0
     C                   PARM                    QQACDT            8 0
     C                   PARM                    QQSPCD            3
     C                   PARM                    QQEOB#            9 0
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
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
     C***************************************************************
     C* INITIALIZATION SECTION
     C                   Z-ADD     *ZEROS        S                 3 0
     C                   Z-ADD     *ZEROS        Q                 3 0
     C                   Z-ADD     *ZEROS        XXALOW            9 2
     C*** BEGIN CHG *** 09/25/01 **************************************
     C**********           Z-ADD*ZEROS    XXOVRP  32
     C                   Z-ADD     *ZEROS        XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C                   MOVE      *ZEROS        QQS
     C                   MOVE      *ZEROS        QQC
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C**
     C*
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ                         5.0
     C     *LIKE         DEFINE    CAFEIN        HLFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         HLMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        HLPRTY                         1
     C     *LIKE         DEFINE    CAEOB#        HLEOB#                         9.0
     C     *LIKE         DEFINE    CACHK#        HLCHK#                         7.0
     C     *LIKE         DEFINE    CACHK#        PTCHK#                         7.0
     C     *LIKE         DEFINE    CADSEQ        PTDSEQ                         5.0
     C     *LIKE         DEFINE    CAEOB#        PTEOB#                         9.0
     C     *LIKE         DEFINE    CAFEIN        PTFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         PTMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        PTPRTY                         1
     C     *LIKE         DEFINE    CACHK#        SVCHK#                         7.0
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ                         5.0
     C     *LIKE         DEFINE    CAEOB#        SVEOB#                         9.0
     C     *LIKE         DEFINE    CAFEIN        SVFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SVMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        SVPRTY                         1
     C     *LIKE         DEFINE    PDSPCD        SVSPCD
     C     *LIKE         DEFINE    DUUNTS        SVUNTS                         5.2
     C     *LIKE         DEFINE    CSKEY         TABKEY                         10
     C     *LIKE         DEFINE    DURATE        XPCPT4                         5
     C     *LIKE         DEFINE    CAEOMB        XPEOMB                         3
     C     *LIKE         DEFINE    CAPMOD        XPPMOD                         2
     C     *LIKE         DEFINE    CATOTA        XPTOTA                         9.2
     C     *LIKE         DEFINE    CAMRA         XPTOTD                         9.2
     C     *LIKE         DEFINE    CAPAID        XPTOTP                         9.2
     C     *LIKE         DEFINE    CABRTC        XPBRTC                         ADD 09/19/94
     C     *LIKE         DEFINE    DURATE        XXCPT4                         5
     C     *LIKE         DEFINE    DURVCD        XXSVCD                         4.0
     C     *LIKE         DEFINE    DUUNTS        XXUNTS                         5.2
     C     *LIKE         DEFINE    CAFDAT        XXYMD                          6.0
     C*
     C     *LIKE         DEFINE    CAFEIN        SXFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SXMOD                          3.0
     C*
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MGMSEQ        XXMSEQ
     C     *LIKE         DEFINE    MGDSEQ        XXDSEQ
     C     *LIKE         DEFINE    DEESRC        QQESRC
     C     *LIKE         DEFINE    DEDSEQ        QQDSEQ
     C     *LIKE         DEFINE    DEDSEQ        KKDSEQ
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    DERCN         XXRCN
     C     *LIKE         DEFINE    EOSTYP        XXOUT
     C***************************************************************
     C*
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
     C*
     C*
     C** GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C** IF THE GET WAS UNSUCCESSFUL END THE PROGRAM.
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99999'       QQS(1)
     C                   MOVE      '95'          QQC(1)                         INVALID USER PARM
     C                   GOTO      TERM
     C                   END
     C*
     C** GET THE CLAIM MASTER RECORD.
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C** IF THE GET WAS UNSUCCESSFUL END THE PROGRAM.
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99998'       QQS(1)
     C                   MOVE      '96'          QQC(1)                         INVALID CASE #
     C                   GOTO      TERM
     C                   END
     C*
     C* Find the recon number.  If this is a recon.
     C     EOKY          CHAIN     PEO01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EORCN         XXRCN
     C*
     C                   ELSE
     C                   Z-ADD     *ZERO         XXRCN
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     CAKY1         SETLL     PCA01
     C     CAKY1         READE     PCA01                                  54
      *
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
      *
     C     CAFDAT        IFGE      0010101
     C     XXRCN         ANDEQ     CARCN
     C*
     C                   Z-ADD     CADSEQ        XXDSEQ
     C*
     C     DUKY          CHAIN     PDU01                              60
     C*
     C                   Z-ADD     CAFDAT        XXYMD
     C                   MOVE      *BLANKS       XPEOMB
      *
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     DUUNTS        SVUNTS
     C                   MOVEL     DURATE        XPCPT4
     C                   MOVE      CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C* --- Get and save the Provider's Specialty Code ----------------
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD                         ADD 12/28/92
     C                   END
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C*
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '79'          QQC(Q)
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
     C*
     C                   GOTO      NXTRCD
     C*
     C                   END                                                    END EQ *ZEROS
     C*
     C** GET THE CLAIM STATUS RECORD. SEE IF OVERRIDE FLAG IS 'Y'.
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C*
     C** A HARD ERROR WILL CAUSE THE PROGRAM TO TERMINATE ASAP.
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '97'          QQC(Q)                         INVALID CLM STS CODE
     C*** Err Msg 97: "Invalid Claim Status Code, See Programmer"
     C*
     C                   GOTO      NXTRCD
     C*
     C                   END
     C*
     C* * * IF OVERRIDE FLAG IS NOT EQUAL TO 'Y' THEN PROCESS THIS RECORD
     C* * * ELSE DO NOT ADJUDICATE.
     C*
     C     CSOVRD        IFNE      'Y'
     C*
     C* ===== NON-OVERRIDE TYPE ======================================
     C*** CHECK FOR LEVEL BREAK (PROVIDER)
     C     SXFEIN        IFNE      CAFEIN
     C     SXMOD         ORNE      CAMOD
     C                   EXSR      NEWPRO
     C                   END
     C*
     C* ===== NON-HOSPITAL TYPE ======================================
     C     CAPRTY        IFNE      '4'
     C                   GOTO      NXTRCD
     C                   END
     C*
     C* ===== HOSPITAL TYPE ==========================================
     C                   EXSR      HOSP
     C*
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM ON HOSPITAL.
     C*
     C     DUKY1         CHAIN     PDU02                              54
      *
     C     *IN54         DOWEQ     '0'
      *
     C     DUDSEQ        IFNE      D1DSEQ
      *
     C                   Z-ADD     D1DSEQ        KKDSEQ
      *
     C     CAKYF         CHAIN     PCA0F                              55
      *
     C     *IN55         IFEQ      '0'
     C     CAFDAT        ANDEQ     XFFDAT
     C     CAFEIN        ANDEQ     XFFEIN
     C     XFFLAG        ANDNE     'P'
      *
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C                   MOVE      *ON           *IN54
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C                   END
     C*
     C                   END
     C*
     C  N54DUKY1         READE     PDU02                                  54                3
     C*
     C                   ENDDO                                                  *IN54 DOWEQ
     C*
     C* ===== OVERRIDE TYPE ==========================================
     C*** IF OVERRIDE TYPE WE MUST STILL OUTPUT THE ALLOWED/DISALLOWED ETC.
     C     CAMRA         IFEQ      0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CAPAID        ANDEQ     0
     C*
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        CATOTA
     C                   END
     C*
     C                   Z-ADD     *ZEROS        CABRTC                         ADD 09/19/94
     C     CACHRG        SUB       CATOTA        CAMRA
     C                   Z-ADD     CATOTA        CAPAID
     C                   END
     C                   Z-ADD     XCMRA         CAMRAA
     C*
     C                   UPDATE    PCA01
     C*
     C                   END
     C*
     C     NXTRCD        TAG
     C*
     C*** BEFORE NEXT RECORD IS READ, SAVE THE KEY FIELDS NEEDED TO
     C*** RETRIEVE THE PHYSICAL THERAPY RECORDS TO BE UPDATED IN THE
     C*** PHYS2 ROUTINE WHICH IS EXECUTED ON A PROVIDER LEVEL BREAK...
     C**
     C                   Z-ADD     CADSEQ        PTDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        PTFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         PTMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        PTPRTY
     C                   Z-ADD     CAEOB#        PTEOB#
     C                   Z-ADD     CACHK#        PTCHK#
     C*
     C                   ENDIF                                                  CAFDAT GT 940101
     C*
     C*+*+*+*+*+*+*+*+*+*+ Read Next Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     CAKY1         READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C*
     C     *IN54         IFEQ      '0'                                          SAVE FIELDS OF
     C                   Z-ADD     CADSEQ        SVDSEQ                         CURRENT REC.
     C                   MOVE      CAFEIN        SVFEIN
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     DUUNTS        SVUNTS
     C                   MOVEL     DURATE        XPCPT4
     C                   MOVE      CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C*----------------------------------------------------------------
     C* --- Get and save the Provider's Specialty Code ----------------
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD                         ADD 12/28/92
     C*
     C                   END
     C*
     C                   END
     C*
     C                   ENDDO
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C* Write the adjudication messages to FDMGP
     C                   EXSR      MSG
     C*
     C                   Z-ADD     0             XCMRA             9 2
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *********************************************************************
     *** UPDATE FADEP WITH EOMB                                E O M B   *
     *********************************************************************
     C     EOMB          BEGSR
     C*
     C* Find the EOMB description
     C                   MOVEL(P)  XPEOMB        EBKEY
     C*
     C     EBKY          CHAIN     PEB01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     EBDESC        XXNOT1
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       XXNOT1
     C*
     C                   ENDIF                                                  *IN60=ON
     C*
     C                   MOVE      'A'           QQESRC
     C                   MOVE      CADSEQ        QQDSEQ
     C*
     C* Determine if an EOMB already exists for this DSEQ number
     C     DEKY          CHAIN     PDE01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      XPEOMB        DEEOB
     C                   MOVEL(P)  XXNOT1        DENOT1
     C*
     C* Update file maintenance fields
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVEL     SRDTO         DEDT
     C                   Z-ADD     DSTIME        DETIME
     C*
     C                   UPDATE    PDE01
     C*
     C                   ELSE
     C*
     C* No EOMB exists so find the next sequence number and add to file
     C     DEKY1         SETGT     PDE02
     C     DEKY1         READPE    PDE02                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C                   ADD       1             DESEQ#
     C*
     C                   ELSE
     C                   Z-ADD     1             DESEQ#
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   MOVE      CACO#         DECO#
     C                   MOVE      CASUB#        DESUB#
     C                   MOVE      CAFND         DEFND
     C                   MOVE      CAFYR         DEFYR
     C                   MOVE      CACASE        DECASE
     C                   MOVE      CASS#         DESS#
     C                   MOVE      CAACDT        DEACDT
     C                   MOVE      CAEOB#        DEEOB#
     C                   MOVE      CADSEQ        DEDSEQ
     C                   MOVE      XPEOMB        DEEOB
     C                   MOVE      XXNOT1        DENOT1
     C                   MOVE      *BLANKS       DENOT2
     C                   MOVE      'A'           DESTAT
     C                   MOVE      'A'           DEESRC
     C                   Z-ADD     XXRCN         DERCN
     C*
     C* Update file maintenance fields
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C                   MOVE      Q1USER        DEIUSR
     C                   MOVE      Q1PGM         DEIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     SRDTO         DEIDT
     C                   Z-ADD     DSTIME        DETIME
     C                   Z-ADD     DSTIME        DEITIM
     C*
     C                   WRITE     PDE01
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     EOMBE         ENDSR
     *********************************************************************
     *** ADJUDICATION MESSAGES SUBROUTINE                    M S G       *
     *********************************************************************
     C     MSG           BEGSR
      *
      * Load the template
      *
     C                   MOVEL     'XX00000'     XXMSG#
      *
      * Is this message already in the file for this line item
      *
     C                   Z-ADD     1             N                 3 0
      *
     C     N             DOWLE     999
      *
     C     QQC(N)        IFEQ      '00'
      *
     C                   LEAVE
      *
     C                   ENDIF
      *
     C                   MOVE(P)   QQS(N)        XXDSEQ
     C                   MOVE      QQC(N)        XXMSG#
      *
     C     MGKY2         CHAIN     PMG02                              60
      *
     C     *IN60         IFEQ      *ON
      *
      * Find the next available message sequence number
      *
     C     MGKY1         SETGT     PMG01
     C     MGKY1         READPE(N) PMG01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     MGMSEQ        ADD       1             XXMSEQ
      *
     C                   ELSE
      *
     C                   Z-ADD     1             XXMSEQ
      *
     C                   ENDIF                                                  *IN60 = OFF
      *
      * Write the new adjudication message to FDMGP
      *
     C                   Z-ADD     QQCO#         MGCO#
     C                   Z-ADD     QQSUB#        MGSUB#
     C                   MOVE      QQFND         MGFND
     C                   MOVE      QQFYR         MGFYR
     C                   MOVE      QQCASE        MGCASE
     C                   MOVE      *ZEROS        MGDCS#
     C                   Z-ADD     QQEOB#        MGEOB#
     C                   Z-ADD     XXDSEQ        MGDSEQ
     C                   Z-ADD     XXMSEQ        MGMSEQ
     C                   MOVE      'S'           MGSRC
     C                   MOVE      *BLANKS       MGCSI
     C                   MOVE      XXMSG#        MGMSG#
     C                   MOVE      'A'           MGSTAT
     C                   Z-ADD     XXRCN         MGRCN
      *
     C                   MOVE      Q1USER        MGUSER
     C                   MOVE      Q1PGM         MGPGM
     C                   MOVE      Q1PGM         MGIPGM
     C                   MOVE      Q1USER        MGIUSR
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSTIME        MGTIME
     C                   Z-ADD     MGTIME        MGITIM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C                   MOVEL     SRDTO         MGIDT
      *
     C                   WRITE     PMG01
      *
     C                   ENDIF                                                  *IN60 = OFF
      *
     C                   ADD       1             N
      *
     C                   ENDDO                                                  N LE 999
      *
     C     MSGE          ENDSR
     *********************************************************************
     *** NEW PROVIDER SUBROUTINE (LEVEL BREAK)               N E W P R O *
     *********************************************************************
     C     NEWPRO        BEGSR
     C*
     C                   Z-ADD     CADSEQ        HLDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        HLFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         HLMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        HLPRTY
     C                   Z-ADD     CAEOB#        HLEOB#
     C                   Z-ADD     CACHK#        HLCHK#
     C*
     C**** NOT NEEDED *************
     C                   Z-ADD     HLDSEQ        CADSEQ                         SAVE FIELD VALS
     C                   MOVE      HLFEIN        CAFEIN                         FOR RECORD WE
     C                   MOVE      HLMOD         CAMOD                          ARE PROCESSING
     C                   MOVE      HLPRTY        CAPRTY
     C                   Z-ADD     HLEOB#        CAEOB#
     C                   Z-ADD     HLCHK#        CACHK#
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ
     C                   MOVE      CAFEIN        SVFEIN                            CURR REC.
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C*----------------------------------------------------------------
     C* Get and save the Provider's Specialty Code
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD                         ADD 12/28/92
     C                   END
     C*
     C                   MOVE      CAFEIN        SXFEIN
     C                   MOVE      CAMOD         SXMOD
     C*
     C     NEWPRE        ENDSR
     *********************************************************************
     *** HOSPITAL SUBROUTINE   SERVICE CODES                     H O S P *
     *********************************************************************
     C     HOSP          BEGSR
     C*
     C* PAY ALL CHARGES AT 75%
     C*
     C*
     C                   Z-ADD     DURVCD        XXRVCD
     C                   MOVE      DURATE        XXHCPC
     C                   Z-ADD     *ZEROS        XXUNIT                         ADD 05/31/92
     C*
     C     SVKY          SETGT     PSV01                                        GET SERV CODE
     C     NXTSV1        TAG
     C                   READP     PSV01                                  60     RECORD
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     CACO#
     C     SVCMSV        ANDEQ     XXCMSV
     C     SVSTAT        CABEQ     'I'           NXTSV1
     C                   Z-ADD     SVALOW        XCMRA
     C*
     C* CHECK SERVICE CODE FOR ELEVATED CARE
     C*
     C     SVTSRV        IFEQ      'EC'
     C*
     C     FNKEY         CHAIN     PFN01                              80
     C     *IN80         IFEQ      '0'
     C     CAFDAT        IFGE      FNEFDT
     C                   Z-ADD     1             X                 5 0
     C     'N'           LOOKUP    FSY(X)                                 80
     C     *IN80         IFEQ      '1'
     C*
     C     HSKEY         CHAIN     PHS01                              80
     C     *IN80         IFEQ      '0'
     C     CAFDAT        IFGE      HSCEDT
     C     CAFDAT        ANDLE     HSTMDT
     C     HSECFL        ANDEQ     'Y'
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '73'          QQC(Q)                         EC ERR MSG
     C*** Err Msg 72: "THIS CHARGE IS FOR ELEVATED CARE, PLEASE REVIEW"
     C                   END                                                    END GE/LE/EQ
     C                   END                                                    END *IN80 HS
     C                   END                                                    END *IN80 NET
     C                   END                                                    END GE FNEFDT
     C                   END                                                    END *IN80 FN
     C                   END                                                    END EQ EC
     C*
     C* CHECK SERVICE CODE FOR MEDICAL DEVICES
     C*
     C     SVTSRV        IFEQ      'MD'
     C*
     C     FNKEY         CHAIN     PFN01                              80
     C     *IN80         IFEQ      '0'
     C     CAFDAT        IFGE      FNEFDT
     C                   Z-ADD     1             X                 5 0
     C     'N'           LOOKUP    FSY(X)                                 80
     C     *IN80         IFEQ      '1'
     C*
     C     HSKEY         CHAIN     PHS01                              80
     C     *IN80         IFEQ      '0'
     C     CAFDAT        IFGE      HSCEDT
     C     CAFDAT        ANDLE     HSTMDT
     C     HSMDFL        ANDEQ     'Y'
     C*
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '72'          QQC(Q)                         MD ERR MSG
     C*** Err Msg 72: "THIS CHARGE IS FOR A MEDICAL DEVICE, PLEASE REVIEW"
     C                   END                                                    END GE/LE/EQ
     C                   END                                                    END *IN80 HS
     C                   END                                                    END *IN80 NET
     C                   END                                                    END GE FNEFDT
     C                   END                                                    END *IN80 FN
     C                   END                                                    END EQ MD
     C*** END ADD ***** 10/10/89 *************************************
     C*
     C                   END                                                    END *IN60
     C*
     C*       PAY ALL CHARGES AT 75%
     C*
     C     CACHRG        MULT      .75           XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC                         ADD 09/19/94
     C*
     C                   EXSR      UPDAT
     C*
     C     HOSPE         ENDSR
     *********************************************************************
     *** UPDATE SUBROUTINE                                     U P D A T *
     *********************************************************************
     C     UPDAT         BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CHARGES EXCEED MAXIMUM ALLOWANCE FOUND IN REIMBURSEMENT MANUAL
     C*
     C     XPTOTD        IFGT      *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C     XPTOTA        ANDGT     *ZEROS
     C                   MOVE      '054'         XPEOMB
     C*** EOMB 054: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   END
     C*
     C* Call subroutine to write EOMB to FDDEP
     C     XPEOMB        IFNE      *BLANKS
      *
     C                   EXSR      EOMB
      *
     C                   ENDIF                                                  XPEOMB NE BLANK
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM ON HOSPITAL.
     C*
     C     DUKY1         CHAIN     PDU02                              54
      *
     C     *IN54         DOWEQ     '0'
      *
     C     DUDSEQ        IFNE      D1DSEQ
      *
     C                   Z-ADD     D1DSEQ        KKDSEQ
      *
     C     CAKYF         CHAIN     PCA0F                              55
      *
     C     *IN55         IFEQ      '0'
     C     CAFDAT        ANDEQ     XFFDAT
     C     CAFEIN        ANDEQ     XFFEIN
     C     XFFLAG        ANDNE     'P'
      *
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     999           Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C                   MOVE      *ON           *IN54
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C                   END
     C*
     C                   END
     C*
     C  N54DUKY1         READE     PDU02                                  54                3
     C*
     C                   ENDDO                                                  *IN54 DOWEQ
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** ONLY PROCESS THE RECORD IF NOT A OVERRIDE TYPE.
     C*
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTP        CAPAID
     C                   Z-ADD     XPBRTC        CABRTC                         ADD 09/19/94
     C                   MOVE      XPPMOD        CAPMOD
     C                   MOVE      XPEOMB        CAEOMB
     C*
     C                   MOVE      Q1USER        CAUSER
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME
     C*
     C*
     C                   Z-ADD     XCMRA         CAMRAA
     C*
     C                   UPDATE    PCA01
     C*
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CLEAR THE WORK SAVE FIELDS.
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPBRTC                         ADD 09/19/94
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE         ENDSR
