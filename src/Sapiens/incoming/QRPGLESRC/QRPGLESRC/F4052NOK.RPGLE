     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4052ROK                                          *
     H*    DESCRIPTION: ADJUDICATION (01/01/96 RULES)                 *
     H*   This program is called from F0070R (Claim Adjudication)     *
     H*   and will perform the actual adjudication of the detail      *
     H*   claim records written by F0070R.                            *
     H*                                                               *
     H*  This Sub-program is intended to be used to calculate the     *
     H*  amount that is due a provider of services, by the carrier    *
     H*  of the Workers' Compensation insurance.                      *
     H*                                                               *
     H*  It should be used for any service provided on or after       *
     H*  01/01/96 and before 01/01/98                                 *
     H*****************************************************************
     H*                                   Conversion
     H*  Subroutines/Codes                  Factor
     H*      HOSP     Hospital            $ 920.00   Unit Value Charge
     H*                                   $ 450.00   Per Day Charge
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*                                                               *
     H*  12/27/99 C:117 Project: MC010  Sup Seq#: 21094
     H*                 Taking DUUNTS out of DUKY1; Poss Duplicate
     H*                 problem.
     H*
     H*  06/08/99 C:79 PROJ: OK008
     H*                Modified to work with Provider Bills
     H*
     H*  04/08/99 C:117 Support: MC095 Seq# 17568
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs
     H*                 and changed all occurences of UPCO#, UPSUB#,
     H*                 UPFND to the above mentioned fields.
     H*  09/04/98 C:31  Project: 95MAL
     H*                 Ntwk Disc:Lesser of FS/Chr
     H*                 1. Add update of CAMRAA
     H*
     H*  07/28/98 C:79   PROJ:MC095 SEQ:12432                         *
     H*                  OPCODE FOR 40/16/98 MODIFICATION WAS "ANDGE" *
     H*                  AND IT SHOULD BE "ANDLT"                     *E
     H*                                                               *
     H*  04/16/98 C:79   PROJ:OK006
     H*                  1. CHANGE EFFECTIVE DATE RANGE TO INCLUDE
     H*                     ITEMS UP TO 01/01/98                       E
     H*                                                               *
     H*  01/20/96 C:32   REQ/PROJ: OK003
     H*                  1. CHG CALC FOR STOP-LOSS, USE TOT CHARGED
     H*                     INSTEAD OF PREVIOUSLY PAID                 E
     H*                  2. ADD MSG FOR BY-REPORT DRG
     H*                                                               *
     H*  11/22/96 C:32   REQ/PROJ: OK003
     H*                  1. CHG CALC FOR STOP-LOSS - DO NOT READ HIST
     H*                     FILE USE CURRENT CHARGE ONLY TO DETERMINE  E
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMSVL1OK  IF   E           K DISK
     FFTABLOK   IF   E           K DISK
     FFDCALH    UF   E           K DISK
     FFMFNL     IF   E           K DISK
     FNMHSL     IF   E           K DISK
     FFDHCL     IF   E           K DISK
     F*** BEGIN DEL *** 11/22/96 **************************************
     F*FDCALC  IF  E           K        DISK
     F*******      PCA01                             KRENAMEPCA0C
     F*** END   DEL *** 11/22/96 **************************************
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0O)
     FFDDUL     IF   E           K DISK
     FFDDUL1    IF   E           K DISK
     F                                     RENAME(PDU01:PDU02)
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDEL4    UF A E           K DISK
     FFAEOL4    IF   E           K DISK
     FFDMGL1    UF A E           K DISK
     FFDMGL5    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)
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
     I*** BEGIN DEL *** 11/22/96 **************************************
     I*  RENAME THE DUPLICATE FIELD NAMES.
     I*PCA0C
     I***********   CACPT4                          XCCPT4
     I***********   CACO#                           XCCO#
     I***********   CASUB#                          XCSUB#
     I***********   CAFND                           XCFND
     I***********   CAFYR                           XCFYR
     I***********   CACASE                          XCCASE
     I***********   CACSCD                          XCCSCD
     I***********   CAFDAT                          XCFDAT
     I***********   CATDAT                          XCTDAT
     I***********   CARDAT                          XCRDAT
     I***********   DUPOS                           XCIPOP
     I***********   CAFEIN                          XCFEIN
     I***********   CAMOD                           XCMOD
     I***********   CAPRTY                          XCPRTY
     I***********   CATRCD                          XCTRCD
     I***********   CASVCD                          XCSVCD
     I***********   CAICD9                          XCICD9
     I***********   CAANMN                          XCANMN
     I***********   CAUNTS                          XCUNTS
     I***********   CATOTA                          XCTOTA
     I***********   CACHRG                          XCTOTC
     I***********   CAMRA                           XCTOTD
     I***********   CAPAID                          XCTOTP
     I***********   CADSEQ                          XCDSEQ
     I***********   CADELF                          XCDELF
     I***********   CAFLAG                          XCFLAG
     I***********   CASTAT                          XCSTAT
     I***********   CAP1                            XCP1
     I***********   CAPMOD                          XCPMOD
     I***********   CAUSER                          XCUSER
     I***********   CAPGM                           XCPGM
     I***********   CADT                            XCDT
     I***********   CATIME                          XCTIME
     I***********   CACHK#                          XCCHK#
     I***********   CAEOB#                          XCEOB#
     I***********   CAPART                          XCPART
     I***********   CAEOMB                          XCEOMB
     I***********   CARCFL                          XCRCFL
     I***********   CADIFF                          XCDIFF
     I***********   CAOTHR                          XCOTHR
     I***********   CANDIS                          XCNDIS
     I***********   CARDTP                          XCRDTP
     I***********   CABRSV                          XCBRSV
     I***********   CABRPM                          XCBRPM
     I***********   CABRPC                          XCBRPC
     I***********   CABRAM                          XCBRAM
     I***********   CABRUN                          XCBRUN
     I***********   CABRTC                          XCBRTC
     I***********   CAZIPA                          XCZIPA
     I***********   CAAST                           XCAST
     I*** END   DEL *** 11/22/96 **************************************
     I*
     IPCA0O
     I              CACO#                       OXCO#
     I              CAFND                       OXFND
     I              CAFYR                       OXFYR
     I              CACASE                      OXCASE
     I              CACSCD                      OXCSCD
     I              CAFDAT                      OXFDAT
     I              CATDAT                      OXTDAT
     I              CARDAT                      OXRDAT
     I              CAIPOP                      OXIPOP
     I              CAFEIN                      OXFEIN
     I              CAMOD                       OXMOD
     I              CAPRTY                      OXPRTY
     I              CATRCD                      OXTRCD
     I              CASVCD                      OXSVCD
     I              CAPMOD                      OXPMOD
     I              CACPT4                      OXCPT4
     I              CAICD9                      OXICD9
     I              CATOTA                      OXTOTA
     I              CAEOMB                      OXEOMB
     I              CADSEQ                      OXDSEQ
     I              CADELF                      OXDELF
     I              CAFLAG                      OXFLAG
     I              CACHK#                      OXCHK#
     I              CAEOB#                      OXEOB#
     I              CAPART                      OXPART
     I              CAP1                        OXP1
     I              CASS#                       OXSS#
     I              CAACDT                      OXACDT
     I              CASTAT                      OXSTAT
     I              CAUSER                      OXUSER
     I              CAPGM                       OXPGM
     I              CATIME                      OXTIME
     I              CADT                        OXDT
     I              CAIUSR                      OXIUSR
     I              CAIDT                       OXIDT
     I              CADXR                       OXDXR
     I              CADCS#                      OXDCS#
     I              CALTR#                      OXLTR#
     I              CARCFL                      OXRCFL
     I              CASUB#                      OXSUB#
     I              CAOTHR                      OXOTHR
     I              CANDIS                      OXNDIS
     I              CARDTP                      OXRDTP
     I              CABRSV                      OXBRSV
     I              CABRPM                      OXBRPM
     I              CABRPC                      OXBRPC
     I              CABRAM                      OXBRAM
     I              CABRUN                      OXBRUN
     I              CABRTC                      OXBRTC
     I              CAZIPA                      OXZIPA
     I              CAAST                       OXAST
     I              CADIFF                      OXDIFF
     I              CAIPGM                      OXIPGM
     I              CACHRG                      OXTOTC
     I              CAMRA                       OXTOTD
     I              CAOTH2                      OXOTH2
     I              CAPAID                      OXTOTP
     I              CARCN                       OXRCN
     I              CALIN#                      OXLIN#
     I              CARULE                      OXRULE
     I              CAOT1C                      OXOT1C
     I              CAOT2C                      OXOT2C
     I              CAMRAA                      OXMRAA
     I              CAITIM                      OXITIM
     I              CAMNAA                      OXMNAA
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

     C*
     C*  CLAIM DETAIL FILE
     C*
     C     CAKY1A        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXYMD
     C                   KFLD                    SVDSEQ
     C*
     C     CAKY1Z        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXYMD
     C                   KFLD                    HLDSEQ
     C*
     C*  CLAIM MASTER FILE
     C     CMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
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
     C*
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
     C     HCKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CADSEQ
     C*
     C     CAKYH         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKYMD
     C*
     C     CAKYH1        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C     DUKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DUKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    DURVCD
     C                   KFLD                    DURATE
     C*** BEG DEL *** 12/27/99 ****************************************
     C***********          KFLD           DUUNTS
     C*** END DEL *** 12/27/99 ****************************************
     C*
     C     DEKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKESRC
     C*
     C*  EOMB CODE FILE MASTER (FDDEL4)
     C     DEKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     EOKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C     EBKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    EBKEY
     C* Adjudication Message File (PMG01)
     C*
     C     MGKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    KKDSEQ
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
     C                   KFLD                    KKDSEQ
     C                   KFLD                    XXMSG#
     C                   KFLD                    XXRCN
      *
     C     CAKYO         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    KKDSEQ
     C*
     C* DETAIL CHARGE AUXILIARY FILE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9 0
     C                   PARM                    QQACDT            8 0          (MMDDYY ORD)
     C                   PARM                    QQSPCD            3
     C                   PARM                    QQEOB#            9 0
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C*** END ADD *** 04/08/99 ****************************************
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
     C*
     C     *LIKE         DEFINE    CACHK#        PTCHK#                         7.0
     C     *LIKE         DEFINE    CADSEQ        PTDSEQ                         5.0
     C     *LIKE         DEFINE    CAEOB#        PTEOB#                         9.0
     C     *LIKE         DEFINE    CAFEIN        PTFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         PTMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        PTPRTY                         1
     C*
     C     *LIKE         DEFINE    CACHK#        SVCHK#                         7.0
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ                         5.0
     C     *LIKE         DEFINE    CAEOB#        SVEOB#                         9.0
     C     *LIKE         DEFINE    CAFEIN        SVFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SVMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        SVPRTY                         1
     C     *LIKE         DEFINE    DUUNTS        SVUNTS                         5.2
     C*
     C     *LIKE         DEFINE    CSKEY         TABKEY                         10
     C*
     C     *LIKE         DEFINE    DURATE        XPCPT4                         5
     C     *LIKE         DEFINE    CAEOMB        XPEOMB                         3
     C     *LIKE         DEFINE    CAPMOD        XPPMOD                         2
     C     *LIKE         DEFINE    CATOTA        XPTOTA                         9.2
     C     *LIKE         DEFINE    CAMRA         XPTOTD                         9.2
     C     *LIKE         DEFINE    CAPAID        XPTOTP                         9.2
     C*
     C     *LIKE         DEFINE    DURATE        XXCPT4                         5
     C     *LIKE         DEFINE    CAFDAT        XXDATE                         6.0
     C     *LIKE         DEFINE    DURVCD        XXSVCD                         4.0
     C     *LIKE         DEFINE    DUUNTS        XXUNTS                         5.2
     C     *LIKE         DEFINE    CAFDAT        XXYMD                          6.0
     C     *LIKE         DEFINE    CARCN         XXRCN
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MGMSEQ        XXMSEQ
     C     *LIKE         DEFINE    CAEOB#        XXEOB#
     C     *LIKE         DEFINE    CAACDT        XXACDT
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C*
     C     *LIKE         DEFINE    CAFEIN        SXFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SXMOD                          3.0
     C*
     C     *LIKE         DEFINE    CAFDAT        KKYMD
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    DEESRC        KKESRC
     C*
     C                   Z-ADD     920.00        XXHOSC            7 2
     C                   Z-ADD     450.00        XXHOSD            7 2
     C                   Z-ADD     *ZEROS        XTHOSC            9 2
     C                   Z-ADD     *ZEROS        XTHOSD            9 2
     C*
     C***************************************************************
     C*
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
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
     C                   Z-ADD     *ZEROS        KKYMD
     C*
     C     CAKYH         SETLL     PCA01
     C     CAKYH1        READE     PCA01                                  54
     C*
     C*+*+*+*+*+*+* Do Loop for Each Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
     C*
     C                   EXCEPT    ECA01
     C*
     C     CAFDAT        IFGE      960101
     C     CAFDAT        ANDLT     980101
     C     CARCN         ANDEQ     XXRCN
     C*
     C                   Z-ADD     CAFDAT        XXYMD
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C     DUKY          CHAIN     PDU01                              60
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     DUUNTS        SVUNTS
     C                   MOVEL     DURATE        XPCPT4
     C                   MOVE      XPPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C                   MOVE      *BLANKS       XPEOMB
     C*
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '79'          QQC(Q)
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
     C                   END                                                    END GT SYDATE
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
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '97'          QQC(Q)                         INVALID CLM STS CODE
     C*** Err Msg 97: "Invalid Claim Status Code, See Programmer"
     C                   END
     C*
     C* * * IF OVERRIDE FLAG IS NOT EQUAL TO 'Y' THEN PROCESS THIS RECORD
     C* * * ELSE DO NOT ADJUDICATE.
     C     CSOVRD        IFNE      'Y'
     C*
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
     C*
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
     C     CAKYO         CHAIN     PCA0O                              55
      *
     C     *IN55         IFEQ      '0'
     C     CAFDAT        ANDEQ     OXFDAT
     C     CAFEIN        ANDEQ     OXFEIN
     C     CADSEQ        ANDNE     OXDSEQ
     C     OXFLAG        ANDNE     'P'
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
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C*** IF OVERRIDE TYPE WE MUST STILL OUTPUT THE ALLOWED/DISALLOWED ETC.
     C     CAMRA         IFEQ      0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CAPAID        ANDEQ     0
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C*
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C                   END
     C*
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CAPAID
     C                   END
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XCMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C                   END
     C*
     C                   END
     C*
     C                   ENDIF                                                  CAFDAT GE
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
     C*+*+*+*+*+*+*+*+*+*+ Read Next Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     CAKYH1        READE     PCA01                                  54      GET NEXT RCD
     C*
     C                   ENDDO
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C** RELEASE ALL THE FORMATS FOR A SAVE RETURN.
     C*
     C     DUMMY         CHAIN     PCA01                              54
     C  N54              EXCEPT    ECA01
     C*
     C* Write adjudication messages to FDMGP
     C                   EXSR      MSG
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             XCMRA             9 2
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *********************************************************************
     *** UPDATE FDDEP WITH EOMB                                E O M B   *
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
     C                   MOVE      'A'           KKESRC
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
     C                   MOVE      QQCO#         DECO#
     C                   MOVE      QQSUB#        DESUB#
     C                   MOVE      QQFND         DEFND
     C                   MOVE      QQFYR         DEFYR
     C                   MOVE      QQCASE        DECASE
     C                   MOVE      QQSS#         DESS#
     C                   MOVE      XXACDT        DEACDT
     C                   MOVE      QQEOB#        DEEOB#
     C                   MOVE      KKDSEQ        DEDSEQ
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
     C* Restore the sequence number key field to the current record
     C                   Z-ADD     SVDSEQ        KKDSEQ
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
     C                   MOVE(P)   QQS(N)        KKDSEQ
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
     C                   Z-ADD     KKDSEQ        MGDSEQ
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
     C*
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
     C*** REGET RECORD.
     C     CAKY1Z        CHAIN     PCA01                              54
     C*
     C  N54              EXCEPT    ECA01
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ
     C                   MOVE      CAFEIN        SVFEIN                            CURR REC.
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
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
     C                   Z-ADD     DURVCD        XXRVCD
     C                   MOVE      DURATE        XXHCPC
     C                   Z-ADD     *ZEROS        XXUNIT
     C*
     C     SVKY          SETGT     PSV01                                        GET SERV CODE
     C     NXTSV1        TAG
     C                   READP     PSV01                                  60     RECORD
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     CACO#
     C     SVCMSV        ANDEQ     XXCMSV
     C     SVSTAT        CABEQ     'I'           NXTSV1
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SVALOW        XCMRA
     C*** END ADD *** 09/04/98 ****************************************
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
     C  N60              Z-ADD     30            Q
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
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '72'          QQC(Q)                         MD ERR MSG
     C*** Err Msg 72: "THIS CHARGE IS FOR A MEDICAL DEVICE, PLEASE REVIEW"
     C                   END                                                    END GE/LE/EQ
     C                   END                                                    END *IN80 HS
     C                   END                                                    END *IN80 NET
     C                   END                                                    END GE FNEFDT
     C                   END                                                    END *IN80 FN
     C                   END                                                    END EQ MD
     C*
     C*
     C*-- GET INVOICE AMOUNT FOR IMPLANTABLES -----------------------------
     C********** DURATE    IFEQ '00002'
     C     DURATE        IFEQ      'IMPLT'
     C*
     C* CHAIN TO THE DETAIL CHARGE AUXILIARY FILE TO GET INVOICE AMT
     C*
     C     HCKY          CHAIN     PHC01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     HC92N2        MULT      1.10          XXINVC            9 2
     C*
     C     CACHRG        IFLE      XXINVC
     C*
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   ELSE
     C*
     C                   Z-ADD     XXINVC        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   END                                                    END CACHRG
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   ELSE
     C*
     C*  USE THIS UNTIL BILL ENTRY WILL ASK FOR INVOICE AMT
     C*  PAY CHARGED AMOUNT AT 1.10%
     C     CACHRG        MULT      1.10          XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   END                                                    END *IN60 EQ '0'
     C                   END                                                    END DURATE
     C*
     C* Per pg. H - 2 # 5
     C*  Stop-Loss Method.  Stop-loss is an independent reimbursement
     C*  methodology that will reimburse that hospital for unusually
     C*  costly services rendered during treatment to an injured
     C*  worker. To be eligible for the stop-loss payment, the
     C*  total charges (audited) for the hospital admission must
     C*  exceed $50,000.
     C*  The total charges for the admission are mult by 80%
     C*  to determine the MRA.
     C*
     C*
     C                   Z-ADD     *ZEROS        XXPAID            9 2
     C                   Z-ADD     *ZEROS        XXCHRG            9 2          ADD 01/20/96
     C*** BEGIN DEL *** 11/22/96 **************************************
     C********** CAKYC2    CHAINPCA0C                60
     C**********
     C********** *IN60     DOWEQ'0'
     C*** BEGIN DEL *** 01/20/96 **************************************
     C********** XCCPT4    IFEQ DURATE
     C********** XCDSEQ    ANDNECADSEQ
     C********** XCTOTP    ANDGT*ZEROS
     C********** XCCPT4    ORNE DURATE
     C********** XCTOTP    ANDGT*ZEROS
     C**********           ADD  XCTOTP    XXPAID
     C*** END   DEL *** 01/20/96 **************************************
     C*** ACCUMULATE TOTAL CHARGED AMOUNT
     C**********           ADD  XCTOTC    XXCHRG           ADD 01/20/96
     C**********           ENDIF                           DEL 01/20/96
     C********** CAKYC2    READEPCA0C                    60
     C**********           ENDDO                           END DOWEQ
     C*** END   DEL *** 11/22/96 **************************************
     C*
     C*
     C** If code is in the FMSVP File and
     C     SVALOW        IFNE      *ZERO
     C*
     C**      If there is an MRA or Unit Value:
     C*  Per page H-1:
     C** DRG Unit Value X Conversion Factor (XXHOSC)
     C**     +    #Days X Per Day Charge (XXHOSD)
     C**     =    MRA per hospital stay
     C     SVALOW        MULT      XXHOSC        XTHOSC
     C     DUUNTS        MULT      XXHOSD        XTHOSD
     C     XTHOSC        ADD       XTHOSD        XPTOTA
     C*
     C*  Per pg. H - 2 Stop-Loss Method
     C*** BEGIN CHG *** 01/20/96 **************************************
     C*********  XXPAID    IFGT 50000
     C*********  XPTOTA    MULT .80       XPTOTA
     C*********  XXCHRG    IFGT 50000                      DEL 11/22/96
     C     CACHRG        IFGT      50000                                        ADD 11/22/96
     C     CACHRG        MULT      .80           XPTOTA
     C*** END   CHG *** 01/20/96 **************************************
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'CO'          QQC(Q)
     C*** Err Msg CO: "Stop-Loss: Total Claim > 50,000 MRA reduced
     C***              to 80%"
     C                   END
     C*
     C     XPTOTA        IFLT      CACHRG
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   ELSE                                                   ELS LT TOTC
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   END                                                    END LT TOTC
     C*
     C                   ELSE                                                   ELS NE *ZERO
     C*
     C**      If there is NOT an MRA or Unit Value:
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C*** BEGIN ADD *** 01/20/96 **************************************
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '06'          QQC(Q)
     C*** END   ADD *** 01/20/96 **************************************
     C*
     C*
     C                   END                                                    END NE *ZERO
     C*
     C                   ELSE                                                   ELS 0 *IN60
     C*
     C** If code is NOT in the FMSVP File:
     C*******************  Z-ADDCACHRG    XPTOTA
     C*******************  Z-ADD*ZERO     XPTOTD
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   MOVE      '201'         XPEOMB
     C*** EOMB 201: "Service is not reimbursable under the Worker's Comp Pgm"
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE                                                    ORD
     C*
     C*
     C                   END                                                    END 0 *IN60
     C*
     C                   Z-ADD     XPTOTA        XPTOTP
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
     C                   MOVE      '002'         XPEOMB
     C*** EOMB 002: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   END
     C*
     C     XPEOMB        IFNE      *BLANK
     C*
     C                   Z-ADD     SVDSEQ        KKDSEQ
     C                   EXSR      EOMB
     C*
     C                   ENDIF
     C*
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
     C     CAKYO         CHAIN     PCA0O                              55
      *
     C     *IN55         IFEQ      '0'
     C     CAFDAT        ANDEQ     OXFDAT
     C     CAFEIN        ANDEQ     OXFEIN
     C     CADSEQ        ANDNE     OXDSEQ
     C     OXFLAG        ANDNE     'P'
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
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** ONLY PROCESS THE RECORD IF NOT A OVERRIDE TYPE.
     C*
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     CSOVRD        IFNE      'Y'
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTP        CAPAID
     C                   MOVE      XPPMOD        XPPMOD
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
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XCMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C  N54              UPDATE    PCA01
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CLEAR THE WORK SAVE FIELDS.
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE         ENDSR
     OPCA01     E            ECA01
