     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4052NTX                                          *
     H*    DESCRIPTION: ADJUDICATION (08/01/97 RULES)                 *
     H*   This program is called from F0070R (Claim Adjudication)     *
     H*   and will perform the actual adjudication of the detail      *
     H*   claim records written by F0070R.                            *
     H*                                                               *
     H*  This Sub-program is intended to be used to calculate the     *
     H*  amount that is due a provider of services, by the carrier    *
     H*  of the Workers' Compensation insurance.                      *
     H*                                                               *
     H*  It should be used for any service provided on or after       *
     H*  08/01/97.                                                    *
     H*****************************************************************
     H*  CREATED: C:59 09/05/97    PROJECT: TX007                     *
     H*****************************************************************
     H*  Specialty Codes used in this program:
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                                    *
     H*  03/03/09 C:212 Support: 65839                                *    *
     H*                 Add additional parameter while calling FA747R *    *
     H*                                                               *    *
     H*  06/09/08 C:079 Support: MC144 63823                               *
     H*                 Fix so that no update is done without read on *    *
     H*                 FDCAL13                                       *    *
     H*                                                                    *
     H*  07/21/07 C:079 Support: MC144 57519                               *
     H*                 Correct 95 day rule logic                          *
     H*                                                               *
     H*  07/21/07 C:079 Support: 57703                                *
     H*                 Set the covered,not covered flag              *
     H*                                                               *
     H*  06/21/07 C:182 Support: 57703                                *
     H*                 Added call to FA747R to compute @@TECH.       *
     H*                                                               *
     H*  05/10/07 C:079 Support: 57703                                *
     H*                 1. Allow per diem payment > charged           *
     H*                 2. Deny and give a message on implants        *
     H*                                                               *
     H*  11/02/06 C:182 Project: TX033                                *
     H*                 Added logic for disallowing bill if #days frm *
     H*                 srv date is more than 95 days.                *
     H*
     H*  02/18/05 C:79  Project: TX029
     H*                 Add logic for Texas 837 ANSI Reason Codes
     H*
     H*  04/29/05 C:079 SUPPORT: MC025 46607
     H*                 ADD CACODE TO RENAME OF FDCAP TO PREVENT
     H*                 LOADING THE WRONG VALUE
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*
     H*  10/18/00 C:079 Support: MC025/#24669                         *
     H*                 Change key list and file for stop-loss to     *
     H*                 read only the current bill.                   *
     H*
     H*  07/31/00 C:079 Project: MCMNT
     H*                 Remove use of FDHCP
     H*
     H*  12/27/99 C:117 Project: MC010  Sup Seq#: 21094
     H*                 Taking DUUNTS out of DUKY1; Poss Duplicate
     H*                 problem.
     H*
     H*  04/08/99 C:117 Support: MC095 Seq# 17568
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs
     H*                 and changed all occurences of UPCO#, UPSUB#,
     H*                 UPFND to the above mentioned fields.
     H*
     H*                                                               *
     H*  09/21/98 C:79  Project: MC152                                *
     H*                 1.  Make Dates year 2000 compatible.          *
     H*                 2.  Modify to work with New Provider bills    *
     H*                                                               *
     H*  09/04/98 C:31  Project: 95MAL                                *
     H*                 Ntwk Disc:Lesser of FS/Chr                    *
     H*                 1. Add update of CAMRAA                       *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSVL1TX  IF   E           K DISK
     FFTABLTX   IF   E           K DISK
      *** BEG DEL *** 09/21/98 ****************************************
     F************FDCAL7  UF  E           K        DISK
      *** END DEL *** 09/21/98 ****************************************
     F*** BEG DEL *** 09/21/98 ****************************************
     F********FADCL   IF  E           K        DISK
     F********FDCALO  IF  E           K        DISK
     F********    PCA01                             KRENAMEPCA0O
     F*** END DEL *** 09/21/98 ****************************************
     F*** BEG CHG *** 10/18/00 ****************************************
     F*********** FDCALC  IF  E           K        DISK
     FFDCALH    IF   E           K DISK
     F*** END CHG *** 10/18/00 ****************************************
     F                                     RENAME(PCA01:PCA0C)
     F*** BEG DEL *** 07/31/00 ****************************************
     F**************** FDHCL   IF  E           K        DISK
     F*** END DEL *** 07/31/00 ****************************************
     F*** BEG ADD *** 09/21/98 ****************************************
     F* Message file logicals
     F*
     FFDMGL1    UF A E           K DISK
     FFDMGL5    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)
     F*  FDCAL13 is the Primary File Read (PCA01)
    F*** BEG CHG ***** 06/21/07 *************************************
     F**********FDCAL13 UF  E           K        DISK
     FFDCAL13   UF   E           K DISK    INFDS(DBFDS)
     F                                     RENAME(PCA01:PCA13)
    F*** END CHG ***** 06/21/07 *************************************
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDEL4    UF A E           K DISK
     FFAEOL4    IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFDDUL1    IF   E           K DISK
     F                                     RENAME(PDU01:PDU02)
     FFAEUL     IF   E           K DISK
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0F)
    F*** BEG ADD ***** 11/02/06 *************************************

     D*** BEG DEL *** 07/21/07 ****************************************
     F*****FDCAL27 IF  E           K        DISK
     F*****            PCA01                             KRENAMEPCA27
     D*** END DEL *** 07/21/07 ****************************************
    F*** END ADD ***** 11/02/06 *************************************
     F*** END ADD *** 09/21/98 ****************************************
     F*
     D*
      *** BEG DEL *** 09/21/98 ****************************************
     D******************* QQS        30  5               DETAIL SEQ #
     D******************* QQC        30  2               ERROR MESSAGES
     D******************* QQF        50  6               FROM DATES
      *** END DEL *** 09/21/98 ****************************************
      *** END ADD *** 09/21/99 ****************************************
    D*** BEG ADD ***** 11/02/06 ****************************************
    D*** BEG DEL ***** 11/02/06 ****************************************
     D*********PCA27
     D*********     CACO#                           TXCO#
     D*********     CAFND                           TXFND
     D*********     CAFYR                           TXFYR
     D*********     CACASE                          TXCASE
     D*********     CACSCD                          TXCSCD
     D*********     CAFDAT                          TXFDAT
     D*********     CATDAT                          TXTDAT
     D*********     CARDAT                          TXRDAT
     D*********     CAIPOP                          TXIPOP
     D*********     CAFEIN                          TXFEIN
     D*********     CAMOD                           TXMOD
     D*********     CAPRTY                          TXPRTY
     D*********     CATRCD                          TXTRCD
     D*********     CASVCD                          TXSVCD
     D*********     CAPMOD                          TXPMOD
     D*********     CACPT4                          TXCPT4
     D*********     CAICD9                          TXICD9
     D*********     CATOTA                          TXTOTA
     D*********     CAEOMB                          TXEOMB
     D*********     CADSEQ                          TXDSEQ
     D*********     CADELF                          TXDELF
     D*********     CAFLAG                          TXFLAG
     D*********     CACHK#                          TXCHK#
     D*********     CAEOB#                          TXEOB#
     D*********     CAPART                          TXPART
     D*********     CAP1                            TXP1
     D*********     CASS#                           TXSS#
     D*********     CAACDT                          TXACDT
     D*********     CASTAT                          TXSTAT
     D*********     CAUSER                          TXUSER
     D*********     CAPGM                           TXPGM
     D*********     CATIME                          TXTIME
     D*********     CADT                            TXDT
     D*********     CAIUSR                          TXIUSR
     D*********     CAIDT                           TXIDT
     D*********     CADXR                           TXDXR
     D*********     CADCS#                          TXDCS#
     D*********     CALTR#                          TXLTR#
     D*********     CARCFL                          TXRCFL
     D*********     CASUB#                          TXSUB#
     D*********     CAOTHR                          TXOTHR
     D*********     CANDIS                          TXNDIS
     D*********     CARDTP                          TXRDTP
     D*********     CABRSV                          TXBRSV
     D*********     CABRPM                          TXBRPM
     D*********     CABRPC                          TXBRPC
     D*********     CABRAM                          TXBRAM
     D*********     CABRUN                          TXBRUN
     D*********     CABRTC                          TXBRTC
     D*********     CAZIPA                          TXZIPA
     D*********     CAAST                           TXAST
     D*********     CADIFF                          TXDIFF
     D*********     CAIPGM                          TXIPGM
     D*********     CACHRG                          TXTOTC
     D*********     CAMRA                           TXTOTD
     D*********     CAOTH2                          TXOTH2
     D*********     CAPAID                          TXTOTP
     D*********     CARCN                           TXRCN
     D*********     CALIN#                          TXLIN#
     D*********     CARULE                          TXRULE
     D*********     CAOT1C                          TXOT1C
     D*********     CAOT2C                          TXOT2C
     D*********     CAMRAA                          TXMRAA
     D*********     CAITIM                          TXITIM
     D*********     CAMNAA                          TXMNAA
     D*********     CACODE                          TXCODE
     D*********
    D*** END DEL ***** 11/02/06 ****************************************
    D*** END ADD ***** 11/02/06 ****************************************
     D*
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
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     D*** BEG ADD *** 06/21/07 ****************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0

     I*
     I*  RENAME THE DUPLICATE FIELD NAMES.
     IPCA0C
     I              CACO#                       XCCO#
     I              CAFND                       XCFND
     I              CAFYR                       XCFYR
     I              CACASE                      XCCASE
     I              CACSCD                      XCCSCD
     I              CAFDAT                      XCFDAT
     I              CATDAT                      XCTDAT
     I              CARDAT                      XCRDAT
     I              CAIPOP                      XCIPOP
     I              CAFEIN                      XCFEIN
     I              CAMOD                       XCMOD
     I              CAPRTY                      XCPRTY
     I              CATRCD                      XCTRCD
     I              CASVCD                      XCSVCD
     I              CAPMOD                      XCPMOD
     I              CACPT4                      XCCPT4
     I              CAICD9                      XCICD9
     I              CATOTA                      XCTOTA
     I              CAEOMB                      XCEOMB
     I              CADSEQ                      XCDSEQ
     I              CADELF                      XCDELF
     I              CAFLAG                      XCFLAG
     I              CACHK#                      XCCHK#
     I              CAEOB#                      XCEOB#
     I              CAPART                      XCPART
     I              CAP1                        XCP1
     I              CASS#                       XCSS#
     I              CAACDT                      XCACDT
     I              CASTAT                      XCSTAT
     I              CAUSER                      XCUSER
     I              CAPGM                       XCPGM
     I              CATIME                      XCTIME
     I              CADT                        XCDT
     I              CAIUSR                      XCIUSR
     I              CAIDT                       XCIDT
     I              CADXR                       XCDXR
     I              CADCS#                      XCDCS#
     I              CALTR#                      XCLTR#
     I              CARCFL                      XCRCFL
     I              CASUB#                      XCSUB#
     I              CAOTHR                      XCOTHR
     I              CANDIS                      XCNDIS
     I              CARDTP                      XCRDTP
     I              CABRSV                      XCBRSV
     I              CABRPM                      XCBRPM
     I              CABRPC                      XCBRPC
     I              CABRAM                      XCBRAM
     I              CABRUN                      XCBRUN
     I              CABRTC                      XCBRTC
     I              CAZIPA                      XCZIPA
     I              CAAST                       XCAST
     I              CADIFF                      XCDIFF
     I              CAIPGM                      XCIPGM
     I              CACHRG                      XCTOTC
     I              CAMRA                       XCTOTD
     I              CAOTH2                      XCOTH2
     I              CAPAID                      XCTOTP
     I              CARCN                       XCRCN
     I              CALIN#                      XCLIN#
     I              CARULE                      XCRULE
     I              CAOT1C                      XCOT1C
     I              CAOT2C                      XCOT2C
     I              CAMRAA                      XCMRAA
     I              CAITIM                      XCITIM
     I              CAMNAA                      XCMNAA
     I*** BEG ADD *** 04/29/05 ****************************************
     I              CACODE                      XCCODE
     I*** END ADD *** 04/29/05 ****************************************
     I*
      *** BED DEL *** 09/21/99 ****************************************
     I******PCA0O
     I************* CACO#                           OXCO#
     I************* CAFND                           OXFND
     I************* CAFYR                           OXFYR
     I************* CACASE                          OXCASE
     I************* CACSCD                          OXCSCD
     I************* CAFDAT                          OXFDAT
     I************* CATDAT                          OXTDAT
     I************* CARDAT                          OXRDAT
     I************* CAIPOP                          OXIPOP
     I************* CAFEIN                          OXFEIN
     I************* CAMOD                           OXMOD
     I************* CAPRTY                          OXPRTY
     I************* CATRCD                          OXTRCD
     I************* CASVCD                          OXSVCD
     I************* CAPMOD                          OXPMOD
     I************* CACPT4                          OXCPT4
     I************* CAICD9                          OXICD9
     I************* CATOTA                          OXTOTA
     I************* CAEOMB                          OXEOMB
     I************* CADSEQ                          OXDSEQ
     I************* CADELF                          OXDELF
     I************* CAFLAG                          OXFLAG
     I************* CACHK#                          OXCHK#
     I************* CAEOB#                          OXEOB#
     I************* CAPART                          OXPART
     I************* CAP1                            OXP1
     I************* CASS#                           OXSS#
     I************* CAACDT                          OXACDT
     I************* CASTAT                          OXSTAT
     I************* CAUSER                          OXUSER
     I************* CAPGM                           OXPGM
     I************* CATIME                          OXTIME
     I************* CADT                            OXDT
     I************* CAIUSR                          OXIUSR
     I************* CAIDT                           OXIDT
     I************* CADXR                           OXDXR
     I************* CADCS#                          OXDCS#
     I************* CALTR#                          OXLTR#
     I************* CARCFL                          OXRCFL
     I************* CASUB#                          OXSUB#
     I************* CAOTHR                          OXOTHR
     I************* CANDIS                          OXNDIS
     I************* CARDTP                          OXRDTP
     I************* CABRSV                          OXBRSV
     I************* CABRPM                          OXBRPM
     I************* CABRPC                          OXBRPC
     I************* CABRAM                          OXBRAM
     I************* CABRUN                          OXBRUN
     I************* CABRTC                          OXBRTC
     I************* CAZIPA                          OXZIPA
     I************* CAAST                           OXAST
     I************* CADIFF                          OXDIFF
     I************* CAIPGM                          OXIPGM
     I************* CACHRG                          OXTOTC
     I************* CAMRA                           OXTOTD
     I************* CAOTH2                          OXOTH2
     I************* CAPAID                          OXTOTP
     I************* CARCN                           OXRCN
     I************* CALIN#                          OXLIN#
     I************* CARULE                          OXRULE
     I************* CAOT1C                          OXOT1C
     I************* CAOT2C                          OXOT2C
     I************* CAMRAA                          OXMRAA
     I************* CAITIM                          OXITIM
     I************* CAMNAA                          OXMNAA
      *** END DEL *** 09/21/99 ****************************************
      *** BEG ADD *** 09/21/99 ****************************************
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
     I*** BEG ADD *** 04/29/05 ****************************************
     I              CACODE                      XFCODE
     I*** END ADD *** 04/29/05 ****************************************
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
     I*** END ADD *** 06/21/07 ****************************************

     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***CLAIM*DETAIL FILE
     C********** CAKY1     KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           QQSUB#
     C**********           KFLD           QQFND
     C**********           KFLD           QQFYR
     C**********           KFLD           QQCASE
     C**********           KFLD           XXYMD
      *** END DEL *** 09/21/98 ****************************************
     C*
     C     CAKY1A        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C                   KFLD                    SVPRTY
     C                   KFLD                    SVDSEQ
     C                   KFLD                    SVEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** KFLD           SVCHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C     CAKY1Z        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    HLFEIN
     C                   KFLD                    HLMOD
     C                   KFLD                    HLPRTY
     C                   KFLD                    HLDSEQ
     C                   KFLD                    HLEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** KFLD           HLCHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C     CAKY4         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXSVCD
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXUNTS
     C*
     C*
     C     CAKYO         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    XXSVCD
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXUNTS
     C*
     C*** BEG DEL *** 10/18/00 ****************************************
     C********** CAKYC2    KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           QQSUB#
     C**********           KFLD           QQFND
     C**********           KFLD           QQFYR
     C**********           KFLD           QQCASE
     C**********           KFLD           CAFEIN
     C**********           KFLD           CAMOD
     C*** END DEL *** 10/18/00 ****************************************
     C*
     C*** BEG ADD *** 10/18/00 ****************************************
     C     CAKYC2        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*** END ADD *** 10/18/00 ****************************************
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
      *** BEG DEL *** 09/21/98 ****************************************
     C**********
     C***DIAGNOSIS CODE/DETAIL CHARGE FILE
     C********** DCKY      KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           QQSUB#
     C**********           KFLD           QQFND
     C**********           KFLD           QQFYR
     C**********           KFLD           QQCASE
     C**********           KFLD           CAFEIN
     C**********           KFLD           CAMOD
      *** END DEL *** 09/21/98 ****************************************
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
     C* DETAIL CHARGE AUXILIARY FILE
     C*
     C*** BEG DEL *** 07/31/00 ****************************************
     C********** HCKY      KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           QQSUB#
     C**********           KFLD           QQFND
     C**********           KFLD           QQFYR
     C**********           KFLD           QQCASE
     C**********           KFLD           CADSEQ
     C*** END DEL *** 07/31/00 ****************************************
     C*** BEG ADD *** 09/21/98 ****************************************
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
     C     CAKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXYMD
     C*
     C     CAKY2         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C     CAKYF         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    KKDSEQ
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
     C     DEKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    QQDSEQ
     C*
     C     EBKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    EBKEY
     C*
     C     EOKY          KLIST
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
     C                   KFLD                    XXDSEQ
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
     C     EUKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*** END ADD *** 09/21/98 ****************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** PARM           QQFDT 150
     C******************** PARM           QQFDT2150
     C******************** PARM           QQS
     C******************** PARM           QQC
      *** END DEL *** 09/21/98 ****************************************
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9 0
     C                   PARM                    QQACDT            8 0          (MMDDYY ORD)
     C                   PARM                    QQSPCD            3
      *** BEG ADD *** 09/21/98 ****************************************
     C                   PARM                    QQEOB#            9 0
      *** END ADD *** 09/21/98 ****************************************
     C*
     C*** BEG ADD *** 04/08/99 ***************************************
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C*** END ADD *** 04/08/99 ***************************************
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
    C*
    C*** BEG ADD ***** 11/02/06 *****************************************
    C*
     C     DDIFF         PLIST
     C                   PARM                    SRDAT1            7 0
     C                   PARM                    SRDAT2            7 0
     C                   PARM      *ZEROS        SR#DYS            5 0
     C                   PARM      *BLANKS       SRERR             1
    C*
    C*** END ADD ***** 11/02/06 *****************************************
    C*
     C***************************************************************
     C* INITIALIZATION SECTION
     C                   Z-ADD     *ZEROS        S                 3 0
     C                   Z-ADD     *ZEROS        Q                 3 0
     C                   Z-ADD     *ZEROS        XXALOW            9 2
     C*** BEGIN CHG *** 09/25/01 **************************************
     C**********           Z-ADD*ZEROS    XXOVRP  32
     C                   Z-ADD     *ZEROS        XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C*
     C                   Z-ADD     001.75        XXRADI            5 2           RADI Conversion
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
      *** BEG DEL *** 09/21/98 ****************************************
     C********** *LIKE     DEFN CACHK#    HLCHK#           7.0
     C********** *LIKE     DEFN CACHK#    SVCHK#           7.0
      *** END DEL *** 09/21/98 ****************************************
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
     C     *LIKE         DEFINE    CADIFF        XPDIFF                         9.2
     C     *LIKE         DEFINE    CABRTC        XPBRTC
     C     *LIKE         DEFINE    DURATE        XXCPT4                         5
     C     *LIKE         DEFINE    CAFDAT        XXDATE
     C     *LIKE         DEFINE    DURVCD        XXSVCD                         3.0
     C     *LIKE         DEFINE    DUUNTS        XXUNTS                         5.2
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C     *LIKE         DEFINE    CATOTA        XXINVC                         9.2
     C     *LIKE         DEFINE    CATOTA        XXDIEM                         9.2
     C     *LIKE         DEFINE    CATOTA        XXMRA                          9.2
     C*
     C     *LIKE         DEFINE    CAFEIN        SXFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SXMOD                          3.0
     C*** BEG ADD *** 09/21/98 ****************************************
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MGMSEQ        XXMSEQ
     C     *LIKE         DEFINE    MGDSEQ        XXDSEQ
     C     *LIKE         DEFINE    DEESRC        QQESRC
     C     *LIKE         DEFINE    DEDSEQ        QQDSEQ
     C     *LIKE         DEFINE    DEDSEQ        KKDSEQ
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    DERCN         XXRCN
     C     *LIKE         DEFINE    EOSTYP        XXOUT
     C     *LIKE         DEFINE    EUICN         XXTOS
     C*** END ADD *** 09/21/98 ****************************************
     C*
     C***************************************************************
     C*
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVEAQQFDT     QQF,1
     C******************** MOVEAQQFDT2    QQF,26
      *** END DEL *** 09/21/98 ****************************************
     C*** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'XX00000'     XXMSG#
     C*** END ADD *** 09/21/98 ****************************************
     C*
     C** GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C** IF THE GET WAS UNSUCCESSFUL END THE PROGRAM.
     C*
     C     *IN60         IFEQ      '1'
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVE '99999'   QQS,1
     C******************** MOVE '95'      QQC,1
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '95'          XXMSG#
     C                   MOVE      '99999'       XXDSEQ
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
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
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVE '99998'   QQS,1
     C******************** MOVE '96'      QQC,1
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '96'          XXMSG#
     C                   MOVE      '99998'       XXDSEQ
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C* Err Msg 96: Invalid Case Number, See Programmer
     C                   GOTO      TERM
     C                   END
      *** BEG DEL *** 09/21/98 ****************************************
      *******************
     C*******************
     C***ADJUDICATE*FOR*EACH POSSIBLE DATE ENTERED IF AFTER 08/01/91
     C*******************
     C*******************  DO   50        S
     C*******************  MOVE QQF,S     XXDATE
     C*******************  Z-ADDXXDATE    SRDTI
     C*******************  CALL 'SRCVTDAT'DTCYMD
     C*******************  MOVELSRDTO     XXYMD
     C*******************
     C***********QQF,S***  IFNE *ZERO
     C***********XXYMD***  ANDGE970801
     C***********CAKY1***  CHAINPCA01                54
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
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
     C     CAKY2         SETLL     PCA13
     C     CAKY2         READE     PCA13                                  54
      *
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
      *
     C     CAFDAT        IFGE      970801
     C     XXRCN         ANDEQ     CARCN
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     CADSEQ        XXDSEQ
     C*
     C     DUKY          CHAIN     PDU01                              60
     C*
     C                   Z-ADD     CAFDAT        XXYMD
     C                   MOVE      *BLANKS       XPEOMB
      *** END ADD *** 09/21/98 ****************************************
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     DUUNTS        SVUNTS
     C                   MOVEL     DURATE        XPCPT4
     C                   MOVE      CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADDCACHK#    SVCHK#
      *** END DEL *** 09/21/98 ****************************************
     C* --- Get and save the Provider's Specialty Code ----------------
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C*+*+*+*+*+*+* Do Loop for Each Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C********** *IN54     DOWEQ'0'                        DO FOR EACH RCD
     C**********           EXCPTRCA01
      *** END DEL *** 09/21/98 ****************************************
     C*
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '79'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '79'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END EQ *ZEROS
      *** END DEL *** 09/21/98 ****************************************
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
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    IFEQ *ZEROS
     C********** CACHK#    ANDEQ*ZEROS
     C**********           Z-ADD1         Q
     C********** '00000'   LOKUPQQS,Q                    60
     C**N60*****           Z-ADD30        Q
     C**********           MOVELCADSEQ    QQS,Q
     C**********           MOVE '97'      QQC,Q            INVALID CLM STS CODE
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '97'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 97: "Invalid Claim Status Code, See Programmer"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
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
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    IFEQ *ZERO
     C*
     C**********           MOVE DURVCD    XXSVCD
     C**********           MOVE DURATE    XXCPT4
     C**********           MOVE DUUNTS    XXUNTS
     C**********
     C********** CAKYO     CHAINPCA0O                54
     C**********
     C********** DUPCL3    TAG
     C**********
     C********** *IN54     IFEQ '0'
     C**********
     C********** CADSEQ    IFNE OXDSEQ
     C********** CADSEQ ** OREQ PXDSEQ
     C********** CAEOB# ** ANDNEPXEOB#
     C*
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    61
     C**N61*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '03'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM ON HOSPITAL.
      *** BEG CHG *** 06/09/08 ****************************************
     C********** DUKY1     CHAINPDU02                54
     C********** *IN54     DOWEQ'0'
     C     DUKY1         CHAIN     PDU02                              56
     C     *IN56         DOWEQ     '0'
      *
      *** END CHG *** 06/09/08 ****************************************
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
     C                   MOVE      '03'          XXMSG#
     C                   EXSR      MSG
      *** BEG CHG *** 06/09/08 ****************************************
     C******************** MOVE *ON       *IN54
     C                   MOVE      *ON           *IN56
      *** END CHG *** 06/09/08 ****************************************
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C                   END
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** ELSE
     C********************
     C***********CAKYO**** READEPCA0O                    54            3
     C**N54*************** GOTO DUPCL3
      *** END DEL *** 09/21/98 ****************************************
     C                   END
      *** BEG ADD *** 09/21/98 ****************************************
      *** BEG CHG *** 06/09/08 ****************************************
     C**N54***** DUKY1     READEPDU02                    54            3
     C  N56DUKY1         READE     PDU02                                  56                3
      *** END CHG *** 06/09/08 ****************************************
     C*
     C                   ENDDO                                                  *IN56 DOWEQ
      *** END ADD *** 09/21/98 ****************************************
     C                   END
     C                   END
     C*
     C* ===== OVERRIDE TYPE ==========================================
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAKY1A    CHAINPCA01                54
     C********** *IN54     IFEQ '0'
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     CAEOB#        IFEQ      QQEOB#
      *** BEG ADD *** 06/09/08 ****************************************
     C     CSOVRD        ANDEQ     'Y'
      *** END ADD *** 06/09/08 ****************************************
      *** END ADD *** 09/21/98 ****************************************
     C*** IF OVERRIDE TYPE WE MUST STILL OUTPUT THE ALLOWED/DISALLOWED ETC.
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    IFEQ *ZERO
      *** BEG DEL *** 09/21/98 ****************************************
     C     CAMRA         ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CAPAID        ANDEQ     0
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CATOTA    ANDEQ0
      *** END DEL *** 09/21/98 ****************************************
     C*
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     *ZEROS        XPTOTA
      *** END ADD *** 09/21/98 ****************************************
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C                   END
     C*
     C                   Z-ADD     *ZEROS        CABRTC
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CAPAID
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XCMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
      *** BEG CHG *** 09/21/98 ****************************************
     C******************** UPDATPCA01
     C                   UPDATE    PCA13
      *** END CHG *** 09/21/98 ****************************************
     C                   END
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C     NXTRCD        TAG
     C*
     C*
     C*+*+*+*+*+*+*+*+*+*+ Read Next Claim Detail Record +*+*+*+*+*+*+*+*+*+**
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAKY1     READEPCA01                    54  GET NEXT RCD
     C     CAKY2         READE     PCA13                                  54      GET NEXT RCD
      *** END CHG *** 09/21/98 ****************************************
     C*                                                      TO PROCESS
     C*
      *** BEG DEL *** 09/21/98 ****************************************
      **********
     C********** *IN54     IFEQ '0'                        SAVE FIELDS OF
     C**********           Z-ADDCADSEQ    SVDSEQ           CURRENT REC.
     C**********           MOVE CAFEIN    SVFEIN
     C**********           MOVE CAMOD     SVMOD
     C**********           MOVE CAPRTY    SVPRTY
     C**********           Z-ADDDUUNTS    SVUNTS
     C**********           MOVELDURATE    XPCPT4
     C**********           MOVE CAPMOD    XPPMOD
     C**********           Z-ADDCAEOB#    SVEOB#
     C**********           Z-ADDCACHK#    SVCHK#
     C*
     C*----------------------------------------------------------------
     C* --- Get and save the Provider's Specialty Code ----------------
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDDO                                                  *IN54=OFF
      *** END ADD *** 09/21/98 ****************************************
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
     C******************** ENDDO                           *IN54=OFF
      *** END DEL *** 09/21/98 ****************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C** RELEASE ALL THE FORMATS FOR A SAVE RETURN.
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***********DUMMY     CHAINPCA01                54
     C**N54******          EXCPTRCA01
      *** END DEL *** 09/21/98 ****************************************
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             XCMRA             9 2
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *** BEG ADD *** 09/21/98 *****************************************
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
     *** END ADD *** 09/21/98 *****************************************
     *** BEG ADD *** 09/21/98 *****************************************
     *********************************************************************
     *** ADJUDICATION MESSAGES SUBROUTINE                    M S G       *
     *********************************************************************
     C     MSG           BEGSR
      *
      * Load the Key fields
      *
     C                   MOVE      CADSEQ        XXDSEQ
      *
      * Is this message already in the file for this line item
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
     C     MSGE          ENDSR
     *** END ADD *** 09/21/98 *****************************************
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
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADDCACHK#    HLCHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C**** NOT NEEDED *************
     C                   Z-ADD     HLDSEQ        CADSEQ                         SAVE FIELD VALS
     C                   MOVE      HLFEIN        CAFEIN                         FOR RECORD WE
     C                   MOVE      HLMOD         CAMOD                          ARE PROCESSING
     C                   MOVE      HLPRTY        CAPRTY
     C                   Z-ADD     HLEOB#        CAEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADDHLCHK#    CACHK#
      *** END DEL *** 09/21/98 ****************************************
     C*** REGET RECORD.
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAKY1Z    CHAINPCA01                54
     C**********
     C**N54*****           EXCPTRCA01
      *** END DEL *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ
     C                   MOVE      CAFEIN        SVFEIN                            CURR REC.
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAEOB#        SVEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADDCACHK#    SVCHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C*----------------------------------------------------------------
     C* Get and save the Provider's Specialty Code
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
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
     C*----------------------------------------------------------------
     C* Get INPATIENT / OUTPATIENT FLAG
     C                   MOVE      *BLANKS       XXOUT
      *** BEG ADD *** 09/21/98 ****************************************
      *
     C     EOKY          CHAIN     PEO01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      EOSTYP        XXOUT
      *
     C                   ENDIF                                                  *IN60=OFF
      *
     C     EUKY          CHAIN     PEU01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      EUICN         XXTOS
      *
     C                   ENDIF                                                  *IN60=OFF
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C********** DCKY      CHAINPDC01                60
     C********** *IN60     IFEQ '0'
     C**********           SUBSTDC10A1:6  XXOUT   3
     C**********           SUBSTDC10A3:1  XXTOS   2
     C**********           END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C*-- GET INVOICE AMOUNT FOR IMPLANTABLES -----------------------------
     C*
     C*--------------------------------------------------------------------
     C*
     C*** USE STANDARD (000xxxxx000) AS SVKY (I.E. 000ROOMM000)
     C*** TO RETRIEVE SERVICE CODE RECS (FOR PER DIEM RATES)
     C                   Z-ADD     DURVCD        XXRVCD
     C*** BEGIN CHG *** 08/29/97 **************************************
     C** ALLOWS USER TO INPUT BOTH CPT AND REVENUE WITH DESC FOR REV
     C     DURATE        IFGE      '70000'
     C     DURATE        ANDLE     '76400'
     C                   MOVE      '000'         XXRVCD
     C                   MOVE      DURATE        XXHCPC            5
     C                   ELSE
     C                   MOVE      *BLANKS       XXHCPC            5
     C                   ENDIF
     C*** END CHG ***** 08/29/97 **************************************
     C                   Z-ADD     *ZEROS        XXUNIT
     C*
     C     SVKY          SETGT     PSV01                                        GET SERV CODE
     C     NXTSV1        TAG
     C                   READP     PSV01                                  60     RECORD
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     CACO#
     C     SVCMSV        ANDEQ     XXCMSV
     C     SVEFDT        ANDLE     CAFDAT                                       ADD 08/24/95
     C     SVSTAT        CABEQ     'I'           NXTSV1
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     SVALOW        XCMRA
     C*** END ADD *** 09/04/98 ****************************************
      *
      * USED TO ADD 10% TO ANY CODE WITH SERVICE TYPE EQUAL TO '6'
      *
     C                   MOVEL     SVTYPE        XXTYPE            1
      *
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C     SVCO#         ORNE      CACO#
     C     SVCMSV        ORNE      XXCMSV
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZERO         XPDIFF
     C*** BEG ADD *** 02/18/05 ***************************************
     C******************** MOVE 'G  '     XPEOMB
     C                   MOVE      '97 '         XPEOMB
     C*** END ADD *** 02/18/05 ***************************************
     C*** EOMB G:   "INCLUDED IN GLOBAL CHARGE"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   END                                                    END *IN60=1
    C*
    C*** BEG ADD ***** 11/02/06 *****************************************
    C*
     C* Check for the # of days between carrier received date and latest
     C* service date in the bill, if # of days is 95 days or less then
     C* disallow the bill.
    C*
     C     EORCN         IFEQ      0
    C*
    C*
    C* Fetch the latest to service date from FDCAL27
    C*
     C*** BEG DEL *** 07/21/07 ****************************************
     C********   *LOVAL    SETLLFDCAL27
     C********   CAKYC2    SETGTFDCAL27
     C********   CAKYC2    REDPEFDCAL27                  71
     C********
    C********
     C********   *IN71     IFEQ *OFF
    C********
     C********             MOVE TXTDAT    SRDAT1
    C********
     C********             ELSE
    C********
     C********             MOVE EOTODT    SRDAT1
    C********
     C********             ENDIF
    C********
     C********             MOVE EOCRDT    SRDAT2
     C*** END DEL *** 07/21/07 ****************************************
    C*
     C*** BEG ADD *** 07/21/07 ****************************************
     C                   MOVE      CAFDAT        SRDAT1
     C                   MOVE      EOCSDT        SRDAT2
     C*** END ADD *** 07/21/07 ****************************************
     C                   CALL      'SRDDIF7'     DDIFF
    C*
     C     SR#DYS        IFLE      95
     C     SRERR         ORNE      '0'
    C*
     C                   GOTO      SKIP
    C*
     C                   ENDIF
    C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZERO         XPDIFF
     C                   MOVE      '29 '         XPEOMB
    C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
    C*
     C     SKIP          TAG
    C*
     C                   END
    C*
    C*** END ADD ***** 11/02/06 *****************************************
    C*
     C****************************************************************
      *  Page 66, #5  if any of the following ICD9 codes are billed
      *  as the #1 spot, diagnosis 800 95950, 940-9499, 042-0449 pay
      *  100% with error message
     C*
     C     CAICD9        IFGE      '800  '
     C     CAICD9        ANDLE     '95950'
      *
     C     CAICD9        ORGE      '042  '
     C     CAICD9        ANDLE     '0449 '
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'GI'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'GI'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg GI: "Carrier to set payment at fair and reasonable  te
     C*                rate."
      *
      *    Uses CACHRG to pay in full in calculation
      *
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   ENDIF                                                  END DURVCD
     C*
      *
      ****************************************************************
      *  Priority 2
      *  Page 71, If the total billed amount is > $40,000.00 pay the
      *  billed amount X 75%
      *
     C*  Stop-Loss Method.  Stop-loss is an independent reimbursement
     C*  methodology that will reimburse that hospital for unusually
     C*  costly services rendered during treatment to an injured
     C*  worker. To be eligible for the stop-loss payment, the
     C*  total charges (audited) for the hospital admission must
     C*  exceed $40,000.
     C*  The total charges for the admission are mult by 75%
     C*  to determine the MRA.
     C*
     C                   Z-ADD     *ZEROS        XXCHRG            9 2          ADD 01/20/96
     C     CAKYC2        CHAIN     PCA0C                              60
     C*
     C     *IN60         DOWEQ     '0'
     C***        XCCPT4    IFEQ DURATE
     C***        XCDSEQ    ANDNECADSEQ
     C***        XCTOTP    ANDGT*ZEROS                     IN TESTING
     C***        XCCPT4    ORNE DURATE
     C***        XCTOTP    ANDGT*ZEROS                     IN TESTING
     C*   ACCUMULATE TOTAL CHARGED AMOUNT
     C                   ADD       XCTOTC        XXCHRG
     C***                  ENDIF
     C     CAKYC2        READE     PCA0C                                  60
     C                   ENDDO                                                  END DOWEQ
     C     XXCHRG        IFGT      40000.00
     C     CACHRG        MULT      .75           XXMRA                                      sion
      *
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'GJ'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'GJ'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg GJ: "Stop Loss reached, billed is greater than $40,000.00
      *
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   ENDIF
      ****************************************************************
      *  Page 70A, any service code with service type equal to 6, add
      *  10% to CHARGED AMOUNT.  This will be just a message.
      *
     C     XXTYPE        IFEQ      '6'
     C     XXTOS         IFEQ      'IR'
     C     XXTOS         OREQ      'IP'
     C     XXTOS         OREQ      'IM'
     C     XXTOS         OREQ      'IS'
     C*** BEG ADD *** 05/10/07 ****************************************
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C                   MOVE      'AD'          XXMSG#
     C                   EXSR      MSG
     C*
     C* Update charge with payment
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C*** END ADD *** 05/10/07 ****************************************
      *
      * CHAIN TO THE DETAIL CHARGE AUXILIARY FILE TO GET INVOICE AMT
      *
     C*** BEG DEL *** 07/31/00 ****************************************
     C********** HCKY      CHAINPHC01                60
     C********** *IN60     IFEQ '0'
     C**********
     C********** XXRVCD    IFGE 274
     C********** XXRVCD    ANDLE276
     C********** XXRVCD    OREQ 278
     C********** HC92N2    IFGE 250
     C********** HC92N2    MULT 1.10      XXINVC
     C**********           ELSE
     C**********           Z-ADDHC92N2    XXINVC
     C**********           ENDIF
     C**********
     C********** CACHRG    IFLE XXINVC
     C**********
     C**********           Z-ADD*ZEROS    XPDIFF
     C**********           Z-ADDCACHRG    XPTOTA
     C**********           Z-ADD*ZEROS    XPTOTD
     C**********           Z-ADDXPTOTA    XPTOTP
     C**********           Z-ADDXPTOTA    XPBRTC
     C**********           ELSE
     C**********
     C**********           Z-ADD*ZEROS    XPDIFF
     C**********           Z-ADDXXINVC    XPTOTA
     C********** CACHRG    SUB  XPTOTA    XPTOTD
     C**********           Z-ADDXPTOTA    XPTOTP
     C**********           Z-ADDXPTOTA    XPBRTC
     C**********
     C**********           END                             END CACHRG
     C**********           ENDIF                           END XXRVCD
     C**********
     C**********
     C**Update*charge with payment
     C**********
     C**********           EXSR UPDAT
     C**********           GOTO HOSPE
     C**********
     C**********           END                             END *IN60 EQ '0'
     C*** END DEL *** 07/31/00 ****************************************
     C                   END                                                    END XXTOS
     C                   END                                                    END SVTYPE EQ 6
     C*
      ****************************************************************
      *  Page 70A, any service code with service type equal to 6, add
      *  If IR, IP, IM, IS with 250-269  no pay
      *  If HS, HO, AS, AO with 250-269  Pay in full
      *
     C     XXRVCD        IFGE      250
     C     XXRVCD        ANDLE     269
      *
     C     XXTOS         IFEQ      'IR'
     C     XXTOS         OREQ      'IP'
     C     XXTOS         OREQ      'IM'
     C     XXTOS         OREQ      'IS'
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C*** BEG ADD *** 02/18/05 ***************************************
     C******************** MOVE 'G  '     XPEOMB
     C                   MOVE      '97 '         XPEOMB
     C*** END ADD *** 02/18/05 ***************************************
     C*** EOMB G:   "INCLUDED IN GLOBAL CHARGE"
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C     CACHRG        COMP      XXMRA                              60
     C   60              Z-ADD     XXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   END                                                    END XXTOS
     C*
     C* Update charge with payment
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   ENDIF                                                  END XXRVCD
     C*
      ****************************************************************
      *  REVISION -- SUPPORT 9314
     C*
     C     DURATE        IFGE      '70000'
     C     DURATE        ANDLE     '79999'
      *****************************************************************
      *
      * DETERMINES INPATIENT OR OUTPATIENT AND GETS FLAG ACCORDINGLY
      *
     C     XXTOS         IFEQ      'IR'
     C     XXTOS         OREQ      'IP'
     C     XXTOS         OREQ      'IM'
     C     XXTOS         OREQ      'IS'
      *
     C                   MOVEL     SVFL01        XXFL              1
     C                   ELSE
     C                   MOVEL     SVFL02        XXFL
     C                   ENDIF
     C*** BEG ADD *** 07/21/07 ***************************************
     C                   MOVEL     'C'           XXFL
     C*** END ADD *** 07/21/07 ***************************************
      *****************************************************************
      *
     C     XXFL          IFNE      'C'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZERO         XPDIFF
     C*** BEG ADD *** 02/18/05 ***************************************
     C******************** MOVE 'G  '     XPEOMB
     C                   MOVE      '97 '         XPEOMB
     C*** END ADD *** 02/18/05 ***************************************
     C*** EOMB G:   "INCLUDED IN GLOBAL CHARGE"
     C*
     C                   ELSE
     C*****BEG ADD******06/21/07*************************************
     C                   CALL      'FA747R'
     C                   PARM      DBRRN         @@RRN             9 0
     C                   PARM                    @@MRA             9 2
     C                   PARM                    @@PROF            9 2
     C                   PARM                    @@TECH            9 2
     C*** BEG ADD *** 03/03/09 ****************************************
     C                   PARM      *BLANKS       @@1A7             1
     C*** END ADD *** 03/03/09 ****************************************
     C*
     C                   MOVE      @@TECH        SVALOW
     C*****END ADD******06/21/07*************************************
     C                   Z-ADD     *ZEROS        XPDIFF
     C     CACHRG        COMP      SVALOW                             60
     C   60              Z-ADD     SVALOW        XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
      *
     C                   ENDIF                                                  END XXTOS
      *
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   ENDIF                                                  END DURATE
      *
      ****************************************************************
      *  PAGE 68.
      *  Revenue codes 100-169 need to caulculate the number of units
      *  for the line item by either 870.00 (IM) or 1118.00(IS)
      *
     C     XXTOS         IFEQ      'IM'
     C     XXTOS         OREQ      'IS'
     C                   MOVE      'Y'           XXFLG             1
      *
     C     DURVCD        IFGE      200
     C     DURVCD        ANDLE     214
     C     DUUNTS        MULT      SVALOW        XXMRA
     C                   MOVE      'N'           XXFLG
     C                   ENDIF                                                  END XXTOS IM
      *
     C     XXTOS         IFEQ      'IM'
     C     DURVCD        ANDGE     100
     C     DURVCD        ANDLE     169
     C     DUUNTS        MULT      870.00        XXMRA
     C                   MOVE      'N'           XXFLG
     C                   ENDIF                                                  END XXTOS IM
      *
     C     XXTOS         IFEQ      'IS'
     C     DURVCD        ANDGE     100
     C     DURVCD        ANDLE     169
     C     DUUNTS        MULT      1118.00       XXMRA
     C                   MOVE      'N'           XXFLG
     C                   ENDIF                                                  END XXTOS IM
     C*
     C     DURVCD        IFGE      380
     C     DURVCD        ANDLE     399
     C     DURVCD        OREQ      413
     C     DURVCD        OREQ      545
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'GI'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'GI'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg GI: "Carrier to set payment at fair and reasonable  te
     C*                rate."
     C                   MOVE      'N'           XXFLG
     C     DUUNTS        MULT      SVALOW        XXMRA
     C                   ENDIF                                                  END DURVCD
      *
     C     XXFLG         IFEQ      'Y'
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZERO         XPDIFF
     C*** BEG ADD *** 02/18/05 ***************************************
     C******************** MOVE 'G  '     XPEOMB
     C                   MOVE      '97 '         XPEOMB
     C*** END ADD *** 02/18/05 ***************************************
     C*** EOMB G:   "INCLUDED IN GLOBAL CHARGE"
     C*
     C                   ELSE
     C                   Z-ADD     *ZEROS        XPDIFF
     C*** BEG DEL *** 05/10/07 ****************************************
     C***        CACHRG    COMP XXMRA                60
     C***60                Z-ADDXXMRA     XPTOTA
     C**N60                Z-ADDCACHRG    XPTOTA
     C*** END DEL *** 05/10/07 ****************************************
     C*** BEG ADD *** 05/10/07 ****************************************
     C                   Z-ADD     XXMRA         XPTOTA
     C*** END ADD *** 05/10/07 ****************************************
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   ENDIF                                                  END DURVCD
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   ENDIF                                                  END XXTOS
     C*
      ****************************************************************
      *  Page 66, If type of service IR, IP, HS, HO, AS or AO is billed
      *  pay charged amount with error message.
     C*
     C     XXTOS         IFEQ      'IR'
     C     XXTOS         OREQ      'IP'
     C     XXTOS         OREQ      'HS'
     C     XXTOS         OREQ      'HO'
     C     XXTOS         OREQ      'AS'
     C     XXTOS         OREQ      'AP'
     C     XXTOS         OREQ      'IM'
     C     XXTOS         OREQ      'IS'
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    60
     C**N60*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE 'GI'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'GI'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg GI: "Carrier to set payment at fair and reasonable
     C*                rate."
      *
     C     DUUNTS        MULT      SVALOW        XXMRA
      *
      *    Uses CACHRG to pay in full in calculation
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C     CACHRG        COMP      XXMRA                              60
     C   60              Z-ADD     XXMRA         XPTOTA
     C  N60              Z-ADD     CACHRG        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   ENDIF                                                  END DURVCD
     C*
      ****************************************************************
     C*
     C     DURATE        IFEQ      'OUT'
     C     CACHRG        MULT      .90           XXMRA
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C*
     C                   END                                                    END DURATE
     C*
     C*
     C* If Code is found in the file and it has an MRA amount, pay
     C*
     C*
     C     DUUNTS        IFGT      1
     C     SVALOW        MULT(H)   DUUNTS        XXDIEM
     C                   ELSE
     C                   Z-ADD     SVALOW        XXDIEM
     C                   END
     C*
     C*
     C***  RADIOLOGY CPT4 CODES FOR CAT SCANS AND MRI CODES
     C***    HAVE BEEN ADDED TO THE SERVICE CODE MASTER FILE
     C***    WITH THESE CODES PAY THE LESSER OF BILLED CHARGES
     C***    VS. MRA (WHICH IS FEE SCHEDULE) AMOUNT
     C***
     C                   SUBST     DURATE:1      XXRAD7            1            RADIOLOGY CPT4
     C     XXRAD7        IFEQ      '7'
     C     CAFDAT        IFLT      970801                                       ADD 03/27/96
     C     XXDIEM        MULT      XXRADI        XXDIEM                         RADI CONV   sion
     C                   ENDIF                                                  ADD 03/27/96
     C                   ENDIF
     C     XXRAD7        IFEQ      '7'
     C     CACHRG        ANDLT     XXDIEM
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   ELSE                                                   ELSE XXRAD7
     C*
     C*
     C     CACHRG        IFLE      XXDIEM
     C     XXDIEM        SUB       CACHRG        XPDIFF
     C                   Z-ADD     *ZEROS        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     CACHRG        XPBRTC
     C*
     C                   ELSE
     C                   Z-ADD     *ZEROS        XPDIFF
     C     CACHRG        SUB       XXDIEM        XPTOTD
     C                   Z-ADD     XXDIEM        XPTOTA
     C                   Z-ADD     XXDIEM        XPTOTP
     C                   Z-ADD     XXDIEM        XPBRTC
     C*
     C                   END                                                    END CACHRG
     C                   END                                                    END XXRAD7
     C*
     C     DURATE        IFEQ      'MISC'
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   END
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
     C*** BEG ADD *** 05/10/07 ***************************************
     C     XXFLG         OREQ      'N'
     C     XPEOMB        ANDEQ     *BLANK
     C     XPTOTA        ANDGT     *ZEROS
     C*
     C                   MOVE      'Y'           XXFLG
     C*** END ADD *** 05/10/07 ***************************************
     C*
     C*** BEG ADD *** 02/18/05 ***************************************
     C******************** MOVE 'F  '     XPEOMB
     C                   MOVE      'W1 '         XPEOMB
     C*** END ADD *** 02/18/05 ***************************************
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** IF REDUCED BY TO .90 USE 'M' INSTEAD OF 'F' FOR EOMB
     C***
     C     DURATE        IFEQ      'IR'
     C     DURATE        OREQ      'OUT'
     C*
     C*** BEG ADD *** 02/18/05 ***************************************
     C******************** MOVE 'M  '     XPEOMB
     C                   MOVE      'W10'         XPEOMB
     C*** END ADD *** 02/18/05 ***************************************
     C*** EOMB M:   "REDUCED TO FAIR AND REASONABLE"
     C                   END
     C                   END
     *** BEG ADD *** 09/21/98 *****************************************
     C* Call subroutine to write EOMB to FADEP
     C     XPEOMB        IFNE      *BLANKS
      *
     C                   EXSR      EOMB
      *
     C                   ENDIF                                                  XPEOMB NE BLANK
     C* If a reduction was made and no EOMB code assigned then write
     C* a message for the user to assign an EOMB code.
     C     XPTOTP        IFNE      CACHRG
     C     XPEOMB        ANDEQ     *BLANKS
      *
     C                   MOVE      'BU'          XXMSG#
     C                   EXSR      MSG
      *
     C                   ENDIF                                                  XPEOMB NE BLANK
     *** END ADD *** 09/21/98 *****************************************
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM ON HOSPITAL.
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    ANDEQ*ZERO
     C**********           MOVE DURVCD    XXSVCD
     C**********           MOVE DURATE    XXCPT4
     C**********           MOVE DUUNTS    XXUNTS
     C**********
     C********** CAKYO     CHAINPCA0O                54
     C**********
     C********** DUPCL2    TAG
     C**********
     C********** *IN54     IFEQ '0'
     C**********
     C********** CADSEQ    IFNE OXDSEQ
     C********** CADSEQ ** OREQ PXDSEQ
     C********** CAEOB# ** ANDNEPXEOB#
     C*
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    61
     C**N61*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '03'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
      *** BEG CHG *** 06/09/08 ****************************************
     C********** DUKY1     CHAINPDU02                54
      *
     C********** *IN54     DOWEQ'0'
     C     DUKY1         CHAIN     PDU02                              56
      *
     C     *IN56         DOWEQ     '0'
      *** END CHG *** 06/09/08 ****************************************
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
     C                   MOVE      '03'          XXMSG#
     C                   EXSR      MSG
     C*** BEG CHG 06/09/08 ********************************************
     C******************** MOVE *ON       *IN54
     C                   MOVE      *ON           *IN56
     C*** END CHG 06/09/08 ********************************************
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C                   END
      *** BEG DEL *** 09/21/98 ****************************************
     C*******************  ELSE
     C*******************
     C***********CAKYO***  READEPCA0O                    54
     C**N54**************  GOTO DUPCL2
      *** END DEL *** 09/21/98 ****************************************
     C                   END
      *** BEG ADD *** 09/21/98 ****************************************
     C*** BEG CHG 06/09/08 ********************************************
     C**N54****  DUKY1     READEPDU02                    54            3
     C  N56DUKY1         READE     PDU02                                  56                3
     C*** END CHG 06/09/08 ********************************************
     C*
     C                   ENDDO                                                  *IN56 DOWEQ
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** ONLY PROCESS THE RECORD IF NOT A OVERRIDE TYPE.
     C*
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAKY1A    CHAINPCA01                54
      *** END DEL *** 09/21/98 ****************************************
     C     CSOVRD        IFNE      'Y'
      *** BEG ADD *** 09/21/98 ****************************************
     C     CAEOB#        ANDEQ     QQEOB#
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CACHK#    ANDEQ*ZERO
     C********** CAEOB#    ANDEQ*ZERO
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTP        CAPAID
     C                   Z-ADD     XPDIFF        CADIFF
     C                   Z-ADD     XPBRTC        CABRTC
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
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XCMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
      *** BEG CHG *** 09/21/98 ****************************************
     C**N54*************** UPDATPCA01
     C                   UPDATE    PCA13
      *** END CHG *** 09/21/98 ****************************************
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CLEAR THE WORK SAVE FIELDS.
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZERO         XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE         ENDSR
      *** BEG DEL *** 09/21/98 ****************************************
     O**********PCA01   E                RCA01
      *** END DEL *** 09/21/98 ****************************************
