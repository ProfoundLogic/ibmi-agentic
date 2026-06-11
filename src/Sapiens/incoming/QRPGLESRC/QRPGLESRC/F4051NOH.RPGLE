     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4051ROH                                          *
     H*    DESCRIPTION: ADJUDICATION (03/01/97 RULES)                 *
     H*   This program is called from F0070R (Claim Adjudication)     *
     H*   and will perform the actual adjudication of the detail      *
     H*   claim records written by F0070R.                            *
     H*                                                               *
     H*  This Sub-program is intended to be used to calculate the     *
     H*  amount that is due a provider of services, by the carrier    *
     H*  of the Workers' Compensation insurance.                      *
     H*                                                               *
     H*  It should be used for any service provided on or after       *
     H*  03/01/97 and before 03/01/98.                                *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*                                                               *
     H*  06/15/99 C:79 PROJ: OH011
     H*                Modified to work with Provider Bills
     H*                                                               *
     H*  04/08/99 C:117 Support: MC095 Seq# 17568
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs
     H*                 and changed all occurences of UPCO#, UPSUB#,
     H*                 UPFND to the above mentioned fields.
     H*
     H*  09/04/98 C:31  Project: 95MAL
     H*                 Ntwk Disc:Lesser of FS/Chr
     H*                 1. Add update of CAMRAA
     H*
     H*****************************************************************
     H*  NEW COMMENTS HERE
     H*****************************************************************
     H*
     H*  04/30/98 C:79  Project: OH010
     H*                 ADD ENDING EFFECTIVE DATE RANGE OF 03/01/98
     H*
     H*  02/27/98 C:53  Project: MC145
     H*                 BASED REMOVAL OF EOB CODES ON WCMS SELF INSURED
     H*                 FLAG OR INSTALL FLAGS
     H*
     H*  12/15/97 C:59  Project: FL011 CLIENT REQUEST
     H*                 ALLOW PAYMENT ON EOB'S 97, 99, 100, 101. REMOVE
     H*                 EOB.  CHANGE ERROR MSG FOR EOB 524 TO ALLOW
     H*                 WITH ERROR MESSAGE, "A Consultation has been
     H*                 paid to this provider".
     H*
     H*  11/11/97 C:53  Project: MC145
     H*                 YEAR 2000
     H*
     H*  10/14/97 C:59  Project: MC087/8979
     H*                 CHG EDIT TO USE TYPE OF BILL INSTEAD OF
     H*                 PLACE OF SERVICE.
     H*                 PAYS AS FOLLWS:       IN STATE, OUT OF STATE
     H*                    1.  TOB = 111-115     79%        75%
     H*                    2.  TOB = 131-135     68%        68%
     H*
     H*
     H*  08/04/97 C:59  Project: OH009 PHASE 3
     H*                 REMOVE ALL PDS EOMB CODES 001, 201, 202
     H*
     H*  08/07/97 C:32  Project: 87MBY CO# 2
     H*                 ADD EDITS FOR DIAGNOSIS CODE BY ADDING CALL
     H*                 TO PGM F4200ROH (NEED TO CHECK FOR INVALID
     H*                 ICD9 CODES AND GROUP EDIT)
     H*
     H*  08/11/97 C:59  Project: OH009
     H*                 CORRECTION TO 99 AND 97 EOMBS, DENY WAS SET
     H*                 TO ZEROS INSTEAD OF CACHRG (TOTAL CHARGED)
     H*
     H*  08/19/97 C:32  Project: 87MBY  CO#3
     H*                 CHG EDIT FOR DIAGNOSIS EOB 276
     H*
     H*  08/25/97 C:59  Project: MC087/8474
     H*                 CHG EDIT TO ONLY CHECK FOR REVENUE CODES
     H*                 NOT ASSOCIATED CPT4 IN THE ENTRY.
     H*                 REVENUE CODES WILL ONLY MATCH.
     H*****************************************************************
     H*  NEW COMMENTS AT TOP
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMSVL1OH  IF   E           K DISK
     FFTABLOH   IF   E           K DISK
     FFDCALH    UF   E           K DISK
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0A)
     FFMP1L     IF   E           K DISK
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
     FFAEUL     IF   E           K DISK
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
     IPCA0A
     I              CACO#                       XACO#
     I              CAFND                       XAFND
     I              CAFYR                       XAFYR
     I              CACASE                      XACASE
     I              CACSCD                      XACSCD
     I              CAFDAT                      XAFDAT
     I              CATDAT                      XATDAT
     I              CARDAT                      XARDAT
     I              CAIPOP                      XAIPOP
     I              CAFEIN                      XAFEIN
     I              CAMOD                       XAMOD
     I              CAPRTY                      XAPRTY
     I              CATRCD                      XATRCD
     I              CASVCD                      XASVCD
     I              CAPMOD                      XAPMOD
     I              CACPT4                      XACPT4
     I              CAICD9                      XAICD9
     I              CATOTA                      XATOTA
     I              CAEOMB                      XAEOMB
     I              CADSEQ                      XADSEQ
     I              CADELF                      XADELF
     I              CAFLAG                      XAFLAG
     I              CACHK#                      XACHK#
     I              CAEOB#                      XAEOB#
     I              CAPART                      XAPART
     I              CAP1                        XAP1
     I              CASS#                       XASS#
     I              CAACDT                      XAACDT
     I              CASTAT                      XASTAT
     I              CAUSER                      XAUSER
     I              CAPGM                       XAPGM
     I              CATIME                      XATIME
     I              CADT                        XADT
     I              CAIUSR                      XAIUSR
     I              CAIDT                       XAIDT
     I              CADXR                       XADXR
     I              CADCS#                      XADCS#
     I              CALTR#                      XALTR#
     I              CARCFL                      XARCFL
     I              CASUB#                      XASUB#
     I              CAOTHR                      XAOTHR
     I              CANDIS                      XANDIS
     I              CARDTP                      XARDTP
     I              CABRSV                      XABRSV
     I              CABRPM                      XABRPM
     I              CABRPC                      XABRPC
     I              CABRAM                      XABRAM
     I              CABRUN                      XABRUN
     I              CABRTC                      XABRTC
     I              CAZIPA                      XAZIPA
     I              CAAST                       XAAST
     I              CADIFF                      XADIFF
     I              CAIPGM                      XAIPGM
     I              CACHRG                      XATOTC
     I              CAMRA                       XATOTD
     I              CAOTH2                      XAOTH2
     I              CAPAID                      XATOTP
     I              CARCN                       XARCN
     I              CALIN#                      XALIN#
     I              CARULE                      XARULE
     I              CAOT1C                      XAOT1C
     I              CAOT2C                      XAOT2C
     I              CAMRAA                      XAMRAA
     I              CAITIM                      XAITIM
     I              CAMNAA                      XAMNAA
     I              CAUNTS                      XAANMN
     I*

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
     C     CAKYA         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
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
     C*  PROVIDER AUXILIARY FILE
     C     P1KY          KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C*
     C*  DUMMY KEY USED FOR RELEASE OF ALL CA RECORDS
     C*
     C     DUMMY         KLIST
     C                   KFLD                    QQCO#
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
     C                   KFLD                    DUUNTS
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
     C*
     C     CAKYO         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    KKDSEQ
     C*
     C     EUKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
      *
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9 0
     C                   PARM                    QQACDT            8 0          CHG 11/11/97
     C                   PARM                    QQSPCD            3
     C                   PARM                    QQEOB#            9 0
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
     C***************************************************************
     C* INITIALIZATION SECTION
     C                   Z-ADD     *ZEROS        S                 3 0
     C                   Z-ADD     *ZEROS        Q                 3 0
     C*** BEGIN CHG *** 09/25/01 **************************************
     C**********           Z-ADD*ZEROS    XXOVRP  32
     C                   Z-ADD     *ZEROS        XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C                   MOVE      *BLANKS       SRDTE             8 0          CHG 11/11/97
     C                   MOVE      *BLANKS       SRERR             1
     C                   MOVE      *BLANKS       SRTERM            1
     C                   Z-ADD     0             SRPST             5 0
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0          CHG 11/11/97
     C**
     C*
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ                         5.0
     C     *LIKE         DEFINE    CAFEIN        HLFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         HLMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        HLPRTY                         1
     C     *LIKE         DEFINE    CAEOB#        HLEOB#                         9.0
     C     *LIKE         DEFINE    CACHK#        HLCHK#                         7.0
     C     *LIKE         DEFINE    CACHK#        SVCHK#                         7.0
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ                         5.0
     C     *LIKE         DEFINE    CAEOB#        SVEOB#                         9.0
     C     *LIKE         DEFINE    CAFEIN        SVFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SVMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        SVPRTY                         1
     C     *LIKE         DEFINE    DUUNTS        SVUNTS                         5.2
     C     *LIKE         DEFINE    CSKEY         TABKEY                         10
     C     *LIKE         DEFINE    DURATE        XPCPT4                         5
     C     *LIKE         DEFINE    CAEOMB        XPEOMB                         3
     C     *LIKE         DEFINE    CAPMOD        XPPMOD                         2
     C     *LIKE         DEFINE    CATOTA        XPTOTA                         9.2
     C     *LIKE         DEFINE    CAMRA         XPTOTD                         9.2
     C     *LIKE         DEFINE    CAPAID        XPTOTP                         9.2
     C     *LIKE         DEFINE    DURATE        XXCPT4                         5
     C     *LIKE         DEFINE    CAFDAT        XXDATE                         7.0
     C     *LIKE         DEFINE    DURVCD        XXSVCD                         4.0
     C     *LIKE         DEFINE    DUUNTS        XXUNTS                         5.2
     C     *LIKE         DEFINE    CAFDAT        XXYMD                          7.0
     C     *LIKE         DEFINE    CARCN         XXRCN
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MGMSEQ        XXMSEQ
     C     *LIKE         DEFINE    CAEOB#        XXEOB#
     C     *LIKE         DEFINE    CAACDT        XXACDT
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    EUBLTY        XXTOB
     C*
     C     *LIKE         DEFINE    CAFDAT        KKYMD
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    DEESRC        KKESRC
     C*
     C*********  *LIKE     DEFN CAFDAT    WKFDAT           DEL 11/11/97
     C*
     C     *LIKE         DEFINE    CAFEIN        SXFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SXMOD                          3.0
     C*
     C***************************************************************
     C*
     C* CHANGES MADE PER YVONNE
     C***        QQRTCD    CABEQ'P'       TERM             PRE-OPEN FILES
     C*
     C                   MOVE      *ZEROS        QQC
     C                   MOVE      *ZEROS        QQS
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
     C* Chain to the header file to get the bill type
     C     EUKY          CHAIN     PEU01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   EUBLTY        XXTOB
     C                   ENDIF
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
     C                   EXCEPT    RCA01
     C*
     C     CAFDAT        IFGE      970301
     C     CAFDAT        ANDLT     980301
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
     C                   MOVE      CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C*** BEGIN ADD *** 02/27/98 **************************************
     C* CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C*
     C                   CALL      'FA701R'
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FND             3
     C                   PARM      ' '           ##OUTS            1
      *   CALL API WA900R TO DETERMINE FUND TYPE
      *
     C                   MOVEL     *BLANK        @@RCTD            1
     C                   MOVEL     CACO#         @@CO#
     C                   MOVEL     CASUB#        @@SUB#
     C                   MOVEL     CAFND         @@FND
     C                   MOVEL     *BLANKS       @@TYPE
     C*
     C*** THE VALUE OF 'SI' IN @@TYPE WILL ALLOW THE ENTRY OF AN EOB CODE
     C*** OR ELSE THE EOB CODE WILL BE BLANK
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C     ##OUTS        ANDNE     'Y'
     C                   CALL      'WA900R'
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@TYPE            2
     C     @@RTCD        IFEQ      'F'
     C                   MOVE      'SI'          @@TYPE
     C                   ENDIF
     C                   ELSE
     C                   MOVE      'SI'          @@TYPE
     C                   ENDIF
     C*** END   ADD *** 02/27/98 **************************************
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '79'          QQC(Q)
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
     C                   END                                                    END GT SYDATE
     C* ==================================================================
     C* EOMB # 125,
     C* If the Date of service is 2years or more after the carrier rcvdessage
     C* date, disallow item.
     C*
     C* FIND DATE 2 years prior
     C                   Z-ADD     730           SRPST
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKFDAT            8 0          CHG 11/11/97
     C                   Z-ADD     WKFDAT        SRDTE
     C                   CALL      'SRPAST8'                                    CHG 11/11/97
     C                   PARM                    SRDTE
     C                   PARM                    SRPST
     C                   PARM                    SRERR
     C****                 PARM           SRTERM           CHG 11/11/97
     C*
     C                   Z-ADD     SRDTE         XXPST8            8 0          CHG 11/11/97
     C                   Z-ADD     XXPST8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPAST            7 0          CHG 11/11/97
     C     EOCRDT        IFLT      XXPAST
     C                   MOVE      '125'         XPEOMB
     C* EOMB Code 125: This Charge has been paid per Reasonable & Customary
     C                   Z-ADD     *ZEROS        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       Get next record
     C                   END                                                    END GT SYDATE
     C*
     C*** BEGIN ADD *** 08/07/97 ********** ***************************
     C*** CALL ICD9 EDIT PROGRAM
     C                   MOVEL     EUDIA1        QQICD9
     C                   MOVE      'P'           QQPRSC
     C                   MOVE      'A'           Q2CALL
     C                   MOVE      'P'           QQSYS
     C                   MOVE      *BLANKS       QQSTYP
     C                   MOVE      *BLANKS       QQVLD
     C                   MOVE      *BLANKS       QQGRP
     C                   MOVE      *BLANKS       QQERR
     C                   MOVE      *BLANKS       QQSTS
     C                   MOVE      CACO#         Q1CO#             3
     C                   MOVE      CASUB#        Q1SUB
     C                   MOVE      CAFND         Q1FND
     C                   MOVE      CAFYR         Q2FYR
     C                   MOVE      CACASE        Q2CASE
     C                   CALL      'F4200ROH'                           99
     C                   PARM                    QQICD9            5            IDC9 CODE
     C                   PARM                    QQPRSC            1            PRIMARY/SECOND
     C                   PARM                    Q2CALL            1            CALLED PGM CODE
     C                   PARM                    QQSYS             1            SYSTEM (P/H)
     C                   PARM                    QQSTYP            1            INPAT/OUTPAT
     C                   PARM                    QQVLD             1            VALID/INVALID
     C                   PARM                    QQGRP             1            GROUP COVERAGE
     C                   PARM                    QQERR             2            Ennn/Vnnn ICD9
     C                   PARM                    QQSTS             2            CLAIM STATUS
     C                   PARM                    Q1CO#             3            CO #
     C                   PARM                    Q1SUB             5            SUB #
     C                   PARM                    Q1FND             3            FUND
     C                   PARM                    Q2FYR             2            FUND YEAR
     C                   PARM                    Q2CASE            7            CASE#
     C*
     C* CHECK WHETHER ICD9 CODE HAS BEEN FLAGGED AS BWC INVALID
     C*   ONLY FOR CLIENTS WITH PROGRAM F4200ROH
     C*
     C     *IN99         IFEQ      '0'
     C     QQVLD         IFEQ      'I'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '344'         XPEOMB
     C* EOMB Code 344: "Payment is denied as the ICD9 code is invalid" e"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FW'          QQC(Q)
     C* Err Msg FW: "Invalid ICD9 Code, send to UR"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ENDIF
     C*
     C* CHECK WHETHER ICD9 CODE HAS DISALLOWED STATUS
     C     QQGRP         IFEQ      'G'
     C     QQSTS         ANDEQ     'DA'
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '270'         XPEOMB
     C* EOMB Code 270: "Payment is denied as this ICD9 code has been   e"
     C*    disallowed, there is no proof of relationship to injury"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FX'          QQC(Q)
     C* Err Msg FX: "Diag unrelated, send to UR"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 08/19/97 **************************************
     C*
     C* CHECK WHETHER ICD9 CODE IS ON CLAIM BUT BWC STATUS IS BLANK
     C     QQGRP         IFEQ      'G'
     C     QQSTS         ANDEQ     '  '
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GG'          QQC(Q)
     C* Err Msg GG: "Billed diagnosis has blank BWC status in Case Mgt"
     C                   ENDIF
     C*
     C*** END   ADD *** 08/19/97 **************************************
     C*
     C* CHECK WHETHER ICD9 CODE HAS DISALLOWED STATUS
     C     QQGRP         IFEQ      'X'
     C*** BEGIN DEL *** 08/19/97 **************************************
     C*********  QQGRP     OREQ 'G'
     C*********  QQSTS     ANDNE'DA'
     C*********  QQSTS     ANDNE'AL'
     C*** END   DEL *** 08/19/97 **************************************
     C                   Z-ADD     CACHRG        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '276'         XPEOMB
     C* EOMB Code 276: "Payment is denied as the billed diagnosis is   e"
     C*    not allowed in this claim"
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FY'          QQC(Q)
     C* Err Msg FY: "Billed Diag is not allowed on claim, send to UR"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 08/07/97 **************************************
     C* ==================================================================
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
     C     CAKYO         CHAIN     PCA0A                              55
      *
     C     *IN55         IFEQ      '0'
     C     CAFDAT        ANDEQ     XAFDAT
     C     CAFEIN        ANDEQ     XAFEIN
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
     C                   Z-ADD     0             XPTOTA
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
     C                   ENDIF                                                  CAFDAT IFGE
     C*
     C     NXTRCD        TAG
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
     C  N54              EXCEPT    RCA01
     C*
     C* Write adjudication messages to FDMGP
     C                   EXSR      MSG
     C*
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
     C                   Z-ADD     HLDSEQ        CADSEQ                         SAVE FIELD VALS
     C                   MOVE      HLFEIN        CAFEIN                         FOR RECORD WE
     C                   MOVE      HLMOD         CAMOD                          ARE PROCESSING
     C                   MOVE      HLPRTY        CAPRTY
     C                   Z-ADD     HLEOB#        CAEOB#
     C                   Z-ADD     HLCHK#        CACHK#
     C*** REGET RECORD.
     C     CAKY1Z        CHAIN     PCA01                              54
     C*
     C  N54              EXCEPT    RCA01
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
     C* THIS CODE IS HERE JUST FOR A DUMMY READ OF FMSVL1OH
     C*
     C                   Z-ADD     DURVCD        XXRVCD
     C*** BEGIN CHG *** 08/25/97 **************************************
     C*   WE ONLY NEED THE REVENUE CODES TO MATCH, NOT THE ASSOCIATED
     C*   CPT4 CODES.
     C*********            MOVE DURATE    XXHCPC
     C                   MOVE      *BLANKS       XXHCPC
     C*** END CHG ***** 08/25/97 **************************************
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
     C                   END                                                    END *IN60
     *********************************************************************
     C*  INVALID CODE, DOESN'T EXIST IN PSV01 PAY NOTHING WITH ERROR
     C*
     C     SVCMSV        IFNE      XXCMSV
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '98'          QQC(Q)                         INVALID CLM STS CODE
     C*** Err Msg 98: "Invalid poocedure code, see data processing'
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C*** BEGIN CHG *** 02/27/98 **************************************
     C     @@TYPE        IFEQ      'SI'
     C                   MOVE      '001'         XPEOMB
     C                   ELSE
     C                   MOVE      '   '         XPEOMB
     C                   ENDIF
     C*** END   CHG *** 02/27/98 **************************************
     C*** EOMB : ""
     C***
     C*** EOMB 001: "Service is not reimburseable under the Workers'
     C************   Compensation Program."
     C*** END CHG ***** 08/04/97 **************************************
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C*
     C                   ENDIF
     C*************************************************************************
     C*
     C     P1KY          SETGT     PP101
     C                   READP     PP101                                  61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     P1PST         XX1ST             2
     C                   ELSE
     C                   MOVEL     '  '          XX1ST
     C                   ENDIF
     C* PAYMENT SCHEDULE DETERMINED BY IN-STATE, OUT-STATE, INPATIENT,OUTPATIENT
     C*
     C     XX1ST         IFEQ      'OH'
     C                   SELECT
     C* IN-STATE
     C* INPATIENT
     C*** BEGIN CHG *** 10/14/97 **************************************
     C*   USING TYPE OF BILL INSTEAD OF PLACE OF SERVICE
     C*
     C*********  DUPOS     WHEQ '1'
     C     XXTOB         WHENGE    '111'
     C     XXTOB         ANDLE     '115'
     C*** END CHG ***** 10/14/97 **************************************
     C     CACHRG        MULT      .79           XPTOTA
     C* OUTPATIENT
     C*** BEGIN CHG *** 10/14/97 **************************************
     C*********  DUPOS     WHEQ '2'
     C     XXTOB         WHENGE    '131'
     C     XXTOB         ANDLE     '135'
     C*** END CHG ***** 10/14/97 **************************************
     C     CACHRG        MULT      .68           XPTOTA
     C* OTHER - NOT PAY
     C*
     C*** BEGIN CHG *** 10/14/97 **************************************
     C*   USING TYPE OF BILL TO CALCULATE RATHER THAN PLACE OF SERVICE
     C*********  DUPOS     WHNE '1'
     C*********  DUPOS     ANDNE'2'
     C*
     C     XXTOB         WHENGE    '135'
     C     XXTOB         ORLE      '111'
     C     XXTOB         ORGE      '116'
     C     XXTOB         ANDLE     '130'
     C*** END CHG ***** 10/14/97 **************************************
     C     CACHRG        MULT      .00           XPTOTA
     C*** BEGIN CHG *** 08/04/97 **************************************
     C                   MOVE      '145'         XPEOMB
     C*** EOMB 145: "Payment is denied, the place of service is invalid"
     C*
     C***********          MOVE '201'     XPEOMB
     C*** EOMB 201: "These services are not reimbursable under the "
     C***********   "workers compensation program. "
     C*
     C*** END CHG ***** 08/04/97 **************************************
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FO'          QQC(Q)                         INVALID CLM STS CODE
     C*** Err Msg FO: "Invalid Place of Serice"
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C* OUT OF STATE
     C* INPATIENT
     C*** BEGIN CHG *** 10/14/97 **************************************
     C*   USING TYPE OF BILL INSTEAD OF PLACE OF SERVICE
     C*
     C*********  DUPOS     WHEQ '1'
     C     XXTOB         WHENGE    '111'
     C     XXTOB         ANDLE     '115'
     C*** END CHG ***** 10/14/97 **************************************
     C     CACHRG        MULT      .75           XPTOTA
     C* OUTPATIENT
     C*** BEGIN CHG *** 10/14/97 **************************************
     C*********  DUPOS     WHEQ '2'
     C     XXTOB         WHENGE    '131'
     C     XXTOB         ANDLE     '135'
     C*** END CHG ***** 10/14/97 **************************************
     C     CACHRG        MULT      .68           XPTOTA
     C* OTHER - NOT PAY
     C*** BEGIN CHG *** 10/14/97 **************************************
     C*   USING TYPE OF BILL TO CALCULATE RATHER THAN PLACE OF SERVICE
     C*
     C*********  DUPOS     WHGT '2'
     C*
     C     XXTOB         WHENGE    '135'
     C     XXTOB         ORLE      '111'
     C     XXTOB         ORGE      '116'
     C     XXTOB         ANDLE     '130'
     C*** END CHG ***** 10/14/97 **************************************
     C     CACHRG        MULT      .00           XPTOTA
     C*** BEGIN CHG *** 08/04/97 **************************************
     C                   MOVE      '145'         XPEOMB
     C*** EOMB 145: "Payment is denied, the place of service is invalid"
     C*
     C***********          MOVE '201'     XPEOMB
     C*** EOMB 201: "These services are not reimbursable under the "
     C***********   "workers compensation program. "
     C*
     C*** END CHG ***** 08/04/97 **************************************
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FO'          QQC(Q)                         INVALID CLM STS CODE
     C*** Err Msg FO: "Invalid Place of Serice"
     C*
     C                   ENDSL
     C                   ENDIF
     C*
     C* EOMB # 394 -- DETERMINING COVERAGE FOR PLACE OF SERVICE
     C* DEPENDING ON INPATIENT OR OUTPATIENT.
     C*
     C* INPATIENT
     C     DUPOS         IFEQ      '1'
     C     SVFL01        ANDEQ     'N'
     C* OUTPATIENT
     C     DUPOS         OREQ      '2'
     C     SVFL02        ANDEQ     'N'
     C*
     C                   MOVE      '394'         XPEOMB
     C*** EOMB 394: "Payment is denied as revenue code is not covered
     C***           "by BWC. "
     C*
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   ENDIF
     C*
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C*
     C*
     C*
     C* MUST UPDATE RECORD HERE
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
     C     XPTOTP        ANDGT     *ZEROS
     C     XPEOMB        ANDEQ     *BLANK
     C*** BEGIN CHG *** 08/04/97 **************************************
     C*** BEGIN CHG *** 02/27/98 **************************************
     C     @@TYPE        IFEQ      'SI'
     C                   MOVE      '202'         XPEOMB
     C                   ELSE
     C                   MOVE      '   '         XPEOMB
     C                   ENDIF
     C*** END   CHG *** 02/27/98 **************************************
     C*** EOMB : ""
     C***
     C************         MOVE '202'     XPEOMB
     C*** EOMB 202: "Charges exceed Maximum Allowance found in reimburse manual"
     C*** END CHG ***** 08/04/97 **************************************
     C                   END
     C*
     C     XPEOMB        IFNE      *BLANKS
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
     C     CAKYO         CHAIN     PCA0A                              55
      *
     C     *IN55         IFEQ      '0'
     C     CAFDAT        ANDEQ     XAFDAT
     C     CAFEIN        ANDEQ     XAFEIN
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
     OPCA01     E            RCA01
