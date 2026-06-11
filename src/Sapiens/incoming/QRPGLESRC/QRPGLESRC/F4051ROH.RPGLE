     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
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
     H*                 TO ZEROS INSTEAD OF CATOTC (TOTAL CHARGED)
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
     FFADCL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMSVL1OH  IF   E           K DISK
     FFTABLOH   IF   E           K DISK
     FFDCAL7    UF   E           K DISK
     FFDCALA    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0A)
     FFMP1L     IF   E           K DISK
     F*

     D*
     D QQS             S              5    DIM(30)                              DETAIL SEQ #
     D QQC             S              2    DIM(30)                              ERROR MESSAGES
     D QQF             S              6    DIM(50)                              FROM DATES
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
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*** BEG ADD *** 10/14/97 **************************************
     D*  TYPE OF BILL
     D                 DS
     D  DC10A2                 1     10
     D  XXTOB                  7      9

     I*
     I*  RENAME THE DUPLICATE FIELD NAMES.
     I*
     IPCA0A
     I              CACPT4                      XACPT4
     I              CACO#                       XACO#
     I              CASUB#                      XASUB#
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
     I              CAICD9                      XAICD9
     I              CAANMN                      XAANMN
     I              CAUNTS                      XAUNTS
     I              CATOTA                      XATOTA
     I              CATOTC                      XATOTC
     I              CATOTD                      XATOTD
     I              CATOTP                      XATOTP
     I              CADSEQ                      XADSEQ
     I              CADELF                      XADELF
     I              CAFLAG                      XAFLAG
     I              CASTAT                      XASTAT
     I              CAP1                        XAP1
     I              CAPMOD                      XAPMOD
     I              CAUSER                      XAUSER
     I              CAPGM                       XAPGM
     I              CADT                        XADT
     I              CATIME                      XATIME
     I              CACHK#                      XACHK#
     I              CAEOB#                      XAEOB#
     I              CAPART                      XAPART
     I              CAEOMB                      XAEOMB
     I              CARCFL                      XARCFL
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

     C*** END ADD *** 10/14/97 **************************************
     C*
     C*  Diagnosis code/Detail charge temp file
     C     FADKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    CADCS#
     C*
     C*  CLAIM DETAIL FILE
     C     CAKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
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
     C                   KFLD                    SVCHK#
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
     C                   KFLD                    HLCHK#
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
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQFDT           150
     C                   PARM                    QQFDT2          150
     C                   PARM                    QQS
     C                   PARM                    QQC
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9 0
     C                   PARM                    QQACDT            8 0          CHG 11/11/97
     C                   PARM                    QQSPCD            3
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
     C                   Z-ADD     *ZEROS        XXOVRP            3 2
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
     C     *LIKE         DEFINE    CAUNTS        SVUNTS                         5.2
     C     *LIKE         DEFINE    CSKEY         TABKEY                         10
     C     *LIKE         DEFINE    CACPT4        XPCPT4                         5
     C     *LIKE         DEFINE    CAEOMB        XPEOMB                         3
     C     *LIKE         DEFINE    CAPMOD        XPPMOD                         2
     C     *LIKE         DEFINE    CATOTA        XPTOTA                         9.2
     C     *LIKE         DEFINE    CATOTD        XPTOTD                         9.2
     C     *LIKE         DEFINE    CATOTP        XPTOTP                         9.2
     C     *LIKE         DEFINE    CACPT4        XXCPT4                         5
     C     *LIKE         DEFINE    CAFDAT        XXDATE                         7.0
     C     *LIKE         DEFINE    CASVCD        XXSVCD                         4.0
     C     *LIKE         DEFINE    CAUNTS        XXUNTS                         5.2
     C     *LIKE         DEFINE    CAFDAT        XXYMD                          7.0
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
     C                   MOVEA     QQFDT         QQF(1)
     C                   MOVEA     QQFDT2        QQF(26)
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
     C** DO THE ADJUDICATION FOR EACH POSSIBLE DATE ENTERED IF AFTER 01/01/01
     C*
     C                   DO        50            S
     C                   MOVE      QQF(S)        XXDATE                         CHG 11/11/97  MOVEL
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C     QQF(S)        IFNE      *ZERO
     C     XXYMD         ANDGE     970301
     *** BEG ADD *** 04/30/98 *****************************************
     C     XXYMD         ANDLT     980301
     *** END ADD *** 04/30/98 *****************************************
     C     CAKY1         CHAIN     PCA01                              54
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     CACPT4        XPCPT4
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
     C*
     C*+*+*+*+*+*+* Do Loop for Each Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
     C                   EXCEPT    RCA01
     C*
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q                 3 0
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '79'          QQC(Q)
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
     C                   END                                                    END EQ *ZEROS
     C                   END                                                    END GT SYDATE
     C* ==================================================================
     C* EOMB # 125,
     C* If the Date of service is 2years or more after the carrier rcvdessage
     C* date, disallow item.
     C*
     C     FADKY         CHAIN     PDC01                              60
     C     *IN60         IFEQ      '0'
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
     C     DCCRDT        IFLT      XXPAST
     C                   MOVE      '125'         XPEOMB
     C* EOMB Code 125: This Charge has been paid per Reasonable & Customary
     C                   Z-ADD     *ZEROS        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       Get next record
     C                   END                                                    END GT SYDATE
     C                   END                                                    END *IN60   E
     C*
     C*** BEGIN ADD *** 08/07/97 **************************************
     C*** CALL ICD9 EDIT PROGRAM
     C                   MOVEL     CAICD9        QQICD9
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
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '344'         XPEOMB
     C* EOMB Code 344: "Payment is denied as the ICD9 code is invalid" e"
     C*
     C     CACHK#        IFEQ      *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FW'          QQC(Q)
     C* Err Msg FW: "Invalid ICD9 Code, send to UR"
     C                   END
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ENDIF
     C*
     C* CHECK WHETHER ICD9 CODE HAS DISALLOWED STATUS
     C     QQGRP         IFEQ      'G'
     C     QQSTS         ANDEQ     'DA'
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '270'         XPEOMB
     C* EOMB Code 270: "Payment is denied as this ICD9 code has been   e"
     C*    disallowed, there is no proof of relationship to injury"
     C*
     C     CACHK#        IFEQ      *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FX'          QQC(Q)
     C* Err Msg FX: "Diag unrelated, send to UR"
     C                   END
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
     C     CACHK#        IFEQ      *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'GG'          QQC(Q)
     C* Err Msg GG: "Billed diagnosis has blank BWC status in Case Mgt"
     C                   ENDIF
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
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   MOVE      '276'         XPEOMB
     C* EOMB Code 276: "Payment is denied as the billed diagnosis is   e"
     C*    not allowed in this claim"
     C*
     C     CACHK#        IFEQ      *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FY'          QQC(Q)
     C* Err Msg FY: "Billed Diag is not allowed on claim, send to UR"
     C                   END
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
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '97'          QQC(Q)                         INVALID CLM STS CODE
     C*** Err Msg 97: "Invalid Claim Status Code, See Programmer"
     C                   END
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
     C     CAEOB#        IFEQ      *ZERO
     C*
     C     CAKYA         CHAIN     PCA0A                              54
     C*
     C     DUPCL3        TAG
     C*
     C     *IN54         IFEQ      '0'
     C*
     C     CASVCD        IFEQ      XASVCD
     C     CACPT4        ANDEQ     XACPT4
     C*
     C     CAPMOD        IFEQ      XAPMOD
     C     CAFDAT        ANDEQ     XAFDAT
     C     CADSEQ        IFNE      XADSEQ
     C     CADSEQ        OREQ      XADSEQ
     C     CAEOB#        ANDNE     XAEOB#
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C*** BEGIN DEL *** 12/15/97 **************************************
     C*   REMOVAL OF EOB 97, 99 -- REQUEST OF CLIENT
     C*********
     C*********  XATOTP    IFGT *ZERO
     C*********  XATOTC    IFEQ CATOTC
     C*********  XAUNTS    ANDEQCAUNTS
     C*********  XAPART    ANDEQCAPART
     C*********  XAPMOD    ANDEQCAPMOD
     C*********            MOVE '099'     XPEOMB
     C**EOMB*Code 099: Payment is denied, BWC's records indicate
     C*********        this is an exact duplicate."
     C*********            Z-ADD*ZERO     XPTOTA
     C****BEGIN CHG *** 08/11/97 **************************************
     C**********           Z-ADD*ZERO     XPTOTD
     C*********            Z-ADDCATOTC    XPTOTD
     C****BEGIN CHG *** 08/11/97 **************************************
     C*********            Z-ADD*ZERO     XPTOTP
     C*********            ELSE
     C*********            MOVE '097'     XPEOMB
     C**EOMB*Code 097: Payment is denied, this is a duplicate of a
     C*********        previously paid service."
     C*********            Z-ADD*ZERO     XPTOTA
     C****BEGIN CHG *** 08/11/97 **************************************
     C**********           Z-ADD*ZERO     XPTOTD
     C*********            Z-ADDCATOTC    XPTOTD
     C****BEGIN CHG *** 08/11/97 **************************************
     C*********            Z-ADD*ZERO     XPTOTP
     C*********            ENDIF                           CATOTC
     C*********            ENDIF
     C*** END DEL ***** 12/15/97 **************************************
     C                   END
     C                   END                                                    EQ CAPMOD
     C                   END                                                    EQ CACPT4
     C                   END
     C*
     C     CAKYA         READE     PCA0A                                  54
     C  N54              GOTO      DUPCL3
     C                   END
     C                   END
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C*** IF OVERRIDE TYPE WE MUST STILL OUTPUT THE ALLOWED/DISALLOWED ETC.
     C     CAEOB#        IFEQ      *ZERO
     C     CATOTD        ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CATOTP        ANDEQ     0
     C*
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CATOTC        MULT      XXOVRP        CATOTA
     C                   END
     C*
     C     CATOTC        SUB       CATOTA        CATOTD
     C                   Z-ADD     CATOTA        CATOTP
     C                   END
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XCMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
     C                   UPDATE    PCA01
     C                   END
     C*
     C                   END
     C*
     C     NXTRCD        TAG
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
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     CACPT4        XPCPT4
     C                   MOVE      CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C                   END
     C*
     C                   END
     C*
     C                   END
     C                   END
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
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             XCMRA             9 2
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
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
     C                   Z-ADD     CASVCD        XXRVCD
     C*** BEGIN CHG *** 08/25/97 **************************************
     C*   WE ONLY NEED THE REVENUE CODES TO MATCH, NOT THE ASSOCIATED
     C*   CPT4 CODES.
     C*********            MOVE CACPT4    XXHCPC
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
     C     CATOTC        SUB       XPTOTA        XPTOTD
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
     C*********  CAIPOP    WHEQ '1'
     C     XXTOB         WHENGE    '111'
     C     XXTOB         ANDLE     '115'
     C*** END CHG ***** 10/14/97 **************************************
     C     CATOTC        MULT      .79           XPTOTA
     C* OUTPATIENT
     C*** BEGIN CHG *** 10/14/97 **************************************
     C*********  CAIPOP    WHEQ '2'
     C     XXTOB         WHENGE    '131'
     C     XXTOB         ANDLE     '135'
     C*** END CHG ***** 10/14/97 **************************************
     C     CATOTC        MULT      .68           XPTOTA
     C* OTHER - NOT PAY
     C*
     C*** BEGIN CHG *** 10/14/97 **************************************
     C*   USING TYPE OF BILL TO CALCULATE RATHER THAN PLACE OF SERVICE
     C*********  CAIPOP    WHNE '1'
     C*********  CAIPOP    ANDNE'2'
     C*
     C     XXTOB         WHENGE    '135'
     C     XXTOB         ORLE      '111'
     C     XXTOB         ORGE      '116'
     C     XXTOB         ANDLE     '130'
     C*** END CHG ***** 10/14/97 **************************************
     C     CATOTC        MULT      .00           XPTOTA
     C*** BEGIN CHG *** 08/04/97 **************************************
     C                   MOVE      '145'         XPEOMB
     C*** EOMB 145: "Payment is denied, the place of service is invalid"
     C*
     C***********          MOVE '201'     XPEOMB
     C*** EOMB 201: "These services are not reimbursable under the "
     C***********   "workers compensation program. "
     C*
     C*** END CHG ***** 08/04/97 **************************************
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FO'          QQC(Q)                         INVALID CLM STS CODE
     C*** Err Msg FO: "Invalid Place of Serice"
     C                   END
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
     C*********  CAIPOP    WHEQ '1'
     C     XXTOB         WHENGE    '111'
     C     XXTOB         ANDLE     '115'
     C*** END CHG ***** 10/14/97 **************************************
     C     CATOTC        MULT      .75           XPTOTA
     C* OUTPATIENT
     C*** BEGIN CHG *** 10/14/97 **************************************
     C*********  CAIPOP    WHEQ '2'
     C     XXTOB         WHENGE    '131'
     C     XXTOB         ANDLE     '135'
     C*** END CHG ***** 10/14/97 **************************************
     C     CATOTC        MULT      .68           XPTOTA
     C* OTHER - NOT PAY
     C*** BEGIN CHG *** 10/14/97 **************************************
     C*   USING TYPE OF BILL TO CALCULATE RATHER THAN PLACE OF SERVICE
     C*
     C*********  CAIPOP    WHGT '2'
     C*
     C     XXTOB         WHENGE    '135'
     C     XXTOB         ORLE      '111'
     C     XXTOB         ORGE      '116'
     C     XXTOB         ANDLE     '130'
     C*** END CHG ***** 10/14/97 **************************************
     C     CATOTC        MULT      .00           XPTOTA
     C*** BEGIN CHG *** 08/04/97 **************************************
     C                   MOVE      '145'         XPEOMB
     C*** EOMB 145: "Payment is denied, the place of service is invalid"
     C*
     C***********          MOVE '201'     XPEOMB
     C*** EOMB 201: "These services are not reimbursable under the "
     C***********   "workers compensation program. "
     C*
     C*** END CHG ***** 08/04/97 **************************************
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'FO'          QQC(Q)                         INVALID CLM STS CODE
     C*** Err Msg FO: "Invalid Place of Serice"
     C*
     C                   END
     C                   ENDSL
     C                   ENDIF
     C*
     C* EOMB # 394 -- DETERMINING COVERAGE FOR PLACE OF SERVICE
     C* DEPENDING ON INPATIENT OR OUTPATIENT.
     C*
     C* INPATIENT
     C     CAIPOP        IFEQ      '1'
     C     SVFL01        ANDEQ     'N'
     C* OUTPATIENT
     C     CAIPOP        OREQ      '2'
     C     SVFL02        ANDEQ     'N'
     C*
     C                   MOVE      '394'         XPEOMB
     C*** EOMB 394: "Payment is denied as revenue code is not covered
     C***           "by BWC. "
     C*
     C                   Z-ADD     *ZEROS        XPTOTA
     C                   ENDIF
     C*
     C     CATOTC        SUB       XPTOTA        XPTOTD
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
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM ON HOSPITAL.
     C*
     C     CAEOB#        IFEQ      *ZERO
     C*
     C     CAKYA         CHAIN     PCA0A                              54
     C*
     C     DUPCL2        TAG
     C*
     C     *IN54         IFEQ      '0'
     C*
     C     CASVCD        IFEQ      XASVCD
     C     CACPT4        ANDEQ     XACPT4
     C*
     C     CAPMOD        IFEQ      XAPMOD
     C     CAFDAT        ANDEQ     XAFDAT
     C     CADSEQ        IFNE      XADSEQ
     C     CADSEQ        OREQ      XADSEQ
     C     CAEOB#        ANDNE     XAEOB#
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C*
     C*** BEGIN DEL *** 12/15/97 **************************************
     C*********
     C*********  XATOTP    IFGT *ZERO
     C*********  XATOTC    IFEQ CATOTC
     C*********  XAUNTS    ANDEQCAUNTS
     C*********  XAPART    ANDEQCAPART
     C*********  XAPMOD    ANDEQCAPMOD
     C*********            MOVE '099'     XPEOMB
     C**EOMB*Code 099: Payment is denied, BWC's records indicate
     C*********        this is an exact duplicate."
     C*********            Z-ADD*ZERO     XPTOTA
     C****BEGIN CHG *** 08/11/97 **************************************
     C**********           Z-ADD*ZERO     XPTOTD
     C*********            Z-ADDCATOTC    XPTOTD
     C****BEGIN CHG *** 08/11/97 **************************************
     C*********            Z-ADD*ZERO     XPTOTP
     C*********            ELSE
     C*********            MOVE '097'     XPEOMB
     C**EOMB*Code 097: Payment is denied, this is a duplicate of a
     C*********        previously paid service."
     C*********            Z-ADD*ZERO     XPTOTA
     C****BEGIN CHG *** 08/11/97 **************************************
     C**********           Z-ADD*ZERO     XPTOTD
     C*********            Z-ADDCATOTC    XPTOTD
     C****BEGIN CHG *** 08/11/97 **************************************
     C*********            Z-ADD*ZERO     XPTOTP
     C*********            ENDIF                           CATOTC
     C*********            ENDIF
     C*** END DEL ***** 12/15/97 **************************************
     C                   END
     C                   END                                                    EQ CAPMOD
     C                   END                                                    EQ CACPT4
     C                   END
     C*
     C     CAKYA         READE     PCA0A                                  54
     C  N54              GOTO      DUPCL2
     C                   END
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** ONLY PROCESS THE RECORD IF NOT A OVERRIDE TYPE.
     C*
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     CSOVRD        IFNE      'Y'
     C     CACHK#        ANDEQ     *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CATOTD
     C                   Z-ADD     XPTOTP        CATOTP
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
