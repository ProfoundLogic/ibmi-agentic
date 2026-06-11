     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE N9910R - NETWORK INFORMATION REPORT - PRT PGM
     H*****************************************************************
     H*
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*  08    Batch Mode                                             *
     H*  09    Demand Mode                                            *
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H*  11/02/09 C:232 Project: 139WMR
     H*                 OFAC Reporting-Add Pending Status Indicator.
     H*
     H*  10/03/06 C:62  REQ/PROJ: 71WHI
     H*                 ADDED PRINTING FOR D9 TYPE CODE
     H*                 BACK TO BASE
     H*
     H*  07/10/02 C:62  Project: NW074
     H*                 ADD 'DJ' ALGORITHM
     H*
     H*  05/23/01 C:62  Project: 114MAP
     H*                 ADD 'DG' ALGORITHM
     H*
     H*  11/11/99 C:31  Project: 48MEA
     H*                  N3 billing algorithms
     H*                 1. Add N3 billing algorithm to print
     H*
     H*  02/17/00 C:31  Project: NWMNT
     H*                 NTWK Maintenance
     H*                 1. Add heading if no routing page
     H*                 2. Change dates to print in 8.0 format.
     H*
     H*  05/18/99 C:31  Project: SY031
     H*                 1999 Users conference
     H*                 1. Compile only
     H*
     H*  02/19/99 C:31  Project: 95MAR
     H*                 DC discount changes
     H*                 1. Change reinbursement amount entry
     H*
     H*  08/20/98 C:31  Project: 48MBP
     H*                 DB Discount Alogrithm
     H*                 1. 'DB' Code support added
     H*
     H*  05/29/98 C:31  Project: 48MAY
     H*                 Create 'DC' Discount
     H*                 1. Add 'DC' code to output
     H*                 2. Corrected BDCOM1 overlay for D1 - D7 lines
     H*
     H*  05/21/98 C:31  Project: 51MCT
     H*                 CorVel Network
     H*                 1. Add DETDF for 'DF' code output
     H*
     H*  04/30/98 C:31  Project: 51MCM
     H*                 P.E.N Network
     H*                 1. Add DETDD for 'DD' code output
     H*
     H*  02/19/98 C:62  REQ/PROJ: MC145
     H*                 ** CHANGED NMPHL & NMHSL TO FMS4L2/L3.
     H*
     H*  01/15/98 C:31  Project: 60MNS
     H*                 Anthem Network
     H*                 1. Add 'DE' support
     H*
     H*  03/07/95 C:20  REQ/PROJ: NW007
     H*                 1. ADDED PRINTING FIR D7 TYPE CODE
     H*
     H*  03/10/94 C:20  REQ/PROJ: 98/NW001
     H*                 1. ADDED PRINTING OF NETWORK CODE
     H*                 AND NAME TO THE DISCOUNT SECTION
     H*
     H*  03/15/93 C:20  PROJ: 99AAE
     H*                 1. CHANGED CODE TO USE NMBDLB WHEN
     H*                    GETTING GROUP BILLING CODE
     H*                 2. CHANGED CODE TO USE NMBDLD WHEN
     H*                    GETTING PROVIDER DISCOUNT CODE
     H*
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FNMBDLD    IF   E           K DISK
     F                                     RENAME(PBD01:PBD02)
     FNMBDLB    IF   E           K DISK
     F*** BEGIN CHG *** 02/19/98 **************************************
     F********** NMHSL   IF  E           K        DISK
     F********** NMPHL   IF  E           K        DISK
     FFMS4L2    IF   E           K DISK
     FFMS4L3    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     F*** END   CHG *** 02/19/98 **************************************
     FNMNWL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  FSY                    1     10
     D                                     DIM(10) ASCEND                       GROUP VALID SYS
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
     D                                     DIM(10) ASCEND                       GROUP VALID BRCD
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
     D*** BEGIN ADD *** 10/03/06 **************************************
     D                 DS
     D  XXCOM2                 1     60
     D  XXCM02                 1     50
     D  XXSRCE                51     51
     D  XXLESS                52     52
     D  XXEXTR                53     60
     D*** END   ADD *** 10/03/06 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEGIN ADD *** 02/17/00 **************************************
     D HEAD01          C                   CONST('NETWORK INFORMATION -
     D                                     REPORT')

     I*** END ADD *** 02/17/00 ****************************************
     I*
     I*

     C*
     C*   Company Master File
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C* PROVIDER MASTER FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*   Output Routing File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*   User Parameter File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* KEYLIST FOR SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C* KEYLIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*  BILILNG CODE TABLE FILE
     C*
     C     BDKEY         KLIST
     C                   KFLD                    XXCODE
     C                   KFLD                    XXEFDT
     C*
     C* KEYLIST FOR PROVIDER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*** BEGIN DEL *** 02/19/98 **************************************
     C*
     C*  PROVIDER FILE NMPHL - PHYSICIANS ONLY
     C*
     C********** PHKEY     KLIST
     C**********           KFLD           XXFEIN
     C**********           KFLD           XXMOD
     C**********           KFLD           XXNWCD           ADD 03/10/94
     C*
     C*  PROVIDER FILE NMHSL - HOSPITALS ONLY
     C*
     C********** HSKEY     KLIST
     C**********           KFLD           XXFEIN
     C**********           KFLD           XXMOD
     C**********           KFLD           XXNWCD           ADD 03/10/94
     C*** END   DEL *** 02/19/98 **************************************
     C*** BEGIN ADD *** 02/19/98 **************************************
     C*
     C*  PROVIDER NETWORK CONTRACT.
     C*
     C     S4KEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXNWCD                         ADD 03/10/94
     C*** END   ADD *** 02/19/98 **************************************
     C*
     C*  PPO NETWORK MASTER FILE
     C*
     C     NWKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXNWCD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQSYS             1
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQNWCD            5            ADD 03/10/94
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
     C*** BEGIN CHG *** 02/17/00 **************************************
     C********             PARM '*MDY'    SRTFMT
     C                   PARM      *BLANKS       SRTFMT
     C*** END CHG *** 02/17/00 ****************************************
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
     C     *LIKE         DEFINE    SMCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    SMSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    FNFND         XXFND                          3.0N
     C     *LIKE         DEFINE    PDFEIN        XXFEIN                         9.0N
     C     *LIKE         DEFINE    PDMOD         XXMOD                          3.0N
     C     *LIKE         DEFINE    ORFIL#        XXPRT                          1A
     C     *LIKE         DEFINE    BDCODE        XXCODE                         3A
     C     *LIKE         DEFINE    BDEFDT        XXEFDT                         7.0N
     C     *LIKE         DEFINE    FNSY01        XXSYS                          1A
     C     *LIKE         DEFINE    S4DISC        XXDTCD                         3A
     C     *LIKE         DEFINE    S4ECFL        XXECFL                         1A
     C     *LIKE         DEFINE    S4MDFL        XXMDFL                         1A
     C     *LIKE         DEFINE    S4FSFL        XXFSFL                         1A
     C     *LIKE         DEFINE    S4NWCD        XXNWCD                         5A  ADD 03/10/94
     C     *LIKE         DEFINE    NWNAME        XWNAME                         40A ADD 03/10/94
     C                   Z-ADD     *ZEROS        XXPC25            1 0          1.0N
     C                   Z-ADD     *ZEROS        XXPC01            4 1          4.1N
     C                   Z-ADD     *ZEROS        XXPC02            4 1          4.1N
     C                   Z-ADD     *ZEROS        XXPC03            4 1          4.1N
     C                   Z-ADD     *ZEROS        XXPC04            4 1          4.1N
     C                   Z-ADD     *ZEROS        XXPC05            4 1          4.1N
     C                   Z-ADD     *ZEROS        XXPC06            4 1          4.1N
     C                   Z-ADD     *ZEROS        XXPC07            4 1          4.1N
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQSYS         XXSYS
     C                   MOVE      QQFEIN        XXFEIN
     C                   MOVE      QQMOD         XXMOD
     C                   MOVE      QQNWCD        XXNWCD                         ADD 03/10/94
     C*
     C                   TIME                    DSDTTM
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   GET THE USER PARAMETERS FILE.
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C*  Get the distribution information
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
     C*** BEGIN ADD *** 02/17/00 **************************************
     C   60              MOVEL     HEAD01        ORTITL
     C*** END ADD *** 02/17/00 ****************************************
     C*
     C*  Print the distribution information
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********  N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C* GET THE SUBSCRIBER INFORMATION
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    HDSUB
     C                   END
     C*
     C* GET THE GROUP INFORMATION
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     FNEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C*** BEGIN CHG *** 02/17/00 **************************************
     C********             MOVELSRDTO     XGEFDT  60
     C                   MOVEL     SRDTO         XGEFDT            8 0
     C*** END CHG *** 02/17/00 ****************************************
     C*
     C                   Z-ADD     1             X                 5 0          ##
     C     XXSYS         LOOKUP    FSY(X)                                 60    ##
     C     *IN60         IFEQ      '1'                                          ##
     C                   MOVE      *BLANKS       XXCODE
     C                   MOVEL     FBR(X)        XXCODE
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBD1        TAG
     C                   READP     PBD01                                  60
     C     *IN60         IFEQ      '0'
     C     BDCODE        ANDEQ     XXCODE                                       CHG 04/28/92
     C     BDSTAT        IFNE      'I'
     C                   Z-ADD     BDPC25        XXPC25
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C*** BEGIN CHG *** 02/17/00 **************************************
     C********             MOVELSRDTO     XBEFDT  60
     C                   MOVEL     SRDTO         XBEFDT            8 0
     C*** END CHG *** 02/17/00 ****************************************
     C*
     C                   EXCEPT    HDFND
     C     BDTYPE        IFEQ      'H1'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C                   EXCEPT    DETH1
     C                   END
     C     BDTYPE        IFEQ      'H2'
     C                   EXCEPT    DETH2
     C                   END
     C     BDTYPE        IFEQ      'H3'
     C                   EXCEPT    DETH3
     C                   END
     C     BDTYPE        IFEQ      'N1'
     C     BDPC01        MULT      100           XXPC01
     C                   EXCEPT    DETN1
     C                   END
     C     BDTYPE        IFEQ      'N2'
     C     BDPC01        MULT      100           XXPC01
     C                   EXCEPT    DETN2
     C                   END
     C*** BEGIN ADD *** 11/11/99 **************************************
     C     BDTYPE        IFEQ      'N3'
     C     BDPC01        MULT      100           XXPC01
     C                   EXCEPT    DETN3
     C                   END
     C*** END ADD *** 11/11/99 ****************************************
     C     BDTYPE        IFEQ      'P1'
     C                   EXCEPT    DETP1
     C                   END
     C     BDTYPE        IFEQ      'P2'
     C                   EXCEPT    DETP2
     C                   END
     C     BDTYPE        IFEQ      'P3'
     C                   EXCEPT    DETP3
     C                   END
     C     BDTYPE        IFEQ      'U1'
     C                   EXCEPT    DETU1
     C                   END
     C                   ELSE
     C                   GOTO      NXTBD1
     C                   END
     C                   END
     C                   END                                                    ##
     C                   END                                                    ##
     C*
     C* GET THE PROVIDER INFORMATION
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    ANDNE'I'
     C     PDSTAT        ANDEQ     'A'
     C     *IN60         OREQ      '0'
     C     PDSTAT        ANDEQ     *BLANKS
     C*** END CHG *** 11/02/09 **************************************
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C     NWKEY         CHAIN     PNW01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     NWNAME        XWNAME
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C                   MOVE      *BLANKS       XWNAME
     C                   END                                                    END *IN60 EQ '0'
     C     PDTYPE        IFEQ      '4'
     C*** BEGIN CHG *** 02/19/98 **************************************
     C********** HSKEY     CHAINPHS01                60
     C     S4KEY         CHAIN     PS401                              60
     C     *IN60         IFEQ      '0'
     C*
     C**********           Z-ADDHSCEDT    SRDTI
     C                   Z-ADD     S4BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C*** BEGIN CHG *** 02/17/00 **************************************
     C********             MOVELSRDTO     XXFRDT  60
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C*** END CHG *** 02/17/00 ****************************************
     C*
     C**********           Z-ADDHSCEDT    XXEFDT
     C                   Z-ADD     S4BGDT        XXEFDT
     C*
     C**********           Z-ADDHSTMDT    SRDTI
     C     S4ENDT        IFEQ      0
     C                   Z-ADD     0             XXTODT
     C                   ELSE                                                   ENDT = 0
     C                   Z-ADD     S4ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C*** BEGIN CHG *** 02/17/00 **************************************
     C********             MOVELSRDTO     XXTODT  60
     C                   MOVEL     SRDTO         XXTODT            8 0
     C*** END CHG *** 02/17/00 ****************************************
     C                   ENDIF                                                  ENDT = 0
     C*
     C**********           MOVE HSECFL    XXECFL
     C**********           MOVE HSMDFL    XXMDFL
     C**********           MOVE HSFSFL    XXFSFL
     C**********           MOVELHSDTCD    XXDTCD
     C**********           MOVELHSDTCD    XXCODE    P
     C                   MOVE      S4ECFL        XXECFL
     C                   MOVE      S4MDFL        XXMDFL
     C                   MOVE      S4FSFL        XXFSFL
     C                   MOVEL     S4DISC        XXDTCD
     C                   MOVEL(P)  S4DISC        XXCODE
     C*** END   CHG *** 02/19/98 **************************************
     C     BDKEY         SETGT     PBD02                                        ADD 03/15/93
     C     NXTBD2        TAG
     C                   READP     PBD02                                  60    ADD 03/15/93
     C     *IN60         IFEQ      '0'
     C     BDCODE        ANDEQ     XXCODE                                       CHG 04/28/92
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C*** BEGIN CHG *** 02/17/00 **************************************
     C********             MOVELSRDTO     XPEFDT  60
     C                   MOVEL     SRDTO         XPEFDT            8 0
     C*** END CHG *** 02/17/00 ****************************************
     C*
     C     BDSTAT        IFNE      'I'
     C                   EXCEPT    HDPRO
     C*** BEGIN ADD *** 05/29/98 **************************************
     C     BDTYPE        IFEQ      'DC'
     C     BDPC01        MULT      100           XXPC01
     C*** BEGIN ADD *** 02/19/99 **************************************
     C     BDPC02        MULT      100           XXPC02
     C     BDPC03        MULT      100           XXPC03
     C*** END ADD *** 02/19/99 ****************************************
     C                   EXCEPT    DETDC
     C                   ENDIF                                                  TYPE = 'DC'
     C*** END ADD *** 05/29/98 ****************************************
     C*** BEGIN ADD *** 04/30/98 **************************************
     C     BDTYPE        IFEQ      'DD'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C                   MOVE      'N'           XXFL03            1
     C     BDPC03        IFEQ      1
     C                   MOVE      'Y'           XXFL03
     C                   ENDIF                                                  PC03 = 1
     C     BDPC04        MULT      100           XXPC04
     C     BDPC05        MULT      100           XXPC05
     C                   EXCEPT    DETDD
     C                   ENDIF                                                  TYPE = 'DD'
     C*** END ADD *** 04/30/98 ****************************************
     C*** BEGIN ADD *** 05/21/98 **************************************
     C     BDTYPE        IFEQ      'DF'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C     BDPC03        MULT      100           XXPC03
     C     BDPC04        MULT      100           XXPC04
     C                   EXCEPT    DETDF
     C                   ENDIF                                                  TYPE = 'DF'
     C*** END ADD *** 05/21/98 ****************************************
     C     BDTYPE        IFEQ      'D1'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C     BDPC03        MULT      100           XXPC03
     C                   EXCEPT    DETD1
     C                   END
     C     BDTYPE        IFEQ      'D2'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C     BDPC03        MULT      100           XXPC03
     C     BDPC04        MULT      100           XXPC04
     C     BDPC05        MULT      100           XXPC05
     C     BDPC06        MULT      100           XXPC06
     C     BDPC07        MULT      100           XXPC07
     C                   EXCEPT    DETD2
     C                   END
     C     BDTYPE        IFEQ      'D3'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C     BDPC03        MULT      100           XXPC03
     C     BDPC04        MULT      100           XXPC04
     C                   EXCEPT    DETD3
     C                   END
     C     BDTYPE        IFEQ      'D4'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C     BDPC03        MULT      100           XXPC03
     C                   EXCEPT    DETD4
     C                   END
     C     BDTYPE        IFEQ      'D5'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C     BDPC03        MULT      100           XXPC03
     C     BDPC04        MULT      100           XXPC04
     C                   EXCEPT    DETD5
     C                   END
     C     BDTYPE        IFEQ      'D7'
     C*** BEGIN ADD *** 01/15/98 **************************************
     C     BDTYPE        OREQ      'DE'
     C*** END ADD *** 01/15/98 ****************************************
     C*** BEGIN ADD *** 05/23/01 **************************************
     C     BDTYPE        OREQ      'DG'
     C*** END   ADD *** 05/23/01 **************************************
     C                   EXCEPT    DETD7
     C                   END
     C*** BEGIN ADD *** 08/20/98 **************************************
     C     BDTYPE        IFEQ      'DB'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C                   EXCEPT    DETDB
     C                   END
     C*** END ADD *** 08/20/98 ****************************************
     C*** BEGIN ADD *** 10/03/06 ***********************************************
     C     BDTYPE        IFEQ      'D9'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C                   MOVE      BDCOM2        XXCOM2
     C                   EXCEPT    DETD9
     C                   END
     C*** END ADD *** 10/03/06 *************************************************
     O*** BEGIN ADD *** 07/10/02 **************************************
     C     BDTYPE        IFEQ      'DJ'
     C     BDPC01        MULT      100           XXPC01
     C                   EXCEPT    DETDJ
     C                   ENDIF
     O*** END   ADD *** 07/10/02 **************************************
     C                   ELSE                                                   ELSE BDSTAT = I
     C                   GOTO      NXTBD2
     C                   END                                                    END BDSTAT <> I
     C                   END                                                    END *IN60
     C                   END                                                    END *IN60
     C                   ELSE
     C     PDTYPE        IFEQ      '1'
     C*** BEGIN CHG *** 02/19/98 **************************************
     C********** PHKEY     CHAINPPH01                60
     C     S4KEY         CHAIN     PS402                              60
     C     *IN60         IFEQ      '0'
     C*
     C**********           Z-ADDPHNEDT    SRDTI
     C                   Z-ADD     S4BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C**********           Z-ADDPHNXDT    SRDTI
     C     S4ENDT        IFEQ      0
     C                   Z-ADD     0             XXTODT
     C                   ELSE                                                   ENDT = 0
     C                   Z-ADD     S4ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT
     C                   ENDIF                                                  ENDT = 0
     C*
     C**********           Z-ADDPHNEDT    XXEFDT
     C                   Z-ADD     S4BGDT        XXEFDT
     C                   MOVE      'N'           XXECFL
     C                   MOVE      'N'           XXMDFL
     C**********           MOVE PHFSFL    XXFSFL
     C**********           MOVELPHDTCD    XXDTCD
     C**********           MOVELPHDTCD    XXCODE
     C                   MOVE      S4FSFL        XXFSFL
     C                   MOVEL     S4DISC        XXDTCD
     C                   MOVEL(P)  S4DISC        XXCODE
     C*** END   CHG *** 02/19/98 **************************************
     C     BDKEY         SETGT     PBD02                                        ADD 03/15/93
     C     NXTBD3        TAG
     C                   READP     PBD02                                  60    ADD 03/15/93
     C     *IN60         IFEQ      '0'
     C     BDCODE        ANDEQ     XXCODE                                       CHG 04/28/92
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XPEFDT
     C*
     C     BDSTAT        IFNE      'I'
     C                   EXCEPT    HDPRO
     C*** BEGIN ADD *** 05/29/98 **************************************
     C     BDTYPE        IFEQ      'DC'
     C     BDPC01        MULT      100           XXPC01
     C*** BEGIN ADD *** 02/19/99 **************************************
     C     BDPC02        MULT      100           XXPC02
     C     BDPC03        MULT      100           XXPC03
     C*** END ADD *** 02/19/99 ****************************************
     C                   EXCEPT    DETDC
     C                   ENDIF                                                  TYPE = 'DC'
     C*** END ADD *** 05/29/98 ****************************************
     C*** BEGIN ADD *** 04/30/98 **************************************
     C     BDTYPE        IFEQ      'DD'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C                   MOVE      'N'           XXFL03
     C     BDPC03        IFEQ      1
     C                   MOVE      'Y'           XXFL03
     C                   ENDIF                                                  PC03 = 1
     C     BDPC04        MULT      100           XXPC04
     C     BDPC05        MULT      100           XXPC05
     C                   EXCEPT    DETDD
     C                   ENDIF                                                  TYPE = 'DD'
     C*** END ADD *** 04/30/98 ****************************************
     C*** BEGIN ADD *** 05/21/98 **************************************
     C     BDTYPE        IFEQ      'DF'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C     BDPC03        MULT      100           XXPC03
     C     BDPC04        MULT      100           XXPC04
     C                   EXCEPT    DETDF
     C                   ENDIF                                                  TYPE = 'DF'
     C*** END ADD *** 05/21/98 ****************************************
     C     BDTYPE        IFEQ      'D1'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C     BDPC03        MULT      100           XXPC03
     C                   EXCEPT    DETD1
     C                   END
     C     BDTYPE        IFEQ      'D2'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C     BDPC03        MULT      100           XXPC03
     C     BDPC04        MULT      100           XXPC04
     C     BDPC05        MULT      100           XXPC05
     C     BDPC06        MULT      100           XXPC06
     C     BDPC07        MULT      100           XXPC07
     C                   EXCEPT    DETD2
     C                   END
     C     BDTYPE        IFEQ      'D3'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C     BDPC03        MULT      100           XXPC03
     C     BDPC04        MULT      100           XXPC04
     C                   EXCEPT    DETD3
     C                   END
     C     BDTYPE        IFEQ      'D4'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C     BDPC03        MULT      100           XXPC03
     C                   EXCEPT    DETD4
     C                   END
     C     BDTYPE        IFEQ      'D5'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C     BDPC03        MULT      100           XXPC03
     C     BDPC04        MULT      100           XXPC04
     C                   EXCEPT    DETD5
     C                   END
     C     BDTYPE        IFEQ      'D7'
     C*** BEGIN ADD *** 01/15/98 **************************************
     C     BDTYPE        OREQ      'DE'
     C*** END ADD *** 01/15/98 ****************************************
     C*** BEGIN ADD *** 05/23/01 **************************************
     C     BDTYPE        OREQ      'DG'
     C*** END   ADD *** 05/23/01 **************************************
     C                   EXCEPT    DETD7
     C                   END
     C*** BEGIN ADD *** 08/20/98 **************************************
     C     BDTYPE        IFEQ      'DB'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C                   EXCEPT    DETDB
     C                   END
     C*** END ADD *** 08/20/98 ****************************************
     C*** BEGIN ADD *** 10/03/06 ***********************************************
     C     BDTYPE        IFEQ      'D9'
     C     BDPC01        MULT      100           XXPC01
     C     BDPC02        MULT      100           XXPC02
     C                   MOVE      BDCOM2        XXCOM2
     C                   EXCEPT    DETD9
     C                   END
     C*** END ADD *** 10/03/06 *************************************************
     O*** BEGIN ADD *** 07/10/02 **************************************
     C     BDTYPE        IFEQ      'DJ'
     C     BDPC01        MULT      100           XXPC01
     C                   EXCEPT    DETDJ
     C                   ENDIF
     O*** END   ADD *** 07/10/02 **************************************
     C                   ELSE                                                   ELSE BDSTAT
     C                   GOTO      NXTBD3
     C                   END                                                    END BDSTAT
     C                   END                                                    END *IN60
     C                   END                                                    END *IN60
     C                   END                                                    END PDTYPE = 4
     C                   END                                                    END PDTYPE = 1
     C                   END                                                    END PDKEY
     C*
     C     END           TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
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
     O**********        09   UPUSNM              67
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
     O          E            ROUTE          2
     O                                           26 'RUN DATE       :'
     O                       DSDATE              38 '  /  /  '
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               58 'INC. 1993 - 1999'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HDSUB          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              97
     O                       DSDATE        Y    127
     O          E            HDSUB          2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          121 'PAGE:'
     O                       PAGE          Z    127
     O          E            HDSUB          1
     O                                           11 'Subscriber:'
     O                       XXSUB#              18
     O                                           20 '-'
     O                       SMNAME              61
     O          E            HDSUB          1
     O                                           29 'Systems subscribed to:'
     O                       SMSY01              32
     O                       SMSY02              35
     O                       SMSY03              38
     O                       SMSY04              41
     O                       SMSY05              44
     O                       SMSY06              47
     O                       SMSY07              50
     O                       SMSY08              53
     O                       SMSY09              56
     O                       SMSY10              59
     O          E            HDSUB          1
     O                                           29 'A/R Routine types....:'
     O                       SMAR01              32
     O                       SMAR02              35
     O                       SMAR03              38
     O                       SMAR04              41
     O                       SMAR05              44
     O                       SMAR06              47
     O                       SMAR07              50
     O                       SMAR08              53
     O                       SMAR09              56
     O                       SMAR10              59
     O          E            HDFND       1
     O                                           11 'Group.....:'
     O                       XXFND               16
     O                                           18 '-'
     O                       FNNAME              79
     O          E            HDFND       1
     O                                           22 'Group Eff Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XGEFDT    32 '  /  /  '
     O                       XGEFDT              34 '  /  /    '
     O*** END CHG *** 02/17/00 ****************************************
     O          E            HDFND       1
     O                                           22 'Rate Code.....:'
     O                       XXCODE              27
     O                                           38 'for system'
     O                       XXSYS               40
     O          E            HDFND       1
     O                                           22 'Rate Type.....:'
     O                       BDTYPE              26
     O          E            HDFND       1
     O                                           22 'Rate Eff Date.:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XBEFDT    32 '  /  /  '
     O                       XBEFDT              34 '  /  /    '
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETH1       2
     O                                           12 'TYPE:'
     O          E            DETH1       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'HBA Billing-(X% of MRA '
     O                                           58 'saved, < $X > $Y) + X% '
     O                                           73 'of other saved.'
     O          E            DETH1       2
     O                                           20 'Billing Code:'
     O                       BDCODE              25
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XBEFDT    54 '  /  /  '
     O                       XBEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETH1       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETH1       1
     O                       BDCOM2              78
     O          E            DETH1       2
     O                                           37 'Bill Both Systems:'
     O                       XXPC25        Z     43
     O          E            DETH1       2
     O                                           38 '1 is to produce only net'
     O                                           59 'work or PBR/HBA Bills'
     O          E            DETH1       1
     O                                           38 '2 is to produce both net'
     O                                           60 'work and PBR/HBA bills'
     O          E            DETH1       2
     O                       XXPC01        M     15
     O                                           17 '%'
     O                                           41 'of MRA savings, not to '
     O                                           47 'exceed'
     O                       BDAM01        K     62
     O                                           63 ','
     O          E            DETH1       1
     O                                           41 'with a minimum charge of'
     O                       BDAM01        K     55
     O                                           56 ','
     O          E            DETH1       1
     O                                           21 'plus'
     O                       XXPC02        M     27
     O                                           46 '% of other savings'
     O***
     O          E            DETH2       2
     O                                           12 'TYPE:'
     O          E            DETH2       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'HBA Billing-$X for 1ST '
     O                                           58 'line, & $X for any addi'
     O                                           70 'tional line.'
     O          E            DETH2       2
     O                                           20 'Billing Code:'
     O                       BDCODE              25
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XBEFDT    54 '  /  /  '
     O                       XBEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETH2       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETH2       1
     O                       BDCOM2              78
     O          E            DETH2       2
     O                                           37 'Bill Both Systems:'
     O                       XXPC25        Z     43
     O          E            DETH2       1
     O                                           38 '1 is to produce only net'
     O                                           59 'work or PBR/HBA Bills'
     O          E            DETH2       1
     O                                           38 '2 is to produce both net'
     O                                           60 'work and PBR/HBA bills'
     O          E            DETH2       2
     O                                           36 'Amount to bill for 1st l'
     O                                           40 'ine:'
     O                       BDAM01        K     55
     O          E            DETH2       1
     O                                           36 'Amount to bill for each '
     O                                           52 'additional line:'
     O                       BDAM02        K     67
     O***
     O          E            DETH3       2
     O                                           12 'TYPE:'
     O          E            DETH3       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'HBA Billing-Flat charge '
     O                                           58 'per bill. ($X.XX per bi'
     O                                           61 'll.'
     O          E            DETH3       2
     O                                           20 'Billing Code:'
     O                       BDCODE              25
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XBEFDT    54 '  /  /  '
     O                       XBEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETH3       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETH3       1
     O                       BDCOM2              78
     O          E            DETH3       2
     O                                           37 'Bill Both Systems:'
     O                       XXPC25        Z     43
     O          E            DETH3       1
     O                                           38 '1 is to produce only net'
     O                                           59 'work or PBR/HBA Bills'
     O          E            DETH3       1
     O                                           38 '2 is to produce both net'
     O                                           60 'work and PBR/HBA bills'
     O          E            DETH3       2
     O                                           34 'Amount:'
     O                       BDAM01        K     49
     O***
     O          E            DETN1       2
     O                                           12 'TYPE:'
     O          E            DETN1       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'Network Billing-X% of ne'
     O                                           57 'twork discount amount.'
     O          E            DETN1       2
     O                                           20 'Billing Code:'
     O                       BDCODE              25
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XBEFDT    54 '  /  /  '
     O                       XBEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETN1       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETN1       1
     O                       BDCOM2              78
     O          E            DETN1       2
     O                                           37 'Bill Both Systems:'
     O                       XXPC25        Z     43
     O          E            DETN1       1
     O                                           38 '1 is to produce only net'
     O                                           59 'work or PBR/HBA Bills'
     O          E            DETN1       1
     O                                           38 '2 is to produce both net'
     O                                           60 'work and PBR/HBA bills'
     O          E            DETN1       2
     O                                           38 'Percent of Network Disco'
     O                                           49 'unt Amount:'
     O                       XXPC01        M     55
     O                                           57 '%'
     O          E            DETN1       1
     O                                           38 'Minimum Billing Amount: '
     O                       BDAM01        K     53
     O***
     O          E            DETN2       2
     O                                           12 'TYPE:'
     O          E            DETN2       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'Network Billing-Other Re'
     O                                           59 'duct. Amt + Network Disc'
     O                                           76 'ount Amount) * X%'
     O          E            DETN2       2
     O                                           20 'Billing Code:'
     O                       BDCODE              25
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XBEFDT    54 '  /  /  '
     O                       XBEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETN2       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETN2       1
     O                       BDCOM2              78
     O          E            DETN2       2
     O                                           37 'Bill Both Systems:'
     O                       XXPC25        Z     43
     O          E            DETN2       2
     O                                           38 '1 is to produce only net'
     O                                           59 'work or PBR/HBA Bills'
     O          E            DETN2       1
     O                                           38 '2 is to produce both net'
     O                                           60 'work and PBR/HBA bills'
     O          E            DETN2       2
     O                                           38 'Percent of (Network Disc'
     O                                           62 'ount Amount + Other Redu'
     O                                           74 'ctions Amt):'
     O                       XXPC01        M     81
     O                                           83 '%'
     O          E            DETN2       1
     O                                           38 'Minimum Billing Amount: '
     O                       BDAM01        K     52
     O*** BEGIN ADD *** 11/11/99 **************************************
     O***
     O          E            DETN3       2
     O                                           12 'TYPE:'
     O          E            DETN3       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'Network Billing-FS reduc'
     O                                           59 'tions Amt + Network Disc'
     O                                           76 'ount Amount) * X%'
     O          E            DETN3       2
     O                                           20 'Billing Code:'
     O                       BDCODE              25
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XBEFDT    54 '  /  /  '
     O                       XBEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETN3       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETN3       1
     O                       BDCOM2              78
     O          E            DETN3       2
     O                                           37 'Bill Both Systems:'
     O                       XXPC25        Z     43
     O          E            DETN3       2
     O                                           38 '1 is to produce only net'
     O                                           59 'work or PBR/HBA Bills'
     O          E            DETN3       1
     O                                           38 '2 is to produce both net'
     O                                           60 'work and PBR/HBA bills'
     O          E            DETN3       2
     O                                           38 'Percent of (Network Disc'
     O                                           62 'ount Amount + FS Reducti'
     O                                           74 'ons Amt):'
     O                       XXPC01        M     81
     O                                           83 '%'
     O          E            DETN3       1
     O                                           38 'Minimum Billing Amount: '
     O                       BDAM01        K     52
     O*** END ADD *** 11/11/99 ****************************************
     O***
     O          E            DETP1       2
     O                                           12 'TYPE:'
     O          E            DETP1       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'PBR Billing-Flat charge '
     O                                           58 'per bill. ($X.XX per bi'
     O                                           61 'll.'
     O          E            DETP1       2
     O                                           20 'Billing Code:'
     O                       BDCODE              25
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XBEFDT    54 '  /  /  '
     O                       XBEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O*** BEGIN CHG *** 02/17/00 **************************************
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETP1       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETP1       1
     O                       BDCOM2              78
     O          E            DETP1       2
     O                                           37 'Bill Both Systems:'
     O                       XXPC25        Z     43
     O          E            DETP1       2
     O                                           38 '1 is to produce only net'
     O                                           59 'work or PBR/HBA Bills'
     O          E            DETP1       1
     O                                           38 '2 is to produce both net'
     O                                           60 'work and PBR/HBA bills'
     O          E            DETP1       2
     O                                           34 'Amount:'
     O                       BDAM01        K     49
     O***
     O          E            DETP2       2
     O                                           12 'TYPE:'
     O          E            DETP2       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'PBR Billing-$X for 1ST '
     O                                           58 'line, & $X for any addi'
     O                                           70 'tional line.'
     O          E            DETP2       2
     O                                           20 'Billing Code:'
     O                       BDCODE              25
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XBEFDT    54 '  /  /  '
     O                       XBEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETP2       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETP2       1
     O                       BDCOM2              78
     O          E            DETP2       2
     O                                           37 'Bill Both Systems:'
     O                       XXPC25        Z     43
     O          E            DETP2       2
     O                                           38 '1 is to produce only net'
     O                                           59 'work or PBR/HBA Bills'
     O          E            DETP2       1
     O                                           38 '2 is to produce both net'
     O                                           60 'work and PBR/HBA bills'
     O          E            DETP2       2
     O                                           36 'Amount to bill for 1st l'
     O                                           40 'ine:'
     O                       BDAM01        K     55
     O          E            DETP2       1
     O                                           36 'Amount to bill for each '
     O                                           52 'additional line:'
     O                       BDAM02        K     67
     O***
     O          E            DETP3       2
     O                                           12 'TYPE:'
     O          E            DETP3       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'PBR Billing-(X% of MRA '
     O                                           58 'saved, < $X > $Y) + X% '
     O                                           73 'of other saved.'
     O          E            DETP3       2
     O                                           20 'Billing Code:'
     O                       BDCODE              25
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XBEFDT    54 '  /  /  '
     O                       XBEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETP3       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETP3       1
     O                       BDCOM2              78
     O          E            DETP3       2
     O                                           37 'Bill Both Systems:'
     O                       XXPC25        Z     43
     O          E            DETP3       2
     O                                           38 '1 is to produce only net'
     O                                           59 'work or PBR/HBA Bills'
     O          E            DETP3       1
     O                                           38 '2 is to produce both net'
     O                                           60 'work and PBR/HBA bills'
     O          E            DETP3       2
     O                       XXPC01        M     15
     O                                           17 '%'
     O                                           41 'of MRA savings, not to '
     O                                           47 'exceed'
     O                       BDAM01        K     62
     O                                           63 ','
     O          E            DETP3       1
     O                                           41 'with a minimum charge of'
     O                       BDAM01        K     55
     O                                           56 ','
     O          E            DETP3       1
     O                                           21 'plus'
     O                       XXPC02        M     27
     O                                           46 '% of other savings'
     O***
     O          E            DETU1       2
     O                                           12 'TYPE:'
     O          E            DETU1       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'Pre-Certification Billi'
     O                                           50 'ng-Flat Charge '
     O                                           73 'per bill. ($X.XX per bi'
     O                                           76 'll.'
     O          E            DETU1       2
     O                                           20 'Billing Code:'
     O                       BDCODE              25
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XBEFDT    54 '  /  /  '
     O                       XBEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETU1       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETU1       1
     O                       BDCOM2              78
     O          E            DETU1       2
     O                                           37 'Bill Both Systems:'
     O                       XXPC25        Z     43
     O          E            DETU1       2
     O                                           38 '1 is to produce only net'
     O                                           62 'work or Pre/Re-cert bill'
     O                                           63 's'
     O          E            DETU1       1
     O                                           38 '2 is to produce both net'
     O                                           62 'work and Pre/Re-Cert bil'
     O                                           64 'ls'
     O          E            DETU1       2
     O                                           34 'Pre-Cert Amount:'
     O                       BDAM01        K     49
     O          E            DETU1       2
     O                                           34 'Re-Cert Amount:'
     O                       BDAM02        K     49
     O***
     O          E            HDPRO       2
     O                                           11 'Provider..:'
     O                       XXFEIN              22
     O                                           24 '-'
     O                       XXMOD               27
     O                       XXLAST              84
     O          E            HDPRO       1
     O                                           11 'Network...:'
     O                       XXNWCD              17
     O                       XWNAME              58
     O          E            HDPRO       1
     O                                           21 'Provider Type:'
     O                       PDTYPE              25
     O          E            HDPRO       1
     O                                           22 'Contract Dates:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XXFRDT    32 '  /  /  '
     O                       XXFRDT              34 '  /  /    '
     O********                           34 '-'
     O                                           36 '-'
     O********                 XXTODT    43 '  /  /  '
     O                       XXTODT              47 '  /  /    '
     O*** END CHG *** 02/17/00 ****************************************
     O          E            HDPRO       1
     O                                           22 'Manual FS Flag:'
     O                       XXFSFL              25
     O          E            HDPRO       1
     O                                           19 'EC Cap Flag:'
     O                       XXECFL              25
     O          E            HDPRO       1
     O                                           19 'MD Cap Flag:'
     O                       XXMDFL              25
     O          E            HDPRO       1
     O                                           21 'Discount Code:'
     O                       XXDTCD              27
     O          E            HDPRO       1
     O                                           21 'Discount Type:'
     O                       BDTYPE              26
     O          E            HDPRO       1
     O                                           21 'Disc Eff Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XPEFDT    32 '  /  /  '
     O                       XPEFDT              34 '  /  /    '
     O*** END CHG *** 02/17/00 ****************************************
     O***
     O*** BEGIN ADD *** 05/29/98 **************************************
     O***
     O          E            DETDC       2
     O                                           12 'TYPE:'
     O          E            DETDC       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           37 'Network Discount: Lesser'
     O                                           61 ' of % off State Fee Sche'
     O                                           85 'dule vs. Billed Charges '
     O          E            DETDC       2
     O                                           21 'Discount Code:'
     O                       BDCODE              26
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XPEFDT    54 '  /  /  '
     O                       XPEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETDC       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETDC       1
     O                       BDCOM2              78
     O*** BEGIN DEL *** 02/19/99 **************************************
     O********E 2              DETDC
     O********                           31 'Percentage off State Fee'
     O********                           55 ' Schedule Reimbursement '
     O********                           62 'Amount:'
     O********                 XXPC01M   68
     O********                           70 '%'
     O*** END DEL *** 02/19/99 ****************************************
     O*** END ADD *** 05/29/98 ****************************************
     O*** BEGIN ADD *** 02/19/99 **************************************
     O          E            DETDC       2
     O                                           29 'In-patient % off State'
     O                                           42 'Fee Schedule'
     O                                           65 'Reimbursement Amount :'
     O                       XXPC01        M     73
     O                                           75 '%'
     O          E            DETDC       2
     O                                           30 'Out-patient % off State'
     O                                           43 'Fee Schedule'
     O                                           65 'Reimbursement Amount:'
     O                       XXPC02        M     73
     O                                           75 '%'
     O          E            DETDC       2
     O                                           31 'Percentage off State Fee'
     O                                           55 ' Schedule Reimbursement '
     O                                           62 'Amount:'
     O                                           29 'Non-listed % off State'
     O                                           42 'Fee Schedule'
     O                                           65 'Reimbursement Amount :'
     O                       XXPC03        M     73
     O                                           75 '%'
     O*** END ADD *** 02/19/99 ****************************************
     O*** BEGIN ADD *** 04/30/98 **************************************
     O***
     O          E            DETDD       2
     O                                           12 'TYPE:'
     O          E            DETDD       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           27 'P.E.N. Network'
     O          E            DETDD       2
     O                                           21 'Discount Code:'
     O                       BDCODE              26
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XPEFDT    54 '  /  /  '
     O                       XPEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETDD       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETDD       1
     O                       BDCOM2              78
     O          E            DETDD       1
     O                                           25 'Conversion Factor:'
     O                       BDAM01        K     40
     O                                           60 'Time Unit Factor:'
     O                       BDAM02        K     75
     O                                           94 'Carve out (Y/N):'
     O                       XXFL03              96
     O          E            DETDD       1
     O                                           17 '% off MRA:'
     O                       XXPC01        M     23
     O                                           36 '% off U&C:'
     O                       XXPC02        M     42
     O          E            DETDD       1
     O                                           29 '% off Billed Inpatient'
     O                                           38 'Charges:'
     O                       XXPC04        M     44
     O                                           70 '% off Billed Outpatient'
     O                                           79 'Charges:'
     O                       XXPC05        M     85
     O*** END ADD *** 04/30/98 ****************************************
     O*** BEGIN ADD *** 05/21/98 **************************************
     O***
     O          E            DETDF       2
     O                                           12 'TYPE:'
     O          E            DETDF       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           27 'CorVel Network'
     O          E            DETDF       2
     O                                           21 'Discount Code:'
     O                       BDCODE              26
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XPEFDT    54 '  /  /  '
     O                       XPEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETDF       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETDF       1
     O                       BDCOM2              78
     O          E            DETDF       1
     O                                           41 'Discount % off fee sched'
     O                                           45 ' . :'
     O                       XXPC01        K     60
     O          E            DETDF       1
     O                                           41 'Billed amount discount %'
     O                                           45 ' . :'
     O                       XXPC02        K     60
     O          E            DETDF       1
     O                                           41 'Inpatient discount % . .'
     O                                           45 ' . :'
     O                       XXPC03        K     60
     O          E            DETDF       1
     O                                           41 'Inpatient bill amt discn'
     O                                           45 't %:'
     O                       XXPC04        K     60
     O*** END ADD *** 05/21/98 ****************************************
     O***
     O          E            DETD1       2
     O                                           12 'TYPE:'
     O          E            DETD1       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'Network Discount: MRA -'
     O                                           58 ' X%, or 80% if U&C - X%'
     O                                           78 ' in pat, Y% out pat.'
     O          E            DETD1       2
     O                                           21 'Discount Code:'
     O                       BDCODE              26
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XPEFDT    54 '  /  /  '
     O                       XPEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETD1       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O*** BEGIN ADD *** 05/29/98 **************************************
     O          E            DETD1       1
     O*** END ADD *** 05/29/98 ****************************************
     O                       BDCOM2              78
     O          E            DETD1       2
     O                                           31 '1) If MRA Method: use MR'
     O                                           37 'A less'
     O                       XXPC01        M     43
     O                                           45 '%'
     O          E            DETD1       1
     O*** BEGIN CHG *** 05/18/99 **************************************
     O********                           31 '2) If U&C Method: use 80'
     O                                           31 '2) If U&C Method: use U&'
     O********                           44 '% of U&C less'
     O                                           48 'C percentile less'
     O********                 XXPC02M   50
     O                       XXPC02        M     54
     O********                           52 '%'
     O                                           56 '%'
     O********                           71 'for in patient, or'
     O                                           75 'for in patient, or'
     O********                 XXPC03M   77
     O                       XXPC03        M     81
     O********                           79 '%'
     O                                           83 '%'
     O********                          105 'for out patient services'
     O                                          109 'for out patient services'
     O*** END CHG *** 05/18/99 ****************************************
     O***
     O          E            DETD2       2
     O                                           12 'TYPE:'
     O          E            DETD2       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'Network Discount: MRA -'
     O                                           58 ' X%, U&C $ amt for date'
     O                                           82 ' range, gives X,Y and Z%'
     O          E            DETD2       2
     O                                           21 'Discount Code:'
     O                       BDCODE              26
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XPEFDT    54 '  /  /  '
     O                       XPEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETD2       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O*** BEGIN ADD *** 05/29/98 **************************************
     O          E            DETD2       1
     O*** END ADD *** 05/29/98 ****************************************
     O                       BDCOM2              78
     O          E            DETD2       2
     O                                           31 '1) If MRA Method: use MR'
     O                                           37 'A less'
     O                       XXPC01        M     43
     O                                           45 '%'
     O          E            DETD2       1
     O                                           31 '2) If U&C Method: Amount'
     O                                           43 ' of U&C from'
     O                                           57 '0'
     O                                           59 '-'
     O                       BDAM01        K     73
     O                                           76 'at'
     O                       XXPC02        M     82
     O                                           84 '%'
     O          E            DETD2       1
     O                       BDAM02        K     57
     O                                           59 '-'
     O                       BDAM03        K     73
     O                                           76 'at'
     O                       XXPC03        M     82
     O                                           84 '%'
     O          E            DETD2       1
     O                       BDAM04        K     57
     O                                           59 '-'
     O                       BDAM05        K     73
     O                                           76 'at'
     O                       XXPC04        M     82
     O                                           84 '%'
     O          E            DETD2       1
     O                       BDAM06        K     57
     O                                           59 '-'
     O                       BDAM07        K     73
     O                                           76 'at'
     O                       XXPC05        M     82
     O                                           84 '%'
     O          E            DETD2       1
     O                       BDAM08        K     57
     O                                           59 '-'
     O                       BDAM09        K     73
     O                                           76 'at'
     O                       XXPC06        M     82
     O                                           84 '%'
     O          E            DETD2       1
     O                       BDAM10        K     57
     O                                           59 '-'
     O                       BDAM11        K     73
     O                                           76 'at'
     O                       XXPC07        M     82
     O                                           84 '%'
     O          E            DETD2       1
     O                                           31 'within'
     O                       BDAM25        K     45
     O                                           68 'months from Providers '
     O                                           82 'effective date'
     O***
     O          E            DETD3       2
     O                                           12 'TYPE:'
     O          E            DETD3       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'Network Discount: MRA -'
     O                                           58 ' X% IP, or Y% OP, or U&'
     O                                           75 'C - X% IP, Y% OP.'
     O          E            DETD3       2
     O                                           21 'Discount Code:'
     O                       BDCODE              26
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XPEFDT    54 '  /  /  '
     O                       XPEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETD3       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O*** BEGIN ADD *** 05/29/98 **************************************
     O          E            DETD3       1
     O*** END ADD *** 05/29/98 ****************************************
     O                       BDCOM2              78
     O          E            DETD3       2
     O                                           31 '1) If MRA Method: use MR'
     O                                           37 'A less'
     O                       XXPC01        M     43
     O                                           45 '%'
     O                                           64 'for in patient, or'
     O                       XXPC02        M     70
     O                                           72 '%'
     O                                           89 'for out patient.'
     O          E            DETD3       1
     O*** BEGIN CHG *** 05/18/99 **************************************
     O********                           31 '2) If U&C Method: use 80'
     O                                           31 '2) If U&C Method: use U&'
     O********                           44 '% of U&C less'
     O                                           48 'C percentile less'
     O********                 XXPC03M   50
     O                       XXPC03        M     54
     O********                           52 '%'
     O                                           56 '%'
     O********                           71 'for in patient, or'
     O                                           75 'for in patient, or'
     O********                 XXPC04M   77
     O                       XXPC04        M     81
     O********                           79 '%'
     O                                           83 '%'
     O********                           96 'for out patient.'
     O                                          100 'for out patient.'
     O*** END CHG *** 05/18/99 ****************************************
     O***
     O          E            DETD4       2
     O                                           12 'TYPE:'
     O          E            DETD4       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'Network Discount: INPA;'
     O                                           58 'MRA - X%, OUT-PAT;MRA-Y'
     O                                           78 '%, non-listed;U&C-Z%'
     O          E            DETD4       2
     O                                           21 'Discount Code:'
     O                       BDCODE              26
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XPEFDT    54 '  /  /  '
     O                       XPEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETD4       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O*** BEGIN ADD *** 05/29/98 **************************************
     O          E            DETD4       1
     O*** END ADD *** 05/29/98 ****************************************
     O                       BDCOM2              78
     O          E            DETD4       2
     O                                           31 '1) For in-patient servic'
     O                                           46 'es use MRA less'
     O                       XXPC01        M     52
     O                                           54 '%'
     O          E            DETD4       1
     O                                           31 '2) For out-patient servi'
     O                                           47 'ces use MRA less'
     O                       XXPC02        M     53
     O                                           55 '%'
     O          E            DETD4       1
     O                                           31 '3) For non-listed(non-Fe'
     O                                           55 'e Schedule) items use U&'
     O                                           61 'C less'
     O                       XXPC03        M     67
     O                                           69 '%'
     O***
     O          E            DETD5       2
     O                                           12 'TYPE:'
     O          E            DETD5       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'Network Discount: Hosp;'
     O                                           58 'MRA - X%, Non-Hosp;MRA-'
     O                                           78 'Y%, Non-Listed;U&C-Z'
     O                                           89 '%, 70540-A%'
     O          E            DETD5       2
     O                                           21 'Discount Code:'
     O                       BDCODE              26
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XPEFDT    54 '  /  /  '
     O                       XPEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETD5       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O*** BEGIN ADD *** 05/29/98 **************************************
     O          E            DETD5       1
     O*** END ADD *** 05/29/98 ****************************************
     O                       BDCOM2              78
     O          E            DETD5       2
     O                                           31 '1) For Hospital services'
     O                                           44 ' use MRA less'
     O                       XXPC01        M     50
     O                                           52 '%'
     O          E            DETD5       1
     O                                           31 '2) For Non-Hospital serv'
     O                                           48 'ices use MRA less'
     O                       XXPC02        M     54
     O                                           56 '%'
     O          E            DETD5       1
     O                                           31 '3) For non-listed(non-Fe'
     O                                           55 'e Schedule) items use U&'
     O                                           61 'C less'
     O                       XXPC03        M     67
     O                                           69 '%'
     O          E            DETD5       1
     O                                           31 'Except code 70540 which '
     O                                           50 'will be at U&C less'
     O                       XXPC04        M     56
     O                                           58 '%'
     O***
     O          E            DETD7       2
     O                                           12 'TYPE:'
     O          E            DETD7       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'Hospital or Provider: C'
     O                                           52 'arve-out per code'
     O          E            DETD7       2
     O                                           21 'Discount Code:'
     O                       BDCODE              26
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XPEFDT    54 '  /  /  '
     O                       XPEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETD7       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O*** BEGIN ADD *** 05/29/98 **************************************
     O          E            DETD7       1
     O*** END ADD *** 05/29/98 ****************************************
     O                       BDCOM2              78
     O          E            DETD7       2
     O                                           25 'Conversion Factor:'
     O                       BDAM01        M     39
     O          E            DETD7       1
     O                                           25 'Time Unit Factor.:'
     O                       BDAM02        M     39
     O          E            DETD7       1
     O                                           31 'For individual code carv'
     O                                           55 'e-out amounts, see Disco'
     O                                           75 'unt Code Maintenance'
     O*** BEGIN ADD *** 08/20/98 **************************************
     O          E            DETDB       2
     O                                           12 'TYPE:'
     O          E            DETDB       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'Hospital or Provider: %'
     O                                           52 'off State Fee Sch'
     O                                           75 'edule, specific CPT-4 F'
     O                                           86 'ee Schedule'
     O          E            DETDB       2
     O                                           21 'Discount Code:'
     O                       BDCODE              26
     O                                           43 'Effective Date:'
     O*** BEGIN CHG *** 02/17/00 **************************************
     O********                 XPEFDT    54 '  /  /  '
     O                       XPEFDT              56 '  /  /    '
     O********                           65 'Status:'
     O                                           67 'Status:'
     O********                 BDSTAT    68
     O                       BDSTAT              70
     O*** END CHG *** 02/17/00 ****************************************
     O          E            DETDB       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETDB       1
     O                       BDCOM2              78
     O          E            DETDB       2
     O                                           31 'for medical services, us'
     O                                           42 'e MRA less '
     O                       XXPC01        M     48
     O                                           50 '%'
     O          E            DETDB       1
     O                                           31 'for unlisted (non-fee sc'
     O                                           55 'hedule) services, use R&'
     O                                           62 'C less '
     O                       XXPC02        M     68
     O                                           70 '%'
     O          E            DETDB       1
     O                                           31 'For specific CPT-4 Fee S'
     O                                           55 'chedule amounts, see Dis'
     O                                           77 'count Code Maintenance'
     O*** END ADD *** 08/20/98 ****************************************
     O*** BEGIN ADD *** 10/03/06 **************************************
     O***
     O          E            DETD9       2
     O                                           12 'TYPE:'
     O          E            DETD9       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'Network Discount: PMD/F'
     O                                           58 'S/CHRGS, +/- %, Lesser '
     O                                           79 'of Charges vs. PMD/FS'
     O          E            DETD9       2
     O                                           21 'Discount Code:'
     O                       BDCODE              26
     O                                           43 'Effective Date:'
     O                       XPEFDT              56 '  /  /    '
     O                                           65 'Status:'
     O                       BDSTAT              68
     O          E            DETD9       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETD9       1
     O                       XXCM02              68
     O          E            DETD9       1
     O                                           14 'Source:'
     O                       XXSRCE              17
     O                                           30 'P = PMD'
     O                                           50 'F = Fee Schedule'
     O                                           72 'B = Billed Charges'
     O          E            DETD9       2
     O                                           12 'Plus:'
     O                       XXPC01        M     20
     O                                           22 '%'
     O                                           34 'Minus:'
     O                       XXPC02        M     42
     O                                           44 '%'
     O          E            DETD9       2
     O                                           31 'Lesser of Billed Charges'
     O                                           42 ' or PMD/FS:'
     O                       XXLESS              46
     O***
     O*** END   ADD *** 10/03/06 **************************************
     O*** BEGIN ADD *** 07/10/02 **************************************
     O          E            DETDJ       2
     O                                           12 'TYPE:'
     O          E            DETDJ       1
     O                       BDTYPE              10
     O                                           12 '-'
     O                                           35 'Network Discount: % off'
     O                                           58 ' Billed Charges        '
     O          E            DETDJ       2
     O                                           21 'Discount Code:'
     O                       BDCODE              26
     O                                           43 'Effective Date:'
     O                       XPEFDT              56 '  /  /    '
     O                                           67 'Status:'
     O                       BDSTAT              70
     O          E            DETDJ       2
     O                                           16 'Comments:'
     O                       BDCOM1              78
     O          E            DETDJ       1
     O                       BDCOM2              78
     O          E            DETDJ       2
     O                                           31 'Discount % off billed ch'
     O                                           46 'arges . . . . :'
     O                       XXPC01        M     52
     O                                           54 '%'
     O          E            DETDJ       2
     O                                           31 'MRA reductions  . . . . '
     O                                           46 '. . . . .(Y/N):'
     O                       BDMRAF            +001
     O          E            DETDJ       1
     O                                           31 'Other 1 reductions  . . '
     O                                           46 '. . . . .(Y/N):'
     O                       BDOT1F            +001
     O          E            DETDJ       2
     O                                           31 'Other 1 reduction code  '
     O                                           46 '. . . . . . . :'
     O                       BDOT1C            +001
     O          E            DETDJ       1
     O                                           31 'Other 2 reductions  . . '
     O                                           46 '. . . . .(Y/N):'
     O                       BDOT2F            +001
     O          E            DETDJ       2
     O                                           31 'Other 2 reduction code  '
     O                                           46 '. . . . . . . :'
     O                       BDOT2C            +001
     O          E            DETDJ       2
     O                                           31 'Network reductions  . . '
     O                                           46 '. . . . .(Y/N):'
     O                       BDNETF            +001
     O*** END   ADD *** 07/10/02 **************************************
