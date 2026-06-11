     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE U7020R - C/R REVIEW REPORT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    PROGRAM TO CREATE PRINTED REPORT OF EXCEPTION CLAIM RECORDS*
     H*    FOUND IN THE UAEEP EXCEPTION CLAIM WORKFILE                *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    UTILIZATION/REVIEW MENU                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  PROGRAMMER: KEN RUTHVEN        PROJECT: 99AAF                *
     H*  CREATED: 10/20/92                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     h* 08/26/14 C:307 Project: SY354                                 *
     h*                -Implement Routing Page Changes.               *
     h*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  08/23/93  C:20    REQ/PROJ: UR007                            *
     F*                    1. CHANGED ROUTING SHEET TO PRINT EDIT CODE*
     H*                       INFORMATION.  CHANGED PRINTING OF DESC. *
     H*                       TO BE DESCRIPTION ENTERED IN C/R REVIEW *
     H*                       MAINTENANCE INSTEAD OF GENERIC DESC.    *
     H*  12/13/93  C:20    REQ/PROJ: UR007                            *
     F*                    1. IF NO RECORDS SELECTED STILL PRINT      *
     H*                       ROUTING AND HEADER INFORMATION WITH THE *
     H*                       NO RECORDS SELECTED MESSAGE             *
     H*                                                              **
     H*  12/22/93 C:20     REQ/PROJ: 8/SY008                         **
     H*                    1. DO NOT USE "UP" VALUES BECAUSE PROGRAM **
     H*                       IS SUBMITTED                           **
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    OF           OVERFLOW                                      *
     H*    70           LOKUP ETYP                                    *
     H*    45           PRE-CERT EXTENSION <> ZERO                    *
     H*    46           EEMOD <> ZERO                                 *
     H*    50           DOWEQ PEE01                                   *
     H*    55           ALL SUBSCRIBERS                               *
     H*    56           ALL GROUPS                                    *
     H*    57           ALL EDIT CODES                                *
     H*    60           GENERAL CHAIN/DOWEQ                           *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FUAEEL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FUMCRL     IF   E           K DISK
     FUMPCL3    IF   E           K DISK
     FFDCAL2    IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F*

     D ETYP            S              2    DIM(21) CTDATA PERRCD(1)             EDIT TYPE/EXTENSION
     D EXT             S              1    DIM(21) ALT(ETYP)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  X1SS#                  1      9
     D  SSPRE                  1      3
     D  SSMID                  4      5
     D  SSSUF                  6      9
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    EECO#
     C                   KFLD                    EESUB#
     C                   KFLD                    EEFND
     C                   KFLD                    EEFYR
     C                   KFLD                    EECASE
     C*
     C* PRE-CERTIFICATION MASTER FILE
     C*
     C     PCKEY         KLIST
     C                   KFLD                    EECO#
     C                   KFLD                    EESUB#
     C                   KFLD                    EEFND
     C                   KFLD                    EEFYR
     C                   KFLD                    EECASE
     C                   KFLD                    EEFEIN
     C                   KFLD                    EEMOD
     C*
     C* DETAIL CHARGE FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EECO#
     C                   KFLD                    EESUB#
     C                   KFLD                    EEFND
     C                   KFLD                    EEFYR
     C                   KFLD                    EECASE
     C                   KFLD                    EEFEIN
     C                   KFLD                    EEMOD
     C*
     C* KEY LIST FOR SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    EECO#
     C                   KFLD                    EESUB#
     C*
     C* KEY LIST FOR FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    EECO#
     C                   KFLD                    EESUB#
     C                   KFLD                    EEFND
     C*
     C* KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*   KEY LIST FOR THE C/R REVIEW FILE
     C*
     C     CRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEDIT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQEDIT            5
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
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CRETYP        XXETYP                         2A
     C     *LIKE         DEFINE    CMSS#         XXSS#                          9.0N
     C     *LIKE         DEFINE    EECO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    EESUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    EEFND         XXFND                          3.0N
     C     *LIKE         DEFINE    EEEDIT        XXEDIT                         5A
     C     *LIKE         DEFINE    EECASE        XXCASE                         7.0N
     C     *LIKE         DEFINE    PCCERT        XXCERT                         15A
     C     *LIKE         DEFINE    PCEXT         XXEXT                          3.0N
     C     *LIKE         DEFINE    CATDAT        XXTDAT                         7.0N
     C*
     C                   Z-ADD     0             Y                 5 0
     C                   Z-ADD     0             XXCNT             5 0
     C                   Z-ADD     0             XXFCNT            5 0
     C                   Z-ADD     0             XXSCNT            5 0
     C                   Z-ADD     0             XXMED             9 2
     C                   Z-ADD     0             XXTMED           11 2
     C                   Z-ADD     0             XXGMED           11 2
     C                   Z-ADD     0             XXSMED           11 2
     C                   MOVE      *BLANKS       XXDESC           80
     C                   MOVE      *BLANKS       X1DESC           40
     C*
     C* MOVE ENTRY PARMS INTO WORK FIELDS
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEDIT        XXEDIT
     C*
     C* DETERMINE REPORT SELECTIONS
     C*
     C     XXSUB#        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN55                                      S
     C                   END                                                    END XXSUB#
     C     XXFND         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN56
     C                   END                                                    END XXFND
     C     XXEDIT        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN57
     C                   END                                                    END XXEDIT
     C*
     C* SET-UP AND INITIALIZE
     C*
     C                   TIME                    DSDTTM
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*
     C* GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C* GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* READ THE EXCEPTION CLAIM FILE
     C*
     C                   READ      PEE01                                  50
     C*
     C* MOVE IN FISRT TIME VALUES
     C*
     C                   Z-ADD     EECO#         XXCO#
     C                   Z-ADD     EESUB#        XXSUB#
     C                   Z-ADD     EEFND         XXFND
     C                   MOVE      EEEDIT        XXEDIT
     C*
     C* GET THE EDIT TYPE AND DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXDESC
     C                   MOVE      *BLANKS       X1DESC
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CRETYP        XXETYP
     C                   Z-ADD     1             Y
     C     XXETYP        LOOKUP    ETYP(Y)                                70
     C     *IN70         IFEQ      '1'
     C                   MOVEL     EXT(Y)        YNEXT             1
     C                   ELSE
     C                   MOVE      *BLANKS       YNEXT
     C                   END
     C                   MOVE      '0'           *IN30
     C                   MOVE      '0'           *IN31
     C                   MOVE      '0'           *IN32
     C                   MOVE      '0'           *IN33
     C                   MOVE      '0'           *IN34
     C                   MOVE      '0'           *IN35
     C                   MOVE      '0'           *IN36
     C                   MOVE      '0'           *IN37
     C                   MOVE      '0'           *IN38
     C                   MOVE      '0'           *IN39
     C                   MOVE      '0'           *IN40
     C                   MOVE      '0'           *IN41
     C*
     C     CRETYP        IFEQ      'A1'
     C     CRETYP        OREQ      'A2'
     C     CRETYP        OREQ      'A3'
     C     CRETYP        OREQ      'A4'
     C     CRETYP        OREQ      'A5'
     C     CRETYP        OREQ      'A7'
     C     CRETYP        OREQ      'AD'
     C     CRETYP        OREQ      'AE'
     C     CRETYP        OREQ      'AF'
     C     CRETYP        OREQ      'AG'
     C     CRETYP        OREQ      'AH'
     C     CRETYP        OREQ      'AI'
     C     CRETYP        OREQ      'AJ'
     C     CRETYP        OREQ      'AK'
     C     CRETYP        OREQ      'AL'
     C                   MOVE      '1'           *IN33
     C                   END                                                    END EQ 'A1'
     C*
     C     CRETYP        IFEQ      'A1'
     C     CRETYP        OREQ      'A4'
     C     CRETYP        OREQ      'A5'
     C     CRETYP        OREQ      'A6'
     C     CRETYP        OREQ      'A7'
     C     CRETYP        OREQ      'A8'
     C     CRETYP        OREQ      'A9'
     C     CRETYP        OREQ      'AA'
     C     CRETYP        OREQ      'AF'
     C     CRETYP        OREQ      'AI'
     C     CRETYP        OREQ      'AK'
     C                   MOVE      '1'           *IN35
     C                   END                                                    END EQ 'A1'
     C*
     C     CRETYP        IFEQ      'A1'
     C     CRETYP        OREQ      'A2'
     C     CRETYP        OREQ      'A3'
     C     CRETYP        OREQ      'A4'
     C     CRETYP        OREQ      'A5'
     C     CRETYP        OREQ      'A6'
     C     CRETYP        OREQ      'A7'
     C     CRETYP        OREQ      'A8'
     C     CRETYP        OREQ      'A9'
     C     CRETYP        OREQ      'AA'
     C     CRETYP        OREQ      'AB'
     C     CRETYP        OREQ      'AD'
     C     CRETYP        OREQ      'AE'
     C     CRETYP        OREQ      'AF'
     C     CRETYP        OREQ      'AG'
     C     CRETYP        OREQ      'AH'
     C     CRETYP        OREQ      'AI'
     C     CRETYP        OREQ      'AJ'
     C     CRETYP        OREQ      'AK'
     C     CRETYP        OREQ      'AL'
     C                   MOVE      '1'           *IN41
     C                   END                                                    END EQ 'A1'
     C*
     C     CRETYP        IFEQ      'A4'
     C     CRETYP        OREQ      'A5'
     C     CRETYP        OREQ      'A6'
     C     CRETYP        OREQ      'A8'
     C     CRETYP        OREQ      'AA'
     C     CRETYP        OREQ      'AE'
     C     CRETYP        OREQ      'AF'
     C     CRETYP        OREQ      'AG'
     C     CRETYP        OREQ      'AI'
     C                   MOVE      '1'           *IN36
     C                   END                                                    END EQ 'A4'
     C*
     C     CRETYP        IFEQ      'A1'
     C     CRETYP        OREQ      'A4'
     C     CRETYP        OREQ      'AE'
     C     CRETYP        OREQ      'AJ'
     C     CRETYP        OREQ      'AK'
     C     CRETYP        OREQ      'AL'
     C                   MOVE      '1'           *IN37
     C                   END                                                    END EQ 'A1'
     C*
     C     CRETYP        IFEQ      'AG'
     C     CRETYP        OREQ      'AH'
     C     CRETYP        OREQ      'AL'
     C                   MOVE      '1'           *IN39
     C                   END                                                    END EQ 'AG'
     C*
     C     CRETYP        IFEQ      'A1'
     C     CRETYP        OREQ      'A3'
     C     CRETYP        OREQ      'A6'
     C     CRETYP        OREQ      'A8'
     C     CRETYP        OREQ      'A9'
     C     CRETYP        OREQ      'AA'
     C     CRETYP        OREQ      'AB'
     C     CRETYP        OREQ      'AD'
     C                   MOVE      '1'           *IN40
     C                   END                                                    END EQ 'A1'
     C                   MOVEL     CRDESC        XXDESC
     C                   END                                                    END *IN60
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SMNAME
     C                   END
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       FNNAME
     C                   END
     C*
     C* GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END                                                    END *IN60
     C*
     C                   EXCEPT    HEAD
     C*
     C* IF NO MATCH PRINT "ERROR MESSAGE"
     C*
     C     *IN50         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END                                                    END *IN60
     C*
     C* START LOOP
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C* CHECK FOR CHANGE IN COMPANY, SUBSCRIBER, GROUP, EDIT CODE
     C*
     C     EECO#         IFNE      XXCO#
     C                   EXSR      SREDIT
     C                   EXSR      SRGRP
     C                   EXSR      SRSUB
     C                   EXSR      SRCO
     C                   EXSR      SREDT1
     C                   EXCEPT    HEAD
     C                   EXSR      SRCASE
     C                   ELSE                                                   ELSE EECO#
     C*
     C     EESUB#        IFNE      XXSUB#
     C                   EXSR      SREDIT
     C                   EXSR      SRGRP
     C                   EXSR      SRSUB
     C                   EXSR      SRSUB1
     C                   EXSR      SRGRP1
     C                   EXSR      SREDT1
     C                   EXCEPT    ROUTE
     C                   EXCEPT    HEAD
     C                   EXSR      SRCASE
     C                   ELSE                                                   ELSE EESUB#
     C*
     C     EEFND         IFNE      XXFND
     C                   EXSR      SREDIT
     C                   EXSR      SRGRP
     C                   EXSR      SRGRP1
     C                   EXSR      SREDT1
     C                   EXCEPT    ROUTE
     C                   EXCEPT    HEAD
     C                   EXSR      SRCASE
     C                   ELSE                                                   ELSE EEFND
     C*
     C     EEEDIT        IFNE      XXEDIT
     C                   EXSR      SREDIT
     C                   EXSR      SREDT1
     C                   EXCEPT    ROUTE
     C                   EXCEPT    HEAD
     C                   EXSR      SRCASE
     C                   ELSE                                                   ELSE EEEDIT
     C*
     C     EECASE        IFNE      XXCASE
     C                   EXSR      SRCASE
     C*
     C                   END                                                    END XXCASE
     C                   END                                                    END EEEDIT
     C                   END                                                    END EEFND
     C                   END                                                    END EESUB#
     C                   END                                                    END CO#
     C*
     C*
     C* GET THE NEXT EXCEPTED CLAIM RECORD
     C*
     C                   READ      PEE01                                  50
     C                   END                                                    END *IN50
     C*
     C* PRINT TOTALS
     C*
     C                   Z-ADD     0             XXCASE
     C                   EXSR      SREDIT
     C                   EXSR      SRGRP
     C                   EXSR      SRSUB
     C*                    EXCPTTOT
     C*                    EXCPTTOT1
     C*                    EXCPTTOT2
     C*
     C* END THE PROGRAM
     C*
     C     TERM          TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain orkey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C*****************************************************************
     C* SRCASE SUBROUTINE - GET NEW CASE INFORMATION                  *
     C*****************************************************************
     C     SRCASE        BEGSR
     C*
     C*          XXCASE    IFNE *ZEROS
     C*          XXCASE    ANDNEEECASE
     C*                    ADD  XXMED     XXTMED
     C*                    END
     C*
     C                   ADD       1             XXCNT
     C                   Z-ADD     EECASE        XXCASE
     C                   Z-ADD     0             XXMED
     C*
     C* GET CLAIMANT SS# FROM CLAIM MASTER FILE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         XXSS#
     C                   MOVE      CMSS#         X1SS#
     C                   ELSE
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVE      *BLANKS       X1SS#
     C                   END                                                    END *IN60
     C*
     C* GET CLAIMANT NAME FROM DEMOGRAPHICS FILE
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C     XXSS#         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         XXLAST           56
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END CDFSN
     C                   END                                                    END *IN60
     C*
     C* READ DETAIL CHARGE FILE FOR THIS PROVIDER AND ACCUMULATE THE
     C* TOTAL MEDICAL PAID (PAID=EOMB NE *ZEROS)
     C*
     C                   Z-ADD     0             XXTDAT
     C     CAKEY         CHAIN     PCA01                              60
     C     *IN60         DOWEQ     '0'
     C     CAEOMB        IFNE      *ZEROS
     C                   ADD       CATOTP        XXMED
     C     CATDAT        IFGT      XXTDAT
     C                   Z-ADD     CATDAT        XXTDAT
     C                   END                                                    END CATDAT
     C                   END                                                    END CAEOMB
     C     CAKEY         READE     PCA01                                  60
     C                   END                                                    END *IN60
     C*
     C                   Z-ADD     XXTDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXLRDT            6 0
     C*
     C                   ADD       XXMED         XXTMED
     C*
     C* GET THE MOST RECENT PRE-CERT FOR THIS PROV IF ONE EXISTS
     C*
     C     PCKEY         CHAIN     PPC01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXCERT
     C                   Z-ADD     *ZEROS        XXEXT
     C                   END
     C     *IN60         DOWEQ     '0'
     C     XXEXT         IFEQ      *ZEROS
     C                   MOVE      PCCERT        XXCERT
     C                   Z-ADD     PCEXT         XXEXT
     C                   ELSE
     C     XXEXT         IFLT      PCEXT
     C                   MOVE      PCCERT        XXCERT
     C                   Z-ADD     PCEXT         XXEXT
     C                   END                                                    END XXEXT
     C                   END                                                    END XXEXT
     C     PCKEY         READE     PPC01                                  60
     C                   END                                                    END *IN60
     C*
     C* DECIDE WHETHER OR NOT TO PRINT EXTENSION
     C*
     C     XXEXT         IFNE      *ZEROS
     C                   MOVE      '1'           *IN45
     C                   ELSE
     C                   MOVE      '0'           *IN45
     C                   END                                                    END XXEXT
     C*
     C* CHECK IF PROVIDER HAS A LOCATION MODIFIER
     C*
     C     EEMOD         IFNE      *ZEROS
     C                   MOVE      '1'           *IN46
     C                   ELSE
     C                   MOVE      '0'           *IN46
     C                   END                                                    END EEMOD
     C*
     C* PRINT THE DETAIL LINE
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END                                                    END *INOF
     C                   EXCEPT    DET
     C*
     C                   ENDSR
     C*****************************************************************
     C* SRCO SUBROUTINE - GET COMPANY NAME ON CHANGE                  *
     C*****************************************************************
     C     SRCO          BEGSR
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C                   ENDSR
     C*****************************************************************
     C* SREDIT SUBROUTINE - PRINT EDIT CODE TOTALS                    *
     C*****************************************************************
     C     SREDIT        BEGSR
     C*
     C                   EXCEPT    TOT
     C*
     C     EECASE        IFNE      XXCASE
     C                   ADD       XXTMED        XXGMED
     C                   ADD       XXCNT         XXFCNT
     C*                    ADD  XXTMED    XXSMED
     C*                    ADD  XXCNT     XXSCNT
     C                   END
     C                   Z-ADD     0             XXCNT
     C                   Z-ADD     0             XXTMED
     C                   MOVE      *BLANKS       XXEDIT
     C                   MOVE      EEEDIT        XXEDIT
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* SREDT1 SUBROUTINE - GET NEW EDIT CODE DESC. FOR HEADING       *
     C*****************************************************************
     C     SREDT1        BEGSR
     C*
     C* GET THE EDIT TYPE AND DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXDESC
     C                   MOVE      *BLANKS       X1DESC
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CRETYP        XXETYP
     C                   Z-ADD     1             Y
     C     XXETYP        LOOKUP    ETYP(Y)                                70
     C     *IN70         IFEQ      '1'
     C                   MOVEL     EXT(Y)        YNEXT             1
     C                   ELSE
     C                   MOVE      *BLANKS       YNEXT
     C                   END
     C                   MOVE      '0'           *IN30
     C                   MOVE      '0'           *IN31
     C                   MOVE      '0'           *IN32
     C                   MOVE      '0'           *IN33
     C                   MOVE      '0'           *IN34
     C                   MOVE      '0'           *IN35
     C                   MOVE      '0'           *IN36
     C                   MOVE      '0'           *IN37
     C                   MOVE      '0'           *IN38
     C                   MOVE      '0'           *IN39
     C                   MOVE      '0'           *IN40
     C                   MOVE      '0'           *IN41
     C*
     C     CRETYP        IFEQ      'A1'
     C     CRETYP        OREQ      'A2'
     C     CRETYP        OREQ      'A3'
     C     CRETYP        OREQ      'A4'
     C     CRETYP        OREQ      'A5'
     C     CRETYP        OREQ      'A7'
     C     CRETYP        OREQ      'AD'
     C     CRETYP        OREQ      'AE'
     C     CRETYP        OREQ      'AF'
     C     CRETYP        OREQ      'AG'
     C     CRETYP        OREQ      'AH'
     C     CRETYP        OREQ      'AI'
     C     CRETYP        OREQ      'AJ'
     C     CRETYP        OREQ      'AK'
     C     CRETYP        OREQ      'AL'
     C                   MOVE      '1'           *IN33
     C                   END                                                    END EQ 'A1'
     C*
     C     CRETYP        IFEQ      'A1'
     C     CRETYP        OREQ      'A4'
     C     CRETYP        OREQ      'A5'
     C     CRETYP        OREQ      'A6'
     C     CRETYP        OREQ      'A7'
     C     CRETYP        OREQ      'A8'
     C     CRETYP        OREQ      'A9'
     C     CRETYP        OREQ      'AA'
     C     CRETYP        OREQ      'AF'
     C     CRETYP        OREQ      'AI'
     C     CRETYP        OREQ      'AK'
     C                   MOVE      '1'           *IN35
     C                   END                                                    END EQ 'A1'
     C*
     C     CRETYP        IFEQ      'A1'
     C     CRETYP        OREQ      'A2'
     C     CRETYP        OREQ      'A3'
     C     CRETYP        OREQ      'A4'
     C     CRETYP        OREQ      'A5'
     C     CRETYP        OREQ      'A6'
     C     CRETYP        OREQ      'A7'
     C     CRETYP        OREQ      'A8'
     C     CRETYP        OREQ      'A9'
     C     CRETYP        OREQ      'AA'
     C     CRETYP        OREQ      'AB'
     C     CRETYP        OREQ      'AD'
     C     CRETYP        OREQ      'AE'
     C     CRETYP        OREQ      'AF'
     C     CRETYP        OREQ      'AG'
     C     CRETYP        OREQ      'AH'
     C     CRETYP        OREQ      'AI'
     C     CRETYP        OREQ      'AJ'
     C     CRETYP        OREQ      'AK'
     C     CRETYP        OREQ      'AL'
     C                   MOVE      '1'           *IN41
     C                   END                                                    END EQ 'A1'
     C*
     C     CRETYP        IFEQ      'A4'
     C     CRETYP        OREQ      'A5'
     C     CRETYP        OREQ      'A6'
     C     CRETYP        OREQ      'A8'
     C     CRETYP        OREQ      'AA'
     C     CRETYP        OREQ      'AE'
     C     CRETYP        OREQ      'AF'
     C     CRETYP        OREQ      'AG'
     C     CRETYP        OREQ      'AI'
     C                   MOVE      '1'           *IN36
     C                   END                                                    END EQ 'A4'
     C*
     C     CRETYP        IFEQ      'A1'
     C     CRETYP        OREQ      'A4'
     C     CRETYP        OREQ      'AE'
     C     CRETYP        OREQ      'AJ'
     C     CRETYP        OREQ      'AK'
     C     CRETYP        OREQ      'AL'
     C                   MOVE      '1'           *IN37
     C                   END                                                    END EQ 'A1'
     C*
     C     CRETYP        IFEQ      'AG'
     C     CRETYP        OREQ      'AH'
     C     CRETYP        OREQ      'AL'
     C                   MOVE      '1'           *IN39
     C                   END                                                    END EQ 'AG'
     C*
     C     CRETYP        IFEQ      'A1'
     C     CRETYP        OREQ      'A3'
     C     CRETYP        OREQ      'A6'
     C     CRETYP        OREQ      'A8'
     C     CRETYP        OREQ      'A9'
     C     CRETYP        OREQ      'AA'
     C     CRETYP        OREQ      'AB'
     C     CRETYP        OREQ      'AD'
     C                   MOVE      '1'           *IN40
     C                   END                                                    END EQ 'A1'
     C                   MOVEL     CRDESC        XXDESC
     C                   END                                                    END *IN60
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* SRGRP SUBROUTINE- PRINT GROUP TOTALS                          *
     C*****************************************************************
     C     SRGRP         BEGSR
     C*
     C                   EXCEPT    TOT1
     C                   ADD       XXGMED        XXSMED
     C                   ADD       XXFCNT        XXSCNT
     C                   Z-ADD     0             XXGMED
     C                   Z-ADD     0             XXFCNT
     C*
     C                   Z-ADD     EEFND         XXFND
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* SRGRP1 SUBROUTINE- GET NEW GROUP INFORMATION                  *
     C*****************************************************************
     C     SRGRP1        BEGSR
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       FNNAME
     C                   END
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* SRSUB SUBROUTINE - PRINT SUBSCRIBER TOTALS                    *
     C*****************************************************************
     C     SRSUB         BEGSR
     C*
     C                   EXCEPT    TOT2
     C*
     C                   Z-ADD     EESUB#        XXSUB#
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* SRSUB1 SUBROUTINE- GET NEW SUBSCRIBER INFORMATION             *
     C*****************************************************************
     C     SRSUB1        BEGSR
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SMNAME
     C                   END
     C*
     C                   ENDSR
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O*********                                   8 'XXXXXXXX'
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
     O          E            ROUTE          2
     O                                           21 'REPORT SELECTION:'
     O          E            ROUTE          2
     O                                           31 'SUBSCRIBER......:'
     O                       XXSUB#              38
     O                                           40 '-'
     O                       SMNAME              82
     O          E            ROUTE          2
     O                                           31 'GROUP...........:'
     O                       XXFND               38
     O                                           40 '-'
     O                       FNNAME             102
     O          E            ROUTE          2
     O                                           31 'EDIT CODE.......:'
     O                       XXEDIT              38
     O                                           50 'EDIT TYPE:'
     O                       XXETYP              53
     O                                           60 'DESC:'
     O                       XXDESC             142
     O          E            ROUTE          2
     O                                           31 'EXTENSIONS......:'
     O                       YNEXT               38
     O          E            ROUTE          2
     O                                           31 'PLACE OF SERVICE:'
     O               30      CRPOS               38
     O                                           61 'LOST TIME.......:'
     O               31      CRLTFL              64
     O          E            ROUTE          2
     O                                           31 'PROV/MOD........:'
     O               32      CRFEIN        Z     42
     O               32      CRMOD         Z     46
     O                                           61 'SPECAILTY CODE..:'
     O               33      CRSPCD              66
     O          E            ROUTE          1
     O                                           31 'DIAGNOSIS.......:'
     O               34      CRDIA1              38
     O                                           61 'PROCEDURES......:'
     O               35      CRCPT1              68
     O                                           89 'Include or Exclude:'
     O               41      CRIEFL              91
     O          E            ROUTE          1
     O               34      CRDIA2              38
     O               35      CRCPT2              68
     O          E            ROUTE          1
     O               34      CRDIA3              38
     O               35      CRCPT3              68
     O          E            ROUTE          1
     O               34      CRDIA4              38
     O               35      CRCPT4              68
     O          E            ROUTE          1
     O               34      CRDIA5              38
     O               35      CRCPT5              68
     O          E            ROUTE          1
     O               35      CRCPT6              68
     O          E            ROUTE          1
     O               35      CRCPT7              68
     O          E            ROUTE          2
     O               35      CRCPT8              68
     O          E            ROUTE          1
     O                                           31 'NUMBER OF VISITS'
     O               36      CRVIST        Z     37
     O                                           63 'WEEKS OF TREATMENT:'
     O               37      CRWKST        Z     69
     O          E            ROUTE          2
     O                                           31 'AGE:'
     O               38      CRAGE         Z     37
     O                                           55 'PAID IN MEDICAL:'
     O               39      CRAMT1        J     70
     O                                           88 '# OF OCCURENCES:'
     O               40      CROCCR        Z     94
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS, INC.'
     O***                                63 '1992, 1993, 1994'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    146
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          140 'PAGE:'
     O                       PAGE          Z    146
     O          E            HEAD           2
     O                                          137 'RUN DATE :'
     O                       DSDATE        Y    146
     O          E            HEAD           1
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              17
     O                                           19 '-'
     O                       SMNAME              61
     O          E            HEAD           2
     O                                           11 'GROUP.....:'
     O                       XXFND               17
     O                                           19 '-'
     O                       FNNAME              81
     O          E            HEAD           2
     O                                           10 'EDIT CODE:'
     O                       XXEDIT              16
     O                                           28 'EDIT TYPE:'
     O                       XXETYP              31
     O                                           38 'DESC:'
     O                       XXDESC             120
     O                       X1DESC             161
     O          E            HEAD           2
     O                                           04 'YEAR'
     O                                           09 'CASE'
     O                                           22 'CLAIMANT'
     O                                           82 'SOC. SEC.#'
     O                                           98 'TOTAL MEDICAL'
     O                                          115 'LAST REVIEW DATE'
     O                                          127 'PRE-CERT #'
     O                                          146 'PROVIDER'
     O          E            DET            1
     O                       EEFYR               03
     O                       EECASE              12
     O                       XXLAST              70
     O                       SSPRE               75
     O                                           76 '-'
     O                       SSMID               78
     O                                           79 '-'
     O                       SSSUF               83
     O                       XXMED         K     97
     O                       XXLRDT             107 '  /  /  '
     O                       XXCERT             132
     O               45                         133 '-'
     O               45      XXEXT              136
     O                       EEFEIN             147
     O               46                         148 '-'
     O               46      EEMOD              151
     O*
     O          E            TOT         1  2
     O                                           59 'EDIT CODE TOTAL:'
     O                       XXCNT         Z     69
     O                                           75 'CASES'
     O                       XXTMED        K     97
     O          E            TOT1        1  2
     O                                           59 'GROUP TOTALS:'
     O                       XXFCNT        Z     69
     O                                           75 'CASES'
     O                       XXGMED        K     97
     O          E            TOT2        1  2
     O                                           59 'SUBSCRIBER TOTALS:'
     O                       XXSCNT        Z     69
     O                                           75 'CASES'
     O                       XXSMED        K     97
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
     O**
**  EDIT TYPE CODES
A1Y
A2Y
A3N
A4Y
A5Y
A6Y
A7Y
A8Y
A9Y
AAY
ABN
ACN
ADN
AEN
AFY
AGN
AHN
AIY
AJN
AKY
ALN
