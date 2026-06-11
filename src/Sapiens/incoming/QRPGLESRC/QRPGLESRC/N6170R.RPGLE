     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE N6170R - APPLY NETWORK DISCOUNTS IN BATCH MODE.
     H*****************************************************************
     H*                                                              **
     H*   P R O G R A M      S U M M A R Y                           **
     H*                                                              **
     H*   THIS BATCH  PROGRAM WILL DETERMINE IF A NETWORK DISCOUNT   **
     H*   SHOULD BE APPLIED BASED ON IF THE GROUP AND PROVIDER ARE   **
     H*   FLAGGED AS NETWORK PARTICIPATORS. WHEN CRITERIA IS MET,    **
     H*   ONE OF THE FOLLOWING PGMS WILL BE CALLED, DEPENDING ON THE **
     H*   DISCOUNT TYPE CODE -- D1, D2, D3, D4, OR D5.               **
     H***                                                           ***
     H*   1. CALL NETWORK DISCOUNT PGMS - N94D1R, N94D2R, N94D3R,    **
     H*                                   N94D4R, N94D5R.            **
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*                                                              **
     H*  03/09/99 C:94  PROJECT: MC152                               **
     H*                 CHANGE FDCAL2 TO FDCALL AND CHANGE KLIST     **
     H*                                                              **
     H*  02/18/98 C:62  Project: MC159                               **
     H*                 ** CHANGED NMPHL & NMHSL WITH FMS4L2/L3.     **
     H*                                                              **
     H*****************************************************************
     F***
     FFAEOL     IF   E           K DISK
     F*** BEGIN CHG *** 02/18/98 **************************************
     F********** NMHSL   IF  E           K        DISK
     F********** NMPHL   IF  E           K        DISK
     FFMS4L2    IF   E           K DISK
     FFMS4L3    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     F*** END   CHG *** 02/18/98 **************************************
     FFMPDL     IF   E           K DISK
     FNMBDLD    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F*** BEG CHG *** 03/09/99 ***************************************
     FFDCALL    IF   E           K DISK
     F**********FDCAL2  IF  E           K        DISK
     F*** END CHG *** 03/09/99 ***************************************
     F***

     D TYP             S              2    DIM(5) CTDATA PERRCD(1)
     D*
     D                 DS
     D  XXPGM                  1      8
     D  XXTYPE                 4      5
     D**
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

     C**********************************************************************
     C*
     C*   KEY LIST FOR THE DOCTOR PROVIDER FILE
     C*
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
     C*** BEGIN DEL *** 02/18/98 **************************************
     C*
     C********** PHKEY     KLIST
     C**********           KFLD           EOFEIN
     C**********           KFLD           EOMOD
     C*
     C*   KEY LIST FOR THE HOSPITAL PROVIDER FILE
     C*
     C********** HSKEY     KLIST
     C**********           KFLD           EOFEIN
     C**********           KFLD           EOMOD
     C*** END   DEL *** 02/18/98 **************************************
     C*** BEGIN ADD *** 02/18/98 **************************************
     C*
     C*  PROVIDER NETWORK CONTRACT FILE
     C*
     C     S4KEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*** END   ADD *** 02/18/98 **************************************
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C*  CASE DETAIL RECORDS
     C*
     C     CAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C**********           KFLD           EOTRCD  2        DEL 03/09/99
     C                   KFLD                    EOEOB#
     C*
     C*
     C*  GROUP FILE - FMFNL
     C*
     C     FNKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C*
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKEY         KLIST
     C                   KFLD                    QDTCD
     C                   KFLD                    XXDATE
     C*
     C     *LIKE         DEFINE    BDDT          XXDATE
     C*
     C     *LOVAL        SETLL     PEO01
     C                   READ      PEO01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     FNKEY         CHAIN     PFN01                              61
     C     *IN61         IFEQ      '0'
     C*
     C*******************************
     C*  CALL NETWORK DISCOUNT PGMS *
     C*******************************
     C*
     C*** DETERMINE IF GROUP & PROVIDER ARE PARTICIPATING IN THE
     C*** NETWORK DISCOUNT PROGRAM -- IF NOT, DO NOT CALL PGMS.
     C*
     C                   Z-ADD     1             X                 5 0
     C     'N'           LOOKUP    FSY(X)                                 62
     C*
     C     *IN62         IFEQ      '1'
     C                   MOVEL     FSY(X)        SYCODE            1
     C                   ELSE
     C                   MOVEL     *BLANK        SYCODE
     C                   END                                                    *IN62=1
     C*
 B1  C     SYCODE        IFEQ      'N'
     C*
     C     PDKEY         CHAIN     PPD01                              63
     C     *IN63         IFEQ      '0'
     C*
 B2  C     PDPARN        IFEQ      'P'
     C*
     C*** RETREIVE THE CASE DETAIL RECORD TO DETERMINE PLACE OF SERVICE
     C*
     C     CAKEY         CHAIN     PCA01                              64
     C     *IN64         IFEQ      '0'
     C*
 B3  C     CAPRTY        IFEQ      '1'
     C*
     C*** BEGIN CHG *** 02/18/98 **************************************
     C********** PHKEY     CHAINPPH01                65
     C     S4KEY         CHAIN     PS402                              65
 B4  C     *IN65         IFEQ      '0'
     C**********           MOVE PHDTCD    QDTCD   3        DISC CODE
     C                   MOVE      S4DISC        QDTCD             3            DISC CODE
     C*
     C**********           Z-ADDPHNEDT    SRDTI
     C                   Z-ADD     S4BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QNEDT             8
     C*
 E4  C                   END                                                    *IN65=0
     C*
 E3  C                   END                                                    CAPRTY=1
     C*
B3   C     CAPRTY        IFEQ      '4'
     C*
     C********** HSKEY     CHAINPHS01                65
     C     S4KEY         CHAIN     PS401                              65
B4   C     *IN65         IFEQ      '0'
     C**********           MOVE HSDTCD    QDTCD
     C                   MOVE      S4DISC        QDTCD
     C*
     C**********           Z-ADDHSCEDT    SRDTI
     C                   Z-ADD     S4BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QNEDT
     C*
E4   C                   END                                                    *IN65=0
     C*** END   CHG *** 02/18/98 **************************************
     C*
 E3  C                   END                                                    CAPRTY=4
     C*
     C*** NOW FIND THE DISCOUNT MASTER RECORD BASED ON THE DISCOUNT
     C*** IN QDTCD.
     C*
     C*
     C*  GET THE CURRENT DATE TO USE FOR KEY TO DISCOUNT MASTER
     C*
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*  GET THE DISCOUNT MASTER RECORD
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBD         TAG
     C                   READP     PBD01                                  66
     C*
     C*** VERIFY DISCOUNT CODE AND STATUS
     C*
     C     *IN66         IFEQ      '0'
     C     BDCODE        IFNE      QDTCD
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBD
     C                   END                                                    BDCODE/BDSTAT
     C                   END                                                    *IN66 OFF
     C*
     C     *IN66         IFEQ      '0'
     C*
     C*** CALL THE APPROPRIATE PGM TO CALCULATE THE NETWORK DISCOUNT
     C*** BASED ON THE DISCOUNT TYPE IN BDTYPE.
     C*
     C                   MOVE      *ZEROS        QCO#              3
     C                   MOVE      *ZEROS        QSUB#             5
     C                   MOVE      *ZEROS        QFUND             3
     C                   MOVE      *ZEROS        QFYR              3
     C                   MOVE      *ZEROS        QCASE             7
     C                   MOVE      *ZEROS        QFEIN             9
     C                   MOVE      *ZEROS        QMOD              3
     C                   MOVE      *ZEROS        QEOB#             9
     C*
     C                   MOVE      EOCO#         QCO#              3
     C                   MOVE      EOSUB#        QSUB#             5
     C                   MOVE      EOFND         QFUND             3
     C                   MOVE      EOFYR         QFYR              3
     C                   MOVE      EOCASE        QCASE             7
     C                   MOVE      EOFEIN        QFEIN             9
     C                   MOVE      EOMOD         QMOD              3
     C                   MOVE      EOEOB#        QEOB#
     C*
     C** Call QCAEXEC to GET PROGRAM BASED ON TYPE ENTERED.
     C                   MOVEL     'N94XXR'      XXPGM
     C                   MOVE      BDTYPE        XXTYPE            2
     C                   MOVE      ' '           QRTCD             1
     C*
     C                   CALL      'N94CL'
     C                   PARM                    XXPGM
     C                   PARM                    QCO#
     C                   PARM                    QSUB#
     C                   PARM                    QFUND
     C                   PARM                    QFYR
     C                   PARM                    QCASE
     C                   PARM                    QFEIN
     C                   PARM                    QMOD
     C                   PARM                    QEOB#
     C                   PARM                    QDTCD
     C                   PARM                    QNEDT
     C                   PARM                    QRTCD
     C*
     C                   END                                                    PDPARN=Y
     C*
     C                   END                                                    SYCODE=Y
     C*
     C*****************************************************************
     C*
     C                   END                                                    66 = 0
     C*
     C                   END                                                    64 = 0
     C*
     C                   END                                                    63 = 0
     C*
     C                   END                                                    61 = 0
     C*
     C                   READ      PEO01                                  60
     C                   END                                                    60 = 0
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   MOVE      *ZEROS        QCO#              3
     C                   MOVE      *ZEROS        QSUB#             5
     C                   MOVE      *ZEROS        QFUND             3
     C                   MOVE      *ZEROS        QFYR              3
     C                   MOVE      *ZEROS        QCASE             7
     C                   MOVE      *ZEROS        QFEIN             9
     C                   MOVE      *ZEROS        QMOD              3
     C                   MOVE      *ZEROS        QEOB#
     C                   MOVE      *ZEROS        QDTCD             3
     C                   MOVE      *ZEROS        QNEDT
     C                   MOVE      '9'           QRTCD             1
     C*
     C                   DO        5             X                 5 0
     C                   MOVEL     'N94XXR'      XXPGM
     C                   MOVE      TYP(X)        XXTYPE            2
     C                   CALL      'N94CL'
     C                   PARM                    XXPGM
     C                   PARM                    QCO#
     C                   PARM                    QSUB#
     C                   PARM                    QFUND
     C                   PARM                    QFYR
     C                   PARM                    QCASE
     C                   PARM                    QFEIN
     C                   PARM                    QMOD
     C                   PARM                    QEOB#
     C                   PARM                    QDTCD
     C                   PARM                    QNEDT
     C                   PARM                    QRTCD
     C*
     C                   END                                                    ENDDO X 5
     C*
     C*
**
D1
D2
D3
D4
D5
