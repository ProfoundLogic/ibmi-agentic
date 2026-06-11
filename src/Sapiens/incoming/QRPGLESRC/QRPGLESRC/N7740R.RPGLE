     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H/TITLE N7740R - AGED TRIAL BALANCE - REPORT PROGRAM
     H***************************************************************
     H** REVISIONS:
     H*                                                               *
     H* 06/09/20 C:212 Project : SY420                                *
     H*                Mask SSN                                       *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H*  10/02/2000 C:62  Project: MC065 24538
     H*                   MAKE SURE PROPER GROUP IS LOADED IN SUMMARY
     H*                   REPORT.
     H*
     H*  03/24/2000 C:62  Project: SUPP #22333
     H*                   -CHANGED DSDATE FROM 6.0 TO 8.0.
     H*                   -WHY CERTAIN CLAIMANT NAMES NOT FOUND.
     H*
     H*  01/27/00 C:62  Project: Y2K
     H*                 CHANGE DATES TO MDYY.
     H*
     H*  04/22/99 C:94  PROJECT: SUP17616
     H*                 CORRECT DATE CONVERSION ISSUE
     H*
     H*  01/04/99 C:31  Project: MC152
     H*                 1. Change size of invoice number
     H*
     H*****************************************************************
      ** INDICATORS:                                                 **
      ** 08   BATCH MODE                                             **
      ** 09   DEMAND MODE                                            **
      ** 10   DETAIL REPORT                                          **
      ** 11   SUMMARY REPORT                                         **
      ** 12   CASE CHGD - PRINT NEW CASE HEADING INFO ON DETAIL LINE **
      ** 13   PRINT DATE RANGE                                       **
      ** 50   PRIMARY FILE CHAIN/DOWEQ  (PAR01)                      **
      ** 60   GENERAL USAGE                                          **
      *****************************************************************
     FSMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FNMARL9    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     F*** DETAIL REPORT
     FQSYSPRT   O    F  198        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOA)
     F*** SUMMARY REPORT
     FQSYSPRT1  O    F  198        PRINTER INFDS(PRTDS1)
     F                                     OFLIND(*INOF)
     F*

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D NM              S             40    DIM(1) CTDATA PERRCD(1)              NO CASE/NAME
     D                 DS
     D*** BEGIN CHG *** 03/24/00 **************************************
     D**********                              1  120DSDTTM
     D**********                              1   60DSTIME
     D**********                              7  120DSDATE
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** END   CHG *** 03/24/00 **************************************
     D                 DS
     D  XC#                    1     10
     D  XXYR                   1      2  0
     D  XXHYP1                 3      3
     D  XXCASE                 4     10  0
     D PRTDS           DS
     D  FIL11                  1    200
     D  FIL12                201    366
     D  XXLIN1               367    368B 0
     D  XXPAG1               369    372B 0
     D PRTDS1          DS
     D  FIL21                  1    200
     D  FIL22                201    366
     D  XXLIN2               367    368B 0
     D  XXPAG2               369    372B 0
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
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
     C*   KEY LIST FOR THE SUBSCRIBER MASTER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*  OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C     *LIKE         DEFINE    UPDT          X1DT
     C*** BEGIN ADD *** 10/02/00 **************************************
     C     *LIKE         DEFINE    FNSUB#        ZZSUB#
     C     *LIKE         DEFINE    FNFND         ZZFND
     C*** END   ADD *** 10/02/00 **************************************
     C*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQFRD2            8
     C                   PARM                    QQTOD2            8
     C                   PARM                    QQRTYP            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
      **** BEGIN ADD **** 05/14/98 ************************************
     C                   PARM                    QQSYS             1
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
      **** END   ADD **** 05/14/98 ************************************
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      QQFRDT        X1FRDT            8 0
     C*
     C*** BEGIN DEL *** 01/27/00 **************************************
     C**********           Z-ADDX1FRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXFRDT  60
     C*** END   DEL *** 01/27/00 **************************************
     C*
     C                   MOVE      QQTODT        X1TODT            8 0
     C*
     C*** BEGIN DEL *** 01/27/00 **************************************
     C**********           Z-ADDX1TODT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXTODT  60
     C*** END   DEL *** 01/27/00 **************************************
     C*
     C                   MOVE      QQFRD2        X1FR2             8 0
     C*
     C*** BEGIN DEL *** 01/27/00 **************************************
     C**********           Z-ADDX1FR2     SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXFR2   60
     C*** END   DEL *** 01/27/00 **************************************
     C*
     C                   MOVE      QQTOD2        X1TO2             8 0
     C*
     C*** BEGIN DEL *** 01/27/00 **************************************
     C**********           Z-ADDX1TO2     SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXTO2   60
     C*** END   DEL *** 01/27/00 **************************************
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      '-'           XXHYP1            1
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
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
     C*   SET INDICATOR IF DATE RANGE IS TO BE PRINTED
     C*
     C*** BEGIN CHG *** 01/27/00 **************************************
     C********** XXFRDT    IFNE *ZEROS
     C********** XXTODT    ORNE *ZEROS
     C     X1FRDT        IFNE      *ZEROS
     C     X1TODT        ORNE      *ZEROS
     C*** END   CHG *** 01/27/00 **************************************
     C                   MOVE      '1'           *IN13
     C                   END
     C*
     C*   SET INDICATOR IF THERE EXISTS A SECOND SET OF DATES FROM
     C*   BATCH TO PRINT WHEN ALL UNBILLED RECORDS ARE PROCESSED
     C*
     C*** BEGIN CHG *** 01/27/00 **************************************
     C********** XXFR2     IFNE *ZEROS
     C********** XXTO2     ANDNE*ZEROS
     C     X1FR2         IFNE      *ZEROS
     C     X1TO2         ANDNE     *ZEROS
     C*** END   CHG *** 01/27/00 **************************************
     C                   MOVE      '1'           *IN14
     C                   END
     C*
     C                   Z-ADD     0             XXAMT            10 2
     C*
     C                   Z-ADD     0             XXIA             10 2          INV TOTALS
     C                   Z-ADD     0             XX30             10 2
     C                   Z-ADD     0             XX60             10 2
     C                   Z-ADD     0             XX90             10 2
     C                   Z-ADD     0             XX91             10 2
     C                   Z-ADD     0             XXBAL            10 2
     C*
     C                   Z-ADD     0             XXIA2            10 2          GRP TOTALS
     C                   Z-ADD     0             XX302            10 2
     C                   Z-ADD     0             XX602            10 2
     C                   Z-ADD     0             XX902            10 2
     C                   Z-ADD     0             XX912            10 2
     C                   Z-ADD     0             XXBAL2           10 2
     C*
     C                   Z-ADD     0             XXIA3            10 2          SUB  TOTALS
     C                   Z-ADD     0             XX303            10 2
     C                   Z-ADD     0             XX603            10 2
     C                   Z-ADD     0             XX903            10 2
     C                   Z-ADD     0             XX913            10 2
     C                   Z-ADD     0             XXBAL3           10 2
     C*
     C                   Z-ADD     0             XXIA4            10 2          PRODUCT TOTAL
     C                   Z-ADD     0             XX304            10 2
     C                   Z-ADD     0             XX604            10 2
     C                   Z-ADD     0             XX904            10 2
     C                   Z-ADD     0             XX914            10 2
     C                   Z-ADD     0             XXBAL4           10 2
     C*
     C                   Z-ADD     0             XXIA5            10 2          GRAND TOTAL
     C                   Z-ADD     0             XX305            10 2          (ALL PRODS)
     C                   Z-ADD     0             XX605            10 2
     C                   Z-ADD     0             XX905            10 2
     C                   Z-ADD     0             XX915            10 2
     C                   Z-ADD     0             XXBAL5           10 2
     C*
     C*  CHAIN TO USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*  GET ROUTING INFORMATION FOR REPORT
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
     C                   MOVE      ORTITL        XXTITL           60
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********   10              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END                                                    END IFEQ '0'
     C*
     C*  GET ROUTING INFORMATION FOR REPORT
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      ORTITL        YYTITL           60
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********   11              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *********************************************
     C                   END                                                    END IFEQ '0'
     C*
     C*  READ A/R FILE
     C*
     C     *LOVAL        SETLL     PAR01
     C                   READ      PAR01                                  50
     C*
     C     *IN50         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C   10              EXCEPT    HEAD2
     C   10              EXCEPT    ERRMSG
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C   11              EXCEPT    ERRMS1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*
     C     *IN50         IFEQ      '0'
     C*
     C                   Z-ADD     ARCO#         XXCO#             3 0
     C                   Z-ADD     ARSUB#        XXSUB#            5 0
     C                   Z-ADD     ARFND         XXFND             3 0
     C                   Z-ADD     ARFYR         XXFYR             3 0
     C                   Z-ADD     ARFYR         XXYR              2 0
     C                   Z-ADD     ARCASE        XXCASE            7 0
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             Z-ADD0         XXINV#  50
     C                   Z-ADD     0             XXINV#            9 0
     C*** END CHG *** 01/04/99 ****************************************
     C                   MOVE      ARSYS         XXSYS             1
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CONAME        XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   END
     C*
     C*  GET GROUP NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C*  GET SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSUBN           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSUBN
     C                   END
     C*
     C   10              EXCEPT    HEAD
     C   10              EXCEPT    HEAD2
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CMSS#         XXSS#             9
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       xxSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXACDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           MOVEL*BLANKS   XXCLNM 30
     C                   MOVEL     *BLANKS       XXCLNM           25
     C*** END   CHG *** 01/27/00 **************************************
     C                   MOVEL     *BLANKS       SRLNAM           56
     C                   MOVEL     *BLANKS       SRFNAM           15
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM
     C                   MOVE      ' '           SSTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SSTERM
     C                   MOVEL     SRLNAM        XXCLNM
     C                   ELSE                                                   ELS IF *IN60
     C                   MOVEL     NM(1)         XXCLNM
     C                   END                                                    END IF *IN60
     C                   ELSE                                                   ELS IF *IN60
     C                   MOVEL     NM(1)         XXCLNM
     C                   END                                                    END IF *IN60
     C                   MOVE      '1'           *IN12
     C                   END                                                    END IF *IN50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*  IF INV# CHANGES, CALCULATE AGING TOTALS, PRINT DETAIL LINE
     C*  IF GROUP CHANGES, PRINT GROUP TOTALS, GET NEW GRP INFORMATION
     C*  IF SUBSC   CHANGES, PRINT SUBSC   TOTALS, PAGE BREAK HEADINGS
     C*  IF PRODUCT CHANGES, PRINT PRODUCT TOTALS, PAGE BREAK HEADINGS
     C*
     C     ARSYS         IFNE      XXSYS
     C                   EXSR      SRINV                                        INV TOTALS
     C                   EXSR      SRCASE                                       CASE CHG
     C                   EXSR      SRGRP                                        GRP TOTALS
     C                   EXSR      SRSUB                                        SUB  TOTALS
     C                   EXSR      SRPRO                                        PROD TOTALS
     C                   ELSE
     C     ARSUB#        IFNE      XXSUB#
     C                   EXSR      SRINV                                        INV TOTALS
     C                   EXSR      SRCASE                                       CASE CHG
     C                   EXSR      SRGRP                                        GRP TOTALS
     C                   EXSR      SRSUB                                        SUB  TOTALS
     C                   ELSE
     C     ARFND         IFNE      XXFND
     C                   EXSR      SRINV                                        INV TOTALS
     C                   EXSR      SRCASE                                       CASE CHG
     C                   EXSR      SRGRP                                        GRP TOTALS
     C                   ELSE
     C     ARCASE        IFNE      XXCASE
     C                   EXSR      SRINV                                        INV TOTALS
     C                   EXSR      SRCASE                                       CASE CHG
     C                   ELSE
     C     ARINV#        IFNE      XXINV#
     C                   EXSR      SRINV                                        INV TOTALS
     C                   END                                                    ENDNE XXINV#
     C                   END                                                    ENDNE XXCASE
     C                   END                                                    ENDNE XXFND
     C                   END                                                    ENDNE XXSUB#
     C                   END                                                    ENDNE XXSYS
     C*
     C*  MOVE FIELDS TO DETAIL FIELDS
     C*
     C     ARTRTP        IFEQ      'I'
     C                   MOVE      ARTRDT        X1DT
     C                   ADD       ARAMT         XXIA
     C                   END
     C*
     C     ARTRTP        IFEQ      'P'
     C                   SUB       ARAMT         XXAMT
     C                   ELSE
     C                   ADD       ARAMT         XXAMT
     C                   END
     C**********************************
     C*  READ NEXT RECORD              *
     C**********************************
     C                   READ      PAR01                                  50
     C*
     C                   END                                                    END DOWEQ 50
     C**********************************
     C*     END THE PROGRAM            *
     C**********************************
     C                   MOVE      '1'           *INLR
     C*
     C                   EXSR      SRINV
     C                   EXSR      SRGRP
     C                   EXSR      SRSUB
     C                   EXSR      SRPRO
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C   11              EXCEPT    GRTOT1
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
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
     C*
     C*****************************************************************--*
     C*  CALCULATE AGING AMOUNTS PER INVOICE, PRINT DETAIL LINE
     C*****************************************************************--*
     C*
     C     SRINV         BEGSR
     C*
     C     XXAMT         IFNE      *ZERO
     C*
     C                   Z-ADD     X1DT          SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXDT    60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT              8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C                   Z-ADD     X1DT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X2DT              8 0
     C*
     C                   MOVE      X2DT          SRFRDC            8 0
     C     SRFRDC        IFEQ      *ZERO
     C                   MOVE      '01011901'    SRFRDC
     C                   END
     C*
     C*** BEG CHG *** 04/22/99 ****************************************
     C*** BEGIN CHG *** 03/24/00 **************************************
     C**********           MOVE DSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY8
     C**********           MOVELSRDTO     SRTODC  80
     C                   Z-ADD     DSDATE        SRTODC            8 0
     C*** END   CHG *** 03/24/00 **************************************
     C**********           MOVE DSDATE    SRTODC
     C**********           CALL 'SRCVTDAT'DTMDY8
     C**********           MOVELSRDTO     SRTODC  80
     C*** END CHG *** 04/22/99 ****************************************
     C*
     C                   Z-ADD     0             SRDAYS            5 0
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C     SRDAYS        IFLE      30
     C                   Z-ADD     XXAMT         XX30
     C                   Z-ADD     0             XX60
     C                   Z-ADD     0             XX90
     C                   Z-ADD     0             XX91
     C                   END
     C     SRDAYS        IFGT      30
     C     SRDAYS        ANDLE     60
     C                   Z-ADD     0             XX30
     C                   Z-ADD     XXAMT         XX60
     C                   Z-ADD     0             XX90
     C                   Z-ADD     0             XX91
     C                   END
     C     SRDAYS        IFGT      60
     C     SRDAYS        ANDLE     90
     C                   Z-ADD     0             XX30
     C                   Z-ADD     0             XX60
     C                   Z-ADD     XXAMT         XX90
     C                   Z-ADD     0             XX91
     C                   END
     C     SRDAYS        IFGT      90
     C                   Z-ADD     0             XX30
     C                   Z-ADD     0             XX60
     C                   Z-ADD     0             XX90
     C                   Z-ADD     XXAMT         XX91
     C                   END
     C                   Z-ADD     XXAMT         XXBAL
     C*
     C                   ADD       XXIA          XXIA2
     C                   ADD       XX30          XX302
     C                   ADD       XX60          XX602
     C                   ADD       XX90          XX902
     C                   ADD       XX91          XX912
     C                   ADD       XXBAL         XXBAL2
     C*
     C     *INOA         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C   10              EXCEPT    HEAD2
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C   10              EXCEPT    DETAIL
     C                   MOVE      '0'           *IN12
     C                   END                                                    END NE *ZERO
     C*
     C                   MOVE      ARINV#        XXINV#
     C*
     C                   Z-ADD     01011901      X2DT
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           Z-ADD010101    XXDT
     C                   Z-ADD     01011901      XXDT
     C*** END   CHG *** 01/27/00 **************************************
     C                   Z-ADD     0             XXAMT
     C                   Z-ADD     0             XXIA
     C*
     C                   ENDSR
     C*****************************************************************--*
     C*  GET CASE INFORMATION
     C*****************************************************************--*
     C*
     C     SRCASE        BEGSR
     C*
     C                   MOVE      '1'           *IN12                          NEW CASE
     C*
     C*** BEGIN ADD *** 03/24/00 **************************************
     C                   Z-ADD     ARCO#         XXCO#
     C*** BEGIN ADD *** 10/02/00 **************************************
     C                   Z-ADD     XXSUB#        ZZSUB#
     C                   Z-ADD     XXFND         ZZFND
     C*** END   ADD *** 10/02/00 **************************************
     C                   Z-ADD     ARSUB#        XXSUB#
     C                   Z-ADD     ARFND         XXFND
     C*** END   ADD *** 03/24/00 **************************************
     C                   Z-ADD     ARFYR         XXFYR
     C                   Z-ADD     ARFYR         XXYR
     C                   Z-ADD     ARCASE        XXCASE
     C*
     C     *INLR         IFEQ      '0'
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     CMSS#         XXSS#             9
      /Free
       xxSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXACDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           MOVEL*BLANKS   XXCLNM 30
     C                   MOVEL     *BLANKS       XXCLNM
     C*** END   CHG *** 01/27/00 **************************************
     C                   MOVEL     *BLANKS       SRLNAM           56
     C                   MOVEL     *BLANKS       SRFNAM           15
     C                   MOVEL     CDLSN         SRLNAM
     C                   MOVEL     CDFSN         SRFNAM
     C                   MOVE      ' '           SSTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SSTERM
     C                   MOVEL     SRLNAM        XXCLNM
     C*** BEGIN ADD *** 03/24/00 **************************************
     C                   ELSE                                                   ELS IF *IN60
     C                   MOVEL     NM(1)         XXCLNM
     C*** END   ADD *** 03/24/00 **************************************
     C                   END                                                    END IF *IN60
     C*** BEGIN ADD *** 03/24/00 **************************************
     C                   ELSE                                                   ELS IF *IN60
     C                   MOVEL     NM(1)         XXCLNM
     C*** END   ADD *** 03/24/00 **************************************
     C                   END                                                    END IF *IN60
     C                   MOVE      '1'           *IN12
     C                   END                                                    END IF *INLR
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM GROUP TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRGRP         BEGSR
     C*
     C                   ADD       XXIA2         XXIA3
     C                   ADD       XX302         XX303
     C                   ADD       XX602         XX603
     C                   ADD       XX902         XX903
     C                   ADD       XX912         XX913
     C                   ADD       XXBAL2        XXBAL3
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C   11              EXCEPT    GRPTL1
     C*
     C                   Z-ADD     0             XXIA2            10 2          ZERO GROUP
     C                   Z-ADD     0             XX302            10 2          TOTALS
     C                   Z-ADD     0             XX602            10 2
     C                   Z-ADD     0             XX902            10 2
     C                   Z-ADD     0             XX912            10 2
     C                   Z-ADD     0             XXBAL2           10 2
     C*
     C                   Z-ADD     ARFND         XXFND
     C*
     C     *INLR         IFEQ      '0'
     C*  CHAIN TO FUND MASTER FILE - GET FUND NAME
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C                   END                                                    END *INLR
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM SUBSCRIBER TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRSUB         BEGSR
     C*
     C                   ADD       XXIA3         XXIA4
     C                   ADD       XX303         XX304
     C                   ADD       XX603         XX604
     C                   ADD       XX903         XX904
     C                   ADD       XX913         XX914
     C                   ADD       XXBAL3        XXBAL4
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C   11              EXCEPT    SBTOT1
     C*
     C                   Z-ADD     0             XXIA3            10 2          ZERO SUBSC
     C                   Z-ADD     0             XX303            10 2          TOTALS
     C                   Z-ADD     0             XX603            10 2
     C                   Z-ADD     0             XX903            10 2
     C                   Z-ADD     0             XX913            10 2
     C                   Z-ADD     0             XXBAL3           10 2
     C*
     C                   Z-ADD     ARSUB#        XXSUB#
     C*
     C     *INLR         IFEQ      '0'
     C*  CHAIN TO SUBSCRIBER MASTER FILE - GET SUBSCRIBER NAME
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSUBN
     C                   ELSE
     C                   MOVE      *BLANKS       XXSUBN
     C                   END
     C                   END                                                    END *INLR
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM PRODUCT TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRPRO         BEGSR
     C*
     C                   ADD       XXIA4         XXIA5
     C                   ADD       XX304         XX305
     C                   ADD       XX604         XX605
     C                   ADD       XX904         XX905
     C                   ADD       XX914         XX915
     C                   ADD       XXBAL4        XXBAL5
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C   10              EXCEPT    PRTOT                                        DETAIL PROD TOTALS
     C   11              EXCEPT    PRTOT1                                       SUMMARY PROD TOTALS
     C*
     C                   Z-ADD     0             XXIA4            10 2          ZERO PROD
     C                   Z-ADD     0             XX304            10 2          TOTALS
     C                   Z-ADD     0             XX604            10 2
     C                   Z-ADD     0             XX904            10 2
     C                   Z-ADD     0             XX914            10 2
     C                   Z-ADD     0             XXBAL4           10 2
     C*
     C                   MOVE      ARSYS         XXSYS
     C*
     C                   ENDSR
     O**
     O*** DETAIL REPORT
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
      **** BEGIN ADD **** 05/12/98 ************************************
     O          E            ROUTE       2  2
     O                                           19 'SYSTEM:'
     O                       QQSYS               21
     O          E            ROUTE          2
     O                                           19 'SUBSCRIBER:'
     O                       QQSUB#              25
     O          E            ROUTE          2
     O                                           19 'GROUP:'
     O                       QQFND               23
     O          E            ROUTE          2
     O                                           19 'YEAR/CASE:'
     O                       QQFYR               23
     O                       QQCASE              32
     O          E            ROUTE          2
     O                                           19 'DATE RANGE:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFRDT    28 '  /  /  '
     O**********                         31 'TO'
     O**********               XXTODT    40 '  /  /  '
     O                       X1FRDT              30 '  /  /    '
     O                                           33 'TO'
     O                       X1TODT              44 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TYPE:'
     O                       QQRTYP              23
      **** END   ADD **** 05/12/98 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
      **** BEGIN CHG **** 05/07/98 ************************************
     O********************************** 57 'INC. 1997'
     O***                                46 'INC.'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      **** BEGIN CHG **** 05/07/98 ************************************
     OQSYSPRT   E            HEAD           1 06
     O*** BEGIN CHG *** 03/24/00 **************************************
     O**********               DSDATE     8 '  /  /  '
     O                       DSDATE              10 '  /  /    '
     O*** END   CHG *** 03/24/00 **************************************
     O                       XXCONM             119
     O                       DSTIME             198 '  :  :  '
     O          E            HEAD           1
     O                       ORPNAM              10
     O                       XXTITL             129
     O                       PAGE          Z    198
     O                                          192 'PAGE:'
     O*
     O          E    13      HEAD           1
     O                                           89 'FROM:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFRDTY   98
     O**********                        102 'TO:'
     O**********               XXTODTY  111
     O                       X1FRDT             100 '  /  /    '
     O                                          104 'TO:'
     O                       X1TODT             115 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O*
     O          E   N13 14   HEAD           1
     O                                           89 'FROM:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFR2 Y   98
     O**********                        102 'TO:'
     O**********               XXTO2 Y  111
     O                       X1FR2              100 '  /  /    '
     O                                          104 'TO:'
     O                       X1TO2              115 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            HEAD        1  2
      **** BEGIN CHG **** 05/07/98 ************************************
     O*********************************** 8 'PRODUCT:'
     O                                            8 'SYSTEM:'
      **** END   CHG **** 05/07/98 ************************************
     O                       XXSYS               10
     O*
     O          E            HEAD2          2
     O                                           11 'CASE NUMBER'
     O                                           47 'C L I E N T    N A M E'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O********                           67 'SOC SEC #'
     O********                           78 'ACC DATE'
     O*** BEGIN CHG *** 01/04/99 **************************************
     O********                           86 'INV #'
     O********                           88 'INV #'
     O********                           97 'INV DATE'
     O********                           99 'INV DATE'
     O********                          114 'INV AMT'
     O********                          116 'INV AMT'
     O********                          130 '0 - 30'
     O********                          132 '0 - 30'
     O********                          146 '31 - 60'
     O********                          148 '31 - 60'
     O********                          162 '61 - 90'
     O********                          164 '61 - 90'
     O********                          178 '91+'
     O********                          180 '91+'
     O********                          194 'BALANCE'
     O********                          196 'BALANCE'
     O*** END CHG *** 01/04/99 ****************************************
     O                                           62 'SOC SEC #'
     O                                           75 'ACC DATE'
     O                                           85 'INV #'
     O                                           98 'INV DATE'
     O                                          115 'INV AMT'
     O                                          131 '0 - 30'
     O                                          147 '31 - 60'
     O                                          163 '61 - 90'
     O                                          179 '91+'
     O                                          195 'BALANCE'
     O*** END   CHG *** 01/27/00 **************************************
     O          E            PRTOT       1  2
     O*** BEGIN CHG *** 01/04/99 **************************************
     O********                           75 'PRODUCT'
     O                                           77 'PRODUCT'
     O********                 XXSYS     77
     O                       XXSYS               79
     O********                           85 'TOTALS:'
     O                                           87 'TOTALS:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O********                 XXIA4 J  115
     O********                 XXIA4 J  117
     O********                 XX304 J  131
     O********                 XX304 J  133
     O********                 XX604 J  147
     O********                 XX604 J  149
     O********                 XX904 J  163
     O********                 XX904 J  165
     O********                 XX914 J  179
     O********                 XX914 J  181
     O********                 XXBAL4J  195
     O********                 XXBAL4J  197
     O                       XXIA4         J    116
     O                       XX304         J    132
     O                       XX604         J    148
     O                       XX904         J    164
     O                       XX914         J    180
     O                       XXBAL4        J    196
     O*** END   CHG *** 01/27/00 **************************************
     O*** END CHG *** 01/04/99 ****************************************
     O          E   NLRNOA   PRTOT          2
      **** BEGIN CHG **** 05/07/98 ************************************
     O*********************************** 8 'PRODUCT:'
     O                                            8 'SYSTEM:'
      **** END   CHG **** 05/07/98 ************************************
     O                       ARSYS               10
     O          E            DETAIL         1
     O               12      XXSUB#              05
     O               12                          06 '-'
     O               12      XXFND               09
     O               12                          10 '-'
     O               12      XXYR                12
     O               12                          13 '-'
     O               12      XXCASE              20
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********       12      XXCLNM B  55
     O**********       12      XXSS#  B  67
     O**********       12      XXACDT B  78 '  /  /  '
     O*** BEGIN CHG *** 01/04/99 **************************************
     O********                 XXINV#Z   86
     O********                 XXINV#Z   88
     O********                 XXDT   B  97 '  /  /  '
     O********                 XXDT   B  99 '  /  /  '
     O********                 XXIA  JB 115
     O********                 XXIA  JB 117
     O********                 XX30  JB 131
     O********                 XX30  JB 133
     O********                 XX60  JB 147
     O********                 XX60  JB 149
     O********                 XX90  JB 163
     O********                 XX90  JB 165
     O********                 XX91  JB 179
     O********                 XX91  JB 181
     O********                 XXBAL JB 195
     O********                 XXBAL JB 197
     O*** END CHG *** 01/04/99 ****************************************
     O               12      XXCLNM         B    50
     O               12      XXSS#          B    62
     O               12      XXACDT         B    75 '  /  /    '
     O                       XXINV#        Z     85
     O                       XXDT           B    98 '  /  /    '
     O                       XXIA          JB   116
     O                       XX30          JB   132
     O                       XX60          JB   148
     O                       XX90          JB   164
     O                       XX91          JB   180
     O                       XXBAL         JB   196
     O*** END   CHG *** 01/27/00 **************************************
     O**
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
     O*** SUMMARY REPORT
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
     O**********        09   UPUSNM              67
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
     O                  09   UPUSNM              67
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
      **** BEGIN ADD **** 05/12/98 ************************************
     O          E            ROUTE1      2  2
     O                                           19 'SYSTEM:'
     O                       QQSYS               21
     O          E            ROUTE1         2
     O                                           19 'SUBSCRIBER:'
     O                       QQSUB#              25
     O          E            ROUTE1         2
     O                                           19 'GROUP:'
     O                       QQFND               23
     O          E            ROUTE1         2
     O                                           19 'YEAR/CASE:'
     O                       QQFYR               23
     O                       QQCASE              32
     O          E            ROUTE1         2
     O                                           19 'DATE RANGE:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFRDT    28 '  /  /  '
     O**********                         31 'TO'
     O**********               XXTODT    40 '  /  /  '
     O                       X1FRDT              30 '  /  /    '
     O                                           33 'TO'
     O                       X1TODT              44 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TYPE:'
     O                       QQRTYP              23
      **** END   ADD **** 05/12/98 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE1
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
      **** BEGIN CHG **** 05/07/98 ************************************
     O********************************** 57 'INC. 1997'
     O***                                46 'INC.'
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      **** END   CHG **** 05/07/98 ************************************
     OQSYSPRT1  E            HEAD1          1 06
     O*** BEGIN CHG *** 03/24/00 **************************************
     O**********               DSDATE     8 '  /  /  '
     O                       DSDATE              10 '  /  /    '
     O*** END   CHG *** 03/24/00 **************************************
     O                       XXCONM              84
     O                       DSTIME             127 '  :  :  '
     O          E            HEAD1          1
     O                       ORPNAM              10
     O                       YYTITL              94
     O                       PAGE1         Z    127
     O                                          121 'PAGE:'
     O*
     O          E    13      HEAD1          1
     O                                           54 'FROM:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFRDTY   63
     O**********                         67 'TO:'
     O**********               XXTODTY   76
     O                       X1FRDT              65 '  /  /    '
     O                                           69 'TO:'
     O                       X1TODT              80 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O*
     O          E   N13 14   HEAD1          1
     O                                           54 'FROM:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFR2 Y   63
     O**********                         67 'TO:'
     O**********               XXTO2 Y   76
     O                       X1FR2               65 '  /  /    '
     O                                           69 'TO:'
     O                       X1TO2               80 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            HEAD1          2
     O*
     O          E            HEAD12         2
      **** BEGIN CHG **** 05/07/98 ************************************
     O*********************************** 7 'PRODUCT'
     O                                            7 'SYSTEM'
      **** END   CHG **** 05/07/98 ************************************
     O                                           20 'SUBSCRIBER'
     O                                           28 'GROUP'
     O                                           46 'INV AMT'
     O                                           62 '0 - 30'
     O                                           78 '31 - 60'
     O                                           94 '61 - 90'
     O                                          110 '91+'
     O                                          126 'BALANCE'
     O          E            GRPTL1         2
     O                       XXSYS                4
     O*** BEGIN CHG *** 10/02/00 **************************************
     O**********               XXSUB#    17
     O**********               XXFND     27
     O                       ZZSUB#              17
     O                       ZZFND               27
     O*** END   CHG *** 10/02/00 **************************************
     O                       XXIA2         J     46
     O                       XX302         J     62
     O                       XX602         J     78
     O                       XX902         J     94
     O                       XX912         J    110
     O                       XXBAL2        J    126
     O          E            SBTOT1         2
     O                                           32 'SUBSCRIBER SUBTOTAL:'
     O                       XXIA3         J     46
     O                       XX303         J     62
     O                       XX603         J     78
     O                       XX903         J     94
     O                       XX913         J    110
     O                       XXBAL3        J    126
     O          E            PRTOT1         2
      **** BEGIN CHG **** 05/07/98 ************************************
     O********************************** 32 'PRODUCT TOTAL:'
     O                                           32 'SYSTEM TOTAL:'
      **** END   CHG **** 05/07/98 ************************************
     O                       XXIA4         J     46
     O                       XX304         J     62
     O                       XX604         J     78
     O                       XX904         J     94
     O                       XX914         J    110
     O                       XXBAL4        J    126
     O          E            GRTOT1      1  2
     O                                           32 'GRAND TOTAL:'
     O                       XXIA5         J     46
     O                       XX305         J     62
     O                       XX605         J     78
     O                       XX905         J     94
     O                       XX915         J    110
     O                       XXBAL5        J    126
     O**
     O          E            ERRMS1      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
     O**
** NM - NO CASE/NAME MESSAGE
NO CASE/NAME ON FILE
