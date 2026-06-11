     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      //*** Beg Add ***** 12/16/24 ************************************
     H bnddir('STBNDDIR')
      //*** End Add ***** 12/16/24 ************************************
     H*****************************************************************
     H/TITLE W9201R - A/R BATCH EDIT
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM CREATES A REPORT LISTING ALL A/R BATCH ENTRIES   *
     H* THAT HAVE NOT YET BEEN TRANSFRRED TO THE A/R FILE.  THE       *
     H* PROGRAM WILL PRINT ALL OR ONE BATCHES DEPENDING ON WHAT THE   *
     H* USER HAS SPECIFIED ON THE PROMPT SCREEN                       *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W9201C                                                      *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER: KENNETH KENNEDY                                   *
     H* CREATED: 07/24/91               PROJECT: MD021                *
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   40  INDICATOR IS SET IF REPORT IS TO BE RUN FOR ALL         *
     H*       BATCH NUMBERS.  INDICATOR IS ZERO IF REPORT IS BEING    *
     H*       RUN FOR A PARTICULAR BATCH NUMBER.                      *
     H*   60  GENERAL CHAIN INDICATOR                                 *
     H*   OF  OVERFLOW INDICATOR                                      *
     H*   LR  LAST RECORD INDICATOR                                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/16/24 C:333 Project: WC2990                                *
     H*                A/R Batch: Update Locking Process.             *
     H*                                                               *
     H* 02/05/16 C:136 Support: 37550                                 *
     H*                Use Company Maintenance for EFT Batch Security *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 08/09/10 C:191 Project: 71WKU                                 *
     H*               - CHANGE TO INCLUDE RETRO BATCHES               *
     H*                                                               *
     H* 04/02/10 C:191 Project: 25WQI PRISS #25                       *
     H*               - SUPRESS LEADING ZERO FOR STATEMENT#           *
     H*                                                               *
     H* 03/15/10 C:65  Project: 25WQI PRISS #25                       *
     H*               - SHORTEN A/R DESCRIPTION TO PREVENT IT FROM    *
     H*                 RUNNING INTO THE STATEMENT #.                 *
     H*                                                               *
     H* 02/25/10 C:191 Project: 25WQI PRISS #25                       *
     H*               - DETAIL FORMAT WRONGLY PRINTED FOR 025         *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -Print 'Statement #' along with the invoice #   *
     H*                                                               *
     H* 04/03/09 C:191 SUPPORT WA045/67891                            *
     H*              - ACH BATCH BEING SKIPPED                        *
     H*                                                               *
     H* 12/17/08 C:220 SUPPORT 66418                                  *
     H*              - TRANSFERED BATCHES NOT PRINTING DETAIL.        *
     H*                                                               *
     H* 05/22/08 C:61  PROJECT 45WGS PRISS 12                         *
     H*              - Transaction date to print in header of every   *
     H*                page.                                          *
     H*              - Correct print of Totals on first page.         *
     H*              - Insure page break with print of batch totals.  *
     H*                                                               *
     H* 03/13/08 C:183 PROJECT : WC1634                               *
     H*                Remove client-flagged sort logic since sort    *
     H*                sequence will now be soft-coded at company     *
     H*                level.                                         *
     H*                                                               *
     H* 02/21/08 C:189 PROJECT : 45WGS                                *
     H*                 ADD TRANSACTION DATE TO A/R BATCH EDIT        *
     H*                                                               *
     H* 02/04/08 C:136 SUPPORT: WA136 62005                           *
     H*                CORRECT ALGORITHM THAT TOTALS CHECK AMOUNTS    *
     H*                - DISREGARD EMP AND DIV IN CHK COUNT           *
     H*                - DO NOT COUNT TRANSACTIONS WITHOUT A CHECK    *
     H*                  NUMBER AS A CHECK                            *
     H*                                                               *
     H* 11/30/07 C:136 SUPPORT: WA133 61312                           *
     H*                133 WANTS THE FIRST IN SORT ALSO               *
     H*                                                               *
     H* 10/09/07 C:65  SUPP# 60434 (109WDZ/109CJ)                     *
     H*                ADD CLIENT CONTROL TO THE FIRST IN SORT USED   *
     H*                BY 109 AND REVERT BASE BACK TO SORTING IN      *
     H*                CHECK # ORDER.                                 *
     H*                                                               *
     H* 09/27/07 C:61  PROJECT 132WFZ                                 *
     H*                Add column to indicate if employer is S/R or   *
     H*                Estimating.                                    *
     H*                                                               *
     H* 09/20/06 C:136 71WHE (EMC BACK TO BASE)                       *
     H*                SORT IN EMP# INSTEAD OF CHK# ORDER IF FLAGGED  *
     H*                IN DARTS/ADMIN CONTROL                         *
     H*                                                               *
     H* 10/20/05 C:053 PROJECT: WC1324                                *
     H*                Add SR logic.                                  *
     H*                                                               *
     H*  06/24/05 C:33  Project: 109WDZ                               *
     H*                 {109CJ}                                       *
     H*                 Change seq in which transactions print.       *
     H*                                                               *
     H*  03/03/05 C:062 PROJECT: WC1229                               *
     H*                 Add group restrictions logic.                 *
     H*                                                               *
     H* 02/22/05 C:062 PROJECT: WC1274                                *
     H*                Check ACH Batch Security.                      *
     H*                                                               *
     H*  04/11/03 C:136 PROJECT: WC1116                               *
     H*                 Display upload date in header if ACH batch    *
     H*                 file. Will need to break page by batch#       *
     H*                 when editing across batches if ACH processing *
     H*                 is enabled.                                   *
     H*                                                               *
     H*  06/21/99 C:69  Support: 15981                                *
     H*                 Changed program so that the number of checks  *
     H*                 and check total would be computed differently *
     H*                 if the check number is zero.                  *
     H*                                                               *
     H*  11/11/98 C:69  Support: 14885                                *
     H*                 Allow the same check number for different     *
     H*                 divisions; also excluded the 99999999 record  *
     H*                                                               *
     H*  01/16/98 C:69  PROJECT: WC577                                *
     H*                 CHANGED REPORT ISPLAY "NO RECORDS SELECTED"   *
     H*  01/26/96 C:48  PROJECT: WC484                                *
     H*                 FIXED CODE TO DISPLAY "NO RECORDS SELECTED"   *
     H*                 PROPERLY.                                     *
     H*                                                               *
     H*  07/31/96 C:64  PROJECT: WC527                                *
     H*                 CORRECTED LOGIC TO USE CHECK COUNTER TO COUNT *
     H*                 THE NUMBER OF CHECKS PROPERLY AND ADDED COUNT-*
     H*                 ER TO COUNT THE NUMBER OF RECORDS SELECTED    *
     H*                 PROPERLY.                                     *
     H*                                                               *
     H*  04/30/97 C:62 PROJECT WC693                                  *
     H*                MAKE SURE THAT A/R BATCH RECOGNIZES THAT EVEN  *
     H*                IF TWO ENTRIES HAVE THE SAME CHK# AND CHKAMT,  *
     H*                AS LONG AS THE TWO ENTRIES ARE DIFFERENT EMPS, *
     H*                RECOGNIZE THEM AS BEING DIFFERENT.             *
     H*                                                               *
     H*****************************************************************
      *
      ***** BEG ADD ******* 09/27/07 **********************************
      *
      * EMPLOYER POLICY YEAR
     FWDELL     IF   E           K DISK
      ***** END ADD ******* 09/27/07 **********************************
     FWMUPL     IF   E           K DISK
     F*** BEG DLT ***** 03/13/08 *********************************
     F*WDEAL4  IF  E           K        DISK
     F*********   PEA01                             KRENAMEPEA04
     F*** END DLT ***** 03/13/08 *********************************
     F*** BEG ADD ***** 12/17/08 *********************************
     FWDEAL4    IF   E           K DISK
     F                                     RENAME(PEA01:PEA04)
     F*** END ADD ***** 12/17/08 *********************************
     F***  Begin Chg  ***  06/24/05  **************************************
     F**********WDEAL   IF  E           K        DISK
     FWDEAL6    IF   E           K DISK
     F***  BEGIN ADD  ***  10/09/07  **************************************
     F                                     RENAME(PEA01:PEA06)
     FWDEAL     IF   E           K DISK
     F***  END ADD ******  10/09/07  **************************************
     F***  End   Chg  ***  06/24/05  **************************************
     FWTABL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
      *** BEG ADD ***** 04/11/03 *********************************
     FSWCPF     IF   E             DISK
      *** END ADD ***** 04/11/03 *********************************
     F*** BEGIN ADD *** 02/22/05 ***********************************************
      *** beg del ***** 02/05/16 ***********************************************
     F*** WMCBL     IF   E           K DISK
      *** end del ***** 02/05/16 ***********************************************
     F*** END   ADD *** 02/22/05 ***********************************************
      *** BEG ADD ***** 09/20/06 *********************************
     FWDEAL7    IF   E           K DISK
     F                                     RENAME(PEA01:PEA07)
      *** END ADD ***** 09/20/06 *********************************
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)

     D*
     D***  Begin Add  ***  06/24/05  **************************************
     D*** BEG CHG ***** 02/04/08 ******************************************
     D*****************   CK        999 30               CHECKS PROCESSED
     D CK              S             16    DIM(999)                             CHECKS PROCESSED
     D*** END CHG ***** 02/04/08 ******************************************
      //*** Beg Add ***** 12/16/24 ****************************************
     D inCo#           s              3  0
     D inFndVar        s              3  0  inz(0)
     D inBat#          s              9  0
     D/copy sPrLock
      //*** End Add ***** 12/16/24 ****************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D***  Begin Add  ***  06/24/05  **************************************
     D*** BEG CHG ***** 02/04/089 *****************************************
     D*********** DS
     D***********                             1  30 XXEACK
     D***********                             1   70EACHK#
     D***********                             8  162EACKAM
     D***********                            17  250EAEMP#
     D***********                            26  300EADIV
     D                 DS
     D  XXEACK                 1     16
     D  EACHK#                 1      7  0
     D  EACKAM                 8     16  2
     D*** END CHG ***** 02/04/08 ******************************************
     D***  End   Add  ***  06/24/05  **************************************
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      ***** BEG ADD ******* 09/27/07 **********************************
      *
      *
      *============================================================
      * KLIST
      *============================================================
      *
      *  EMPLOYER POLICY YEAR
      *

     C     ELKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
      *
      *  BILLING SCHEDULE CODE
      *
     C     BSKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    KKKEY            10
      *
      ***** END ADD ******* 09/27/07 **********************************
     C*** BEGIN ADD *** 02/22/05 ***********************************************
     C     CBKY          KLIST
     C                   KFLD                    XXCO#
     C*** END   ADD *** 02/22/05 ***********************************************
     C*
     C*   KEY LIST FOR THE EMPLOYER A/R BATCH FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSTAT
     C*
     C     EAKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSTAT
     C                   KFLD                    XXBAT#
     C*
     C     EAKEY4        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBAT#
     C*
     C*   KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EAFND
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
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
     C     *LIKE         DEFINE    EACO#         XXCO#
     C     *LIKE         DEFINE    EABAT#        XXBAT#
     C     *LIKE         DEFINE    EAAMT         X1AMT
     C     *LIKE         DEFINE    EACKAM        X1CKAM
     C     *LIKE         DEFINE    EAAMT         X2AMT
     C     *LIKE         DEFINE    EACKAM        X2CKAM
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    TTTYPE        XXTYPE
     C     *LIKE         DEFINE    TTKEY         XXKEY
     C     *LIKE         DEFINE    EASTAT        XXSTAT
     C***  Begin Del  ***  06/24/05  **************************************
     C********** *LIKE     DEFN EACHK#    XXCHK#
     C********** *LIKE     DEFN EACKAM    XXCKAM
     C***  End   Del  ***  06/24/05  **************************************
     C*** BEGIN ADD *** 04/30/97 **************************************
     C     *LIKE         DEFINE    EAEMP#        XXEMP#
     C     *LIKE         DEFINE    EADIV         XXDIV
     C*** END   ADD *** 04/30/97 **************************************
     C*** BEG ADD ***** 04/11/03 **************************************
     C     *LIKE         DEFINE    EABAT#        SVBAT#
     C*** END ADD ***** 04/11/03 **************************************
     C*** BEGIN ADD *** 03/03/05 **************************************
     C     *LIKE         DEFINE    *IN99         FAILED
     C     *LIKE         DEFINE    *IN99         MULTIG
     C*** END   ADD *** 03/03/05 **************************************
     C***  Begin Add  ***  06/24/05  **************************************
     C     *LIKE         DEFINE    EAFND         XXFND
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQBAT#            9
     C*** BEGIN ADD *** 02/22/05 **************************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C*** END   ADD *** 02/22/05 **************************************
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
     C*** BEG  ADD***** 02/21/08 **************************************
      ***** BEG DEL ******* 05/22/08 **********************************
     C*********  DTCYM1    PLIST
     C*********            PARM           SRDTI
     C*********            PARM '*CYMD'   SRFFMT
     C*********            PARM '*MDYY  ' SRTFMT
     C*********            PARM           SRDTO
     C*********            PARM           SRERR
      ***** END DEL ******* 05/22/08 **********************************
     C*** END  ADD***** 02/21/08 **************************************
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
      ***** BEG ADD ******* 05/22/08 **********************************
     C                   MOVE      'Y'           XX1ST             1
      ***** END ADD ******* 05/22/08 **********************************
      ***** BEG ADD ******* 09/27/07 **********************************
     C                   MOVE      *ALL'_'       LINE            198
      ***** END ADD ******* 09/27/07 **********************************
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQBAT#        XXBAT#
     C                   TIME                    DSDTTM
     C     XXBAT#        COMP      *ZEROS                                 40
      *
      *** BEG ADD ***** 04/11/03 ************************************
     C                   READ      SWCPF                                  60
     C     *IN60         IFEQ      *OFF
     C     WCFL24        ANDEQ     'Y'
     C                   SETON                                        41
     C                   ELSE
     C                   SETOFF                                       41
     C                   ENDIF
      *** END ADD ***** 04/11/03 ************************************
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C*** BEG ADD ***** 03/13/08 ************************************
     C                   SETOFF                                       313233
     C                   SELECT
     C     COARSQ        WHENEQ    'C'
     C                   SETON                                        31
     C     COARSQ        WHENEQ    'E'
     C                   SETON                                        32
     C                   OTHER
     C                   SETON                                        33
     C                   ENDSL
     C*** END ADD ***** 03/13/08 ************************************
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        CONAME
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
     C     UPKEY         CHAIN     PUP01                              60
     C*** BEGIN ADD *** 03/03/05 **************************************
     C                   MOVEL     *BLANKS       XXGRST            1
     C*
     C* DOES MULTI GROUP RESTRICTIONS APPLY
     C*
     C                   CALL      'SA008R'
     C                   PARM      UPCO#         $$CO#
     C                   PARM      Q1USER        $$USR
     C                   PARM      'W'           $$APPL
     C     MULTIG        PARM      *OFF          $$RTCD
     C*** END   ADD *** 03/03/05 **************************************
     C*** BEGIN ADD *** 02/22/05 **************************************
     C*
     C* CHECK THE ACH BATCH SECURITY SO WE CAN DETERMINE IF WE WANT
     C* TO SKIP ACH BATCHES
     C*
     C                   EXSR      CHKSEC
     C*** END   ADD *** 02/22/05 **************************************
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  ORTITL        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      60            SRDLEN            3 0
     C                   MOVEL     SRDFLD        ORTITL
      ***** Beg Del ***** 08/26/14 *************************************************
     C***********        EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *************************************************
     C                   END
      *** BEG DEL ***** 04/11/03 **************************
     C******************   EXCPT
      *** END DEL ***** 04/11/03 **************************
     C*
      *** BEG ADD ***** 09/20/06 **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C                   MOVE      *BLANKS       XXSTAT
     C     *IN40         IFEQ      *OFF
     C*** BEG DLT ***** 03/13/08 **********************************
     C********** EAKEY4    CHAINWDEAL4               50
     C**N50*****           MOVE EASTAT    XXSTAT
     C*** END DLT ***** 03/13/08 **********************************
     C*** BEG ADD ***** 12/17/08 *********************************
     C***************** TO FIND TRANSFER STATUS ******************
     C     EAKEY4        CHAIN     WDEAL4                             50
     C  N50              MOVE      EASTAT        XXSTAT
     C*** END ADD ***** 12/17/08 *********************************
      *** BEG ADD ***** 09/20/06 **********************************
     C*** BEG DLT ***** 03/13/08 **********************************
     C********** XXCLT#    IFEQ '071'
     C**********           MOVE 'Y'       XXSORT  1
     C********** EAKEY2    CHAINPEA07                50
     C**********           ELSE
     C**********           MOVE ' '       XXSORT
     C*** END DLT ***** 03/13/08 **********************************
      *** END ADD ***** 09/20/06 **********************************
     C***  BEGIN CHG  ***  10/09/07  **************************************
     C********** EAKEY2    CHAINPEA01                50
     C*** BEG DLT ***** 03/13/08 **********************************
     C********** XXCLT#    IFEQ '109'
     C*** END DLT ***** 03/13/08 **********************************
      *** BEG ADD ***** 11/30/07 *************************************
     C*** BEG DLT ***** 03/13/08 **********************************
     C********** XXCLT#    OREQ '133'
     C*** END DLT ***** 03/13/08 **********************************
      *** END ADD ***** 11/30/07 *************************************
     C*** BEG DLT ***** 03/13/08 **********************************
     C********** EAKEY2    CHAINPEA06                50
     C**********           ELSE
     C********** EAKEY2    CHAINPEA01                50
     C**********           ENDIF
     C*** END DLT ***** 03/13/08 **********************************
     C***  END CHG ******  10/09/07  **************************************
      *** BEG ADD ***** 09/20/06 **********************************
     C*** BEG DLT ***** 03/13/08 **********************************
     C**********           ENDIF
     C*** END DLT ***** 03/13/08 **********************************
      *** END ADD ***** 09/20/06 **********************************
     C*** BEG ADD ***** 03/13/08 **********************************
      *** Here is the new logic **********************************
     C                   SELECT
     C     *IN31         WHENEQ    '1'
     C     EAKEY2        SETLL     PEA01
     C     EAKEY2        READE     PEA01                                  50
     C     *IN32         WHENEQ    '1'
     C     EAKEY2        SETLL     PEA07
     C     EAKEY2        READE     PEA07                                  50
     C     *IN33         WHENEQ    '1'
     C     EAKEY2        SETLL     PEA06
     C     EAKEY2        READE     PEA06                                  50
     C                   ENDSL
     C*** END ADD ***** 03/13/08 **********************************
     C                   ELSE
      *** BEG ADD ***** 09/20/06 **********************************
     C*** BEG DLT ***** 03/13/08 **********************************
     C********** XXCLT#    IFEQ '071'
     C************         MOVE 'Y'       XXSORT
     C********** EAKEY     CHAINPEA07                50
     C**********           ELSE
     C************         MOVE ' '       XXSORT
     C*** END DLT ***** 03/13/08 **********************************
      *** END ADD ***** 09/20/06 **********************************
     C***  BEGIN CHG  ***  10/09/07  **************************************
     C********** EAKEY     CHAINPEA01                50
     C*** BEG DLT ***** 03/13/08 **********************************
     C********** XXCLT#    IFEQ '109'
     C*** END DLT ***** 03/13/08 **********************************
      *** BEG ADD ***** 11/30/07 *************************************
     C*** BEG DLT ***** 03/13/08 **********************************
     C********** XXCLT#    OREQ '133'
     C*** END DLT ***** 03/13/08 **********************************
      *** END ADD ***** 11/30/07 *************************************
     C*** BEG DLT ***** 03/13/08 **********************************
     C********** EAKEY     CHAINPEA06                50
     C**********           ELSE
     C********** EAKEY     CHAINPEA01                50
     C**********           ENDIF
     C*** END DLT ***** 03/13/08 **********************************
     C***  END CHG ******  10/09/07  **************************************
      *** BEG ADD ***** 09/20/06 **********************************
     C*** BEG DLT ***** 03/13/08 **********************************
     C**********           ENDIF
     C*** END DLT ***** 03/13/08 **********************************
     C*** BEG CHG ***** 03/13/08 **********************************
     C                   SELECT
     C     *IN31         WHENEQ    '1'
     C     EAKEY         SETLL     PEA01
     C     EAKEY         READE     PEA01                                  50
     C     *IN32         WHENEQ    '1'
     C     EAKEY         SETLL     PEA07
     C     EAKEY         READE     PEA07                                  50
     C     *IN33         WHENEQ    '1'
     C     EAKEY         SETLL     PEA06
     C     EAKEY         READE     PEA06                                  50
     C                   ENDSL
     C*** END CHG ***** 03/13/08 **********************************
      *** END ADD ***** 09/20/06 **********************************
     C                   ENDIF
     C*
      *** BEG ADD ***** 04/11/03 **************************
     C   41EAACHF        IFEQ      'A'
     C     EAACHD        ANDNE     *ZERO
     C                   SETON                                        42
     C                   Z-ADD     EAACHD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACHD            8 0
     C                   ELSE
     C                   SETOFF                                       42
     C                   ENDIF                                                  EAACHF <> 'A'
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C* SUPRESS PRINTING OF TRANSACTION DATE FOR RETRO BATCHES
     C     EAACHF        IFEQ      'R'
     C                   SETON                                        59
     C                   ELSE
     C                   SETOFF                                       59
     C                   ENDIF
     C* DISPLAY VERBIAGE TO IDENTIFY BATCHES
     C                   SELECT
     C     EAACHF        WHENEQ    'A'
     C                   MOVEL(P)  'ACH'         XXBCTY           10
     C     EAACHF        WHENEQ    'D'
     C                   MOVEL(P)  'DEDUCT'      XXBCTY
     C                   MOVE      'IBLE'        XXBCTY
     C     EAACHF        WHENEQ    'L'
     C                   MOVEL(P)  'LOCKBOX'     XXBCTY
     C     EAACHF        WHENEQ    'R'
     C                   MOVEL(P)  'RETRO'       XXBCTY
     C     EAACHF        WHENEQ    'S'
     C                   MOVEL(P)  'S/R UPL'     XXBCTY
     C                   MOVE      'OAD'         XXBCTY
     C                   OTHER
     C                   MOVEL(P)  'REGULAR'     XXBCTY
     C                   ENDSL
     C*** END   ADD ****** 08/09/10 ************************************
     C*
      ***** BEG ADD ******* 05/22/08 **********************************
     C                   MOVE      *ON           *INOF
      ***** END ADD ******* 05/22/08 **********************************
      ***** BEG DEL ******* 05/22/08 **********************************
     C*********            EXCPTHEAD
     C*** BEG  ADD***** 02/21/08 **************************************
     C*********43          EXCPTHEAD1
     C*********            EXCPTHEAD2
     C*** END**ADD***** 02/21/08 **************************************
      ***** END DEL ******* 05/22/08 **********************************
      *** END ADD ***** 04/11/03 **************************
     C*
     C                   MOVE      EABAT#        XXBAT#
     C***  Begin Del  ***  06/24/05  **************************************
     C**********           Z-ADD*ZEROS    XXCHK#
     C**********           Z-ADD*ZEROS    XXCKAM
     C***  End   Del  ***  06/24/05  **************************************
     C                   Z-ADD     *ZEROS        X1CKCT            5 0
     C                   Z-ADD     *ZEROS        X2CKCT            5 0
     C                   Z-ADD     *ZEROS        XXRSCT            5 0
     C*** BEGIN ADD *** 04/30/97 **************************************
     C                   Z-ADD     *ZEROS        XXEMP#
     C                   Z-ADD     *ZEROS        XXDIV
     C*** END   ADD *** 04/30/97 **************************************
     C*
     C     *IN50         DOWEQ     '0'
      ***** BEG ADD ******* 05/22/08 **********************************
     C     EATRDT        IFEQ      *ZERO
     C                   MOVE      *OFF          *IN71
     C                   ELSE
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C                   MOVE      *ON           *IN71
     C                   ENDIF
      ***** END ADD ******* 05/22/08 **********************************
     C*** BEG ADD *** 10/20/05 **************************************
     C*
     C                   MOVE      *ON           ACHPAS            1
     C*
     C*** CONDITION PROCESSING FOR 'A' & 'S' ON SECURITY CHECK
     C*
     C                   SELECT
     C     EAACHF        WHENEQ    'A'
     C*** BEGIN CHG ****** 04/03/09 ************************************
     C********** SKIP      ANDEQ*OFF
     C     SKIP          ANDEQ     *ON
     C*** END   CHG ****** 04/03/09 ************************************
     C                   MOVE      *OFF          ACHPAS
     C*
     C     EAACHF        WHENEQ    'S'
     C*** BEGIN CHG ****** 04/03/09 ************************************
     C********** SKIP1     ANDEQ*OFF
     C     SKIP1         ANDEQ     *ON
     C*** END   CHG ****** 04/03/09 ************************************
     C                   MOVE      *OFF          ACHPAS
     C*
     C*** LET 'L' & ' ' PROCESS WITHOUT CONDITIONS
     C*
     C                   ENDSL
     C*
     C     ACHPAS        IFEQ      *ON
     C*** END ADD *** 10/20/05 **************************************
     C*** BEGIN ADD *** 02/22/05 **************************************
     C*** BEG DEL *** 10/20/05 **************************************
     C*******    EAACHF    IFNE 'A'
     C*******    SKIP      OREQ *OFF
     C*** END DEL *** 10/20/05 **************************************
     C*** END   ADD *** 02/22/05 **************************************
     C**** Begin add *** 11/11/98 ************************************
     C     EACKAM        IFNE      *ALL'9'
     C**** End   add *** 11/11/98 ************************************
     C*** BEGIN ADD *** 03/03/05 **************************************
     C*
     C* CHECK GROUP/EMPLOYER/DIC RESTRICTIONS
     C*
     C                   MOVEL     *BLANKS       X1GRST            1
     C*
     C     MULTIG        IFEQ      *ON
     C*
     C                   CALL      'SA008R'
     C                   PARM      UPCO#         $$CO#             3 0
     C                   PARM      Q1USER        $$USR            10
     C                   PARM      'W'           $$APPL            1
     C     FAILED        PARM      *OFF          $$RTCD            1
     C                   PARM      ' '           $$AUTH            1
     C                   PARM      EAFND         $$FND             3 0
     C                   PARM      EAEMP#        $$EMP#            9 0
     C*
     C     FAILED        IFEQ      *ON
     C                   MOVEL     'Y'           X1GRST
     C                   ENDIF
     C*
     C                   ELSE
     C     UPXFND        IFEQ      'Y'
     C     UPFND         ANDNE     EAFND
     C     UPXEMP        OREQ      'Y'
     C     UPFND         ANDNE     EAFND
     C     UPXEMP        OREQ      'Y'
     C     UPEMP#        ANDNE     EAEMP#
     C     UPXEMP        OREQ      'Y'
     C     UPDIV         ANDNE     EADIV
     C                   MOVEL     'Y'           X1GRST
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 03/03/05 **************************************
     C*
      *** BEG CHG ***** 04/11/03 **************************************
     C********** EABAT#    IFNE XXBAT#
     C     EABAT#        IFNE      SVBAT#
     C                   Z-ADD     EABAT#        SVBAT#
      *** END CHG ***** 04/11/03 **************************************
     C*
      ***** BEG DEL ******* 05/22/08 **********************************
     C*********  *INOF     IFEQ '1'
     C*********            EXCPTHEAD
     C*** BEG  ADD***** 02/21/08 **************************************
     C*********43          EXCPTHEAD1
     C*********            EXCPTHEAD2
      ***** END DEL ******* 05/22/08 **********************************
     C*********            MOVE '0'       *INOF
     C*********            END
     C*** END  ADD***** 02/21/08 **************************************
     C*
     C     X1CKCT        IFNE      *ZEROS
      ***** BEG ADD ******* 05/22/08 **********************************
     C     XX1ST         ANDEQ     'N'
      ***** END ADD ******* 05/22/08 **********************************
     C***  Begin Chg  ***  11/19/09  **************************************
     C**********           EXCPTTOT1
     C                   EXSR      SRT1
     C***  End   Chg  ***  11/19/09  **************************************
      ***** BEG ADD ******* 05/22/08 **********************************
     C                   MOVE      *ON           *INOF
      ***** END ADD ******* 05/22/08 **********************************
     C                   END
     C*** BEG ADD **** 02/04/08 ***************************************
     C     X1CKCT        IFEQ      *ZEROS
      ***** BEG ADD ******* 05/22/08 **********************************
     C     XX1ST         ANDEQ     'N'
      ***** END ADD ******* 05/22/08 **********************************
     C***  Begin Chg  ***  11/19/09  **************************************
     C**********           EXCPTTOT1A
     C                   EXSR      SRT1A
     C***  End   Chg  ***  11/19/09  **************************************
      ***** BEG ADD ******* 05/22/08 **********************************
     C                   MOVE      *ON           *INOF
      ***** END ADD ******* 05/22/08 **********************************
     C                   END
     C*** END ADD **** 02/04/08 ***************************************
     C*** BEGIN ADD *** 03/03/05 **************************************
     C     XXGRST        IFEQ      'Y'
     C***  Begin Chg  ***  11/19/09  **************************************
     C**********           EXCPTTOT1B
     C                   EXSR      SRT1B
     C***  End   Chg  ***  11/19/09  **************************************
     C                   ENDIF
     C*** END   ADD *** 03/03/05 **************************************
     C*
      *** BEG ADD ***** 04/11/03 ************************************
     C   41RD#1          IFEQ      *ON
     C     X1CKCT        ANDNE     *ZEROS
     C                   SETON                                        OF
      *
     C     EAACHF        IFEQ      'A'
     C     EAACHD        ANDNE     *ZERO
     C                   SETON                                        42
     C                   Z-ADD     EAACHD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACHD            8 0
     C                   ELSE
     C                   SETOFF                                       42
     C                   ENDIF                                                  EAACHF <> 'A'
      *
      ***** BEG DEL ******* 05/22/08 **********************************
     C*********            EXCPTHEAD
     C****BEG**ADD***** 02/21/08 **************************************
     C*********43          EXCPTHEAD1
     C*********            EXCPTHEAD2
     C****END**ADD***** 02/21/08 **************************************
      ***** END DEL ******* 05/22/08 **********************************
     C                   ENDIF                                                  RD#1
      *
     C                   MOVE      *ON           RD#1              1
      *** END ADD ***** 04/11/03 ************************************
     C*
     C*** BEGIN ADD *** 03/03/05 **************************************
     C                   MOVEL     X1GRST        XXGRST
     C*
     C     X1GRST        IFNE      'Y'
     C*** END   ADD *** 03/03/05 **************************************
     C                   ADD       X1CKCT        X2CKCT
     C                   ADD       X1CKAM        X2CKAM
     C                   ADD       X1AMT         X2AMT
     C*** BEGIN ADD *** 03/03/05 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/03/05 **************************************
     C                   Z-ADD     *ZEROS        X1CKAM
     C                   Z-ADD     *ZEROS        X1AMT
     C                   MOVE      EABAT#        XXBAT#
     C                   Z-ADD     *ZEROS        X1CKCT
     C***  Begin Add  ***  06/24/05  **************************************
     C                   MOVEA     *BLANKS       CK
     C                   Z-ADD     *ZEROS        L                 4 0
     C***  End   Add  ***  06/24/05  **************************************
     C                   END
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN50         IFEQ      '0'
     C     TTSTAT        ANDNE     'I'
     C                   MOVEL     TTTYPE        XXTYPE
     C                   ELSE
     C                   MOVE      *BLANKS       XXTYPE
     C                   END
     C*
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPODT            6 0
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C*** END   ADD ****** 08/09/10 ************************************
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNSHNM        XXFNNM           20
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXEMNM           19
     C                   ELSE
     C                   MOVE      *BLANKS       XXEMNM
     C                   END
     C                   MOVEL     EADESC        XXDESC           25
     C*** BEGIN ADD ***  03/15/10  ************************************
     C                   MOVEL     EADESC        X1DESC           24
     C*** END ADD *****  03/15/10  ************************************
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C                   MOVEL     EADESC        X2DESC           19
     C*** END   ADD ****** 08/09/10 ************************************
     C*
      ***** BEG DEL ******* 05/22/08 **********************************
     C*********  *INOF     IFEQ '1'
     C*********            EXCPTHEAD
     C*** BEG  ADD***** 02/21/08 **************************************
     C*********43          EXCPTHEAD1
     C*********            EXCPTHEAD2
     C****END**ADD***** 02/21/08 **************************************
     C*********            MOVE '0'       *INOF
     C*********            END
      ***** END DEL ******* 05/22/08 **********************************
     C*
     C*** BEGIN ADD *** 03/03/05 **************************************
     C     XXGRST        IFNE      'Y'
     C                   MOVEL     X1GRST        XXGRST
     C                   ENDIF
      ***** BEG ADD ******* 09/27/07 **********************************
      *
      * Determine if S/R or Estimating Employer
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     ELBSCD        KKKEY
     C     BSKY          CHAIN     PBS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      BSSEFL        XXSEFL            1
     C                   ELSE
     C                   MOVE      *BLANKS       XXSEFL            1
     C                   ENDIF                                                  BSKY
      *
     C                   ENDIF                                                  ELKY
      *
      ***** END ADD ******* 09/27/07 **********************************
     C*
     C     X1GRST        IFNE      'Y'
      ***** BEG ADD ******* 05/22/08 **********************************
     C***  Begin Chg  ***  11/19/09  **************************************
     C***OF*****           EXCPTHEAD
     C   OF              EXSR      SRHEAD
     C***  End   Chg  ***  11/19/09  **************************************
     C   OF              MOVE      *OFF          *INOF
      ***** END ADD ******* 05/22/08 **********************************
     C*** END   ADD *** 03/03/05 **************************************
     C***  Begin Chg  ***  11/19/09  **************************************
     C**********           EXCPTDETAIL
     C                   EXSR      SRDTL
     C***  End   Chg  ***  11/19/09  **************************************
     C*** BEGIN ADD *** 03/03/05 **************************************
      ***** BEG ADD ******* 05/22/08 **********************************
     C                   MOVE      'N'           XX1ST
      ***** END ADD ******* 05/22/08 **********************************
     C                   ENDIF
     C*** END   ADD *** 03/03/05 **************************************
     C*
     C                   ADD       1             XXRSCT
     C*
     C*
     C*
     C*** BEGIN ADD *** 03/03/05 **************************************
     C     X1GRST        IFNE      'Y'
     C*** END   ADD *** 03/03/05 **************************************
     C***  Begin Chg  ***  06/24/05  **************************************
     C****Begin*add *** 06/21/99 **************************************
     C********** EACHK#    IFNE *ZEROS
     C****End***add *** 06/21/99 **************************************
     C********** EACHK#    IFNE XXCHK#
     C********** EACKAM    ORNE XXCKAM
     C****BEGIN*ADD *** 04/30/97 **************************************
     C****Begin*dlt *** 06/21/99 **************************************
     C********** EAEMP#    ORNE XXEMP#
     C****End***dlt *** 06/21/99 **************************************
     C****Begin*dlt *** 11/11/98 **************************************
     C********** EADIV     ORNE XXDIV
     C****End***dlt *** 11/11/98 **************************************
     C****END***ADD *** 04/30/97 **************************************
     C**********           ADD  1         X1CKCT
     C**********           ADD  EACKAM    X1CKAM
     C**********           Z-ADDEACHK#    XXCHK#
     C**********           Z-ADDEACKAM    XXCKAM
     C****BEGIN*ADD *** 04/30/97 **************************************
     C**********           Z-ADDEAEMP#    XXEMP#
     C**********           Z-ADDEADIV     XXDIV
     C****END***ADD *** 04/30/97 **************************************
     C**********           END
     C****Begin*add *** 06/21/99 **************************************
     C**********           ELSE                            EACHK# = 0
     C********** EACHK#    IFNE XXCHK#
     C********** EACKAM    ORNE XXCKAM
     C********** EAEMP#    ORNE XXEMP#
     C********** EADIV     ORNE XXDIV
     C**********           ADD  1         X1CKCT
     C**********           ADD  EACKAM    X1CKAM
     C**********           Z-ADDEACHK#    XXCHK#
     C**********           Z-ADDEACKAM    XXCKAM
     C**********           Z-ADDEAEMP#    XXEMP#
     C**********           Z-ADDEADIV     XXDIV
     C**********           ENDIF
     C**********           ENDIF
     C****End***add *** 06/21/99 **************************************
      *
      * LOOK IN ARRAY CK TO SEE IF THIS CHECK HAS ALREADY BEEN ADDED
      * INTO THE TOTALS. IF NOT, MAKE AN ENTRY IN CK FOR THIS CHECK,
      * AND ACCUMULATE  IT INTO TOTALS.
      *
      *** BEG ADD ***** 02/04/08 **************************************
     C     EACHK#        IFNE      *ZERO
      *** END ADD ***** 02/04/08 **************************************
      *
     C     XXEACK        LOOKUP    CK                                     60
     C*
     C     *IN60         IFEQ      '0'
      *
     C     L             IFLT      999
     C                   ADD       1             L
      *
     C                   ELSE
      *
     C                   Z-ADD     1             L
     C                   ENDIF
      *
     C                   MOVE      XXEACK        CK(L)
     C                   ADD       1             X1CKCT
     C                   ADD       EACKAM        X1CKAM
     C                   Z-ADD     EAEMP#        XXEMP#
     C                   Z-ADD     EADIV         XXDIV
     C                   ENDIF
     C***  End   Chg  ***  06/24/05  **************************************
      *
      *** BEG ADD ***** 02/04/08 **************************************
     C                   ENDIF                                                  EACHK#    IFNE *ZERO
      *** END ADD ***** 02/04/08 **************************************
      *
     C                   ADD       EAAMT         X1AMT
     C*** BEGIN ADD *** 03/03/05 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/03/05 **************************************
     C*** Begin add *** 11/11/98 **************************************
     C                   ENDIF
     C*** End   add *** 11/11/98 **************************************
     C*** BEGIN ADD *** 02/22/05 **************************************
     C                   ENDIF
     C*** END   ADD *** 02/22/05 **************************************
     C*
      *** BEG ADD ***** 09/20/06 **********************************
     C*** BEG DLT ***** 03/13/08 **********************************
     C********** XXSORT    IFEQ 'Y'
     C***40***** EAKEY     READEPEA07                    50
     C**N40***** EAKEY2    READEPEA07                    50
     C**********           ELSE
     C*** END DLT ***** 03/13/08 **********************************
      *** END ADD ***** 09/20/06 **********************************
     C***  BEGIN CHG  ***  10/09/07  **************************************
     C***40***   EAKEY     READEPEA01                    50
     C**N40***   EAKEY2    READEPEA01                    50
     C*** BEG DLT ***** 03/13/08 **********************************
     C*********  XXCLT#    IFEQ '109'
     C*** END DLT ***** 03/13/08 **********************************
      *** BEG ADD ***** 11/30/07 *************************************
     C*** BEG DLT ***** 03/13/08 **********************************
     C*********  XXCLT#    OREQ '133'
     C*** END DLT ***** 03/13/08 **********************************
      *** END ADD ***** 11/30/07 *************************************
     C*** BEG DLT ***** 03/13/08 **********************************
     C***40****  EAKEY     READEPEA06                    50
     C**N40****  EAKEY2    READEPEA06                    50
     C*********            ELSE
     C***40****  EAKEY     READEPEA01                    50
     C**N40****  EAKEY2    READEPEA01                    50
     C**********           ENDIF
     C*** END DLT ***** 03/13/08 **********************************
     C***  END CHG ******  10/09/07  **************************************
      *** BEG ADD ***** 09/20/06 **********************************
     C*** BEG DLT ***** 03/13/08 **********************************
     C***********          ENDIF
     C*** END DLT ***** 03/13/08 **********************************
      *** END ADD ***** 09/20/06 **********************************
     C*** BEG CHG ***** 03/13/08 **********************************
     C                   SELECT
     C     *IN40         WHENEQ    '0'
     C                   SELECT
     C     *IN31         WHENEQ    '1'
     C     EAKEY2        READE     PEA01                                  50
     C     *IN32         WHENEQ    '1'
     C     EAKEY2        READE     PEA07                                  50
     C     *IN33         WHENEQ    '1'
     C     EAKEY2        READE     PEA06                                  50
     C                   ENDSL
      *
     C                   OTHER
     C                   SELECT
     C     *IN31         WHENEQ    '1'
     C     EAKEY         READE     PEA01                                  50
     C     *IN32         WHENEQ    '1'
     C     EAKEY         READE     PEA07                                  50
     C     *IN33         WHENEQ    '1'
     C     EAKEY         READE     PEA06                                  50
     C                   ENDSL
      *
     C                   ENDSL
     C*** END CHG ***** 03/13/08 **********************************
     C*
     C*** BEG  ADD***** 02/21/08 **************************************
      ***** BEG DEL ******* 05/22/08 **********************************
     C*********            MOVE *OFF      *IN43
     C*********  EAACHF    IFEQ 'L'
     C*********            MOVE *ON       *IN43
     C*********            Z-ADDEATRDT    SRDTI
     C*********            MOVE *BLANKS   SRDTO
     C*********            MOVE *BLANKS   SRERR
     C*********            Z-ADD0         XXDATE  80
     C*********            CALL 'SRCVTDAT'DTCYM1
     C*********            MOVELSRDTO     XXDATE
     C*********            ENDIF
      ***** END DEL ******* 05/22/08 **********************************
     C*** END  ADD***** 02/21/08 **************************************
     C                   ENDDO                                                  *IN50
     C*
     C*  CHECK TO SEE IF NO RECORDS WERE READ
     C*
     C     XXRSCT        IFEQ      *ZEROS
     C                   EXCEPT    NORECS
     C                   END
     C*
      ***** BEG DEL ******* 05/22/08 **********************************
     C*********  *INOF     IFEQ '1'
     C*********            EXCPTHEAD
     C****BEG**ADD***** 02/21/08 **************************************
     C*********43          EXCPTHEAD1
     C*********            EXCPTHEAD2
     C****END**ADD***** 02/21/08 **************************************
     C*********            MOVE '0'       *INOF
     C*********            END
      ***** END DEL ******* 05/22/08 **********************************
     C*
     C     X1CKCT        IFNE      *ZEROS
      ***** BEG ADD ******* 05/22/08 **********************************
     C     XX1ST         ANDEQ     'N'
      ***** END ADD ******* 05/22/08 **********************************
     C***  Begin Chg  ***  11/19/09  **************************************
     C**********           EXCPTTOT1
     C                   EXSR      SRT1
     C***  End   Chg  ***  11/19/09  **************************************
      ***** BEG ADD ******* 05/22/08 **********************************
     C                   MOVE      *ON           *INOF
      ***** END ADD ******* 05/22/08 **********************************
     C                   END
     C*** BEG ADD **** 02/04/08 ***************************************
     C     X1CKCT        IFEQ      *ZEROS
      ***** BEG ADD ******* 05/22/08 **********************************
     C     XX1ST         ANDEQ     'N'
      ***** END ADD ******* 05/22/08 **********************************
     C***  Begin Chg  ***  11/19/09  **************************************
     C**********           EXCPTTOT1A
     C                   EXSR      SRT1A
     C***  End   Chg  ***  11/19/09  **************************************
      ***** BEG ADD ******* 05/22/08 **********************************
     C                   MOVE      *ON           *INOF
      ***** END ADD ******* 05/22/08 **********************************
     C                   END
     C*** END ADD **** 02/04/08 ***************************************
     C*** BEGIN ADD *** 03/03/05 **************************************
     C     XXGRST        IFEQ      'Y'
     C***  Begin Chg  ***  11/19/09  **************************************
     C**********           EXCPTTOT1B
     C                   EXSR      SRT1B
     C***  End   Chg  ***  11/19/09  **************************************
     C                   ENDIF
     C*** END   ADD *** 03/03/05 **************************************
     C*
     C                   ADD       X1CKCT        X2CKCT
     C                   ADD       X1CKAM        X2CKAM
     C                   ADD       X1AMT         X2AMT
     C*
      ***** BEG DEL ******* 05/22/08 **********************************
     C*********  *INOF     IFEQ '1'
     C*********            EXCPTHEAD
     C*** BEG  ADD***** 02/21/08 **************************************
     C*********43          EXCPTHEAD1
     C*********            EXCPTHEAD2
      ******END DEL ******* 05/22/08 **********************************
     C*********            MOVE '0'       *INOF
     C*********            END
     C*** END  ADD***** 02/21/08 **************************************
     C*
     C     X2CKCT        IFNE      *ZEROS
     C***  Begin Chg  ***  11/19/09  **************************************
     C**********           EXCPTTOT2
     C                   EXSR      SRT2
     C***  End   Chg  ***  11/19/09  **************************************
     C                   END
     C*
     C*** BEG ADD **** 02/04/08 ***************************************
     C     X2CKCT        IFEQ      *ZEROS
     C***  Begin Chg  ***  11/19/09  **************************************
     C**********           EXCPTTOT2A
     C                   EXSR      SRT2A
     C***  End   Chg  ***  11/19/09  **************************************
     C                   END
     C*** END ADD **** 02/04/08 ***************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
      //*** Beg Add ***** 12/16/24 ************************************
       If QQBAT# <> *Blanks;
          inCo# = %Dec(QQCo#:3:0);
          inBat# = %Dec(QQBat#:9:0);
          ClearPolicyLock(inCo#:inFndVar:inBat#);
       Endif;
      //*** End Add ***** 12/16/24 ************************************
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*** BEGIN ADD *** 02/22/05 **************************************
     C*================================================================
     C* CHKSEC - Checks to see if there is sufficient security
     C*================================================================
     C     CHKSEC        BEGSR
     C*
     C                   MOVE      *OFF          SKIP              1
     C                   CALL      'WA001R'      COMPL
     C*
      *** beg chg ***** 02/05/16 ***************************
     C*****CBKY**        CHAIN     PCB01                              99
     C******IN99*        IFEQ      *OFF
     C*****CBACHL        IFNE      *ZEROS
     C*****CBACHT        ORNE      *BLANKS
     C***********
     C***********        MOVE      ##LVL         ##ULVL
     C***********        MOVE      CBACHL        ##CLVL
     C***********
     C***********        CALL      'SRCCSC'
     C***********        PARM      ##TYPE        ##UTYP            1
     C***********        PARM                    ##ULVL            3
     C***********        PARM      CBACHT        ##CTYP            1
     C***********        PARM                    ##CLVL            3
     C***********        PARM      '1'           ##EXEC            1
     C***********        PARM      ' '           ##RES             1
     C*****##RES*        IFEQ      *ON
     C***********        MOVE      *ON           SKIP
     C***********        ENDIF
     C***********        ENDIF
     C***********        ENDIF

     C     COACHL        IFNE      *ZEROS
     C     COACHT        ORNE      *BLANKS
     C*
     C                   MOVE      ##LVL         ##ULVL
     C                   MOVE      COACHL        ##CLVL
     C*
     C                   CALL      'SRCCSC'
     C                   PARM      ##TYPE        ##UTYP            1
     C                   PARM                    ##ULVL            3
     C                   PARM      COACHT        ##CTYP            1
     C                   PARM                    ##CLVL            3
     C                   PARM      '1'           ##EXEC            1
     C                   PARM      ' '           ##RES             1
     C*
     C     ##RES         IFEQ      *ON
     C                   MOVE      *ON           SKIP
     C                   ENDIF
     C                   ENDIF
      *** end chg ***** 02/05/16 ***************************
     C*
      *** BEG ADD *** 10/20/05 ************************************
     C*
     C                   MOVE      *OFF          SKIP1             1
     C*
     C     XXCO#         CHAIN     PCO01                              99
     C     *IN99         IFEQ      *OFF
     C     COSRPL        IFNE      *ZEROS
     C     COSRPT        ORNE      *BLANKS
     C*
     C                   MOVE      ##LVL         ##ULVL
     C                   MOVE      COSRPL        ##CLVL
     C*
     C                   CALL      'SRCCSC'
     C                   PARM      ##TYPE        ##UTYP
     C                   PARM                    ##ULVL
     C                   PARM      COSRPT        ##CTYP
     C                   PARM                    ##CLVL
     C                   PARM      '1'           ##EXEC
     C                   PARM      ' '           ##RES             1
     C*
     C     ##RES         IFEQ      *ON
     C                   MOVE      *ON           SKIP1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
      *** END ADD *** 10/20/05 ************************************
     C                   ENDSR
     C*** END   ADD *** 02/22/05 **************************************
     C*
     C***  Begin Add  ***  11/19/09  **************************************
     C*================================================================
     C* Process the report detail printing
     C*================================================================
     C     SRDTL         BEGSR
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '025'
     C*** BEGIN CHG ****** 02/25/10 ************************************
     C**************       EXCPTDETAIL
     C                   EXCEPT    DTL025
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C     *IN59         WHENEQ    *ON
     C                   EXCEPT    DTLRDP
     C*** END   ADD ****** 08/09/10 ************************************
     C                   OTHER
     C**************       EXCPTDTL025
     C                   EXCEPT    DETAIL
     C*** END   CHG ****** 02/25/10 ************************************
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* Process the report headings printing
     C*================================================================
     C     SRHEAD        BEGSR
     C*
     C                   EXCEPT    HEAD
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '025'
     C                   EXCEPT    H1025
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C     *IN59         WHENEQ    *ON
     C                   EXCEPT    HDRDP
     C*** END   ADD ****** 08/09/10 ************************************
     C                   OTHER
     C                   EXCEPT    HEAD1
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* Process the report level 1 totals printing
     C*================================================================
     C     SRT1          BEGSR
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '025'
     C                   EXCEPT    T1025
     C                   OTHER
     C                   EXCEPT    TOT1
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* Process the report level 1 totals printing
     C*================================================================
     C     SRT1A         BEGSR
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '025'
     C                   EXCEPT    T1A025
     C                   OTHER
     C                   EXCEPT    TOT1A
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* Process the report level 1 totals printing
     C*================================================================
     C     SRT1B         BEGSR
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '025'
     C                   EXCEPT    TOT1B
     C                   OTHER
     C                   EXCEPT    TOT1B
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* Process the report level 2 totals printing
     C*================================================================
     C     SRT2          BEGSR
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '025'
     C                   EXCEPT    T2025
     C                   OTHER
     C                   EXCEPT    TOT2
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* Process the report level 2 totals printing
     C*================================================================
     C     SRT2A         BEGSR
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '025'
     C                   EXCEPT    T2A025
     C                   OTHER
     C                   EXCEPT    TOT2A
     C                   ENDSL
     C*
     C                   ENDSR
     C***  End   Add  ***  11/19/09  **************************************
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
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
     O**********             UPUSNM              67
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              67
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *************************************************
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
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
      *
      ***** BEG DEL ******* 09/27/07 **********************************
     O********E  160           ROUTE
     O*********                          23 '(C) Copyright Progressi'
     O*********                             've Data Solutions, Inc.,'
     O*********                *YEAR   +001
      ***** END DEL ******* 09/27/07 **********************************
      ***** BEG ADD ******* 09/27/07 **********************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O*****   E  1             ROUTE
     O*****   E  159           ROUTE
     O****                             +001 '(c)'
     O****                     *YEAR   +001
     O****                             +001 'StoneRiver, Inc.'
     O****                             +001 'All rights reserved.'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      ***** END ADD ******* 09/27/07 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             119
     O                       DSDATE        Y    198
      *** BEG CHG ***** 04/11/03 *************************************
     O********E**2*********    HEAD
      *********************
     O*********************    Q1PGM     10
     O*********************    ORTITL   129
     O*********************             192 'PAGE:'
     O*********************    PAGE  Z  198
     O          E   N42      HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E    42      HEAD           1
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E    42      HEAD           2
     O                                          100 'UPLOAD DATE:'
     O                       XXACHD        Y    109
      *** END ADD ***** 04/11/03 *************************************
      *********
      ***** BEG DEL ******* 09/27/07 **********************************
      *****
     O*****   E  1             HEAD
     O*****                             137 'SYSTEM'
     O*****                             150 'AMOUNT'
     O*****   E  2             HEAD
     O*****                              10 'BATCH #'
     O*****                              26 'BATCH AMT'
     O*****                              36 'GROUP'
     O*****                              62 'YR'
     O*****                              75 'EMP #'
     O*****                              95 'EMPLOYER NAME'
     O*****                             112 'CHECK #'
     O*****                             127 'CHECK AMT'
     O*****                             136 'CODE'
     O*****                             150 'POSTED'
     O*****                             163 'POST DT'
     O*****                             181 'A/R CODE,TYPE,'
     O*****                             192 'DESCRIPTION'
     O*****   E  1             DETAIL
     O*****                    EABAT#Z   10
     O*****                    EABATAJ   27
     O*****                    EAFND Z   34
     O*****                    XXFNNM    55
     O*****                    EAFYR Z   62
     O*****                    EAEMP#Z   75
     O*****                    EADIV Z   81
     O*****                    XXEMNM   101
     O*****                    EACHK#Z  112
     O*****                    EACKAMB  129
     O*****                    EASYCD   135
     O*****                    EAAMT B  152
     O*****                    XXPODT   163 '  /  /  '
     O*****                    EAARCD   169
     O*****                    XXTYPE   171
     O*****                    XXDESC   198
      ***** END DEL ******* 09/27/07 **********************************
      ***** BEG ADD ******* 09/27/07 **********************************
      *
     C*** BEG  ADD***** 02/21/08 **************************************
      ***** BEG CHG ******* 05/22/08 **********************************
     O********E  2             HEAD1
     O*** BEGIN CHG ****** 08/09/10 ************************************
     O******* E  2     71      HEAD
     O          E    71N59   HEAD           2
     O*** END   CHG ****** 08/09/10 ************************************
      ***** END CHG ******* 05/22/08 **********************************
     O                                           17 'TRANSACTION DATE:'
     O                       XXDATE        Y     28
     O*** BEGIN ADD ****** 08/09/10 ************************************
     O          E            HEAD           2
     O                                           17 'A/R BATCH TYPE  :'
     O                       XXBCTY              28
     O*** END   ADD ****** 08/09/10 ************************************
     C*** END  ADD***** 02/21/08 **************************************
     C*** BEG  CHG **** 02/21/08 **************************************
     O*****   E  1             HEAD
      ***** BEG CHG ******* 05/22/08 **********************************
     O******  E  1             HEAD2
     O***  Begin Chg  ***  11/19/09  **************************************
     O********E* 1             HEAD
     O          E            HEAD1          1
     O***  End   Chg  ***  11/19/09  **************************************
      ***** END CHG ******* 05/22/08 **********************************
     C*** END  CHG **** 02/21/08 **************************************
     O                                         +075 'S/R'
     O                                          137 'SYSTEM'
     O                                          150 'AMOUNT'
      *
     C*** BEG  CHG **** 02/21/08 **************************************
     O******  E  0             HEAD
      ***** BEG CHG ******* 05/22/08 **********************************
     O******* E  0             HEAD2
     O***  Begin Chg  ***  11/19/09  **************************************
     O********E* 0             HEAD
     O          E            HEAD1          0
     O***  End   Chg  ***  11/19/09  **************************************
      ***** END CHG ******* 05/22/08 **********************************
     C*** END  CHG **** 02/21/08 **************************************
     O                                         +000 'BATCH #'
     O                                         +009 'BATCH AMT'
     O                                         +003 'GROUP'
     O                                         +021 'YR'
     O                                         +008 'EMP#'
     O                                         +007 'EST'
     O                                         +003 'EMPLOYER NAME'
     O                                          112 'CHECK #'
     O                                          127 'CHECK AMT'
     O                                          136 'CODE'
     O                                          150 'POSTED'
     O                                          163 'POST DT'
     O                                          181 'A/R CODE,TYPE,'
     O                                          192 'DESCRIPTION'
     C*** BEG  CHG **** 02/21/08 **************************************
     O*****   E  2             HEAD
      ***** BEG CHG ******* 05/22/08 **********************************
     O******  E  2             HEAD2
     O***  Begin Chg  ***  11/19/09  **************************************
     O********E* 2             HEAD
     O          E            HEAD1          2
     O***  End   Chg  ***  11/19/09  **************************************
      ***** END CHG ******* 05/22/08 **********************************
     C*** END  CHG **** 02/21/08 **************************************
     O                       LINE              +000
      *
     O          E            DETAIL         1
     O                       EABAT#        Z   +000
     O                       EABATA        J   +002
     O                       EAFND         Z   +002
     O                       XXFNNM            +001
     O                       EAFYR         Z   +002
     O                       EAEMP#        Z   +002
     O                       EADIV         Z   +001
     O                       XXSEFL         B  +002
     O                       XXEMNM            +004
     O                       EACHK#        Z   +005
     O                       EACKAM        B    129
     O                       EASYCD             135
     O                       EAAMT         B    152
     O                       XXPODT             163 '  /  /  '
     O                       EAARCD             169
     O                       XXTYPE             171
     O                       XXDESC             198
      *
      ***** END ADD ******* 09/27/07 **********************************
     O          E            TOT1        1  2
     O                                           69 'TOTALS FOR BATCH #'
     O                       XXBAT#        Z     79
     O                                           80 ':'
     O                                           95 '# OF CHKS='
     O                       X1CKCT        Z    111
     O*** BEG CHG ***** 02/04/08 **************************************
     O***********              X1CKAMB  129
     O***********              X1AMT B  152
     O                       X1CKAM        A    129
     O                       X1AMT         A    152
     O*** END CHG ***** 02/04/08 **************************************
     O*** BEG ADD ***** 02/04/08 **************************************
     O          E            TOT1A       1  2
     O                                           69 'TOTALS FOR BATCH #'
     O                       XXBAT#        Z     79
     O                       X1AMT         A    152
     O*** END ADD ***** 02/04/08 **************************************
     O*** BEGIN ADD *** 03/03/05 **************************************
     O          E            TOT1B       1  2
     O                                           08 'Batch #:'
     O                       XXBAT#        Z   +002
     O                                         +002 '*** There are detail'
     O                                         +001 'records in this A/R'
     O                                         +001 'Batch that were not'
     O                                         +001 'printed due to'
     O                                         +001 'Group/Employer/Division'
     O                                         +001 'restrictions ***'
     O*** END   ADD *** 03/03/05 **************************************
     O          E            TOT2           2
     O                                           80 'GRAND TOTALS:'
     O                                           95 '# OF CHKS='
     O                       X2CKCT        Z    111
     O*** BEG CHG ***** 02/04/08 **************************************
     O***********              X2CKAMB  129
     O***********              X2AMT B  152
     O                       X2CKAM        A    129
     O                       X2AMT         A    152
     O*** END CHG ***** 02/04/08 **************************************
     O*** BEG ADD ***** 02/04/08 **************************************
     O          E            TOT2A       1  2
     O                                           80 'GRAND TOTALS:'
     O                       X2AMT         A    152
     O*** END ADD ***** 02/04/08 **************************************
     O*** BEGIN ADD ****** 08/09/10 ************************************
     O          E            HDRDP          1
     O                                         +075 'S/R'
     O                                          137 'SYSTEM'
     O                                          150 'AMOUNT'
     O          E            HDRDP          0
     O                                         +000 'BATCH #'
     O                                         +009 'BATCH AMT'
     O                                         +003 'GROUP'
     O                                         +021 'YR'
     O                                         +008 'EMP#'
     O                                         +007 'EST'
     O                                         +003 'EMPLOYER NAME'
     O                                          112 'CHECK #'
     O                                          127 'CHECK AMT'
     O                                          136 'CODE'
     O                                          150 'POSTED'
     O                                          163 'POST DT'
     O                                          172 'TRANS DT'
     O                                          184 'A/R CD,TYP,'
     O                                          195 'DESCRIPTION'
     O          E            HDRDP          2
     O                       LINE              +000
      *
     O          E            DTLRDP         1
     O                       EABAT#        Z   +000
     O                       EABATA        J   +002
     O                       EAFND         Z   +002
     O                       XXFNNM            +001
     O                       EAFYR         Z   +002
     O                       EAEMP#        Z   +002
     O                       EADIV         Z   +001
     O                       XXSEFL         B  +002
     O                       XXEMNM            +004
     O                       EACHK#        Z   +005
     O                       EACKAM        B    129
     O                       EASYCD             135
     O                       EAAMT         B    152
     O                       XXPODT             163 '  /  /  '
     O                       XXTRDT             172 '  /  /  '
     O                       EAARCD             176
     O                       XXTYPE             178
     O                       X2DESC             198
     O*** END   ADD ****** 08/09/10 ************************************
     O***  Begin Add  ***  11/19/09  **************************************
     O          E            H1025          1
     O                                          078 'S/R'
     O                                          131 'SYSTEM'
     O                                          144 'AMOUNT'
     O          E            H1025          0
     O                                            7 'BATCH #'
     O                                           24 'BATCH AMT'
     O                                           31 'GROUP'
     O                                           57 'YR'
     O                                           68 'EMP#'
     O                                           78 'EST'
     O                                           93 'EMPLOYER NAME'
     O                                          108 'CHECK #'
     O                                          122 'CHECK AMT'
     O                                          130 'CODE'
     O                                          144 'POSTED'
     O                                          156 'POST DT'
     O                                          172 'A/R CODE,TYPE,'
     O                                         +000 'DESCRIPTION'
     O                                          198 'STATEMENT#'
     O          E            H1025          2
     O                       LINE              +000
      *
     O          E            DTL025         1
     O                       EABAT#        Z   +000
     O                       EABATA        J   +002
     O                       EAFND         Z   +002
     O                       XXFNNM            +001
     O                       EAFYR         Z   +002
     O                       EAEMP#        Z   +002
     O                       EADIV         Z   +001
     O                       XXSEFL         B    77
     O                       XXEMNM              99
     O                       EACHK#        Z    108
     O                       EACKAM        B    124
     O                       EASYCD             129
     O                       EAAMT         B    146
     O                       XXPODT             156 '  /  /  '
     O                       EAARCD             160
     O                       XXTYPE             162
     O*** BEGIN CHG ***  03/15/10  ************************************
     O**********               XXDESC   188
     O                       X1DESC             187
     O*** END CHG *****  03/15/10  ************************************
     O*** BEG CHG *****  04/02/10  ************************************
     O***********              EAINV#   198
     O                       EAINV#        4    198
     O*** END CHG *****  04/02/10  ************************************
      *
     O          E            T1025       1  2
     O                                           69 'TOTALS FOR BATCH #'
     O                       XXBAT#        Z     79
     O                                           80 ':'
     O                                           95 '# OF CHKS='
     O                       X1CKCT        Z    111
     O                       X1CKAM        A    124
     O                       X1AMT         A    146
     O          E            T1A025      1  2
     O                                           69 'TOTALS FOR BATCH #'
     O                       XXBAT#        Z     79
     O                       X1AMT         A    146
     O          E            T2025          2
     O                                           80 'GRAND TOTALS:'
     O                                           95 '# OF CHKS='
     O                       X2CKCT        Z    111
     O                       X2CKAM        A    124
     O                       X2AMT         A    146
     O          E            T2A025      1  2
     O                                           80 'GRAND TOTALS:'
     O                       X2AMT         A    146
     O***  End   Add  ***  11/19/09  **************************************
     O          E            NORECS      2  1
     O                                           23 '*** NO RECORDS MEET THE'
     O                                           46 'SELECTION CRITERIA ***'
