     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W9206R - TRANSFER OF A/R BATCH ENTRIES
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM CREATES A REPORT LISTING ALL A/R BATCH ENTRIES   *
     H* THAT ARE BEING TRANSFERRED.                                   *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W9206C                                                      *
     H* CALLS TO:                                                     *
     H*   AP002R - API THAT CALLS GL INTERFACE.                       *
     H*                                                               *
     H* PROGRAMMER: KENNETH KENNEDY                                   *
     H* CREATED: 07/24/91               PROJECT: MD021                *
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   50  GENERAL CHAIN INDICATOR                                 *
     H*   55  AT LEAST ONE RECORD WAS TRANSFERRED                     *
     H*   60  GENERAL CHAIN INDICATOR                                 *
     H*   70  RECORD READ                                             *
     H*   OF  OVERFLOW INDICATOR                                      *
     H*   LR  LAST RECORD INDICATOR                                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/03/25 C:342 Ticket: 120040(133)                            *
     H*                Correctly load xxco# if qqco# holds            *
     H*                1- or 2-digit value.                           *
     H*                                                               *
     H* 10/03/24 C:62  Ticket: 117721(133)                            *
     H*                If policy is contested then don't resend       *
     H*                invoices to IVC.                               *
     H*                                                               *
     H* 11/16/23 C:62  Ticket: 112950(129)                            *
     H*                If an invoice being transferred still has a    *
     H*                balance then resend to IVC.                    *
     H*                                                               *
     H* 09/14/23 C:65  Project: Phw#113935 (inhouse-WC2663)           *
     H*                Update to the 08/16/22 change - that change    *
     H*                prevented the assignment of the invoice # when *
     H*                the A/R records were being written to WMEAP.   *
     H*                Force a write to file so subsequent A/R rcds   *
     H*                can find a record just created.                *
     H*                                                               *
     H* 08/16/22 C:65  Project: Phw#103334 (inhouse-WC2663)           *
     H*                Do not send S/R invoices to IVC.               *
     H*                                                               *
     H* 09/12/21 C:079 Project: WC2663                                *
     H*                Call Findalize 3rd party invoicing for each    *
     H*                Employer                                       *
     H*                                                               *
     H* 05/18/21 C:295 Support: 94788 (SUNZ)                          *
     H*                Check for deleted employer in the batch.       *
     H*                                                               *
     H* 03/25/21 C:079 Project: WC2663                                *
     H*                Invoice Cloud changes                          *
     H*                                                               *
     H* 02/05/16 C:136 Support: 37550                                 *
     H*                Use Company Maintenance for EFT Batch Security *
     H*                                                               *
     H* 12/10/15 C:306 Support 38130                                  *
     H*                Grand totals was printed on separate page.     *
     H*                Also Fixed the Route heading. It was printing  *
     H*                twice                                          *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 07/22/15 C:33  35103                                          *
     H*                Adjust written amount position.                *
     H*                                                               *
     H* 07/06/15 C:079 Phaseware 34264 LUBA                           *
     H*                Restore Routing Page Changes for LUBA          *
     H*                                                               *
     H* 04/13/15 C:062 Support: 34301(133)                            *
     H*                Condition QSYSPRT1.                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/06/14  C:283  Support: WA133 84479                         *
     H*                  Check the Employer/Group Restrictions only   *
     H*                  for the Batch which are Marked for Transfer. *
     H*                                                               *
     H* 08/28/13  C:283  Support: WA090 83018                         *
     H*                  If there are no records available in         *
     H*                  A/R Entry screen and first record is entered *
     H*                  and processed, then it prints incorrect      *
     H*                  Header.                                      *
     H*                                                               *
     H* 07/27/13  C:283  Support: WA146 82896                         *
     H*                  Header is always printing for the first      *
     H*                  record and the detail prints for the valid   *
     H*                  record. For next records the header and      *
     H*                  detail are printing correctly.               *
     H*                  Changed to correctly print the header for    *
     H*                  first valid record.                          *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H*                                                               *
     H* 03/26/12  C:279  Support: WA133 79070                         *
     H*                  Do not write batch till transfer and approval*
     H*                  flag both are set as 'Y'.                    *
     H*                                                               *
     H* 10/19/11  C:062  SUPPORT 78278                                *
     H*                  Do not update WDEAP if it was never chained  *
     H*                  to.  Set the 999999999 record to transfer    *
     H*                                                               *
     H* 09/22/11  C:180  SUPPORT 77963                                *
     H*                  Donot lock WDEAP records if they are not     *
     H*                  to be updated.
     H*
     H* 08/09/10 C:191 Project: 71WKU                                 *
     H*               - CHANGE TO INCLUDE RETRO BATCHES               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*                Load invoice #.                                *
     H*                                                               *
     H* 03/13/08 C:183 Project: WC1634                                *
     H*                Remove client-flagged sort logic since sort    *
     H*                sequence will now be soft-coded at company     *
     H*                level.                                         *
     H*                                                               *
     H* 02/04/08 C:136 SUPPORT: WA136 62005                           *
     H*                CORRECT ALGORITHM THAT TOTALS CHECK AMOUNTS    *
     H*                - DISREGARD EMP AND DIV IN CHK COUNT           *
     H*                - DO NOT COUNT TRANSACTIONS WITHOUT A CHECK    *
     H*                  NUMBER AS A CHECK                            *
     H*                                                               *
     H* 09/20/06 C:136 71WHE (EMC BACK TO BASE)                       *
     H*                SORT IN EMP# INSTEAD OF CHK# ORDER IF CLTID    *
     H*                =  '071'                                       *
     H*                                                               *
     H* 08/18/06 C:136 SUPPORT: WA109 54270                           *
     H*                TRANSACTION DATE NOT PRINTING ON FIRST PAGE    *
     H*                OF TRANSFER REPORT                             *
     H*                                                               *
     H*  06/22/06 C:062 PROJECT: WA099 - 53266                        *
     H*                 Correct various errors.                       *
     H*                                                               *
     H*  03/30/06 C:132 PROJECT: WA139 - 52102                        *
     H*                 Add check before write WMEAP record to see if *
     H*                 EATRDT is zero, if so, load EAPODT in the     *
     H*                 EATRDT field.                                 *
     H*                                                               *
     H*  10/20/05 C:53  PROJECT: WC1324                               *
     H*           C:136 Add logic to auto transfer S/R uploaded batches.
     H*                                                               *
     H*  06/24/05 C:33  Project: 109WDZ                               *
     H*                 {109AN}                                       *
     H*                 Use A/R batch transaction date instead of     *
     H*                  system date when writing entries to the      *
     H*                  "live" A/R file.                             *
     H*                                                               *
     H*  03/03/05 C:062 PROJECT: WC1229                               *
     H*                 Add group restrictions logic.                 *
     H*                                                               *
     H* 02/22/05 C:062 PROJECT: WC1274                                *
     H*                Check ACH Batch Security.                      *
     H*                                                               *
     H* 04/13/04 C:132 PROJECT:  WC1190  WA109 42171                  *
     H*              - On ADD, populate Inital Date, User & Pgm       *
     H*                                                               *
     H* 08/12/03 C:132 WC1117  Chg Date on Revision to 04/15/03 to    *
     H*                        match dates in code  (FROM 04/16/03)   *
     H*                                                               *
     H* 04/15/03 C:142 WC1117  LOCK BOX PROCESSING CHANGES            *
     H*                        CHG DATE TO 04/15/03 (FROM 04/16/03)   *
     H*                                                               *
     H* 05/03/01 C:61  SUPPORT WA109 27268                            *
     H*                Do not lock WMEML.                             *
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
     H*  10/14/98 C:69 Support: 14384                                 *
     H*                Changed indicator to one not being used.       *
     H*                                                               *
     H*  01/18/98 C:69 Project: WC577                                 *
     H*                Changed column headings to V3 standards        *
     H*  10/21/91 LPK  PROJECT WC085                                  *
     H*                IF TTTYPE IS 'I', FIELD EMINDE IN WMEMP        *
     H*                MUST BE UPDATED.                               *
     H*                                                               *
     H*  01/16/92 KWK  PROJECT WC112                                  *
     H*                TIME FIELD (EATIME) FOR THE A/R FILE (WMEAP)   *
     H*                IS NOT BEING UPDATED WITH THE CORRECT TIME     *
     H*                AND THIS IS CAUSING PROBLEMS WITH THE A/R      *
     H*                INQUIRY PROGRAM.  FIXED THIS CODE.             *
     H*                                                               *
     H*  09/29/92 JJJ  PROJECT WC179                                  *
     H*                RECORD LOCK PROBLEM ON WMEML                   *
     H*                                                               *
     H*  01/07/93 C:11 PROJECT WC181                                  *
     H*                BASE MOD TO PRINT .00 FOR BATCH# TOTALS IF     *
     H*                THEY ARE ZERO, INSTEAD OF LEAVING THEM BLANK.  *
     H*                                                               *
     H*  01/27/94 C:24 PROJECT WC245                                  *
     H*                CORRECT WHEN AND HOW "NO RECORDS" MESSAGE      *
     H*                IS PRODUCED.                                   *
     H*                                                               *
     H*  06/03/94 C:25 PROJECT WC335                                  *
     H*                CORRECT THE "NO RECORDS" MESSAGE TO PRINT ONLY *
     H*                WHEN NO RECORD IS TRANSFERRED.                 *
     H*                                                               *
     H*  04/30/97 C:62 PROJECT WC693                                  *
     H*                MAKE SURE THAT A/R BATCH RECOGNIZES THAT EVEN  *
     H*                IF TWO ENTRIES HAVE THE SAME CHK# AND CHKAMT,  *
     H*                AS LONG AS THE TWO ENTRIES ARE DIFFERENT EMPS, *
     H*                RECOGNIZE THEM AS BEING DIFFERENT.             *
     H*                                                               *
     H* 10/27/97  C:62  PROJECT WC560                                 *
     H*                 GL/AP REFERENCE IN W9206R.                    *
     H*                                                               *
     H*****************************************************************
     FWDEAP     UF   E             DISK
     F                                     RENAME(PEA01:PEA00)
     FWDEAL     IF   E           K DISK
     F                                     INFDS(INDS1)
     FWDEAL6    IF   E           K DISK
     F                                     RENAME(PEA01:PEA06)
     F                                     INFDS(INDS6)
     FWDEAL7    IF   E           K DISK
     F                                     RENAME(PEA01:PEA07)
     F                                     INFDS(INDS7)
     FWMUPL     IF   E           K DISK
     FWMEAP     O    E           K DISK
     F                                     RENAME(PEA01:PEA02)
     f*** Beg Add *** 03/25/21 ***************************************
     fwmeal8    if   e           k Disk    Rename(pea01:pea08) prefix(E8:2)
     fwdf2l     if   e           k Disk
     f*** End Add *** 03/25/21 ***************************************
     FWTABL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMEML     UF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D TITLE1          C                   CONST('A/R Batch Transfers ')
     D TITLE2          C                   CONST('A/R Batch Exceptions')
     D*
     D*
     d*** Beg Add *** 03/25/21 **************************************
     d**
       dcl-ds arrInv# qualified dim(9999) Inz;
         intCo# zoned(3);
         intFnd zoned(3);
         intFyr zoned(3);
         intEmp# zoned(9);
         intDiv zoned(5);
         intInv# zoned(10);
         chrType Char(20);
       end-ds;
     d*** End Add *** 03/25/21 **************************************
     D INDS1           DS
     D  DBRRN1               397    400B 0
     D INDS6           DS
     D  DBRRN6               397    400B 0
     D INDS7           DS
     D  DBRRN7               397    400B 0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     DintSavBtc#       s              9P 0 Inz(0)
     DisBatchcg        s               n   inz(*Off)
      *** Begin Add ****** 05/18/21 ************************************
     DisEmpDel         s               n   inz(*Off)
     DintSavBat#       s              9P 0 Inz(0)
     Dx1msg            s            100A
     Dxxreasn          s            100A
      *** End   Add ****** 05/18/21 ************************************
     d*** Beg Add *** 03/25/21 **************************************
     d inKey           s             20
     d inJson          s          32000    Varying
     d intInvCnt       s              4  0
     d isInvoiceCloud  s              1n   inz
     d isICgroup       s              1n   Inz
     d isSendInvoice   s              1n   Inz
     d strArKey        s             10
     d intSvFnd        s                   Like(EAfnd)
     d*** Beg Add *** 09/12/21 *************************************************
     d intSvEmp#       s                   Like(EAemp#)
     d chrInvoiceType  s             25
     d*** End Add *** 09/12/21 *************************************************
     d/Copy sPrModule
     d/Copy wPrPmtSvc
     d/Copy WprAr
     d*** End Add *** 03/25/21 ******************************************* d

     d*** Begin Add *** 11/16/23 *******************************************
     d chrMode         s             25    Inz('Invoice Cloud')
     d chrIncBatch     s              1    Inz('Y')
     d chrExcludeDep   s              1    Inz(' ')

     d arrInvoice      s             30    Dim(9999) Inz
     d balanceAmount   s             15  2 Inz
     d fromDt7         s              7  0 Inz(0)
     d index           s              5  0 Inz
     d invoice#        s             30    Inz
     d toDt7           s              7  0 Inz(0)
     d*** End   Add *** 11/16/23 *******************************************
     d*** Begin Add *** 10/03/24 *******************************************
     d chrContested    s              1    Inz(' ')
     d*** End   Add *** 10/03/24 *******************************************
     IPEA01
     I              EAUSER                      XRUSER
     I              EAPGM                       XRPGM
     I              EADT                        XRDT
     I              EATIME                      XRTIME
     IPEA07
     I              EAUSER                      XZUSER
     I              EAPGM                       XZPGM
     I              EADT                        XZDT
     I              EATIME                      XZTIME
     IPEA06
     I              EAUSER                      XYUSER
     I              EAPGM                       XYPGM
     I              EADT                        XYDT
     I              EATIME                      XYTIME

     C     CBKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE EMPLOYER A/R BATCH FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSTAT
     C*
     C*   KEY LIST FOR THE EMPLOYER A/R BATCH FILE
     C*
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSTAT
     C                   KFLD                    XXBAT#1
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
     C*   KEY LIST FOR G/L SYSTEM CONTROL FILE
     C*
     C     GLKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C     *LIKE         DEFINE    EACO#         XXCO#
     C     *LIKE         DEFINE    EABAT#        XXBAT#
     C     *LIKE         DEFINE    EABAT#        XXBAT#1
     C     *LIKE         DEFINE    EAAMT         X1AMT
     C     *LIKE         DEFINE    EATFAM        X1TFAM
     C     *LIKE         DEFINE    EACKAM        X1CKAM
     C     *LIKE         DEFINE    EAAMT         X2AMT
     C     *LIKE         DEFINE    EATFAM        X2TFAM
     C     *LIKE         DEFINE    EACKAM        X2CKAM
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    EAARTY        XXTYPE
     C     *LIKE         DEFINE    SYTYPE        XXSYTP
     C     *LIKE         DEFINE    EASTAT        XXSTAT
     C     *LIKE         DEFINE    EACHK#        XXCHK#
     C     *LIKE         DEFINE    EACKAM        XXCKAM
     C     *LIKE         DEFINE    EAEMP#        XXEMP#
     C     *LIKE         DEFINE    EADIV         XXDIV
     C     *LIKE         DEFINE    GLKEY         KKKEY
     C     *LIKE         DEFINE    *IN99         FAILED
     C     *LIKE         DEFINE    *IN99         MULTIG
     C     *LIKE         DEFINE    *IN99         FIRST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C*
     C*   PARAMETER LIST FOR API AP002R
     C*
     C     AP2PL         PLIST
     C                   PARM                    ##CO#             3            COMPANY
     C                   PARM                    ##GRP             3            GROUP
     C                   PARM                    ##YEAR            3            YEAR
     C                   PARM                    ##EMP#            9            EMPLOYER
     C                   PARM                    ##DIV             5            DIVISION
     C                   PARM                    ##CODE            2            A/R CODE
     C                   PARM                    ##TYPE            1            A/R TYPE
     C                   PARM                    ##DESC           30            DESCRIPTION
     C                   PARM                    ##AMT            11            AMOUNT
     C                   PARM                    ##TRDT            7            TRANS DATE
     C                   PARM                    ##PODT            8            POST DATE
     C                   PARM                    ##TIME            6            TIME
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    #1CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
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
     C**** Beg Chg ***** 11/03/25 ******************************
     C****               MOVE      QQCO#         XXCO#
     C                   EVAL      XXCO# = %Dec(%Trim(QQCO#):3:0)
     C**** End Chg ***** 11/03/25 ******************************
     C                   MOVE      QQDATE        XXDATE            8 0
     C                   TIME                    DSDTTM
     C                   MOVE      '0'           *IN70
     C     UPKEY         CHAIN     PUP01                              60
B01  C     QQCO#         IFEQ      '000'
     C     QQCO#         OREQ      *BLANKS
     C                   Z-ADD     UPCO#         XXCO#
E01  C                   ENDIF
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   SETOFF                                       313233
B01  C                   SELECT
S01  C     COARSQ        WHENEQ    'C'
     C                   SETON                                        31
S02  C     COARSQ        WHENEQ    'E'
     C                   SETON                                        32
S03  C                   OTHER
     C                   SETON                                        33
E01  C                   ENDSL
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        CONAME
     C*
     C* DOES MULTI GROUP RESTRICTIONS APPLY
     C*
     C                   CALL      'SA008R'
     C                   PARM      UPCO#         $$CO#
     C                   PARM      Q1USER        $$USR
     C                   PARM      'W'           $$APPL
     C     MULTIG        PARM      *OFF          $$RTCD
     C                   Z-ADD     0             COUNT1            5 0
     C*
     C* IS MULTI GROUP A FACTOR?
     C*
     C                   EXSR      CHKGEA
     C*
     C* CHECK THE ACH BATCH SECURITY SO WE CAN DETERMINE IF WE WANT
     C* TO SKIP ACH BATCHES
     C*
     C                   EXSR      CHKSEC
     C*
     C*
     C                   MOVE      *OFF          XAUTTR            1
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'W8361C'      ##PGM            10
     C                   PARM      *BLANKS       ##IN              1
     C*
B01  C     ##IN          IFEQ      *ON
     C                   MOVE      *ON           XAUTTR
     C*
X01  C                   ELSE
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'W8381C'      ##PGM
     C                   PARM      *BLANKS       ##IN
     C*
B02  C     ##IN          IFEQ      *ON
     C                   MOVE      *ON           XAUTTR
E02  C                   ENDIF
E01  C                   ENDIF
     C*
     C* POPULATE THE TITLES
     C*
     C                   CALL      'SRCENTER'
     C                   PARM      TITLE1        SRDFLD
     C                   PARM      60            SRDLEN
     C                   MOVEL(P)  SRDFLD        TITL1            60
     C*
     C                   CALL      'SRCENTER'
     C                   PARM      TITLE2        SRDFLD
     C                   PARM      60            SRDLEN
     C                   MOVEL(P)  SRDFLD        TITL2            60
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
B01  C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  ORTITL        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      60            SRDLEN            3 0
     C                   MOVEL     SRDFLD        ORTITL
E01  C                   END
      *
B01  C     XXNOGE        IFEQ      *OFF
     C*
     C                   MOVE      '1'           XXPRT
      *
     C     ORKEY         CHAIN     POR01                              60
      *
B02  C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  ORTITL        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      60            SRDLEN            3 0
     C                   MOVEL     SRDFLD        ORTITL
E02  C                   END
      *
     C                   EXCEPT    HEAD1
E01  C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XXSTAT
     C*
     C* CHECK SGLPF TO SEE IF GLFL04 IS NOT BLANK.
     C*
     C                   MOVEL     'GL'          KKKEY
     C*
     C     GLKY          CHAIN     SGL01                              99
     C*
B01  C     *IN99         IFEQ      *OFF
     C     GLFL04        ANDNE     *BLANKS
     C                   SETON                                        77
E01  C                   ENDIF
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C                   MOVE      ' '           SVACHF            1
       //*** Beg Add *** 03/25/21 ****************************************
       isInvoiceCloud = IsModEnabled('Invoice Cloud');
       //*** End Add *** 03/25/21 ****************************************
      *
B01  C                   SELECT
S01  C     *IN31         WHENEQ    '1'
     C     EAKEY         SETLL     PEA01
     C     EAKEY         READE     PEA01                                  50
S02  C     *IN32         WHENEQ    '1'
     C     EAKEY         SETLL     PEA07
     C     EAKEY         READE     PEA07                                  50
S03  C     *IN33         WHENEQ    '1'
     C     EAKEY         SETLL     PEA06
     C     EAKEY         READE     PEA06                                  50
E01  C                   ENDSL
      *
     C                   Z-ADD     *ZEROS        XXCHK#
     C                   Z-ADD     *ZEROS        XXCKAM
     C                   Z-ADD     *ZEROS        X1CKCT            5 0
     C                   Z-ADD     *ZEROS        X2CKCT            5 0
     C                   Z-ADD     *ZEROS        XXEMP#
     C                   Z-ADD     *ZEROS        XXDIV
     C                   MOVE      *OFF          FIRST
     C*
B01  C     EATRDT        IFEQ      0
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXTRDT            8 0
X01  C                   ELSE
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT
E01  C                   ENDIF                                                  EATRDT IFEQ 0
     C*
     C* CHECK GROUP/EMPLOYER/DIV RESTRICTIONS FOR THE WHOLE BATCH
     C*
B01  C     *IN50         IFEQ      '0'
     C                   EXSR      CHKGED
E01  C                   ENDIF
       //*** Beg Add *** 03/25/21 ****************************************
       intInvCnt = 0;
      //*** End Add *** 03/25/21 ****************************************
     C*
B01  C     *IN50         DOWEQ     '0'
       //*** Beg Add *** 03/25/21 ****************************************
       isIcGroup = IcGroupOpt(EAco#:EAfnd);
       Chain (EAco#:EAfnd) pf201;
      //*** End Add *** 03/25/21 ****************************************
     C********** 05/18/21 ******* Beg Add ********************************
     C     EAbat#        IFEQ      intSavBat#
     C     isEmpDel      ANDEQ     *ON
     C                   GOTO      DELEMP
     C                   ENDIF
     C                   MOVE      *OFF          isEmpDel
     C                   MOVE      *ZEROS        intSavBat#
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ON           isEmpDel
     C                   MOVE      EAbat#        intSavBat#
     C                   GOTO      NEXT
     C                   ENDIF
     C*********** 05/18/21 ******* End Add ***********************************
     C*
     C                   MOVE      *ON           ACHPAS            1
     C*
     C*** CONDITION PROCESSING FOR 'A' & 'S' ON SECURITY CHECK
     C*
B02  C                   SELECT
S01  C     EAACHF        WHENEQ    'A'
     C     SKIP          ANDEQ     *ON
     C                   MOVE      *OFF          ACHPAS
     C*
S02  C     EAACHF        WHENEQ    'S'
     C     SKIP1         ANDEQ     *ON
     C                   MOVE      *OFF          ACHPAS
     C*
     C*** LET 'L' & ' ' PROCESS WITHOUT CONDITIONS
     C*
E02  C                   ENDSL
     C*
     C*** IF VALID, ONLY PROCESS AUTO TRANSFER RCDS
     C*
B02  C     XAUTTR        IFEQ      *ON
     C     EAAUTR        ANDNE     'Y'
     C                   MOVE      *OFF          ACHPAS
E02  C                   ENDIF
     C*
B02  C     ACHPAS        IFEQ      *ON
     C                   MOVE      *BLANKS       XXKEY            10
     C                   MOVEL     EASYCD        XXKEY
      *
     C     TBKEY         CHAIN     PSY01                              60
      *
B03  C     *IN50         IFEQ      '0'
     C     SYSTAT        ANDNE     'I'
     C                   MOVEL     SYTYPE        XXSYTP
X03  C                   ELSE
     C                   MOVE      *BLANKS       XXSYTP
E03  C                   END
     C*
B03  C     EATFAP        IFEQ      'Y'
     C     EATFFL        ANDEQ     'Y'
     C*
B04  C     EACKAM        IFNE      *ALL'9'
     C*
B05  C     EABAT#        IFNE      XXBAT#
     C*
B06  C     EATRDT        IFEQ      0
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXTRDT            8 0
X06  C                   ELSE
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT
E06  C                   ENDIF                                                  EATRDT IFEQ 0
B05  C     FIRST         IFEQ      *OFF
     C                   Z-ADD     EABAT#        XXBAT#
     C                   EXSR      SRFLAG
     C                   EXSR      SRHEAD
     C                   MOVE      *ON           FIRST
     C                   MOVE      '0'           XXCHG
     C                   EXCEPT    HEAD2
E05  C                   ENDIF
     C*
B06  C     *INOF         IFEQ      '1'
     C                   EXSR      SRHEAD
     C                   MOVE      '0'           XXCHG
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
E06  C                   END
     C*
B06  C     X1CKCT        IFNE      *ZEROS
     C                   EXCEPT    TOT1
B07  C     XXCHG         IFEQ      '1'
     C                   EXSR      SRHEAD
     C                   MOVE      '0'           XXCHG
E07  C                   ENDIF
     C                   EXCEPT    HEAD2
E06  C                   END
B05  C     EABAT#        IFNE      XXBAT#
B06  C     X1CKCT        IFEQ      *ZEROS
     C                   EXCEPT    TOT1A
B07  C     XXCHG         IFEQ      '1'
     C                   EXSR      SRHEAD
     C                   MOVE      '0'           XXCHG
E07  C                   ENDIF
     C                   EXCEPT    HEAD2
E06  C                   END
B05  C                   ENDIF
     C*
     C* CHECK GROUP/EMPLOYER/DIV RESTRICTIONS FOR THE WHOLE BATCH
     C*
     C                   EXSR      CHKGED
     C*
     C                   ADD       X1CKCT        X2CKCT
     C                   ADD       X1CKAM        X2CKAM
     C                   ADD       X1AMT         X2AMT
     C                   ADD       X1TFAM        X2TFAM
     C                   Z-ADD     *ZEROS        X1CKAM
     C                   Z-ADD     *ZEROS        X1AMT
     C                   Z-ADD     *ZEROS        X1TFAM
     C                   Z-ADD     *ZEROS        X1CKCT
     C                   MOVE      EABAT#        XXBAT#
E05  C                   END
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
      *
     C     TBKEY         CHAIN     PTT01                              60
      *
B05  C     *IN50         IFEQ      '0'
     C     TTSTAT        ANDNE     'I'
     C                   MOVEL     TTTYPE        XXTYPE
X05  C                   ELSE
     C                   MOVE      *BLANKS       XXTYPE
E05  C                   END
     C*
B05  C                   SELECT
S01  C     *IN31         WHENEQ    '1'
     C     DBRRN1        CHAIN     PEA00                              61
S02  C     *IN32         WHENEQ    '1'
     C     DBRRN7        CHAIN     PEA00                              61
S03  C     *IN33         WHENEQ    '1'
     C     DBRRN6        CHAIN     PEA00                              61
E05  C                   ENDSL
      *
B05  C     XXSYTP        IFEQ      'Y'
     C                   MOVE      XXTYPE        EAARTY
     C*
B06  C     EATRDT        IFEQ      0
      *
B07  C     EAACHF        IFEQ      ' '
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
E07  C                   ENDIF
     C*
E06  C                   ENDIF                                                  EATRDT IFEQ 0
      *
     C                   MOVE      *BLANKS       EAEDIT
     C                   MOVE      *BLANKS       EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C                   MOVE      EABAT#        EABUL1
     C                   Z-ADD     EAAMT         EATFAM
      *
      * If EATRDT = zero, load with EAPODT, unless EAPODT also zeros. If
      * so, then load both with DSDATE (EADT)
B06  C     EATRDT        IFEQ      *ZEROS
B07  C     EAPODT        IFNE      *ZEROS
     C                   Z-ADD     EADT          EATRDT
X07  C                   ELSE
     C                   Z-ADD     EADT          EATRDT
     C                   Z-ADD     EADT          EAPODT
E07  C                   ENDIF                                                  EAPODT <> ZEROS
E06  C                   ENDIF                                                  EATRDT = ZEROS
     C                   Z-ADD     EAINV#        EANINV
     C
       //*** Beg Add *** 03/25/21 ************************************
       If isInvoiceCloud and isICgroup;
          strArKey = EAarCd;
          Chain (EAco#:strArKey) ptt01;
          //*** begin chg ***  08/16/22  ***********************************
          //*****If %Found(WTABL) and BillType(TTtype) and TTexIc <> 'Y';
          If %Found(WTABL) and BillType(TTtype) and TTexIc <> 'Y'
           and EAOnli <> 'Y';
          //*** end chg *****  08/16/22  ***********************************
             // Determine whether deductible or retrospective should be sent
             Exsr SrSendInvoice;
             If isSendInvoice;
                If EAinv# = 0;
                   E8ninv = 0;
                   Setll (EAco#:EAfnd:EAfYr:EAemp#:EAdiv:EApoDt) pea08;
                   Dou %Eof;
                      Reade (EAco#:EAfnd:EAfYr:EAemp#:EAdiv:EApoDt) pea08;
                      If %Eof;
                         Leave;
                      Endif;
                     // Process Retro and Deductible transactions
                     // separate from the other categories
                      Select;
                      When EActyp = 'R' And EActyp = E8ctyp;
                         EAninv = E8ninv;
                         Leave;
                      When EActyp = 'C' And EActyp = E8ctyp;
                         EAninv = E8ninv;
                         Leave;
                      When EActyp = E8ctyp;
                         EAninv = E8ninv;
                         Leave;
                      EndSl;
                   Enddo;
                   If E8nInv <> 0;
                      EAnInv = E8nInv;
                   Else;
                     inKey =  %EditC(EAco#:'X') + %EditC(EAfnd:'X')  +
                     %EditC(EAemp#:'X') + %EditC(EAdiv:'X');
                      EAnInv = GetNext3rdPartySeq#(inKey);
                   Endif;
                //*** Beg Add *** 09/12/21 *************************************
                Else;
                   EAnInv = EAinv#;
                //*** End Add *** 09/12/21 *************************************
                Endif;
                intInvCnt = intInvCnt +1;
                arrInv#(intInvCnt).intCo# = EAco#;
                arrInv#(intInvCnt).intFnd = EAfnd;
                arrInv#(intInvCnt).intFyr = EAfyr;
                arrInv#(intInvCnt).intEmp# = EAemp#;
                arrInv#(intInvCnt).intDiv = EAdiv;
                arrInv#(intInvCnt).intInv# = EAnInv;
                Select;
                When EAcTyp = 'R';
                   arrInv#(intInvCnt).chrType = 'deductible';
                When EAcTyp = 'C';
                   arrInv#(intInvCnt).chrType = 'retro';
                Other;
                   arrInv#(intInvCnt).chrType = 'billing';
                EndSl;
             Endif;
          Endif;
       Endif;
       //*** End Add *** 03/25/21 ************************************
       //*** begin add ***  09/14/23  ********************************
       //* Assign an invoice # if one has not already and its a billing record.
       //* If not a billing type find existing invoice # and bail if not found.
       If EANInv = 0;
          strArKey = EAarCd;
          Chain (EAco#:strArKey) ptt01;
          If %Found(WTABL);
             E8ninv = 0;
             Setll (EAco#:EAfnd:EAfYr:EAemp#:EAdiv:EApoDt) pea08;
             Dou %Eof;
                Reade (EAco#:EAfnd:EAfYr:EAemp#:EAdiv:EApoDt) pea08;
                If %Eof;
                   Leave;
                Endif;
               // Process Retro and Deductible transactions
               // separate from the other categories
                Select;
                When EActyp = 'R' And EActyp = E8ctyp;
                   EAninv = E8ninv;
                   Leave;
                When EActyp = 'C' And EActyp = E8ctyp;
                   EAninv = E8ninv;
                   Leave;
                When EActyp = E8ctyp;
                   EAninv = E8ninv;
                   Leave;
                EndSl;
             Enddo;
             //* Use existing invoice # when found.
             If E8nInv <> 0;
                EAnInv = E8nInv;
             Else;
             //* Only assign new invoice # for billing types.
                If BillType(TTtype);
                   inKey =  %EditC(EAco#:'X') + %EditC(EAfnd:'X')  +
                    %EditC(EAemp#:'X') + %EditC(EAdiv:'X');
                   EAnInv = GetNext3rdPartySeq#(inKey);
                Endif;
             Endif;
          Endif;
       Endif;
       //*** end add *****  09/14/23  ********************************
      *
     C                   WRITE     PEA02
       //*** begin add ***  09/14/23  ********************************
     C                   FEOD      WMEAP
       //*** end add *****  09/14/23  ********************************
       //*** Begin Add *** 11/16/23 ****************************************
       // Only process if IVC Group
       If isInvoiceCloud and isICgroup;
          // Check to see if this invoice was processed
          invoice# = %EditC(EAco#:'X')  +
                     %EditC(EAfnd:'X')  +
                     %EditC(EAemp#:'X') +
                     %EditC(EAdiv:'X')  +
                     %EditC(EAnInv:'X');

          // See if this invoice has been processed
          index    = %LookUp(invoice#:arrInvoice);
          If index = 0;
             // save Invoice#
             index    = %LookUp(' ':arrInvoice);
             arrInvoice(index) = invoice#;
          EndIf;
       EndIf;
       //*** End   Add *** 11/16/23 ****************************************
     C                   MOVE      '1'           *IN70
     C*
     C* * * IF TTTYPE IS EQUAL TO 'I' (INTIAL DEPOSIT) THEN WE MUST UPDATE
     C* * * THE WMEML FILE AFTER ADDING THE AMOUNT TO EMINDE.
     C*
B06  C     TTTYPE        IFEQ      'I'
     C     EMKEY         CHAIN     PEM01                              60
     C                   ADD       EAAMT         EMINDE
     C                   MOVE      Q1USER        EMUSER
     C                   MOVE      Q1PGM         EMPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C*
     C                   UPDATE    PEM01
E06  C                   END
     C*
     C*
B06  C     TTTYPE        IFEQ      'R'
     C     *IN77         ANDEQ     *ON
     C                   EXSR      CALLAP
E06  C                   ENDIF
     C*
E05  C                   END                                                    XXSYTP = 'Y'
     C*
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPODT            6 0
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRD6            6 0
     C*
     C     FNKEY         CHAIN     PFN01                              60
      *
B05  C     *IN60         IFEQ      '0'
     C                   MOVEL     FNSHNM        XXFNNM           20
X05  C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
E05  C                   END
      *
     C     EMKEY         CHAIN(N)  PEM01                              60
      *
B05  C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXEMNM           19
X05  C                   ELSE
     C                   MOVE      *BLANKS       XXEMNM
E05  C                   END
      *
     C                   MOVEL     EADESC        XXDESC           25
     C                   MOVEL     EADESC        X2DESC           19
     C*
B05  C     *INOF         IFEQ      '1'
     C                   EXSR      SRHEAD
     C                   MOVE      '0'           XXCHG
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
E05  C                   END
     C*
B05  C                   SELECT
S01  C     *IN59         WHENEQ    *ON
     C                   EXCEPT    DTLRDP
S02  C                   OTHER
     C                   EXCEPT    DETAIL
E05  C                   ENDSL
     C*
B05  C     EACHK#        IFNE      *ZEROS
      *
B06  C     EACHK#        IFNE      XXCHK#
     C     EACKAM        ORNE      XXCKAM
     C                   ADD       1             X1CKCT
     C                   ADD       EACKAM        X1CKAM
     C                   Z-ADD     EACHK#        XXCHK#
     C                   Z-ADD     EACKAM        XXCKAM
     C                   Z-ADD     EAEMP#        XXEMP#
     C                   Z-ADD     EADIV         XXDIV
E06  C                   END
      *
      *
E05  C                   ENDIF
      *
     C                   ADD       EAAMT         X1AMT
     C                   ADD       EATFAM        X1TFAM
     C*
     C*  UPDATE ORIGINAL RECORD WITH TRANSFER INFO
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATFDT
     C                   MOVE      'T'           EASTAT
B05  C     EAIDT         IFEQ      *ZEROS
     C                   MOVEL     SRDTO         EAIDT
     C                   MOVE      Q1USER        EAIUSR
     C                   MOVE      Q1PGM         EAIPGM
E05  C                   ENDIF
     C  N61              UPDATE    PEA00
X04  C                   ELSE
     C*
B05  C                   SELECT
S01  C     *IN31         WHENEQ    '1'
     C     DBRRN1        CHAIN     PEA00                              61
S02  C     *IN32         WHENEQ    '1'
     C     DBRRN7        CHAIN     PEA00                              61
S03  C     *IN33         WHENEQ    '1'
     C     DBRRN6        CHAIN     PEA00                              61
E05  C                   ENDSL
     C*
B05  C     *IN61         IFEQ      *OFF
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATFDT
     C*
     C                   MOVE      'T'           EASTAT
     C*
B06  C     EAIDT         IFEQ      *ZEROS
     C                   MOVEL     SRDTO         EAIDT
     C                   MOVE      Q1USER        EAIUSR
     C                   MOVE      Q1PGM         EAIPGM
E06  C                   ENDIF
     C*
     C                   UPDATE    PEA00
     C*
E05  C                   ENDIF
E04  C                   ENDIF
E03  C                   END                                                    APFL,TFFL = 'Y'
     C*
     C     NEXT          TAG
     C*
B03  C     XXGRST        IFEQ      'Y'
     C     XXNOGE        ANDEQ     *OFF
     C     EATFAP        ANDEQ     'Y'
     C     EATFFL        ANDEQ     'Y'
B04  C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOA
E04  C                   END
     C*
     C******** 05/18/21 ********* Beg Add ******************************************
      /free
              xxreasn = 'Group/Employer Restriction(s).';
      /End-free
     C******** 05/18/21 ********* End Add ******************************************
     C                   EXCEPT    DET1
     C                   ADD       1             COUNT1
     C*
E03  C                   ENDIF
E02  C                   ENDIF
     C*
     C******** 05/18/21 ********* Beg Add ******************************************
      /free
         If EAbat# = intSavBat#;
           if isEmpDel = *on and EAtfap ='Y' and EAtffl = 'Y';
              if not %open(qsysprt1);
                 open QSYSPRT1;
                 except HEAD1;
              else;
                 if *INOA = *on;
                    except HEAD1;
                    *INOA = *off;
                 endif;
              endif;
              xxreasn = 'Some Employer/Divison(s) are Deleted.';
              except DET1;
           endif;
         endif;
      /End-free
     C     DELEMP        TAG
     C******** 05/18/21 ********* End Add ******************************************
     C
B02  C                   SELECT
S01  C     *IN31         WHENEQ    '1'
     C     EAKEY         READE     PEA01                                  50
S02  C     *IN32         WHENEQ    '1'
     C     EAKEY         READE     PEA07                                  50
S03  C     *IN33         WHENEQ    '1'
     C     EAKEY         READE     PEA06                                  50
E02  C                   ENDSL
     C                   EXSR      SRFLAG
     C*
E01  C                   END                                                    *IN50
     C*
     C*  CHECK TO SEE IF NO RECORDS WERE READ
     C*
B01  C     *IN70         IFEQ      '0'
     C*
     C                   EXCEPT    NORECS
     C                   GOTO      TERM
E01  C                   END
     C*
B01  C     *INOF         IFEQ      '1'
     C                   EXSR      SRHEAD
     C                   MOVE      '0'           XXCHG
     C                   MOVE      '0'           *INOF
E01  C                   END
     C*
B01  C     X1CKCT        IFNE      *ZEROS
     C                   EXCEPT    TOT1
      /Free
        If XXCHG = '1' And isBatchcg = *Off;
      /End-free
     C                   EXSR      SRHEAD
     C                   MOVE      '0'           XXCHG
E02  C                   ENDIF
E01  C                   END
B01  C     X1CKCT        IFEQ      *ZEROS
     C     X1AMT         ANDNE     *ZEROS
     C                   EXCEPT    TOT1A
B02  C     XXCHG         IFEQ      '1'
     C                   EXSR      SRHEAD
     C                   MOVE      '0'           XXCHG
E02  C                   ENDIF
E01  C                   END
     C*
     C                   ADD       X1CKCT        X2CKCT
     C                   ADD       X1CKAM        X2CKAM
     C                   ADD       X1AMT         X2AMT
     C                   ADD       X1TFAM        X2TFAM
     C*
B01  C     *INOF         IFEQ      '1'
     C                   EXSR      SRHEAD
     C                   MOVE      '0'           *INOF
E01  C                   END
     C*
B01  C     X2CKCT        IFNE      *ZEROS
     C                   EXCEPT    TOT2
E01  C                   END
     C*
B01  C     X2CKCT        IFEQ      *ZEROS
     C                   EXCEPT    TOT2A
E01  C                   END
       //*** Beg Add *** 03/25/21 ************************************
       // Read through the invoice array and call FinalizeThirdpartyInvoicing
       // when a new group is encountered.
       If isInvoiceCloud and intInvCnt > 0;
       //*** Beg Del *** 09/12/21 *************************************
       //***   SortA %Subarr(arrInv#(*).intFnd:1:intInvCnt);
       //*** End Del *** 09/12/21 *************************************
          intInvCnt = 1;
          intSvFnd = arrInv#(intInvCnt).intFnd;
       //*** Beg Add *** 09/12/21 *************************************
          intSvEmp# = arrInv#(intInvCnt).intEmp#;
       //*** End Add *** 09/12/21 *************************************
          Dow arrInv#(intInvCnt).intFnd > 0;
             inJson =
             '{"type":"' + arrInv#(intInvCnt).chrType + '",' +
             '"invoice":"' + %Editc(arrInv#(intInvCnt).intInv#:'X') + '",' +
             '"company":"' + %Editc(arrInv#(intInvCnt).intCo#:'X') + '",' +
             '"group":"' + %Editc(arrInv#(intInvCnt).intFnd:'X') + '",' +
             '"employer":"' + %Editc(arrInv#(intInvCnt).intEmp#:'X') + '",' +
             '"division":"' + %Editc(arrInv#(intInvCnt).intDiv:'X') + '",' +
             '"active":' + '"true"}';
             If DeleteUnpaidInvoices(arrInv#(intInvCnt).intCo#:
                 arrInv#(intInvCnt).intFnd:arrInv#(intInvCnt).intEmp#:
                 arrInv#(intInvCnt).intDiv:arrInv#(intInvCnt).intFyr:
                 arrInv#(intInvCnt).intInv#:chrInvoiceType);
                ProcessAdjustedInvoicing(inJson);
             Else;
                ThirdPartyInvoicing(inJson);
             EndIf;
             intInvCnt = intInvCnt + 1;
       //*** Beg Chg *** 09/12/21 *************************************
             //***If arrInv#(intInvCnt).intFnd <> intSvFnd;
             If arrInv#(intInvCnt).intFnd <> intSvFnd or
              arrInv#(intInvCnt).intEmp# <> intSvEmp#;
       //*** End Chg *** 09/12/21 *************************************
                FinalizeThirdPartyInvoicing
                 (arrInv#(intInvCnt - 1).intCo#:intSvFnd);
             Endif;
             intSvFnd = arrInv#(intInvCnt).intFnd;
       //*** Beg Add *** 09/12/21 *************************************
             intSvEmp# = arrInv#(intInvCnt).intEmp#;
       //*** End Add *** 09/12/21 *************************************
          Enddo;
       Endif;
       //*** End Add *** 03/25/21 ************************************
       //*** Begin Add *** 11/16/23 ****************************************
       // Process Invoices
       For index=1 to 9999;
          If arrInvoice(index) <> ' ';

             // Extract key values from invoice#;
             EAco#  = %Dec(%Subst(arrInvoice(index):1:3):3:0);
             EAfnd  = %Dec(%Subst(arrInvoice(index):4:3):3:0);
             EAemp# = %Dec(%Subst(arrInvoice(index):7:9):9:0);
             EAdiv  = %Dec(%Subst(arrInvoice(index):16:5):5:0);
             EAinv# = %Dec(%Subst(arrInvoice(index):21:10):10:0);

             // Get the policy Year
             Exec Sql Select EAfyr Into :EAfyr From WMEAP
                      Where EAco#=:EAco# And EAfnd=:EAfnd And
                            EAemp#=:EAemp# And EAdiv=:EAdiv And
                            EAninv=:EAinv# Limit 1;

             If EAfyr = 0;
                // Get the policy Year from holding file
                // if not found in WMEAP
                Exec Sql Select XAfyr Into :EAFyr From WMXAP
                         Where XAco#=:EAco# And XAfnd=:EAfnd And
                               XAemp#=:EAemp# And XAdiv=:EAdiv And
                               XAninv=:EAinv# Limit 1;
             Endif;

             //*** Begin Add *** 10/03/24 **********************************
             //
             // Determine if policy is contested.  If it is then don't send
             // invoices for that policy to IVC.
             Exec Sql Select ELctst
                      Into :chrContested
                      From WDELP
                      Where ELco#=:EAco# And ELfnd=:EAfnd And
                            ELemp#=:EAemp# And ELdiv=:EAdiv And
                            ELfyr=:EAfyr;
             If chrContested = 'Y';
                Iter;
             EndIf;
             //*** End   Add *** 10/03/24 **********************************

             // get the posting date to use as the To Date
             Exec Sql Select Max(EApoDt) Into :toDt7 From WMEAP
                      Where EAco#=:EAco# And EAfnd=:EAfnd And
                            EAemp#=:EAemp# And EAdiv=:EAdiv And
                            EAninv=:EAinv#;

             balanceAmount = ARBalance(fromDt7:toDt7:EAco#:EAfnd:EAfyr:
                             EAemp#:EAdiv:chrMode:EAinv#:chrIncBatch:
                             chrExcludeDep);

             inJson        = '{"type":"billing",' +
                             '"invoice":"'  + %EditC(EAinv#:'X') + '",' +
                             '"company":"'  + %EditC(EAco#:'X')  + '",' +
                             '"group":"'    + %EditC(EAfnd:'X')  + '",' +
                             '"employer":"' + %EditC(EAemp#:'X') + '",' +
                             '"division":"' + %EditC(EAdiv:'X')  + '",' +
                             '"active":"true"}';

             // If there is still a balance then resend to IVC
             If balanceAmount > 0;
                // Initialize IVC Feed
                FinalizeThirdPartyInvoicing(EAco#:EAfnd);
                // Prepare Invoice to send to IVC
                ThirdPartyInvoicing(inJson);
                // Send Invoice to IVC
                FinalizeThirdPartyInvoicing(EAco#:EAfnd);
             EndIf;

          Else;
             Leave;
          EndIf;
       EndFor;
       //*** End   Add *** 11/16/23 ****************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
B01    If xxNoge = *Off;
          xxPrt = '1';
          Chain orKey POR01;
          If %found;
             Except Route1;
          Endif;
       EndIf;
      /End-free
     C*
      /EJECT
     C*================================================================
     C* CHKGED - Checks the Group/Employer/Division security for the
     C* whole batch
     C*================================================================
     C     CHKGED        BEGSR
     C*
     C                   Z-ADD     EABAT#        XXBAT#1
     C*
     C                   SETOFF                                       98
     C                   MOVEL     *BLANKS       XXGRST            1
B01  C     UPXFND        IFEQ      'Y'
     C     UPXEMP        OREQ      'Y'
     C     MULTIG        OREQ      *ON
B02  C     *IN98         DOWEQ     *OFF
     C*
B03  C     EAFND         IFNE      *ZEROS
B04  C     MULTIG        IFEQ      *ON
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
B05  C     FAILED        IFEQ      *ON
     C     $$AUTH        OREQ      'I'
     C                   MOVEL     'Y'           XXGRST
     C                   LEAVE
E05  C                   ENDIF
     C*
X04  C                   ELSE
B05  C     UPXFND        IFEQ      'Y'
     C     UPFND         ANDNE     EAFND
     C     UPXEMP        OREQ      'Y'
     C     UPFND         ANDNE     EAFND
     C     UPXEMP        OREQ      'Y'
     C     UPEMP#        ANDNE     EAEMP#
     C     UPXEMP        OREQ      'Y'
     C     UPDIV         ANDNE     EADIV
     C                   MOVEL     'Y'           XXGRST
     C                   LEAVE
E05  C                   ENDIF
E04  C                   ENDIF
E03  C                   ENDIF
B03  C                   SELECT
S01  C     *IN31         WHENEQ    '1'
     C     EAKY2         READE     PEA01                                  98
S02  C     *IN32         WHENEQ    '1'
     C     EAKY2         READE     PEA07                                  98
S03  C     *IN33         WHENEQ    '1'
     C     EAKY2         READE     PEA06                                  98
E03  C                   ENDSL
     C*
E02  C                   ENDDO
     C*
B02  C     XXGRST        IFEQ      'Y'
B03  C                   SELECT
S01  C     *IN31         WHENEQ    '1'
     C     EAKY2         SETGT     PEA01
S02  C     *IN32         WHENEQ    '1'
     C     EAKY2         SETGT     PEA07
S03  C     *IN33         WHENEQ    '1'
     C     EAKY2         SETGT     PEA06
E03  C                   ENDSL
     C                   GOTO      NEXT
X02  C                   ELSE
B03  C                   SELECT
S01  C     *IN31         WHENEQ    '1'
     C     EAKY2         CHAIN     PEA01                              50
S02  C     *IN32         WHENEQ    '1'
     C     EAKY2         CHAIN     PEA07                              50
S03  C     *IN33         WHENEQ    '1'
     C     EAKY2         CHAIN     PEA06                              50
E03  C                   ENDSL
E02  C                   ENDIF
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CHKGEA - Checks the Group/Employer/Division security for the
     C* whole run
     C*================================================================
     C     CHKGEA        BEGSR
     C*
     C                   MOVEL     *ON           XXNOGE            1
     C*
B01  C     UPXFND        IFEQ      'Y'
     C     UPXEMP        OREQ      'Y'
     C     MULTIG        OREQ      *ON
     C*
B02  C                   SELECT
S01  C     *IN31         WHENEQ    '1'
     C     EAKEY         CHAIN(N)  PEA01                              98
S02  C     *IN32         WHENEQ    '1'
     C     EAKEY         CHAIN(N)  PEA07                              98
S03  C     *IN33         WHENEQ    '1'
     C     EAKEY         CHAIN(N)  PEA06                              98
E02  C                   ENDSL
B02  C     *IN98         DOWEQ     *OFF
     C     EATFAP        IFEQ      'Y'
     C     EATFFL        ANDEQ     'Y'
     C*
B03  C     EAFND         IFNE      *ZEROS
B04  C     MULTIG        IFEQ      *ON
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
B05  C     FAILED        IFEQ      *ON
     C     $$AUTH        OREQ      'I'
     C                   MOVE      *OFF          XXNOGE
     C                   LEAVE
E05  C                   ENDIF
     C*
X04  C                   ELSE
B05  C     UPXFND        IFEQ      'Y'
     C     UPFND         ANDNE     EAFND
     C     UPXEMP        OREQ      'Y'
     C     UPFND         ANDNE     EAFND
     C     UPXEMP        OREQ      'Y'
     C     UPEMP#        ANDNE     EAEMP#
     C     UPXEMP        OREQ      'Y'
     C     UPDIV         ANDNE     EADIV
     C                   MOVEL     *OFF          XXNOGE
     C                   LEAVE
E05  C                   ENDIF
E04  C                   ENDIF
E03  C                   ENDIF
     C                   ENDIF
B03  C                   SELECT
S01  C     *IN31         WHENEQ    '1'
     C     EAKEY         READE(N)  PEA01                                  98
S02  C     *IN32         WHENEQ    '1'
     C     EAKEY         READE(N)  PEA07                                  98
S03  C     *IN33         WHENEQ    '1'
     C     EAKEY         READE(N)  PEA06                                  98
E03  C                   ENDSL
E02  C                   ENDDO
E01  C                   ENDIF
     C*
B01  C     XXNOGE        IFEQ      *OFF
     C******** 05/18/21 ********* Beg Chg ******************************************
     C****               OPEN      QSYSPRT1
      /free
              if not %open(qsysprt1);
                 open QSYSPRT1;
              endif;
      /End-free
     C******** 05/18/21 ********* End Chg ******************************************
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CHKSEC - Checks to see if there is sufficient security
     C*================================================================
     C     CHKSEC        BEGSR
     C*
     C                   MOVE      *OFF          SKIP              1
     C                   CALL      'WA001R'      COMPL
     C*

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
     C*
     C                   MOVE      *OFF          SKIP1             1
     C*
     C     XXCO#         CHAIN     PCO01                              99
B01  C     *IN99         IFEQ      *OFF
B02  C     COSRPL        IFNE      *ZEROS
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
B03  C     ##RES         IFEQ      *ON
     C                   MOVE      *ON           SKIP1
E03  C                   ENDIF
E02  C                   ENDIF
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C*   SUBROUTINE - MVEAPV - MOVES WMEAP VALUES TO PARM VALUES.
     C*****************************************************************
     C*
     C     MVEAPV        BEGSR
     C*
     C                   MOVE      EACO#         ##CO#
     C                   MOVE      EAFND         ##GRP
     C                   MOVE      EAFYR         ##YEAR
     C                   MOVE      EAEMP#        ##EMP#
     C                   MOVE      EADIV         ##DIV
     C                   MOVEL     EAARCD        ##CODE
     C                   MOVEL     EAARTY        ##TYPE
     C                   MOVEL     EADESC        ##DESC
     C                   MOVE      EAAMT         ##AMT
     C                   MOVE      EATRDT        ##TRDT
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##PODT
     C*
     C                   MOVE      EATIME        ##TIME
     C*
     C                   ENDSR
     C*
      /EJECT
     C*****************************************************************
     C*   SUBROUTINE - CALLAP - CALLS AP002R TO UPDATE AP/GL RECORDS.
     C*****************************************************************
     C*
     C     CALLAP        BEGSR
     C*
     C                   EXSR      MVEAPV
     C*
     C                   CALL      'AP002R'      AP2PL
     C*
     C                   ENDSR
       //*** Beg Add *** 03/25/21 *****************************************
       //====================================================================
       // Subroutine SrSendInvoice - Determine if Deductible or Retrospective
       // Qualify
       //====================================================================
       // Do not send the invoice # for a deductible invoice when there is
       // no code entered in WDF2P/F2DPCD.  A deductible invoice can be recognized
       // when the billing A/R code has a type of F and a category of R.
       // Do not send the invoice # for a retrospective settlement invoice when
       // there is no code entered in WDF2P/F2RPCD.  A retrospective invoice can be
       // recognized when the billing A/R code has a type of B and a category of C.

       Begsr SrSendInvoice;
          isSendInvoice = *on;
          If TTtype = 'F' and TTcTyp = 'R' and F2dpCd = *blanks;
             isSendInvoice = *off;
          Endif;
          If TTtype = 'B' and TTcTyp = 'C' and F2rpCd = *blanks;
             isSendInvoice = *off;
          Endif;
       Endsr;
       //*** End Add *** 03/25/21 *****************************************
     C*================================================================
     C* Process the flag change
     C*================================================================
     C     SRFLAG        BEGSR
     C*
     C* IF RETRO BATCH AND PREVIOUS BATCH WAS NOT RETRO
B01  C     EAACHF        IFNE      SVACHF
     C     FIRST         OREQ      *OFF
      /Free
          isBatchcg = *off;
          If (EAbat# <> intSavBtc#);
              intSavBtc# = EAbat#;
              isBatchcg = *On;
          Endif;
      /End-Free
     C                   MOVE      EAACHF        SVACHF
     C                   MOVE      '1'           XXCHG             1
     C*
     C* SUPRESS PRINTING OF TRANSACTION DATE FOR RETRO BATCHES
     C                   SETOFF                                       59
     C*
     C* DISPLAY VERBIAGE TO IDENTIFY BATCHES
B02  C                   SELECT
S01  C     EAACHF        WHENEQ    'A'
     C                   MOVEL(P)  'ACH'         XXBCTY           10
S02  C     EAACHF        WHENEQ    'D'
     C                   MOVEL(P)  'DEDUCT'      XXBCTY
     C                   MOVE      'IBLE'        XXBCTY
S03  C     EAACHF        WHENEQ    'L'
     C                   MOVEL(P)  'LOCKBOX'     XXBCTY
S04  C     EAACHF        WHENEQ    'R'
     C                   MOVEL(P)  'RETRO'       XXBCTY
     C                   SETON                                        59
S05  C     EAACHF        WHENEQ    'S'
     C                   MOVEL(P)  'S/R UPL'     XXBCTY
     C                   MOVE      'OAD'         XXBCTY
S06  C                   OTHER
     C                   MOVEL(P)  'REGULAR'     XXBCTY
E02  C                   ENDSL
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* Process the report headings printing
     C*================================================================
     C     SRHEAD        BEGSR
     C*
B01  C                   SELECT
S01  C     *IN59         WHENEQ    *ON
     C                   EXCEPT    HDRDP
S02  C                   OTHER
     C                   EXCEPT    HEAD
E01  C                   ENDSL
     C*
     C                   MOVE      '0'           *INOF
     C*
     C                   ENDSR
     C*
     C*****************************************************************
      /EJECT
     C*
     OQSYSPRT   E            ROUTE          1 06
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              67
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       TITL1              129
     O          E            ROUTE          2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            ROUTE          2
     O                                           19 'OUTPUT QUEUE:'
     O                       OROUTQ              30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
     O          E            ROUTE          1 60
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             119
     O                       DSDATE        Y    198
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           1
     O                                          126 'SYSTEM'
     O                                          136 'AMOUNT'
     O                                          152 'AMOUNT'
     O          E            HEAD           2
     O                                            9 'BATCH #'
     O                                           23 'BATCH AMT'
     O                                           31 'GROUP'
     O                                           55 'YR'
     O                                           66 'EMP #'
     O                                           86 'EMPLOYER NAME'
     O                                          101 'CHECK #'
     O                                          115 'CHECK AMT'
     O                                          125 'CODE'
     O                                          136 'POSTED'
     O                                          152 'WRITTEN'
     O                                          163 'POST DT'
     O                                          181 'A/R CODE,TYPE,'
     O                                          192 'DESCRIPTION'
     O          E   N59      HEAD2          2
     O                                           21 'TRANSACTION DATE'
     O                       XXTRDT              33 '  /  /    '
     O          E            HEAD2          2
     O                                           21 'A/R BATCH TYPE'
     O                       XXBCTY              33
     O          E            DETAIL         1
     O                       EABAT#        Z      9
     O                       EABATA        J     24
     O                       EAFND         Z     29
     O                       XXFNNM              50
     O                       EAFYR         Z     55
     O                       EAEMP#        Z     66
     O                       EADIV         Z     72
     O                       XXEMNM              92
     O                       EACHK#        Z    101
     O                       EACKAM        B    117
     O                       EASYCD             124
     O                       EAAMT         B    138
     O                       EATFAM        B    154
     O                       XXPODT             163 '  /  /  '
     O                       EAARCD             169
     O                       XXTYPE             171
     O                       XXDESC             198
     O          E            HDRDP          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             119
     O                       DSDATE        Y    198
     O          E            HDRDP          2
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HDRDP          1
     O                                          126 'SYSTEM'
     O                                          136 'AMOUNT'
     O                                          152 'AMOUNT'
     O          E            HDRDP          2
     O                                            9 'BATCH #'
     O                                           23 'BATCH AMT'
     O                                           31 'GROUP'
     O                                           55 'YR'
     O                                           66 'EMP #'
     O                                           86 'EMPLOYER NAME'
     O                                          101 'CHECK #'
     O                                          115 'CHECK AMT'
     O                                          125 'CODE'
     O                                          136 'POSTED'
     O                                          152 'WRITTEN'
     O                                          163 'POST DT'
     O                                          172 'TRANS DT'
     O                                          184 'A/R CD,TYP,'
     O                                          195 'DESCRIPTION'
     O          E            DTLRDP         1
     O                       EABAT#        Z      9
     O                       EABATA        J     24
     O                       EAFND         Z     29
     O                       XXFNNM              50
     O                       EAFYR         Z     55
     O                       EAEMP#        Z     66
     O                       EADIV         Z     72
     O                       XXEMNM              92
     O                       EACHK#        Z    101
     O                       EACKAM        B    117
     O                       EASYCD             124
     O                       EAAMT         B    138
     O                       EATFAM        B    154
     O                       XXPODT             163 '  /  /  '
     O                       XXTRD6             172 '  /  /  '
     O                       EAARCD             176
     O                       XXTYPE             178
     O                       X2DESC             198
     O          E            TOT1        1  2
     O                                           69 'TOTALS FOR BATCH #'
     O                       XXBAT#        Z     79
     O                                           80 ':'
     O                                           95 '# OF CHKS='
     O                       X1CKCT        Z    100
     O                       X1CKAM        A    117
     O                       X1AMT         A    138
     O                       X1TFAM        A    154
     O          E            TOT1A       1  2
     O                                           69 'TOTALS FOR BATCH #'
     O                       XXBAT#        Z     79
     O                                           80 ':'
     O                       XXBAT#        Z     79
     O                       X1AMT         A    138
     O                       X1TFAM        A    154
     O          E            TOT2           2
     O                                           80 'GRAND TOTALS:'
     O                                           95 '# OF CHKS='
     O                       X2CKCT        Z    100
     O                       X2CKAM        A    117
     O                       X2AMT         A    138
     O                       X2TFAM        A    154
     O          E            NORECS      2  1
     O                                           23 '*** NO RECORDS HAVE BEE'
     O                                           40 'N TRANSFERRED ***'
     O          E            TOT2A          2
     O                                           80 'GRAND TOTALS:'
     O                       X2AMT         A    138
     O                       X2TFAM        A    154
     OQSYSPRT1  E            ROUTE1         1 06
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              67
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             119
     O                       DSDATE        Y    198
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                       TITL2              129
     O                                          192 'PAGE:'
     O                       PAGE2         Z    198
      ************* 05/18/21 ***********Beg Chg ********************************
     O*****     E            HEAD1          2
     O*****                                      22 'The following A/R Ba'
     O*****                                      42 'tches were not trans'
     O*****                                      62 'ferred due to group/'
     O*****                                      82 'employer/div restric'
     O*****                                     102 'tion:               '
     O*****     E            DET1           1
     O*****                                       9 'Batch #:'
     O*****                  EABAT#        Z   +002
     O*****                                    +004 'Batch amount:'
     O*****                  EABATA        J   +002
     O          E            HEAD1       2  1
     O                                           20 '**** Non-Transferred'
     O                                           40 ' Batches ****       '
     O          E            HEAD1       2  1
     O                                           12 'Batch #:'
     O                                         +003 'Batch amount:'
     O                                         +006 'Reason:'
     O          E            DET1           1
     O                       EABAT#        Z   +002
     O                       EABATA        J   +002
     O                       XXREASN           +006
      ************* 05/18/21 ***********End Add ********************************
