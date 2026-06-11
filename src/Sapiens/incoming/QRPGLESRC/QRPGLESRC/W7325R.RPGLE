     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7325R - DEDUCTIBLE INVOICES
     H* ***************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM WILL PRINT DEDUCTIBLE INVOICES BASED ON        *
     H*   RECORDS CREATED BY THE "CALCULATE DEDUCTIBLE BILL AMOUNTS"  *
     H*   PROGRAM (W8020R) FOR THE INVOICE DATE SPECIFIED.            *
     H* CALLED FROM:                                                  *
     H*   W8007C                                                      *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H* PROGRAMMER: C:24                                              *
     H* CREATED: 02/28/95               PROJECT: WC311                *
     H* ***************************************************************
     H* INDICATORS:                                                   *
     H*   *IN60 - GENERAL CHAIN, DOWEQ                                *
     H* ***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/21/25 C:300 Project: 146WKU (Prescient), Support: 118689   *
     H*                Collateral Phase 2: Spread over Multi-Employers*
     H*                -When Collateral is pulled from another Policy *
     H*                -Collateral Transfer line prints incorrectly on*
     H*                -Deductible Invoice Bills of both policies,    *
     H*                -Showing +ve Balance Due on 1 policy where     *
     H*                -Current Deductible is Billed AND              *
     H*                -Showing -ve Balance Due on other policy which *
     H*                -provided the Collateral for Deductible Payment*
     H*                1) Use EXCOLD code instead of EXCOLT to accumulate*
     H*                   Collateral Transfer amount for each policy  *
     H*                2) Add Collateral Transfer amount to 'Total    *
     H*                   Amount Due' on Summary Deductible Invoice   *
     H*                                                               *
     H* 01/17/25 C:291 Project: WC2494                                *
     H*                Deductible Invoice output in Scriptura.        *
     H*                Removed old Scriptura related code.            *
     H*                Removed commented code from prior releases.    *
     H*                Produce Summary Invoice regardless of IVC      *
     H*                participation.                                 *
     H*                Attach Scriptura version of PEO Invoice to the *
     H*                same iDart as State Reporting Div Ded Invoice. *
     H*                                                               *
     H* 11/18/24 C:300 Project: 146WKR (Prescient)                    *
     H*                Collateral Processing - fix date issue         *
     H*                                                               *
     H* 06/24/24 C:300 Project: 146WKR (Prescient)                    *
     H*                Collateral Processing                          *
     H*                                                               *
     H* 03/25/21 C:79  Project: WC2663                                *
     H*                Invoice Cloud changes                          *
     H*                                                               *
     H* 03/24/21 C:79  Project: 99/95218                              *
     H*                Clean up program by removing revision comments *
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
     H*                                                               *
     H* 11/21/17 C:33  Support: 49955                                 *
     H*                Accept and optional outq.                      *
     H*                                                               *
     H* 05/03/16 C:310 Support: 40869                                 *
     H*                Changed the dimension of array ARZ to prevent  *
     H*                the array out of index error.                  *
     H*
     H* 04/26/16 C:310 Support: 40872                                 *
     H*                Changed code to pass correct program name.     *
     H*                                                               *
     H* 04/21/16 C:307 Support: 40869                                 *
     H*                To handle opening of file W7325O/W7325O1       *
     H*                                                               *
     H* 03/15/16 C:136 Project: SY371                                 *
     H*                Implement Scriptura processing                 *
     H*                                                               *
     H* 12/03/15 C:135 Support: 34655                                 *
     H*                Correct spelling for 'Direct Inquiries to:'    *
     H*                                                               *
     H* 06/16/15  C:309 Support: 35073                                *
     H*                 Fixed extra comma after "Remit to:" zip code  *
     H*                                                               *
     H* 06/10/14  C:180 Support: WA114 84793                          *
     H*                 Print deductible invoice for active div       *
     H*                                                               *
     H* 12/03/13  C:180 Support: WA114 83383                          *
     H*                 Fix error: Summary page not printing.         *
     H*                                                               *
     H* 08/02/12 CL136 SUPPORT: WA025 80472                           *
     H*                PRIOR YEARS MISSING FROM TASB BREAK OUT        *
     H*                SECTION                                        *
     H*                                                               *
     H* 07/09/12 C:136 SUPPORT: WA114 80275                           *
     H*                CLEAR ARRAYS USED FOR CSI SUMM INV ON GROUP    *
     H*                BREAK                                          *
     H*                                                               *
     H* 06/15/12 C:136 SUPPORT: 114WYZ                                *
     H*                IMPLEMENT CORRECTION FOR 73789                 *
     H*                                                               *
     H* 04/19/12 C:279 SUPPORT: WA093  77633                          *
     H*                Modified to not print years having zero due    *
     H*                balance.                                       *
     H*                                                               *
     H* 02/08/12 C:279 SUPPORT: WA132  78481                          *
     H*                Modified to add processing for Current Account *
     H*                policy.                                        *
     H*                                                               *
     H* 10/26/11 C:136 SUPPORT: WA114 78313                          *
     H*                ADJUST ADDRESS LOGIC TO ACCOMODATE SRADDR      *
     H*                NOW RETURNING THE CONTACT NAME                 *
     H*                                                               *
     H* 08/17/11 C:189 PROJECT  114WYZ  PRISS# 41                     *
     H*                Deductibe invoice not being generated for      *
     H*                Large deductible type.                         *
     H*                                                               *
     H* 08/09/11 C:191 PROJECT: 114WYZ PRISS#45                       *
     H*                FIX SUMMARY INVOICE FORMATTING                 *
     H*                                                               *
     H* 08/02/11 C:061 PROJECT  WC2003                                *
     H*                Pass "Print SSN" parm to W7936C2.              *
     H*                                                               *
     H* 07/18/11 C:062 SUPPORT: WA093 77402                           *
     H*                Compile Only                                   *
     H*                                                               *
     H* 07/13/11 C:191 PROJECT: 114WYZ PRISS#41                       *
     H*                - AMOUNT DUE IN HEADER/FOOTER                  *
     H*                - PREVIOUS UNPAID BALANCE                      *
     H*                - TOTAL AMOUNT                                 *
     H*                - ADDRESS ON SUM INV PRINT FROM STATE REP DIV  *
     H*                - RESOLVE SUMM DED INV TO STATE REP DIV        *
     H*                                                               *
     H* 06/28/11  C:189 Seq#: WA025/76502                             *
     H*                      Change the prefrence of the DART code    *
     H*                      which needs to be populated              *
     H*
     H* 05/05/11 C:189 PROJECT: 25WRX Priss 40                        *
     H*                After printing the years in descending order   *
     H*                space is being left at the top instead of the  *
     H*                bottom                                         *
     H*                                                               *
     H* 04/27/11 C: 58 PROJECT: 114WYZ                                *
     H*                1) Change Deductible Invoice: Print            *
     H*                "Due upon receipt" in place of the posting date*
     H*                based on new flag in WMEXP. Calculate & print  *
     H*                Prior Balance line above Current Balance line  *
     H*                with separator lines.                          *
     H*                2) Add new Summary Invoice for multiple        *
     H*                division Employers, W7325O1.                   *
     H*                                                               *
     H* 02/09/11 C:255 PROJECT: 25WRX                                 *
     H*                1) Show Group no. in participant description.  *
     H*                2) Change breakout of current billed by policy *
     H*                   yr. to be in descending order               *
     H*                                                               *
     H* 01/07/11 C:136 SUPPORT: WA071 75370                           *
     H*                SUPPRESS PRINTING OF DEDUCTIBLE DETAIL REPORT  *
     H*                FOR CLT071.                                    *
     H*                                                               *
     H* 09/29/10 C:33  Project: SY190  Priss #166                     *
     H*                When using group select, retrieve the DARTS    *
     H*                 code from Admin/DARTS for the specific group  *
     H*                                                               *
     H* 08/04/10 C:246 SUPPORT: WA093 73789                           *
     H*                Fix the issue with duplicate city and zipcode. *
     H*                                                               *
     H* 06/25/10 C:189 Project: WC1878                                *
     H*                 Add Single Employer Processing to the         *
     H*                 Deductible Invoice Deletion                   *
     H*                                                               *
     H* 06/05/10 C:136 SUPPORT: WA139 73011                           *
     H*                MAKE INTERNAL USE MSG CLIENT SPECIFIC FOR      *
     H*                126 ONLY                                       *
     H*                                                               *
     H* 06/03/10 C:189 SUPPORT: WA025 72955                           *
     H*          C:136 INCREASE TOTAL FIELDS FOR LARGE AMOUNTS.       *
     H*                CLIENT SPECIFIC FOR TASB                       *
     H*                                                               *
     H* 05/27/10 C:220 SUPPORT: 72747                                 *
     H*                Add policy no in the bottom info for KRMS.     *
     H*                                                               *
     H* 04/20/10 C180  SUPPORT 72420
     H*                PRINTER FILE CHANGES ONLY - CHANGED EDTWRD
     H*                FOR DEDUCTIBLE BALANCE DUE.
     H*
     H* 04/07/10 C:136 PROJECT: 25WRB PRISS 31                        *
     H*                ADDITIONAL CHANGES NEEDED FOR INVOICE          *
     H*                                                               *
     H* 04/05/10-A  C:220 PROJECT: 25WRQ                              *
     H*                   MOVE TITLE AND LOGO BOX FOR CLIENT - TASB   *
     H*                                                               *
     H* 04/05/10  C:62  Project: 25WRB Priss 19                       *
     H*                 NUMEROUS PRINT CHANGES.                       *
     H*                                                               *
     H* 03/17/10  C:65  Project: 25WRB Priss                          *
     H*                -ADD ANOTHER PARM TO W7936C2 CALL.             *
     H*                                                               *
     H* 03/15/10  C:33  Project: 25WRB Priss #16                      *
     H*                -Use DDINV# instead of EAINV# for printing.    *
     H*                -Print county description along with disctrict.*
     H*                                                               *
     H* 03/13/10  C:33  Project: 25WRB Priss                          *
     H*            #14 -Use different parms to call W7936C2 so as not *
     H*                  to change parms passing back to calling pgm. *
     H*            #16 -Use correct logo name.                        *
     H*                                                               *
     H* 03/12/10  C:191 Project: 25WRB PRISS                          *
     H*                - Data retained in XXNUM                       *
     H*                                                               *
     H* 02/05/10  C:65  Project: 25WRB                                *
     H*                -ADD CALL TO W7936C2 TO PRODUCE DEDUCTIBLE     *
     H*                 DETAIL REPORT IN SINGLE EMPLOYER MODE TO      *
     H*                 RESOLVE IT TO DARTS WITH THE INVOICE.         *
     H*                                                               *
     H* 02/01/10  C:33  Project: 25WRB                                *
     H*                -Process new deductible invoice for CLT025     *
     H*                -Change to use group select processing         *
     H*                                                               *
     H* 01/11/10  C:33  Project: WC1838                               *
     H*                 Process Large & Small deductibles             *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -Print 'Statement #' along with the invoice #   *
     H*                                                               *
     H* 10/27/09 C:190 SEQ#70025                                      *
     H*                Open W7325O before NEWF & close it after NEWF  *
     H*                to avoid the abend.                            *
     H*                                                               *
     H* 03/10/09 C:136 PROJECT: 114WRK PRISS 14                       *
     H*               -REFORMAT NEW FORMAT BILL PER JOHN MCCALLISTER  *
     H*               -FIX 71WJU - IT BROKE 114WRK                    *
     H*                                                               *
     H* 01/08/09 C:189 PROJECT: 71WJU                                 *
     H*                DEDUCTIBLE INVOICES TO RESOLVE TO DARTS        *
     H*                NOTE: QSYSPRT & QSYSPRT1 WILL NO MORE          *
     H*                GENERATED AND A SEPARATE DEDUCTABLE REPORT     *
     H*                WILL BE PRODUCED FOR EACH POLICY               *
     H*                                                               *
     H* 10/07/08 C:65  PROJECT: 114WRK                                *
     H*                ADD NEW FORMAT LAYOUT FOR PREMIUM NOTICE.      *
     H*                                                               *
     H* 09/24/07  C:190 Project 135WKC                                *
     H*                 Add new parameters to entry parm list.        *
     H*                                                               *
     H* 09/07/07 C:193 PROJECT: 71WHI - BACK TO BASE PHASE 2          *
     H*                USE NEW PRINTER FILE FOR ECM'S VERSION OF THE  *
     H*                DETAIL INVOICE                                 *
     H*                                                               *
     H* 09/26/05 C:61  PROJECT 135WED WC1336                          *
     H*              - Process for a Group Range.                     *
     H*              - CLEAR Summary Invoice Values after print.      *
     H*                Add "B" to output specs QSYSPRT1.              *
     H*                                                               *
     H* 10/07/05 C:65  PROJECT: WC1317                                *
     H*                PRINT EXPANDED NAME FIELDS FOR EMPLOYER NAMES. *
     H*                                                               *
     H*  04/07/05 C:156 PROJECT: 126WDK                               *
     H*                 PROJECT: 126WAO                               *
     H*                 ADD NEW SPOOL FILE FOR SUMMARY INVOICE.  THIS *
     H*                 INVOICE WILL ONLY PRINT FOR PEO GROUPS.       *
     H*                                                               *
     H*  07/01/04 C:15  Support: WC072/43068                          *
     H*                 Chgd code to only load the Deduct Tax billed  *
     H*                 amount from WDEAP if the batch has not been   *
     H*                 transferred.                                  *
     H*                                                               *
     H*  02/16/04 C:15  Project: WC311                                *
     H*                 While testing 126WCD, I found that policy     *
     H*                 years are skipped if ELDCDE is blank, even if *
     H*                 a WDDDP record existed for that year.  This   *
     H*                 is only a problem if a ded code is added to a *
     H*                 policy then removed later after a billing has *
     H*                 occurred.                                     *
     H*                                                               *
     H*  11/12/03 C:132 Project: WA0129 SEQ: 39834                    *
     H*                 Change to back out load of LOCK BOX address   *
     H*                 Commented out the Flag used to indicate that  *
     H*                 LOCK BOX address to be used, can reinstall by *
     H*                 uncommenting this code                        *
     H*                                                               *
     H*  08/12/03 C:132 Project: WA093  SEQ: 38382                    *
     H*                 Change on output to print Office Name, had    *
     H*                 used the City,ST,Zip twice instead of ofc Name*
     H*                                                               *
     H*  06/02/03 C:132 Project: WC1117                               *
     H*                 Changed to use GROUP NAME instead of Office   *
     H*                 Accounting Name if FNCGFL <> "Y" for          *
     H*                 Self-Insured Groups                           *
     H*                                                               *
     H*  05/08/03 C:132 Project: WC1117                               *
     H*                 Changed to use LOCK BOX address (if applies)  *
     H*                 only on the remittance stub only.             *
     H*                                                               *
     H*  02/13/03 C:132 Project: WC1117                               *
     H*                 Added LOCK BOX address check in group         *
     H*                 auxiliary file. If present in WMF2P, use the  *
     H*                 Lock box address instead of accounting office *
     H*                 address.                                      *
     H*                                                               *
     H* 01/09/03 C:136 Support: WA072 35423                           *
     H*                Report wasn't processing tax billings correctly*
     H*                because the deductible bill tax A/R code from  *
     H*                the fund detail year needs to correspond with  *
     H*                the A/R batch policy year being processed.     *
     H*                                                               *
     H* 12/19/02 C:136 Support: WA072 35214                           *
     H*                Report wasn't processing tax billings correctly*
     H*                because the posting date passed in was getting *
     H*                the wrong fund detail year.                    *
     H*                                                               *
     H* 06/29/02 C:136 Project: WC1080 (KY Deductible Billing Tax)    *
     H*                Modified output to include deductible tax      *
     H*                amounts.                                       *
     H*                                                               *
     H* 01/31/02 C:61  PROJECT WC1070                                 *
     H*                Get Employer Contact info from WMECP.          *
     H*                                                               *
     H* 05/05/99 C:65 PROJECT: CM039                                  *
     H*               ADD NEW COSMETICS FOR ACCOUNTING INFO PROJECT:  *
     H*               PLACE ACCOUNTING NAME AND ADDRESS AT TOP        *
     H*               ADD CARRIER POLICY # IF IT EXISTS               *
     H*               CHANGE POLICY NUMBER TO FILE NUMBER             *
     H*               MOVE THE CANCELLATION DATE UP.                  *
     H*               ADD A PERFORATED LINE '---------------'         *
     H*               MOVE CHECKS PAYABLE TO LINE UP BELOW LINE       *
     H*               ADD A DIRECT INQUIRIES TO LINE BELOW CHECKS     *
     H*               ALWAYS USE ACCOUNTING NAME/ADDRESS FOR REMIT TO *
     H*                                                               *
     H* 03/09/99 C:15 Support: WC065/15291                            *
     H*               Fixed pgm, the last invoice was not always      *
     H*               being created.                                  *
     H*                                                               *
     H* 12/10/98 C:61 SUPPORT 14595                                   *
     H*               Only process active (WMA3P/A3STAT = 'A')        *
     H*               Employer 3rd Address records.                   *
     H*                                                               *
     H* 10/09/98 C:65 PROJECT: SUPPORT                                *
     H*               CHECK IF EMPLOYER A3 ADDRESS IS BLANK.          *
     H*                                                               *
     H* 06/04/98 C:65 PROJECT: WC670                                  *
     H*               USE ACCOUNTING INFO FOR ADDRESSES AND BREAKOUT  *
     H*               THE BILLED INFORMATION BY POLICY YEAR.          *
     H*                                                               *
     H* 05/28/98 C:65 PROJECT: WC670                                  *
     H*               CHANGE CURRENT DATE TO PRINT DATE AND REMOVE    *
     H*               BILLING FYR LINE.                               *
     H*                                                               *
     H* 04/07/98 C:65 PROJECT: WC670                                  *
     H*               CHANGE REPORT/INVOICE DATES TO XX/XX/XXXX FORM. *
     H*                                                               *
     H* 11/25/96 C:47 PROJECT: WC600                                  *
     H*               EXCLUDE EMPLOYERS WHO ARE NOT ON A              *
     H*               DEDUCTIBLE PLAN.                                *
     H*                                                               *
     H* 09/22/95 C:24 PROJECT: CM005                                  *
     H*               CALCULATE THE BILLING FUND YEAR SEPARATELY FOR  *
     H*               EACH EMPLOYER.  BILL THE FUND YEAR OF THE POLICY*
     H*               ACTIVE AS OF THE POSTING DATE, OR THE LAST      *
     H*               ACTIVE POLICY IF NO CURRENT POLICY IS FOUND.    *
     H* ***************************************************************
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMEML     IF   E           K DISK
      *
      * Employer Contact
     FWMECL1    IF   E           K DISK
     FWDDDL1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSY01)
     FWMEAL     IF   E           K DISK
     FWDABL8    IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWDELL2    IF   E           K DISK
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
      *** Beg Add ***** 01/17/25  **********************************
     F                                     infds(dbfds)
     fxmlprel1  uf   e           k disk
     Fxmlprel5  uf   e           k disk    rename(xp100:xp105)
      *** End Add ***** 01/17/25  **********************************
     FWMA3L     IF   E           K DISK
     FWMOFL     IF   E           K DISK
     FWMRPL     IF   E           K DISK
      * Batch Detail Fund
     FWDEAL5    IF   E           K DISK
     F                                     RENAME(PEA01:PEA05)
     FWMEXL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
      * Application System Control
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWXGSL     IF   E           K DISK
     F                                     RENAME(PGS01:PGS00)
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     FW7325O    O    E             PRINTER OFLIND(*IN01)
     F                                     USROPN
     FW7325O1   O    E             PRINTER OFLIND(*IN02)
     F                                     USROPN
     fwxdedinv  o    e             disk    usropn
     fwxdedinv1 o    e             disk    usropn
     fwxsuminv  o    e             disk    usropn
     fwxsuminv1 o    e             disk    usropn
      *** Beg Add ***** 01/17/25 *******************************
     fwxdedcrt  o    e             disk    usropn
     fwxdedcrt1 o    e             disk    usropn
      *** End Add ***** 01/17/25 *******************************
     f***xmlprel1  uf   e           k disk    usropn rename(xp100:xp101)
     fwximp     o    e             disk

      *** Beg Add ***** 01/17/25  **********************************
     dsvLDA            ds          1024    dtaara(*lda)
     d ldEnd#                         5  0 overlay(svlda:1)
     d ldEndA                         5    overlay(svlda:1)
     d keyEmail                      40    overlay(svLDA:56)
     d keySeq#                        5    overlay(svLDA:96)
     d ldMassFile                    60    Overlay(svlda:129)
     d ldOutputFile                  60    Overlay(svlda:191)
     d xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d x1Copy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)

       Dcl-S processType Char(10) Inz('DedInvoice');
       Dcl-S processTypeD Char(60) Inz('Deductible Invoice');
       Dcl-S processTypeS Char(60) Inz('Deductible Summary Invoice');
       Dcl-S xxPre Char(2) Inz('DI');
       Dcl-S isReprint Ind Inz(*Off);
       Dcl-S xmlLink Like(XPlink);
       Dcl-S flowFileName Like(XPxml);
       Dcl-S xmlFileBlank Like(XPxml);
       Dcl-S isMassReq Ind Inz(*Off);
       Dcl-S massFlag Char(1);
       Dcl-S message Char(250);
       Dcl-S inXml Char(61);
       Dcl-S string Char(10);
       Dcl-S strXPFile Char(60);
       Dcl-S strXPDir  Char(60);
       Dcl-S strXPFileSRD Char(60);
       Dcl-S strXPDirSRD  Char(60);
       Dcl-S xsRRN Packed(9:0) Inz(0);
       Dcl-S x#RRN Packed(9:0) Inz(0);
       Dcl-S svSeq# Zoned(5:0);
       Dcl-S svSeq#SRD Zoned(5:0);
       Dcl-S strEmailD Char(1000);
       Dcl-S pgmName Char(10);
       Dcl-S pgmExists Char(1);
       Dcl-S svQ#dcId Like(Q#dcId);
       Dcl-S svQ#fold Like(Q#fold);
       Dcl-S svFnd Like(GSfnd) Inz(0);
       Dcl-S flgPeoInv Char(1);
       Dcl-S undLoc Zoned(3:0);
      *** End Add ***** 01/17/25  **********************************

     d isUseScriptura  s              1n   inz
     d isUseSpoolOut   s              1n   inz
     d isUseXML        s              1n   inz
     d xmlFile         s             60    inz
     d WrtDedInvXML    pr
     d WrtDedInv1XML   pr
     d WrtFyrBrkOut    pr             1
     d  fyr                           3  0
     d  amt                          13  2
     d WrtSumInvXML    pr
     d WrtSumInv1XML   pr
      *** Beg Add ***** 01/17/25  **********************************
     d WrtDedCrtXML    pr
     d GenScriptOut    pr                  ExtPgm('S4401C')
     d inXml_                        61
     d StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     d ChkPgmStack     pr                  ExtPgm('SRCHKPGM')
     d   pgmName                     10
     d   pgmExists                    1
      *** End Add ***** 01/17/25  **********************************

     d/copy sPrModule
     d/copy sPrXML
     d/copy wPrPmtSvc
     d DocRoot         pr                  ExtPgm('RTVDOCROOT')
     d   outRoot                     10
     d   outRoot       s             10
     d WrtDrtRmk       pr                  extpgm('DA700R')
     d ##key                         40
     d ##comm                       256
     d ##cDat                         8
     d ##dcId                        12
     d ##pNam                        10
     d ##secl                         3
     d ##user                        10
     d ##pgm                         10
     d ##fold                        80
     d ##diFl                         1
     d ##iPgm                        10
     d ##fl01                         1
     d ##fl02                         1
     d ##rmCd                         5
     d ##seq#                         5
     d ##sbyl                         1
     d ##com2                        19
     d ##apId                         2
     d ##iDoc                        12
     d ##iQue                        10
     d ##imgF                         1
     d ##iPth                        10
     d ##pIdx                         5
     d ##qPty                         1
     d ##dcTp                         5
     d ##csId                         2
     d ##code                         2

     D SZ              S              1    DIM(13)
     D CHI             S              1    DIM(20)
     D CHO             S              1    DIM(20)
     D ART             S              3  0 DIM(12)
     D ARJ             S             13  2 DIM(12)
     D ARU             S              3  0 DIM(100)
     D ARM             S             13  2 DIM(100)
     D ARB             S             13  2 DIM(12)
     D ARZ             S              3  0 DIM(12)
     D KDIV            S              5  0 DIM(999)
     D KDBN            S             50    DIM(999)
     D KAMD            S             13  2 DIM(999)
     D KYR             S              3  0 DIM(99)
     D KYAM            S             13  2 DIM(99)
     D OCL             S              1    DIM(32)
     D WTL             S              1    DIM(46) CTDATA PERRCD(46)
     D MN              S              9    DIM(12) CTDATA PERRCD(1)
     D                 DS
     D  EMN                    1    400
     D                                     DIM(8)
     D  XXEMN1                 1     50
     D  XXEMN2                51    100
     D  XXEMN3               101    150
     D  XXEMN4               151    200
     D  XXEMN5               201    250
     D  XXEMN6               251    300
     D  XXEMN7               301    350
     D  XXEMN8               351    400
     D                 DS
     D  ATT                    1     40
     D                                     DIM(8)
     D  XXATT1                 1      5
     D  XXATT2                 6     10
     D  XXATT3                11     15
     D  XXATT4                16     20
     D  XXATT5                21     25
     D  XXATT6                26     30
     D  XXATT7                31     35
     D  XXATT8                36     40
     D*
     D*    HI-LO to alphabetic
     D                 DS
     D  XKDIV                  1     13
     D  LODIV                  1      5  0 INZ
     D  DASH                   7      7
     D  HIDIV                  9     13  0 INZ
     D*
     D*    Deductible Summary: Attn + N&A
     D                 DS
     D  KMN                    1    400
     D                                     DIM(8)
     D  XKEMN1                 1     50
     D  XKEMN2                51    100
     D  XKEMN3               101    150
     D  XKEMN4               151    200
     D  XKEMN5               201    250
     D  XKEMN6               251    300
     D  XKEMN7               301    350
     D  XKEMN8               351    400
     D                 DS
     D  EMN1                   1    480
     D                                     DIM(8)
     D  X1EMN1                 1     60
     D  X1EMN2                61    120
     D  X1EMN3               121    180
     D  X1EMN4               181    240
     D  X1EMN5               241    300
     D  X1EMN6               301    360
     D  X1EMN7               361    420
     D  X1EMN8               421    480
     D                 DS
     D  INS                    1    200
     D                                     DIM(4)
     D  XXINS1                 1     50
     D  XXINS2                51    100
     D  XXINS3               101    150
     D  XXINS4               151    200
     D                 DS
     D  ARY                    1     36  0
     D                                     DIM(12)
     D  ARY01                  1      3  0
     D  ARY02                  4      6  0
     D  ARY03                  7      9  0
     D  ARY04                 10     12  0
     D  ARY05                 13     15  0
     D  ARY06                 16     18  0
     D  ARY07                 19     21  0
     D  ARY08                 22     24  0
     D  ARY09                 25     27  0
     D  ARY10                 28     30  0
     D  ARY11                 31     33  0
     D  ARY12                 34     36  0
     D                 DS
     D  ARA                    1    156  2
     D                                     DIM(12)
     D  ARA01                  1     13  2
     D  ARA02                 14     26  2
     D  ARA03                 27     39  2
     D  ARA04                 40     52  2
     D  ARA05                 53     65  2
     D  ARA06                 66     78  2
     D  ARA07                 79     91  2
     D  ARA08                 92    104  2
     D  ARA09                105    117  2
     D  ARA10                118    130  2
     D  ARA11                131    143  2
     D  ARA12                144    156  2
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  OFAPHN                 1      7  0
     D  XXPHN1                 1      3  0
     D  XXPHN2                 4      7  0
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  EMSEMP                 1     15
     D  XXCNDX                 2      7
     D                 DS
     D  XXOCR                  1     47
     D  ##TYPE                 1      1
     D  #1FND                  2      4  0
     D  #1EMP#                 5     13  0
     D  #1DIV                 14     18  0
     D  ##FYR                 19     21  0
     D  XXAMT1                22     32  2
     D  XXCKDG                33     33  0
     D                 DS
     D  XQDT                   1      8  0
     D  XXMTH                  1      2  0
     D  XXDAY                  3      4
     D  XXYR                   5      8
      *** Beg Add ***** 01/17/25 ***************************************
     D dbfds           ds
     D  dbrrn                397    400b 0
      *** End Add ***** 01/17/25 ***************************************
     D*  Workfields
     d dtSysDate       s               d   datfmt(*ISO)
     d str3rdVrb       s                   Like(OF3rdVrb)
     d strInv#         s             50
     d*** Beg Add *** 11/18/24 *****************************************
     d sysDate7        s              7s 0
     d*** End Add *** 11/18/24 *****************************************
     D*
     D*  Constants
     D KHDBIL          C                   CONST('       Date of Bill:')
     D KHPOLN          C                   CONST('      Policy Number:')
     D KHDIV           C                   CONST('           Division:')
     D KHCAND          C                   CONST('  Cancellation Date:')
     D KHLOSA          C                   CONST('       Losses as of:')
     D KHADUE          C                   CONST('         Amount Due:')
     D KHDUED          C                   CONST('           Due Date:')
     D KHDUOR          C                   CONST('    Due Upon Receipt')
     D*
     D CARPOL          C                   CONST('Carrier Policy #: ')
     D POL             C                   CONST('Policy #:')
     D FILE            C                   CONST('File #:')
     D PART            C                   CONST('Participant #:')
     D PRTPD           C                   CONST('Participation -
     D                                     Period:')
     D CNDTH           C                   CONST('County-District #:')
     D CHRGS           C                   CONST('Charges')
     D PYMNTS          C                   CONST('Payments')
     D BAL             C                   CONST('Balance')
     D AMDU            C                   CONST('Amount Due:')
     D DUEH            C                   CONST('Due Date:')
     D INSH            C                   CONST('MEMBER:')
     D AMRE            C                   CONST('Amount Remitted:')
     D ADM1            C                   CONST('Administered by the -
     D                                     Texas Association -
     D                                     of School Boards, -
     D                                     Inc.')
     D RTNW            C                   CONST('(Please Return With -
     D                                     Remittance)')
     D ATCH            C                   CONST('See Attached')
     D LOSSH           C                   CONST('Losses as of:')
     D CANDH           C                   CONST('Cancellation Date:')
     D DEDHDR          C                   CONST('DEDUCTIBLE INVOICE')
     D SUMHDR          C                   CONST('SUMMARY DEDUCTIBLE-
     D                                      INVOICE')
     D WCHDR           C                   CONST('WORKERS'' -
     D                                     COMPENSATION')
     D DEDH25          C                   CONST('CLAIMS LIABILITY')
     D BREAKH          C                   CONST('Break Out of Current-
     D                                      Billed by Policy-
     D                                      Year:')
     D SEEH            C                   CONST('See Itemization Atta-
     D                                     ched for Detail')
     D BATH            C                   CONST('Internal Use: Pendin-
     D                                     g Payment Batch#')
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D CHGNUM          C                   CONST('SA020R')
     D DA              C                   CONST('DEDUCTIBLE INV.RUN -
     D                                     ON:')
     D D71             C                   CONST('DEDUCTIBLE INVOICE -
     D                                      RUN ON:')
     D DA2             C                   CONST('FOR  A/R POSTING DAT-
     D                                     E:')
     D DD              C                   CONST('DEDUCTIBLE DETAIL RE-
     D                                     PORT ATTACHED.')
     D DAS             C                   CONST('SUMMARY DEDUCTIBLE -
     D                                     INVOICE RUN ON:')
     D                 DS
     D  ZZKEY                  1     40
     D  ZZCO#                  1      3  0
     D  ZZFND                  4      6  0
     D  ZZEMP#                 7     15  0
     D  ZZDIV                 16     20  0

     IPEA05
     I              EAARCD                      $AARCD
     I              EAAMT                       $AAMT
     I              EAPODT                      $APODT
     I              EAFYR                       $AFYR
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *  Employer Contact
      *

     C     ECKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKPCNT            1
      *
     C*
     C*   KEY LIST FOR THE COMPANY MASTER FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE OFFICE MASTER FILE
     C*
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMOFF#
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBFYR
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C*
     C     EMKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C*
     C*  KEY LIST FOR THE DEDUCTIBLE DETAIL FILE
     C*
     C     DDKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     DDKEY#        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X@EMP#
     C                   KFLD                    X@DIV
     C*
     C     DDKEY@        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X@EMP#
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR THE EMPLOYER A/R FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    X2PODT
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C*
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    DDEMP#
     C                   KFLD                    DDDIV
     C                   KFLD                    DDFYR
      *
      * KEY LIST FOR THE CARRIER/STATE REPORTING FILE
      *
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    XXBFYR
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    XXBFYR
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#            5 0
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C     CHGPL2        PLIST
     C                   PARM                    XXNUM
     C                   PARM                    EDTMSK
     C                   PARM                    ##DEC             3 0
     C                   PARM                    ##THOU            1
      *
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C     *LIKE         DEFINE    DDCO#         XXCO#
     C     *LIKE         DEFINE    DDPODT        X2PODT
     C     *LIKE         DEFINE    BCKEY         XXKEY
     C     *LIKE         DEFINE    EMMAD2        XXCSZ
     C     *LIKE         DEFINE    EMMCTY        XXCTY
     C     *LIKE         DEFINE    EMMST         XXST
     C     *LIKE         DEFINE    *IN60         SUMD
     C     *DTAARA       DEFINE                  DEDINV           10
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQLSDT            8
     C                   PARM                    QQPODT            8
     C                   PARM                    QQPRDT            8
     C                   PARM                    QQCOM1            2
     C                   PARM                    QQCOM2            2
     C                   PARM                    QQCOM3            2
     C                   PARM                    QQCOM4            2
     C                   PARM                    QQTFND            3
     C                   PARM                    QQFRGN            3
     C                   PARM                    QQTRGN            3
     C                   PARM                    QQDFLG            3
     C                   PARM                    QQDRMK            5
     C                   PARM                    QQDDTP            1
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQY4N             1
     C                   PARM                    QQOUTQ           10
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
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    SVEMP#
     C                   PARM                    SVDIV
     C                   PARM                    XXFYR             3 0
     C                   PARM      *zeros        XXAGT             5 0
     C                   PARM      *blanks       XXANAM           60
     C                   PARM                    XXPROD            5 0
     C                   PARM                    XXPNAM           60
     C                   PARM                    XXAREA            3 0
     C                   PARM                    XXPHON            7 0
     C                   PARM                    XXPLAN            1
     C                   PARM                    XXDATE            8 0
     C                   PARM                    XXNWRT            5 2
     C                   PARM                    XXRNRT            5 2
     C                   PARM                    XX@RT#            3 0
     C                   PARM                    XXPEML           60
     C*
     C     ABKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
      *
     C                   Z-ADD     *ZERO         X@EMP#            9 0
     C                   Z-ADD     *ZERO         X@DIV             5 0

     C                   CLEAR                   @@OUTQ           10
     C                   IF        Q1PRMS >= 19
     C                   EVAL      @@OUTQ = QQOUTQ
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      18
     C                   MOVE      QQY4N         #1Y4N             1
     C                   ELSE
     C                   MOVE      'Y'           #1Y4N
     C                   ENDIF
      *
     C*
     C     Q1PRMS        IFGT      15
     C                   MOVE      QQEMP#        X@EMP#
     C                   MOVE      QQDIV         X@DIV
      *
     C*
     C                   SELECT
     C     QQDIV         WHENNE    *BLANKS
     C                   MOVE      *ON           PRCDIV            1
     C     QQEMP#        WHENNE    *ZERO
     C     QQEMP#        ANDNE     *BLANKS
     C                   MOVE      *ON           PRCEMP            1
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C     Q1PRMS        IFGE      11
     C                   MOVE      QQFRGN        XXFRGN            3 0
     C                   MOVE      QQTRGN        XXTRGN            3 0
     C                   ELSE
     C                   Z-ADD     *ZERO         XXFRGN
     C                   Z-ADD     *ZERO         XXTRGN
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      10
     C                   MOVE      QQTFND        XXTFND            3 0
     C                   ELSE
     C                   Z-ADD     *ZERO         XXTFND
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      13
     C                   MOVEL     QQDFLG        XXDFLG            1
     C                   ENDIF
     C     Q1PRMS        IFGE      14
     C                   MOVE      QQDRMK        XXDRMK            5
     C                   ENDIF
     C*
     C     *LIKE         DEFINE    QQDDTP        XXDDTP
     C                   MOVE      'S'           XXDDTP
     C     Q1PRMS        IFGE      15
     C     QQDDTP        ANDNE     *BLANK
     C                   MOVE      QQDDTP        XXDDTP
     C                   ENDIF
     C*
     C                   MOVE      *OFF          USESML            1
     C                   MOVE      *OFF          USELRG            1
     C*
     C     XXDDTP        IFEQ      'S'
     C     XXDDTP        OREQ      'B'
     C                   MOVE      *ON           USESML
     C                   ENDIF
     C*
     C     XXDDTP        IFEQ      'L'
     C     XXDDTP        OREQ      'B'
     C                   MOVE      *ON           USELRG
     C                   ENDIF
     C*
     C                   MOVE      QQPODT        XXPODT            8 0
     C*
     C                   Z-ADD     XXPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2PODT
     C*
     C                   MOVE      QQLSDT        X1LSDT            8 0
     C*
     C                   MOVEL     X1LSDT        XXLSDT            8 0
     C*
     C                   MOVE      *ZEROS        ED
     C                   MOVE      *ZEROS        CD
      *
      * USE THE LATER OF THE POST DATE AND PRINT DATE FOR THE DUE DATE FOR TASB
      * SEE 25WRB, PRISS 22 FOR DETAILS
      *
     C                   MOVE      QQPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PRDT7             7 0
     C     X2PODT        IFGT      PRDT7
     C                   MOVE      QQPODT        XXDUED            8 0
     C                   ELSE
     C                   MOVE      QQPRDT        XXDUED
     C                   ENDIF
     C                   MOVE      QQPRDT        X1PRDT            8 0
     C*
     C                   MOVEL     X1PRDT        XXPRDT            8 0
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C*
     C*
     C     COKEY         CHAIN     PCO01                              60
     **   CONVERT SYSTEM DATE AND TIME TO 8 DIGIT BILLING DATE
     **
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         RPDATE            8 0
       //*** Beg Add *** 11/18/24 ******************************************
       sysDate7 = %dec(%char(%date():*cymd0):7:0);
       //*** End Add *** 11/18/24 ******************************************
     **
     C                   MOVE      QQLSDT        RSDATE            8 0
     C                   MOVE      QQPODT        RSDAT2            8 0
     **
     C     ORKEY         CHAIN     POR01                              60
     C*
     C* PULL THE HOLD STATUS FOR NEW FORMAT BILLS FROM REPORT #0.
     C*
     C                   MOVE      '*YES'        X1HOLD            4
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ORHOLD        X1HOLD
     C                   ENDIF
      *
      *  Get Office Install Flag
      *
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
      *
     C     UPKEY         CHAIN     PUP01                              60

       //*** Beg Add ***** 01/17/25 *******************************************
       Clear xmlLink;
       Clear flowFileName;
       isMassReq = *on;
       massFlag = 'Y';
       isUseSpoolOut = UseSpoolOut(processTypeD);
       isUseXML = UseScriptura(processTypeD) or Use3rdXml(processTypeD);

       //Check if called from Deductible Invoice Reprint
       pgmName   = 'W7325C';
       pgmExists = *Off;
       ChkPgmStack(pgmName:pgmExists);
       If pgmExists = *On;
          isReprint  = *On;
       EndIf;
       //*** End Add ***** 01/17/25 *******************************************

      *  Process Groups
     C                   MOVE      *OFF          USEGRP            1
     C     XXCO#         CHAIN     PGS00                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           USEGRP
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C* Use the group select groups
     C     USEGRP        WHENEQ    *ON
     C*
     C     XXCO#         CHAIN     PGS00                              60
     C*
     C     *IN60         DOUEQ     *ON
     C*
     C                   MOVE      *OFF          VLDGRP            1
     C                   Z-ADD     GSFND         XXFND
     C     FNKEY         CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      *ON           VLDGRP
     C                   ENDIF
     C*
     C     XXTRGN        IFNE      0
     C*
     C     FNKEY         CHAIN     PEM01                              99
     C*
     C  N99EMRGN#        IFLT      XXFRGN
     C     EMRGN#        ORGT      XXTRGN
     C                   MOVE      *OFF          VLDGRP
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     VLDGRP        IFEQ      *ON
     C     GSIEFL        ANDEQ     'I'
     C                   EXSR      SRFND
     C                   ENDIF
     C*
     C     XXCO#         READE     PGS00                                  60
     C*
     C                   ENDDO
     C*
     C                   OTHER
     C*
     C* Use the group range to select groups
      *
     C     XXFND         DOUGT     XXTFND
     C     FNKEY         CHAIN     PFN01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     XXTRGN        IFNE      0
     C     FNKEY         CHAIN     PEM01                              99
     C  N99EMRGN#        IFLT      XXFRGN
     C     EMRGN#        ORGT      XXTRGN
     C     XXFND         IFEQ      999
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             XXFND
     C                   ITER
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      SRFND
     C                   ENDIF
      *
     C     XXFND         IFEQ      999
     C                   LEAVE
     C                   ENDIF
      *
     C                   ADD       1             XXFND
     C                   ENDDO
     C*
     C                   ENDSL
      *
       //*** Beg Add ***** 01/17/25  **********************************
       If isUseXml and (x@Emp# <> 0 or isReprint) ;
          Select;
          When ldMassFile = *Blanks;
          Other;
             inXml = ldMassFile + '$';
             Monitor;
                GenScriptOut(inXml);
             On-error;
             Endmon;
          Endsl;
       Endif;
       //*** End Add ***** 01/17/25  **********************************


     C                   MOVE      *ON           *INLR
      *
      *
       //===========================================================
       // Write to Image Cross Reference File WMIXP
       //===========================================================
       Begsr SrImageXref;
          IMco# = EAco#;
          IMfnd = EAfnd;
          IMEmp# = EAemp#;
          IMdiv  = EAdiv;
          IMinv# = EAinv#;
          IMtype = 'Deductible invoice';
          IMpoDt = EApoDt;
          Callp DocRoot(outRoot);
          If isUseXml = *on;
             IMuNcn = %Trim(XPDir) + %Trim(XPFile);
             IMseq# = XPdrSeq#;
          Else;
             IMuNcn = '/' + %Trim(outRoot) + '/' + %Trim(q#Fold) + '/' +
             %Trim(q#DcId);
             Monitor;
             IMseq# = %Dec(q#Seq#:5:0);
             On-Error;
                Imseq# = 0;
             Endmon;
          Endif;
          IMstat = 'A';
          IMuser = Q1user;
          IMpgm  = Q1pgm;
          dtsysDate = %Date();
          IMdt   = %Dec(%Char(dtSysDate:*cymd0):7:0);
          IMtime = %Dec(%Time():*HMS);
          IMiUsr = IMuser;
          IMiPgm = IMPgm;
          IMiDt  = IMdt;
          IMiTim = IMtime;
          Write pim01;
       Endsr;
      *=============================================================
      * SRFND
      *=============================================================
     C     SRFND         BEGSR

     C                   CLEAR                   SZ
     C                   CLEAR                   ED
     C                   CLEAR                   CD
     C                   CLEAR                   ARA
     C                   CLEAR                   ARY
     C                   CLEAR                   ARU
     C                   CLEAR                   ARM
     C                   CLEAR                   EMN
     C                   CLEAR                   ATT
     C                   MOVE      *ALL'0'       *IN
     C                   CLEAR                   EXDDRM
     C                   EXSR      CLARR
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     FNKEY         CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     EXAHDR        IFEQ      *BLANKS
     C                   MOVEL(P)  'Agent'       EXAHDR
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL(P)  'Agent'       EXAHDR
     C                   ENDIF
     C*
     C* OPEN PRINTER FILE BASED ON CLIENT ID OR OUTPUT TYPE.
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '071'
     C     EXBTYP        WHENEQ    'Y'
     C                   OTHER
     C                   ENDSL
      *
     C                   MOVE      *OFF          *IN66
     C     FNCGFL        IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   ENDIF
     C                   MOVE      *OFF          *IN35
     C     EXPODU        IFEQ      'D'
     C                   MOVE      *ON           *IN35
     C                   ENDIF
     C                   MOVE      '-'           DASH
     C*
     C*  LOAD PRINT FIELD FOR REMITTANCE CITY/ST/ZIP
     C*
     C                   MOVEL     FNNAME        XNNAME           40
     C     FNMAD2        COMP      *BLANKS                                31
     C                   MOVE      FNMST         XXST
     C                   Z-ADD     FNMZIP        XXZIP
     C                   MOVEL     FNMCTY        XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     **
      *  Check for Lock box address in Group Auxiliary file
      *  If exists use lock box address instead of Accounting Ofc addr
     C     FNKEY         CHAIN     PF201                              60
     C                   MOVE      ' '           XXLKBX            1
      * To re-activate the LOCK BOX address, Remove comments to active
      * the XXLKBX flag and use LOCK BOX address if it exists.
      *
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        FNCSZ            40
     C*
     C     QQCOM1        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     QQCOM1        XXKEY
     C     TBKEY         CHAIN     PBC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXCOM1
     C                   MOVEL     BCDESC        XXCOM1
     C                   END
     C                   END
     C     QQCOM2        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     QQCOM2        XXKEY
     C     TBKEY         CHAIN     PBC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     BCDESC        XXCOM2
     C                   END
     C                   END
     C     QQCOM3        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     QQCOM3        XXKEY
     C     TBKEY         CHAIN     PBC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXCOM3
     C                   MOVEL     BCDESC        XXCOM3
     C                   END
     C                   END
     C     QQCOM4        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     QQCOM4        XXKEY
     C     TBKEY         CHAIN     PBC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXCOM4
     C                   MOVEL     BCDESC        XXCOM4
     C                   END
     C                   END
     C*
     C* TOTAL ALL AMOUNT FIELDS FOR SUMMARY INVOICE FOR PEO POLICIES.
     C*
     C                   Z-ADD     0             SVAMT            11 2
     C                   Z-ADD     0             SVBILL           11 2
     C                   Z-ADD     0             SVPAID           11 2
     C                   Z-ADD     0             SVPTAX           11 2
     C                   Z-ADD     0             SVBTAX           11 2
     C                   Z-ADD     0             SVCTAX           11 2
     C*
     C*  READ ALL DEDUCTIBLE DETAIL RECORDS
     C*
     C                   SELECT
     C     PRCDIV        WHENEQ    '1'
     C     DDKEY#        CHAIN     PDD01                              50
     C*
     C     PRCEMP        WHENEQ    '1'
     C     DDKEY@        CHAIN     PDD01                              50
     C*
     C                   OTHER
     C     DDKEY         CHAIN     PDD01                              50
     C*
     C                   ENDSL
     C*
     C                   Z-ADD     *ZEROS        XXAMT            11 2
     C                   Z-ADD     *ZEROS        XXBILL           11 2
     C                   Z-ADD     DDEMP#        SVEMP#
     C                   Z-ADD     DDDIV         SVDIV
     C                   Z-ADD     0             SVFYR             3 0
     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     70            IX                3 0
     C                   CLEAR                   ARA
     C                   CLEAR                   ARY
     C                   CLEAR                   ARU
     C                   CLEAR                   ARM
     C                   MOVEA     '00000000'    *IN(71)
     C                   MOVEA     '0000'        *IN(79)
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C     DDEMP#        IFEQ      SVEMP#
     C     DDDIV         ANDNE     SVDIV
     C                   MOVE      *ON           SUMD
     C                   Z-ADD     DDDIV         HIDIV
     C                   ENDIF
     C*
     C     ELKY3         CHAIN     PEL03                              51
     C*  Determine if policy fits deductible plan selected
     C                   MOVEL(P)  ELBSCD        XXKEY
     C                   CLEAR                   BSLPGM
     C     TBKEY         CHAIN     PBS01                              60
     C                   SELECT
     C     BSLPGM        WHENEQ    'L'
     C     USELRG        ANDEQ     *OFF
     C                   MOVE      *ON           *IN51
     C     BSLPGM        WHENNE    'L'
     C     USESML        ANDEQ     *OFF
     C                   MOVE      *ON           *IN51
     C                   ENDSL
     C     *IN51         IFEQ      *OFF
     C*
     C     DDEMP#        IFNE      SVEMP#
     C     DDDIV         ORNE      SVDIV
     C*
     C     X             IFGT      12
     C                   MOVE      '1'           XX                1
     C                   MOVE      X             @Z                3 0
     C                   ELSE
     C                   MOVE      '0'           XX                1
     C                   ENDIF
     C                   EXSR      SREBRK
     C*
     C                   Z-ADD     0             SVFYR             3 0
     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     70            IX                3 0
     C                   CLEAR                   ARA
     C                   CLEAR                   ARY
     C                   CLEAR                   ARU
     C                   CLEAR                   ARM
     C                   MOVEA     '00000000'    *IN(71)
     C                   MOVEA     '0000'        *IN(79)
     C*
     C                   END
     C*
     C     DDEMP#        IFNE      SVEMP#
     C                   EXSR      CLARR
     C                   END
     C*
     C* Retrieve invoice #
     C     *LIKE         DEFINE    DDINV#        SVINV#
     C     *LIKE         DEFINE    DDPODT        SVPODT
     C     DDPODT        IFGT      SVPODT
     C     DDPODT        ANDLE     X2PODT
     C                   Z-ADD     DDINV#        SVINV#
     C                   Z-ADD     DDPODT        SVPODT
     C                   ENDIF
     C*
     C*  CALCULATE CURRENT DEDUCTIBLE AMOUNT AS WELL AS PAST
     C*  DEDUCTIBLE BILLED AMOUNT
     C*
     C     DDPODT        IFEQ      X2PODT
     C                   ADD       DDAMTB        XXAMT
     C     DDFYR         IFNE      SVFYR
     C                   Z-ADD     DDFYR         SVFYR
     C                   ADD       1             X
     C     X             IFLT      12
     C                   ADD       1             IX
     C                   MOVE      *ON           *IN(IX)
     C                   Z-ADD     DDFYR         ARY(X)
     C                   Z-ADD     DDFYR         ARU(X)
     C                   ENDIF
     C     X             IFEQ      12
     C                   Z-ADD     DDFYR         ARY12
     C                   ADD       1             IX
     C                   MOVE      *ON           *IN(IX)
     C                   Z-ADD     DDFYR         ARU(X)
     C                   ENDIF
     C     X             IFGT      12
     C                   Z-ADD     DDFYR         ARU(X)
     C                   ENDIF
     C                   ENDIF
     C*
     C     X             IFGT      12
     C                   ADD       DDAMTB        ARM(X)
     C                   ELSE
     C                   ADD       DDAMTB        ARA(X)
     C                   ADD       DDAMTB        ARM(X)
     C                   ENDIF
     C                   ELSE
     C     DDPODT        IFLT      X2PODT
     C                   ADD       DDAMTB        XXBILL
     C                   END
     C                   END
     C*
     C                   END
     C                   SELECT
     C     PRCDIV        WHENEQ    '1'
     C     DDKEY#        READE     PDD01                                  50
     C*
     C     PRCEMP        WHENEQ    '1'
     C     DDKEY@        READE     PDD01                                  50
     C*
     C                   OTHER
     C     DDKEY         READE     PDD01                                  50
     C*
     C                   ENDSL
     C                   END
     C*
     C                   EXSR      SREBRK
     C     FNPEOF        IFNE      *BLANK
     C                   EXSR      SUMINV
     C                   ENDIF
      *
     C*
     C* CLOSE PRINTER FILE BASED ON CLIENT ID OR OUTPUT TYPE.
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '071'
     C     EXBTYP        WHENEQ    'Y'
     C                   OTHER
     C                   ENDSL
     C                   ENDSR
      *
      *
      *
      *
     C*===============================================================
     C*   SUMDED - Print Summary Deductible Invoice
     C*===============================================================
     C     SUMDED        BEGSR
     C*
     C*
     C                   if        not %open (W7325O1)
      //*** Beg Add ***** 01/17/25 ***************************************
     C                             and isUseSpoolOut = *On
      //*** End Add ***** 01/17/25 ***************************************
     C                   OPEN      W7325O1
     C                   endif
        //*** Beg Add ***** 01/17/25 ***************************************
         If isUseXML;
            Open WxSumInv1;
         Endif;
        //*** End Add ***** 01/17/25 ***************************************
     C                   MOVEL(P)  SUMHDR        X1HDR
      *
     C                   CLEAR                   ATT
     C     XKCONL        IFNE      *ZEROS
     C                   Z-ADD     XKCONL        A
     C                   MOVEL(P)  'Attn:'       ATT(A)
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXPRDT        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         XKDBID
     C                   MOVE      KHDBIL        XKDBIH
     C*
     C                   MOVE      KHPOLN        XKPOLH
     C                   MOVEL(P)  XAFND         XKPOLN
     C     XKPOLN        CAT       XAEMP:0       XKPOLN
     C     XKPOLN        CAT       XAFYR:0       XKPOLN
     C*
     C                   MOVE      KHDIV         XKDIVH
     C*
     C                   MOVEL     KHCAND        XKCANH
     C                   MOVEL(P)  XXCNDT        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         XKCAND
     C*
     C                   MOVEL     KHLOSA        XKLOSH
     C                   MOVEL(P)  X1LSDT        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         XKLOSD
     C*
     C                   Z-ADD     XTPBTT        XXPBTT
     C                   XFOOT     KAMD          XXTOTL
     C                   ADD       XXPBTT        XXTOTL
     C*** Begin Add ***  03/21/25  *********************************************
     C                   ADD       XXCOLSUM      XXTOTL
     C* Print Collateral Transfer Line
     C                   IF        XXCOLSUM <> 0
     C                   EVAL      *IN49 = *On
     C                   ENDIF
     C*** End   Add ***  03/21/25  *********************************************
     C                   MOVEL(P)  XXTOTL        XXNUM
     C                   MOVEL(P)  'J'           EDTMSK
     C                   Z-ADD     2             ##DEC
     C                   MOVE      *ON           ##THOU
     C                   CALL      CHGNUM        CHGPL2
     C     ' '           CHECK     XXNUM         N                 3 0
     C     N             IFLE      80
     C                   SUBST(P)  XXNUM:N       XXNUM
     C                   ENDIF
     C     '$'           CAT       XXNUM:0       XKAMDU
     C*
     C                   MOVEL     KHADUE        XKAMDH
     C                   MOVEL     KHDUED        XKDUEH
     C                   MOVEL(P)  XXDUED        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         XKDUED
     C*
     C                   MOVEL(P)  KHDUOR        XKDORH
     C*
     C                   MOVEL(P)  'Remit'       XKREMH
     C                   CAT       'to:':1       XKREMH
     C* - - - - - - - - - - - - - - - - - - - - - - - - -
     C                   Z-ADD     1             LPG               5 0
     C                   Z-ADD     0             LP#               5 0
     C*
      /free
       //*** Beg Add ***** 01/17/25 ***************************************
       If isUseSpoolOut;
       //*** End Add ***** 01/17/25 ***************************************
          Write kHead;
          Write kHead1;
          Write kLine1;
          Write kHead2;
          Write kHead3;
       Endif;
      /end-free
       //*** Beg Add ***** 01/17/25 *******************************
       If isUseXml;
          XXSUMM = '1';
          Exsr ScriptXml;
       Endif;
       //*** End Add ***** 01/17/25 *******************************
     C* - - - - - - - - - - - - - - - - - - - - - - - - -/
     C*   Print Body Detail Section
     C                   Z-ADD     0             XXAMT            11 2
     C                   Z-ADD     3.4           KLIN
     C*
     C                   DO        LDV#          S
     C                   MOVEL     KDBN(S)       XKDBNM
     C     KAMD(S)       IFNE      *ZEROS
     C                   Z-ADD     KDIV(S)       XKDIV#
     C                   MOVEL     KDBN(S)       XKDBNM
     C*    Amount
     C                   ADD       KAMD(S)       XXAMT
     C*
     C                   MOVEL(P)  KAMD(S)       XXNUM
     C                   MOVEL(P)  'J'           EDTMSK
     C                   Z-ADD     2             ##DEC
     C                   MOVE      *ON           ##THOU
     C                   CALL      CHGNUM        CHGPL2
     C     ' '           CHECK     XXNUM         N                 3 0
     C     N             IFLE      80
     C                   SUBST(P)  XXNUM:N       XXNUM
     C                   ENDIF
     C     '$'           CAT       XXNUM:0       XKAMTD
     C*
       //*** Beg Add ***** 01/17/25 ***************************************
       If isUseSpoolOut;
          Write kBody;
       Endif;
       If isUseXml;
          WrtSumInv1XML();
       Endif;
       //*** End Add ***** 01/17/25 ***************************************
     C                   ADD       .15           KLIN
     C                   ADD       1             LP#
     C                   ENDIF
     C*
     C*    New page
     C*
       If lp# = 18
       and ldv# > 0
       //*** Beg Add ***** 01/17/25 ***************************************
       and isUseSpoolOut = *on;
       //*** End Add ***** 01/17/25 ***************************************
          Write kfoot;
          Write kbot;
          Write kendpg;
          Write khead;
          Write khead1;
          Write kline1;
          Write khead2;
          Write khead3;
          lpg += 1;
          lp#  = 0;
          kLin = 3.4;
       Endif;
     C                   ENDDO

       //*** Beg Add ***** 01/17/25 ***************************************
       If isUseXml;
          Close WxSumInv1;
       Endif;

       If isUseSpoolOut;
          Write kFoot;
          Write kBot;
       Endif;

       If isUseXml;
          WrtSumInvXML();
          WrtDedCrtXML();                //Write Deductible Criteria file
       Endif;

       If isUseSpoolOut;
          Close W7325O1;
       Endif;
      //*** End Add ***** 01/17/25 ***************************************

     C                   ENDSR
     C*===============================================================
     C* LODSD - Load Arrays with data for Summary Deductible Report
     C*===============================================================
     C     LODSD         BEGSR
     C*   Move data to "K" arrays for report body
     C                   ADD       1             S                 5 0
     C                   ADD       1             LDV#              5 0
     C*
     C                   MOVEL     SVDIV         KDIV(S)
     C                   MOVEL     EMNAME        KDBN(S)
     C                   ADD       XXCTOT        KAMD(S)
     C                   Z-ADD     DDFYR         KYR(S)
     C                   ADD       XXPBTT        XTPBTT
     C*** Begin Add ***  03/21/25  *********************************************
     C                   ADD       XXCOLT        XXCOLSUM
     C*** End   Add ***  03/21/25  *********************************************
     C*
     C                   ENDSR
     C*===============================================================
     C* CLARR - Clear Arrays with Name
     C*===============================================================
     C     CLARR         BEGSR
     C*
     C*   Clear the arrays.
     C                   MOVEA     *BLANKS       KDBN
     C                   Z-ADD     0             KAMD
     C                   Z-ADD     0             KYR
     C                   Z-ADD     0             KYAM
     C                   Z-ADD     0             KDIV
     C                   Z-ADD     0             XTPBTT           11 2
     C*
     C                   Z-ADD     0             LN#               5 0
     C                   Z-ADD     0             LP#               5 0
     C                   Z-ADD     0             S
     C                   Z-ADD     0             LDV#
     C                   MOVE      *OFF          SUMD
     C*** Begin Add ***  03/21/25  *********************************************
     C                   Z-ADD     0             XXCOLSUM
     C                   MOVE      *OFF          *IN49                          Print Collat line
     C*** End   Add ***  03/21/25  *********************************************
     C*
     C                   ENDSR
     C*===============================================================
     C* PRTYR - Populate Arrays with Name
     C*===============================================================
     C     PRTYR         BEGSR
     C                   ENDSR
     C*===============================================================
     C* GETSTR  Get State Reporting Name and Address
     C*===============================================================
     C*     Get STRP Div N&A data ==> into "K" arrays
     C     GETSTR        BEGSR
     C*
     C     EMKEY2        CHAIN     PEM01                              60
     C     *IN60         DOWEQ     '0'
     C     EMSTRP        IFEQ      'Y'
     C                   MOVE      'W'           ##ODR             1
     C                   MOVE      ' '           ##NODR            1
     C                   MOVE      ' '           ##AODR            1
     C                   MOVE      *BLANKS       ##EMN1           50
     C                   MOVE      *BLANKS       ##EMN2           50
     C                   MOVE      *BLANKS       ##EMN3           50
     C                   MOVE      *BLANKS       ##EMN4           50
     C                   MOVE      *BLANKS       ##EMN5           50
     C                   MOVE      *BLANKS       ##EMN6           50
     C                   MOVE      *BLANKS       ##EMN7           50
     C                   Z-ADD     EMDIV         SRDIV             3 0
     C*
     C                   MOVE      EMEMP#        ##EMP#
     C                   MOVE      EMDIV         ##DIV
     C                   CALL      'SRADDR'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ODR
     C                   PARM                    ##NODR
     C                   PARM                    ##AODR
     C                   PARM                    ##EMN1
     C                   PARM                    ##EMN2
     C                   PARM                    ##EMN3
     C                   PARM                    ##EMN4
     C                   PARM                    ##EMN5
     C                   PARM                    ##EMN6
     C                   PARM                    ##EMN7
     C                   PARM                    ##ADRL            3 0
     C                   PARM                    ##NAML            3 0
     C                   PARM                    ##FEDL            3 0
     C                   PARM      *BLANKS       ##PEO1           57
     C                   PARM      *BLANKS       ##PEO2           57
     C                   PARM      *BLANK        ##PGFL            1
     C                   PARM      *ZEROS        ##CONL            3 0
      *
     C                   CLEAR                   XKCONL
     C     ##CONL        IFNE      *ZERO
     C     ##CONL        ADD       1             XKCONL            3 0
     C                   ENDIF
     C*
     C                   MOVEA     *BLANKS       EMN
     C                   MOVEA     *BLANKS       ATT
     C*
     C                   MOVE      ##EMN1        XXEMN2           50
     C                   MOVE      ##EMN2        XXEMN3           50
     C                   MOVE      ##EMN3        XXEMN4           50
     C                   MOVEL     ##EMN4        XXEMN5           50
     C                   MOVEL     ##EMN5        XXEMN6           50
     C                   MOVEL     ##EMN6        XXEMN7           50
     C                   MOVEL     ##EMN7        XXEMN8           50
     C*
     C                   CLEAR                   KMN
     C                   MOVEA     EMN           KMN
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C     EMKEY2        READE     PEM01                                  60
     C                   ENDDO
     C*
     C                   ENDSR
     C*===============================================================
     C*
      *===============================================================
      * SRCONT - Get Employer Contact Name
      *===============================================================
      *
     C     SRCONT        BEGSR
     C                   CLEAR                   XXCNT
      *
      * Get Contact Name
      *
     C     ECKY          CHAIN     PEC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ECFSN         IFNE      *BLANKS
     C     ECFSN         CAT       ECLSN:1       XXCNT            50
      *
     C                   ELSE
      *
     C     ECTITL        CAT       ECLSN:1       XXCNT
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
      *
      * =================================================================
     C* SREBRK SUBROUTINE              ** SUBROUTINE **   S R E B R K *
      * =================================================================
     C     SREBRK        BEGSR
      *
     C*  FIRST: Get STRP Division N&A data ==> to "K" arrays
     C                   EXSR      GETSTR
     C*
     C                   Z-ADD     1             CNT               3 0
     C                   Z-ADD     71            IX                3 0
     C*
     C* Verify if year to print has due balance > 0.
     C     CNT           DOWLE     12
     C     *IN(IX)       IFEQ      *ON
     C     ARA(CNT)      ANDEQ     0
     C                   MOVE      *OFF          *IN(IX)
     C                   ENDIF
     C                   ADD       1             IX
     C                   ADD       1             CNT
     C                   ENDDO
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXCHEC           40
     C*
     C     OFKY          CHAIN     POF01                              62
     C     *IN62         IFEQ      *OFF
     C*
     C*  LOAD PRINT FIELD FOR REMITTANCE CITY/ST/ZIP
     C*
     C                   MOVEL     OFCKNM        XXCHEC
     C   66              MOVEL(P)  OFACNM        XNNAME           40
     C  N66              MOVEL(P)  FNNAME        XNNAME
     C     OFAAD2        COMP      *BLANKS                                31
     C                   MOVE      OFAST         XXST
     C                   MOVE      OFAZIP        XXZIP
     C                   MOVEL     OFACTY        XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XACSZ            40
     C                   MOVEL(P)  OFACNM        LBACNM
     C                   MOVEL(P)  OFAAD1        LBAAD1
     C                   MOVEL(P)  OFAAD2        LBAAD2
     C                   MOVEL     XACSZ         LBACSZ
     C     LBAAD2        IFEQ      *BLANKS
     C                   MOVEL(P)  LBACSZ        LBAAD2
     C                   CLEAR                   LBACSZ
     C                   ENDIF
      *  If EXISTS use LOCK BOX address instead of Accounting Ofc addr
      *  by Moving LOCK BOX address fields into accunting Ofc addr flds
     C     XXLKBX        IFEQ      'Y'
     C     F2LBNM        IFNE      *BLANKS
     C                   MOVEL(P)  F2LBNM        LBACNM           40
     C                   ENDIF
     C                   MOVEL(P)  F2LAD1        LBAAD1           40
     C                   MOVEL(P)  F2LAD2        LBAAD2           40
     C     F2LCTY        CAT(P)    ',':0         LBACSZ           40
     C     LBACSZ        CAT       F2LST:1       LBACSZ
     C                   MOVE      F2LZIP        XXZIP
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVE      XXZPM2        XXZP2
     C                   CAT       XXZP1:1       LBACSZ
     C     XXZP2         IFNE      *ZEROS
     C                   CAT       '-':0         LBACSZ
     C                   CAT       XXZP2:0       LBACSZ
     C                   ENDIF
     C     F2LAD2        IFEQ      *BLANK
     C                   MOVE      *ON           *IN31
     C                   ELSE
     C                   MOVE      *OFF          *IN31
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     EMKEY         CHAIN     PA301                              61
     C     *IN61         IFEQ      *OFF
     C     A3STAT        ANDEQ     'A'
     C     A3ADR1        ANDNE     *BLANKS
     C     A3CONT        COMP      *BLANKS                                28
     C     A3ADR2        COMP      *BLANKS                                30
     C                   MOVEL     A3ADR1        XXADR1
     C                   MOVEL     A3ADR2        XXADR2
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      A3ST          XXST
     C                   MOVEL     A3ZIP1        XXZIP
     C                   MOVE      A3ZIP2        XXZIP
     C                   MOVEL     A3CTY         XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C                   MOVEL     XXCSZ         XXECSZ
     C                   MOVEL(P)  A3CONT        XXCONT           40
     C*
     C                   ELSE
     C*
      *
      * Get Employer Contact Name
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         EMCONT
      *
     C                   MOVEL(P)  EMCONT        XXCONT           40
     C     EMCONT        COMP      *BLANKS                                28
     C     EMMPFL        IFEQ      'M'
     C     EMMAD2        COMP      *BLANKS                                30
     C                   MOVEL     EMMAD1        XXADR1
     C                   MOVEL     EMMAD2        XXADR2
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      EMMST         XXST
     C                   Z-ADD     EMMZIP        XXZIP
     C                   MOVEL     EMMCTY        XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C                   MOVEL     XXCSZ         XXECSZ
     C                   ELSE
     C*
     C     EMPAD2        COMP      *BLANKS                                30
     C                   MOVEL     EMPAD1        XXADR1
     C                   MOVEL     EMPAD2        XXADR2
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      EMPST         XXST
     C                   Z-ADD     EMPZIP        XXZIP
     C                   MOVEL     EMPCTY        XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C                   MOVEL     XXCSZ         XXECSZ
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *BLANKS       XXADR1
     C                   MOVE      *BLANKS       XXADR2
     C                   MOVE      *BLANKS       XXECSZ
     C                   END
     C                   MOVE      XXCO#         ##CO#             3
     C                   MOVE      XXFND         ##FND             3
     C                   MOVE      SVEMP#        ##EMP#            9
     C                   MOVE      SVDIV         ##DIV             5
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C                   MOVE      ' '           ##ODR
     C                   OTHER
     C                   MOVE      'W'           ##ODR             1
     C                   ENDSL
     C                   MOVE      ' '           ##NODR            1
     C                   MOVE      ' '           ##AODR            1
     C                   MOVE      *BLANKS       ##EMN1           50
     C                   MOVE      *BLANKS       ##EMN2           50
     C                   MOVE      *BLANKS       ##EMN3           50
     C                   MOVE      *BLANKS       ##EMN4           50
     C                   MOVE      *BLANKS       ##EMN5           50
     C                   MOVE      *BLANKS       ##EMN6           50
     C                   MOVE      *BLANKS       ##EMN7           50
     C                   CALL      'SRADDR'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ODR
     C                   PARM                    ##NODR
     C                   PARM                    ##AODR
     C                   PARM                    ##EMN1
     C                   PARM                    ##EMN2
     C                   PARM                    ##EMN3
     C                   PARM                    ##EMN4
     C                   PARM                    ##EMN5
     C                   PARM                    ##EMN6
     C                   PARM                    ##EMN7
     C                   PARM                    ##ADRL            3 0
     C                   PARM                    ##NAML            3 0
     C                   PARM                    ##FEDL            3 0
     C                   PARM      *BLANKS       ##PEO1           57
     C                   PARM      *BLANKS       ##PEO2           57
     C                   PARM      *BLANK        ##PGFL            1
     C                   PARM      *ZEROS        ##CONL            3 0
     C*
     C                   MOVEA     *BLANKS       EMN
     C                   MOVEA     *BLANKS       ATT
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C                   Z-ADD     1             A
     C                   OTHER
     C                   Z-ADD     8             A                 2 0
     C                   ENDSL
     C*
     C                   MOVE      ##EMN1        XXEMN2           50
     C                   MOVE      ##EMN2        XXEMN3           50
     C                   MOVE      ##EMN3        XXEMN4           50
     C                   MOVEL     ##EMN4        XXEMN5           50
     C                   MOVEL     ##EMN5        XXEMN6           50
     C                   MOVEL     ##EMN6        XXEMN7           50
     C                   MOVEL     ##EMN7        XXEMN8           50
     C*
     C*
      *
     C     ##CONL        IFNE      *ZERO
     C     ##CONL        ADD       1             A
     C                   MOVEL     'ATTN:'       ATT(A)
     C                   ENDIF
     C*
     C*
     C*  CHECK FOR EMPLOYER CANCELLATION DATE
     C*
     C                   Z-ADD     *ZERO         XXCNDT
     C                   MOVE      *OFF          *IN32
     C*
     C                   Z-ADD     1             Y                 1 0
     C     *ZERO         LOOKUP    ED(Y)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C     Y             DOWNE     0
     C     X2PODT        COMP      ED(Y)                              60  60
     C  N60              SUB       1             Y
     C  N60              END
     C*
     C     Y             IFNE      *ZERO
     C     CD(Y)         ANDNE     *ZERO
     C     CD(Y)         ANDLT     X2PODT
     C*
     C                   Z-ADD     CD(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCNDT            8 0
     C*
     C                   MOVE      *ON           *IN32
     C                   END
     C*
     C*  DETERMINE THE BILLING FUND YEAR FOR THE EMPLOYER
     C*
     C     ELKY1         SETGT     PEL01
     C*
     C     *IN60         DOUEQ     *ON
     C     ELKY2         READPE    PEL01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     ELPFDT        IFNE      ELPTDT
     C                   Z-ADD     ELFYR         XXBFYR            3 0
     C                   MOVE      XXBFYR        XXFYR2            2 0
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     F1KEY         CHAIN     PF101                              60
      *
      * CHECK FOR A CARRIER/STATE REPORTING RECORD
      *
     C                   MOVE      *OFF          *IN33
     C     RPKY          CHAIN     PRP01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     RPCAP#        IFNE      *BLANKS
     C                   MOVE      *ON           *IN33
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C*  CALCULATE PRIOR DEDUCTIBLE BILLED AND PRIOR DEDUCTIBLE PAID
     C*
     C                   Z-ADD     *ZEROS        XXPAID           11 2
     C                   Z-ADD     *ZEROS        XXPTAX            9 2
     C                   Z-ADD     *ZEROS        XXBTAX            9 2
     C                   Z-ADD     *ZEROS        XXCTAX            9 2
     C                   Z-ADD     *ZEROS        XXSVYR            3 0
     C*** Begin Add ***  06/24/24  *********************************************
     C                   Z-ADD     *ZERO         XXCOL             9 2          Collateral Transfer
     C                   Z-ADD     *ZERO         XXCOLT            9 2          Collateral Transfer
     C                   MOVE      *OFF          *IN48                          Print Collat line
     C*** End   Add ***  06/24/24  *********************************************
     C*
     C                   MOVE      *ZEROS        XXFYR
     C                   MOVE      '*'           XXPLAN
     C                   CALL      'WA002R'      AGTPL
     C*
     C* Process for current account if XXPLAN = 'A'
     C     XXPLAN        IFEQ      'A'
     C                   EXSR      SRCACT
     C                   ELSE
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     EAFYR         IFNE      XXSVYR
     C                   Z-ADD     EAFYR         XXBFYR
     C     F1KEY         CHAIN     PF101                              60
     C   60              MOVE      *BLANKS       F1DTXB
     C                   ENDIF
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'R'
     C     EAARTY        ANDEQ     'Q'
     C     EAARCD        ANDNE     F1DTXP
     C                   SUB       EAAMT         XXPAID
     C                   ENDIF
     C*
     C* ACCUMULATE PRIOR PAID TAX RECORDS
     C*
     C     EAARCD        IFEQ      F1DTXP
     C                   SUB       EAAMT         XXPTAX
     C                   ENDIF
     C*
     C* ACCUMULATE PRIOR BILLED TAX RECORDS
     C*
     C     EAARCD        IFEQ      F1DTXB
     C     EAPODT        ANDLT     X2PODT
     C                   ADD       EAAMT         XXBTAX
     C                   ENDIF
     C*
     C* ACCUMULATE CURRENT BILLED TAX RECORDS
     C*
     C     EAARCD        IFEQ      F1DTXB
     C     EAPODT        ANDEQ     X2PODT
     C                   ADD       EAAMT         XXCTAX
     C                   ENDIF
     C*** Begin Add ***  06/24/24  *********************************************
     C*
     C* Accumulate Collateral Transfer from WMEAP file
     C*
     C                   IF        ELcolad = 'Y'
     C*** Begin Chg ***  03/21/25  *********************************************
     C*****EAARCD        IFEQ      EXCOLT
     C     EAARCD        IFEQ      EXCOLD
     C     EAPODT        ANDEQ     X2PODT
     C*****              ADD       EAAMT         XXCOL
     C*****              ADD       EAAMT         XXCOLT
     C                   SUB       EAAMT         XXCOL
     C                   SUB       EAAMT         XXCOLT
     C*** End   Chg ***  03/21/25  *********************************************
     C                   ENDIF
     C                   ENDIF
     C*** End   Add ***  06/24/24  *********************************************
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     EAFYR         XXSVYR            3 0
     C     EAKEY         READE     PEA01                                  60
     C                   END
     C*
     C                   ENDIF
     C*
     C*
     C* ADD CURRENT TAX BILLED RECORDS FROM BATCH DETAIL TO ACCUMULATED CURRENT
     C* TAX BILLED AMOUNT
     C*
     C     EAKEY         CHAIN     PEA05                              60
     C                   Z-ADD     *ZEROS        XXSVYR            3 0
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     $AFYR         IFNE      XXSVYR
     C                   Z-ADD     $AFYR         XXBFYR
     C     F1KEY         CHAIN     PF101                              60
     C   60              MOVE      *BLANKS       F1DTXB
     C                   ENDIF
     C*
     C     $AARCD        IFEQ      F1DTXB
     C     $APODT        ANDEQ     X2PODT
     C     EATFDT        ANDEQ     0
     C                   ADD       $AAMT         XXCTAX
     C                   ENDIF
     C*** Begin Add ***  06/24/24  *********************************************
     C*
     C* Accumulate Collateral Transfer from Pending A/R Batches in WDEAP file
     C*
     C                   IF        ELcolad = 'Y'
     C*** Begin Chg ***  03/21/25  *********************************************
     C*****$AARCD        IFEQ      EXCOLT
     C     $AARCD        IFEQ      EXCOLD
     C     $APODT        ANDEQ     X2PODT
     C     EATFDT        ANDEQ     0
     C*****              ADD       $AAMT         XXCOL
     C*****              ADD       $AAMT         XXCOLT
     C                   SUB       $AAMT         XXCOL
     C                   SUB       $AAMT         XXCOLT
     C*** End   Chg ***  03/21/25  *********************************************
     C                   ENDIF
     C                   ENDIF
     C*** End   Add ***  06/24/24  *********************************************
     C*
     C                   Z-ADD     $AFYR         XXSVYR            3 0
     C     EAKEY         READE     PEA05                                  60
     C                   ENDDO
     C*
     C* CALCULATE PRIOR BILLED TOTAL, PRIOR PAID TOTAL, AND CURRENT BILLED TOTAL
     C*
     C     XXBILL        ADD       XXBTAX        XXBTOT           11 2
     C     XXPAID        ADD       XXPTAX        XXPTOT           11 2
     C     XXAMT         ADD       XXCTAX        XXCTOT           11 2
     C     XXBILL        ADD       XXPAID        XPRBAL           11 2
     C     XXBTAX        ADD       XXPTAX        XPRTAX            9 2
     C     XXBTOT        ADD       XXPTOT        XPRTOT           11 2
     C*
     C* CALCULATE PRIOR BALANCE Amounts
     C     XXBILL        ADD       XXPAID        XXPBAL           11 2
     C     XXBTAX        ADD       XXPTAX        XXPBTX           11 2
     C     XXBTOT        ADD       XXPTOT        XXPBTT           11 2
     C*
     C* CALCULATE BALANCE DUE
     C*
     C     XXBTOT        ADD       XXCTOT        XXTOTL           11 2
     C                   ADD       XXPTOT        XXTOTL
     C*** Begin Add ***  06/24/24  *********************************************
     C                   IF        ELcolad = 'Y' and XXCOLT <> 0
     C                   ADD       XXCOLT        XXTOTL
     C                   EVAL      *IN48 = *On
     C                   ENDIF
     C*** End   Add ***  06/24/24  *********************************************
     C*
     C*** Begin Chg ***  06/24/24  *********************************************
     C***  XXCLT#        IFNE      '071'
     C***  XXTOTL        ANDNE     *ZEROS
     C                   IF        XXCLT# <> '071' and
     C                             (XXTOTL <> 0 or XXCOLT <> 0)
     C*** End   Chg ***  06/24/24  *********************************************
     C                   EXSR      LODSD
     C                   ENDIF
     C*
     C*** Begin Chg ***  06/24/24  *********************************************
     C***  XXTOTL        IFNE      *ZEROS
     C                   IF        (XXTOTL <> 0 or XXCOLT <> 0)
     C*** End   Chg ***  06/24/24  *********************************************
     C*
     C     XXCLT#        IFNE      '071'
     C                   MOVE      *OFF          *IN63
     C                   if        not %open (W7325O)
      //*** Beg Add ***** 01/17/25 ***************************************
     C                             and isUseSpoolOut = *On
      //*** End Add ***** 01/17/25 ***************************************
     C                   OPEN      W7325O
     C                   endif
      //*** Beg Add ***** 01/17/25 ***************************************
       FlgPeoInv = 'N';
      //*** End Add ***** 01/17/25 ***************************************
     C                   EXSR      NEWF
      //*** Beg Chg ***** 01/17/25 ***************************************
     C***                CLOSE     W7325O
        If isUseSpoolOut;
           Close W7325O;
        Endif;
      //*** End Chg ***** 01/17/25 ***************************************
     C     *LOCK         IN        *DTAARA
     C                   MOVEL(P)  'W7325O'      DEDINV
     C                   OUT       *DTAARA
     C                   MOVE      '0'           XXSUMM
     C                   EXSR      DARTS
       // Write to Image Cross Reference File WMIXP
       If IsModEnabled('Invoice Cloud') and IcGroupOpt(EAco#:EAfnd);
          Exsr SrImageXref;
       Endif;
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C*  New employer, print deductible summary if flag on unless
     C*  the employer is opted in to Invoice Cloud
     C     DDEMP#        IFNE      SVEMP#
     C     *IN50         OREQ      *ON
      *** Beg Chg ***** 01/17/25 *********************************
       //***If SumD = *on and (ICPolicyOpt(EAco#:EAfnd:EAemp#) <> 'Y')
       //*** and S >= 1 and lDv# >= 1;
       If SumD = *On and S >= 1 and lDv# >= 1;
      *** End Chg ***** 01/17/25 *********************************
     C                   EXSR      SUMDED
     C     *LOCK         IN        *DTAARA
     C                   MOVEL(P)  'W7325O1'     DEDINV
     C                   OUT       *DTAARA
     C                   MOVE      '1'           XXSUMM            1
     C                   EXSR      DARTS
     C                   MOVE      *OFF          SUMD
     C                   ENDIF
     C                   EXSR      CLARR
     C                   Z-ADD     0             LODIV
     C                   Z-ADD     0             HIDIV
     C                   ENDIF
     C*
     C*** Begin Chg ***  06/24/24  *********************************************
     C***  XXTOTL        IFNE      *ZEROS
     C                   IF        (XXTOTL <> 0 or XXCOLT <> 0)
     C*** End   Chg ***  06/24/24  *********************************************
     C     XXCLT#        IFEQ      '071'
     C                   MOVEL(P)  EMNAME        XXILN1
     C                   MOVEL(P)  XXADR1        XXILN2
     C                   MOVEL(P)  XXADR2        XXILN3
     C                   MOVEL(P)  XXECSZ        XXILN4
     C*
     C                   EXSR      FILLI
     C*
     C                   MOVEL(P)  LBACNM        XXRLN1
     C                   MOVEL(P)  LBAAD1        XXRLN2
     C                   MOVEL(P)  LBAAD2        XXRLN3
     C                   MOVEL(P)  LBACSZ        XXRLN4
     C*
     C                   EXSR      FILLR
     C*
     C                   Z-ADD     XXFND         @XFND
     C                   Z-ADD     SVEMP#        @VEMP#
     C                   Z-ADD     SVDIV         @VDIV
     C                   Z-ADD     XXBFYR        @XBFYR
     C                   Z-ADD     XXTOTL        @XTOTL
     C*
     C                   MOVE      '3'           ##TYPE
     C                   Z-ADD     XXFND         #1FND
     C                   Z-ADD     SVEMP#        #1EMP#
     C                   Z-ADD     SVDIV         #1DIV
     C                   Z-ADD     XXBFYR        ##FYR
     C                   Z-ADD     XXTOTL        XXAMT1
     C*
     C* Check Digit
     C                   MOVEL     XXOCR         XXOCR2           32
     C                   MOVEA     XXOCR2        OCL
     C                   Z-ADD     *ZEROS        XXSUM
     C     1             DO        32            A                 2 0
     C                   MOVE      OCL(A)        XXLN              1 0
     C                   MOVE      WTL(A)        XXRN              1 0
     C     XXLN          MULT      XXRN          XX2N              2 0
     C                   MOVEL     XX2N          XXLN
     C                   MOVE      XX2N          XXRN
     C     XXLN          ADD       XXRN          XXTTL             2 0
     C                   ADD       XXTTL         XXSUM             5 0
     C                   ENDDO
     C     XXSUM         DIV       10            XXWRK             2 0
     C                   MVR                     XXREM             1 0
     C     10            SUB       XXREM         XXCKDG
     C*
     C                   MOVE      '0'           *IN84
     C                   if        not %open (W7325O)
      //*** Beg Add ***** 01/17/25 ***************************************
     C                             and isUseSpoolOut = *On
      //*** End Add ***** 01/17/25 ***************************************
     C                   OPEN      W7325O
     C                   endif
     C                   MOVE      '1'           *IN84
     C                   WRITE     INVOIC1
     C                   ENDIF
     C*
     C     *IN84         IFEQ      '1'
      //*** Beg Chg ***** 01/17/25 ***************************************
     C***                CLOSE     W7325O
        If isUseSpoolOut;
           Close W7325O;
        Endif;
      //*** End Chg ***** 01/17/25 ***************************************
     C                   MOVE      '0'           *IN84
     C     *LOCK         IN        *DTAARA
     C                   MOVEL(P)  'W7325O'      DEDINV
     C                   OUT       *DTAARA
     C                   MOVE      '0'           XXSUMM
     C                   EXSR      DARTS
     C                   ENDIF
     C*
     C                   END
     C*
     C*
     C* SAVE AMOUNT FIELDS FOR SUMMARY INVOICE FOR PEO POLICIES.
     C*
     C                   ADD       XXAMT         SVAMT
     C                   ADD       XXBILL        SVBILL
     C                   ADD       XXPAID        SVPAID
     C                   ADD       XXPTAX        SVPTAX
     C                   ADD       XXBTAX        SVBTAX
     C                   ADD       XXCTAX        SVCTAX
     C                   Z-ADD     DDEMP#        SVEMP#
     C                   Z-ADD     DDDIV         SVDIV
     C                   Z-ADD     *ZEROS        XXAMT
     C                   Z-ADD     *ZEROS        XXBILL
     C                   CLEAR                   SVINV#
     C                   CLEAR                   SVPODT
     C*
     C                   ENDSR
      *
      *
      *
      * =================================================================
     C*
      * =================================================================
     C* SUBROUTINE TO PRINT SUMMARY INVOICE FOR PEO POLICY
      * =================================================================
     C*
     C     SUMINV        BEGSR
     C*
     C                   Z-ADD     1             SVEMP#
     C                   Z-ADD     0             SVDIV
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXCHEC           40
     C*
     C     OFKY          CHAIN     POF01                              62
     C     *IN62         IFEQ      *OFF
     C*
     C*  LOAD PRINT FIELD FOR REMITTANCE CITY/ST/ZIP
     C*
     C                   MOVEL     OFCKNM        XXCHEC
     C                   MOVEL     OFACNM        XNNAME           40
     C     OFAAD2        COMP      *BLANKS                                31
     C                   MOVE      OFAST         XXST
     C                   MOVE      OFAZIP        XXZIP
     C                   MOVEL     OFACTY        XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XACSZ            40
     C                   ENDIF
     C*
     C     EMKEY         CHAIN     PA301                              61
     C     *IN61         IFEQ      *OFF
     C     A3STAT        ANDEQ     'A'
     C     A3ADR1        ANDNE     *BLANKS
     C     A3CONT        COMP      *BLANKS                                28
     C     A3ADR2        COMP      *BLANKS                                30
     C                   MOVEL     A3ADR1        XXADR1
     C                   MOVEL     A3ADR2        XXADR2
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      A3ST          XXST
     C                   MOVEL     A3ZIP1        XXZIP
     C                   MOVE      A3ZIP2        XXZIP
     C                   MOVEL     A3CTY         XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C                   MOVEL     XXCSZ         XXECSZ
     C                   MOVEL(P)  A3CONT        XXCONT           40
     C*
     C                   ELSE
      *
      * Get Employer Contact Name
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         EMCONT
      *
     C                   MOVEL(P)  EMCONT        XXCONT           40
     C     EMCONT        COMP      *BLANKS                                28
     C     EMMPFL        IFEQ      'M'
     C     EMMAD2        COMP      *BLANKS                                30
     C                   MOVEL     EMMAD1        XXADR1
     C                   MOVEL     EMMAD2        XXADR2
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      EMMST         XXST
     C                   Z-ADD     EMMZIP        XXZIP
     C                   MOVEL     EMMCTY        XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C                   MOVEL     XXCSZ         XXECSZ
     C*
     C                   ELSE
     C*
     C     EMPAD2        COMP      *BLANKS                                30
     C                   MOVEL     EMPAD1        XXADR1
     C                   MOVEL     EMPAD2        XXADR2
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      EMPST         XXST
     C                   Z-ADD     EMPZIP        XXZIP
     C                   MOVEL     EMPCTY        XXCTY
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C                   MOVEL     XXCSZ         XXECSZ
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       XXADR1
     C                   MOVE      *BLANKS       XXADR2
     C                   MOVE      *BLANKS       XXECSZ
     C                   END
     C                   MOVE      XXCO#         ##CO#             3
     C                   MOVE      XXFND         ##FND             3
     C                   MOVE      SVEMP#        ##EMP#            9
     C                   MOVE      SVDIV         ##DIV             5
     C                   MOVE      'W'           ##ODR             1
     C                   MOVE      ' '           ##NODR            1
     C                   MOVE      ' '           ##AODR            1
     C                   MOVE      *BLANKS       ##EMN1           50
     C                   MOVE      *BLANKS       ##EMN2           50
     C                   MOVE      *BLANKS       ##EMN3           50
     C                   MOVE      *BLANKS       ##EMN4           50
     C                   MOVE      *BLANKS       ##EMN5           50
     C                   MOVE      *BLANKS       ##EMN6           50
     C                   MOVE      *BLANKS       ##EMN7           50
     C                   CALL      'SRADDR'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ODR
     C                   PARM                    ##NODR
     C                   PARM                    ##AODR
     C                   PARM                    ##EMN1
     C                   PARM                    ##EMN2
     C                   PARM                    ##EMN3
     C                   PARM                    ##EMN4
     C                   PARM                    ##EMN5
     C                   PARM                    ##EMN6
     C                   PARM                    ##EMN7
     C                   PARM                    ##ADRL            3 0
     C                   PARM                    ##NAML            3 0
     C                   PARM                    ##FEDL            3 0
     C                   PARM      *BLANKS       ##PEO1           57
     C                   PARM      *BLANKS       ##PEO2           57
     C                   PARM      *BLANK        ##PGFL            1
     C                   PARM      *ZEROS        ##CONL            3 0
     C*
     C                   MOVEA     *BLANKS       EMN
     C                   MOVEA     *BLANKS       ATT
     C                   Z-ADD     8             A                 2 0
     C*
     C                   MOVE      ##EMN1        XXEMN2           50
     C                   MOVE      ##EMN2        XXEMN3           50
     C                   MOVE      ##EMN3        XXEMN4           50
     C                   MOVEL     ##EMN4        XXEMN5           50
     C                   MOVEL     ##EMN5        XXEMN6           50
     C                   MOVEL     ##EMN6        XXEMN7           50
     C                   MOVEL     ##EMN7        XXEMN8           50
     C*
      *
     C     ##CONL        IFNE      *ZERO
     C     ##CONL        ADD       1             A
     C                   MOVEL     'ATTN:'       ATT(A)
     C                   ENDIF
     C*
     C*
     C*  CHECK FOR EMPLOYER CANCELLATION DATE
     C*
     C                   Z-ADD     *ZERO         XXCNDT
     C                   MOVE      *OFF          *IN32
     C*
     C                   Z-ADD     1             Y                 1 0
     C     *ZERO         LOOKUP    ED(Y)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C     Y             DOWNE     0
     C     X2PODT        COMP      ED(Y)                              60  60
     C  N60              SUB       1             Y
     C  N60              END
     C*
     C     Y             IFNE      *ZERO
     C     CD(Y)         ANDNE     *ZERO
     C     CD(Y)         ANDLT     X2PODT
     C*
     C                   Z-ADD     CD(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCNDT            8 0
     C*
     C                   MOVE      *ON           *IN32
     C                   END
     C*
     C*  DETERMINE THE BILLING FUND YEAR FOR THE EMPLOYER
     C*
     C     ELKY1         SETGT     PEL01
     C*
     C     *IN60         DOUEQ     *ON
     C     ELKY2         READPE    PEL01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     ELPFDT        IFNE      ELPTDT
     C                   Z-ADD     ELFYR         XXBFYR            3 0
     C                   MOVE      XXBFYR        XXFYR2            2 0
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     F1KEY         CHAIN     PF101                              60
      *
      * CHECK FOR A CARRIER/STATE REPORTING RECORD
      *
     C                   MOVE      *OFF          *IN33
     C     RPKY          CHAIN     PRP01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     RPCAP#        IFNE      *BLANKS
     C                   MOVE      *ON           *IN33
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C*  GET AGENT NAME VIA AGENT HISTORY AND AGENT MASTER FILES
     C*
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C     AHKY          READPE    PAH01                                  50
     C                   END
     C*
     C     *IN50         IFEQ      '0'
     C                   MOVEL     AHAGT#        XXAGT#
     C*
     C     AAKY          CHAIN     PAA01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     AAFEIN        XXFEIN
     C                   MOVEL     AAMOD         XXMOD
     C                   ELSE
     C                   MOVE      *ZEROS        XXFEIN
     C                   MOVE      *ZEROS        XXMOD
     C                   END
     C*
     C     AGKY          CHAIN     PAG01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C     AGMAD1        IFNE      *BLANKS
     C                   MOVEL     AGMAD1        XXAAD1           40
     C                   MOVEL     AGMCTY        XXSHRT           19
     C                   MOVEL     XXSHRT        XXLAST           56
     C                   MOVEL     AGMST         XXSTZP           13
     C                   MOVE      AGMZIP        XXSTZP
     C                   ELSE
     C                   MOVEL     AGPAD1        XXAAD1
     C                   MOVEL     AGPCTY        XXSHRT
     C                   MOVEL     XXSHRT        XXLAST
     C                   MOVEL     AGPST         XXSTZP
     C                   MOVE      AGPZIP        XXSTZP
     C                   ENDIF
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C     AGMAD1        IFNE      *BLANKS
     C                   MOVEL     AGMZIP        XXST10           10
     C                   ELSE
     C                   MOVEL     AGPZIP        XXST10           10
     C                   ENDIF
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   ENDIF
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXACSZ           30
     C                   ELSE
     C                   MOVE      *BLANKS       AGNAME
     C                   MOVE      *BLANKS       XXAAD1
     C                   MOVE      *BLANKS       XXACSZ
     C                   MOVE      *ZEROS        AGARE1
     C                   MOVE      *ZEROS        AGPHN1
     C                   END
     C*
     C                   ELSE
     C                   MOVE      *BLANK        AHAGT#
     C                   MOVE      *BLANK        AGNAME
     C                   MOVE      *BLANKS       XXAAD1
     C                   MOVE      *BLANKS       XXACSZ
     C                   MOVE      *ZEROS        AGARE1
     C                   MOVE      *ZEROS        AGPHN1
     C                   END
     C*
     C     AGPHN1        IFEQ      *ZEROS
     C                   MOVE      *OFF          *IN69
     C                   ELSE
     C                   MOVE      *ON           *IN69
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XXRDPY            2
     C*
     C* RETRIEVE PAYMENT BATCH#
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      'R'
     C     TTTYPE        ANDEQ     'Q'
     C                   MOVEL     TTKEY         XXRDPY
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C  N60              READ      PTT01                                  60
     C                   ENDDO
     C*
      *
      *  POPULATE BATCH VERBIAGE (XPBATV) FOR CLT 126 ONLY PROVIDING THAT
      *  A NON-TRANSFERRED BATCH WITH A DEDUCTIBLE PAYMENT CODE EXISTS
      *  WITH A POST DATE EQUAL TO THE POST DATE PASSED INTO THE PGM
      *
     C     CLTID         IFEQ      '126'
     C     XXRDPY        ANDNE     *BLANK
     C                   Z-ADD     0             XPBAT#            9 0
     C     FNKEY         SETGT     PEA05
     C     FNKEY         READPE    PEA05                                  60
     C     *IN60         DOWEQ     *OFF
     C     EASTAT        IFEQ      ' '
     C     $AARCD        ANDEQ     XXRDPY
     C     $APODT        ANDEQ     X2PODT
     C                   Z-ADD     EABAT#        XPBAT#
     C                   LEAVE
     C                   ENDIF
     C     FNKEY         READPE    PEA05                                  60
     C                   ENDDO
     C     XPBAT#        IFNE      *ZERO
     C                   MOVEL     BATH          XPBATV
     C                   MOVE      XPBAT#        XPBATV
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     SVAMT         XXAMT
     C                   Z-ADD     SVBILL        XXBILL
     C                   Z-ADD     SVPAID        XXPAID
     C                   Z-ADD     SVPTAX        XXPTAX
     C                   Z-ADD     SVBTAX        XXBTAX
     C                   Z-ADD     SVCTAX        XXCTAX
     C*
     C* CALCULATE PRIOR BILLED TOTAL, PRIOR PAID TOTAL, AND CURRENT BILLED TOTAL
     C*
     C     XXBILL        ADD       XXBTAX        XXBTOT
     C     XXPAID        ADD       XXPTAX        XXPTOT
     C     XXAMT         ADD       XXCTAX        XXCTOT
     C*
     C* CALCULATE BALANCE DUE
     C*
     C     XXBTOT        ADD       XXCTOT        XXTOTL
     C                   ADD       XXPTOT        XXTOTL
     C*
     C*** Begin Chg ***  06/24/24  *********************************************
     C***  XXTOTL        IFNE      *ZEROS
     C                   IF        (XXTOTL <> 0 or XXCOLT <> 0)
     C*** End   Chg ***  06/24/24  *********************************************
     C*
     C* PRINT NEW FORMAT PREMIUM NOTICE.
      *
     C                   MOVE      *ON           *IN63
     C                   if        not %open (W7325O)
      //*** Beg Add ***** 01/17/25 ***************************************
     C                             and isUseSpoolOut = *On
      //*** End Add ***** 01/17/25 ***************************************
     C                   OPEN      W7325O
     C                   endif
      //*** Beg Add ***** 01/17/25 ***************************************
       FlgPeoInv = 'Y';
      //*** End Add ***** 01/17/25 ***************************************
     C                   EXSR      NEWF
      //*** Beg Chg ***** 01/17/25 ***************************************
     C***                CLOSE     W7325O
        If isUseSpoolOut;
           Close W7325O;
        Endif;
      //*** End Chg ***** 01/17/25 ***************************************
     C                   END
     C*
     C                   ENDSR
      *
      *
      *
     C*================================================================
     C* Adjust for blank 'Insured' address lines
     C*================================================================
     C     FILLI         BEGSR
     C*
     C                   DO        3
     C*
     C     XXILN1        IFEQ      *BLANKS
     C                   MOVEL(P)  XXILN2        XXILN1
     C                   MOVEL(P)  XXILN3        XXILN2
     C                   MOVEL(P)  XXILN4        XXILN3
     C                   CLEAR                   XXILN4
     C                   ENDIF
     C*
     C     XXILN2        IFEQ      *BLANKS
     C                   MOVEL(P)  XXILN3        XXILN2
     C                   MOVEL(P)  XXILN4        XXILN3
     C                   CLEAR                   XXILN4
     C                   ENDIF
     C*
     C     XXILN3        IFEQ      *BLANKS
     C                   MOVEL(P)  XXILN4        XXILN3
     C                   CLEAR                   XXILN4
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* Adjust for blank 'Remit to' address lines
     C*================================================================
     C     FILLR         BEGSR
     C*
     C                   DO        3
     C*
     C     XXRLN1        IFEQ      *BLANKS
     C                   MOVEL(P)  XXRLN2        XXRLN1
     C                   MOVEL(P)  XXRLN3        XXRLN2
     C                   MOVEL(P)  XXRLN4        XXRLN3
     C                   CLEAR                   XXRLN4
     C                   ENDIF
     C*
     C     XXRLN2        IFEQ      *BLANKS
     C                   MOVEL(P)  XXRLN3        XXRLN2
     C                   MOVEL(P)  XXRLN4        XXRLN3
     C                   CLEAR                   XXRLN4
     C                   ENDIF
     C*
     C     XXRLN3        IFEQ      *BLANKS
     C                   MOVEL(P)  XXRLN4        XXRLN3
     C                   CLEAR                   XXRLN4
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*
     C* NEW FORMAT OUTPUT SUBROUTINE.
     C*
     C     NEWF          BEGSR
     C*
     C* POLICY INFO SECTION
     C*
     C     RPKY          CHAIN     PEL03                              60
      //*** Beg Add ***** 01/17/25 ***************************************
       x#RRN = dbrrn;
       If ELDiv = SRDiv;
          xsRRN = dbrrn;
       Endif;
      //*** End Add ***** 01/17/25 ***************************************
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XQST
     C                   ELSE
     C                   MOVE      *BLANKS       XQST
     C                   ENDIF
     C*
     C                   MOVEL(P)  LOSSH         XQLOSH
     C*
     C                   MOVEL(P)  DEDHDR        XXHDR
     C                   MOVEL(P)  SUMHDR        X1HDR
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C     WCHDR         CAT(P)    DEDH25:1      XXHDR
     C                   Z-ADD     2.55          HRZ1
     C                   Z-ADD     1.80          VRT1
     C                   OTHER
     C                   Z-ADD     3.49          HRZ1
     C                   Z-ADD     0.40          VRT1
     C                   ENDSL
     C*
     C                   MOVEL(P)  OFAAD1        XXAAD1
     C                   MOVEL(P)  OFAAD2        XXAAD2
     C*
     C     OFAAD2        IFNE      *BLANKS
     C                   MOVEL(P)  XACSZ         XOACSZ
     C                   ELSE
     C                   MOVEL(P)  XACSZ         XXAAD2
     C                   MOVE      *BLANKS       XOACSZ
     C                   ENDIF
     C*
     C     1             DO        8             A
     C     ATT(A)        IFNE      *BLANKS
     C                   MOVEL(P)  'Attn:'       ATT(A)
     C                   ENDIF
     C                   ENDDO
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C     1             DO        8             A
     C                   Z-ADD     A             B                 1 0
     C                   Z-ADD     0             Z                 1 0
     C     ATT(A)        IFNE      *BLANKS
     C                   Z-ADD     1             Z
     C* Move Attn: line to top
     C  N99
     CAN 99EMN(1)        IFNE      *BLANKS
     C     B             DOWGT     1
     C                   SUB       1             B
     C     B             ADD       1             C                 1 0
     C                   MOVEL     EMN1(B)       EMN1(C)
     C                   ENDDO
     C                   ENDIF
     C  N99
     CAN 99              Z-ADD     1             B
     C                   ENDIF
     C                   MOVEL(P)  ATT(A)        EMN1(B)
     C                   CAT       EMN(A):Z      EMN1(B)
     C                   ENDDO
     C                   Z-ADD     1             B
     C     ##NAML        ADD       1             A
     C                   MOVEL     EMN(A)        INS(B)
     C                   Z-ADD     ##ADRL        A
     C     2             DO        4             B
     C                   ADD       1             A
     C                   MOVEL     EMN(A)        INS(B)
     C                   ENDDO
     C                   ENDSL
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C* mm/dd/yyyy to mm/dd/yyyy
     C                   MOVEL(P)  XXPRDT        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XQPRDT
     C                   MOVEL(P)  XXDUED        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         XQDUED
     C*
     C                   OTHER
     C* month dd, yyyy to month dd, yyyy
     C                   Z-ADD     XXPRDT        XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MN(X)         XQPRDT
     C                   CAT       XXDAY:1       XQPRDT
     C                   CAT       ',':0         XQPRDT
     C                   CAT       XXYR:1        XQPRDT
     C*
     C                   ENDSL
     C*
     C                   MOVEL(P)  'Division'    XQDIVH           20
     C                   CAT       ':':0         XQDIVH
     C*
     C                   MOVEL(P)  'Rating S'    XQRSTH           20
     C                   CAT       'tate:':0     XQRSTH
     C*
     C*
     C                   MOVEL(P)  'Policy'      XQPROF           20
     C                   CAT       'For:':1      XQPROF
     C*
     C                   MOVEL(P)  'Print'       XQPROD           20
     C                   CAT       'Date:':1     XQPROD
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C                   MOVEL(P)  PRTPD         X1PERD
     C                   OTHER
     C                   MOVEL(P)  'For Peri'    XQPERD           20
     C                   CAT       'od:':0       XQPERD
     C                   ENDSL
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C* mm/dd/yyyy to mm/dd/yyyy
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XQPFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XQPTDT
     C*
     C                   OTHER
     C* month dd, yyyy to month dd, yyyy
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MN(X)         XQPFDT           20
     C                   CAT       XXDAY:1       XQPFDT
     C                   CAT       ',':0         XQPFDT
     C                   CAT       XXYR:1        XQPFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MN(X)         XQPTDT           20
     C                   CAT       XXDAY:1       XQPTDT
     C                   CAT       ',':0         XQPTDT
     C                   CAT       XXYR:1        XQPTDT
     C*
     C                   ENDSL
     C*
     C                   MOVEL(P)  XQPFDT        XQPOLP           45
     C                   CAT       ' to':1       XQPOLP
     C                   CAT       XQPTDT:1      XQPOLP
     C*
     C                   MOVEL(P)  CARPOL        XQCAPH
     C*
     C                   MOVE      *OFF          *IN93
     C     XXCLT#        IFEQ      '093'
     C                   MOVE      *ON           *IN93
     C                   ENDIF
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C                   MOVEL(P)  PART          @@PLCY
     C                   CAT       '#:':1        @@PLCY
     C     *IN33         WHENEQ    *ON
     C                   MOVEL     FILE          @@PLCY
     C                   MOVEL     FILE          @@PL93
     C                   OTHER
     C                   MOVEL     POL           @1PLCY
     C                   MOVEL     POL           @1PL93
     C                   ENDSL
     C*
     C                   MOVE      XXFND         XAFND             3
     C                   MOVE      SVEMP#        XAEMP             9
     C                   MOVE      XXBFYR        XAFYR             3
     C                   MOVE      SVDIV         XADIV             5
       If EAinv# <> 0;
          strInv# = 'Invoice #: ' + %Editc(xxCo#:'X') +
          %Editc(xxFnd:'X') + %Editc(SVemp#:'X') + '-' + %Editc(EAInv#:'X');
       Else;
          strInv# = *blanks;
       Endif;
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C                   MOVEL(P)  XAEMP         XXNUM
     C                   MOVEL(P)  '4'           EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XAEMP
     C                   CLEAR                   CHI
     C                   CLEAR                   CHO
     C                   MOVEA     XAEMP         CHI
     C                   Z-ADD     1             PO                2 0
     C*
     C     1             DO        20            PI                2 0
     C     CHI(PI)       IFNE      *BLANKS
     C                   MOVE      CHI(PI)       CHO(PO)
     C                   ADD       1             PO
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVEA     CHO           XAEMP
     C*
     C                   MOVEL(P)  XAEMP         XXPLCY
     C     XAFND         CAT       '-'           @@POL             4
     C     @@POL         CAT(P)    XXPLCY        XXPLCY
     C*
     C                   CLEAR                   CHI
     C                   CLEAR                   CHO
     C                   MOVEA     XQPRDT        CHI
     C                   Z-ADD     1             PO                2 0
     C*
     C     1             DO        20            PI                2 0
     C     CHI(PI)       IFNE      *BLANKS
     C                   MOVE      CHI(PI)       CHO(PO)
     C                   ADD       1             PO
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVEA     CHO           XQPRDT
     C*
     C                   CLEAR                   CHI
     C                   CLEAR                   CHO
     C                   MOVEA     XQDUED        CHI
     C                   Z-ADD     1             PO                2 0
     C*
     C     1             DO        20            PI                2 0
     C     CHI(PI)       IFNE      *BLANKS
     C                   MOVE      CHI(PI)       CHO(PO)
     C                   ADD       1             PO
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVEA     CHO           XQDUED
     C                   MOVEL(P)  XADIV         XXNUM
     C                   MOVEL(P)  '4'           EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XADIV
     C                   CAT       XADIV:1       XXPLCY
     C                   OTHER
     C                   MOVEL(P)  XAFND         XXPLCY
     C                   CAT       XAEMP:0       XXPLCY
     C                   CAT       XAFYR:0       XXPLCY
     C                   MOVE      XXPLCY        XXPL93
     C                   ENDSL
     C*
     C                   MOVEL(P)  BREAKH        XXBRK
     C*
     C                   MOVEL(P)  SEEH          XXSEE
     C*
     C*
     C                   MOVEL(P)  CANDH         XQCNDH
     C*
     C                   MOVEL(P)  CHRGS         X1BXH1
     C                   MOVEL(P)  PYMNTS        X1BXH2
     C                   MOVEL(P)  BAL           X1BXH3
     C                   MOVEL(P)  'Current'     X2BXH1
     C                   CAT       'Due':1       X2BXH1
     C                   MOVEL(P)  'Past'        X3BXH1
     C                   CAT       'Due':1       X3BXH1
     C                   MOVEL(P)  'Prior'       XXPRTO
     C                   CAT       'To':1        XXPRTO
     C                   MOVEL(P)  'Current'     XXCUBL
     C                   CAT       'Billed':1    XXCUBL
     C                   MOVEL(P)  'Balance'     XXBLDU
     C                   CAT       'Due':1       XXBLDU
     C*
     C* AGENT INFO SECTION
     C*
     C                   MOVE      *BLANKS       XXAGHD
     C     EXAHDR        CAT       ':':0         XXAGHD
     C*
     C                   MOVE      *BLANKS       XXAGPH
     C*
     C     AGPHN1        IFNE      *ZERO
     C                   MOVE      AGARE1        X1AREA            3
     C                   MOVEL     AGPHN1        X1AGN1            3
     C                   MOVE      AGPHN1        X1AGN2            4
     C*
     C                   MOVEL     '('           XXAGPH
     C                   CAT       X1AREA:0      XXAGPH
     C                   CAT       ')':0         XXAGPH
     C                   CAT       X1AGN1:1      XXAGPH
     C                   CAT       '-':1         XXAGPH
     C                   CAT       X1AGN2:1      XXAGPH
     C                   ENDIF
     C*
     C* INFO BOX SECTION
     C*
     C                   MOVE      *BLANKS       XXCHKH
     C                   MOVE      *BLANKS       XXREMH
     C                   MOVE      *BLANKS       XXSERH
     C                   MOVE      *BLANKS       XXDIRH
     C                   MOVE      *BLANKS       XXOFAD
     C                   MOVE      *BLANKS       XXSERN
     C                   MOVE      *BLANKS       XXDIRN
     C*
     C                   MOVEL(P)  CNDTH         XXCNDH
     C                   CLEAR                   CNDESC
     C                   SELECT
     C     EMPST         WHENNE    *BLANKS
     C     EMPST         CAT(P)    EMCNTY:0      XXKEY
     C                   OTHER
     C     EMMST         CAT(P)    EMCNTY:0      XXKEY
     C                   ENDSL
     C     TBKEY         CHAIN     PCN01                              60
     C     CNDESC        IFNE      *BLANKS
     C     CNDESC        CAT(P)    '-':0         XXCNDS
     C                   ENDIF
     C     XXCNDS        CAT(P)    XXCNDX:0      XXCNDS
     C                   MOVEL(P)  INSH          XXINSH
     C                   MOVEL(P)  AMDU          XXAMDH
     C                   MOVEL(P)  AMRE          XXAMRH
     C                   MOVEL(P)  DUEH          XQDUEH
     C                   MOVEL(P)  *ALL'_'       XXUNL1
     C                   MOVEL(P)  RTNW          XXRTNW
     C                   MOVEL(P)  ATCH          XXATCH
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C                   MOVEL(P)  'Make'        XXCHKH
     C                   CAT       'Check':1     XXCHKH
     C                   CAT       'Pay':1       XXCHKH
     C                   CAT       'able':0      XXCHKH
     C                   CAT       'To:':1       XXCHKH
     C                   OTHER
     C                   MOVEL     'Please'      XXCHKH
     C                   CAT       'make':1      XXCHKH
     C                   CAT       'checks':1    XXCHKH
     C                   CAT       'payabl':1    XXCHKH
     C                   CAT       'e to:':0     XXCHKH
     C                   ENDSL
       // Use Third party Verbiage if Opted in
       str3rdVrb = ' ';
       *in68 = *off;
       If (ICPolicyOpt(EAco#:EAfnd:EAemp#) = 'Y');
          str3rdVrb = OF3rdVrb;
          xxChkH  = ' ';
          xxChec  = ' ';
          xnName  = ' ';
          *in68 = *on;
       EndIf;
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C                   MOVEL(P)  'REMIT'       XXREMH
     C                   CAT       'TO:':1       XXREMH
     C                   OTHER
     C                   MOVEL     'Remit'       XXREMH
     C                   CAT       'to:':1       XXREMH
     C                   ENDSL
     C*
     C                   MOVEL     'Serviced'    XXSERH
     C                   CAT       'by:':1       XXSERH
     C*
      /free
       xxDirH = 'Direct Inquiries to:';
      /end-free
     C*
     C                   MOVEL     LBAAD1        XXOFAD
     C                   CAT       LBAAD2:1      XXOFAD
     C                   If        LBACSZ <> *Blanks
     C                   CAT       ',':0         XXOFAD
     C                   CAT       LBACSZ:1      XXOFAD
     C                   Endif
     C*
     C                   MOVEL     LBACNM        XXSERN
     C*
     C                   MOVEL     OFPRNM        XXDIRN
     C*
     C     XXPHN1        IFNE      *ZERO
     C                   MOVE      OFAARA        X1AREA            3
     C                   MOVE      XXPHN1        X1AGN1            3
     C                   MOVE      XXPHN2        X1AGN2            4
     C*
     C     *LIKE         DEFINE    XXDIRN        XPDIRN
     C                   MOVEL(P)  '('           XPDIRN
     C                   CAT       X1AREA:0      XPDIRN
     C                   CAT       ')':0         XPDIRN
     C                   CAT       X1AGN1:1      XPDIRN
     C                   CAT       '-':1         XPDIRN
     C                   CAT       X1AGN2:1      XPDIRN
     C                   CAT       XPDIRN:1      XXDIRN
     C                   MOVEL(P)  XPDIRN        X1DIRN
     C                   ENDIF
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '046'
     C     F2BLOG        ANDEQ     'TIMBER'
     C                   Z-ADD     0.5           LOGHRZ
     C                   Z-ADD     6.3           LOGVRT
     C     XXCLT#        WHENEQ    '025'
     C     F2BLOG        ANDEQ     'LOGO025'
     C                   Z-ADD     .625          LOGHRZ
     C                   Z-ADD     .625          LOGVRT
     C     XXCLT#        WHENEQ    '025'
     C     F2BLOG        ANDEQ     'TASBPN'
     C                   Z-ADD     1.060         LOGHRZ
     C                   Z-ADD     0.625         LOGVRT
     C                   OTHER
     C                   Z-ADD     0.6           LOGHRZ
     C                   Z-ADD     5.2           LOGVRT
     C                   ENDSL
     C*
     C     CLTID         IFEQ      '025'
     C                   Z-ADD     5.36          STMHV
     C     'State'       CAT(P)    'ment':0      XQSTMH
     C                   CAT       '#:':1        XQSTMH
     C                   MOVEL(P)  SVINV#        XXNUM
     C                   MOVEL(P)  '4'           EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XQSTMD
     C*
     C                   CLEAR                   CHI
     C                   CLEAR                   CHO
     C                   MOVEA     XQSTMD        CHI
     C                   Z-ADD     1             PO
     C*
     C     1             DO        20            PI
     C     CHI(PI)       IFNE      *BLANKS
     C                   MOVE      CHI(PI)       CHO(PO)
     C                   ADD       1             PO
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVEA     CHO           XQSTMD
     C                   ENDIF
     C*
     C* Define line positions for headers
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C                   Z-ADD     3.3           HRZ2              5 3
     C                   Z-ADD     5.2           HRZ3              5 3
     C                   Z-ADD     3.3           HRZ4              5 3
     C                   OTHER
     C                   Z-ADD     3.3           HRZ2
     C                   Z-ADD     5.2           HRZ3
     C                   Z-ADD     3.3           HRZ4
     C                   ENDSL
     C*
     C     XXBTOT        ADD       XXCTOT        XXCBAL
     C     XXBTOT        ADD       XXPTOT        XXPBAL
     C                   Z-ADD     XXPTOT        X1PTOT
     C                   Z-ADD     XXCTOT        X1CTOT
     C                   Z-ADD     XXPBAL        X1PBAL
     C                   MOVEL     XXCHEC        X1CHEC
     C*
     C                   SELECT
       //*** Beg Add ***** 01/17/25 *******************************
       When Not(isUseSpoolOut);
       //*** End Add ***** 01/17/25 *******************************
     C     CLTID         WHENEQ    '025'
     C                   EXSR      INV025
     C                   when      not(isUseSpoolOut)
     C                   OTHER
     C                   WRITE     QHEAD
     C                   WRITE     QHEAD1
     C                   WRITE     QLINE1
     C                   WRITE     QHEAD2
     C                   WRITE     QHEAD3
     C                   WRITE     QDET
     C     XX            IFEQ      '1'
     C                   WRITE     QATCH
     C                   ELSE
     C* REVERSE ARRAY ORDER
     C                   Z-ADD     1             I                 2 0
     C     ARA(12)       IFNE      0
     C                   Z-ADD     ARA(12)       ARB(1)
     C                   ADD       1             I
     C                   ENDIF
     C*
     C                   Z-ADD     11            X
     C     X             DOWGT     0
     C     ARA(X)        IFNE      0
     C                   Z-ADD     ARY(X)        ARZ(I)
     C                   Z-ADD     ARA(X)        ARB(I)
     C                   ADD       1             I
     C                   ENDIF
     C                   SUB       1             X
     C                   ENDDO
     C*
     C                   MOVEA     ARZ           ARY
     C                   MOVEA     ARB           ARA
     C  N63              WRITE     QBREAK
     C   63              WRITE     QAGT
     C                   ENDIF
     C                   WRITE     QINFO
     C                   WRITE     QBOT
     C     XX            IFEQ      '1'
     C*
     C                   Z-ADD     3.60          LIN
     C                   Z-ADD     0.6           POS
     C                   Z-ADD     1.1           POS1
     C                   Z-ADD     3.0           POS2
     C                   Z-ADD     3.6           POS3
     C*
     C                   WRITE     QENDPG
     C                   WRITE     QHEAD1
     C                   WRITE     QLN1OTH
     C                   WRITE     QHD2OTH
     C                   WRITE     QPOLBRK
     C*Print Arrays in descending order
     C     @Z            DOWGT     0
     C                   MOVE      ARU(@Z)       XXARU
     C                   MOVE      ARM(@Z)       XXARM
     C                   SUB       1             @Z
     C     @Z            IFEQ      0
     C                   MOVE      *ON           *IN67
     C                   WRITE     QBREAK1
     C                   LEAVE
     C                   ENDIF
     C                   MOVE      ARU(@Z)       XXARU1
     C                   MOVE      ARM(@Z)       XXARM1
     C                   WRITE     QBREAK1
     C                   SUB       1             @Z
     C                   ADD       .15           LIN
     C                   ENDDO
     C                   ENDIF
     C                   ENDSL

       //*** Beg Add ***** 01/17/25 *******************************
       If isUseXml;
          If FlgPeoInv = 'Y';
             XXSUMM = '1';
             Exsr ScriptXml;
             FlgPeoInv = 'N';
          Else;
             XXSUMM = '0';
             Exsr ScriptXml;
             If xxFnd <> svFnd;
                svFnd = xxFnd;
                WrtDedCrtXML1();             //Write Deductible Criteria for each selected group
             Endif;
          Endif;
          WrtDedInvXML();
          WrtDedInv1XML();
          WrtDedCrtXML();
       Endif;
       //*** End Add ***** 01/17/25 *******************************

      /End-free
     C*
     C                   ENDSR
     C*****************************************************************
     C*

       //*** Beg Add ***** 01/17/25 ********************************************
       //=========================================================================
       //  ScriptXML - Reserve/Update XML record for Scriptura output.
       //=========================================================================
       Begsr ScriptXML;

       Chain Q1User pup01;

       xmlFile = ' ';
       message = %trim(processTypeD);
       string = ' ';

       If isMassReq;
          // Create mass entry
          StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
          processType:'000':'000':massFlag:xmlLink);

          // Update LDA with mass flow name
          In *Lock *dtaara;
          ldMassFile = %trim(xmlFile);          //Flow File Name
          Out *dtaara;

          Chain (xmlFile) xp100;
          XPCat = 'PS';
          Update xp100;

          isMassReq = *off;
          flowFileName = xmlFile;
          xmlLink = xmlFile;
       Endif;

       Select;
       When xxSumm = '1';
          If FlgPeoInv = 'Y';
             message = %trim(processTypeD);
          Else;
             message = %trim(processTypeS);
          Endif;
          // Create individual entry for Summary Invoice
          // or Summary Invoice for PEO Policy (NEWF called from SUMINV)
          StLetter('XMLREQUEST':xmlFile:message:'E':%editc(xsRRN:'X'):xxPre:
           string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:xmlLink);

          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
          If %Found(XMLPREL1);
             XPEmailD = strXPDirSRD;
             XPEmailF = strXPFileSRD;
             XPdrseq# = svSeq#SRD;
             XPData = 'Y';
             Clear XPMail;
             Clear XPMailAdd;
             If FlgPeoInv = 'Y';
                undLoc = %Scan('_':XPFile);
                If undLoc <> 0;
                   XPFile = 'PEO' + %Subst(%Trim(XPFile):undLoc);
                Endif;
             Endif;
             Update xp100;
          Endif;
       Other;
          //Create individual entry for Regular Invoice
          StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x#RRN:'X'):xxPre:
           string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:xmlLink);

          //Set keyEmial field in LDA. This value will get picked up and written to
          // XMLPREP record of the Deductible Invoice Detail Report to later set them as
          // an additional attachment to the Deductible Invoice email.
          In *Lock *dtaara;
          keyEmail= %Trim(Q1User) + %subst(%char(%timestamp:*iso0):1:17);
          Out *dtaara;

          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
          If %Found(XMLPREL1);
             //Save XPDir and XPFile to update XMLPREP for the detail report
             strXPDir = XPDir;
             strXPFile = XPFile;
             If svDiv = SRDiv;
                strXPDirSRD = XPDir;
                strXPFileSRD = XPFile;
             Endif;
             XPData = 'Y';
             Update xp100;
          Endif;

       ENDSL;

       Endsr;
       //*** End Add ***** 01/17/25 ********************************************
     C*====================================================================
     C* Write the invoice(s) for CLT025
     C*====================================================================
     C     INV025        BEGSR
     C     X             IFGT      12
     C                   MOVE      '1'           XX                1
     C                   MOVE      X             @Z                3 0
     C                   ELSE
     C                   MOVE      '0'           XX                1
     C                   ENDIF
     C                   MOVEL(P)  XXTOTL        XXNUM
     C                   MOVEL(P)  'J'           EDTMSK
     C                   Z-ADD     2             ##DEC
     C                   MOVE      *ON           ##THOU
     C                   CALL      CHGNUM        CHGPL2
     C     ' '           CHECK     XXNUM         N                 3 0
     C     N             IFLE      80
     C                   SUBST(P)  XXNUM:N       XXNUM
     C                   ENDIF
     C     '$'           CAT       XXNUM:0       XQTOTL
     C*
     C                   WRITE     QHEAD
     C                   WRITE     QH1025
     C                   WRITE     QLN025
     C                   WRITE     QHD025
     C                   WRITE     QDT025
     C     XX            IFEQ      '1'
     C                   WRITE     QATCH025
     C                   ELSE
     C*
     C* X HAS THE NUMBER OF ELEMENT POPULATED IN THE ARRAY
     C*
     C                   Z-ADD     X             M                 2 0
     C                   CLEAR                   ART
     C                   CLEAR                   ARJ
     C*
     C* MOVING ALL THE VALUES TOWARD THE END OF THE ARRAY  ARY AND ARA
     C*
      *
      * PRV LOGIC WOULD LOOP THROUGH ARY ONLY 6 TIMES
      * BECAUSE L WAS DECREMENTED WITHIN THE LOOP.
      * CONSEQUENTLY, THE ARRAYS COULD ONLY HOLD 6 YEARS.
      *
     C     M             IFLT      12
     C     M             ANDGT     0
     C     13            SUB       M             L                 2 0
     C                   DO        M             #X                2 0
     C                   MOVE      ARY(#X)       ART(L)
     C                   MOVE      ARA(#X)       ARJ(L)
     C                   ADD       1             L
     C                   ENDDO
     C                   MOVEA     ART           ARY
     C                   MOVEA     ARJ           ARA
     C                   ENDIF
     C*
     C  N63              WRITE     QBR025
     C   63              WRITE     QAG025
     C                   ENDIF
     C                   WRITE     QBT025
     C*
     C     XX            IFEQ      '1'
     C*
     C                   Z-ADD     5.35          LIN
     C                   Z-ADD     0.6           POS
     C                   Z-ADD     0.9           POS1
     C                   Z-ADD     2.8           POS2
     C                   Z-ADD     3.1           POS3
     C*
     C                   WRITE     QENDPG
     C                   WRITE     QH1025
     C                   WRITE     QLN1025
     C                   WRITE     QHD1025
     C* "BREAK OUT OF CURRENT BILLED..." Text
     C                   WRITE     QBRK025
     C*Print records on next page in loop
     C     @Z            DOWGT     0
     C                   MOVE      ARU(@Z)       XXARU
     C                   MOVE      ARM(@Z)       XXARM
     C                   SUB       1             @Z
     C     @Z            IFEQ      0
     C                   MOVE      *ON           *IN67
     C                   WRITE     QBR1025
     C                   LEAVE
     C                   ENDIF
     C                   MOVE      ARU(@Z)       XXARU1
     C                   MOVE      ARM(@Z)       XXARM1
     C                   WRITE     QBR1025
     C                   SUB       1             @Z
     C                   ADD       .15           LIN
     C                   ENDDO
     C                   ENDIF
     C                   ENDSR
     C*
     C*====================================================================
     C* Calculation for Current account policy
     C*====================================================================
     C     SRCACT        BEGSR
     C*
     C     ABKEY         CHAIN     PAB01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     ABFYR         IFNE      XXSVYR
     C                   Z-ADD     ABFYR         XXBFYR
     C     F1KEY         CHAIN     PF101                              60
     C   60              MOVE      *BLANKS       F1DTXB
     C                   ENDIF
     C*
     C                   MOVEL(P)  ABARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'R'
     C     ABARTY        ANDEQ     'Q'
     C     ABARCD        ANDNE     F1DTXP
     C                   SUB       ABAMT         XXPAID
     C                   ENDIF
     C*
     C* ACCUMULATE PRIOR PAID TAX RECORDS
     C*
     C     ABARCD        IFEQ      F1DTXP
     C                   SUB       ABAMT         XXPTAX
     C                   ENDIF
     C*
     C* ACCUMULATE PRIOR BILLED TAX RECORDS
     C*
     C     ABARCD        IFEQ      F1DTXB
     C     ABPODT        ANDLT     X2PODT
     C                   ADD       ABAMT         XXBTAX
     C                   ENDIF
     C*
     C* ACCUMULATE CURRENT BILLED TAX RECORDS
     C*
     C     ABARCD        IFEQ      F1DTXB
     C     ABPODT        ANDEQ     X2PODT
     C                   ADD       ABAMT         XXCTAX
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     ABFYR         XXSVYR            3 0
     C     ABKEY         READE     PAB01                                  60
     C                   END
     C*
     C                   ENDSR
     C*
      ********************************************************************
      *  DARTS SUBROUTINE  -  CREATE DARTS RECORD
      ********************************************************************
     C     DARTS         BEGSR
      *
      *
      *
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
      *
      *  If office is installed, resolve output for DARTS
      *
     C     SYOFIF        IFEQ      'Y'
     C     XXDFLG        ANDEQ     'Y'
     c     isUseSpoolout AndEQ     *on
     C                   MOVE      XXFND         ##FND
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       ##FLR            80
     C                   PARM      *BLANKS       ##DCID           12
     C                   PARM      'PN'          ##PRE             2
     C                   PARM      QQCO#         ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM      'P'           ##RTCD            1
      *
     C                   MOVE      SVEMP#        ##EMP#
      *
     C                   CALL      'W8008C0'
     C                   PARM                    ##FLR            80
     C                   PARM                    ##DCID           12
     C                   PARM                    ##EMP#            9
     C                   PARM                    X1HOLD
      *
     C                   MOVEL(P)  ##FLR         Q#FOLD           80
     C                   MOVEL(P)  ##DCID        Q#DCID           12
     C                   ENDIF
      *
     C                   SELECT
      *
     C     XXDFLG        WHENEQ    'Y'
     C     USEGRP        ANDEQ     *ON
     C                   MOVEL     EXDDRM        ##DRMK
     C     ##DRMK        IFEQ      ' '
     C                   MOVEL     XXDRMK        ##DRMK
     C                   ENDIF
      *
     C     XXDFLG        WHENEQ    'Y'
     C     USEGRP        ANDEQ     *OFF
     C     XXDRMK        IFEQ      ' '
     C                   MOVEL     EXDDRM        ##DRMK
     C                   ELSE
     C                   MOVEL     XXDRMK        ##DRMK
     C                   ENDIF
     C                   ENDSL
      *
      *   ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       ZZKEY
     C                   MOVE      XXCO#         ZZCO#
     C                   MOVE      XXFND         ZZFND
     C                   MOVE      SVEMP#        ZZEMP#
     C                   MOVE      SVDIV         ZZDIV
     C* IF RESOLVING SUMMARY DED THEN SET TO STATE REPORTING DIV
     C     XXSUMM        IFEQ      '1'
     C                   MOVE      SRDIV         ZZDIV
     C                   ENDIF
     C*
      // Remark (##DRMK)is blank when called from W7325C (Reprint) so following logic is not
      //  executed for Reprint Ded Inv (Reprint Ded Inv will not be attched to Darts), the
      //  PDF output will be in user's folder for reprint.
      // Following logic is only executed for Single Employer run or
      //   Mass run (W8008C in call stack)

     C     ##DRMK        IFNE      ' '
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  ZZKEY         Q#KEY            40
     C     CLTID         IFEQ      '071'
     C                   MOVEL     D71           DACOMM          255
     C                   ELSE
     C                   MOVEL     DA            DACOMM          255
     C                   ENDIF
     C     XXSUMM        IFEQ      '1'
     C                   MOVEL     DAS           DACOMM
     C                   ENDIF
     C                   MOVEL(P)  RPDATE        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   CALL      CHGNUM        CHGPL
     C                   CAT       XXNUM:1       DACOMM
     C                   CAT       DA2:1         DACOMM
     C                   MOVEL     RSDAT2        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   CALL      CHGNUM        CHGPL
     C                   CAT       XXNUM:1       DACOMM
     C                   CAT       '.':0         DACOMM
     C                   CAT       'BY:':1       DACOMM
     C                   CAT       UPUSNM:1      DACOMM
     C                   CAT       '.':0         DACOMM
     C                   CAT       'AMOUNT':1    DACOMM
     C                   CAT       'DUE:':1      DACOMM
     C                   MOVEL(P)  XXTOTL        XXNUM
     C                   MOVEL     'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   CAT       XXNUM:0       DACOMM
     C                   CAT       '.':0         DACOMM
     C*
     C* CHANGE DESCRIPTION IN THE COMMENT IF IDARTS IS INSTALLED, TO INDICATE
     C* THE DEDUCTIBLE DETAIL REPORT IS ALSO INCLUDED ON THE REMARK.
     C*
     C     SYSCIF        IFEQ      'Y'
     C     XXDFLG        ANDEQ     'Y'
     C     XXSUMM        ANDNE     '1'
     C*
     C                   CAT       DD:1          DACOMM
     C*
     C                   ENDIF
     C                   MOVEL(P)  DACOMM        Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  'N'           Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVEL(P)  ##DRMK        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  'O'           Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2

     C                   ENDIF

       //*** Beg Add ***** 01/17/25 ********************************************
       svQ#dcId = Q#dcId;
       svQ#fold = Q#fold;
       Clear svSeq#;
       //Write iDarts remark for Scriptura if Deductible Invoice is being generated
       //  Saparate Darts is not needed for Summary Invoice if Scriptura output
       If isUseXml and XXSUMM = '0' and ##DRMK <> ' ';   //Regular Ded Invoice & Not Reprint
          Q#dcId = *Blanks;
          Q#fold = *Blanks;
          //Q#uncn = %trim(strXPDir) + %trim(strXPFile);
          WrtDrtRmk(q#key:q#comm:q#cdat:q#dcid:q#pnam:q#secl:q#user:
           q#pgm:q#fold:q#difl:q#ipgm:q#fl01:q#fl02:q#rmcd:q#seq#:
           q#sbyl:q#com2:q#apid:q#idoc:q#ique:q#imgf:q#ipth:q#pidx:
           q#qpty:q#dctp:q#csid:q#code);

          //Save Seq#. This Seq# will also be used to update XMLPREP for
          // Ded Invoice detail Report
          If Q#seq# <> *Blank and Q#seq# <> '00000';
             svSeq# = %Int(Q#seq#);
             If svDiv = SRDiv;
                svSeq#SRD = svSeq#;
             Endif;
          Endif;

          //Update XMLPREP record of the Ded Invoice with Darts Seq#
          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
          If %Found(XMLPREL1);
             XPdrseq# = svSeq#;
             Update xp100;
          Endif;

       Endif;
       //For: Single Employer run or Mass run (W8008C in call stack)
       //  Generate Darts if Spool file output and Summary Invoice is being generated
       //   OR Deductible Invoice is being generated
       If ##DRMK <> ' ' and isUseSpoolOut;                    //Spool file output & Not Reprint
          Q#dcId = svQ#dcId;
          Q#fold = svQ#fold;
          WrtDrtRmk(q#key:q#comm:q#cdat:q#dcid:q#pnam:q#secl:q#user:
           q#pgm:q#fold:q#difl:q#ipgm:q#fl01:q#fl02:q#rmcd:q#seq#:
           q#sbyl:q#com2:q#apid:q#idoc:q#ique:q#imgf:q#ipth:q#pidx:
           q#qpty:q#dctp:q#csid:q#code);
       Endif;

       Exsr DedDtlRpt;

       In *Lock *dtaara;
       Out *dtaara;

       //Update XMLPREP for Ded Invoice Detail Report with XPDIR and XPFILE value from
       // the individual XMLPREP record created for Ded Invoice
       // This will allow Deductible detail report to be emailed with Ded Invoice.
       If keyEmail <> *blank;
          strEmailD = keyEmail;
          Setll strEmailD xp105;
          Reade strEmailD xp105;
          Dow not %Eof(xmlprel5);
             XPEmailD = strXPDir;
             XPEmailF = strXPFile;
             XPdrseq# = svSeq#;         //Update detail Rpt w/ Same Seq# as Ded Invoice
             Update xp105;
             Reade strEmailD xp105;
          Enddo;
       Endif;
      //*** End Add ***** 01/17/25 ********************************************
      *
     C                   ENDSR
      *================================================================
      * Call Deductible Dtl Rpt in Single Employer Mode
      *================================================================
     c     DedDtlRpt     Begsr

     c                   If        xxDFlg <> 'Y' or cltId = '071'
     c                             or xxSumm = *on
     c                   LeaveSr
     c                   Endif

     C                   MOVE      XXFND         ##FND
     C                   MOVE      SVEMP#        ##EMP#
     C                   MOVE      SVDIV         ##DIV
     C                   MOVE      XXBFYR        #1FYR

     C                   CALL      'W7936C2'
     C                   PARM      QQCO#         ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM      QQPODT        ##PODT            8
     C                   PARM      @@OUTQ        ##QUE            10
     C                   PARM      QQLSDT        ##LSDT            8
     C                   PARM      QQTFND        ##TFND            3
     C                   PARM      'B'           ##MODE            1
     C                   PARM      ' '           ##EXCL            1
     C                   PARM      QQFRGN        ##FRGN            3
     C                   PARM      QQTRGN        ##TRGN            3
     C                   PARM      'B'           ##DDTP            1
     C                   PARM                    ##EMP#            9
     C                   PARM                    ##DIV             5
     C                   PARM                    #1FYR             3
     C                   PARM                    ##DRMK            5
     C                   PARM                    #1Y4N             1

     c                   Endsr
       //===============================================================
       // Write Scriptura Workfile - Deductible Invoice
       //===============================================================
     p WrtDedInvXML    b
      /free
       Open WxDedInv;
       //*** Beg Chg ***** 01/17/25 *******************************
       //***DIxmlkey = xmlfile;
       DIxmlkey = XPxmlT;
       //*** End Chg ***** 01/17/25 *******************************
       DIxxco#  = xxco#;
       DIxxfnd  = xxfnd;
       DIsvemp# = svemp#;
       DIsvdiv  = svdiv;
       DIxxhdr  = xxhdr;
       DIx1hdr  = x1hdr;
       DIxnname = xnname;
       DIxxaad1 = xxaad1;
       DIxxaad2 = xxaad2;
       DIxoacsz = xoacsz;
       //*** Beg Chg ***** 01/17/25 *******************************
       //***DI@@plcy = @@plcy;
       //***DI@1plcy = @1plcy;
       DIFIL#LBL = @@plcy;
       DIPOL#LBL = @1plcy;
       //*** End Chg ***** 01/17/25 *******************************
       DIxxplcy = xxplcy;
       DIxqperd = xqperd;
       DIxqpolp = xqpolp;
       DIxqcaph = xqcaph;
       DIrpcap# = rpcap#;
       DIxxatt1 = xxatt1;
       DIxxemn1 = xxemn1;
       DIxqdivh = xqdivh;
       DIsvdiv  = svdiv;
       DIxqprod = xqprod;
       //*** Beg Chg ***** 01/17/25 ************************
       //***DIxqprdt = xqprdt;
       If xxPrDt <> 0;
          DIxqprdt = %Editc(xxPrDt:'X');
       Endif;
       //*** End Chg ***** 01/17/25 ************************
       DIxxatt2 = xxatt2;
       DIxxemn2 = xxemn2;
       DIxqrsth = xqrsth;
       DIxqst   = xqst;
       DIxxatt3 = xxatt3;
       DIxxemn3 = xxemn3;
       DIxxatt4 = xxatt4;
       DIxxemn4 = xxemn4;
       DIxxatt5 = xxatt5;
       DIxxemn5 = xxemn5;
       DIxxatt6 = xxatt6;
       DIxxemn6 = xxemn6;
       DIxxatt7 = xxatt7;
       DIxxemn7 = xxemn7;
       DIxxatt8 = xxatt8;
       DIxxemn8 = xxemn8;
       DIxqcndh = xqcndh;
       //*** Beg Chg ***** 01/17/25 ************************
       //***DIxxcndt = %subst(%editc(xxcndt:'X'):1:2) + '/' +
       //***           %subst(%editc(xxcndt:'X'):3:2) + '/' +
       //***           %subst(%editc(xxcndt:'X'):5:4);
       If xxCnDt <> 0;
          DIxxcndt = %Editc(xxCnDt:'X');
       Endif;
       //*** End Chg ***** 01/17/25 ************************
       DIxqlosh = xqlosh;
       //*** Beg Chg ***** 01/17/25 ****************************
       //***DIxxlsdt = %subst(%editc(xxlsdt:'X'):1:2) + '/' +
       //***           %subst(%editc(xxlsdt:'X'):3:2) + '/' +
       //***           %subst(%editc(xxlsdt:'X'):5:4);
       If xxLsDt <> 0;
          DIxxlsdt = %Editc(xxLsDt:'X');
       Endif;
       //*** End Chg ***** 01/17/25 ****************************
       DIxxbill = xxbill;
       DIxxbtax = xxbtax;
       DIxxbtot = xxbtot;
       DIxxpaid = xxpaid;
       DIxxptax = xxptax;
       DIxxptot = xxptot;
       DIxxpbal = xxpbal;
       DIxxpbtx = xxpbtx;
       DIxxpbtt = xxpbtt;
       DIxxamt  = xxamt;
       DIxxctax = xxctax;
       DIxxctot = xxctot;
       DIxxbrk  = xxbrk;
       DIxxaghd = xxaghd;
       DIagname = AGname;
       DIxxcom1 = xxcom1;
       DIxxcom2 = xxcom2;
       DIxxcom3 = xxcom3;
       DIxxcom4 = xxcom4;
       DIxxaad1 = xxaad1;
       DIxxacsz = xxacsz;
       DIxxagph = xxagph;
       DIxxsee  = xxsee;
       DIxxchkh = xxchkh;
       DIxxchec = xxchec;
       DIxxremh = xxremh;
       DIxxofad = xxofad;
       DIxxserh = xxserh;
       DIxxsern = xxsern;
       DIxxdirh = xxdirh;
       DIxxdirn = xxdirn;
       DIxxtotl = xxtotl;
       //*** Beg Chg ***** 01/17/25 *******************************
       //***DIxxpodt = %subst(%editc(xxpodt:'X'):1:2) + '/' +
       //***           %subst(%editc(xxpodt:'X'):3:2) + '/' +
       //***           %subst(%editc(xxpodt:'X'):5:4);
       If xxPoDt <> 0;
          DIxxpodt = %Editc(xxPoDt:'X');
       Endif;
       //*** End Chg ***** 01/17/25 *******************************
       //*** Beg Add ***** 01/17/25 *******************************
       DIPSSN   = #1Y4N;
       DIXXCOTR = XXcol;
       DIXXCTTOT= XXcolT;
       DIXXCD1  = QQcom1;
       DIXXCD2  = QQcom2;
       DIXXCD3  = QQcom3;
       DIXXCD4  = QQcom4;
       DIRMTOAD1 = LBAAD1;
       DIRMTOAD2 = LBAAD2;
       DIRMTOCSZ = LBACSZ;
       DIINV# = %Editc(xxCo#:'X') +
          %Editc(xxFnd:'X') + %Editc(SVemp#:'X') + '-' + %Editc(EAInv#:'X');
       DIDRNAME = OFPRNM;
       DIDRPHN# = XPDIRN;
       If (ICPolicyOpt(EAco#:EAfnd:EAemp#) = 'Y');
          DICHKPYBL = OF3rdVrb;
       Else;
          DICHKPYBL = 'Please make checks payable to:  ' + %Trim(xxchec);
       Endif;
       //*** End Add ***** 01/17/25 *******************************
       Write pdi01;
       Close WxDedInv;
      /end-free
     p WrtDedInvXML    e

      //*** Beg Add ***** 01/17/25 *******************************
      //======================================================================
      // Write Selection Criteria Work File - One record per each Ded Invoice
      //======================================================================
     p WrtDedCrtXML    b
      /free
       Open WxDedCrt;
       Clear pcr01;
       CRXMLKEY  = XPxmlT;
       CRCO#     = xxCo#;
       CRFND     = xxFnd;
       CREMP#    = svEmp#;
       CRDIV     = svDiv;
       If X@DIV = 0;
          CRALLDIV  =  'Y';         //All Div flag is not passed to this pgm directly
       Else;
          CRALLDIV  =  'N';
       Endif;
       CRLOSSDT  =  xxLsDt;
       CRPOSTDT  = xxPoDt;
       CRPRNTDT  = xxPrDt;
       CRDBLTYP  = XXDDTP;
       CRPRTSSN  = #1Y4N;
       CRCOMCD1  = QQcom1;
       CRCOMCD2  = QQcom2;
       CRCOMCD3  = QQcom3;
       CRCOMCD4  = QQcom4;
       CRCOMDES1 = xxcom1;
       CRCOMDES1 = xxcom2;
       CRCOMDES1 = xxcom3;
       CRCOMDES1 = xxcom4;
       CRDARTS = XXDFLG;
       CRRMKCD   = XXDRMK;
       CREXPNACT = *Blank;          //Excl Pol W/ No act is not passed, It's for Ded dtl Rpt
       CRDINVFLG = 'Y';             //Not Passed but this pgm is only called if this flg is 'Y'
       CRDRPTFLG = 'Y';             //Ded Rpt Flg is not passed to this pgm
       CROutQ = @@OUTQ;
       Write pcr01;
       Close WxDedCrt;
      /end-free
     p WrtDedCrtXML    e

      //==========================================================================
      // Write Selection Criteria Work File - One record per Group selected to Run
      //==========================================================================
     p WrtDedCrtXML1   b
      /free
       Open WxDedCrt1;
       Clear pcr02;
       C1XMLKEY = ldMassFile;
       C1CO#    = xxCo#;
       C1FND    = xxFnd;
       C1FNDNM  = FNname;
       Write pcr02;
       Close WxDedCrt1;
      /end-free
     p WrtDedCrtXML1   e
      //*** End Add ***** 01/17/25 *******************************

      //===============================================================
      // Write Scriptura Workfile - Deductible Invoice detail per year
      //===============================================================
     p WrtDedInv1XML   b
     d ctr             s              3  0
     d noFyrBreakOut   c                   '0'
     d fyr             s              3  0
     d amt             s             13  2
      /free
       Open WxDedInv1;
       For ctr = 1 to 100;
          If WrtFyrBrkOut(aru(ctr):arm(ctr)) = noFyrBreakOut;
             Leave;
          Endif;
       Endfor;
       Close WxDedInv1;
      /end-free
     p WrtDedInv1XML   e

      //===============================================================
      // Write Scriptura Workfile - Deductible Invoice detail per year
      //===============================================================
     p WrtFyrBrkOut    b
     d WrtFyrBrkOut    pi             1
     d  fyr                           3  0
     d  amt                          13  2
      /free
       If fyr = 0;
          Return '0';
       Endif;
       //*** Beg Chg ***** 01/17/25 *******************************
       //***D2xmlkey = xmlfile;
       D2xmlkey = XPxmlT;
       //*** End Chg ***** 01/17/25 *******************************
       D2xxco#  = xxco#;
       D2xxfnd  = xxfnd;
       D2svemp# = svemp#;
       D2svdiv  = svdiv;
       D2xxfyr  = fyr;
       D2xxamt  = amt;
       Write pdi02;
       Return '1';
      /end-free
     p WrtFyrBrkOut    e

      //===============================================================
      // Write Scriptura Workfile - Summary Deductible Invoice
      //===============================================================
     p WrtSumInvXML    b
      /free
       Open WxSumInv;
       //*** Beg Chg ***** 01/17/25 *******************************
       //***DSxmlkey = xmlFile;
       DSxmlkey = XPxmlT;
       //*** End Chg ***** 01/17/25 *******************************
       DSxxco#  = xxco#;
       DSxxfnd  = xxfnd;
       DSsvemp# = svemp#;
       DSx1hdr  = x1hdr;
       DSxnname = xnname;
       DSxxaad1 = xxaad1;
       DSxxaad2 = xxaad2;
       DSxoacsz = xoacsz;
       DSxxatt1 = xxatt1;
       DSxkemn1 = xkemn1;
       DSxkdbih = xkdbih;
       //*** Beg Chg ***** 01/17/25 ************************
       //***DSxkdbid = xkdbid;
       If xxPrDt <> 0;
          DSxkdbid = %Editc(xxPrDt:'X');
       Endif;
       //*** End Chg ***** 01/17/25 ************************
       DSxxatt2 = xxatt2;
       DSxkemn2 = xkemn2;
       DSxkpolh = xkpolh;
       DSxkpoln = xkpoln;
       DSxxatt3 = xxatt3;
       DSxkemn3 = xkemn3;
       DSxkdivh = xkdivh;
       DSxkdiv  = xkdiv ;
       DSxxatt4 = xxatt4;
       DSxkemn4 = xkemn4;
       DSxkcanh = xkcanh;
       //*** Beg Chg ***** 01/17/25 ************************
       //***DSxkcand = xkcand;
       If xxCnDt <> 0;
          DSxkcand = %Editc(xxCnDt:'X');
       Endif;
       //*** End Chg ***** 01/17/25 ************************
       DSxxatt5 = xxatt5;
       DSxkemn5 = xkemn5;
       DSxklosh = xklosh;
       //*** Beg Chg ***** 01/17/25 ****************************
       //***DSxklosd = xklosd;
       If x1LsDt <> 0;
          DSxklosd = %Editc(x1LsDt:'X');
       Endif;
       //*** End Chg ***** 01/17/25 ****************************
       DSxxatt6 = xxatt6;
       DIxkemn6 = xkemn6;
       DSxkamdh = xkamdh;
       DSxkamdu = xkamdu;
       DSxxatt7 = xxatt7;
       DSxkemn7 = xkemn7;
       DSxkdueh = xkdueh;
       //*** Beg Chg ***** 01/17/25 ****************************
       //***DSxkdued = xkdued;
       If XXDued <> 0;
          DSxkdued = %Editc(XXDued:'X');
       Endif;
       //*** End Chg ***** 01/17/25 ****************************
       DSxkdorh = xkdorh;
       DSxxatt8 = xxatt8;
       DSxkemn8 = xkemn8;
       DSxxpbtt = xxpbtt;
       DSxxchkh = xxchkh;
       DSxkremh = xkremh;
       DSxxdirh = xxdirh;
       DSxxdirn = xxdirn;
       DSxxamt  = xxamt ;
       DSxxtotl = xxtotl;
       //*** Beg Add ***** 01/17/25 *******************************
       DSDRNAME = OFPRNM;
       DSDRPHN# = XPDIRN;
       If (ICPolicyOpt(EAco#:EAfnd:EAemp#) = 'Y');
          DSCHKPYBL = OF3rdVrb;
       Else;
          DSCHKPYBL = 'Please make checks payable to:  ' + %Trim(xxchec);
       Endif;
       //*** End Add ***** 01/17/25 *******************************
       Write pds01;
       Close WxSumInv;
      /end-free
     p WrtSumInvXML    e

      //===============================================================
      // Write Scriptura Workfile - Summary Deductible Invoice
      //===============================================================
     p WrtSumInv1XML   b
      /free
       //*** Beg Chg ***** 01/17/25 *******************************
       //***DSxmlkey = xmlFile;
       DSxmlkey = XPxmlT;
       //*** End Chg ***** 01/17/25 *******************************
       DSxxco#  = xxco# ;
       DSxxfnd  = xxfnd ;
       DSxnname = xnname;
       DSsvemp# = svemp#;
       DSxkdivh = xkdivh;
       DSxkdiv# = xkdiv#;
       DSxkdbnm = xkdbnm;
       DSxkamtd = xkamtd;
       Write pds02;
      /end-free
     p WrtSumInv1XML   e
      *==============================================================
** WTL {Weight}
3713713713713713713713713713713713713713713713
** MN
January
February
March
April
May
June
July
August
September
October
November
December
