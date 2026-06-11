     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('W7936R')
     H bnddir('WTBNDDIR')
     H/title W7936R - Deductible Detail Report
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*   THIS PROGRAM WILL LIST ALL CLAIMS FOR EMPLOYERS WHO ARE     *
     H*   UNDER THE REIMBURSABLE DEDUCTIBLE PLAN.  INFORMATION        *
     H*   PRODUCED ON THE DETAIL LEVEL FOR EACH CLAIM INCLUDES:       *
     H*   FUND YEAR, CLAIM, CLAIMANT NAME, REPORT DATE, SS#,          *
     H*   OCCUPATION CODE, ALL INCURRED AND PAID AMOUNTS, CLAIM       *
     H*   STATUS AND REIMBURSABLE DEDUCTIBLE BILLED AMOUNTS.  IN      *
     H*   ADDITION, SUMMARY INFORMATION IS PRODUCED FOR EACH FUND     *
     H*   YEAR AND ALL FUND YEARS.  SUMMARY INFORMATION PRODUCED      *
     H*   INCLUDES: FUND YEAR, PER OCCURRENCE AND AGGREGATE LIMITS    *
     H*   AS SET UP ON THE EMPLOYER DETAIL LEVEL AND DEDUCTIBLE       *
     H*   BILLED AMOUNTS.                                             *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W7936C - DEDUCTIBLE INVOICE                                 *
     H*   W7936C1 - DEDUCTIBLE DETAIL REPORT                          *
     H*   W7936C2 - RESOLVE TO DARTS COPY FROM DEDUCTIBLE INVOICE     *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER: C:24                                              *
     H* CREATED: 02/28/95               PROJECT: WC311                *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*   *IN60 - GENERAL CHAIN, DOWEQ                                *
     H**      08      Batch Mode                                     **
     H**      09      Demand Mode                                    **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/20/25 C:300 Project: WC2494                                *
     H*                Incorporate scriptura.                         *
     H*                Cleanup-Remove all prior comments & Commented code
     H*                Read WDF1L file correctly                      *
     H*                Remove / separator from all date fields        *
     H*                                                               *
     H* 06/09/20 C:33  Project: SY420
     H*                Mask SSN
     H*
     H* 08/26/16 C:136 Support: 43100                                 *
     H*                Move total logic in printDetail to beginning   *
     H*                of procedure so that all claim activity will   *
     H*                print for single employer run. Also cleaned    *
     H*                out comments.
     H*                                                               *
     H* 02/17/16 C:136 Project: SY371                                 *
     H*                Scriptura changes                              *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 02/11/15  C:295 Support: 32944                                *
     H*                 Add "As of date" to single employer reports.  *
     H*                                                               *
     H* 09/30/14  C:33  Expand doc parm for ISEWMD.                   *
     H*                                                               *
     H* 04/02/14  C:295 Project: 114WMV                               *
     H*                 Add fields to Deductible Detail report.       *
     H*                 - Losses As of Date                           *
     H*                 - A/R Posting Date                            *
     H*                 - Deductible Billing Type                     *
     H*                 - Exclude Policies with no Activity           *
     H*                                                               *
     H* 01/10/14 C:180 Support 84545                                  *
     H*                - Fix error causing records to be omitted.     *
     H*                - Override run type for KRMS: Billed claims    *
     H*                  only.                                        *
     H*                                                               *
     H* 09/16/13 C:279 Support: WA146 83410                           *
     H*                Modified to list correct claim status.         *
     H*                                                               *
     H* 05/01/13 C:65  Support: WA146 82241                           *
     H*                Make sure getBilledAmt returns a value.        *
     H*                                                               *
     H* 02/12/13 C:65  Support: CSI Redmine #2321 (Change #80472)     *
     H*                Support change 80472 is to be made TASB only.  *
     H*                The prior functionality is appropriate for     *
     H*                base.                                          *
     H*                                                               *
     H* 10/10/12 C:136 Support: SY195                                 *
     H*                Update call for ISEWMD                         *
     H*                                                               *
     H* 08/08/12 C:062 Support: WA025 80472                           *
     H*                Fix Single Emp run to include claims that      *
     H*                have current billed amounts or previous        *
     H*                billed amounts.                                *
     H*                                                               *
     H* 07/10/12 C:136 Support: WA114 80330                           *
     H*                Fix Single Emp run to include claims that      *
     H*                have current billed amounts.                   *
     H*                                                               *
     H* 04/05/12 C:136 Support: WA071 79409                           *
     H*               -Correct totals and selection logic for ECM     *
     H*               -If loss date = 0, use month end prior to       *
     H*                A/R Post Date                                  *
     H*                                                               *
     H* 03/29/12 C:136 Support: WA114 77875                           *
     H*                Further enhancement for billed/non-billed      *
     H*                selection                                      *
     H*                                                               *
     H* 03/28/12 C:53  Support: WA025 79490                           *
     H*                Reimplement 04/15/11 fix                       *
     H*                                                               *
     H* 03/07/12 C:255 Support: WA099 79503 (Xref- 79409)             *
     H*                Adjusted position for Total Claim#,            *
     H*                Current billed, current fund paid,             *
     H*                total current paid,total claim#.               *
     H*                                                               *
     H* 09/30/11 C:136 Support: WA114 77875 (114WRM)                  *
     H*               -Rewrite SRFND for 114WRM to include claims     *
     H*                with non-billed amounts                        *
     H*               -Rework 71WEN to fit into 114WRM                *
     H*               -Add data area W7936CTL to control display of   *
     H*                report                                         *
     H*               -Add total paid and total non-billed columns    *
     H*                                                               *
     H* 08/02/11 C:061 Project WC2003                                 *
     H*                Print SSN information based upon new           *
     H*                parameter.                                     *
     H*                                                               *
     H* 07/06/11 C:136 Support: WA071 77290                           *
     H*                Client specific: correct detail and total      *
     H*                paid amounts                                   *
     H*                                                               *
     H* 05/31/11  C:062 Project: 76858                                *
     H*                 Correct total paid column.                    *
     H*                                                               *
     H* 05/10/11  C:062 Project: 75944                                *
     H*                 Only apply net recoveries if policy is net    *
     H*                                                               *
     H* 04/25/11  C:062 Project: 76654                                *
     H*                 Populate plan on first page.                  *
     H*                                                               *
     H* 04/15/11  C:191 Project: 114WRM  Priss #23                    *
     H*                - Increase array sizes!                        *
     H*                                                               *
     H* 04/15/11  C:062 Support: 76568                                *
     H*                 Condition 10/01 changes to TASB               *
     H*                                                               *
     H* 03/23/11  C:191 Project: 114WRM  Priss #22                    *
     H*                -Report employers with no current billing      *
     H*                -Expand ASO array size, CSI has more than 50   *
     H*                 policy years on several groups!!! Expanded    *
     H*                 to 200 policy years                           *
     H*                                                               *
     H* 01/05/11 C:136 Support: WA071 75370                           *
     H*                Client specific - don't check for current      *
     H*                billed info when called for a single employer. *
     H*                                                               *
     H* 10/18/10  C:33  Project: WC1940  Priss #2                     *
     H*                -Allow for more groups to be selected than 256.*
     H*                                                               *
     H* 10/01/10  C:246 Support:72450                                 *
     H*                -Stop printing blanks pages on the report.     *
     H*                                                               *
     H* 09/20/10  C:33  Project: SY190   Priss# 172                   *
     H*                -Clear group totals when group changes.        *
     H*                                                               *
     H* 08/17/10  C:62  Project: 73948                                *
     H*                 -Correct spelling.                            *
     H*                 -Exclude if no Ded Current Amount Billed.     *
     H*                 -Condition Deductible Amount.                 *
     H*                 -Correct totals.                              *
     H*                                                               *
     H* 07/06/10  C:62  Project: 73447                                *
     H*                 -Correct overflow.                            *
     H*                 -Group select change                          *
     H*                                                               *
     H* 05/12/10  C:33  Project: WC1838  Seq# 72625                   *
     H*                -Break and total at each year.                 *
     H*                                                               *
     H* 05/06/10  C:33  Project: WC1838  Seq# 72562/72625             *
     H*                -When creating the resolve to DARTS copy, only *
     H*                  produce years with current activity.         *
     H*                -Ensure report breaks correctly.               *
     H*                                                               *
     H* 04/16/10  C:62  Project: 25WQW  Priss# 24                     *
     H*                 Correct darts resolve.                        *
     H*                                                               *
     H* 04/14/10  C:62  Project: 25WQW  Priss# 23                     *
     H*                 Expand length of xyamtperocc                  *
     H*                                                               *
     H* 03/15/10  C:33  Project: 25WRB  Priss# 14                     *
     H*                -Update Groups Select on output routing page.  *
     H*                -When type of claim not passed in, 'Billed'    *
     H*                  claims should be selected.                   *
     H*                                                               *
     H* 03/14/10  C:33  Project: 25WRB  Priss# 15                     *
     H*                -Update fields when record not found.          *
     H*                                                               *
     H* 03/12/10  C:62  Project: 25WRB  PRISS 13                      *
     H*                 Expand the size of work file.                 *
     H*                                                               *
     H* 02/05/10  C:65  Project: 25WRB                                *
     H*                -Add parameters to allow program to be called  *
     H*                 in single employer mode, along with a DARTS   *
     H*                 remark to resolve the single report to DARTS. *
     H*                                                               *
     H* 02/01/10  C:33  Project: 25WRB                                *
     H*                -Change to use group select processing         *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*                  Retrieve brand Name                          *
     H*                                                               *
     H* 01/11/10  C:33  Project: WC1838                               *
     H*                 Process Large & Small deductibles             *
     H*                                                               *
     H* 08/09/09 C:220 Project: 114WRM                                *
     H*          C:136 LARGE DEDUCTIBLE REPORT - PASS NEW PARAMETER & *
     H*                MODIFY THE REPORT TO USE THE GROUP SELECTIONS  *
     H*                                                               *
     H* 10/11/07 C:190 SUPPORT WA099 54504                            *
     H*                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
     H*                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
     H*                                                               *
     H* 09/24/07  C:190 Project 135WKC                                *
     H*                 Add new parameters to entry parm list.        *
     H*                                                               *
     H* 09/29/06 C:132 PROJECT 93WGL                                  *
     H*              - Add parm for excluding policies w/no activity. *
     H*              - Add code to calculate the deductible A/R       *
     H*                balance in WMEAP                               *
     H*              - Add code to calculate if there is a current    *
     H*                billed amount.                                 *
     H*              - If new exclude parm is "Y", there is no deduct *
     H*                amount & no current billed amt, do not print   *
     H*                                                               *
     H* 09/27/06 C:15  Project: 71WHI                                 *
     H*                Added code to implement 71WEE and 71WEN.       *
     H*                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
     H*                                                               *
     H* 09/26/05 C:61  Project: 135WED WC1336                         *
     H*              - Process for Group Range.                       *
     H*              - Clear Group Totals after printing.             *
     H*              - Add Report Totals to print if group range      *
     H*                specified.                                     *
     H*                                                               *
     H* 06/20/05 C:15  Project: 135WDM                                *
     H*                Added code to write data to work file WFDDP.   *
     H*                                                               *
     H* 02/16/04 C:15  Project: WC311                                 *
     H*                While testing 126WCD, I found that policy      *
     H*                years are skipped if ELDCDE is blank, even if  *
     H*                a WDDDP record existed for that year.  This    *
     H*                is only a problem if a ded code is added to a  *
     H*                policy then removed later after a billing has  *
     H*                occurred.                                      *
     H*                                                               *
     H* 01/31/03 C:15  Project:WC577                                  *
     H*                Fixed code that retrieves the deduct code to   *
     H*                use the deduct eff dt.                         *
     H*                                                               *
     H* 01/29/03 C:15  Project: WC1072                                *
     H*                Added code to identify multiple claims on the  *
     H*                report.                                        *
     H*                                                               *
     H* 04/10/00 C:118 PROJECT: WC987                                 *
     H*                LOAD ALTERNATE COMPANY NAME FROM FUND MASTER   *
     H*                                                               *
     H* 12/07/99 C:15 Support: WC093/20911                            *
     H*               Added code to prevent bad payment records from  *
     H*               being added to the incurred and paid amts on    *
     H*               the report.                                     *
     H*                                                               *
     H* 08/25/99 C:90 PROJECT: WC902                                  *
     H*               PRINT 'PER CLAIM' OR 'PER OCCURRENCE' BASED ON  *
     H*               THE VALUE OF DDVERB IN WTDDP.                   *
     H*                                                               *
     H* 11/04/98 C:65 PROJECT: WC492                                  *
     H*               ADD DEDUCTIBLE RECOVERY BACK INTO TOTAL INCUR'D *
     H*                                                               *
     H* 06/30/98 C:65 PROJECT: WC492                                  *
     H*               ADD DEDUCTIBLE RECOVERY BACK INTO TOTAL PAID.   *
     H*                                                               *
     H* 06/04/98 C:65 PROJECT: WC670                                  *
     H*               EXCEPT DETAIL EVEN IF PRIOR/CURRENT BILLED = 0. *
     H*                                                               *
     H* 05/28/98 C:65 PROJECT: WC670                                  *
     H*               CHANGE FUND TO GROUP ON THE REPORT.             *
     H*                                                               *
     H* 04/07/98 C:65 PROJECT: WC670                                  *
     H*               CHANGE REPORT/INVOICE DATES TO XX/XX/XXXX FORM. *
     H*                                                               *
     H* 03/27/98 C:15 Support: WC091/#10869                           *
     H*               Added code to rename WTDDP/DDFYR to prevent it  *
     H*               from over writing WDDDP/DDFYR.                  *
     H*                                                               *
     H* 11/25/96 C:47 PROJECT: WC600                                  *
     H*               EXCLUDE EMPLOYERS WHO ARE NOT ON A              *
     H*               DEDUCTIBLE PLAN.                                *
     H*                                                               *
     H* 10/10/95 C:41 PROJ: *NONE                                     *
     H*               1. ALLOW USER OR ROUTING INFO TO PRINT.         *
     H*                                                               *
     H* 09/22/95 C:24 PROJECT: CM005                                  *
     H*               CALCULATE THE BILLING FUND YEAR SEPARATELY FOR  *
     H*               EACH EMPLOYER.  BILL THE FUND YEAR OF THE POLICY*
     H*               ACTIVE AS OF THE POSTING DATE, OR THE LAST      *
     H*               ACTIVE POLICY IF NO CURRENT POLICY IS FOUND.    *
     H*****************************************************************
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     fwmcml     if   e           k disk    rename(pcm01 : pcm0l)
     FWMCDL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     Fwmchl5    if   e           k disk
     FWDDDL1    IF   E           K DISK
     F                                     infds(ddFDS1)
     FWDDDL2    IF   E           K DISK
     F                                     RENAME(PDD01:PDD02)
     f                                     prefix(l2)
     FWMEAL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PSY01)
     FWDF1L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
      *** Beg Add ***** 01/20/25  **********************************
     F                                     infds(dbfds)
     FSTSP1L    IF   E           K DISK    rename(psp011:psp010)
      *** End Add ***** 01/20/25  **********************************
     FWTDDL2    IF   E           K DISK
     F                                     RENAME(PDD01:PDD0T)
     FWDPAL     IF   E           K DISK
     FWAMCL5    IF   E           K DISK
     FWFD1P     O    E             DISK
     FWXGSL     IF   E           K DISK
     Fwmldl1    if   e           k disk
     Fwmrhl     if   e           k disk
     fwfd2p     o    e             disk    rename(pd201:pd20p) usropn
     fwfd2l     if   e           k disk    rename(pd201:pd20l) usropn
     Fssylf     if   e           k disk     ignore(sbc01:scs01:src01:spc01)
     FSDCMLD    IF   E           K DISK    RENAME(PCM01:PCM02)
     F                                     PREFIX(C2)
     fwxdd0p    o    e             disk    usropn prefix(xml)                   xml rpt hdr
     fwxdd1p    o    e             disk    usropn prefix(xml)                   xml fnd hdr
     fwxdd2p    o    e             disk    usropn prefix(xml)                   xml emp hdr
     fwxdd3p    o    e             disk    usropn prefix(xml)                   xml emp dtl
     fwxdd4p    o    e             disk    usropn prefix(xml)                   xml emp tot by fyr
     fwxdd5p    o    e             disk    usropn prefix(xml)                   xml emp tot
     fwxdd6p    o    e             disk    usropn prefix(xml)                   xml multiclaim e
     fwxdd7p    o    e             disk    usropn prefix(xml)                   xml grp tot file
     fwxdd8p    o    e             disk    usropn prefix(xml)                   xml rpt tot file
     fxmlprel1  uf   e           k disk    usropn rename(xp100:xp101)           xml prep file
     FQSYSPRT   O    F  215        PRINTER OFLIND(*INOF)
     F                                     USROPN
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN
     d xxcnam          s             30
     d xxacdt          s              8  0
     d xxdtrp          s              8  0
     d xxdbpv          s             11  2
     d xxdbcu          s             11  2
     d xxpfpd          s             11  2
     d xxcfpd          s             11  2
     d xxtlpd          s             11  2
     d xxwkrr          s             11  2
     d prcFyr          s                   like(ddfyr)
     d prcEmp#         s                   like(ddemp#)
     d prcDiv          s                   like(dddiv)
     d prcCase         s                   like(ddcase)
     d include         s               n
     d xxtype          s              1
     d xxGrpSel        s               n
     D singleEmpRun    s               n
     D prtIndvYrTot    s               n
     d processYr       s               n
     d printYr         s               n
     d printFoot       s               n
     d processEmp      s               n
     d largeVsSmall    s               n
     d largeDed        s               n
     d smallDed        s               n
     d useLargeDed     s               n
     d useSmallDed     s               n
     d xpDDTp          s             50
     d x2DDTp          s             20
     d xxDedEffDt      s               d   datfmt(*usa)
     d xxGrossNet      s              5
     d xxOccClm        s             10
     d xyAmtAgg        s                   like(LDAMG1)
     d xyAmtPerOcc     s                   like(LDAMP1)
     d xyDedEffDt      s                   like(ELDCEF)
     d xyGrossNet      s                   like(DDTYPE)
     d xyOccClm        s                   like(DDVERB)
     d xxrec           s              1
     d xxfrec          s              1
     d xxar01          s              9  2
     d xxar02          s              9  2
     d xxar03          s              9  2
     d xxe#cl          s              8  0
     d xxemdp          s             11  2
     d xxemdc          s             11  2
     d xxemdt          s             11  2
     d xxemtn          s             11  2
     d xxemtd          s             11  2
     d xxempp          s             11  2
     d xxemcp          s             11  2
     d xxdbal          s             11  2
     d xxnonb          s             11  2
     d xxt#cl          s              8  0
     d xxtldp          s             11  2
     d xxtldc          s             11  2
     d xxtldt          s             11  2
     d xxtltn          s             11  2
     d xxtltd          s             11  2
     d xxf#cl          s              8  0
     d xxfydp          s             11  2
     d xxfydc          s             11  2
     d xxfydt          s             11  2
     d xxfytn          s             11  2
     d xxfytd          s             11  2
     d xxtlpp          s             11  2
     d xxtlcp          s             11  2
     d xxfypp          s             11  2
     d xxfycp          s             11  2
     d xxtcup          s             11  2
     d xxycup          s             11  2
     d xxttpd          s             11  2
     d xxtlfp          s             11  2
     d xxfyfp          s             11  2
     d xxtfpd          s             11  2
     d xxlldt          s              8  0
     d xxexcl          s                   like(QQEXCL)
     d isUseScriptura  s              1n   inz
     d isUseSpoolOut   s              1n   inz
     d isUseXML        s              1n   inz
     d isUse3rdParty   s              1n   inz
     d outCo#          s              3  0 inz
     d outFnd          s              3  0 inz
     d outFldrPrefix   s              2    inz
     d outPreString    s             10    inz
     d outReturn       s            250    inz
     d outXMLFile      s             61    inz
     d processType     s            100    inz
     d processTypeD    s            100    inz
     d processTypeR    s            100    inz
     d xmlFile         s             60    inz
     d/copy sPrModule
     d/copy sPrXML

      *** Beg Add ***** 01/20/25  **********************************
     d isMassReq       s              1n   inz
     d massFlag        s              1    inz(*blanks)
     d xmlFileBlank    s                   like(XPXML)
     d xmlLink         s                   like(XPLink)
     d xmlFlowName     s                   like(XPXML)
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     d x#RRN           s              9  0 inz(0)
     d sysdt           s              7  0 inz(0)
     d xxPre           s              2    inz('DD')
     d xsFyr           s              3  0 Inz(0)

     dsvLDA            ds          1024    dtaara(*lda)
     d ldEnd#                         5  0 overlay(svlda:1)
     d ldEndA                         5    overlay(svlda:1)
     d keyEmail                      40    overlay(svLDA:56)
     d keySeq#                        5    overlay(svLDA:96)
     d ldMassFile                    60    Overlay(svlda:129)
     d ldOutputFile                  60    Overlay(svlda:191)
     d xxDart2                        1    overlay(svlda:251)
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

     D/copy sPrJobInfo
      *** End Add ***** 01/20/25  **********************************

     D/copy sPrMask

     d                 ds
     d rhcod                   1     20    dim(10)
     d  RHRT01                 1      2
     d  RHRT02                 3      4
     d  RHRT03                 5      6
     d  RHRT04                 7      8
     d  RHRT05                 9     10
     d  RHRT06                11     12
     d  RHRT07                13     14
     d  RHRT08                15     16
     d  RHRT09                17     18
     d  RHRT10                19     20

     d                 ds
     d rhamt                   1     90  2 dim(10)
     d  RHSU01                 1      9  2
     d  RHSU02                10     18  2
     d  RHSU03                19     27  2
     d  RHSU04                28     36  2
     d  RHSU05                37     45  2
     d  RHSU06                46     54  2
     d  RHSU07                55     63  2
     d  RHSU08                64     72  2
     d  RHSU09                73     81  2
     d  RHSU10                82     90  2

     d rptCtlFlags     ds            10    dtaara(W7936CTL)
     d  ctlFlag1                      1
     d  ctlFlag2                      1
     d  ctlFlag3                      1
     d  ctlFlag4                      1
     d  ctlFlag5                      1
     d  ctlFlag6                      1
     d  ctlFlag7                      1
     d  ctlFlag8                      1
     d  ctlFlag9                      1
     d  ctlFlag10                     1

     d breakByYr       s               n
     d mc#             s                   like(MCCLM#) dim(9999)
     d mct             s             13  2 dim(9999)
     d cs#             s                   like(CMCASE) dim(9999)
     d cst             s             13  2 dim(9999)
     d csb             s             13  2 dim(9999)
     d csn             s             13  2 dim(9999)

     D NM              S              1    DIM(56)
     D FY              S              3  0 DIM(200)
     D ed              s               d   DIM(200)
     D oc              s             10    DIM(200)
     D gn              s              5    DIM(200)
     D sl              s              1    DIM(200)
     D PO              s                   DIM(200) like(LDAMP1)
     D AG              s                   DIM(200) like(LDAMG1)
     D ASO             s               n   DIM(200)
     D TC              S              8  0 DIM(200)
     D DP              S             11  2 DIM(200)
     D DC              S             11  2 DIM(200)
     D DT              S             11  2 DIM(200)
     D PP              S             11  2 DIM(200)
     D CP              S             11  2 DIM(200)
     D TP              S             11  2 DIM(200)
     D TD              S             11  2 DIM(200)
     D TN              S             11  2 DIM(200)
     d wrkDate         s               d
     D maxFNDs         c                   315
     D fnd             s              5    dim(maxFNDs)
     d allvbg          c                   const('- Both billed and non -
     d                                     billed claims')
     d bilvbg          c                   const('- Billed claims')
     d nonvbg          c                   const('- Non-billed claims')
     d x0Title         c                   const('Deductible Detail Report')
     DddFDS1           ds
     D ddRRN1                397    400b 0
      *** Beg Add ***** 01/20/25 ***************************************
     D dbfds           ds
     D  dbrrn                397    400b 0
      *** End Add ***** 01/20/25 ***************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  CMSS#                  1      9
     D  XXSS1                  1      3
     D  XXSS2                  4      5
     D  XXSS3                  6      9
     D                 DS
     D  XXFRDT                 1      7  0
     D  DD                     6      7  0
     D XAFND           DS
     D  XAFND1                 1    175
     D  XAFND2               176    350
     D  XAFND3               351    525
     D  XAFND4               526    700
     D  XAFND5               701    875
     D  XAFND6               876   1050
     D  XAFND7              1051   1225
     D  XAFND8              1226   1400
     D  XAFND9              1401   1575
      *
      * Deductible table reserve type codes
      *
     d                 ds
     d  rtc                    1     20
     d                                     dim(10)
     d  DDDC01                 1      2
     d  DDDC02                 3      4
     d  DDDC03                 5      6
     d  DDDC04                 7      8
     d  DDDC05                 9     10
     d  DDDC06                11     12
     d  DDDC07                13     14
     d  DDDC08                15     16
     d  DDDC09                17     18
     d  DDDC10                19     20
     d                 ds
     d  rtcLrg1                1     20    dim(10)
     d  LDR101                 1      2
     d  LDR102                 3      4
     d  LDR103                 5      6
     d  LDR104                 7      8
     d  LDR105                 9     10
     d  LDR106                11     12
     d  LDR107                13     14
     d  LDR108                15     16
     d  LDR109                17     18
     d  LDR110                19     20
     d                 ds
     d  rtcLrg2                1     20    dim(10)
     d  LDR201                 1      2
     d  LDR202                 3      4
     d  LDR203                 5      6
     d  LDR204                 7      8
     d  LDR205                 9     10
     d  LDR206                11     12
     d  LDR207                13     14
     d  LDR208                15     16
     d  LDR209                17     18
     d  LDR210                19     20
     D                 DS
     D  l7rt                   1     20
     D                                     DIM(10)
     D  l7CMRT01               1      2
     D  l7CMRT02               3      4
     D  l7CMRT03               5      6
     D  l7CMRT04               7      8
     D  l7CMRT05               9     10
     D  l7CMRT06              11     12
     D  l7CMRT07              13     14
     D  l7CMRT08              15     16
     D  l7CMRT09              17     18
     D  l7CMRT10              19     20
     D                 DS
     D  l7ar                   1     90  2
     D                                     DIM(10)
     D  l7CMAR01               1      9  2
     D  l7CMAR02              10     18  2
     D  l7CMAR03              19     27  2
     D  l7CMAR04              28     36  2
     D  l7CMAR05              37     45  2
     D  l7CMAR06              46     54  2
     D  l7CMAR07              55     63  2
     D  l7CMAR08              64     72  2
     D  l7CMAR09              73     81  2
     D  l7CMAR10              82     90  2

     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *=========================================================================
      * Procedure Prototypes
      *=========================================================================
     d/copy sPrString

     d wrtWrkFile      pr
     d initFndFld      pr
     d getHdrInfo      pr
     d setYearArr      pr
     d initPolFlds     pr
     d initClmFlds     pr
     d getDedTblInf    pr
     d excludPolicy    pr              n
     d dedTypeMatch    pr              n
     d getDedBilld     pr
     d getClmIncrd     pr
     d getClmPaid      pr
     d printDetail     pr
     d printFyrTtl     pr
     d printGrpTtl     pr
     d getRateDate     pr                  extpgm('SREFFD')
     d  co#                           3
     d  fnd                           3
     d  fyr                           3
     d  emp#                          9
     d  div                           5
     d  rateDate                      7
     d wrtDedWrkFil    pr
     d calcNonBillA    pr
     d payValue        pr                  like(PAAMT)
     d  paymentAmt                         like(PAAMT)
     d  paymentType                        like(PATYPE)
     d getBilledAmt    pr                  like(PAAMT)
     d  paymentAmt                         like(PAAMT)
     d  prvTotalAmt                  13  2
     d  curTotalAmt                  13  2
     d  limitAmt                           like(LDAMP1)
     d UpdateXmlPrep   pr
     d WrtEmpHdr       pr
     d WrtFndHdr       pr
     d WrtEmpDtl       pr
     d WrtFyrDtl       pr
     d WrtEmpTtl       pr
     d WrtMltClm       pr
     d WrtFndTtl       pr
     d WrtRptHdr       pr
     d WrtRptTtl       pr
     d WrtScriptDoc    pr                  extpgm('S4401C')
     d  outXMLString                 61
      *** Beg Add ***** 01/20/25  **********************************
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
      *** End Add ***** 01/20/25  **********************************
     IPDD0T
     I              DDFYR                       D1FYR
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCD              3
     C*
     C*   KEY LIST FOR THE COMPANY FILE
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
     C*   KEY LIST FOR THE OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    DDCO#
     C                   KFLD                    DDFND
     C                   KFLD                    SVFYR
     C                   KFLD                    SVCASE
     C     CMKEY1        KLIST
     C                   KFLD                    DDCO#
     C                   KFLD                    DDFND
     C                   KFLD                    S1FYR
     C                   KFLD                    S1CASE
     C     CHKEY1        KLIST
     C                   KFLD                    DDCO#
     C                   KFLD                    DDFND
     C                   KFLD                    S1FYR
     C                   KFLD                    S1CASE
     C     MCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    S1FYR
     C                   KFLD                    XACASE

     C     cmky2         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    svemp#
     C                   kfld                    svdiv
     C                   kfld                    xxefyr

     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE CLAIM HISTORY FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    DDCO#
     C                   KFLD                    DDFND
     C                   KFLD                    SVFYR
     C                   KFLD                    SVCASE
     C*
     C*  KEY LIST FOR THE DEDUCTIBLE DETAIL FILE
     C*
     C     DDKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    DDFND
     C                   KFLD                    DDEMP#
     C                   KFLD                    DDDIV
     C*
     C     DDKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    DDFND
     C                   KFLD                    X2PODT
     C                   KFLD                    DDEMP#
     C                   KFLD                    DDDIV
     C*
     C     DDKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XSEMP#
     C                   KFLD                    XSDIV
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*   KEY LIST FOR THE DARTS FILE
     C*
     C     D1KY          KLIST
     C                   KFLD                    X1KEY            40
     C                   KFLD                    XXDART
     C*
     C*  Key List for the Group Detail file.
     C*
     C     F1KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    DDCO#
     C                   KFLD                    DDFND
     C                   KFLD                    DDEMP#
     C                   KFLD                    DDDIV
     C                   KFLD                    DDFYR
     C*
     C*  KEY LIST FOR THE DEDUCTIBLE DISCOUNT CODE FILE
     C*
     C     DDKYT         KLIST
     C                   KFLD                    DDCO#
     C                   KFLD                    DDFND
     C                   KFLD                    XXST
     C                   KFLD                    ELDCDE
     C*
     C     DDKYT2        KLIST
     C                   KFLD                    DDCO#
     C                   KFLD                    DDFND
     C                   KFLD                    XXST
     C                   KFLD                    ELDCDE
     C                   KFLD                    XXDT              7 0
     C*
     C*  Key list for the Reinsurance Multiple Claim File
     C*
     C     MCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVFYR
     C                   KFLD                    XACASE
     C*
     C     MCKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C     EMKEY1        KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    S1EMP#
     C                   KFLD                    S1DIV
     C*
     C     CMKEY3        KLIST
     C                   KFLD                    DDCO#
     C                   KFLD                    DDFND
     C                   KFLD                    DDEMP#
     C                   KFLD                    DDDIV
     C                   KFLD                    DDFYR
     C*
     C     WDKEY5        KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     ELKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C     DDKYT4        KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    XXST
     C                   KFLD                    ELDCDE
     C                   KFLD                    XXDT
     C*
     C     DDKYT5        KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    XXST
     C                   KFLD                    ELDCDE
     C*
     C     DDKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C     DDKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    X2PODT
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*          EMKEY1    KLIST
     C*                    KFLD           XXCO#
     C*                    KFLD           XXFND
     C*                    KFLD           S1EMP#
     C*                    KFLD           S1DIV
     C*
     C*
     C*  INITIALIZE ALL PROGRAM DEFINED VARIABLES
     C*
     C     *LIKE         DEFINE    DDCO#         XXCO#
     C     *LIKE         DEFINE    DDFND         XXFND
     C     *LIKE         DEFINE    DDFND         XXTFND
     C     *LIKE         DEFINE    DDST          XXST
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    RTKEY         XXKEY
     C     *LIKE         DEFINE    DDBFYR        XXEFYR
     C     *LIKE         DEFINE    LDAMP1        XXAMPO
     C     *LIKE         DEFINE    LDAMG1        XXAMAG
     C     *LIKE         DEFINE    DDFYR         SVFYR
     C     *LIKE         DEFINE    DDEMP#        SVEMP#
     C     *LIKE         DEFINE    DDEMP#        S1EMP#
     C     *LIKE         DEFINE    DDFYR         S1FYR
     C     *LIKE         DEFINE    DDDIV         S1DIV
     C     *LIKE         DEFINE    DDCASE        S1CASE
     C     *LIKE         DEFINE    CMAR01        XPDCO@
     C     *LIKE         DEFINE    CMAR01        XINCO@
     C     *LIKE         DEFINE    CMAR01        XPDME@
     C     *LIKE         DEFINE    CMAR01        XINME@
     C     *LIKE         DEFINE    CMAR01        XPDEX@
     C     *LIKE         DEFINE    RTTYPE        RTYPE@
     C     *LIKE         DEFINE    PATYPE        PTYPE@
     C     *LIKE         DEFINE    PAAMT         PAAMT@
     C     *LIKE         DEFINE    XXAMT         XAMT@
     C     *LIKE         DEFINE    PABR2         PABR2@
     C     *LIKE         DEFINE    FNMEP1        FNEP1@
     C     *LIKE         DEFINE    FNCOP1        FNCO1@
     C     *LIKE         DEFINE    FNEXP1        FNEX1@
     C     *LIKE         DEFINE    FNLEP1        FNLE1@
     C     *LIKE         DEFINE    CHOCST        CHOST@
     C     *LIKE         DEFINE    ELRTCD        ELRCD@
     C     *LIKE         DEFINE    RSJRST        RSJST@
     C     *LIKE         DEFINE    ELDCEF        ELDCE@
     C     *LIKE         DEFINE    DDTYPE        DDTYP@
     C     *LIKE         DEFINE    DDAMAG        DDAMA@
     C     *LIKE         DEFINE    DDAMPO        DDAMP@
     C     *LIKE         DEFINE    DDSTAT        DDSTT@
     C     *LIKE         DEFINE    DDDIV         SVDIV
     C     *LIKE         DEFINE    CMAR01        XXPDCO
     C     *LIKE         DEFINE    CMAR01        XXPDME
     C     *LIKE         DEFINE    CMAR01        XXPDEX
     C     *LIKE         DEFINE    CMAR01        XXPDTL
     C     *LIKE         DEFINE    CMAR01        XXINCO
     C     *LIKE         DEFINE    CMAR01        XXINME
     C     *LIKE         DEFINE    CMAR01        XXINEX
     C     *LIKE         DEFINE    CMAR01        XXINTL
     C     *LIKE         DEFINE    FNNAME        XXFNAM
     C     *LIKE         DEFINE    EMNAME        XXENAM
     C     *LIKE         DEFINE    DDPODT        X2PODT
     C     *LIKE         DEFINE    DDCASE        SVCASE
     C     *LIKE         DEFINE    PATRDT        XXLSDT
     C     *LIKE         DEFINE    MCCASE        XACASE
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C                   MOVE      *ZEROS        XACASE
     C     *LIKE         DEFINE    PAAMT         XXAMT
     C*
     C     *LIKE         DEFINE    DDFYR         XXFYR
     C     *LIKE         DEFINE    DDEMP#        XEEMP#
     C     *LIKE         DEFINE    DDDIV         XEDIV
     C     *LIKE         DEFINE    EMNAME        XEENAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQPODT            8
     C                   PARM                    QQLSDT            8
     C                   PARM                    QQTFND            3
     C                   PARM                    QQMODE            1
     C                   PARM                    QQEXCL            1
     C                   PARM                    QQFRGN            3
     C                   PARM                    QQTRGN            3
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQDDTP            1
     c                   PARM                    QQEMP#            9
     c                   PARM                    QQDIV             5
     c                   PARM                    QQFYR             3
     c                   PARM                    QQDART            5
     c                   PARM                    QQY4N             1


     c                   if        q1prms >= 16
     c                   move      qqy4n         xxy4n             1
     c                   else
     c                   move      'Y'           xxy4n
     c                   endif

     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     c
     c     cltid         ifeq      '093'
     c                   move      'B'           qqtype
     c                   endif

      /free
        if  QQlsdt <> *blanks ;
          xxlldt = %Int(QQlsdt);
        endif;

        if  QQexcl = 'Y';
          xxexcl = 'Yes';
        else;
          xxexcl = 'No';
        endif;
      /end-free
     C     *LIKE         DEFINE    QQDDTP        xxDDTp
     C                   eval      xxDDTp = 'S'
     C                   if        %parms >= 11 and QQDDTP <> *blank
     C                   eval      xxDDTp = QQDDTP
     C                   endif
     C*
     C                   eval      useSmallDed = (xxDDTp = 'S'
     C                                         or xxDDTp = 'B')
     C                   eval      useLargeDed = (xxDDTp = 'L'
     C                                         or xxDDTp = 'B')
     C*
     C                   clear                   WCFL43
     C     'WC'          chain     swc01
     C                   eval      largeVsSmall = WCFL43 = 'Y'
     C*
     C                   if        WCFL43 = 'Y'
     C                   select
     C                   when      xxDDTp = 'S'
     C                   eval      x2DdTp = 'S - Small'
     C                   when      xxDDTp = 'L'
     C                   eval      x2DdTp = 'L - Large'
     C                   other
     C                   eval      x2DdTp = '*ALL'
     C                   endsl
     C                   eval      xpDdTp = 'DEDUCTIBLE PLAN: ' + x2DdTp
     C                   endif
     c                   z-add     0             xsemp#            9 0
     c                   z-add     0             xsdiv             5 0
     c                   move      *blanks       xxdart            5
     c
     C                   if        %parms >= 12
     c                   move      qqemp#        xsemp#
     c                   move      qqdiv         xsdiv
     c                   move      qqdart        xxdart
     c                   endif
       //*** Beg Add ***** 01/20/25 **********************************
       If %parms >= 14;
          If qqFyr <> *Blank;
             xsFyr = %Int(qqFyr);
          Endif;
       Endif;
       //*** End Add ***** 01/20/25 **********************************
     C*
     C                   eval      singleEmpRun = xsEmp# <> *zero
     c                   in        rptCtlFlags
     c                   if        ctlFlag1 = *blank
     c                   eval      breakByYr = *on
     c                   endif
     c                   eval      *in51 = singleEmpRun
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
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     UPUSNM        IFNE      *BLANKS
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   END
     C*
     C                   TIME                    DSDTTM
     C     QQFND         IFEQ      *BLANKS
     C                   MOVEL     '000'         QQFND
     C                   ENDIF
     C     QQFRGN        IFEQ      *BLANKS
     C                   MOVEL     '000'         QQFRGN
     C                   ENDIF
     C     QQTRGN        IFEQ      *BLANKS
     C                   MOVEL     '000'         QQTRGN
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C*** Beg Add ***** 01/20/25  **********************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT             7 0
     C*** End Add ***** 01/20/25  **********************************

     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   Z-ADD     0             XXFRGN            3 0
     C                   Z-ADD     0             XXTRGN            3 0
     C     Q1PRMS        IFGE      8
     C                   MOVE      QQFRGN        XXFRGN            3 0
     C                   MOVE      QQTRGN        XXTRGN            3 0
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      5
     C                   MOVE      QQTFND        XXTFND
     C                   ELSE
     C                   Z-ADD     *ZERO         XXTFND
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      6
     C                   MOVE      QQMODE        XXMODE            1
     C                   ELSE
     C                   MOVE      'B'           XXMODE
     C                   ENDIF
     c                   eval      xxType = *blank
     c                   if        Q1PRMS >= 10 and qqType <> *blank
     c                   eval      xxType = qqType
     c                   endif
     c*
      /Free
       //*** Beg Add ***** 01/20/25 *******************************************
       // get WDELP RRN into x#RRN (if called for a single employer)
       If xsemp# <> 0;
          //***Setll elky1 pel01;
          //***Reade elky1 pel01;
          Setll (xxCo#:xxFnd:xsEmp#:xsDiv:xsFyr) pel01;
          Reade (xxCo#:xxFnd:xsEmp#:xsDiv:xsFyr) pel01;
          If not %eof(wdell);
             x#RRN = dbrrn;
          Endif;
       Endif;

       Clear xmlLink;
       Clear xmlFile;
       isMassReq = *On;
       massFlag = 'Y';
       If PgmInStack('W7936C1') = *On;
          // if the program is being called for just the mass detail report
          // (via report reprint CL W7936C1 program)
          isMassReq = *off;
          massFlag = ' ';
       Endif;
       processType = 'Deductible Invoice';          //To request XMLPREP
       processTypeD = 'Deductible Invoice Report';  //To get Doc Id & path from stsp1l
       processTypeR = 'Deductible Detail Report';   //Changed to create this document name in iDarts
       //*** End Add ***** 01/20/25 *******************************************

       isUseSpoolOut  = UseSpoolOut(processType);
       isUseScriptura = UseScriptura(processType);
       isUse3rdParty  = Use3rdXml(processType);
       isUseXML = isUseScriptura or isUse3rdParty;
       outReturn = processType;
       If isUseXML;
          //*** Beg Del ***** 01/20/25 *******************************************
          //***outPreString = 'RDedDtlRpt';
          //***xmlFile = GetXMLFileName(' ':*zeros:outCo#:outFnd:
          //*** outFldrPrefix:outPreString:outReturn);
          //*** End Del ***** 01/20/25 *******************************************
          Open XmlPrel1;
          Open Wxdd0p;
          Open Wxdd1p;
          Open Wxdd2p;
          Open Wxdd3p;
          Open Wxdd4p;
          Open Wxdd5p;
          Open Wxdd6p;
          Open Wxdd7p;
          Open Wxdd8p;
          //*** Beg Add ***** 01/20/25 *******************************************
          // Generate Scriptura records
          Exsr ScriptXml;
          //*** End Add ***** 01/20/25 *******************************************
       Endif;
      /End-free
     C     xxCo#         chain     pgs01
     C                   if        %found
     c                             and xsemp# = 0
     C                   eval      xxGrpSel = *on
     C     xxCo#         setll     pgs01
     C                   endif
     c*
     c                   if        xxType <> '*'
     c                   eval      xxtype = qqtype
     C                   eval      xxGrpSel = *on
     C                   eval      *in34 = *on

     c                   select
     c     xxtype        wheneq    'A'
     c                   movel     allvbg        xxtypd           40
     c     xxtype        wheneq    'B'
     c                   movel     bilvbg        xxtypd           40
     c     xxtype        wheneq    'N'
     c                   movel     nonvbg        xxtypd           40
     c                   other
     c                   endsl
     c                   else
     c                   move      *blank        xxtype
     c                   move      *off          *in34
     c                   endif
     c     xxtype        ifeq      *blank
     c                   move      'B'           xxtype
     c                   movel     bilvbg        xxtypd           40
     c                   endif
     c                   move      qqfnd         x5fnd             3 0
      *
     C                   MOVE      QQPODT        X1PODT            8 0
     C*
     C                   Z-ADD     X1PODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2PODT
     C*
     C                   Z-ADD     X2PODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT            8 0
     C*

      *
      *  qqlsdt will be blank if called from new report (W793R1). Use
      *  month end prior to post date.
      *
     c                   if        qqlsdt = *blank
     c                   move(p)   x2podt        qqlsdt
     c                   move      '01'          qqlsdt
     c     *cymd0        move      qqlsdt        wrkDate
     c                   subdur    1:*days       wrkDate
     c     *usa0         move      wrkDate       qqlsdt
     c                   endif

     C                   MOVE      QQLSDT        X1LSDT            8 0
     C*
     C                   Z-ADD     X1LSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXLSDT
     C*
     C*  Calculate the first day of the "losses as of" month.
     C*
     C                   MOVE      XXLSDT        XXFRDT            7 0
     C                   Z-ADD     1             DD
     C*
     C*  Get Client ID
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C*  Open the appropriate spoolfile.
     C*

      /free
       Select;
       When cltID = '071';
          Open QsysPrt1;
       When isUseSpoolOut;
          Open QsysPrt;
       Endsl;
      /end-free
     C*
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     COKEY         CHAIN     PCO01                              60
      *
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              72
     C                   SELECT
     c     xsemp#        whenne    0
     C     xxGrpSel      WHENEQ    *ON
     C                   eval      *in34 = *on
     C                   CLEAR                   XAFND
     C                   Z-ADD     1             X                 3 0
     C     *LOVAL        SETLL     PGS01
     C                   READ      PGS01                                  60
     C     *IN60         DOWEQ     *OFF
     C     GSIEFL        IFEQ      'I'
     C                   select
     C                   when      x <= maxFNDs
     C                   MOVEL     GSFND         FND(X)
     C                   MOVE      ', '          FND(X)
     C                   ADD       1             X
     C                   other
     C                   endsl
     C                   ENDIF
     C                   READ      PGS01                                  60
     C                   ENDDO
     C                   select
     C                   when      x > maxFNDs
     C                   eval      FND(maxFNDs) = '...  '
     C                   other
     C                   eval      %subst(FND(x-1):4:2) = *blanks
     C                   endsl
     C                   MOVEA     FND           XAFND
     C                   OTHER
     C                   ENDSL

     C                   eval      ORTITL = LeftJstify(ORTITL)
     C                   eval      ORTITL = x0Title
     C                   eval      ORTITL = centerFld(ORTITL)
      *
     C                   Z-ADD     *ZERO         XXR#CL            8 0
     C                   Z-ADD     *ZERO         XXRMDP           11 2
     C                   Z-ADD     *ZERO         XXRMDC           11 2
     C                   Z-ADD     *ZERO         XXRMDT           11 2
     C                   Z-ADD     *ZERO         XXRMTN           11 2
     C                   Z-ADD     *ZERO         XXRMTD           11 2
     C                   Z-ADD     *ZERO         XXRMPP           11 2
     C                   Z-ADD     *ZERO         XXRMCP           11 2
      *
      * No Records processed flag
      *
     C                   MOVE      *OFF          XXREC             1
      *
      * Process Groups
      *
     C                   SELECT
     c     xsemp#        whenne    *zero
     c                   exsr      srfnd
     C     xxGrpSel      WHENEQ    *OFF
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
     C     xxGrpSel      WHENEQ    *ON
     C     *LOVAL        SETLL     PGS01
     C                   READ      PGS01                                  60
     C     *IN60         DOWEQ     *OFF
     C     GSIEFL        IFEQ      'I'
     C                   Z-ADD     *ZEROS        XXCOUN            9 0
     C                   Z-ADD     *ZEROS        XXCOUB            9 0
     C                   Z-ADD     *ZEROS        XXCOUA            9 0
     C                   MOVE      GSFND         XXFND
     C                   EXSR      SRFND
     C                   ENDIF
     C                   READ      PGS01                                  60
     C                   ENDDO
     C                   OTHER
     C                   ENDSL
      *
      *  Print Report Totals
      *

      /free
       If isUseXML;
          WrtRptHdr();
          WrtRptTtl();
          UpdateXMLPrep();
          //*** Beg Add ***** 01/20/25 *******************************
          // Close all work files before calling Scriptura
          Close Wxdd0p;
          Close Wxdd1p;
          Close Wxdd2p;
          Close Wxdd3p;
          Close Wxdd4p;
          Close Wxdd5p;
          Close Wxdd6p;
          Close Wxdd7p;
          Close Wxdd8p;
          //*** End Add ***** 01/20/25 *******************************
          //*** Beg Chg ***** 01/20/25 *******************************
          //*** outXMLFile = xmlFile + '@';
          //*** WrtScriptDoc(outXMLFile); //which calls s4401c as an extproc
          If singleEmpRun = *Off;         //running for Mass Report
             outXMLFile = ldMassFile + '$';
             If PgmInStack('W7936C1') = *On; //if called from reprint report
                outXMLFile = XPxml + '$';
             Endif;
             WrtScriptDoc(outXMLFile); //which calls s4401c as an extproc
          Endif;
          //*** End Chg ***** 01/20/25 *******************************
       Endif;
       Select;
       When Not(isUseSpoolOut);
       When xxRec = *off and cltID = '071';
          Except noRec1;
       When xxRec = *off;
          Except noRecs;
       When singleEmpRun;
       When cltID = '071' and xxTFnd <> 0;
          Except rHead1;
          Except rTot1;
       When xxTFnd <> 0;
          Except rHead;
          Except rTotal;
       Endsl;
       If singleEmpRun;
          Select;
          When xxDart = *Blanks;
          When isUseSpoolOut;
             Close QsysPrt;
             Exsr docu;
          Endsl;
       Else;
          xxPrt = '0';
          Chain orKey por01;
          Select;
          When Not(%Found);
          When Not(isUseSpoolOut);
          When cltID = '071';
             Except route1;
          Other;
             Except route;
          Endsl;
       Endif;
       //*** Beg Del ***** 01/20/25 *******************************
       //*** If isUseXML;
       //***    Close Wxdd0p;
       //***    Close Wxdd1p;
       //***    Close Wxdd2p;
       //***    Close Wxdd3p;
       //***    Close Wxdd4p;
       //***    Close Wxdd5p;
       //***    Close Wxdd6p;
       //***    Close Wxdd7p;
       //***    Close Wxdd8p;
       //*** Endif;
       //*** End Del ***** 01/20/25 *******************************
       *Inlr = *On;
      /end-free
     C*================================================================
     C*  DOCU - SUBROUTINE to resolve report into a document
     C*================================================================
     C     DOCU          BEGSR
     C*
     C                   CALL      'SNXTDCID'
     C                   PARM      *blanks       QQFOLD
     C                   PARM      *blanks       QQDCID
     C                   PARM      'PN'          QQPRE             2
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM      'P'           QQRTCD            1
     C                   PARM      '00000'       QQSUB#            5
     C                   PARM      *BLANKS       QQPFLR           80
     c                   movel     'QSYSPRT'     qqsplf
     C*
     C                   CALL      'SRESOLVE'
     C                   PARM                    QQSPLF           10
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQDCID           12
     C                   PARM      *blanks       QQOUTQ           10
     C*
     C                   EXSR      DARTS
     c
     c                   endsr
     C*================================================================
     C*  DARTS - SUBROUTINE TO WRITE A DARTS RECORD
     C*================================================================
     C     DARTS         BEGSR
     C*
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     XXFND         QCFND
     C                   Z-ADD     XSEMP#        QCEMP#
     C                   Z-ADD     XSDIV         QCDIV
     C*
     C                   MOVE      *BLANK        QCQTE
     C*
     c                   movel     qckey         x1key
     c*
     c     d1ky          setgt     pcm02
     c     d1ky          readpe    pcm02                                  60
     c*
     c                   if        syscif = 'Y' and *in60 = *off
     c
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEL(P)  QQDCID        Q2DCID           40
     C                   MOVEL(P)  QQFOLD        Q#FOLD           60
     c                   movel     c2cmseq#      q#seq#            5
     c                   move      'O'           q#otyp            1
     c                   movel     x0title       q#odsc           40
     c                   move      xxdart        q#rmcd            5
     c                   movel     'AS400'       q#sysn           20
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVEL(P)  Q1USER        Q#USER           10
     c
     c                   call      'ISEWMD'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q2DCID
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#OTYP
     C                   PARM                    Q#ODSC
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SYSN
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#USER
     C                   PARM      ' '           QQRTCD            1
     C                   PARM      *blanks       QQEDSC           80
     C                   PARM      *blanks       QQPATH           80
     C                   PARM      *blanks       QQRPYN            1
     C                   PARM      *blanks       QQUNCN          255
     C                   PARM      *blanks       QQIFSP            1
     c*
     c                   else
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     c                   move      xxdart        q#rmcd            5
     C                   MOVE      *BLANKS       Q#COMM          256
     c
     c                   movel     x0title       q#comm
     c
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  QQDCID        Q#DCID           12
     C                   MOVEL(P)  QQFOLD        Q#FOLD           60
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  *BLANK        Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVE(P)   'N'           Q#SBYL            1
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
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     c
     c                   endif
     c
     C                   ENDSR

      *** Beg Add ***** 01/20/25 *******************************
       //=========================================================================
       //  ScriptXML - Reserve/Update XML record for Scriptura output.
       //=========================================================================
       Begsr ScriptXML;
        //* xmlFile - contains the name of the template XML file, used for work file linking.
        //* xmlFlowName - contains the name of the flow XML file, used for S4401C call.
        //* xmlLink - used to link individual records together in a mass run - XML flow file name.

        xmlFile = ' ';
        message = %trim(processType);
        string = ' ';

        If isMassReq;
           In *Lock *dtaara;
           Out *dtaara;

           If ldMassFile <> ' ';
              xmlFile = ldMassFile;
              xmlLink = xmlFile;

              //* Flow XML file name is used for the S4401C call.
              outXMLFile = xmlFile;
           Endif;
           isMassReq = *off;
        Endif;

        // Create individual entry
        StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x#RRN:'X'):xxPre:
         string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:xmlLink);

        If xmlLink = *Blank;
           Chain (xmlFile) xp101;
        Else;
           Chain (xmlFileBlank:xmlLink:xmlFile) xp101;
        Endif;

        If ldMassFile <> *Blank and XPEmp# <> 0;
           XPCat = 'PS';
           XPData = 'Y';
           Clear XPMail;
           Clear XPMailAdd;
        Else;
           XPCat = 'RP';
        Endif;

        XPType = %trim(processTypeD);
        //* Determine report template information.
        Setgt (XPType:sysDt) psp010;
        Readpe(n) (XPType) psp010;
        If not %eof;
           If S1SCltT <> ' ';
              XPDcid = %trim(S1SCltT);
              XPPath = %trim(S1PathC);
           Else;
              If S1SDefT <> ' ';
                 XPDcid = %trim(S1SDefT);
                 XPPath = %trim(S1PathD);
              Endif;
           Endif;
        Endif;
        XPType = %trim(processTypeR);

        If keyEmail <> *blanks;
           XPemaild = keyEmail;
        Endif;

        Update xp101;

       Endsr;
      *** End Add ***** 01/20/25 *******************************
      *========================================================================
      * SRFND - process group (rewrite)
      *========================================================================
     c     srfnd         begsr

     c     elky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd

     c     elky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xsemp#
     c                   kfld                    xsdiv

     c     cmky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c                   callp     initFndFld
     c                   callp     getHdrInfo
     c                   callp     setYearArr

      * Loop thru WDELP processing deductible policies that meet
      * selection criteria

     c                   select
     c                   when      xsemp# <> 0
     c     elky1         setll     pel01
     c                   other
     c     elky          setll     pel01
     c                   endsl
     c     elky          reade     pel01
     c                   dow       not %eof(wdell)

     c                   callp     initPolFlds

     c                   if        xyDedEffDt  = 0     or
     c                             dedTypeMatch = *off or
     c                             excludPolicy = *on

     c                   select
     c                   when      xsemp# <> 0
     c     elky1         reade     pel01
     c                   other
     c     elky          reade     pel01
     c                   endsl


     c                   callp     printFyrTtl

     c                   iter
     c                   endif

      * Loop thru WMCMP for each selected policy and gather
      * deductible information

     c     cmky          setll     pcm0l
     c     cmky          reade     pcm0l
     c                   dow       not(%eof)
     c                   callp     initClmFlds
     c                   callp     getDedBilld
     c                   callp     getClmIncrd
     c                   callp     getClmPaid
     c                   callp     printDetail
     c     cmky          reade     pcm0l
     c                   enddo

     c                   select
     c                   when      xsemp# <> 0
     c     elky1         reade     pel01
     c                   other
     c     elky          reade     pel01
     c                   endsl

     c                   callp     printFyrTtl

     c                   enddo

     c                   callp     printGrpTtl

     c                   endsr
      * ==============================================================
     OQSYSPRT   E            ROUTE          1 06
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
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE          2 33
     O                                           18 'REPORT SELECTIONS'
     O          E            ROUTE          2
     O                                           21 'COMPANY    :'
     O                       XXCO#         Z     25
     O          E   N34      ROUTE          2
     O                                           21 'GROUP RANGE:'
     O                       X5FND         J     25
     O                                         +001 '-'
     O                       XXTFND        Z   +001
     O          E            ROUTE          2
     O                                           22 'POSTING DATE:'
     O                       XXPODT              32 '  /  /    '
     O          E            ROUTE          2
     O                                           22 'EXCLUDE NO AC'
     O                                         +000 'TIVITY POLICIES:'
     O                       QQEXCL            +002
     O          E            ROUTE          2
     O                       XPDDTP            +009
     O          E    34      ROUTE          2
     O                                           18 'RUN TYPE:'
     O                       XXTYPE            +001
     O                       XXTYPD            +001
     O          E    34      ROUTE          1
     O                                           16 'GROUPS:'
     O                       XAFND1            +001
     O          E    34      ROUTE          1
     O                       XAFND2            +017
     O          E    34      ROUTE          1
     O                       XAFND3            +017
     O          E    34      ROUTE          1
     O                       XAFND4            +017
     O          E    34      ROUTE          1
     O                       XAFND5            +017
     O          E    34      ROUTE          1
     O                       XAFND6            +017
     O          E    34      ROUTE          1
     O                       XAFND7            +017
     O          E    34      ROUTE          1
     O                       XAFND8            +017
     O          E    34      ROUTE          1
     O                       XAFND9            +017
     O          E            ROUTE          1 59
     O                                         +001 '(c)'
     O                       *YEAR             +001
     O                                         +001 'StoneRiver, Inc.'
     O                                         +001 'All rights reserved.'
     O          E            ROUTE          1
     O                                         +001 'StoneRiver is a'
     O                                         +001 'trademark of'
     O                                         +001 'StoneRiver, Inc.'
      * ==============================================================
      *
      * HEADER FOR FULL REPORT
      *
     O          E            BHEAD          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             125
     O                       XXDATE             206 '  /  /    '
     O          E            BHEAD          2
     O                       Q1PGM               10
     O                       ORTITL             135
     O                                          201 'PAGE:'
     O                       PAGE          Z    206
     O          E            BHEAD          2
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                       XXFNAM              71
     O                                           95 'Losses As Of date:'
     O                       XXLLDT             105 '  /  /    '
     O                                          143 'Deductible Billing Type:'
     O                       X2DDTP             164
     O          E            BHEAD          2
     O                                           09 'EMPLOYER:'
     O                       PRCEMP#       Z     19
     O                       PRCDIV        Z     25
     O                       XXENAM              67
     O                                           94 'A/R Posting date:'
     O                       XXPODT             104 '  /  /    '
     O                                          144 'Exclude Policies With No '
     O                                         +000 'Activity:'
     O                       XXEXCL             155
      *
      * HEADER FOR SINGLE EMPLOYER REPORT (*IN 51 DOESN'T WORK ON PAGE KEYWORD)
      *
     O          E            BHEADS         1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             111
     O                       XXDATE             192 '  /  /    '
     O          E            BHEADS         2
     O                       Q1PGM               10
     O                       ORTITL             121
     O                                          187 'PAGE:'
     O                       PAGE          Z    192
     O          E            BHEADS         2
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                       XXFNAM              71
     O                                           95 'Losses As Of date:'
     O                       XXLLDT             105 '  /  /    '
     O                                          143 'Deductible Billing Type:'
     O                       X2DDTP             164
     O          E            BHEADS         2
     O                                           09 'EMPLOYER:'
     O                       PRCEMP#       Z     19
     O                       PRCDIV        Z     25
     O                       XXENAM              67
     O                                           94 'A/R Posting date:'
     O                       XXPODT             104 '  /  /    '
     O                                          144 'Exclude Policies With No '
     O                                         +000 'Activity:'
     O                       XXEXCL             155
     O          E            HEAD           1
     O                                          169 'REIMBURSABLE'
     O                                          187 'DEDUCTIBLE'
     O          E            HEAD           1
     O                                          167 'PREVIOUS'
     O                                          186 'CURRENT'
     O          E            HEAD           1
     O                                           29 'CLAIMANT NAME'
     O                                           68 'ACCIDENT/'
     O                                          166 'AMOUNT'
     O                                          185 'AMOUNT'
     O              n51                         206 'AMOUNT'
     O          E            HEAD           1
     O                                            4 'YEAR'
     O                                           13 'CLAIM #'
     O                                           19 'SSN'
     O                                           56 'PLAN TYPE'
     O                                           69 'REPORT DATE'
     O                                           96 'COMP'
     O                                          113 'MEDICAL'
     O                                          130 'EXPENSES'
     O                                          146 'TOTAL'
     O                                          153 'O/C'
     O                                          166 'BILLED'
     O                                          185 'BILLED'
     O              n51                         206 'NOT BILLED'
     O          E            HEAD           2
     O                                           24 '------------------------'
     O                                           48 '------------------------'
     O                                           72 '------------------------'
     O                                           96 '------------------------'
     O                                          120 '------------------------'
     O                                          144 '------------------------'
     O                                          168 '------------------------'
     O                                          188 '--------------------'
     O              n51                         206 '------------------'
     O          E            DETAIL         1
     O                       prcFyr        Z     03
     O                       prcCase             13
     O              N21                          15 '*'
     O                       XXCNAM              46
     O                       xxGrossNet          54
     O                       XXACDT              68 '  /  /    '
     O                                           80 'INCURRED'
     O                       XXINCO        J     97
     O                       XXINME        J    114
     O                       XXINEX        J    131
     O                       XXINTL        J    147
     O                       CHOCST             152
     O                       XXDBPV        J    170
     O                       XXDBCU        J    188
     O              n51      XXNONB        J    206
     O          E            DETAIL         2
     O                       XXSS1               19
     O                                           20 '-'
     O                       XXSS2               22
     O                                           23 '-'
     O                       XXSS3               27
     O                       XXDTRP              68 '  /  /    '
     O                                           76 'PAID'
     O                       XXPDCO        J     97
     O                       XXPDME        J    114
     O                       XXPDEX        J    131
     O                       XXPDTL        J    147
     O          E            FOOTER           61
     O                                           23 '* - Indicates a claim '
     O                                           41 'that is part of a '
     O                                           64 'multi-claim occurrence.'
     O          E            EHEAD          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             111
     O                       XXDATE             192 '  /  /    '
     O          E            EHEAD          2
     O                       Q1PGM               10
     O                       ORTITL             121
     O                                          187 'PAGE:'
     O                       PAGE          Z    192
     O          E            EHEAD          2
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                       XXFNAM              71
     O          E            EHEAD          2
     O                                           09 'EMPLOYER:'
     O                       prcEmp#       Z     19
     O                       prcDiv        Z     25
     O                       XXENAM              67
     O          E            EHEAD          1
     O                                         +000 '    '
     O                                         +002 '          '
     O                                         +002 '   PER    '
     O                                         +002 '            '
     O                                         +002 '             '
     O                                         +002 '           '
     O                                         +003 '  TOTAL AMOUNT '
     O                                         +003 '  TOTAL AMOUNT '
     O                                         +003 '               '
     O          E            EHEAD          1
     O                                         +000 '    '
     O                                         +002 'DEDUCTIBLE'
     O                                         +002 ' CLAIM or '
     O                                         +002 ' DEDUCTIBLE '
     O                                         +002 '  AGGREGATE '
     O                                         +003 '  TOTAL #  '
     O                                         +003 '    PREVIOUS   '
     O                                         +003 '   CURRENTLY   '
     O                                         +003 '     TOTAL AMT '
     O              n51                        +003 '     TOTAL AMT '
     O              n51                        +003 '     TOTAL AMT '
     O          E            EHEAD          1
     O                                         +000 'YEAR'
     O                                         +002 ' EFF DATE '
     O                                         +002 'OCCURRENCE'
     O                                         +002 '   AMOUNT   '
     O                                         +002 '    AMOUNT  '
     O                                         +003 ' OF CLAIMS '
     O                                         +003 '     BILLED    '
     O                                         +003 '     BILLED    '
     O                                         +003 '       BILLED  '
     O              n51                        +004 '       PAID    '
     O              n51                        +005 '  NOT BILLED   '
     O          E            EHEAD          2
     O                                         +000 '------'
     O                                         +000 '------------'
     O                                         +000 '------------'
     O                                         +000 '------------'
     O                                         +000 '-------------'
     O                                         +000 '---------------'
     O                                         +000 '------------------'
     O                                         +000 '------------------'
     O                                         +000 '-----------------'
     O              n51                        +000 '-------------------'
     O              n51                        +000 '-------------------'
     O          E            EDTAIL         1
     O                       XXEFYR        Z   +001
     O                       XXDedEffDt        +002
     O                       XXOccClm          +002
     O              N22      XXAMPO        J   +002
     O                       XXAMAG        J   +002
     O                       XXT#CL        J   +003
     O                       XXTLDP        J   +003
     O                       XXTLDC        J   +003
     O                       XXTLDT        J   +003
     O              n51      XXTLTD        J   +003
     O              n51      XXTLTN        J   +003
     O          E            EFOOT          2
     O                                         +000 '------'
     O                                         +000 '------------'
     O                                         +000 '------------'
     O                                         +000 '------------'
     O                                         +000 '-------------'
     O                                         +000 '---------------'
     O                                         +000 '------------------'
     O                                         +000 '------------------'
     O                                         +000 '-----------------'
     O              n51                        +000 '-------------------'
     O              n51                        +000 '-------------------'
     O          E            ETOTAL         2
     O                                           10 'TOTALS FOR'
     O                                           27 'ALL GROUP YEARS:'
     O                       XXF#CL        J     70
     O                       XXFYDP        J   +003
     O                       XXFYDC        J   +003
     O                       XXFYDT        J   +003
     O              n51      XXFYTD        J   +003
     O              n51      XXFYTN        J   +003
     O          E            MCHEAD      3  1
     O                                           16 'Multiple claims:'
     O                                           44 'Multiple'
     O          E            MCHEAD         1
     O                                           23 'Year'
     O                                           32 '   Case'
     O                                           44 'Claim #'
     O          E            MCDET          2
     O                       MCFYR         4     22
     O                       XXCASE        4     32
     O                       MCCLM#        4     44
      * ==============================================================
     O          E            AHEAD          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             111
     O                       XXDATE             192 '  /  /    '
     O          E            AHEAD          2
     O                       Q1PGM               10
     O                       ORTITL             121
     O                                          187 'PAGE:'
     O                       PAGE          Z    192
     O          E            AHEAD          2
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                       XXFNAM              71
     O          E            AHEAD          1
     O                                           65 'TOTAL AMOUNT'
     O                                           84 'TOTAL AMOUNT'
     O          E            AHEAD          1
     O                                           48 'TOTAL #'
     O                                           63 'PREVIOUS'
     O                                           82 'CURRENTLY'
     O                                          103 'TOTAL AMT.'
     O              n51                         124 'TOTAL AMT.'
     O              n51                         145 'TOTAL AMT.'
     O          E            AHEAD          1
     O                                           47 'CLAIMS'
     O                                           62 'BILLED'
     O                                           81 'BILLED'
     O                                          101 'BILLED'
     O              n51                         122 'PAID'
     O              n51                         145 'NOT BILLED'
     O          E            AHEAD          2
     O                                           24 '------------------------'
     O                                           48 '------------------------'
     O                                           72 '------------------------'
     O                                           96 '------------------------'
     O                                          105 '---------'
     O              n51                         126 '------------------------'
     O              n51                         147 '------------------------'
     O          E            ATOTAL      1  1
     O                                           10 'TOTALS FOR'
     O                                           25 'ALL EMPLOYERS:'
     O                       XXE#CL        JB    48
     O                       XXEMDP        JB    67
     O                       XXEMDC        JB    86
     O                       XXEMDT        JB   105
     O              n51      XXEMTD        JB   126
     O              n51      XXEMTN        JB   147
      * ==============================================================
      *
     O          E            RHEAD          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             111
     O                       XXDATE             192 '  /  /    '
      *
     O          E            RHEAD          2
     O                       Q1PGM               10
     O                       ORTITL             121
     O                                          187 'PAGE:'
     O                       PAGE          Z    192
      *
     O          E            RHEAD       1  1
     O                                           65 'TOTAL AMOUNT'
     O                                           84 'TOTAL AMOUNT'
      *
     O          E            RHEAD          1
     O                                           48 'TOTAL #'
     O                                           63 'PREVIOUS'
     O                                           82 'CURRENTLY'
     O                                          103 'TOTAL AMT.'
     O              n51                         124 'TOTAL AMT.'
     O              n51                         145 'TOTAL AMT.'
      *
     O          E            RHEAD          1
     O                                           47 'CLAIMS'
     O                                           62 'BILLED'
     O                                           81 'BILLED'
     O                                          101 'BILLED'
     O              n51                         122 'PAID'
     O              n51                         145 'NOT BILLED'
      *
     O          E            RHEAD          2
     O                                           24 '------------------------'
     O                                           48 '------------------------'
     O                                           72 '------------------------'
     O                                           96 '------------------------'
     O                                          105 '---------'
     O              n51                         126 '------------------------'
     O              n51                         147 '------------------------'
      *
     O          E            RTOTAL      1  1
     O                                           10 'TOTALS FOR'
     O                                           25 'REPORT:       '
     O                       XXR#CL        JB    48
     O                       XXRMDP        JB    67
     O                       XXRMDC        JB    86
     O                       XXRMDT        JB   105
     O              n51      XXRMTD        JB   126
     O              n51      XXRMTN        JB   147
      *
     O          E            NORECS      2  1
     O                                           23 '*** NO RECORDS MEET THE'
     O                                           46 'SELECTION CRITERIA ***'
     O*========================================================================
     OQSYSPRT1  E            ROUTE1         1 06
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
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE1         2 33
     O                                           18 'REPORT SELECTIONS'
     O          E            ROUTE1         2
     O                                           22 'COMPANY     :'
     O                       XXCO#         Z     26
     O          E            ROUTE1         2
     O                                           22 'GROUP       :'
     O                       XXFND         Z     26
     O          E            ROUTE1         2
     O                                           22 'POSTING DATE:'
     O                       XXPODT              33 '  /  /    '
     O          E            ROUTE1         2
     O                                           22 'LOSSES AS OF:'
     O                       X1LSDT              33 '  /  /    '
     O          E            ROUTE1         2
     O                                           22 'EXCLUDE NO AC'
     O                                         +000 'TIVITY POLICIES:'
     O                       QQEXCL            +002
     O          E            ROUTE1         2
     O                       XPDDTP            +009
     O          E            ROUTE1         1 59
     O                                         +001 '(c)'
     O                       *YEAR             +001
     O                                         +001 'StoneRiver, Inc.'
     O                                         +001 'All rights reserved.'
     O          E            ROUTE1         1
     O                                         +001 'StoneRiver is a'
     O                                         +001 'trademark of'
     O                                         +001 'StoneRiver, Inc.'
      * ==============================================================
     O          E            BHEAD1         1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             111
     O                       XXDATE             192 '  /  /    '
     O          E            BHEAD1         2
     O                       Q1PGM               10
     O                       ORTITL             121
     O                                          187 'PAGE:'
     O                       PAGE          Z    192
     O          E            BHEAD1         2
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                       XXFNAM              71
     O          E            BHEAD1         2
     O                                           09 'EMPLOYER:'
     O                       prcEmp#       Z     19
     O                       prcDiv        Z     25
     O                       XXENAM              67
     O                                           79 'POST DATE:'
     O                       XXPODT              90 '  /  /    '
     O                                          111 'LOSS AS OF DATE:'
     O                       X1LSDT             122 '  /  /    '
     O          E            HEAD1          1
     O                                          147 'REIMBURSABLE'
     O                                          163 'DEDUCTIBLE'
     O          E            HEAD1          1
     O                                          145 'PREVIOUS'
     O                                          162 'CURRENT'
     O                                         +  9 'PREVIOUS'
     O                                         + 10 'CURRENT'
     O          E            HEAD1          1
     O                                         +  6 'CLAIM'
     O                                         +  3 'CLAIMANT NAME'
     O                                           56 'ACCIDENT/'
     O                                          146 'AMOUNT '
     O                                          162 'AMOUNT'
     O                                         +  9 '  FUND  '
     O                                         + 10 ' FUND  '
     O          E            HEAD1          1
     O                                            4 'YEAR'
     O                                         +  2 'NUMBER'
     O                                         +  2 'SSN'
     O                                           56 'REPORT DATE'
     O                                           80 'COMP'
     O                                           95 'MEDICAL'
     O                                          110 'EXPENSES'
     O                                          125 'TOTAL'
     O                                          130 'O/C'
     O                                          145 'BILLED'
     O                                          162 'BILLED'
     O                                         +  9 '  PAID  '
     O                                         + 10 ' PAID  '
     O          E            HEAD1          2
     O                                           24 '------------------------'
     O                                           48 '------------------------'
     O                                           72 '------------------------'
     O                                           96 '------------------------'
     O                                          120 '------------------------'
     O                                          144 '------------------------'
     O                                          168 '------------------------'
     O                                          192 '------------------------'
     O                                          196 '----'
     O          E            DET1           1
     O                       prcFyr        Z     03
     O                       prcCase           +  2
     O                       XXCNAM            +  2
     O                       XXACDT            +  2 '  /  /    '
     O                                         +  2 'INCURRED'
     O                       XXINCO        J   +  2
     O                       XXINME        J   +  2
     O                       XXINEX        J   +  2
     O                       XXINTL        J   +  2
     O                       CHOCST            +  2
     O                       XXDBPV        J   +  2
     O                       XXDBCU        J   +  2
     O                       XXPFPD        J   +  2
     O                       XXCFPD        J   +  2
     O          E            DET1           2
     O                       XXSS1               17
     O                                           18 '-'
     O                       XXSS2               20
     O                                           21 '-'
     O                       XXSS3               25
     O                       XXDTRP              56 '  /  /    '
     O                                         +  2 'PAID'
     O                       XXPDCO        J     81
     O                       XXPDME        J   +  2
     O                       XXPDEX        J   +  2
     O                       XXPDTL        J   +  2
     O          E            FOOT1            61
     O                                           23 '* - Indicates a claim '
     O                                           41 'that is part of a '
     O                                           64 'multi-claim occurrence.'
     O          E            EHEAD1         1
     O                                           77 'CURRENT'
     O                                           94 'TOTAL'
     O                                          125 'PREVIOUS'
     O                                          142 'TOTAL'
     O                                          159 'TOTAL'
     O          E            EHEAD1         1
     O                                           14 'PER'
     O                                           43 'CURRENT #'
     O                                           60 'CURRENT'
     O                                           77 'FUND'
     O                                           94 'CURRENT'
     O                                          108 'TOTAL #'
     O                                          125 'DEDUCTIBLE'
     O                                          142 'DEDUCTIBLE'
     O                                          159 'FUND'
     O                                          176 'TOTAL'
     O          E            EHEAD1         1
     O                                            4 'YEAR'
     O              N25                          16 'OCCURRENCE'
     O               25                          16 '  CLAIM   '
     O                                           30 'AGGREGATE'
     O                                           43 'CLAIMS'
     O                                           60 'BILLED'
     O                                           77 'PAID'
     O                                           94 'PAID'
     O                                          108 'CLAIMS'
     O                                          125 'BILLED'
     O                                          142 'BILLED'
     O                                          159 'PAID'
     O                                          176 'PAID'
     O          E            EHEAD1         2
     O                                           24 '------------------------'
     O                                           48 '------------------------'
     O                                           72 '------------------------'
     O                                           96 '------------------------'
     O                                          120 '------------------------'
     O                                          144 '------------------------'
     O                                          168 '------------------------'
     O                                          180 '------------'
     O*
     O          E            EDET1          1
     O                       XXEFYR        Z      3
     O                       XXAMPO        J   +  2
     O                       XXAMAG        J   +  2
     O*
     O                       XXT#CL        J   +  2
     O                       XXTLDC        J   +  2
     O                       XXTLCP        J   +  2
     O                       XXTCUP        J   +  2
     O*
     O                       XXT#CL        J   +  3
     O                       XXTLDP        J   +  2
     O                       XXTLDT        J   +  2
     O                       XXTLFP        J   +  2
     O                       XXTTPD        J   +  2
     O          E            ETOT1          2
     O                                           24 '------------------------'
     O                                           48 '------------------------'
     O                                           72 '------------------------'
     O                                           96 '------------------------'
     O                                          105 '---------'
     O                                          120 '------------------------'
     O                                          144 '------------------------'
     O                                          168 '------------------------'
     O                                          180 '------------'
     O          E            ETOT1          2
     O                                           10 'TOTALS FOR'
     O                                           27 'ALL GROUP YEARS:'
     O                       XXF#CL        J     44
     O                       XXFYDC        J   +  2
     O                       XXFYCP        J   +  2
     O                       XXYCUP        J   +  2
     O                       XXF#CL        J   +  3
     O                       XXFYDP        J   +  2
     O                       XXFYDT        J   +  2
     O                       XXFYFP        J   +  2
     O                       XXTFPD        J   +  2
     O          E            MCHD1       3  1
     O                                           16 'Multiple claims:'
     O                                           44 'Multiple'
     O          E            MCHD1          1
     O                                           23 'Year'
     O                                           32 '   Case'
     O                                           44 'Claim #'
     O          E            MCDET1         2
     O                       MCFYR         4     22
     O                       XXCASE        4     32
     O                       MCCLM#        4     44
      * ==============================================================
     O          E            AHEAD1         1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             111
     O                       XXDATE             192 '  /  /    '
     O          E            AHEAD1         2
     O                       Q1PGM               10
     O                       ORTITL             121
     O                                          187 'PAGE:'
     O                       PAGE          Z    192
     O          E            AHEAD1         2
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                       XXFNAM              71
     O          E            AHEAD1         1
     O                                           65 'TOTAL AMOUNT'
     O                                           84 'TOTAL AMOUNT'
     O          E            AHEAD1         1
     O                                           48 'TOTAL #'
     O                                           63 'PREVIOUS'
     O                                           82 'CURRENTLY'
     O                                          103 'TOTAL AMT.'
     O                                          123 'PREVIOUS'
     O                                          141 'CURRENT'
     O          E            AHEAD1         1
     O                                           47 'CLAIMS'
     O                                           62 'BILLED'
     O                                           81 'BILLED'
     O                                          101 'BILLED'
     O                                          123 'FUND PAID'
     O                                          142 'FUND PAID'
     O          E            AHEAD1         2
     O                                           24 '------------------------'
     O                                           48 '------------------------'
     O                                           72 '------------------------'
     O                                           96 '------------------------'
     O                                          120 '------------------------'
     O                                          142 '----------------------'
     O          E            ATOT1       1  1
     O                                           10 'TOTALS FOR'
     O                                           25 'ALL EMPLOYERS:'
     O                       XXE#CL        J     48
     O                       XXEMDP        J     66
     O                       XXEMDC        J     86
     O                       XXEMDT        J    104
     O                       XXEMPP        J    124
     O                       XXEMCP        J    143
      * ==============================================================
      *
     O          E            RHEAD1         1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             111
     O                       XXDATE             192 '  /  /    '
      *
     O          E            RHEAD1         2
     O                       Q1PGM               10
     O                       ORTITL             121
     O                                          187 'PAGE:'
     O                       PAGE          Z    192
      *
     O          E            RHEAD1         2
     O                                           65 'TOTAL AMOUNT'
     O                                           84 'TOTAL AMOUNT'
      *
     O          E            RHEAD1         1
     O                                           48 'TOTAL #'
     O                                           63 'PREVIOUS'
     O                                           82 'CURRENTLY'
     O                                          103 'TOTAL AMT.'
     O                                          123 'PREVIOUS'
     O                                          141 'CURRENT'
      *
     O          E            RHEAD1         1
     O                                           47 'CLAIMS'
     O                                           62 'BILLED'
     O                                           81 'BILLED'
     O                                          101 'BILLED'
     O                                          123 'FUND PAID'
     O                                          142 'FUND PAID'
      *
     O          E            RHEAD1         2
     O                                           24 '------------------------'
     O                                           48 '------------------------'
     O                                           72 '------------------------'
     O                                           96 '------------------------'
     O                                          120 '------------------------'
     O                                          142 '----------------------'
      *
     O          E            RTOT1       1  1
     O                                           10 'TOTALS FOR'
     O                                           25 'REPORT:       '
     O                       XXR#CL        JB    48
     O                       XXRMDP        JB    66
     O                       XXRMDC        JB    85
     O                       XXRMDT        JB   104
     O                       XXRMPP        J    124
     O                       XXRMCP        J    143
      * ==============================================================
     O          E            NOREC1      2  1
     O                                           23 '*** NO RECORDS MEET THE'
     O                                           46 'SELECTION CRITERIA ***'
     C*=================================================================
     C* wrtWrkFile - Writes to WFDDP (Modified from SRWRK)             *
     C*=================================================================
     p wrtWrkFile      b
     d wrtWrkFile      pi
     C*
     C                   MOVE      XXCO#         D1CO#
     C                   MOVEL     CONAME        D1CNAM
     C                   MOVE      XXFND         D1FND
     C                   MOVEL     XXFNAM        D1FNAM
     C                   MOVE      XXDATE        D1RNDT
     C                   MOVE      QQLSDT        D1ASOF
     C                   MOVE      QQPODT        D1PODT
     C                   MOVE      PRCEMP#       D1EMP#
     C                   MOVE      PRCDIV        D1DIV
     C                   MOVEL     XXENAM        D1ENAM
     C                   MOVE      PRCFYR        D1FYR
     C                   MOVE      PRCCASE       D1CASE
     C                   MOVEL     XXCNAM        D1CLMN
     C                   MOVE      CMSS#         D1SS#
     C                   MOVE      XXACDT        D1ACDT
     C                   MOVE      XXDTRP        D1RPDT
     C                   MOVE      CHOCST        D1CLST
     C                   MOVE      XXPDCO        D1CPTD
     C                   MOVE      XXPDME        D1MPTD
     C                   MOVE      XXPDEX        D1EPTD
     C                   MOVE      XXPDTL        D1TPTD
     C                   MOVE      XXINCO        D1CINC
     C                   MOVE      XXINME        D1MINC
     C                   MOVE      XXINEX        D1EINC
     C                   MOVE      XXINTL        D1TINC
     C                   MOVE      XXDBPV        D1PRVB
     C                   MOVE      XXDBCU        D1CURB
     C     XXDBPV        ADD       XXDBCU        D1TOTB
     C                   WRITE     PD101
     C*
     p wrtWrkFile      e
      *=========================================================================
      * initFndFld - initialize fields used on fund break
      *=========================================================================
     p initFndFld      b
     d initFndFld      pi
     c                   clear                   nm
     c                   clear                   rt
     c                   clear                   rr
     c                   clear                   ar
     c                   clear                   fy
     c                   clear                   ed
     c                   clear                   oc
     c                   clear                   gn
     c                   clear                   po
     c                   clear                   ag
     c                   clear                   tc
     c                   clear                   dp
     c                   clear                   dc
     c                   clear                   dt
     c                   clear                   tn
     c                   clear                   td
     c                   clear                   pp
     c                   clear                   cp
     c                   clear                   xxfrec
     c                   clear                   xxar01
     c                   clear                   xxar02
     c                   clear                   xxar03
     c                   clear                   xxe#cl
     c                   clear                   xxemdp
     c                   clear                   xxemdc
     c                   clear                   xxemdt
     c                   clear                   xxemtn
     c                   clear                   xxemtd
     c                   clear                   xxempp
     c                   clear                   xxemcp
     c                   eval      *inof = *on
     c                   eval      *inoa = *on
     c                   eval      prcEmp# = 0
     c                   eval      prcDiv  = 0
     p initFndFld      e
      *=========================================================================
      * getHdrInfo  - get printing info for headers
      *=========================================================================
     p getHdrInfo      b
     d getHdrInfo      pi

     c     fnkey         chain     pfn01                              60
     c     *in60         ifeq      '0'
     c                   movel     FNNAME        xxfnam
     c     FNANAM        ifne      *blanks
     c                   movel     FNANAM        CONAME
     c                   endif
     c                   else
     c                   move      *blanks       xxfnam
     c                   end

     c                   call      'SRCENTER'
     c                   parm      CONAME        xxName           80
     c                   parm      40            length            3 0
     c                   eval      CONAME = xxName


     p getHdrInfo      e
      *=========================================================================
      * setYearArr  - set up fund year arrays
      *=========================================================================
     p setYearArr      b
     d setYearArr      pi
     d x               s              3  0

     c                   eval      x = 0
     c     f1key         setll     pf101
      *** Beg Chg ***** 01/20/25  **********************************
     c*****f1key         chain     pf101
     c     f1key         reade     pf101
      *** End Chg ***** 01/20/25  **********************************
     c                   dow       not(%eof)
     c                   add       1             x
     c                   z-add     F1FYR         fy(x)
     c                   eval      aso(x) = *off
     c                   if        F1CLMS='Y' and F1UPAR ='N'
     c                   eval      aso(x) = *on
     c                   endif
     c     f1key         reade     pf101
     c                   enddo

     p setYearArr      e
      *=========================================================================
      * initPolFlds - initial policy fields
      *=========================================================================
     p initPolFlds     b
     d initPolFlds     pi
     d y               s              3  0

     c     emky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
      *
      * Break page by year
      *
     c                   if        breakByYr
     c                   eval      *inoa = *on
     c                   eval      *inof = *on
     c                   endif

     c                   if        ELEMP# <> prcEmp# or ELDIV <> prcDiv
     c                   clear                   xxf#cl
     c                   clear                   xxfydp
     c                   clear                   xxfydc
     c                   clear                   xxfydt
     c                   clear                   xxfytn
     c                   clear                   xxfytd
     c                   clear                   xxfyfp
     c                   clear                   xxtfpd
     c                   clear                   xxycup
     c                   clear                   xxfypp
     c                   clear                   xxfycp
     c                   clear                   ed
     c                   clear                   oc
     c                   clear                   gn
     c                   clear                   pp
     c                   clear                   cp
     c                   clear                   po
     c                   clear                   ag
     c                   clear                   tc
     c                   clear                   dp
     c                   clear                   dc
     c                   clear                   dt
     c                   clear                   tn
     c                   clear                   td
     c                   eval      processYr = *off
     c                   eval      printFoot = *off
     c                   eval      *inoa = *on
     c                   eval      *inof = *on
     c     emky          chain     pem01
     c                   if        %found
     c                   eval      xxenam = EMNAME
     c                   else
     c                   eval      xxenam = *blanks
     c                   endif
     c                   endif
      *
      * Print multi-claims message if any claims detail printed.
      *
      /free
       If printFoot = *on and breakByYr = *on;
          Select;
          When Not(isUseSpoolOut);
          When cltid = '071';
             Except foot1;
          Other;
             Except footer;
          Endsl;
       Endif;
      /end-free

     c                   eval      xyDedEffDt  = 0
     c                   eval      xyAmtPerOcc = 0
     c                   eval      xyAmtAgg    = 0
     c                   eval      xyOccClm    = *blanks
     c                   eval      xyGrossNet  = *blanks
     c                   eval      prcEmp# = ELEMP#
     c                   eval      prcDiv  = ELDIV
     c                   eval      prcFyr  = ELFYR
     c                   eval      xxKey = %editc(ELBSCD:'X')
     c                   clear                   BSLPGM
     c     tbKey         chain     pbs01
     c                   eval      largeDed = BSLPGM  = 'L'
     c                   eval      smallDed = BSLPGM <> 'L'
     c                   eval      xxcd = ELRTCD
     c     rsky          chain     prs01
     c                   eval      xxst = RSJRST
     c                   callp     getDedTblInf

     c                   if        xyDedEffDt  = 0
     c                   return
     c                   endif

     c                   eval      y = 1
     c     ELFYR         lookup    fy(y)                                  60
     c                   eval      sl(y) = BSLPGM
     c                   eval      ed(y) = %date(xyDedEffDt : *cymd)
     c                   eval      po(y) = xyAmtPerOcc
     c                   eval      ag(y) = xyAmtAgg

     c                   select
     c                   when      xyOccClm = 'C'
     c                   eval      oc(y) = 'CLAIM'
     c                   eval      *in25 = *on
     c                   other
     c                   eval      oc(y) = 'OCCURRENCE'
     c                   eval      *in25 = *off
     c                   endsl

     c                   select
     c                   when      xyGrossNet = 'G'
     c                   eval      gn(y) = 'GROSS'
     c                   other
     c                   eval      gn(y) = ' NET '
     c                   endsl


      * Write and update Deductible Payment Workfile for billed/non-billed amoun

     c                   if        singleEmpRun = *off and cltID <> '071'
     c                   open      wfd2p
     c                   open      wfd2l
     c                   callp     wrtDedWrkFil
     c                   callp     calcNonBillA
     c                   close     wfd2p
     c                   close     wfd2l
     c                   endif


     p initPolFlds     e
      *=========================================================================
      * initClmFlds - initial policy fields
      *=========================================================================
     p initClmFlds     b
     d initClmFlds     pi
     d kkcase          s                   like(MCCASE)

     c     cdky          klist
     c                   kfld                    CMSS#

     c     cmky          klist
     c                   kfld                    CMCO#
     c                   kfld                    CMFND
     c                   kfld                    CMFYR
     c                   kfld                    CMCASE

     c     mcky          klist
     c                   kfld                    CMCO#
     c                   kfld                    CMFND
     c                   kfld                    CMEMP#
     c                   kfld                    CMDIV
     c                   kfld                    CMFYR
     c                   kfld                    kkcase

     c                   z-add     *zeros        xxpdco
     c                   z-add     *zeros        xxpdme
     c                   z-add     *zeros        xxpdex
     c                   z-add     *zeros        xxpdtl
     c                   z-add     *zeros        xxinco
     c                   z-add     *zeros        xxinme
     c                   z-add     *zeros        xxinex
     c                   z-add     *zeros        xxintl
     c                   z-add     *zeros        xxdbpv
     c                   z-add     *zeros        xxdbcu
     c                   z-add     *zeros        xxpfpd
     c                   z-add     *zeros        xxcfpd
     c                   z-add     *zeros        xxtlpd

     c                   eval      prcCase = CMCASE
     c     cdky          chain     pcd01
     c                   if        %found
     c                   eval      xxcnam =
     c                             %trimr(CDLSN) +
     c                             ', '          +
     c                             %trimr(CDFSN) +
     c                             ' '           +
     c                             CDMID
     c                   endif

     c                   z-add     CMACDT        srdti
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxacdt

     c                   z-add     CMDTRP        srdti
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxdtrp
      *
      * Retrieve open/closed status
      *
     c     cmky          chain     pch01
     c                   if        not(%found)
     c                   eval      CHOCST = *blanks
     c                   endif
      *
      * Display '*' if part of multiple claim
      *
     c                   move(p)   CMCASE        kkcase
     c     mcky          chain     pmc01                              21


     p initClmFlds     e
      *=========================================================================
      * dedTypeMatch - return *on if the deductible type matches the
      *                prompt selection
      *=========================================================================
     p dedTypeMatch    b
     d dedTypeMatch    pi              n

     c                   if        xxddtp = 'B'
     c                   return    *on
     c                   endif


     c                   select
     c                   when      largeVsSmall = *on
     c                   select
     c                   when      useLargeDed and largeDed
     c                   return    *on
     c                   when      useSmallDed and smallDed
     c                   return    *on
     c                   other
     c                   return    *off
     c                   endsl
     c                   other
     c                   return    *on
     c                   endsl

     p dedTypeMatch    e
      *=========================================================================
      * excludPolicy -  Returns *on if policy is eligible for processing
      *              -  Logic pulled from SREXCL
      *=========================================================================
     p excludPolicy    b
     d excludPolicy    pi              n
     d svfyr           s                   like(EAFYR) inz(0)
     d dedBilTaxCd     s                   like(EAARCD)
     d dedPayTaxCd     s                   like(EAARCD)
     d currBilled      s              1
     d dedBalance      s             +2    like(EAAMT)

     c     eaky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV

     c     ddky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    x2podt
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
      *
      * Return if exclude flag isn't set to exclude policies with no activity
      *
     c                   if        qqexcl <> 'Y'
     c                   return    *off
     c                   endif

     c                   if        cltID = '071'
     c                   return    *off
     c                   endif
      *
      *  Get deductible balance from A/R
      *
     c                   eval      svfyr = 0
     c     eaky          setll     pea01
     c     eaky          reade     pea01
     c                   dow       not(%eof)
     c                   if        EAFYR <> svfyr
     c                   eval      svfyr = EAFYR
     c                   clear                   dedBilTaxCd
     c                   clear                   dedPayTaxCd
     c     f1ky1         chain     pf101
     c                   if        %found
     c                   eval      dedBilTaxCd   = F1DTXB
     c                   eval      dedPayTaxCd   = F1DTXP
     c                   endif
     c                   endif
     c                   eval      xxkey = EAARCD
     c     tbkey         chain     ptt01
     c                   if        %found
     c     TTCTYP        ifeq      'R'
     c     EAARTY        andeq     'F'
     c     EAARCD        andne     dedBilTaxCd
     c                   add       EAAMT         dedBalance
     c                   endif

     c     TTCTYP        ifeq      'R'
     c     EAARTY        andeq     'Q'
     c     EAARCD        andne     dedPayTaxCd
     c                   sub       EAAMT         dedBalance
     c                   endif
     c                   endif

     c     eaky          reade     pea01
     c                   enddo
      *
      * Chk for current deductible billing
      *
     c     ddky          chain     pdd02
     c                   if        %found
     c                   eval      currBilled  = 'Y'
     c                   else
     c                   eval      currBilled  = 'N'
     c                   endif
      *
      * Logic for ckCurrAct was removed because it is impossible to
      * chk if a particular payment has been processed for deductible
      * billing or not at this point in the logic.
      *
     c                   if        dedBalance = 0 and
     c                             currBilled = 'N'
     c                   return    *on
     c                   else
     c                   return    *off
     c                   endif

     p excludPolicy    e
      *=========================================================================
      * getDedBilld -  get previous and current deductible billed for claim
      *=========================================================================
     p getDedBilld     b
     d getDedBilld     pi

     c     ddky          klist
     c                   kfld                    CMCO#
     c                   kfld                    CMFND
     c                   kfld                    CMEMP#
     c                   kfld                    CMDIV
     c                   kfld                    CMFYR
     c                   kfld                    CMCASE

     c     ddky          setll     pdd01
     c     ddky          reade     pdd01
     c                   dow       not(%eof)
     c                   select
     c                   when      DDPODT  < x2podt
     c                   eval      xxdbpv += DDAMTB
     c                   when      DDPODT  = x2podt
     c                   eval      xxdbcu += DDAMTB
     c                   endsl
     c     ddky          reade     pdd01
     c                   enddo

     p getDedBilld     e
      *=========================================================================
      * getClmIncrd -  get incurred total as of the loss date excluding reserve
      *                history records written by net deductibles (W8021R1)
      *=========================================================================
     p getClmIncrd     b
     d getClmIncrd     pi
     d i               s              2  0
     d kkkey           s             10

     c     rhky          klist
     c                   kfld                    CMCO#
     c                   kfld                    CMFND
     c                   kfld                    CMFYR
     c                   kfld                    CMCASE

     c     tbky          klist
     c                   kfld                    CMCO#
     c                   kfld                    kkkey

     c     rhky          setll     prh01
     c     rhky          reade     prh01
     c                   dow       not(%eof)

     c                   if        RHEFDT <= xxlsdt  and
     c                             RHPGM <> 'W8021R1'

     c                   do        10            i
     c                   if        rhamt(i) <> 0
     c                   movel(p)  rhcod(i)      kkkey
     c     tbky          chain     prt01                              60
     c                   select
     c                   when      RTTYPE = 'C'
     c                   add       rhamt(i)      xxinco
     c                   add       rhamt(i)      xxintl
     c                   when      RTTYPE = 'M'
     c                   add       rhamt(i)      xxinme
     c                   add       rhamt(i)      xxintl
     c                   when      RTTYPE = 'E' or RTTYPE = 'L'
     c                   add       rhamt(i)      xxinex
     c                   add       rhamt(i)      xxintl
     c                   endsl
     c                   endif
     c                   enddo

     c                   endif

     c     rhky          reade     prh01                                  71
     c                   enddo

     p getClmIncrd     e
      *=========================================================================
      * getClmPaid  -  get claim paid total for claim as of the loss date
      *=========================================================================
     p getClmPaid      b
     d getClmPaid      pi

     c     paky          klist
     c                   kfld                    CMCO#
     c                   kfld                    CMFND
     c                   kfld                    CMFYR
     c                   kfld                    CMCASE

     c     paky          setll     ppa01
     c     paky          reade     ppa01
     c                   dow       not(%eof)

     c     PATRDT        ifle      xxlsdt
     c     PABR2         andne     FNMEP1
     c     PABR2         andne     FNCOP1
     c     PABR2         andne     FNEXP1
     c     PABR2         andne     FNLEP1

     c     PATYPE        ifeq      'R'
     c     PATYPE        oreq      'S'
     c     PATYPE        oreq      'V'
     c                   mult      -1            PAAMT
     c                   endif

     c                   select
     c                   when      PARETP = 'C'
     c                   add       PAAMT         xxpdco
     c                   add       PAAMT         xxpdtl
     c                   when      PARETP = 'M'
     c                   add       PAAMT         xxpdme
     c                   add       PAAMT         xxpdtl
     c                   when      PARETP = 'E' or PARETP = 'L'
     c                   add       PAAMT         xxpdex
     c                   add       PAAMT         xxpdtl
     c                   endsl

     c                   if        cltid = '071'
     c                   select
     c                   when      PATRDT < xxfrdt
     c                   add       PAAMT         xxpfpd
     c                   add       PAAMT         xxtlpd
     c                   when      PATRDT >= xxfrdt
     c                   add       PAAMT         xxcfpd
     c                   add       PAAMT         xxtlpd
     c                   endsl
     c                   endif

     c                   endif

     c     paky          reade     ppa01
     c                   enddo

     p getClmPaid      e
      *=========================================================================
      * printDetail -  print detail line for claim
      *=========================================================================
     p printDetail     b
     d printDetail     pi
     d clmPaidTotal    s             13  2
     d dedBillTotal    s             13  2
     d y               s              3  0
     d n               s              5  0

     c                   eval      y = %lookup(CMFYR : fy)
     c                   add       1             tc(y)
     c                   add       xxdbpv        dp(y)
     c                   add       xxdbcu        dc(y)
     c                   add       xxpfpd        pp(y)
     c                   add       xxcfpd        cp(y)
     c                   add       xxtlpd        tp(y)
     c     xxdbpv        add       xxdbcu        xxwk11           11 2
     c                   add       xxwk11        dt(y)
     c                   add       xxnonb        tn(y)
     c                   add       xxpdtl        td(y)

     c                   select
     c                   when      cltid = '071'
     c                   eval      xxcfpd = xxcfpd - xxdbcu
     c                   eval      xxpfpd = xxpfpd - xxdbpv
     c                   other
     c                   eval      xxnonb   = 0
     c                   eval      dedBillTotal  = xxdbpv + xxdbcu
     c                   eval      n = %lookup(prcCase:cs#)
     c                   if        n <> 0
     c                   eval      xxnonb = csn(n)
     c                   endif
     c                   select
     c                   when      xxtype = 'N' and xxnonb = 0
     c                   return
     c                   when      xxtype = 'B' and
     c                             (xxdbpv = 0 and xxdbcu = 0)
     c                   return
     c                   when      singleEmpRun and xxdbcu=0 and xxdbpv=0
     c                             and cltid = '025'
     c                   return
      *** Beg Chg ***** 01/20/25  **********************************
      * write a record to the Deductible detail report work file (WFDDP-PDD01)
      * when we run deductible Invoice for a date other than Invoice date in wdddp
     c*****              when      singleEmpRun and xxdbcu = 0
     c*****                        and cltid <> '025'
     c                   when      singleEmpRun and xxdbcu = 0 and xxdbpv = 0
     c                             and cltid <> '025'
      *** End Chg ***** 01/20/25  **********************************
     c                   return
     c                   endsl
     c                   endsl

     c                   select
     c                   when      xxy4n = '4'
     c                   eval      xxss1 = 'XXX'
     c                   eval      xxss2 = 'XX'
     c                   when      xxy4n = 'N'
     c                   eval      xxss1 = 'XXX'
     c                   eval      xxss2 = 'XX'
     c                   eval      xxss3 = 'XXXX'
     c                   endsl

      /FREE
       select;
       When Not(isUseSpoolOut);
       when cltid = '025';
         if xxdbcu <> 0;
           if *inof;
             if printFoot;
               except footer;
             endif;
             select;
             when singleEmpRun;
               except bheads;
             other;
               except bhead;
             endsl;
             except head;
             *inof = *off;
           endif;
         endif;
       when cltid = '071';
         if xxdbcu <> 0 or xxcfpd <> 0 or xxmode = 'D';
           if *inoa;
             if printFoot;
               except foot1;
             endif;
             except bhead1;
             except head1;
             *inoa = *off;
           endif;
         endif;
       other;
         if *inof;
           if printFoot;
              except footer;
           endif;
           select;
           when singleEmpRun;
             except bheads;
           other;
             except bhead;
           endsl;
           except head;
           *inof = *off;
         endif;
       endsl;
       cmSS# = MaskData('SSN':'cmSS#':cmSS#);
       select;
       When Not(isUseSpoolOut);
       when cltid = '025';
         if xxdbcu <> 0;
           except detail;
           printFoot = *on;
         endif;
       when cltid = '071';
         if xxdbcu <> 0 or xxcfpd <> 0 or xxmode = 'D';
           except det1;
         endif;
       other;
          except detail;
          printFoot = *on;
       endsl;
       cmSS# = UnMaskData('SSN':'cmSS#':cmSS#);
       If isUseXML;
          WrtEmpDtl();
       Endif;
      /END-FREE

     c                   eval      xxfrec = *on
     c                   eval      xxrec  = *on
     c                   eval      processYr = *on
     c                   callp     wrtWrkFile

     p printDetail     e
      *=========================================================================
      * printFyrTtl -  print policy year totals
      *=========================================================================
     p printFyrTtl     b
     d printFyrTtl     pi
     d x               s              3  0

     c     mcky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
      *
      * Return if no detail exists for employer/div
      *
     c                   if        processYr = *off
     c                   return
     c                   endif
      *
      * Print fyr totals on emp/div break else return
      *
     c                   if        ELEMP# <> prcEmp# or
     c                             ELDIV  <> prcDiv  or
     c                             %eof(WDELL)
     c                   eval      *inoa = *on
     c                   eval      *inof = *on
     c                   else
     c                   return
     c                   endif
      /FREE
       select;
       When Not(isUseSpoolOut);
       when cltid = '071';
          except foot1;
          except bhead1;
          except ehead1;
       other;
          except footer;
          except ehead;
       endsl;
      /END-FREE

     c                   z-add     1             x
     c     fy(x)         downe     *zeros
     c     x             andle     100
     c     dp(x)         ifne      *zeros
     c     dc(x)         orne      *zeros
     c                   z-add     fy(x)         xxefyr
     c                   eval      xxdedeffdt = ed(x)
     c                   eval      xxoccclm   = oc(x)
     c                   eval      xxgrossnet = gn(x)
     c                   z-add     po(x)         xxampo
     c                   eval      *in22=aso(x)
     c                   z-add     ag(x)         xxamag
     c                   z-add     tc(x)         xxt#cl
     c                   z-add     dp(x)         xxtldp
     c                   z-add     dc(x)         xxtldc
     c                   z-add     dt(x)         xxtldt
     c                   z-add     tn(x)         xxtltn
     c                   z-add     td(x)         xxtltd
     c                   z-add     td(x)         xxttpd

     c                   add       xxt#cl        xxf#cl
     c                   add       xxtldp        xxfydp
     c                   add       xxtldc        xxfydc
     c                   add       xxtldt        xxfydt
     c                   add       xxtltn        xxfytn
     c                   add       xxtltd        xxfytd
     c                   z-add     pp(x)         xxtlpp
     c                   z-add     cp(x)         xxtlcp
     c                   add       xxtlpp        xxfypp
     c                   add       xxtlcp        xxfycp
     c     xxtldc        add       xxtlcp        xxtcup
     c                   add       xxtcup        xxycup
     c     xxttpd        sub       xxtldt        xxtlfp
     c                   add       xxtlfp        xxfyfp
     c                   add       xxttpd        xxtfpd
      /FREE
       select;
       When Not(isUseSpoolOut);
       when cltid = '071';
          if *inoa;
             except bhead1;
             except ehead1;
             *inoa = *off;
          endif;
          except edet1;
       other;
          if *inof;
             except ehead;
             *inof = *off;
          endif;
          except edtail;
       endsl;
       If isUseXML;
          WrtFyrDtl();
       Endif;
      /END-FREE

     c                   endif
     c                   eval      x += 1
     c                   enddo
      *
      * Print totals
      *

      /Free
       Select;
       When Not(isUseSpoolOut);
       When cltID = '071';
          If *inoa;
             Except bhead1;
             Except ehead1;
             *inoa = *off;
          Endif;
          Except etot1;
       Other;
          If *inof;
             Except ehead;
             *inof = *off;
          Endif;
          Except efoot;
          Except etotal;
       Endsl;
       If isUseXml;
          WrtEmpHdr();
          WrtEmpTtl();
       Endif;
      /End-free
      *
      * Accumulate report totals
      *
     c                   add       xxf#cl        xxe#cl
     c                   add       xxfydp        xxemdp
     c                   add       xxfydc        xxemdc
     c                   add       xxfydt        xxemdt
     c                   add       xxfytn        xxemtn
     c                   add       xxfytd        xxemtd
     c                   add       xxf#cl        xxr#cl
     c                   add       xxfydp        xxrmdp
     c                   add       xxfydc        xxrmdc
     c                   add       xxfydt        xxrmdt
     c                   add       xxfytn        xxrmtn
     c                   add       xxfytd        xxrmtd
     c                   add       xxfypp        xxempp
     c                   add       xxfycp        xxemcp
     c                   add       xxfypp        xxrmpp
     c                   add       xxfycp        xxrmcp
      *
      *  List all claims that are part of a multiple claim.
      *

      /Free
       *in21 = *on;
       Setll mcKy pmc01;
       Reade mcKy pmc01;
       Dow Not(%Eof);
          xxCase = %int(MCcase);
          Select;
          When not(isUseSpoolOut);
          When cltID = '071';
             If *in21;
                Except mchd1;
                *In21 = *off;
             Endif;
             Except mcDet1;
          Other;
             If *in21;
                Except mcHead;
                *In21 = *off;
             Endif;
             Except mcDet;
          Endsl;
          If isUseXML;
             WrtMltClm();
          Endif;
       Reade mcKy pmc01;
       Enddo;
      /End-free

     p printFyrTtl     e
      *=========================================================================
      * printGrpTtl -  print group totals
      *=========================================================================
     p printGrpTtl     b
     d printGrpTtl     pi

       //*** Beg Add ***** 01/20/25 **********************************
      /Free
       If isUseXML;
          WrtFndHdr();
          WrtFndTtl();
       Endif;
      /End-free
       //*** End Add ***** 01/20/25 **********************************
     c                   if        xxfrec <> *on  or singleEmpRun
     c                   return
     c                   endif


      /Free
       //*** Beg Del ***** 01/20/25 **********************************
       // If isUseXML;
       //    WrtFndHdr();
       //    WrtFndTtl();
       // Endif;
       //*** End Del ***** 01/20/25 **********************************
       Select;
       When not(isUseSpoolOut);
       When cltID = '071';
          Except ahead1;
          Except atot1;
       Other;
          Except ahead;
          Except atotal;
       Endsl;
      /End-free

     p printGrpTtl     e
      *=========================================================================
      * getDedTblInf -  retrieve deductible table info
      *=========================================================================
     p getDedTblInf    b
     d getDedTblInf    pi

     c     ldky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c     ddky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    xxst
     c                   kfld                    ELDCDE

     c     ddky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    xxst
     c                   kfld                    ELDCDE
     c                   kfld                    xxdt              7 0

     c                   select
     c                   when      smallDed
     c                              and useSmallDed
     c                   exsr      srSmallDed
     c                   when      largeDed
     c                              and useLargeDed
     c                   exsr      srLargeDed
     c                   endsl

      *===============================================================

     c     srLargeDed    begsr

     c     ldky          chain     wmldl1
     c                   select
     c                   when      (xxlsDt >= LDEFD2 and LDEFD2 > 0)
     c                   eval      xyDedEffDt  = LDEFD2
     c                   eval      xyAmtPerOcc = LDAMP2
     c                   eval      xyAmtAgg    = LDAMG2
     c                   eval      xyOccClm    = LDVRB2
     c                   eval      xyAmtAgg    = LDAMG2
     c                   when      (xxlsDt >= LDEFD1 and LDEFD1 > 0)
     c                   eval      xyDedEffDt  = LDEFD1
     c                   eval      xyAmtPerOcc = LDAMP1
     c                   eval      xyAmtAgg    = LDAMG1
     c                   eval      xyOccClm    = LDVRB1
     c                   eval      xyGrossNet  = LDTYP1
     c                   endsl

     c                   endsr

      *========================================================================

     c     srSmallDed    begsr

     c                   move      ELFND         qqfnd             3
     c                   move      ELFYR         q1fyr             3
     c                   move      ELEMP#        q1emp#            9
     c                   move      ELDIV         q1div             5
     c                   select
     c                   when      (xxlsDt >= EL2DEF and EL2DEF > 0)
     c                   move      EL2DEF        q1date            7
     c                   eval      xyDedEffDt  = EL2DEF
     c                   when      (xxlsDt >= ELDCEF and ELDCEF > 0)
     c                   move      ELDCEF        q1date            7
     c                   eval      xyDedEffDt  = ELDCEF
     c                   endsl

     c                   callp     getRateDate (qqco#   :
     c                                          qqfnd   :
     c                                          q1fyr   :
     c                                          q1emp#  :
     c                                          q1div   :
     c                                          q1date )
     c                   move      q1Date        xxdt
     c     ddky1         setgt     pdd0t
     c     ddky          readpe    pdd0t                                  60
     c     *in60         ifeq      '0'
     c     DDSTAT        andeq     'A'
     c                   eval      xyamtperocc = DDAMPO
     c                   eval      xyamtagg    = DDAMAG
     c                   eval      xyoccclm    = DDVERB
     c                   eval      xygrossnet  = DDTYPE
     c                   else
     C                   eval      xyDedEffDt  = 0
     c                   eval      xyamtperocc = 0
     c                   eval      xyamtagg    = 0
     c                   eval      xyoccclm    = *blanks
     c                   eval      xygrossnet  = *blanks
     c                   endif

     c                   endsr

     p getDedTblInf    e
      *=========================================================================
      *==========================================================
      * wrtDedWrkFil - consolidate and write payment info for
      *                policy or claim
      *==========================================================
     p wrtDedWrkFil    b
     d wrtDedWrkFil    pi
     d svcase          s                   like(CMCASE)
     d svclm#          s                   like(MCCLM#)
     d kkcase          s                   like(MCCASE)
     d n               s              5  0

     c     cmky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c     cmky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c     mcky          klist
     c                   kfld                    CMCO#
     c                   kfld                    CMFND
     c                   kfld                    CMEMP#
     c                   kfld                    CMDIV
     c                   kfld                    CMFYR
     c                   kfld                    kkcase

     c     paky          klist
     c                   kfld                    CMCO#
     c                   kfld                    CMFND
     c                   kfld                    CMFYR
     c                   kfld                    CMCASE

     c     d4ky          klist
     c                   kfld                    CMCO#
     c                   kfld                    CMFND
     c                   kfld                    CMEMP#
     c                   kfld                    CMDIV
     c                   kfld                    CMFYR
     c                   kfld                    svclm#

     c                   clear                   cs#
     c                   clear                   csb
     c                   clear                   csn
     c                   clear                   cst
     c                   clear                   mc#
     c                   clear                   mct

     c     cmky          setll     pcm0l
     c     cmky          reade     pcm0l
     c                   dow       not(%eof)

     c                   move      *zeros        kkcase
     c                   move      CMCASE        kkcase
     c     mcky          chain     pmc01
     c                   if        %found and xyOccClm = 'O'
     c                   eval      svclm# = MCCLM#
     c                   else
     c                   eval      svclm# = 0
     c                   endif
      * Populate multi claim and case array for later use in tracking payment
      * and billed totals
     c                   if        svclm# <> 0
     c                   eval      n = %lookup(svclm#:mc#)
     c                   if        n = 0
     c                   eval      n = %lookup(0:mc#)
     c                   eval      mc#(n) = svclm#
     c                   endif
     c                   endif
     c                   eval      n = %lookup(0:cs#)
     c                   eval      cs#(n) = CMCASE

     c     paky          setll     ppa01
     c     paky          reade     ppa01
     c                   dow       not(%eof)

      * Ensure payment is in deductible reserve hierarchy

     c                   eval      n = %lookup(PARECD:rtc)
     c                   if        n = 0
     c     paky          reade     ppa01
     c                   iter
     c                   endif

      * Ensure payment date <=  loss date and not a net deductible recovery paym

     c     PATRDT        ifgt      xxlsdt
     c     PABR2         oreq      FNMEP1
     c     PABR2         oreq      FNCOP1
     c     PABR2         oreq      FNEXP1
     c     PABR2         oreq      FNLEP1
     c     paky          reade     ppa01
     c                   iter
     c                   endif

     c                   eval      D2CO#  = CMCO#
     c                   eval      D2FND  = CMFND
     c                   eval      D2EMP# = CMEMP#
     c                   eval      D2DIV  = CMDIV
     c                   eval      D2FYR  = CMFYR
     c                   eval      D2CLM# = svclm#
     c                   eval      D2CASE = CMCASE
     c                   eval      D2SEQ# = PASEQ#
     c                   eval      D2AMT  = PAAMT
     c                   eval      D2TRDT = PATRDT
     c                   eval      D2TRTM = PATIME
     c                   eval      D2PODT = PAPODT
     c                   eval      D2BR2  = PABR2
     c                   eval      D2BR7  = PABR7
     c                   eval      D2TRCD = PATRCD
     c                   eval      D2TYPE = PATYPE
     c                   eval      D2RECD = PARECD
     c                   eval      D2RETP = PARETP
     c                   write     pd20p

     c     paky          reade     ppa01
     c                   enddo

     c     cmky          reade     pcm0l
     c                   enddo

     p wrtDedWrkFil    e
      *==========================================================
      * calcNonBillA - calc billed/non-billed amount for claim
      *                payments
      *==========================================================
     p calcNonBillA    b
     d calcNonBillA    pi
     d prvOccTotal     s             13  2
     d prvAggTotal     s             13  2
     d curOccTotal     s             13  2
     d curAggTotal     s             13  2
     d paymentAmt      s                   like(PAAMT)
     d occLimit        s                   like(LDAMP1)
     d aggLimit        s                   like(LDAMG1)
     d occBilledAmt    s                   like(PAAMT)
     d aggBilledAmt    s                   like(PAAMT)
     d dedBilledAmt    s                   like(PAAMT)
     d n               s              5  0

     c     d2ky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c                   eval      occLimit = xyAmtPerOcc
     c                   eval      aggLimit = xyAmtAgg
     c     d2ky          setll     pd20l
     c     d2ky          reade     pd20l
     c                   dow       not(%eof)
     c                   eval      paymentAmt = payValue(D2AMT:D2TYPE)
      * Keep agg total for policy to compare to aggregate limit
      * Calculate how much of payment is billed or not due to aggregate limit
     c                   if        aggLimit > 0
     c                   eval      prvAggTotal = curAggTotal
     c                   eval      curAggTotal = curAggTotal + paymentAmt
     c                   eval      aggBilledAmt =  getBilledAmt( paymentAmt  :
     c                                                           prvAggTotal :
     c                                                           curAggTotal :
     c                                                           aggLimit     )
     c                   else
     c                   eval      aggBilledAmt = paymentAmt
     c                   endif
      * Keep occ totals for claim or occurrence to compare to occurrence limit
      * Calculate how much of payment is billed or not due to occurrence limit
     c                   if        occLimit > 0
     c                   if        D2CLM# <> 0
     c                   eval      n = %lookup(D2CLM#:mc#)
     c                   if        n <> 0
     c                   eval      prvOccTotal = mct(n)
     c                   eval      mct(n) = mct(n) + paymentAmt
     c                   eval      curOccTotal = mct(n)
     c                   endif
     c                   else
     c                   eval      n = %lookup(D2CASE:cs#)
     c                   if        n <> 0
     c                   eval      prvOccTotal = cst(n)
     c                   eval      cst(n) = cst(n) + paymentAmt
     c                   eval      curOccTotal = cst(n)
     c                   endif
     c                   endif
     c                   eval      occBilledAmt =  getBilledAmt( paymentAmt  :
     c                                                           prvOccTotal :
     c                                                           curOccTotal :
     c                                                           occLimit     )
     c                   else
     c                   eval      occBilledAmt = paymentAmt
     c                   endif
      * The amount of deductible billed on a payment is the lesser of the amount
      * the occurrence limit or to the aggregate limit
     c                   select
     c                   when      %abs(occBilledAmt) < %abs(aggBilledAmt)
     c                   eval      dedBilledAmt = occBilledAmt
     c                   other
     c                   eval      dedBilledAmt = aggBilledAmt
     c                   endsl

     c                   eval      n = %lookup(D2CASE:cs#)
     c                   if        n <> 0
     c                   eval      csb(n) = csb(n) + dedBilledAmt
     c                   eval      csn(n) = csn(n) + (paymentAmt - dedBilledAmt)
     c                   endif

     c     d2ky          reade     pd20l
     c                   enddo

     p calcNonBillA    e
      *==========================================================
      * payValue - returns neg amount of type is 'R','S', or 'V'
      *==========================================================
     p payValue        b
     d payValue        pi                  like(PAAMT)
     d  paymentAmt                         like(PAAMT)
     d  paymentType                        like(PATYPE)
     c                   if        paymentType = 'R' or
     c                             paymentType = 'S' or
     c                             paymentType = 'V'
     c                   eval      paymentAmt = paymentAmt * -1
     c                   endif
     c                   return    paymentAmt
     p payValue        e
      *==========================================================
      * getBilledAmt - returns amount billed under limit if any
      *==========================================================
     p getBilledAmt    b
     d getBilledAmt    pi                  like(PAAMT)
     d  paymentAmt                         like(PAAMT)
     d  prvTotalAmt                  13  2
     d  curTotalAmt                  13  2
     d  limitAmt                           like(LDAMP1)
     d  amtToLimit     s                   like(PAAMT)

     c                   if        paymentAmt = 0
     c                   return    0
     c                   endif

      * if over limit (prvTotalAmt > limitAmt)
      *   return 0 if curTotalAmt > limitAmt, none of the payment is billed
      *   return diff if over limit and and new curTotalAmt < limitAmt
      * if limit not met (prvTotalAmt < limitAmt
      *   return paymentAmt if limitAmt >= curTotalAmt, it's all billed
      *   return diff if curTotalAmt > limitAmt, it's partially billed

     c                   select
     c                   when      prvTotalAmt >= limitAmt
     c                   select
     c                   when      curTotalAmt >= limitAmt
     c                   return    0
     c                   when      curTotalAmt < limitAmt
     c                   return    curTotalAmt - limitAmt
     c                   endsl
     c                   other
     c                   select
     c                   when      curTotalAmt <= limitAmt
     c                   return    paymentAmt
     c                   when      curTotalAmt >  limitAmt
     c                   return    limitAmt - prvTotalAmt
     c                   endsl
     c                   endsl

     p getBilledAmt    e
      *==============================================================
     pUpdateXMLPrep    b
      /free
       Chain xmlFile xp101;
       If not(%found);
          Return;
       Endif;
       XPco# = xxCo#;
       If xxGrpsel = *on;
          XPfnd = 0;
       Else;
          XPfnd = xxFnd;
       Endif;
       If singleEmpRun;
          XPemp# = xsEmp#;
          XPdiv  = xsDiv;
          If xxDart <> *blanks;
             XPrmcd = xxDart;
             XPdart = 'Y';
          Endif;
       Endif;
       XPuser = Q1user;
       XPpgm  = Q1pgm;
       //*** Beg Add ***** 01/20/25 *******************************
       //*** XPdt   = xxDate;
       XPdt   = SYSDT;
       //*** End Add ***** 01/20/25 *******************************
       XPtime = DStime;
       Update xp101;
      /end-free
     pUpdateXMLPrep    e
      *==============================================================
     pWrtRptHdr        b
      /free
       //*** Beg Chg ***** 01/20/25 *******************************
       //*** xmlD0xmlkey = xmlFile;
       xmlD0xmlkey = XPxmlT;
       xmlD0pedt   = %Editc(xxPoDt:'X');
       xmlD0pssn   = xxY4n;
       //*** End Chg ***** 01/20/25 *******************************
       xmlD0co#    = xxCo#;
       xmlD0cNam   = COname;
       xmlD0rNam   = ORtitl;
       xmlD0pNam   = Q1pgm;
       //*** Beg Chg ***** 01/20/25 *******************************
       // xmlD0RnDt   = %Char(%Date(xxDate:*usa):*usa);
       // xmlD0RnTm   = %Char(%Time(DStime:*hms):*hms);
       // xmlD0AsOf   = %Char(%Date(xxLlDt:*usa):*usa);
       // xmlD0PoDt   = %Char(%Date(xxPoDt:*usa):*usa);
       xmlD0RnDt   = %Editc(xxDate:'X');
       xmlD0RnTm   = %Char(%Time(DStime:*hms):*hms);
       xmlD0AsOf   = %Editc(xxLlDt:'X');
       xmlD0PoDt   = %Editc(xxPoDt:'X');
       //*** End Chg ***** 01/20/25 *******************************
       xmlD0bTyp   = x2DdTp;
       xmlD0ExFl   = xxExcl;
       If singleEmpRun;
          xmlD0msfl   = 'S';
       Else;
          xmlD0msfl   = 'M';
       Endif;
       Write pdd001;
      /end-free
     pWrtRptHdr        e
      *==============================================================
     pWrtFndHdr        b
      /free
       //*** Beg Chg ***** 01/20/25 *******************************
       //*** xmlD1xmlkey = xmlFile;
       xmlD1xmlkey = XPxmlT;
       //*** End Chg ***** 01/20/25 *******************************
       xmlD1co#    = xxCo#;
       xmlD1Fnd    = xxFnd;
       xmlD1fnam   = xxFNam;
       Write pdd101;
      /end-free
     pWrtFndHdr        e
      *==============================================================
     pWrtEmpHdr        b
      /free
       //*** Beg Chg ***** 01/20/25 *******************************
       //*** xmlD2xmlkey = xmlFile;
       xmlD2xmlkey = XPxmlT;
       //*** End Chg ***** 01/20/25 *******************************
       xmlD2co#    = ELco#;
       xmlD2Fnd    = ELfnd;
       //*** Beg Chg ***** 01/20/25 *******************************
       // It was passing next Employer number and Division
       // xmlD2Emp#   = ELemp#;
       // xmlD2Div    = ELdiv;
       xmlD2Emp#   = prcEmp#;
       xmlD2Div    = prcDiv;
       //*** End Chg ***** 01/20/25 *******************************
       xmlD2enam   = xxENam;
       Write Pdd201;
      /end-free
     pWrtEmpHdr        e
      *==============================================================
     pWrtEmpDtl        b
      /free
       //*** Beg Chg ***** 01/20/25 *******************************
       //*** xmlD3xmlkey = xmlFile;
       xmlD3xmlkey = XPxmlT;
       xmlD3clm#   = %editc(CMfnd:'X') + %editc(CMfyr:'X') +
                     %editc(CMcase:'X');
       //*** End Chg ***** 01/20/25 *******************************
       xmlD3co#    = ELco#;
       xmlD3Fnd    = ELfnd;
       xmlD3Emp#   = ELemp#;
       xmlD3Div    = ELdiv;
       xmlD3Fyr    = prcFyr;
       xmlD3Case   = prcCase;
       If *in21;
          xmlD3MoFlg  = ' ';
       Else;
          xmlD3MoFlg  = '*';
       Endif;
       xmlD3Clmn   = xxCnam;
       xmlD3ss#    = xxss1 + '-' + xxss2 + '-' + xxss3;
       xmlD3gnfl   = xxGrossNet;
       //*** Beg Chg ***** 01/20/25 *******************************
       // xmlD3acdt   = %subst(%editc(xxAcDt:'X'):1:2) + '/' +
       //               %subst(%editc(xxAcDt:'X'):3:2) + '/' +
       //               %subst(%editc(xxAcDt:'X'):5:4);
       // xmlD3rpdt   = %subst(%editc(xxDtRp:'X'):1:2) + '/' +
       //               %subst(%editc(xxDtRp:'X'):3:2) + '/' +
       //               %subst(%editc(xxDtRp:'X'):5:4);
       xmlD3acdt   = %Editc(xxAcDt:'X');
       xmlD3rpdt   = %Editc(xxDtRp:'X');
       //*** End Chg ***** 01/20/25 *******************************
       xmlD3clst   = CHocst;
       xmlD3cptd   = xxPdCo;
       xmlD3mptd   = xxPdMe;
       xmlD3eptd   = xxPdEx;
       xmlD3tptd   = xxPdTl;
       xmlD3cinc   = xxInCo;
       xmlD3minc   = xxInMe;
       xmlD3einc   = xxInEx;
       xmlD3tinc   = xxInTl;
       xmlD3prvb   = xxDbPv;
       xmlD3curb   = xxDbCu;
       xmlD3nonb   = xxNonB;
       Write Pdd301;
      /end-free
     pWrtEmpDtl        e
      *==============================================================
     pWrtFyrDtl        b
      /free
       //*** Beg Chg ***** 01/20/25 *******************************
       //*** xmlD4xmlkey = xmlFile;
       xmlD4xmlkey = XPxmlT;
       //*** End Chg ***** 01/20/25 *******************************
       xmlD4Co#    = ELco#;
       xmlD4Fnd    = ELfnd;
       //*** Beg Chg ***** 01/20/25 *******************************
       // It was passing next Employer number and Division
       // xmlD4Emp#   = ELemp#;
       // xmlD4Div    = ELdiv;
       xmlD4Emp#   = prcEmp#;
       xmlD4Div    = prcDiv;
       //*** End Chg ***** 01/20/25 *******************************
       xmlD4Fyr    = xxEfYr;
       xmlD4EfDt   = %char(xxDedEffDt : *usa);
       //*** Beg Add ***** 01/20/25 *******************************
       xmlD4EfDt   = %subst(xmlD4EfDt:1:2) + %subst(xmlD4EfDt:4:2)
                     + %subst(xmlD4EfDt:7:4);
       //*** End Add ***** 01/20/25 *******************************
       xmlD4OcClm  = xxOccClm;
       xmlD4AmPo   = xxAmPo;
       xmlD4AmAg   = xxAmAg;
       xmlD4Tt#Cl  = xxT#Cl;
       xmlD4TtPrBl = xxTlDp;
       xmlD4TtCuBl = xxTlDc;
       xmlD4TtBill = xxTlDt;
       xmlD4TtPaid = xxTlTd;
       xmlD4TtNonB = xxTlTn;
       Write Pdd401;
      /end-free
     pWrtFyrDtl        e
      *==============================================================
     pWrtEmpTtl        b
      /free
       //*** Beg Chg ***** 01/20/25 *******************************
       //*** xmlD5xmlkey = xmlFile;
       xmlD5xmlkey = XPxmlT;
       //*** End Chg ***** 01/20/25 *******************************
       xmlD5Co#    = ELco#;
       xmlD5Fnd    = ELfnd;
       //*** Beg Chg ***** 01/20/25 *******************************
       // It was passing next Employer number and Division
       // xmlD5Emp#   = ELemp#;
       // xmlD5Div    = ELdiv;
       xmlD5Emp#   = prcEmp#;
       xmlD5Div    = prcDiv;
       //*** End Chg ***** 01/20/25 *******************************
       xmlD5TtT#Cl = xxF#Cl;
       xmlD5TtPrBl = xxFyDp;
       xmlD5TtCuBl = xxFyDc;
       xmlD5TtBill = xxFyDt;
       xmlD5TtPaid = xxFyTd;
       xmlD5TtNonB = xxFyTn;
       Write Pdd501;
      /end-free
     pWrtEmpTtl        e
      *==============================================================
     pWrtMltClm        b
      /free
       //*** Beg Chg ***** 01/20/25 *******************************
       //*** xmlD6xmlkey = xmlFile;
       xmlD6xmlkey = XPxmlT;
       //*** End Chg ***** 01/20/25 *******************************
       xmlD6Co#    = MCco#;
       xmlD6Fnd    = MCfnd;
       xmlD6Emp#   = MCemp#;
       xmlD6div    = MCdiv;
       xmlD6fyr    = MCfyr;
       xmlD6case   = MCcase;
       xmlD6clm#   = MCclm#;
       Write pdd601;
      /end-free
     pWrtMltClm        e
      *==============================================================
     pWrtFndTtl        b
      /free
       //*** Beg Chg ***** 01/20/25 *******************************
       //*** xmlD7xmlkey = xmlFile;
       xmlD7xmlkey = XPxmlT;
       //*** End Chg ***** 01/20/25 *******************************
       xmlD7Co#    = ELco#;
       xmlD7Fnd    = ELfnd;
       xmlD7e#cl   = xxE#cl;
       xmlD7TtPrBl = xxEmDp;
       xmlD7TTCuBl = xxEmDc;
       xmlD7TtBill = xxEmDt;
       xmlD7TtPaid = xxEmTd;
       xmlD7TtNonB = xxEmTn;
       Write pdd701;
      /end-free
     pWrtFndTtl        e
      *==============================================================
     pWrtRptTtl        b
      /free
       //*** Beg Chg ***** 01/20/25 *******************************
       //*** xmlD8xmlkey = xmlFile;
       xmlD8xmlkey = XPxmlT;
       //*** End Chg ***** 01/20/25 *******************************
       xmlD8Co#    = xxCo#;
       xmlD8R#Cl   = xxR#Cl;
       xmlD8TtPrBl = xxRmDp;
       xmlD8TtCuBl = xxRmDc;
       xmlD8TtBill = xxRmDt;
       xmlD8TtPaid = xxRmTd;
       xmlD8TtNonB = xxRmTn;
       Write Pdd801;
      /end-free
     pWrtRptTtl        e
      *==============================================================
