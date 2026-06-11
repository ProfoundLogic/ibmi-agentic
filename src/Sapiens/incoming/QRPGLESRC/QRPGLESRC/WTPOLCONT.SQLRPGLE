     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WTPOLCONT - Policy Contacts.
     H DFTNAME(WTPOLCONT)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP('QILE')
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')

     h**********************************************************************
      *===============================================================*
      *  Created   : 07/20/12                                         *
      *  Programmer: C:279                                            *
      *  Project   : WC2115                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      * 05/26/25  C:333 Project: WC3009                               *
      *           Clms, Policy, Agncy contacts: Maint Info            *
      *           - Adding LastUsed and LastUsedBy                    *
      *           DSPF Changes                                        *
      *           - Adding scPLstUpd,scALstUpd,scPLstUpdB,scALstUpdB  *
      *             as hidden Fields in SFLs for Details box.         *
      *                                                               *
      * 11/18/24 C:333 WC2826 - 117560                                *
      *                -Agency Contact grid, Distribution type is not *
      *                 coming for Producer.                          *
      *                -Duplicate Internal Agency contacts.           *
      *                -DSPF changes:                                 *
      *                 ID for distribution type detail.              *
      *                                                               *
      * 10/28/24 C:333 WC2826 - 117560                                *
      *                -Policy Contact grid 'Division' Column.        *
      *                                                               *
      * 10/28/24 C:333 WC2826 - 117731                                *
      *                -When adding a Policy Contact the Contact Type *
      *                 should only pull from the Contact Types Entered
      *                 Policy Contact Grid.                          *
      *                DSPF- Choice selection criteria for Policy Cont*
      *                                                               *
      * 09/24/24 C:342 Ticket: 117552                                 *
      *                1.Show Panel name in Add mode for Policy Con   *
      *                DSPF Changes                                   *
      *                1.Increase the width of Distribution Type,     *
      *                  Sub Categories and Process Output Control    *
      *                  multi select box on rcdAddAgCo               *
      *                                                               *
      * 09/25/24 C:344 Support: 117601                                *
      *                Verify email address is specified when any of  *
      *                the multi-select boxes are populated.          *
      *                                                               *
      * 09/13/24 C:333 WC2826 - 117560                                *
      *                Claims Contacts.                               *
      *                - Adding new column in Policy Contact Grid.    *
      *                 (For Staff Code).                             *
      *                                                               *
      * 08/30/24 C:342 WC2826/117552                                  *
      *               DSPF changes:                                   *
      *               1. Changed choice option field for Distribution *
      *                  type to Show Distribution code - Description *
      *                                                               *
      * 08/29/24 C:333 WC2826 - 117276/116980                         *
      *                Claims Contacts.                               *
      *                - Writing/Updating/Deleting Staff Assignments  *
      *                to WMECP with unique seq#.                     *
      *                                                               *
      * 04/11/24 C:340 Project: 129WXP                                *
      *                Add Distribution Types before Status on Grid   *
      *                Fix Seq# Issue across Divisions.               *
      *                Add Div# as Key to WMECPcrt.                   *
      *                                                               *
      * 03/22/24 C:333 Project: WC2826                                *
      *                Claims Contacts.                               *
      *                 DSPF - Contact type added to Policy grid and  *
      *                        detail box.                            *
      *                      - Contact Type dropdown in Policy Add/Edi*
      *                        t screen.                              *
      *                                                               *
      * 03/07/23 C:79  Support: 150/107500                            *
      *                Increase division in array from 3.0 to 5.0     *
      *                                                               *
      * 02/28/22 C:33  Support: 98710                                 *
      *                Agency Contacts do not filter correctly.       *
      *                                                               *
      * 01/27/22 C:300 Support: 100656                                *
      *                System Flag error: SWCPF flag  WCFL38 vs WCFL48*
      *                                                               *
      * 06/22/21 C:295 Project: WC2302                                *
      *                Auto Tab Closure on Deletion                   *
      *                                                               *
      * 04/20/21 C:298 Project: WC2278                                *
      *                Multiple selection entries are added for       *
      *                Distribution Types,Sub-Categories,Process      *
      *                Output Control and their handling.             *
      *                                                               *
      * 02/23/21 C:318 Support: 94575                                 *
      *                Corrected condition for editing contacts in    *
      *                inquiry mode                                   *
      *                                                               *
      * 02/24/20 C:180 Project: WC2679                                *
      *                Do not allow update if contacts are part of    *
      *                a division that was renewed to quote.          *
      *                                                               *
      * 10/04/19 C:33  Support: 83612                                 *
      *                Read through all sub-file records.             *
      *                                                               *
      * 10/23/18 C:180 Project: 129WVH                                *
      *                Add contact security for client 129.           *
      *                                                               *
      * 02/06/18 C:291 Support: 63953                                 *
      *                Change Division dropdown "max choices" property*
      *                to 100,000. Screen change only.                *
      *                Modified Records:  rcdTabPan, rcdPConFlt,      *
      *                rcdAConFlt, rcdAddCon.                         *
      *                Modified rcdAddCon Division dropdown: Propery  *
      *                order by EMDIV and choice option field.        *
      *                                                               *
      * 01/18/18 C:AZ  Support: PHW51486                              *
      *                Corrected the disabled property on the display *
      *                file WTPOLCONT, data element DivisionDropDown. *
      *                Changed it to 'false'. The division drop down  *
      *                is unlocked now.
      *                                                               *
      * 12/21/16 C:306 Support: 44648                                 *
      *                Corrected the contact save issue. In add mode, *
      *                RcdUpdatedErr procedure should not be executed.*
      *                                                               *
      * 01/07/16 C:65  Project: WC2401                                *
      *                Change the coding done under WC2254 to call an *
    *                API instead of directly calling the various    *
      *                programs for windows here.                     *
      *                                                               *
      *    09/17/15 C:302 Project: WC2254                             *
      *                   Add a framework to be able to call any      *
      *                   Programn as a popup window rather than as   *
      *                   a new tab.                                  *
      *                                                               *
      *  07/02/15 C:306 Support: SEQ86962 / 31805                     *
      *                 Added the email validation check on Policy    *
      *                 contact add/edit popup.                       *
      *                                                               *
      * 05/25/15 C:306 Support: 34935                                 *
      *                Support: 34395 was wrongly mentioned in V15R3  *
      *                Corrected in this V15R4 release.               *
      *                Recompile only                                 *
      *                                                               *
      * 04/16/15 C:65  Project: SY251                                 *
      *                Add API call to launch new Department          *
    *                letters.                                       *
      *                                                               *
      * 04/17/15 C:242 Project: 132WQS                               *
      *                Retrofit of Custom Code to Base for CompSuite:*
      *                Added Preferred Billing Method.               *
      *
      *    10/08/14 C:291 RED 3161                                    *
      *                   Change the date definition on the grid from *
      *                   Number to date data type.                   *
      *                                                               *
      *    07/28/14 C:295 Project: 114WMD                             *
      *                   Compilation only as the changes are in      *
      *                   Action Panel                                *
      *                                                               *
      *    09/03/13 C:189 Redmine: 3015                               *
      *                   Use include instead of ignore for action    *
      *                   panel record format                         *
      *                                                               *
      *    05/07/13 C:191 Redmine: 2840 (Project CI067)               *
      *                   Add SR email notification flag              *
      *                                                               *
      *    04/24/13 C:279 Support: Redmine #2477                      *
      *                   Enable year drop down for year selection.   *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fwtpolcont cf   e             workstn extfile('WTPOLCONT')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflPolCont:rrnPolCon)
     f                                     Sfile(sflAgnCont:rrnAgnCon)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Include(rcdPolPnl)
     f                                     Include(rcdQtePnl)
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwmecp     uf   e             disk    rename(pec01:pec0p)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwmexl     if   e           k disk    rename(pex01:pex00)
     fwmahl     if   e           k disk    rename(pah01:pah00)
     fwmaal1    if   e           k disk    rename(paa01:paa01)
     fwtayl1    if   e           k disk    rename(pay01:pay01)
     fwdell     if   e           k disk    rename(pel01:pel00)
     f                                     infds(dbfds1)
     fwma5l     if   e           k disk    rename(pac01:pac00)
     f                                     infds(dbfds2)
     fwmecl     if a e           k disk    rename(pec01:pec00)
     f                                     InfDs(dbfds3)
      *** Beg Chg ***** 08/29/24 ***********************************
     f*wmecl1    if   e           k disk    rename(pec01:pec01)
     fwmecl1    uf a e           k disk    rename(pec01:pec01)
      *** End Chg ***** 08/29/24 ***********************************
     f                                     InfDs(dbfds5)
     fststl3    if   e           k disk    rename(pst01:pst03)
      *  Workers Compensation Control File
     fssylf     IF   E           K DISK
     fsmupl     IF   E           K DISK    prefix(SM_)
     fwmecpcrt  uf a e           k disk    rename(pec01:pec0p_cr)
     f                                     prefix(CR_)
     fwmecl2    if   e           k disk    rename(pec01:pec02)
     f                                     infds(dbfds6)
     fwmecl3    if   e           k disk    rename(pec01:pec03)
     f                                     infds(dbfds7)
     fwmapl     if   e           k Disk    rename(pap01:pap01)
     f                                     infds(dbfds4)
     fwmagl     if   e           k disk    rename(pag01:pag00)
      *** Beg Del ***** 04/11/24 *** 129WXP ******************
     f***wma5pcrt  if   e             disk    rename(pac01:pac0p)
      *** End Del ***** 04/11/24 *** 129WXP ******************
     fwmahl5    if   e           k disk    rename(pah01:pah05)
      *** Beg Add ***** 04/11/24 *** 129WXP ******************
     fwmeclcrt  if   e           k disk    rename(pec01:pec0p_cl)
     f                                     prefix(CL_)
     fwma5pcrt  if a e           k disk    prefix(PR_)
     f
     fSTDsl     if   e           k disk
      *** End Add ***** 04/11/24 *** 129WXP ******************
      *** Beg Add ***** 08/29/24 *****************************
     fsmupl1    if   e           k disk    rename(pup01:pup02)
     f                                     infds(dbfds8)
      *** End Add ***** 08/29/24 *****************************
      *** Beg Add ***** 11/18/24 *****************************
     fwmappcrt  if a e           k disk    prefix(AP_)
     f                                     rename(pap01:pap0p)
      *** End Add ***** 11/18/24 *****************************
      /copy sCopyPolPr
      /copy sPrSql

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d dbfds2          ds
     d  rrnwma5l             397    400b 0
     d dbfds3          ds
     d  rrnwmecl             397    400b 0
     d dbfds5          ds
     d  rrnwmecl1            397    400b 0
     d dbfds4          ds
     d  rrnwmapl             397    400b 0
     d dbfds6          ds
     d  rrnwmecl2            397    400b 0
     d dbfds7          ds
     d  rrnwmecl3            397    400b 0
      **** Beg Add *** 08/29/24 ****************************
     d dbfds8          ds
     d  rrnsmupl1            397    400b 0
      **** Beg End *** 08/29/24 ****************************
     Ddswma5pcrt     e ds                  extname(wma5pcrt)  qualified inz
     Ddswmecpcrt     e ds                  extname(wmecpcrt)  qualified inz
       //***  Begin Add  ***  02/28/22  *******************
     d#ofDiv           c                   99999
     d#ofProd          c                   25
     daryAgtProd       ds                  likeds(dsAgtProd) dim(#ofProd) inz
     daryDiv           ds                  likeds(dsDiv) dim(#ofProd)
     ddsAgtProd        ds                  qualified
     d key                           23
     d group                          3  0 overlay(key:1)
     d agt#                           5  0 overlay(key:4)
     d prod                           5  0 overlay(key:9)
     d seq#                          15  0 overlay(key:9)
     ddsDiv            ds                  qualified
     d*** Beg Chg *** 03/07/23 *********************************************
     d***** div                            3  0 dim(#ofDiv)
     d div                            5  0 dim(#ofDiv)
     d*** End Chg *** 03/07/23 *********************************************
     d
     ddsWMAHP          ds                  inz qualified
     d co#                            3  0
     d fnd                            3  0
     d emp#                           9  0
     d div                            5  0
     d fyr                            3  0
     d efDt                           7  0
     d agt#1                          5  0
     d prod1                          5  0
     d agt#2                          5  0
     d prod2                          5  0
     d agt#3                          5  0
     d prod3                          5  0

      **** Beg Add *** 08/29/24 ****************************
     d                 ds                  inz
     d  StafCodeAgt            1      6
     d                                     dim(2)
     d  AGsvcr                 1      3
     d  AGmktr                 4      6
      *** Beg Add ***** 11/18/24 *****************************
     d  xxSVCR_lst                    3    dim(50)
     d                                     inz
     d  xxMKTR_lst                    3    dim(50)
     d                                     inz
     d  nxt_pos                       2  0
      *** End Add ***** 11/18/24 *****************************

      ***Staff codes for fields from WMEMP file for Internal Policy Contacts
     d                 ds                  inz
     d  StafCodePol            1     21
     d                                     dim(7)
     d  EMpsrv                 1      3
     d  EMarvw                 4      6
     d  EMclmc                 7      9
     d  EMuwtr                10     12
     d  EMamgr                13     15
     d  EMpacc                16     18
     d  EMlrep                19     21

     d  i              s              1  0
     d  inCo#          s                   like(AGco#)
     d  inFein         s                   like(AGfein)
     d  inMod          s                   like(AGmod)
     d  sysFlag        s              1
     d xxfsn           s             15
     d xxlsn           s             30
     d xxlen           s              2  0
     d spacePos        s              2  0
      **** End Add *** 08/29/24 ****************************
      **** Beg Add *** 09/13/24 ****************************
     d xxusnm          s             50A
      **** End Add *** 09/13/24 ****************************

     DindxDiv          s              6  0
       //***  End   Add  ***  02/28/22  *******************

     d ##co            s                   like(ELco#)
     d ##fnd           s                   like(ELfnd)
     d ##emp           s                   like(ELemp#)
     d ##div           s                   like(ELdiv)
     d ##fyr           s                   like(ELfyr)
     d ##key           s             10
     d ##name          s            100
     d ##pdsc          s            100
     d ##mode          s              1    inz
     d ##trim          s              1    inz('1')
     d ##wild          s              1    inz(' ')
     d ##start         s              3  0 inz(1)
     d ##trans         s              1    inz('0')
     d ##patlen        s              3  0 inz(40)
     d ##strlen        s              3  0 inz(100)
     d ##scnres        s              3  0

     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     D qteMode         s               n   inz(*off)
     D quoteMode       s              1    inz(' ')
     d isValid         s               n   inz(*off)
     d clientID        s              3    inz('000')
     d kkagt#          s                   like(AHagt#)
     d kkrank          s              1
     d kkmod           s              5  0
     d mode            s              1
     d meetPol         s              1
     d meetAgn         s              1
     d process         s               n   inz(*off)
     d procRcd         s              1
     d saveFyr         s                   like(ELfyr)
     d svYear          s                   like(ELfyr)
     d scco#           s                   like(ELco#)
     d scfnd           s                   like(ELfnd)
     d scemp#          s                   like(ELemp#)
     d scdiv           s                   like(ELdiv)
     d scfyr           s                   like(ELfyr)
     d saveEffDt       s                   like(ELpfdt)
     d xxSt            s              2
     d xxRrn           s              9  0
     d xxKey           s             10
     d xxMode          s              1
     d xxPxSt          s              2
     D @@mode          S              1
     d @@next          s             10
     d outIndex        s              5
     d flgRcdUpdated   s              1
     d SVdt            s              7  0
     d SVtime          s              6  0
     d ##type          s              1
     d strContUpd      s              1    inz
     D isDisplay       s               n   inz(*Off)
     d strDisVal       s             25    inz(*blanks)
     d strSubVal       s             25    inz(*blanks)
     d strOCoVal       s            100    inz(*blanks)
       //***  Begin Del  ***  02/28/22  *******************
     d*RecFound        s               n   inz(*Off)
     d*RecFound1       s               n   inz(*Off)
       //***  End   Del  ***  02/28/22  *******************
       //***  Begin Add  ***  02/28/22  *******************
     d strSql          s           5000    varying inz
       //***  End   Add  ***  02/28/22  *******************
     d intrrn          s              9  0
     d maxseq#         s             15  0
     d kkprod          s                   like(AHprod)
     d kkFein          s              9  0
     d kkMod1          s              3  0
     d intDiv          s              5  0 inz
     d agt             s              5  0 DIM(999)
     d found           s              2  0 inz
     D indx            s              3  0

     d dsPFilter       ds                  likerec(RcdPConFlt)
     d dsAFilter       ds                  likerec(RcdAConFlt)

     d* Clear Subfiles
     d ClrSubFiles     pr

     d* get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltID                       3

     d* load agency contact filter clear butoon
     d loadClrFlt      pr

       //***  Begin Del  ***  02/28/22  *******************
     d* load agency contact
     d*loadAgent       pr
     d*  kkagt#                            like(kkagt#)
     d*  kkrank                            like(kkrank) Const
       //***  End   Del  ***  02/28/22  *******************

     d* load Selection fields
     d loadSelection   pr

     d* load policy contact grid
     d loadPContact    pr

     d* load agency contact information
     d loadAContact    pr

       //*** Beg Add *** 08/29/24 *********************************
      * Load Internal Agency Contacts
     dLoadWmagp        pr
     d inCo#                               like(AGco#)
     d inFein                              like(AGfein)
     d inMod                               like(AGmod)
       //*** End Add *** 08/29/24 *********************************

     d* main Processing
     d main            pr

     d* filter for policy contact
     d meetPolFlt      pr

     d****  Begin Del  ***  02/28/22  ***************
     d* filter for agency contact
     d*meetAgnFlt      pr
     d****  End   Del  ***  02/28/22  ***************

     d* program Start
     d programStart    pr
     d processMe                       n

     d* process addition of contact
     d processAdd      pr

     d* process other options
     d procOther       pr

     d* Validate Add/Edit screen
     d Validate        pr

     d* Add contact
     d addContact      pr

     d* Move database record to screen
     d MoveDbSc        pr

     d qclscan         pr                  extpgm('QCLSCAN')
     d   ##name                            like(##name)
     d   ##strlen                          like(##strlen)
     d   ##start                           like(##start)
     d   ##pdsc                            like(##pdsc)
     d   ##patlen                          like(##patlen)
     d   ##trans                           like(##trans)
     d   ##trim                            like(##trim)
     d   ##wild                            like(##wild)
     d   ##scnres                          like(##scnres)

     DGetContUpd       pr             1
     D inUserID                      10    const

      *** Beg Add ***** 08/29/24 ****************************************
     d* Staff Assignment
     d StaffAssign     pr

      *** Beg Add ***** 08/29/24 ****************************************
     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrString
     d/copy sPrGenErr


     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier
     d/copy wPrAR
     d/copy wPrUsrPrms
     d/copy wPrLoc
     d/copy wPrClaims
     d/copy sPrLetter
     d/copy wPrPolCpy

     c* Employer Master File Key List
     c     keyWMEML      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    ##div

     c     keyWMEXL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd

     c     keyWMECL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp

     c     keyWMECL1     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    ##div
     c                   kfld                    ##code            1

     c     keyWMECL#     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    ##div
     c                   kfld                    scAddLstNm
     c                   kfld                    scAddFstNm
     c                   kfld                    scAddMidNm
     c                   kfld                    scAddPstRs
     c                   Kfld                    ##code            1
     c     keyWMECL3     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    ##code            1

     c     keyWMECl2     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    scAddLstNm
     c                   kfld                    scAddFstNm
     c                   kfld                    scAddMidNm
     c                   kfld                    scAddPstRs
     c                   Kfld                    ##code            1

     c     keyWMAPl      Klist
     c                   Kfld                    ##co
     c                   Kfld                    kkfein
     c                   Kfld                    kkmod1
     c                   Kfld                    kkprod

     c     keyWMAHL5     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp

     c* Employer Detail File Key List
     c     keyWDELL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    kkdiv

     c     keyWDELL#     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    kkdiv
     c                   kfld                    ##fyr

     c     keyTable      klist
     c                   kfld                    ELco#
     c                   kfld                    xxKey

     c     keyWMAHL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp

     c     keyWMAAL1     klist
     c                   kfld                    AHco#
     c                   kfld                    AHfnd
     c                   kfld                    kkagt#

     c     keyWMA5L      klist
     c                   kfld                    AHco#
     c                   kfld                    AAfein
     c                   kfld                    kkmod

     c     keyWTAYL1     klist
     c                   kfld                    ##co
     c                   kfld                    A5acty

     c     keySTSTL3     klist
     c                   kfld                    ##key

     c* Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@rrn             9            wdelp
     c                   Parm                    @@mode                         mode
     c                   Parm                    @@next
     c                   Parm                    @@TabInd          5            Tab index
     c                   Parm                    @@Qte             1            Quote

      /free

       // main program processing
       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Else;
          xxRrn = 3386;
       Endif;

       // Program Mode.
       If %parms >= 2;
          mode = @@mode;
       Endif;

       // Tab Index Mode.
       TabIndex = 0;

       // Quote Environment
       If %parms > 4;
          quoteMode = @@Qte;
          Select;
             When @@Qte = 'Q';
                qteMode = *on;
          Endsl;
       Endif;

       isDisplay = not qteMode;
       ProgramStart(process);

       If qteMode = *on and
          GetRnwlQte(##Co:##Fnd:##Emp:##Div) = 'Y' and
          GetRnwlQteDiv(##Co:##Fnd:##Emp:##Div) <> 'Y';
          isDisplay = *On;
          mode ='D';
       Endif;

       scEnv = quoteMode;
       Select;
       When mode = 'D';
          dspMode = *On;
          dspVisb = *Off;
          DisEditLnk = *On;                           //For Edit link Only
          VisEditLnk = *Off;                          //For Edit link Only
          DisDltLnk = *On;                            //For Delete link Only
          VisDltLnk = *Off;                           //For Delete link Only
            If GetContUpd(q1user) = 'Y' and isDisplay;
             DisEditLnk = *Off;
             VisEditLnk = *On;
             DisDltLnk = *Off;
             VisDltLnk = *On;
             dspMode = *Off;
             dspVisb = *On;
          Endif;

       When mode = 'A';
          addMode = *On;
          dspVisb = *On;
          DisEditLnk = *Off;
          VisEditLnk = *On;
          DisDltLnk = *Off;
          VisDltLnk = *On;
       When mode = 'C';
          chgMode= *On;
          dspVisb = *On;
          DisEditLnk = *Off;
          VisEditLnk = *On;
          DisDltLnk = *Off;
          VisDltLnk = *On;
       Endsl;
       scActMode = mode;

       GetClientID(ClientID);
       scActCltId=ClientID;

       If process;
          //*** Beg Add ***** 08/29/24 ************************************
          StaffAssign();
          //*** End Add ***** 08/29/24 ************************************
          Main();
          @@rrn = %Editc(xxRRN:'X');
       Endif;

       Unlock WMECP;
       *InLr = *On;
       Return;

      /end-free
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     P ClrSubFiles     B
     D ClrSubFiles     PI
      /Free

       rrnPolCon = 0;
       rrnAgnCon = 0;
       clrPolCon = *On;
       clrAgnCon = *On;
       dspPolSub = *Off;
       dspPolCtl = *Off;
       dspAgnSub = *Off;
       dspAgnCtl = *Off;
       Write rcdPolCont;

       //***  Begin Add  ***  02/28/22  ****************
       For indx = 1 to #ofProd;
          For indxDiv = 1 to #ofDiv;
             aryDiv(indx).div(indxDiv) = *loval;
          Endfor;
       Endfor;
       //***  End   Add  ***  02/28/22  ****************

      /End-Free
     P ClrSubfiles     E
       //===================================================================
       // Display/Hide "Clear Filter" button for agency contact filter
       //===================================================================
     P loadClrFlt      B
     D loadClrFlt      PI

      /free

       // display clear filter button
       If dsPFilter.ssPConNam = *blanks and
        dsPFilter.ssPConDiv = *blanks and
        dsPFilter.ssPConCFr = *blanks;
          dspclrfltr = *off;
       Else;
          dspclrfltr = *on;
       Endif;

       If dsAFilter.ssAConNm  = *blanks and
          dsAFilter.ssAConDiv = *blanks and
          dsAFilter.ssAConRnk = *blanks and
          dsAFilter.ssAConAgn = *blanks and
          dsAFilter.ssAConTyp = *blanks;
          dspAClrFlt = *off;
       Else;
          dspAClrFlt = *on;
       Endif;

         dspAClrFlt = *off;
      /end-free
     P loadClrFlt      E
       //===================================================================
       // Load Selection - Load the criteria fields for FYR/DIV drop down boxes.
       //===================================================================
     P LoadSelection   B
     D LoadSelection   PI

     dfyrLine          s             31
     ddivLine          s             49
     dfirst            s               n   inz(*on)
      /free
       xxSt = RatingSt(ELco#:ELrtcd);

       clear fyrChoices;
       fyrChoices = 'ELco# = ' + %char(ELco#) + ' AND ELfnd = '
        + %char(ELfnd) + ' AND ELemp# = ' + %char(ELemp#)
        + ' AND ELdiv = ' + %char(kkdiv);

       clear divChoices;
       divChoices = 'EMco# = ' + %char(ELco#) + ' AND EMfnd = '
        + %char(ELfnd) + ' AND EMemp# = ' + %char(ELemp#);

       Clear polChoices;
       polChoices = 'EMco# = ' + %char(ELco#) + ' AND EMfnd = '
        + %char(ELfnd) + ' AND EMemp# = ' + %char(ELemp#);

       Clear agtChoices;
       agtChoices = 'EMco# = ' + %char(ELco#) + ' AND EMfnd = '
        + %char(ELfnd) + ' AND EMemp# = ' + %char(ELemp#);

      /end-free
     P LoadSelection   E
       //===================================================================
       // Load Policy Contacts Information
       //===================================================================
     P LoadPContact    B
     d LoadPContact    PI
      /free

       // clear the subfile.
       clrPolCon = *Off;
       dspPolSub = *On;
       dspPolCtl = *On;

       Chain keyWMEXL pex00;

       Setll KeyWMECL pec00;
       Reade KeyWMECL pec00;
       dow not %eof(Wmecl);
          //*** Beg Chg ***** 10/28/24 *** 117560 *****************************
          //***If ECdivSpec = 'Y';
          If ECdivSpec = 'Y' and ECdiv = ##div;
          //*** End Chg ***** 10/28/24 *** 117560 *****************************
             //*** Beg Add ***** 08/29/24 ********************************
             ClrPolCont();
             //*** End Add ***** 08/29/24 ********************************
             Exsr LoadConGrd;
          Endif;
          Reade KeyWMECL pec00;
       Enddo;

       Setll KeyWMECL pec00;
       Reade KeyWMECL pec00;
       dow not %eof(Wmecl);
          If ECdivSpec <> 'Y';
             //*** Beg Add ***** 08/29/24 ********************************
             ClrPolCont();
             //*** End Add ***** 08/29/24 ********************************
             Exsr LoadConGrd;
          Endif;
          Reade KeyWMECL pec00;
       Enddo;
       rrnPolCon = 1;

       Begsr LoadConGrd;
          clear sflPolCont;
          meetPol = *on;

          // Validate for filter
          MeetPolFlt();
          If meetPol = *on;

             // Write Employer contact info
             // name
             //*** Beg Chg ***** 08/29/24 ********************************
             //***If EClsn <> *blanks;
             //***   scPConNam =%trim(EClsn)+',' + %trim(ECfsn);
             //***Else;
             //***  scPConNam =%trim(ECfsn);
             //***Endif;
             //***If ECmid <> *blanks;
             //***   scPConNam =%trim(scPConNam) + ',' + %trim(ECmid);
             //***Endif;
             scPConNam = FormatName(EClsn:ECfsn:ECmid:'*FML');
             //*** End Chg ***** 08/29/24 ********************************

             //*** Beg Add ***** 08/29/24 ********************************
             // Address
             FormatAddress(ECadr1:ECadr2:ECcty:ECst:ECzip:scPConAd1:scPConAd2:
              scPConCs);

             // Fax
             If ECfara <> *zeros or ECfphn <> *zeros;
                scPConFax# = '(' + %editc(ECfara:'X') + ')' +
                 %subst(%editc(ECfphn:'X'):1:3) + '-' +
                 %subst(%editc(ECfphn:'X'):4:4);
             Endif;
             //*** End Add ***** 08/29/24 ********************************
             //*** Beg Add ***** 05/26/25 **********************
             scPLstUpd = %Trim(%EditW(dtMDYY(ECdt):'  /  /    '));
             Chain ECuser pup01;
             If %found(SMUPL);
                scPLstUpdB = SM_UPusnm;
             Endif;
             //*** End Add ***** 05/26/25 **********************

             // contact for
             If ECpcnt = '1';
                scPConFor = EXcnt1;
             ElseIf ECpcnt = '2';
                scPConFor = EXcnt2;
             ElseIf ECpcnt = '3';
                scPConFor = EXcnt3;
             //*** Beg Add ***** 08/29/24 ********************************
             ElseIf ECpcnt = 'A';
                scPConFor = 'Policy Services';
             ElseIf ECpcnt = 'B';
                scPConFor = 'Audit Reviewer';
             ElseIf ECpcnt = 'C';
                scPConFor = 'Claim Contact';
             ElseIf ECpcnt = 'D';
                scPConFor = 'Underwriter';
             ElseIf ECpcnt = 'E';
                scPConFor = 'Audit Manager';
             ElseIf ECpcnt = 'F';
                scPConFor = 'Premium Accountant';
             ElseIf ECpcnt = 'G';
                scPConFor = 'Loss Control Representative';
             //*** End Add ***** 08/29/24 ********************************
             Endif;

           //*** Beg Add ***** 04/11/24 *** 129WXP ******************
           // Populate the Distribution Types
           Setll (EcCo#:EcFnd:EcEmp#:EcDiv:EcSeq#) pec0p_cl;
           Reade (EcCo#:EcFnd:EcEmp#:EcDiv:EcSeq#) pec0p_cl;
           scConCrt = ' ';
           Dow not %eof(wmeclcrt);
              If CL_ECcritid = 'Distribution';
                 Chain (EcCo#:CL_ECcritVal) STDsl;
                    If %Found;
                       If scConCrt <> ' ';
                          scConCrt = %Trim(scConCrt) + ', ' + DsDesc;
                       Else;
                          scConCrt = DsDesc;
                       Endif;
                    Endif;
              Endif;
              Reade (EcCo#:EcFnd:EcEmp#:EcDiv:EcSeq#) pec0p_cl;
           Enddo;

           //*** End Add ***** 04/11/24 *** 129WXP ******************

             // position
             scPConPst = ECposn;

             // Phone#
             If ECarea <> *zeros or ECphn# <> *zeros or ECext# <> *zeros;
                scPConPhn# = '(' + %editc(ECarea:'X') + ')' +
                 %subst(%editc(ECphn#:'X'):1:3) + '-' +
                 %subst(%editc(ECphn#:'X'):4:4) + 'x' +
                 %trim(%editc(ECext#:'X'));
             Endif;

             // Cell phone#
             If ECcara <> *zeros or ECcphn <> *zeros;
                scPCnCPhn# = '(' + %editc(ECcara:'X') + ')' +
                 %subst(%editc(ECcphn:'X'):1:3) + '-' +
                 %subst(%editc(ECcphn:'X'):4:4);
             Endif;

             // division
             If ECdivSpec = 'Y';
                scPConDiv = %editc(ECdiv:'X');
             Elseif ECdivSpec <> 'Y';
                scPConDiv = 'All';
             Endif;

             // email
             scDtlCnEm = %trim(ECemal);
             scDtlConEm = %trim(ECemal);
             If scDtlConEm = *blanks;
                scDtlConEm = 'No Email available';
             Endif;
             //*** Beg Add ***** 08/29/24 ********************************
             scPConEml = %trim(ECemal);
             //*** End Add ***** 08/29/24 ********************************

             //*** Beg Add ***** 03/22/24 *********************************
             // Contact Type
             scPConType = %trim(ECtype);
             //*** End Add ***** 03/22/24 *********************************
             //*** Beg Add ***** 08/29/24 ********************************
             sysFlag = ' ';
             Exec SQL Select CNsys Into :sysFlag  From STCNP
             Where CNco# = :##co And CNdesc = :scPConFor
             And CNctxt = 'Policy';

             If sysFlag = 'Y';
                scPInter = 'I';
             Else;
                scPInter = ' ';
             Endif;
             //*** End Add ***** 08/29/24 ********************************
             //*** Beg Add ***** 09/13/24 ********************************
             // Fetching Staff Code from SMUPP for System Contacts (Staff Assignments)
             If ECpcnt = 'A' or ECpcnt = 'B' or ECpcnt = 'C' or ECpcnt = 'D'
              or ECpcnt = 'E' or ECpcnt = 'F' or ECpcnt = 'G';

                xxusnm = FormatName(EClsn:ECfsn:' ':'*FML');

                Exec sql DECLARE mycursor Cursor For
                 Select UPSTAF From SMUPP
                  Where UPUSNM =:xxusnm;

                Exec sql OPEN mycursor;

                Exec sql FETCH NEXT from mycursor
                  INTO :ScPStfCd;

                Exec sql CLOSE mycursor;

             Endif;
             //*** End Add ***** 09/13/24 ********************************

             scPConRRN# = rrnWmecl;

             rrnPolCon = rrnPolCon + 1;
             If rrnPolCon = 1;
                scPolRowSl = 'A';
             Else;
                clear scPolRowSl;
             Endif;
             scCrtRrn = rrnPolCon;
             If mode = 'D';
                dspVisb = *Off;
               DisEditLnk = *On;                           //For Edit link Only
               VisEditLnk = *Off;                          //For Edit link Only
               DisDltLnk = *On;                            //For Delete link Only
               VisDltLnk = *Off;                           //For Delete link Only
               If GetContUpd(q1user) = 'Y' and isDisplay;
                  DisEditLnk = *Off;
                  VisEditLnk = *On;
                  DisDltLnk = *Off;
                  VisDltLnk = *On;
                  dspVisb = *On;
               Endif;
             Else;
                dspVisb = *On;
               DisEditLnk = *Off;                           //For Edit link Only
               VisEditLnk = *On;
               DisDltLnk = *Off;                            //For Delete link Only
               //*** Beg Chg ***** 08/29/24 ******************************
               //***VisDltLnk = *On;
               If scPInter = 'I';
                  VisDltLnk = *Off;
               Else;
                  VisDltLnk = *On;
               Endif;
               //*** End Chg ***** 08/29/24 ******************************
             Endif;
             Write sflPolCont;
          Endif;
       Endsr;

      /end-free
     P LoadPContact    E
       //===================================================================
       // Load agency contact information
       //===================================================================
     P LoadAContact    B
     D LoadAContact    PI

       //***  Begin Add  ***  02/28/22  ***************
     disEofWMAHP       s              1n   inz
       //***  End   Add  ***  02/28/22  ***************
      /free

       // clear the subfile.
       clrAgnCon = *Off;
       dspAgnSub = *On;
       dspAgnCtl = *On;

       //***  Begin Add  ***  02/28/22  ***************
       Clear aryAgtProd;
       //***  End   Add  ***  02/28/22  ***************
       Clear agt;
       indx = 1;
       //***  Begin Chg  ***  02/28/22  **************
       //Setll KeyWMAHL pah00;
       //Reade KeyWMAHL pah00;
       //dow not %eof(Wmahl);
       //   If AHfyr = kkfyr and AHstat <> 'I';
       //      If AHagt# > 0;
       //         LoadAgent(AHagt#:'1');
       //      Endif;
       //      If AHsagt > 0;
       //         LoadAgent(AHsagt:'2');
       //      Endif;
       //      If AH3agt > 0;
       //         LoadAgent(AH3agt:'3');
       //      Endif;
       //   Endif;
       //   Reade KeyWMAHL pah00;
       //Enddo;
       //Clear agt;
       //indx = 1;
       //Setll (##co:##fnd:##emp) pah00;
       //Reade (##co:##fnd:##emp) pah00;
       //Dow not %eof(Wmahl);
       //   If AHfyr = kkfyr and AHstat <> 'I';
       //      If AHprod > 0;
       //         LoadProd(AHprod:'1');
       //      Endif;
       //      If AHsprd > 0;
       //         LoadProd(AHsprd:'2');
       //      Endif;
       //      If AH3prd > 0;
       //         LoadProd(AH3prd:'3');
       //      Endif;
       //   Endif;
       //   Reade (##co:##fnd:##emp) pah00;
       //Enddo;
       strSql = 'with ' + SqlWithPolicyInfo +
        'select AHco#, AHfnd, AHemp#, AHdiv, AHfyr, AHefDt, ' +
        ' AHagt#, AHprod, AHsAgt, AHsPrd, AH3agt, AH3prd ' +
        ' from policy_info' +
        ' where ELco#=' + %char(##co) + ' and ELfnd=' + %char(##fnd) +
        '  and ELemp#=' + %char(##emp) + ' and Ahfyr=' + %char(kkFyr) +
        ' order by AHco#, AHfnd, AHemp#, AHdiv';
       Exec sql close cursorWMAHP;
       Exec sql prepare sqlWMAHP From :strSql;
       Exec sql declare cursorWMAHP cursor for sqlWMAHP;
       Exec sql open cursorWMAHP;
       Reset isEofWMAHP;
       Dou isEofWMAHP;
          Exec sql fetch next from cursorWMAHP into :dsWMAHP;
          Select;
          When not IsValidFetch(sqlstt);
             isEofWMAHP = *on;
             Iter;
          Endsl;
          // Primary Agent
          If dsWMAHP.agt#1 > 0;
             LoadAgent(dsWMAHP.agt#1:'1':dsWMAHP.prod1:dsWMAHP.efDt:
              dsWMAHP.div);
          Endif;
          // Secondary Agent
          If dsWMAHP.agt#2 > 0;
             LoadAgent(dsWMAHP.agt#2:'2':dsWMAHP.prod2:dsWMAHP.efDt:
              dsWMAHP.div);
          Endif;
          // Tertiary Agent
          If dsWMAHP.agt#3 > 0;
             LoadAgent(dsWMAHP.agt#3:'3':dsWMAHP.prod3:dsWMAHP.efDt:
              dsWMAHP.div);
          Endif;
       Enddo;
       Clear aryAgtProd;
       Clear agt;
       Exec sql close cursorWMAHP;
       Exec sql open cursorWMAHP;
       Reset isEofWMAHP;
       Dou isEofWMAHP;
           Exec sql fetch next from cursorWMAHP into :dsWMAHP;
           Select;
           When not IsValidFetch(sqlstt);
              isEofWMAHP = *on;
              Iter;
           Endsl;
           // Primary producer
           If dsWMAHP.prod1 > 0;
              LoadProd(dsWMAHP.prod1:'1':dsWMAHP.agt#1:dsWMAHP.efDt
               :dsWMAHP.div);
           Endif;
           If dsWMAHP.prod2 > 0;
              LoadProd(dsWMAHP.prod2:'2':dsWMAHP.agt#2:dsWMAHP.efDt
               :dsWMAHP.div);
           Endif;
           If dsWMAHP.prod3 > 0;
              LoadProd(dsWMAHP.prod3:'3':dsWMAHP.agt#3:dsWMAHP.efDt
               :dsWMAHP.div);
           Endif;
       Enddo;
       //***  End Chg  ***  02/28/22  **************
       rrnAgnCon = 1;

      /end-free
     P LoadAContact    E
       //===================================================================
       // LoadAgent - Load Agent contact records
      //===================================================================
     P LoadAgent       B
     d LoadAgent       PI
       //***  Begin Chg  ***  02/28/22  ******************
     d**##agt#                        5  0
     d  ##agt#                        5  0 const
       //***  End   Chg  ***  02/28/22  ******************
     d  ##rank                        1    const
       //***  Begin Add  ***  02/28/22  ******************
     d  ##prod                        5  0 const
     d  ##efDt                        7  0 const
     d  ##div                         5  0 const

     ddecCount         s             10i 0 inz
     disEofWMA5P       s              1n   inz
     drrnWMA5P         s              9  0 inz
     ddsWMA5P        e ds                  extname('WMA5P')
     ddsWMAAP        e ds                  extname('WMAAP')
       //***  End   Add  ***  02/28/22  ******************

      /free
       //***  Begin Add  ***  02/28/22  ******************
       Exec sql close cursorWMA5P;
       Exec sql declare cursorWMA5P cursor for
        select a5.*, WMAAP.*, rrn(a5)
         from WMA5P a5
         join WMAAP on
          a5.A5co#=AAco# and a5.A5fein=AAfein and a5.A5mod=AAmod
         join WMA5PCRT crt on
          a5.A5co#=crt.A5co# and a5.A5fein=crt.A5fein
           and a5.A5mod=crt.A5mod and a5.A5seq#=crt.A5seq#
         where AAco#=:##co and AAfnd=:##fnd and AAagt#=:##Agt#
          and crt.A5critId='Producer' and crt.A5critVal=:##prod
          and a5.A5stat <> 'I';
       Exec sql open cursorWMA5P;
       Dou isEofWMA5P;
          Exec sql fetch next from cursorWMA5P into :dsWMA5P,:dsWMAAP,:rrnWMA5P;
          Select;
          When not IsValidFetch(sqlstt);
             isEofWMA5P = *on;
             Iter;
          Endsl;
          dsAgtProd.group = AHfnd;
          dsAgtProd.agt# = ##agt#;
          dsAgtProd.seq# = A5seq#;
          found = %lookup(dsAgtProd:aryAgtProd(*).key);
          Select;
          When found > 0;
             Dou found = 0;
                indxDiv = %lookup(##div:aryDiv(found).div);
                Select;
                When indxDiv = 0;
                   indxDiv = %lookup(*loval:aryDiv(found).div);
                   aryDiv(found).div(indxDiv) = ##div;
                Endsl;
                found = %lookup(dsAgtProd:aryAgtProd(*).key:found+1);
             Enddo;
          When found = 0 and MeetAgnFlt();

             // Write Employer contact info
             // name
             //*** Beg Chg ***** 08/29/24 ********************************
             //***If A5lsn <> *blanks;
             //***   scAConNam =%trim(A5lsn)+',' + %trim(A5fsn);
             //***Else;
             //***   scAConNam =%trim(A5fsn);
             //***Endif;
             //***If A5mid <> *blanks;
             //***   scAConNam =%trim(scAConNam) + ',' + %trim(A5mid);
             //***Endif;
             scAConNam = FormatName(A5lsn:A5fsn:A5mid:'*FML');
             //*** End Chg ***** 08/29/24 ********************************
             //*** Beg Add ***** 08/29/24 ********************************
             // Address
             FormatAddress(A5adr1:A5adr2:A5cty:A5st:A5zip:scAConAd1:scAConAd2:
              scAConCs);

             // Fax
             If A5fara <> *zeros or A5fphn <> *zeros;
                scAConfax = '(' + %editc(A5fara:'X') + ')' +
                 %subst(%editc(A5fphn:'X'):1:3) + '-' +
                 %subst(%editc(A5fphn:'X'):4:4);
             Endif;
             //*** End Add ***** 08/29/24 ********************************
             //*** Beg Add ***** 05/26/25 **********************
             scALstUpd = %Trim(%EditW(dtMDYY(A5dt):'  /  /    '));
             Chain A5user pup01;
             If %found(SMUPL);
                scALstUpdB = SM_UPusnm;
             Endif;
             //*** End Add ***** 05/26/25 **********************

             // Email
             scAConEml = A5emal;
             scAConEmal = A5emal;
             If scAConEmal = *blanks;
                scAConEmal = 'No Email available';
             Endif;
             Clear scACnEffDt;
             // Effective date
             scACnEffDt  = CvtToDate(##efdt);

             // Ranking
             Select;
             When ##rank = '1';
                scAConRnk = 'Primary';
             When ##rank = '2';
                scAConRnk = 'Secondary';
             When ##rank = '3';
                scAConRnk = 'Tertiary';
             Other;
                Clear scAConRnk;
             Endsl;

             // Agency
             scAConAgn = %trim(AAname);

           //*** Beg Add ***** 04/11/24 *** 129WXP ******************
           // Populate the Distribution Types
           Setll (A5co#:A5Fein:A5Mod:A5seq#) wma5pcrt;
           Reade (A5co#:A5Fein:A5Mod:A5seq#) wma5pcrt;
           scAConCrt = ' ';
           Dow not %eof(wma5pcrt);
              If PR_A5critID = 'Distribution';
                 Chain (A5co#:PR_A5critVal) STDsl;
                    If %Found;
                       If scAConCrt <> ' ';
                          scAConCrt = %Trim(scAConCrt) + ', ' + DsDesc;
                       Else;
                          scAConCrt = DsDesc;
                       Endif;
                    Endif;
              Endif;
              Reade (A5co#:A5Fein:A5Mod:A5seq#) wma5pcrt;
           Enddo;

           //*** End Add ***** 04/11/24 *** 129WXP ******************

             // Type
             Clear AYdesc;
             Chain keyWTAYL1 pay01;
             scAConTyp = %trim(AYdesc);

             // Phone#
             If A5area<>*zeros or A5phn#<>*zeros or A5ext#<>*zeros;
                scAConPhn# = '(' + %editc(A5area:'X') + ')' +
                 %subst(%editc(A5phn#:'X'):1:3) + '-' +
                 %subst(%editc(A5phn#:'X'):4:4) + 'x' +
                 %trim(%editc(A5ext#:'X'));
             Endif;

             // Cell phone#
             If A5cara <> *zeros or A5cphn <> *zeros;
                scAConCPhn = '(' + %editc(A5cara:'X') + ')' +
                 %subst(%editc(A5cphn:'X'):1:3) + '-' +
                 %subst(%editc(A5cphn:'X'):4:4);
             Endif;

             // division
             scAConDiv = %char(##div);

             // status
             If A5stat = 'A';
                scAConSts = 'Active';
             Else;
                scAConSts = 'Inactive';
             Endif;

             indx = rrnAgnCon + 1;
             aryAgtProd(indx) = dsAgtProd;
             aryDiv(indx).div(1) = ##div;

             scAConRRN# = rrnWMA5P;

             rrnAgnCon = rrnAgnCon + 1;
             If rrnAgnCon = 1;
                scAgnRowSl = 'A';
             Else;
                Clear scAgnRowSl;
             Endif;
             Write sflAgnCont;
             //*** Beg Add *** 08/29/24 **********************************
             inCo#  = A5co#;
             inFein = A5fein;
             inMod  = A5Mod;
             LoadWmagp(inCo#:inFein:inMod);
             //*** End Add *** 08/29/24 **********************************
          Endsl;
       Enddo;
       Return;
       //***  End   Add  ***  02/28/22  ******************

       //***  Begin Chg  ***  02/28/22  ******************
       //found = %Lookup(##agt#:agt);
       //If found = 0;
       dsAgtProd.group = AHfnd;
       dsAgtProd.agt# = ##agt#;
       dsAgtProd.prod = ##prod;
       found = %lookup(dsAgtProd:aryAgtProd(*).key);
       Select;
       When found > 0;
          Dou found = 0;
             indxDiv = %lookup(AHdiv:aryDiv(found).div);
             Select;
             When indxDiv = 0;
                indxDiv = %lookup(*loval:aryDiv(found).div);
                aryDiv(found).div(indxDiv) = AHdiv;
             Endsl;
             found = %lookup(dsAgtProd:aryAgtProd(*).key:found+1);
          Enddo;
       When found = 0;
       //***  End   Chg  ***  02/28/22  ******************
          intDiv = AHdiv;
          kkrank = ##rank;
          kkagt# = ##agt#;
          Chain keyWMAAL1 paa01;
          If %found(Wmaal1);
             kkmod = aamod;
             Setll KeyWMA5L pac00;
             Reade KeyWMA5L pac00;
             dow not %eof(Wma5l);
                If A5stat <> 'I';
                   clear sflAgnCont;

                   meetAgn = *on;
                 //***  Begin Add  ***  02/28/22  ******************
                   Reset decCount;
                   Exec sql
                    select count(*) into :decCount
                     from WMA5PCRT
                     where A5co#=:A5co# and A5fein=:A5fein and A5mod=:A5mod
                      and A5seq#=:A5seq# and A5critId='Producer'
                      and A5critVal=:##prod;
                   If decCount = 0;
                      meetAgn = *off;
                   Endif;
                 //***  End   Add  ***  02/28/22  ******************

                   // Validate for filter
                   MeetAgnFlt();
                   If meetAgn = *on;

                      // Write Employer contact info
                      // name
                      //*** Beg Chg ***** 08/29/24 ********************************
                      //***If A5lsn <> *blanks;
                      //***   scAConNam =%trim(A5lsn)+',' + %trim(A5fsn);
                      //***Else;
                      //***   scAConNam =%trim(A5fsn);
                      //***Endif;
                      //***If A5mid <> *blanks;
                      //***   scAConNam =%trim(scAConNam) + ',' + %trim(A5mid);
                      //***Endif;
                      scAConNam = FormatName(A5lsn:A5fsn:A5mid:'*FML');
                      //*** End Chg ***** 08/29/24 ********************************
                      //*** Beg Add ***** 08/29/24 ********************************
                      // Address
                      FormatAddress(A5adr1:A5adr2:A5cty:A5st:A5zip:scAConAd1:
                       scAConAd2:scAConCs);

                      // Fax
                      If A5fara <> *zeros or A5fphn <> *zeros;
                         scAConfax = '(' + %editc(A5fara:'X') + ')' +
                          %subst(%editc(A5fphn:'X'):1:3) + '-' +
                          %subst(%editc(A5fphn:'X'):4:4);
                      Endif;
                      //*** End Add ***** 08/29/24 ********************************
                      //*** Beg Add ***** 05/26/25 **********************
                      scALstUpd = %Trim(%EditW(dtMDYY(A5dt):'  /  /    '));
                      Chain A5user pup01;
                      If %found(SMUPL);
                         scALstUpdB = SM_UPusnm;
                      Endif;
                      //*** End Add ***** 05/26/25 **********************

                      // Email
                      scAConEml = A5emal;
                      scAConEmal = A5emal;
                      If scAConEmal = *blanks;
                         scAConEmal = 'No Email available';
                      Endif;
                      clear scACnEffDt;
                      // Effective date
                      scACnEffDt  = CvtToDate(AHefdt);

                      // Ranking
                      If ##rank = '1';
                         scAConRnk = 'Primary';
                      Elseif ##rank = '2';
                         scAConRnk = 'Secondary';
                      Elseif ##rank = '3';
                         scAConRnk = 'Tertiary';
                      Endif;

                      // Agency
                      scAConAgn = %trim(AAname);

                      // Type
                      Chain keyWTAYL1 pay01;
                      scAConTyp = %trim(AYdesc);

                      // Phone#
                      If A5area<>*zeros or A5phn#<>*zeros or A5ext#<>*zeros;
                         scAConPhn# = '(' + %editc(A5area:'X') + ')' +
                          %subst(%editc(A5phn#:'X'):1:3) + '-' +
                          %subst(%editc(A5phn#:'X'):4:4) + 'x' +
                          %trim(%editc(A5ext#:'X'));
                      Endif;

                      // Cell phone#
                      If A5cara <> *zeros or A5cphn <> *zeros;
                         scAConCPhn = '(' + %editc(A5cara:'X') + ')' +
                          %subst(%editc(A5cphn:'X'):1:3) + '-' +
                          %subst(%editc(A5cphn:'X'):4:4);
                      Endif;

                      // division
                    //***  Begin Chg  ***  02/28/22  ******************
                    //scAConDiv = intDiv;
                      scAConDiv = %char(intDiv);
                    //***  End   Chg  ***  02/28/22  ******************

                      // status
                      If A5stat = 'A';
                         scAConSts = 'Active';
                      Else;
                         scAConSts = 'Inactive';
                      Endif;

                    //***  Begin Chg  ***  02/28/22  *******************
                    //agt(indx) = ##agt#;
                      indx = rrnAgnCon + 1;
                      aryAgtProd(indx) = dsAgtProd;
                      aryDiv(indx).div(1) = AHdiv;
                    //***  End   Chg  ***  02/28/22  *******************
                      indx = indx + 1;

                      scAConRRN# = rrnwma5l;

                      rrnAgnCon = rrnAgnCon + 1;
                      If rrnAgnCon = 1;
                         scAgnRowSl = 'A';
                      Else;
                         clear scAgnRowSl;
                      Endif;
                      Write sflAgnCont;
                      //*** Beg Add *** 08/29/24 *************************
                      inCo#  = A5co#;
                      inFein = A5fein;
                      inMod  = A5Mod;
                      LoadWmagp(inCo#:inFein:inMod);
                      //*** End Add *** 08/29/24 *************************
                   Endif;
                Endif;
                Reade KeyWMA5L pac00;
             Enddo;
          Endif;
       //***  Begin Chg  ***  02/28/22  ******************
       //Endif;
       Endsl;
       //***  End   Chg  ***  02/28/22  ******************

      /end-free
     P LoadAgent       E
       //===================================================================
       // LoadProducer - Load Agent contact records
      //===================================================================
     P LoadProd        B
     d LoadProd        PI
       //***  Begin Chg  ***  02/28/22  *******************
     d**##Prod                        5  0
     d  ##Prod                        5  0 const
       //***  End   Chg  ***  02/28/22  *******************
     d  ##rank                        1    const
       //***  Begin Add  ***  02/28/22  *******************
     d  ##agt#                        5  0 const
     d  ##efDt                        7  0 const
     d  ##div                         5  0 const
       //***  End   Add  ***  02/28/22  *******************
      /Free
       intDiv = AHdiv;
      *//***  Begin Chg  ***  02/28/22  *******************
       //Chain (AHco#:AHfnd:AHagt#) paa01;
       Chain (##co:##fnd:##agt#) paa01;
       //***  End   Chg  ***  02/28/22  *******************
       If %found(wmaal1);
          kkfein = AAfein;
          kkmod1 = AAmod;
          kkprod = ##Prod;
          Chain keyWMAPl pap01;
          If %found(wmapl);
           //***  Begin Chg  ***  02/28/22  *******************
           //found = %Lookup(APprod:agt);
           //If found = 0;
             dsAgtProd.group = AAfnd;
             dsAgtProd.agt# = AAagt#;
             dsAgtProd.prod = APprod;
             found = %lookup(dsAgtProd:aryAgtProd(*).key);
             Select;
             When found > 0;
                indxDiv = %lookup(##div:aryDiv(found).div);
                Select;
                When indxDiv = 0;
                   indxDiv = %lookup(*loval:aryDiv(found).div);
                   aryDiv(found).div(indxDiv) = ##div;
                Endsl;
             When found = 0;
           //***  End   Chg  ***  02/28/22  *******************
                If APstat <> 'I';
                   meetAgn = *on;

                   // Validate for filter
                 //***  Begin Del  ***  02/28/22  *******************
                 //MeetAgnFlt();
                 //***  End   Del  ***  02/28/22  *******************
                   If meetAgn = *on;

                      // Write Employer contact info
                      // name
                      //*** Beg Chg ***** 08/29/24 ***********************
                      //***scAConNam = FormatName(APlsn:APfsn);
                      scAConNam = FormatName(APlsn:APfsn:' ':'*FML');
                      //*** End Chg ***** 08/29/24 ***********************
                      //*** Beg Add ***** 08/29/24 ***********************
                      FormatAddress(APmad1:scAConAd2:APmcty:APmst:APmzip:
                       scAConAd1:scAConAd2:scAConCs);
                      //*** End Add ***** 08/29/24 ***********************

                      // Email
                      scAConEml = APEmal;
                      scAConEmal = APEmal;
                      If scAConEmal = *blanks;
                         scAConEmal = 'No Email available';
                      Endif;
                      // Effective date
                      Clear scACnEffDt;

                      scAConRnk = 'Producer';

                      // Agency
                      scAConAgn = %trim(AAname);

                      // Type
                      scAConTyp = 'Producer';

                      // Phone#
                      Chain (##co:kkfein:kkmod) pag00;
                      If %Found(wmagl);
                         If AGphn1<>*Zeros;
                            scAConPhn# = '(' + %EditC(AGare1:'X') + ') ' +
                             %Subst(%EditC(AGphn1:'X'):1:3) + '-' +
                             %Subst(%EditC(AGphn1:'X'):4:4);
                         EndIf;
                      EndIf;

                      // Cell phone#
                         scAConCPhn = *blanks;

                      // division
                    //***  Begin Chg  ***  02/28/22  ******************
                    //scAConDiv = intDiv;
                      scAConDiv = %char(##div);
                    //***  End   Chg  ***  02/28/22  ******************
                    //*** Beg Add ***** 05/26/25 **********************
                    scALstUpd = %Trim(%EditW(dtMDYY(APdt):'  /  /    '));
                    Chain APuser pup01;
                    If %found(SMUPL);
                       scALstUpdB = SM_UPusnm;
                    Endif;
                    //*** End Add ***** 05/26/25 **********************

                      // status
                      If APcxDt <> *zero and APcxDt <= DateTo7(SystemDate());
                         scAConSts = 'Cancelled';
                      Else;
                         If APstat = 'A';
                            scAConSts = 'Active';
                         Endif;
                         If APstat = 'I';
                            scAConSts = 'Inactive';
                         Endif;
                      Endif;

                      //*** Beg Add ***** 11/18/24 *** 117560 ******************
                      // Populate the Distribution Types
                      Setll (APco#:APfein:APmod:APseq#) wmappcrt;
                      Reade (APco#:APfein:APmod:APseq#) wmappcrt;
                      scAConCrt = ' ';
                      Dow not %eof(wmappcrt);
                         If AP_APcritID = 'Distribution';
                            Chain (APco#:AP_APcritVal) STDsl;
                               If %Found;
                                  If scAConCrt <> ' ';
                                     scAConCrt = %Trim(scAConCrt) +
                                        ', ' + DsDesc;
                                  Else;
                                     scAConCrt = DsDesc;
                                  Endif;
                               Endif;
                         Endif;
                         Reade (APco#:APfein:APmod:APseq#) wmappcrt;
                      Enddo;

                      //*** End Add ***** 11/18/24 *** 117560 ******************

                    //***  Begin Chg  ***  02/28/22  *******************
                    //agt(indx) = APprod;
                      indx = rrnAgnCon + 1;
                      aryAgtProd(indx) = dsAgtProd;
                      aryDiv(indx).div(1) = ##div;
                    //***  End   Chg  ***  02/28/22  *******************
                      indx = indx + 1;

                      scAConRRN# = rrnwmapl;

                      rrnAgnCon = rrnAgnCon + 1;
                      If rrnAgnCon = 1;
                         scAgnRowSl = 'A';
                         Else;
                            clear scAgnRowSl;
                         Endif;
                         Write sflAgnCont;
                         //*** Beg Add *** 08/29/24 **********************
                         inCo#  = APco#;
                         inFein = APfein;
                         inMod  = APMod;
                         LoadWmagp(inCo#:inFein:inMod);
                         //*** End Add *** 08/29/24 **********************
                      Endif;
                   Endif;
              //***  Begin Chg  ***  02/28/22  *******************
              //Endif;
                Endsl;
              //***  End   Chg  ***  02/28/22  *******************
             Endif;
       Endif;
      /end-free
     P LoadProd        E
       //*** Beg Add *** 08/29/24 *******************************************
       //==========================================================================
       // LoadWmagp - Load Internal Agency Contacts from SMUPP User Parameter file
       //==========================================================================
     P LoadWmagp       B
     d LoadWmagp       PI
     d inCo#                          3  0
     d inFein                         9  0
     d inMod                          3  0
      /free

       // Write Internal Policy Contacts from SMUPP User Paramter File.
       chain (inCo#:inFein:inMod) wmagl;
       If %found(wmagl);
          i = 1;
          Dow i <=2;
             //*** Beg Chg ***** 11/18/24 *****************************
             //***If StafCodeAgt(i) <> *blanks;
             If StafCodeAgt(i) <> *blanks and
              (%Lookup(StafCodeAgt(1):xxSVCR_lst) = 0 or
               %Lookup(StafCodeAgt(2):xxMKTR_lst) = 0 );
                If i = 1;
                   nxt_pos = %Lookup('':xxSVCR_lst);
                   xxSVCR_lst(nxt_pos) = StafCodeAgt(i);
                Else;
                   nxt_pos = %Lookup('':xxMKTR_lst);
                   xxMKTR_lst(nxt_pos) = StafCodeAgt(i);
                Endif;
             //*** End Chg ***** 11/18/24 *****************************
                Chain StafCodeAgt(i) pup02;
                If %found(Smupl1);
                   // Write Claim Contact info
                   ClrAgtCont();
                   scAConNam = %trim(UPusnm);

                   // Address
                   FormatAddress(UPadr1:UPadr2:UPcty:UPst:UPzip:scAConAd1:
                    scAConAd2:scAConCs);

                   // Fax
                   If UPfaxa <> *zeros or UPfax# <> *zeros;
                      scAConfax = '(' + %editc(UPfaxa:'X') + ')' +
                       %subst(%editc(UPfax#:'X'):1:3) + '-' +
                       %subst(%editc(UPfax#:'X'):4:4);
                   Endif;

                   scAConEml = UPmail;
                   scAConEmal = UPmail;
                   If scAConEmal = *Blanks;
                      scAConEmal = 'No Email available';
                   Endif;


                   If UPphna <> *zeros or UPphn# <> *zeros;
                      scAConPhn# = '(' + %editc(UPphna:'X') + ')' +
                       %subst(%editc(UPphn#:'X'):1:3) + '-' +
                       %subst(%editc(UPphn#:'X'):4:4);
                   Endif;

                   // Get contact type depending on internal policy contacts
                   Select;
                   When i = 1;
                      scAConTyp  = 'Agency Services Representative';
                   When i = 2;
                      scAConTyp  = 'Agency Marketing Representative';
                   Endsl;

                   scAConRrn# = rrnSmupl1;

                   // status
                   If UPstat = 'A';
                      scAConSts = 'Active';
                   Else;
                      scAConSts = 'Inactive';
                   Endif;

                   scAInter = 'I';

                   rrnAgnCon += 1;
                   If rrnAgnCon = 1;
                      scAgnRowSl = *on;
                   Else;
                      Clear scAgnRowSl;
                   Endif;

                   Write sflAgnCont;
                Endif;
             Endif;
             i += 1;
          Enddo;
       Endif;

      /end-free
     P LoadWmagp       E

       //===================================================================
       // ClrPolCont - Clear all Policy contact fields
      //===================================================================
     P ClrPolCont      B
     d ClrPolCont      PI
      /free

       clear scPConNam;
       clear scPConFor;
       clear scConCrt;
       clear scPConPst;
       clear scPConPhn#;
       clear scPCnCPhn#;
       clear scPConDiv;
       clear scDtlCnEm;
       clear scDtlConEm;
       clear scPConType;
       clear scPConRRN#;
       clear scCrtRrn;
       clear scPInter;
       //*** Beg Add ***** 05/26/25 ****************************
       Clear scPLstUpd;
       Clear scPLstUpdB;
       //*** Beg Add ***** 05/26/25 ****************************

      /end-free
     P ClrPolCont      E

       //===================================================================
       // ClrAgtCont - Clear all Policy contact fields
      //===================================================================
     P ClrAgtCont      B
     d ClrAgtCont      PI
      /free

       clear scAConEml;
       clear scAConEmal;
       clear scACnEffDt;
       clear scAConNam;
       clear scAConRnk;
       clear scAConAgn;
       clear scAConCrt;
       clear scAConTyp;
       clear scAConPhn#;
       clear scAConCPhn;
       clear scAConDiv;
       clear scAConSts;
       clear scAConRRN#;
       clear scAgt#;
       clear scAInter;
       //*** Beg Add ***** 05/26/25 ********************************
       clear scALstUpd;
       clear scALstUpdB;
       //*** End Add ***** 05/26/25 ********************************

      /end-free
     P ClrAgtCont      E

       //===================================================================
       // StaffAssign - Assigning Staff to WMECP
      //===================================================================
     P StaffAssign     B
     d StaffAssign     PI

       Setll (##co:##fnd:##emp) pem00;
       Reade (##co:##fnd:##emp) pem00;
       Dow not %EOF(WMEML);
          //*** Writing/Updating/Deleting record from WMECP
          // - POLICY SERVICES
          If EMpsrv <> *blanks;
             //* fetching user name
             Chain EMpsrv pup02;
             If %found(SMUPL1);
                xxlen = %len(%trim(UPusnm));
                If xxlen > 15;
                   spacePos = %scan(' ': UPusnm);
                   If spacePos > 0;
                      xxfsn = %subst(UPusnm: 1: spacePos - 1);
                      xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                   Endif;
                Else;
                xxfsn = %trim(UPusnm);
                xxlsn = '';
                Endif;
             Endif;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:'A') pec01;

             If not %found(WMECL1);
                ECco# = EMco#;
                ECfnd = EMfnd;
                Ecemp# = EMemp#;
                ECdiv = EMdiv;
                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECpcnt = 'A';
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECdivspec = 'Y';
                ECiusr = q1user;
                ECipgm = q1pgm;
                ECidt  = Dateto7(SystemDate());
                ECitim = Timeto6(SystemTime());
                Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
                 Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
                ECseq# = maxseq# + 1;

                Write pec01;
             //*** Beg Del ***** 05/26/25 ***********************
             //***Else;

             //***   ECfsn = xxfsn;
             //***   EClsn = xxlsn;
             //***   ECADR1 = UPADR1;
             //***   ECADR2 = UPADR2;
             //***   ECCTY  = UPCTY;
             //***   ECST   = UPST;
             //***   ECZIP  = UPZIP;
             //***   ECEMAL = UPMAIL;
             //***   ECFARA = UPFAXA;
             //***   ECFPHN = UPFAX#;
             //***   ECAREA = UPPHNA;
             //***   ECPHN# = UPPHN#;
             //***   ECuser = q1user;
             //***   ECpgm  = q1pgm;
             //***   ECdt   = Dateto7(SystemDate());
             //***   ECtime = Timeto6(SystemTime());

             //***   Update pec01;
             //*** End Del ***** 05/26/25 ***********************

             Endif;
          Endif;

          //*** Beg Del ***** 05/26/25 ***********************
          //***If EMpsrv = *blanks;
          //***   Chain (EMco#:EMfnd:EMemp#:EMdiv:'A') pec01;
          //***   If %found(WMECL1);
          //***      Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Dow not %eof(wmecpcrt);
          //***         Delete wmecpcrt;
          //***         Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Enddo;
          //***      Unlock wmecpcrt;

          //***      Delete pec01;
          //***   Endif;
          //***Endif;
          //*** End Del ***** 05/26/25 ***********************

          // - AUDIT REVIEWER
          If EMarvw <> *blanks;
             //* fetching user name
             Chain EMarvw pup02;
             If %found(SMUPL1);
                xxlen = %len(%trim(UPusnm));
                If xxlen > 15;
                   spacePos = %scan(' ': UPusnm);
                   If spacePos > 0;
                      xxfsn = %subst(UPusnm: 1: spacePos - 1);
                      xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                   Endif;
                Else;
                xxfsn = %trim(UPusnm);
                xxlsn = '';
                Endif;
             Endif;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:'B') pec01;

             If not %found(WMECL1);
                ECco# = EMco#;
                ECfnd = EMfnd;
                Ecemp# = EMemp#;
                ECdiv = EMdiv;
                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECpcnt = 'B';
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECdivspec = 'Y';
                ECiusr = q1user;
                ECipgm = q1pgm;
                ECidt  = Dateto7(SystemDate());
                ECitim = Timeto6(SystemTime());
                Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
                 Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
                ECseq# = maxseq# + 1;

                Write pec01;
             //*** Beg Del ***** 05/26/25 ***********************
             //***Else;

             //***   ECfsn = xxfsn;
             //***   EClsn = xxlsn;
             //***   ECADR1 = UPADR1;
             //***   ECADR2 = UPADR2;
             //***   ECCTY  = UPCTY;
             //***   ECST   = UPST;
             //***   ECZIP  = UPZIP;
             //***   ECEMAL = UPMAIL;
             //***   ECFARA = UPFAXA;
             //***   ECFPHN = UPFAX#;
             //***   ECAREA = UPPHNA;
             //***   ECPHN# = UPPHN#;
             //***   ECuser = q1user;
             //***   ECpgm  = q1pgm;
             //***   ECdt   = Dateto7(SystemDate());
             //***   ECtime = Timeto6(SystemTime());

             //***   Update pec01;
             //*** End Del ***** 05/26/25 ***********************

             Endif;
          Endif;

          //*** Beg Del ***** 05/26/25 **********************
          //***If EMarvw = *blanks;
          //***   Chain (EMco#:EMfnd:EMemp#:EMdiv:'B') pec01;
          //***   If %found(WMECL1);
          //***      Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Dow not %eof(wmecpcrt);
          //***         Delete wmecpcrt;
          //***         Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Enddo;
          //***      Unlock wmecpcrt;

          //***      Delete pec01;
          //***   Endif;
          //***Endif;
          //*** End Del ***** 05/26/25 **********************

          // - CLAIM CONTACT
          If EMclmc <> *blanks;
             //* fetching user name
             Chain EMclmc pup02;
             If %found(SMUPL1);
                xxlen = %len(%trim(UPusnm));
                If xxlen > 15;
                   spacePos = %scan(' ': UPusnm);
                   If spacePos > 0;
                      xxfsn = %subst(UPusnm: 1: spacePos - 1);
                      xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                   Endif;
                Else;
                xxfsn = %trim(UPusnm);
                xxlsn = '';
                Endif;
             Endif;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:'C') pec01;

             If not %found(WMECL1);
                ECco# = EMco#;
                ECfnd = EMfnd;
                Ecemp# = EMemp#;
                ECdiv = EMdiv;
                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECpcnt = 'C';
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECdivspec = 'Y';
                ECiusr = q1user;
                ECipgm = q1pgm;
                ECidt  = Dateto7(SystemDate());
                ECitim = Timeto6(SystemTime());
                Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
                 Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
                ECseq# = maxseq# + 1;

                Write pec01;
             //**** Beg Del ***** 05/26/25 *******************
             //***Else;

             //***   ECfsn = xxfsn;
             //***   EClsn = xxlsn;
             //***   ECADR1 = UPADR1;
             //***   ECADR2 = UPADR2;
             //***   ECCTY  = UPCTY;
             //***   ECST   = UPST;
             //***   ECZIP  = UPZIP;
             //***   ECEMAL = UPMAIL;
             //***   ECFARA = UPFAXA;
             //***   ECFPHN = UPFAX#;
             //***   ECAREA = UPPHNA;
             //***   ECPHN# = UPPHN#;
             //***   ECuser = q1user;
             //***   ECpgm  = q1pgm;
             //***   ECdt   = Dateto7(SystemDate());
             //***   ECtime = Timeto6(SystemTime());

             //***   Update pec01;
             //**** End Del ***** 05/26/25 *******************

             Endif;
          Endif;

          //*** Beg Del ***** 05/26/25 **********************
          //***If EMclmc = *blanks;
          //***   Chain (EMco#:EMfnd:EMemp#:EMdiv:'C') pec01;
          //***   If %found(WMECL1);
          //***      Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Dow not %eof(wmecpcrt);
          //***         Delete wmecpcrt;
          //***         Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Enddo;
          //***      Unlock wmecpcrt;

          //***      Delete pec01;
          //***   Endif;
          //***Endif;
          //*** End Del ***** 05/26/25 **********************

          // - UNDERWRITER
          If EMuwtr <> *blanks;
             //* fetching user name
             Chain EMuwtr pup02;
             If %found(SMUPL1);
                xxlen = %len(%trim(UPusnm));
                If xxlen > 15;
                   spacePos = %scan(' ': UPusnm);
                   If spacePos > 0;
                      xxfsn = %subst(UPusnm: 1: spacePos - 1);
                      xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                   Endif;
                Else;
                xxfsn = %trim(UPusnm);
                xxlsn = '';
                Endif;
             Endif;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:'D') pec01;

             If not %found(WMECL1);
                ECco# = EMco#;
                ECfnd = EMfnd;
                Ecemp# = EMemp#;
                ECdiv = EMdiv;
                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECpcnt = 'D';
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECdivspec = 'Y';
                ECiusr = q1user;
                ECipgm = q1pgm;
                ECidt  = Dateto7(SystemDate());
                ECitim = Timeto6(SystemTime());
                Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
                 Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
                ECseq# = maxseq# + 1;

                Write pec01;
             //*** Beg Del ***** 05/26/25 *******************
             //***Else;

             //***   ECfsn = xxfsn;
             //***   EClsn = xxlsn;
             //***   ECADR1 = UPADR1;
             //***   ECADR2 = UPADR2;
             //***   ECCTY  = UPCTY;
             //***   ECST   = UPST;
             //***   ECZIP  = UPZIP;
             //***   ECEMAL = UPMAIL;
             //***   ECFARA = UPFAXA;
             //***   ECFPHN = UPFAX#;
             //***   ECAREA = UPPHNA;
             //***   ECPHN# = UPPHN#;
             //***   ECuser = q1user;
             //***   ECpgm  = q1pgm;
             //***   ECdt   = Dateto7(SystemDate());
             //***   ECtime = Timeto6(SystemTime());

             //***   Update pec01;
             //*** End Del ***** 05/26/25 *******************

             Endif;
          Endif;

          //*** Beg Del ***** 05/26/25 **********************
          //***If EMuwtr = *blanks;
          //***   Chain (EMco#:EMfnd:EMemp#:EMdiv:'D') pec01;
          //***   If %found(WMECL1);
          //***      Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Dow not %eof(wmecpcrt);
          //***         Delete wmecpcrt;
          //***         Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Enddo;
          //***      Unlock wmecpcrt;

          //***      Delete pec01;
          //***   Endif;
          //***Endif;
          //*** End Del ***** 05/26/25 **********************

          // - AUDIT MANAGER
          If EMamgr <> *blanks;
             //* fetching user name
             Chain EMamgr pup02;
             If %found(SMUPL1);
                xxlen = %len(%trim(UPusnm));
                If xxlen > 15;
                   spacePos = %scan(' ': UPusnm);
                   If spacePos > 0;
                      xxfsn = %subst(UPusnm: 1: spacePos - 1);
                      xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                   Endif;
                Else;
                xxfsn = %trim(UPusnm);
                xxlsn = '';
                Endif;
             Endif;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:'E') pec01;

             If not %found(WMECL1);
                ECco# = EMco#;
                ECfnd = EMfnd;
                Ecemp# = EMemp#;
                ECdiv = EMdiv;
                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECpcnt = 'E';
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECdivspec = 'Y';
                ECiusr = q1user;
                ECipgm = q1pgm;
                ECidt  = Dateto7(SystemDate());
                ECitim = Timeto6(SystemTime());
                Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
                 Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
                ECseq# = maxseq# + 1;

                Write pec01;
             //*** Beg Del 05/26/25 *************************
             //***Else;

             //***   ECfsn = xxfsn;
             //***   EClsn = xxlsn;
             //***   ECADR1 = UPADR1;
             //***   ECADR2 = UPADR2;
             //***   ECCTY  = UPCTY;
             //***   ECST   = UPST;
             //***   ECZIP  = UPZIP;
             //***   ECEMAL = UPMAIL;
             //***   ECFARA = UPFAXA;
             //***   ECFPHN = UPFAX#;
             //***   ECAREA = UPPHNA;
             //***   ECPHN# = UPPHN#;
             //***   ECuser = q1user;
             //***   ECpgm  = q1pgm;
             //***   ECdt   = Dateto7(SystemDate());
             //***   ECtime = Timeto6(SystemTime());

             //***   Update pec01;
             //*** End Del 05/26/25 *************************

             Endif;
          Endif;

          //*** Beg Del 05/26/25 ****************************
          //***If EMamgr = *blanks;
          //***   Chain (EMco#:EMfnd:EMemp#:EMdiv:'E') pec01;
          //***   If %found(WMECL1);
          //***      Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Dow not %eof(wmecpcrt);
          //***         Delete wmecpcrt;
          //***         Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Enddo;
          //***      Unlock wmecpcrt;

          //***      Delete pec01;
          //***   Endif;
          //***Endif;
          //*** End Del 05/26/25 ****************************

          // - PREMIUM ACCOUNTANT
          If EMpacc <> *blanks;
             //* fetching user name
             Chain EMpacc pup02;
             If %found(SMUPL1);
                xxlen = %len(%trim(UPusnm));
                If xxlen > 15;
                   spacePos = %scan(' ': UPusnm);
                   If spacePos > 0;
                      xxfsn = %subst(UPusnm: 1: spacePos - 1);
                      xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                   Endif;
                Else;
                xxfsn = %trim(UPusnm);
                xxlsn = '';
                Endif;
             Endif;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:'F') pec01;

             If not %found(WMECL1);
                ECco# = EMco#;
                ECfnd = EMfnd;
                Ecemp# = EMemp#;
                ECdiv = EMdiv;
                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECpcnt = 'F';
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECdivspec = 'Y';
                ECiusr = q1user;
                ECipgm = q1pgm;
                ECidt  = Dateto7(SystemDate());
                ECitim = Timeto6(SystemTime());
                Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
                 Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
                ECseq# = maxseq# + 1;

                Write pec01;
             //*** Beg Del ***** 05/26/25 ********************
             //***Else;

             //***   ECfsn = xxfsn;
             //***   EClsn = xxlsn;
             //***   ECADR1 = UPADR1;
             //***   ECADR2 = UPADR2;
             //***   ECCTY  = UPCTY;
             //***   ECST   = UPST;
             //***   ECZIP  = UPZIP;
             //***   ECEMAL = UPMAIL;
             //***   ECFARA = UPFAXA;
             //***   ECFPHN = UPFAX#;
             //***   ECAREA = UPPHNA;
             //***   ECPHN# = UPPHN#;
             //***   ECuser = q1user;
             //***   ECpgm  = q1pgm;
             //***   ECdt   = Dateto7(SystemDate());
             //***   ECtime = Timeto6(SystemTime());

             //***   Update pec01;
             //*** End Del ***** 05/26/25 ********************

             Endif;
          Endif;

          //*** Beg Del ***** 05/26/25 **********************
          //***If EMpacc = *blanks;
          //***   Chain (EMco#:EMfnd:EMemp#:EMdiv:'F') pec01;
          //***   If %found(WMECL1);
          //***      Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Dow not %eof(wmecpcrt);
          //***         Delete wmecpcrt;
          //***         Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Enddo;
          //***      Unlock wmecpcrt;

          //***      Delete pec01;
          //***   Endif;
          //***Endif;
          //*** End Del ***** 05/26/25 **********************

          // - LOSS CONTROL REPRESENTATIVE
          If EMlrep <> *blanks;
             //* fetching user name
             Chain EMlrep pup02;
             If %found(SMUPL1);
                xxlen = %len(%trim(UPusnm));
                If xxlen > 15;
                   spacePos = %scan(' ': UPusnm);
                   If spacePos > 0;
                      xxfsn = %subst(UPusnm: 1: spacePos - 1);
                      xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                   Endif;
                Else;
                xxfsn = %trim(UPusnm);
                xxlsn = '';
                Endif;
             Endif;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:'G') pec01;

             If not %found(WMECL1);
                ECco# = EMco#;
                ECfnd = EMfnd;
                Ecemp# = EMemp#;
                ECdiv = EMdiv;
                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECpcnt = 'G';
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECdivspec = 'Y';
                ECiusr = q1user;
                ECipgm = q1pgm;
                ECidt  = Dateto7(SystemDate());
                ECitim = Timeto6(SystemTime());
                Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
                 Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
                ECseq# = maxseq# + 1;

                Write pec01;
             //*** Beg Del ***** 05/26/25 *******************
             //***Else;

             //***   ECfsn = xxfsn;
             //***   EClsn = xxlsn;
             //***   ECADR1 = UPADR1;
             //***   ECADR2 = UPADR2;
             //***   ECCTY  = UPCTY;
             //***   ECST   = UPST;
             //***   ECZIP  = UPZIP;
             //***   ECEMAL = UPMAIL;
             //***   ECFARA = UPFAXA;
             //***   ECFPHN = UPFAX#;
             //***   ECAREA = UPPHNA;
             //***   ECPHN# = UPPHN#;
             //***   ECuser = q1user;
             //***   ECpgm  = q1pgm;
             //***   ECdt   = Dateto7(SystemDate());
             //***   ECtime = Timeto6(SystemTime());

             //***   Update pec01;
             //*** End Del ***** 05/26/25 *******************

             Endif;
          Endif;

          //*** Beg Del ***** 05/26/25 **********************
          //***If EMlrep = *blanks;
          //***   Chain (EMco#:EMfnd:EMemp#:EMdiv:'G') pec01;
          //***   If %found(WMECL1);
          //***      Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Dow not %eof(wmecpcrt);
          //***         Delete wmecpcrt;
          //***         Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
          //***      Enddo;
          //***      Unlock wmecpcrt;

          //***      Delete pec01;
          //***   Endif;
          //***Endif;
          //*** End Del ***** 05/26/25 **********************
          Reade (##co:##fnd:##emp) pem00;
       Enddo;

     P StaffAssign     E
       //*** End Add *** 08/29/24 *******************************************

       //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     d Main            PI

      /free

       isUsePEF = *off;
       If ClientID = '132';
          isUsePEF = *on;
       Endif;
       // Load action bar
       LoadActionBar();

       // Process until exit button is pressed.
       Dou ExitNow = *On;

          // Check if new division or policy year has been selected.
          If kkFyr <> ##Fyr or kkDiv <> ##Div;
             ##fyr = kkFyr;
             ##div = kkDiv;
             SetGt keyWDELL# PEL00;
             Readpe keyWDELL PEL00;
             If not %eof(WDELL);
                kkfyr = ELFyr;
                ##fyr = ELFyr;
             Endif;
             xxRRN = dbRRN1;
             LoadActionBar();
             LoadSelection();
          Endif;

          // Load Filter Clear Button.
          loadClrFlt();

          // Clear Sub-files
          ClrSubfiles();

          // Load Policy Contact Information
          LoadPContact();

          // Load Agency Contact Information
          LoadAContact();

          //***  Begin Add  ***  02/28/22  ****************
          // Update the agency contact grid with the assocaited divisions
          UpdAgtCntGrid();
          //***  End   Add  ***  02/28/22  ****************

          // Process policy summary screen.
          Exsr ExfmtScreens;

          Select;

          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'PS':txtWinData);
             btnShowWin = *off;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          // policy filter Processing.
          When btnFilter = *on;
             Dou BtnPConApl = *on or BtnPConCan = *on;

                scPCrrn = XXRRN;
                ExFmt RcdPConFlt;
                If BtnPConApl = *on;
                   dsPFilter.ssPConNam = ssPConNam;
                   dsPFilter.ssPConDiv = ssPConDiv;
                   dsPFilter.ssPConCFr = ssPConCFr;
                Endif;
                If BtnPConCan = *on;
                   ssPConNam  = dsPFilter.ssPConNam;
                   ssPConDiv  = dsPFilter.ssPConDiv;
                   ssPConCFr  = dsPFilter.ssPConCFr;
                Endif;
             Enddo;

          // policy filter clear processing
          When btnClrFltr = *on;
             clear dsPFilter;
             clear rcdPConFlt;
             dspclrfltr = *off;
             Clear polChoices;
             polChoices = 'EMco# = ' + %char(ELco#) + ' AND EMfnd = '
              + %char(ELfnd) + ' AND EMemp# = ' + %char(ELemp#);

          // agency filter Processing.
          When btnAFilter = *on;
             Dou BtnAConApl = *on or BtnAConCan = *on;

                scPCrrn = XXRRN;
                ExFmt RcdAConFlt;
                If BtnAConApl = *on;
                   dsAFilter.ssAConNm  = ssAConNm;
                   dsAFilter.ssAConDiv = ssAConDiv;
                   dsAFilter.ssAConRnk = ssAConRnk;
                   dsAFilter.ssAConAgn = ssAConAgn;
                   dsAFilter.ssAConTyp = ssAConTyp;
                Endif;
                If BtnAConCan = *on;
                   ssAConNm   = dsAFilter.ssAConNm;
                   ssAConDiv  = dsAFilter.ssAConDiv;
                   ssAConRnk  = dsAFilter.ssAConRnk;
                   ssAConAgn  = dsAFilter.ssAConAgn;
                   ssAConTyp  = dsAFilter.ssAConTyp;
                Endif;
             Enddo;

          // agency filter clear processing
          When btnAClrFlt = *on;
             clear dsAFilter;
             clear rcdAConFlt;
             dspAClrFlt = *off;
             Clear agtChoices;
             agtChoices = 'EMco# = ' + %char(ELco#) + ' AND EMfnd = '
              + %char(ELfnd) + ' AND EMemp# = ' + %char(ELemp#);

         When btnRefresh = *On;
             btnRefresh =*Off;
             //*** Beg Add ***** 11/18/24 *****************************
             clear xxSVCR_lst;
             clear xxMKTR_lst;
             //*** End Add ***** 11/18/24 *****************************

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;

          When scPconAdd = *on;
             clear RcdAddCon;
             ##mode = 'A';
             BtnAddRcd = *off;
             BtnCanRcd = *off;
             processAdd();

          Other;

             //* Popup Windows
             Popup();

             procOther();

          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 1050;
          APnlHeight = TabHeight + 50;
          Exsr WriteAction;
          Write rcdTabPan;
          Exfmt rcdPolCont;
          Exsr ReadAction;
          Read rcdTabPan;
       EndSl;

       Endsr;

       //===================================================================
       // Write action panel;
       //===================================================================
       BegSr WriteAction;

       If qteMode;
          Write rcdQtePnl;
       Else;
          Write rcdPolPnl;
       Endif;

       Endsr;
       //===================================================================
       // Read action panel;
       //===================================================================
       BegSr ReadAction;

       If qteMode;
          Read rcdQtePnl;
       Else;
          Read rcdPolPnl;
       Endif;

       Endsr;

      /end-free
     P Main            E
       //==================================================================
       // MeetPolFlt - Determine if the records meet the Filter criteria
       //==================================================================
     P MeetPolFlt      b
     d MeetPolFlt      PI
      /free

       // Check name against search string
       If ssPConNam <> ' ';
          clear ##name;
          clear ##pdsc;
          ##name = Upper(EClsn + ECfsn);
          ##pdsc = Upper(ssPConNam);

          QClscan(##name:##strlen:##start:##pdsc:##patlen:##trans:
           ##trim:##wild:##scnres);

          If ##scnres = 0;
             meetPol = *off;
          Endif;
       Endif;

       // Check Employers division
       If ssPConDiv <> *blanks and %int(ssPConDiv) <> ECDiv;
          meetPol = *off;
       Endif;

       // Check Contact for
       If ssPConCFr <> ' ';
          meetPol = *off;
          If (ECpcnt = '1' and ssPConCFr = EXcnt1) or
             (ECpcnt = '2' and ssPConCFr = EXcnt2) or
             (ECpcnt = '3' and ssPConCFr = EXcnt3);
             meetPol = *on;
          Endif;
       Endif;

      /end-free
     P MeetPolFlt      e
       //==================================================================
       // MeetAgnFlt - Determine if the records meet the Filter criteria
       //==================================================================
     P MeetAgnFlt      b
     d****  Begin Chg  ***  02/28/22  ***************
     d*MeetAgnFlt      PI
     d MeetAgnFlt      pi             1n
     d*
     dmeetAgn          s              1n   inz(*on)
     d****  End   Chg  ***  02/28/22  ***************
      /free

       // Check name against search string
       If ssAConNm <> ' ';
          clear ##name;
          clear ##pdsc;
          ##name = Upper(A5lsn + A5fsn);
          ##pdsc = Upper(ssAConNm);

          QClscan(##name:##strlen:##start:##pdsc:##patlen:##trans:
           ##trim:##wild:##scnres);

          If ##scnres = 0;
             meetAgn = *off;
          Endif;
       Endif;

       // Check Employers division
       If ssAConDiv <> *blanks and %int(ssAConDiv) <> AHDiv;
          meetAgn = *off;
       Endif;

       // Ranking
       If ssAConRnk <> ' 'and ssAConRnk <> kkrank;
          meetAgn = *off;
       Endif;

       // Check Agency against search string
       If ssAConAgn <> ' ';
          clear ##name;
          clear ##pdsc;
          ##name = Upper(AAname);
          ##pdsc = Upper(ssAConAgn);

          QClscan(##name:##strlen:##start:##pdsc:##patlen:##trans:
           ##trim:##wild:##scnres);

          If ##scnres = 0;
             meetAgn = *off;
          Endif;
       Endif;

       // Check Agency against search string
       If ssAConTyp <> ' ' and ssAConTyp <> A5acty;
          meetAgn = *off;
       Endif;

       //***  Begin Del  ***  02/28/22  *******************
       //RecFound = *off;
       //RecFound1 = *off;
       //Setll KeyWMAHL5 pah05;
       //Reade KeyWMAHL5 pah05;
       //Dow not %eof(Wmahl5);
       //   If AHfyr = kkfyr and AHstat <> 'I';
       //      If AHprod <> 0;
       //         RecFound = *on;
       //      Endif;
       //   Endif;
       //   Reade KeyWMAHL5 pah05;
       //Enddo;
       //
       //Reset dsWMA5PCRT;
       //Exec sql
       //select * into :dsWMA5PCRT
       //from WMA5PCRT
       //where A5co# = :##co  and A5fein = :AAfein and A5mod = :kkmod
       //and A5critid = 'Producer';
       //If IsValidFetch(sqlStt);
       //   RecFound1 = *on;
       //Endif;
       //If RecFound1 = *off and RecFound = *off;
       //   meetAgn = *off;
       //Endif;
       //***  End   Del  ***  02/28/22  *******************

       //***  Begin Add  ***  02/28/22  *******************
       Return meetAgn;
       //***  End   Add  ***  02/28/22  *******************

      /end-free
     P MeetAgnFlt      e
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n

      /free

       // Chain to Employer Detail for Program.
       Chain xxRrn PEL0P;
       scActRrn=xxRrn;

       // Process only if valid employer detail record found.
       If not %found;
          processme = *Off;
          Return;
       Endif;

       TabIndex = 0;
       APnlHeight = 1100;

       ##co    = ELco#;
       ##Fnd   = ELfnd;
       ##Emp   = ELemp#;
       ##Div   = ELdiv;
       ##Fyr   = ELfyr;
       saveFyr = ELfyr;
       saveEffDt = ELpfdt;

       kkDiv = ELdiv;
       kkFyr = ELfyr;

       LoadSelection();

       clear dsAFilter;
       clear dsPFilter;
       // Check Flag WCFL48 S/R Payroll Online Phase 2&3 is enabled.
       Chain 'WC' ssylf;

       processMe = *on;
       Return;

      /end-free
     P ProgramStart    E
       //===================================================================
       // processAdd - process addition of contact
       //===================================================================
     P processAdd      B
     d processAdd      PI
      /free

       isValid  = *off;
       If WCFL38 = 'Y';
         visSrem = *On;
       Else;
          visSrem = *Off;
       EndIf;

       // processing for Edit mode
       If ##mode = 'C';
          scAddSelDv = *on;
          If clientID = '093';
             dspPrmCon = *off;
             dspConDsg = *on;
          Else;
             dspPrmCon = *on;
             dspConDsg = *off;
          Endif;

          Chain(n) scPconRrn# pec0p;
          //*** Beg Add ***** 08/29/24 ***********************************
          If scPinter = 'I';
             scPolCHdg = 'Edit System Policy Contact';
             scPolCDis = *On;
             scAddSelDv = *On;
          Else;
             scPolCHdg = 'Add/Edit Policy Contact';
             scPolCDis = *Off;
          Endif;
          //*** End Add ***** 08/29/24 ***********************************
          MoveDbSc();
       Endif;

       // processing for Add mode
       If ##mode = 'A';
          scAddSelDv = *off;
          //*** Beg Add ***** 09/24/24 ***********************************
          scPolCHdg = 'Add Policy Contact';
          //*** End Add ***** 09/24/24 ***********************************

          If clientID = '093';
             dspPrmCon = *off;
             dspConDsg = *on;
          Else;
             dspPrmCon = *on;
             dspConDsg = *off;
          Endif;

          scAddDiv = ##div;
       Endif;

       Chain keyWMEXL pex00;
       clear scChoices;
       scChoices = ' ,' + EXcnt1 + ',' +  EXcnt2 + ','
        + EXcnt3;

       If clientID = '071';
          scChoices = %trim(scChoices) + ',' + EXcnt4;
       Endif;

       If clientID = '132';
          scDspClt = *on;
       Else;
          scDspClt = *off;
       Endif;

       Clear divChoice;

       divChoice = 'EMco# = ' + %char(##co) + ' AND EMfnd = '
        + %char(##fnd) + ' AND EMemp# = ' + %char(##emp);

       Dou BtnCanRcd = *on or (BtnAddRcd = *on and isValid = *off);
          scPCrrn = XXRRN;
          Exfmt rcdAddCon;
          isValid = *off;
          Select;
          When BtnAddRcd = *on;

             //*** Beg Chg ***** 08/29/24 ********************************
             //***Validate();
             If scPInter <> 'I';
                Validate();
             Endif;
             //*** End Chg ***** 08/29/24 ********************************
             If isValid = *off;
                addContact();
             Endif;
          When BtnCanRcd = *on;
             BtnCanRcd = *off;
             Leave;
          Other;
                scAddSelDv = *off;
                If clientID = '093';
                   dspPrmCon = *off;
                   dspConDsg = *on;
                Else;
                   dspPrmCon = *on;
                   dspConDsg = *off;
                Endif;

          Endsl;
       Enddo;
       unlock WMECP;
      /end-free
     P processAdd      E
       //===================================================================
       // procOther - process other options
       //===================================================================
     P procOther       B
     d procOther       PI
      /free

       Dou %eof;
          ReadC sflPolCont;
          Select;
          When %eof;
             Iter;
          Endsl;
          Exsr Process;
       Enddo;

       //==================================
       Begsr Process;

       Select;

       // processing for Edit mode
       When scPconEdt = *on;
          scPconEdt = *off;
          // processing based on contact type
          clear RcdAddCon;
          ##mode = 'C';
          BtnAddRcd = *off;
          BtnCanRcd = *off;
          processAdd();

       When scPconDlt = *on;
          scPconDlt = *off;
          Chain scPconRrn# pec0p;
          delete pec0p;
          unlock WMECP;

       //*** Beg Chg ***** 04/11/24 *** 129WXP ******************
       //***   Setll (##co:##fnd:##emp:ECseq#) pec0p_cr;
       //***   Reade (##co:##fnd:##emp:ECseq#) pec0p_cr;
       //***   Dow not %eof(wmecpcrt);
       //***      Delete wmecpcrt;
       //***      Reade (##co:##fnd:##emp:ECseq#) pec0p_cr;
       //***   Enddo;
       //***   Unlock wmecpcrt;

          Setll (EcCo#:EcFnd:EcEmp#:EcDiv:EcSeq#) pec0p_cr;
          Reade (EcCo#:EcFnd:EcEmp#:EcDiv:EcSeq#) pec0p_cr;
          Dow not %eof(wmecpcrt);
             Delete wmecpcrt;
             Reade (EcCo#:EcFnd:EcEmp#:EcDiv:EcSeq#) pec0p_cr;
          Enddo;
          Unlock wmecpcrt;
       //*** End Chg ***** 04/11/24 *** 129WXP ******************
       Endsl;

       Endsr;

      /end-free
     P procOther       E

       //***  Begin Add  ***  02/28/22  ****************
       //===================================================================
       // Update agent contact grid with associations
       //===================================================================
     pUpdAgtCntGrid    b
     dUpdAgtCntGrid    pi

     dstrAConDivL      s            500    varying inz
       For indx = 1 to #ofProd;
          Reset strAConDivL;
          // No more records need to be processed
          If aryDiv(indx).div(1) = *loval;
             Leave;
          Endif;
          For indxDiv = 1 to #ofDiv;
             // No more records need to be processed
             If aryDiv(indx).div(indxDiv) = *loval;
                Leave;
             Endif;
             strAConDivL += %char(aryDiv(indx).div(indxDiv)) + ',';
          Endfor;
          strAConDivL = %scanrpl(',':'':strAConDivL:%len(strAConDivL)-1);
          Chain indx sflAgnCont;
        //This field is used within grid filtering; its used in onfilterchange event
          scAConDivL = strAConDivL;
          Update sflAgnCont;
       Endfor;
     pUpdAgtCntGrid    e
       //***  End   Add  ***  02/28/22  ****************

       //===================================================================
       // Validate - Validate add/edit screen
       //===================================================================
     P Validate        B
     d Validate        PI
     c
      /free

       isValid  = *off;
       erAddSt = *off;
       erLstNm = *off;
       erFstNm = *off;
       erMIDNm = *off;
       erPstRs = *off;
       erCrsTil= *off;
       erAddPfBl = *off;
       erAddConDs = *off;
       erAddPrmCo = *off;
       erAddPhnDs = *off;

       If ##mode = 'C';

       Chain(n) scPconRrn# pec0p;

       SVdt   = ECdt;
       SVtime = ECtime;
       // Check if updated since screen displayed
       flgRcdUpdated = RcdUpdatedErr('WMECP':scPconRrn#:'ECdt':'ECtime':
        'ECuser':SVdt:SVtime);
       Select;
       When flgRcdUpdated = '0'; // No error
       When flgRcdUpdated = '1'; // Error
          isValid = *on;
          Return;
       When flgRcdUpdated = '2'; // Error with refresh
       Endsl;

       EndIf;

       // Last Name is required entry
       If scAddLstNm = *blanks;
          msLstNm = 'WCR0503';
          erLstNm = *on;
          isValid  = *on;
       Endif;

       // Either Title or First Name must be entered.
       If isValid = *off and scAddTitle = *blanks and scAddFstNm = *blanks;
          msFstNm = 'WC31407';
          erFstNm = *on;
          msCrsTil= 'WC31407';
          erCrsTil= *on;
          isValid = *on;
       Endif;

       // State must be valid if entered
       If isValid = *on and scAddSt <> *blanks;
          clear ##key;
          ##key = scAddSt;
          Chain keySTSTL3 pst03;
          If not %found(STSTL3);
             msAddSt = 'WCR0513';
             erAddSt = *on;
             isValid = *on;
          Endif;
       Endif;

       // Preferred Billing method validation
       If isValid = *off and scAddPfBl = *blanks and clientID = '132';
          msAddPfBl = 'WCR0503';
          erAddPfBl = *on;
          isValid = *on;
       Endif;

       If isValid = *off and scAddPfBl <> *blanks and
        scAddPfBl <> 'P' and scAddPfBl <> 'E' and scAddPfBl <> 'F';
          msAddPfBl = 'WC40049';
          erAddPfBl = *on;
          isValid = *on;
       Endif;

       // No other record for this employer may have the same
       // Primary Contact designation

       If isvalid = *off and (scAddPrmCo <> ' ' or
        scAddConDs <> ' ');

          If ##mode = 'A';
             If ClientId <> '093';
                ##code =  scAddPrmCo;
             Else;
                ##code =  scAddConDs;
             Endif;
             If ECdivspec = 'Y';
                Chain(n) keyWMECL1 pec01;
             Endif;
             If ECdivspec <> 'Y';
                Chain(n) (##co:##fnd:##emp:##code) pec03;
             Endif;
             If %found;
                msAddConDs = 'WC31409';
                msAddPrmCo = 'WC31409';
                erAddConDs = *on;
                erAddPrmCo = *on;
                isValid = *on;
             Endif;
          Endif;

          If ##mode = 'C';
             If ClientId <> '093';
                ##code =  scAddPrmCo;
             Else;
                ##code =  scAddConDs;
             Endif;
             If ECdivspec = 'Y';
                Setll keyWMECL1 pec01;
                Reade keyWMECL1 pec01;
                intrrn = rrnWmecl1;
             Endif;
             If ECdivspec <> 'Y';
                Setll keyWMECL3 pec03;
                Reade keyWMECL3 pec03;
                intrrn = rrnWmecl3;
             Endif;
             Dow not %eof;
                //If scPConRRN# <> rrnWmecl1;
                If scPConRRN# <> intrrn;
                   msAddConDs = 'WC31409';
                   msAddPrmCo = 'WC31409';
                   erAddConDs = *on;
                   erAddPrmCo = *on;
                   isValid = *on;
                   Leave;
                Endif;
                If ECdivspec = 'Y';
                   Reade keyWMECL1 pec01;
                ElseIf ECdivspec <> 'Y';
                   Reade keyWMECL3 pec03;
                Endif;
             Enddo;
          Endif;
       Endif;

       // If Phone Number entered has 10 digit
       ##type = 'P';
       erAddPhn = Check_Phone#(scAddPhn:##type:msAddPhn);
       If erAddPhn = *on;
          isValid = *on;
       Endif;

       // If Cell Number entered has 10 digit
       ##type = 'C';
       erAddCell = Check_Phone#(scAddCell:##type:msAddCell);
       If erAddCell = *on;
          isValid = *on;
       Endif;

       // If Fax Number entered has 10 digit
       ##type = 'F';
       erAddFax = Check_Phone#(scAddFax:##type:msAddFax);
       If erAddFax = *on;
          isValid = *on;
       Endif;

       //**** Beg Add **** 09/25/2024 **************************************
       // Validate Email Address entry when there is a selection of
       // Distribution Type or Sub Category or Process Output Control.
       Clear erEmailReq;
       Clear MsEmailReq;
       If scDisTyp <> ' ' or scSubCat <> ' ' or scOutCnt <> ' ';
          If scAddEmail = ' ';
             erEmailReq = *on;
             isValid = *on;
             MsEmailReq = 'An Email Address is required when there are +
             selection(s) in ''Distribution Types'' or  ''Sub Categories'' +
             or ''Process Output Control'' columns below.';
          Endif;
       Endif;
       //**** End Add **** 09/25/2024 **************************************

       // Validate EmailId
       erAddEmail = *Off;
       If %Len(%trim(scAddEmail)) > 0;
       erAddEmail = Check_Email(scAddEmail:MsAddEmail);
         If erAddEmail = *on;
           isValid = *on;
          EndIf;
       EndIf;

       // If Phone Number, Cell Number and Primary Contact entered THEN
       // Primary Phone Flag must be entered.
       If isvalid = *off and scAddPrmCo <> ' ' and (scAddPhn > 0 and
        %rem(scAddPhn:10000000) <> *zeros) and (scAddCell > 0 and
        %rem(scAddCell:10000000) <> *zeros) and scAddPhnDs = ' ';
          msAddPhnDs = 'WC31410';
          erAddPhnDs = *on;
          isValid = *on;
       Endif;

       // SR Email Notification only be entered only when Email address is
       // provided.

       If WCFL38 = 'Y';
          If SCsrem <> *blanks and SCsrem <> 'Y';
            msSrem1 = 'WCR0571';
            erSrem1 = *on;
            isValid = *on;
          EndIf;

          If SCAddEmail = *blanks and SCsrem <> *blanks;
            msSrem2 = 'W400072';
            erSrem2 = *on;
            isValid = *on;
          EndIf;
       EndIf;
       // Record must have a unique key
       // co#/fnd/emp#/div/lsn/fsn/mid/posn/pcnt

       If isValid = *off;
          If ##mode = 'A';
             If ClientId <> '093';
                ##code =  scAddPrmCo;
             Else;
                ##code =  scAddConDs;
             Endif;
             If ECdivspec = 'Y';
                Chain(n) keyWMECL# pec00;
             Endif;
             If ECdivspec <> 'Y';
                Chain(n) (##co:##fnd:##emp:scAddLstNm:scAddFstNm:scAddMidNm:
                 scAddPstRs:##code) pec02;
             Endif;
             If %found;
                msAddPhnDs = 'WC30108';
                erAddPhnDs = *on;
                msLstNm = 'WC30108';
                erLstNm = *on;
                msFstNm = 'WC30108';
                erFstNm = *on;
                msMIDNm = 'WC30108';
                erMIDNm = *on;
                msPstRs = 'WC30108';
                erPstRs = *on;
                msAddPrmCo = 'WC30108';
                erAddPrmCo = *on;
                msAddConDs = 'WC30108';
                erAddConDs = *on;
                isValid = *on;
             Endif;
          Endif;

          If ##mode = 'C';
             If ClientId <> '093';
                ##code =  scAddPrmCo;
             Else;
                ##code =  scAddConDs;
             Endif;
             If ECdivspec = 'Y';
                Setll keyWMECL# pec00;
                Reade keyWMECL# pec00;
             Endif;
             If ECdivspec <> 'Y';
                Setll keyWMECL2 pec02;
                Reade keyWMECL2 pec02;
             Endif;
             Dow not %eof;
                If ECdivspec = 'Y';
                   If (scPConRRN# <> rrnWmecl);
                   msAddPhnDs = 'WC30108';
                   erAddPhnDs = *on;
                   msLstNm = 'WC30108';
                   erLstNm = *on;
                   msFstNm = 'WC30108';
                   erFstNm = *on;
                   msMIDNm = 'WC30108';
                   erMIDNm = *on;
                   msPstRs = 'WC30108';
                   erPstRs = *on;
                   msAddPrmCo = 'WC30108';
                   erAddPrmCo = *on;
                   msAddConDs = 'WC30108';
                   erAddConDs = *on;
                   isValid = *on;
                   Leave;
                Endif;
                Reade keyWMECL# pec00;
                Endif;
                If ECdivspec <> 'Y';
                   If (scPConRRN# <> rrnWmecl2);
                      msAddPhnDs = 'WC30108';
                      erAddPhnDs = *on;
                      msLstNm = 'WC30108';
                      erLstNm = *on;
                      msFstNm = 'WC30108';
                      erFstNm = *on;
                      msMIDNm = 'WC30108';
                      erMIDNm = *on;
                      msPstRs = 'WC30108';
                      erPstRs = *on;
                      msAddPrmCo = 'WC30108';
                      erAddPrmCo = *on;
                      msAddConDs = 'WC30108';
                      erAddConDs = *on;
                      isValid = *on;
                      Leave;
                   Endif;
                   Reade keyWMECL2 pec02;
                Endif;
             Enddo;
          Endif;
       Endif;

      /end-free
     P Validate        E
       //===================================================================
       // addContact - Add Contact
       //===================================================================
     P addContact      B
     d addContact      PI
     c
      /free

       Chain scPconRrn# pec0p;
       ECdiv  = scAddDiv;
       ECtitl = scAddTitle;
       ECfsn  = scAddFstNm;
       ECmid  = scAddMidNm;
       EClsn  = scAddLstNm;
       ECposn = scAddPstRs;
       ECadr1 = scAddAdr1;
       ECadr2 = scAddAdr2;
       ECcty  = scAddCty;
       ECst   = scAddSt;
       ECzip  = scAddZip;

       // phone#
       If scAddPhn > 0;
         ECarea = %Int(scAddPhn/10000000);
         ECphn# = scAddPhn - (ECarea*10000000);
       Else;
         ECarea = 0;
         ECphn# = 0;
       Endif;
       ECext# = scAddPhn3;

       // cell phone#
       If scAddCell > 0;
         ECcara = %Int(scAddCell/10000000);
         ECcphn = scAddCell - (ECcara*10000000);
       Else;
         ECcara = 0;
         ECcphn = 0;
       Endif;

       // fax#
       If scAddFax > 0;
         ECfara = %Int(scAddFax/10000000);
         ECfphn = scAddFax - (ECfara*10000000);
       Else;
         ECfara = 0;
         ECfphn = 0;
       Endif;

       //*** Beg Add ***** 03/22/24 *********************************
       // Contact Type
       ECtype = scConType;
       //*** End Add ***** 03/22/24 *********************************

       ECpef  = scAddPfBl;
       ECpphf = scAddPhnDs;
       ECemal = scAddEmail;
       ECsrem = SCsrem;
       ECdivspec = scDivSpCon;

       If ClientId <> '093';
          ECpcnt =  scAddPrmCo;
       Else;
          ECpcnt =  scAddConDs;
       Endif;

       If ##mode = 'A';
          //*** Beg Add ***** 04/11/24 *** 129WXP ******************
          maxseq# = 0;
          //*** End Add ***** 04/11/24 *** 129WXP ******************
          ECco#  = ##co;
          ECfnd  = ##fnd;
          ECemp# = ##emp;
          ECiusr = q1user;
          ECipgm = q1pgm;
          ECidt  = Dateto7(SystemDate());
          ECitim = Timeto6(SystemTime());
          Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
          //*** Beg Chg ***** 08/29/24 *****************************
          //*Where ECco# = :##co And ECfnd = :##fnd And ECemp# = :##emp
          //*and ECdiv = :##div;
          Where ECco# = :##co And ECfnd = :##fnd And ECemp# = :##emp;
          //*** End Chg ***** 08/29/24 *****************************
          ECseq# = maxseq# + 1;
          Write pec00;
       Else;
          ECuser = q1user;
          ECpgm  = q1pgm;
          ECdt   = Dateto7(SystemDate());
          ECtime = Timeto6(SystemTime());
          Update pec0p;
       Endif;
       Unlock WMECP;

       If ##mode = 'C';
       //*** Beg Chg ***** 04/11/24 *** 129WXP ******************
       //***   Setll (##co:##fnd:##emp:ECseq#) pec0p_cr;
       //***   Reade (##co:##fnd:##emp:ECseq#) pec0p_cr;
       //***   Dow not %eof(wmecpcrt);
       //***      Delete wmecpcrt;
       //***      Reade (##co:##fnd:##emp:ECseq#) pec0p_cr;
       //***   Enddo;
       //***   Unlock wmecpcrt;

          Setll (##co:##fnd:##emp:##div:ECseq#) pec0p_cr;
          Reade (##co:##fnd:##emp:##div:ECseq#) pec0p_cr;
          Dow not %eof(wmecpcrt);
             Delete wmecpcrt;
             Reade (##co:##fnd:##emp:##div:ECseq#) pec0p_cr;
          Enddo;
          Unlock wmecpcrt;
       //*** End Chg ***** 04/11/24 *** 129WXP ******************
       Endif;

       If scDisTyp <> ' ';
          Dow scDisTyp <> ' ';
             Clear strDisVal;
             strDisVal = ParseString(scDisTyp:',':1:'1');
             CR_ECcritid = 'Distribution';
             CR_ECcritval = strDisVal;
             Exsr Write_CritRec;
          Enddo;
       Endif;
       If scSubCat <> ' ';
          Dow scSubCat <> ' ';
             Clear strSubVal;
             strSubVal = ParseString(scSubCat:',':1:'1');
             CR_ECcritid = 'Sub Category';
             CR_ECcritval = strSubVal;
             Exsr Write_CritRec;
          Enddo;
       Endif;
       If scOutCnt <> ' ';
          Dow scOutCnt <>  ' ';
             Clear strOCoVal;
             strOCoVal = ParseString(scOutCnt:',':1:'1');
             CR_ECcritid = 'Process Output Control';
             CR_ECcritval = strOCoVal;
             Exsr Write_CritRec;
          Enddo;
       Endif;

       Begsr Write_CritRec;
          CR_ECseq# = ECseq#;
          CR_ECco#  = ##co;
          CR_ECfnd  = ##fnd;
          CR_ECemp# = ##emp;
          CR_ECdiv = scAddDiv;
          CR_ECiusr = q1user;
          CR_ECipgm = q1pgm;
          CR_ECidt  = Dateto7(SystemDate());
          CR_ECitim = Timeto6(SystemTime());
          Write pec0p_cr;
       Endsr;
      /end-free
     P addContact      E
       //===================================================================
       // MoveDbSc - Move database record to screen
       //===================================================================
     P MoveDbSc        B
     d MoveDbSc        PI
     c
      /free

       scAddDiv   = ECdiv;
       scAddTitle = ECtitl;
       scAddFstNm = ECfsn;
       scAddMidNm = ECmid;
       scAddLstNm = EClsn;
       scAddPstRs = ECposn;
       scAddAdr1  = ECadr1;
       scAddAdr2  = ECadr2;
       scAddCty   = ECcty;
       scAddSt    = ECst;
       scAddZip   = ECzip;

       // phone#
       scAddPhn = ECarea * 10000000 + ECphn#;
       scAddPhn3 = ECext#;

       // cell phone#
       scAddCell = ECcara * 10000000 + ECcphn;

       // fax#
       scAddFax = ECfara * 10000000 + ECfphn;

       //*** Beg Add ***** 03/22/24 *********************************
       // Contact Type
       scConType = ECtype;
       //*** End Add ***** 03/22/24 *********************************

       scAddPfBl  = ECpef;
       scAddPhnDs = ECpphf;
       scAddEmail = ECemal;
       SCsrem = ECsrem;

       If ClientId <> '093';
          scAddPrmCo = ECpcnt;
       Else;
          scAddConDs = ECpcnt;
       Endif;

       scDivSpCon = ECdivspec;
       //*** Beg Chg ***** 04/11/24 *** 129WXP ******************
       //*** Setll (##co:##Fnd:##Emp:ECseq#) pec0p_cr;
       //*** Reade(n) (##co:##Fnd:##Emp:ECseq#) pec0p_cr;

       Setll (EcCo#:EcFnd:EcEmp#:EcDiv:EcSeq#) pec0p_cr;
       Reade(n) (EcCo#:EcFnd:EcEmp#:EcDiv:EcSeq#) pec0p_cr;
       //*** End Chg ***** 04/11/24 *** 129WXP ******************
       Dow not %eof(wmecpcrt);
       If CR_ECcritid = 'Distribution';
          If scDisTyp = ' ';
             scDisTyp = CR_ECcritval;
          Else;
             scDisTyp = %trim(scDisTyp) + ',' + %trim(CR_ECcritval);
          Endif;
       Endif;
       If CR_ECcritid = 'Sub Category';
          If scSubCat = ' ';
             scSubCat = CR_ECcritval;
          Else;
             scSubCat = %trim(scSubCat) + ',' + %trim(CR_ECcritval);
          Endif;
       Endif;
       If CR_ECcritid = 'Process Output Control';
          If scOutCnt = ' ';
             scOutCnt = CR_ECcritval;
          Else;
             scOutCnt = %trim(scOutCnt) + ',' + %trim(CR_ECcritval);
          Endif;
       Endif;
       //*** Beg Chg ***** 04/11/24 *** 129WXP ******************
       //***  Reade(n) (##co:##Fnd:##Emp:ECseq#) pec0p_cr;
       Reade(n) (EcCo#:EcFnd:EcEmp#:EcDiv:EcSeq#) pec0p_cr;
       //*** End Chg ***** 04/11/24 *** 129WXP ******************
       Enddo;
      /end-free
     P MoveDbSc        E
     P*================================================================
     P* GetContUpd - returns contacts update
     P*  user      - passed in for gathering info (required)
     P*================================================================
     PGetContUpd       B
     DGetContUpd       PI             1
     D inUserID                            Like(SM_UPUSR) const

     D strUserID       s                   Like(SM_UPusr)   inz
     D strContUpd      s                   Like(SM_UPContUpd) inz
      /Free
          strUserId = inUserId;
          Chain strUserID pup01;
          If %found;
             strContUpd = SM_UPcontUpd;
          Endif;
          Return strContUpd;
      /End-Free
     PGetContUpd       E

       //===================================================================

      * utility procedure definitions
      /COPY SCOPYPOLPI
