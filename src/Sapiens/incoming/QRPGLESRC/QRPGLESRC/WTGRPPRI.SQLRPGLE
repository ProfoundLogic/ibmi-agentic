     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

     h/title WTGRPPRI- Primary Group Information
     h dftname(WTGRPPRI)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 01/22/20                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2196- Redesign of Group Configuration program  *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  09/09/25 C:342 Project: CN036                                *
      //                 Multi-Group Processing                        *
      //                                                               *
      //  08/21/25 C:300 Support: 119607                               *
      //                 "Receiver value too small to hold result"     *
      //                 when attempt to set up Group 900 for Connect  *
      //                                                               *
      //  07/22/25 C:300 Support: 119421                               *
      //                 1) "Override Company Defaults" link is visible*
      //                 when Connect Portal Block is Hidden           *
      //                 2) In "Include Group for Connect" section on  *
      //                 rcdGrpAdd record format - "Default and Exclude*
      //                 from Comparative Rating" checkboxes are       *
      //                 visible, when Connect Portal Block is Hidden  *
      //                 DSPF: Added changes in onload of rcdGrpAdd    *
      //                                                               *
      // 05/08/25 C:333 Project: 153WBD                                *
      //                Note of Cancel Format-Contact Info Updt.       *
      //                DSPF Changes.                                  *
      //                - Adding alternate company email/phone # to    *
      //                  Group Information.                           *
      //                                                               *
      // 09/25/24 C:65  Project: SY446 (Design and Compose revamp)     *
      //                When D&C module is enabled display a URL logo  *
      //                field in addition to path field.  Only require *
      //                edits based on D&C enabled status.             *
      //                                                               *
      //  08/06/24 C:300 Support: 117247                               *
      //                 Saving info on 1st Tab Clears info on other Tabs
      //                                                               *
      //  05/23/24 C:300 Support: 115634 (SUNZ)                        *
      //                 Refresh Button should clear all errors and    *
      //                 Re-load screen with latest file information   *
      //                                                               *
      //  05/14/24 C:062 Project: 146WKQ                               *
      //                 Add Assessments for S/R.                      *
      //                                                               *
      //  05/08/24 C:053 Support: 115213
      //                 Add ASO flag for EDI (PEO/ASO)
      //                                                               *
      //  04/29/24 C:340 Project: WC2965                               *
      //                 Add Default Email Address TextBox.            *
      //                                                               *
      //  01/16/24 C:340 PROJECT: 153WAD                               *
      //                 Add new Field : F2RNOTE & F2RCXRSN            *
      //                                                               *
      //  11/17/23 C:300 Support: 114591                               *
      //                 Add 'Include Group in Nightly iDARTS extract' *
      //                 checkbox on Group Auxiliary Information Section
      //                                                               *
      //  09/18/23 C:300 Support: 113869 (SUNZ)                        *
      //                 Getting error 'Job queue GRP614 in QGPL not   *
      //                 found' when saving Group Auxiliary Information*
      //                 with value in 'Group Billing Job Queue Name'  *
      //                 To fix - Include 10/19/21 changes from W0010R07
      //                  (check for job queue object in same library  *
      //                  as the subsystem exists in)                  *
      //                                                               *
      //                 Do not call W0010C02 at data center clients   *
      //                                                               *
      //                 DSPF: Modified error message of scJobq and    *
      //                 scSbs to define variable for Replacement Data *
      //                 And text transform to uppercase in both fields*
      //                                                               *
      //  08/28/23 C:300 Support: 113191                               *
      //                 Action Hyperlinks like Activate/Inactivate    *
      //                 should not be included as last column of excel*
      //                 spreadsheet export                            *
      //                 DSPF: Removed 3 hyperlinks with scActivate    *
      //                 and added 6 new hyperlinks for Activate/Inactivate
      //                 on Regions, Chapters and Bank Info grids      *
      //                                                               *
      //  06/27/23 C:300 Project: WC2910                               *
      //                 Check User Security from Application Configuration
      //                 instead of User Parameters file               *
      //                                                               *
      //  06/23/23 C:300 Project: WC2910, Support: 111868              *
      //                 Display all screens in Inquiry mode, even if  *
      //                 user option security level does not allow them*
      //                 to Edit group information.                    *
      //                                                               *
      //  05/23/23 C:300 Project: WC2910                               *
      //                 - Add User Access Security                    *
      //                                                               *
      //  05/22/23 C:300 Project:W2196 Support:110464                  *
      //                 Option to Add Group to Managed Care           *
      //                 When clicked EDIT Group link                  *
      //                                                               *
      //  05/10/23 C:300 Project:W2196 Support:109128                  *
      //                 Do not reload when user re-visit same Tab, if *
      //                 screen is pending with un-saved changes       *
      //                                                               *
      //  04/26/23 C:300 Project:W2196 Support:110192                  *
      //                 Remove Lockbox Name from Auxiliary Information*
      //                                                               *
      //  04/18/23 C:300 Project:W2196 Support:109056                  *
      //                 Change Sections order on Additional Information
      //                 Tab as 1st - Regions, 2nd - Chapters and      *
      //                 3rd - Flexible Headings.                      *
      //                                                               *
      //  02/13/23 C:300 Project: WC2196                               *
      //                 Merge WTGRPPRI DSPF into WTGRPCFG DSPF        *
      //                 (as per review comments by Wes Winner)        *
      //                                                               *
      //  01/17/22 C:291 Project: WC2788                               *
      //                 Add "Copy Configuration" option to Group      *
      //                 Toolbar.                                      *
      //                                                               *
      //  01/07/22 C:062 Project: WC2837                               *
      //                 Add Net Rate For SR Policies                  *
      //                                                               *
      //  11/04/21 C:062 Project 146WJV                                *
      //                 Add Exclude from Comparative Rating           *
      //                                                               *
      //  06/18/21 C:300 Support:96286                                 *
      //                 Remove 'Summarize A/R for Deductible Billing' *
      //                 Checkbox from Group Master Information screen *
      //                                                               *
      //  11/18/20 C:180 Project WC2757                                *
      //                 Recompiled only.                              *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

      // WTGRPPRI DSPF got mergerd into WTGRPCFG DSPF
     fWTGRPCFG  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflPolYr:rrnPolYr)
     f                                     Sfile(sflRegion:rrnRegion)
     f                                     Sfile(sflChapter:rrnChapter)
     f                                     Sfile(sflBank:rrnBank)
     f                                     Sfile(sflSpecAR:rrnSpecAR)
     f                                     Sfile(sflDepPrm:rrnDepPrm)
     f                                     Sfile(sflFlatCx:rrnFlatCx)
     f                                     Sfile(sflCrAuBl:rrnCrAuBl)
     f                                     Sfile(sflDepoSR:rrnDepoSR)
     f                                     Sfile(sflCollAR:rrnCollAR)
     f                                     Sfile(sflDivPln:rrnDivPln)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdGrpPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Fund Master file
     fwmfnp     if   e             disk    rename(pfn01:pfn0p)
     fwmfnl     uf   e           k disk    rename(pfn01:pfn00)
     f                                     infds(dbfds)
      * Fund Detail file for Policy Years
     fwdf1p     uf a e             disk    rename(pf101:pf10p)
     f                                     infds(dbfds9)
     fwdf1l     if   e           k disk    rename(pf101:pf100)
     f                                     infds(dbfds2)
      * Group Auxiliary Information
     fwdf2l     uf a e           k disk    rename(pf201:pf200)
     f                                     infds(dbfds6)
      * Group Carrier Information
     fwmgcl     uf a e           k disk    rename(pgc01:pgc00)
     f                                     infds(dbfds7)
      * Employer Detail file for Flexible Heading Info
     fwdfhl     uf a e           k disk    rename(pfh01:pfh00)
     f                                     infds(dbfds8)
      * Region Master File
     fwmrep     uf a e             disk    rename(pre01:pre0p)
     f                                     infds(dbfds10)
     fwmrel     if   e           k disk    rename(pre01:pre00)
     f                                     infds(dbfds3)
      * Chapter Master File
     fwmcpp     uf a e             disk    rename(pcp01:pcp0p)
     f                                     infds(dbfds11)
     fwmcpl     if   e           k disk    rename(pcp01:pcp00)
     f                                     infds(dbfds4)
      * Bank Info File
     fwmbip     uf a e             disk    rename(pbi01:pbi0p)
     f                                     infds(dbfds12)
     fwmbil     if   e           k disk    rename(pbi01:pbi00)
     f                                     infds(dbfds5)
     fwmbil6    if   e           k disk    rename(pbi01:pbi06)
      * Bank Account Master
     fwsckl     if   e           k disk    rename(pck01:pck00)
      * Check Signature file
     fwmsgl1    if   e           k disk    rename(psg01:psg00)
      * E-Pay Vendor Bank Account File
     fwdb2l1    uf a e           k disk    rename(pb201:pb200)
      * E-Pay Vendor Maintenance File
     fwmevml1   if   e           k disk    rename(pevm01:pevm00)
      * User Parameter File
     fsmupl     if   e           k disk
      * Logical over all table files
     fwtabl     if   e           k disk
      * Application System Control file
     fssylf     if   e           k disk
     f                                     ignore(SSY01)
     f                                     ignore(SPC01)
     f                                     ignore(SRC01)
     f                                     ignore(SBC01)
     f                                     ignore(SCS01)
     f*** Beg Add *** 05/23/23 *************************************************
     fsdtbl1    if   e           k disk
     f*** End Add *** 05/23/23 *************************************************
     f***  Beg Add  ***  01/16/24  *** 153WAD **********************************
     fWTCRL     if   e           k disk
     f***  End Add  ***  01/16/24  *** 153WAD **********************************
     f*** begin add ***  09/25/24  ******************************************
     fstscp     if   e             disk    rename(psc01:psc0p)
     f*** end add *****  09/25/24  ******************************************

     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0
     D
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     D
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     D
     d dbfds4          ds
     d  dbrrn4               397    400b 0
     D
     d dbfds5          ds
     d  dbrrn5               397    400b 0
     D
     d dbfds6          ds
     d  dbrrn6               397    400b 0
     D
     d dbfds7          ds
     d  dbrrn7               397    400b 0
     D
     d dbfds8          ds
     d  dbrrn8               397    400b 0
     D
     d dbfds9          ds
     d  dbrrn9               397    400b 0
     D
     d dbfds10         ds
     d  dbrrn10              397    400b 0
     D
     d dbfds11         ds
     d  dbrrn11              397    400b 0
     D
     d dbfds12         ds
     d  dbrrn12              397    400b 0
     D

     DdsSMCIP        e ds                  extname(SMCIP)  inz qualified
     DdsWMFIP        e ds                  extname(WMFIP)  inz qualified
     DdsWMFIPsave      ds                  likeds(dsWMFIP) inz

     D SbsDA           ds            10    dtaara(JOBQSBS)
     D strSbsD                 1     10


     DarrTabNames      s            100    varying inz dim(20)
     dn1               s             10i 0 inz(1)
     dstrTabNames      s           2000    varying inz

      // work variables
     d saveDT          s                   like(FNdt)
     d saveTM          s                   like(FNtime)
     d saveDT2         s                   like(FNdt)
     d saveTM2         s                   like(FNtime)
     d saveDT3         s                   like(FNdt)
     d saveTM3         s                   like(FNtime)
     d saveDT4         s                   like(FNdt)
     d saveTM4         s                   like(FNtime)
     d saveDT5         s                   like(FNdt)
     d saveTM5         s                   like(FNtime)
     d saveDT6         s                   like(FNdt)
     d saveTM6         s                   like(FNtime)
     d saveDT7         s                   like(FNdt)
     d saveTM7         s                   like(FNtime)
     d saveDT8         s                   like(FNdt)
     d saveTM8         s                   like(FNtime)
     d flgRcdUpdated   s              1    inz
     d i               s              3  0
     d j               s              3  0
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     d inCo#           s              3  0
     d inFnd           s              3  0
     d inReg           s              3  0
     d inChp           s              3  0
     d xxRrn           s              9  0 inz
     D indexTab        s              5  0
     D svrrnBank       s              9  0 inz
     D svrrnPolYr      s              9  0 inz
     D svrrnRegion     s              9  0 inz
     D svrrnChapter    s              9  0 inz
     d ##co#           s              3
     d ##fnd           s              3
     d ##fyr           s              3
     d ##case          s              7
     d ##data1         s             75
     d ##srdt          s              8
     d ##call          s              1
     d ##hdcy          s              1
     d ##chkq          s             10
     d ##ftyp          s             10
     d ##rtcd          s              1
     d ##dt            s              7
     d ##cpyq          s             10
     d ##coid          s             20
     d ##acct          s             20
     d sveftc          s              1    inz
     d hasEFT          s               n   inz(*off)
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d error           s               n   inz(*off)
     d error1          s               n   inz(*off)
     d process         s               n   inz(*off)
     d isOptaComp      s               n   inz(*off)
     d reload          s               n   inz(*off)
     d charMsg         s            100
     d stky            s             10    inz
     d xxkey           s             10    inz
     d strDartsKey     s             40
     d strUrlTabName   s             50    inz('iDARTS')
     DdecFnd           s                   like(FNfnd) inz
     d chkappl         s              1    inz
      ***  Begin Add ***** 05/22/23 *****************************
     d ##wcf#          s              3    inz
     d @@grp           s              3  0 inz
     d ##sub#          s              5    inz
     d @@sub#          s              5  0 inz
     d @@rrn1          s              9  0 inz(*zeros)
      ***  End   Add ***** 05/22/23 *****************************
      ***  Begin Add  ***  05/10/23  *****************
     d t2              s              2  0 inz(*zeros)
     d t3              s              2  0 inz(*zeros)
     d* Array to store Tab index numbers, which is clicked by user before SAVE button
     d ClickTab        s              5  0 dim(4)
      ***  End   Add  ***  05/10/23  *****************
      *** Beg Add *** 05/23/23 *************************************************
       dcl-s  inDspFileName char(10);
       dcl-s  inPgmName     char(50);
       dcl-s  inTabName     char(50);
      *** End Add *** 05/23/23 *************************************************
      // Define Arrays for A/R Codes, Headings and Selection Criteria
      // Grid 1 -- Specific A/R Transaction Codes
     d strARcod1       s              2    dim(18)
     d strHeads1       s             40    dim(18)
     d strSelCr1       s            100    dim(18)
      // Grid 2 -- Deposit to Premium Transfer
     d strARcod2       s              2    dim(8)
     d strHeads2       s             40    dim(8)
     d strSelCr2       s            100    dim(8)
      // Grid 3 -- A/R Transfer Codes for Flat Cancellation
     d strARcod3       s              2    dim(11)
     d strHeads3       s             40    dim(11)
     d strSelCr3       s            100    dim(11)
      // Grid 4 -- A/R Transfer Codes for Credit Audit Balance
     d strARcod4       s              2    dim(17)
     d strHeads4       s             40    dim(17)
     d strSelCr4       s            100    dim(17)
      // Grid 5 -- S/R Deposit Transfer A/R Codes (Mid-Year)
     d strARcod5       s              2    dim(8)
     d strHeads5       s             40    dim(8)
     d strSelCr5       s            100    dim(8)
      // Grid 6 -- Collateral A/R Codes
     d strARcod6       s              2    dim(3)
     d strHeads6       s             40    dim(3)
     d strSelCr6       s            100    dim(3)
      // Grid 7 -- Dividend Plan A/R Codes
     d strARcod7       s              2    dim(3)
     d strHeads7       s             40    dim(3)
     d strSelCr7       s            100    dim(3)
     d*
     d* Screen Claim Reserves Codes
     d*
     d                 ds
     d dsScClmRsv              1     20    dim(10)
     d  scRs01                 1      2
     d  scRs02                 3      4
     d  scRs03                 5      6
     d  scRs04                 7      8
     d  scRs05                 9     10
     d  scRs06                11     12
     d  scRs07                13     14
     d  scRs08                15     16
     d  scRs09                17     18
     d  scRs10                19     20
     d*
     d* Claim Reserves Error Indicator
     d*
     d                 ds
     d dsErClmRsv              1     10    dim(10)
     d  erRs01                 1      1
     d  erRs02                 2      2
     d  erRs03                 3      3
     d  erRs04                 4      4
     d  erRs05                 5      5
     d  erRs06                 6      6
     d  erRs07                 7      7
     d  erRs08                 8      8
     d  erRs09                 9      9
     d  erRs10                10     10
     d*
     d* Claim Reserves Message Id
     d*
     d                 ds
     d dsMsClmRsv              1    100    dim(10)
     d  msRs01                 1     10
     d  msRs02                11     20
     d  msRs03                21     30
     d  msRs04                31     40
     d  msRs05                41     50
     d  msRs06                51     60
     d  msRs07                61     70
     d  msRs08                71     80
     d  msRs09                81     90
     d  msRs10                91    100
      //
     D ##Wcif          s              1
     D ##Mcif          s              1
     D ##Wcin          s              1
      //
     d @@co#           s              3  0
     d @@fnd           s              3  0
     d @@kc            s              1
     d @@kl            s              1
     d @@rtcd          s              1
     d @@objname       s             10
     d @@objtype       s              7
     d @@err           s              1
     d @@folder        s             80
     d @@docid         s             12
     d @@jobq          s             10
     d @@jobqlib       s             10
     d @@jobqsts       s             10
     d @@sbs           s             10
     d @@nojobs        s             10  0
      ***  Begin Add ***** 09/18/23 *****************************
     d ##obj           s             10
     d ##type          s              7
     d ##lib           s             10
     d @@lib           s             10
     d suffix          s              3    inz

     d outCo           s              3    inz
     d outFnd#         s              3    inz
     d outJobQ         s             10    inz
     d outName         s             10    inz
     d outSbs          s             10    inz
     d outSbs1         s             10    inz
     d outMsg          s            132    inz
     d outRtcd         s              1    inz
      ***  End   Add ***** 09/18/23 *****************************

      // Proto-types

      // Check Object
     d CheckObj        pr                  extpgm('SRCHKOBJ')
     d @@objname                     10
     d @@objtype                      7
     d @@err                          1

      // Check Object Name is Valid
     d CheckObjName    pr                  extpgm('SRCKOBJN')
     d @@objname                     10
     d @@err                          1

      ***  Begin Add ***** 09/18/23 *****************************
      // Check Object with Library
     d CheckObj2       pr                  extpgm('SRCHKOB2')
     d @@lib                         10
     d @@objname                     10
     d @@objtype                      7
     d @@err                          1

      // Create JobQ
     dcrtJobQ          pr                  extpgm('W0010C02')
     d outCo                               like(outCo)
     d outFnd#                             like(outFnd#)
     d outJobQ                             like(outJobQ)
     d outName                             like(outName)
     d outSbs                              like(outSbs)
     d outSbs1                             like(outSbs1)
     d outMsg                              like(outMsg)
     d outRtcd                             like(outRtcd)
      ***  End   Add ***** 09/18/23 *****************************

      ***  Begin Add ***** 05/22/23 *****************************
      // WCMC Add Group Pompt
     d F0062R          pr                  extpgm('F0062R')
     d @@kc                           1
     d @@kl                           1
     d @@fnd                          3  0
      // Check if Managed Care Record Exists
     d FA700R          pr                  extpgm('FA700R')
     d @@rtcd                         1
     d ##co#                          3
     d ##wcf#                         3
     d ##sub#                         5
     d ##fnd                          3
      // Add Managed Care Group Information
     d FA027R          pr                  extpgm('FA027R')
     d @@co#                          3  0
     d @@sub#                         5  0
     d @@grp                          3  0
     d @@fnd                          3  0
     d @@rrn1                         9  0
      ***  End   Add ***** 05/22/23 *****************************

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      // Determine if EDI Format ID exists in any group
     d GetEDIF         pr                  extpgm('WA075R')
     d @@co#                          3  0
     d @@fnd                          3  0
     d @@rtcd                         1

     d GetSystem       pr                  extpgm('SA001R')
     d ##Wcif                         1
     d ##Mcif                         1
     d ##Wcin                         1

      ***  Begin Add ***** 09/18/23 *****************************
      // Retrieve Client Suffix
     d RtvCltSuffix    pr                  ExtPgm('SA030R')
     d suffix                         3

      // Retrieve Object Library
     d RtvObjLib       pr                  extpgm('RTVOBJLIB')
     d ##obj                         10
     d ##type                         7
     d ##lib                         10
      ***  End   Add ***** 09/18/23 *****************************

      // Retrieve JobQ attributes
     d RtvJobQA        pr                  extpgm('RTVJOBQA')
     d @@jobq                        10
     d @@jobqlib                     10
     d @@jobqsts                     10
     d @@sbs                         10
     d @@nojobs                      10  0

      // Validate the Existance of a Document
     d ValidDoc        pr                  extpgm('SVALDOC')
     d @@docid                       12
     d @@folder                      80
     d @@rtcd                         1

      // Validate the Existance of a Folder
     d ValidFlr        pr                  extpgm('SVALFLR')
     d @@folder                      80
     d @@rtcd                         1


      * Initialize the Screen fields for Additional Information
     d InzAddInfScVal  pr

      * Initialize the Screen fields for Financial Information
     d InzFinInfScVal  pr
     d strmode                        1

      * Initialize the Screen fields for Policy Years/ Group Details
     d InzGrpDtlScVal  pr

      * Initialize the Screen fields for Group Master info
     d InzGrpInfScVal  pr

      // Load Group Auxiliary Info
     d LoadAuxInfo     pr

      // Load Bank Info Grid
     d LoadBankGrid    pr

      // Load Group Carrier Info
     d LoadCarInfo     pr

      // Load Chapter Info Grid
     d LoadChpGrid     pr

      // Load Flexible Heading Info
     d LoadFlxInfo     pr

      // Load Group Master Info
     d LoadMstInfo     pr

      // Load Region Info Grid
     d LoadRegionGrid  pr

      // Load Policy Years Grid Info
     d LoadYearsGrid   pr

      * $ain Processing
     d Main            pr

      * Process Bank Info Addition
     d ProcessAddBank  pr

      * Process Chapter Addition
     d ProcessAddChp   pr

      * Process Region Addition
     d ProcessAddReg   pr

      * Process Policy Years/Group Details Addition
     d ProcessAddYear  pr

      * Process other Options for Policy Years Grid
     d ProcOther2      pr

      * Process other Options for Region Grid
     d ProcOther3      pr

      * Process other Options for Chapter Grid
     d ProcOther4      pr

      * Process other Options for Bank info Grid
     d ProcOther5      pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     * Validate Additional info. procedure.
     d ValidAddInfo    pr              n

     * Validate Bank info. procedure.
     d ValidateBnkDtl  pr              n
     d strmode                        1

     * Validate Chapter Details info. procedure.
     d ValidateChpDtl  pr              n
     d strmode                        1

     * Validate Group Addition info. procedure.
     d ValidateGroup   pr              n

     * Validate Group Details info. procedure.
     d ValidateGrpDtl  pr              n
     d strmode                        1

     * Validate Region Details info. procedure.
     d ValidateRegDtl  pr              n
     d strmode                        1


     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrSql
     d/copy sPrMessage
     d/copy sPrModule
     d/copy sCopyCompr
     d/copy sprCompany
     d/copy sPrTable
     d/copy sPrTrigger
     d/copy sPrTrig
     d/copy wPrTable
     d/copy sPrAtrium
     d/copy wPrPolErr
     d/copy wPrGroup
     d/copy wPrBank
      //*** Beg Add ***** 01/17/22 *****************************
     d/copy sCopyGrpPr
      //*** End Add ***** 01/17/22 *****************************
      *** Beg Add *** 05/23/23 ************************************************
     d/copy sPrSecure
      *** End Add *** 05/23/23 ************************************************
      *** Beg Add ***  04/29/24  *** WC2965 *************************
     d/copy sPrGenErr
      *** End Add ***  04/29/24  *** WC2965 *************************

      //
      //   Key List for the Documents Table File
      //

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9
     c                   parm                    @@mode            1
     c                   parm                    @@next           10
     c                   parm                    @@TabInd          5

     c     keyWDF1L      klist
     c                   kfld                    scCo#
     c                   kfld                    scFnd
     c                   kfld                    scFyr

     c     keyWMREL      klist
     c                   kfld                    scCo#
     c                   kfld                    scFnd
     c                   kfld                    inReg

     c     keyWMCPL      klist
     c                   kfld                    scCo#
     c                   kfld                    scFnd
     c                   kfld                    inChp

     c     keyWMBIL6     klist
     c                   kfld                    scCo#
     c                   kfld                    scAppl
     c                   kfld                    scFnd
     c                   kfld                    scCktp
     c                   kfld                    scAcct
     c                   kfld                    inBkEfDt          7 0

      /free
       Exec Sql Set Option Commit = *None;

       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Endif;

       Chain xxRrn pfn0p;
       inCo# = FNco#;
       inFnd = FNfnd;
       scCo# = FNco#;
       scFnd = FNfnd;

       If (@@next = *Blanks or @@next = 'WTGRPPRI');
          GetClientID(clientID);

          ProgramStart(process);

          If process;
          //*** Beg Add *** 05/23/23 ******************************************
          If TbNmRcdPnl <> *Blanks;
          //*** End Add *** 05/23/23 ******************************************

             Main();

          //*** Beg Add *** 05/23/23 ******************************************
          Else;
             LoadActionBar();
             isNoAccess = *on;
             TbNmRcdPnl = 'Security';
             pnlHeight = 560;
             aPnlHeight = 620;
             Write rcdGrpPnl;
             Exfmt rcdPanel;
             Read rcdGrpPnl;
             If scSelOpt <> *blanks;
                @@next = scSelOpt;
             Endif;
          Endif;
          //*** End Add *** 05/23/23 ******************************************
          Endif;
       Endif;

      // Load Region Search Screen
       If @@next = 'REGION';
          If scCo# = 0;
             intCo# = GetUserInfo(q1user);
             scCo# = intCo#;
          EndIf;
          If scFnd = 0;
             Chain q1user pup01;
             If %found(smupl);
                scFnd = UPfnd;
             EndIf;
          EndIf;

         // Panel Heading
          scPnlHdg = 'Select Region - Group ' + %char(scFnd);
          Chain(n) (scCo#:scFnd) pfn00;
          If %found(wmfnl);
             scPnlHdg = %trim(scPnlHdg) + '-' + FNname;
          EndIf;

         // Load Region Info
          LoadRegionGrid();
          visbPnl = *on;                              // Show Panel for Search Screen
          visbRegCan = *on;                           // Show cancel Button
          visbChpCon = *on;                           // Hide Chapter Grid
          TopRegion = 105;                            // Top of Region Grid
          Exfmt rcdAddInf;

          ReadC sflRegion;
          If btnRegSel = *on;
             btnRegSel = *off;
             @@TabInd = '  ' + %char(scRgn#);
          Endif;
       Endif;

      // Load Chapter Search Screen
       If @@next = 'CHAPTER';
          If scCo# = 0;
             intCo# = GetUserInfo(q1user);
             scCo# = intCo#;
          EndIf;
          If scFnd = 0;
             Chain q1user pup01;
             If %found(smupl);
                scFnd = UPfnd;
             EndIf;
          EndIf;

         // Panel Heading
          scPnlHdg = 'Select Chapter - Group ' + %char(scFnd);
          Chain(n) (scCo#:scFnd) pfn00;
          If %found(wmfnl);
             scPnlHdg = %trim(scPnlHdg) + '-' + FNname;
          EndIf;

         // Load Chapter Info
          LoadChpGrid();
          visbPnl = *on;                              // Show Panel for Search Screen
          visbChpCan = *on;                           // Show cancel Button
          visbRegCon = *on;                           // Hide Region Grid
          TopChapter = 105;                           // Top of Chapter Grid
          Exfmt rcdAddInf;

          ReadC sflChapter;
          If btnChpSel = *on;
             btnChpSel = *off;
             @@TabInd = '  ' + %char(scChp#);
          Endif;
       Endif;

       *InLr = *On;
      /end-free

       //===================================================================
       // Add information to list
       //===================================================================
     PAddToList        b
     DAddToList        pi           101a   varying
     D inString                     100a   varying const
     D inArray                      100a   varying dim(20)
     D inIndex                       10i 0

     DoutString        s            101a   varying inz
      /Free
       If inIndex > 1;
          outString = ',';
       Endif;
       outString += inString;
       inArray(inIndex) = inString;
       inIndex += 1;
       Return outString;
      /End-free
     PAddToList        e

       //===================================================================
       // InzAddInfScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzAddInfScVal  b
     d InzAddInfScVal  pi
     d
      /Free
       scCo# = intCo#;
       scCltid = clientID;

      // Set selection criteria for Table Id
        scTbidCH = 'GIco# = ' + %EditC(scCo#:'X') + ' and ' +
         'GIstat <> ' + '''I''';

      /End-Free
     p InzAddInfScVal  e

       //===================================================================
       // InzFinInfScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzFinInfScVal  b
     d InzFinInfScVal  pi
     d strmode                        1
      /Free
       scCo# = intCo#;
       scCltid = clientID;

      // Check If E-Pay is installed and setup at bank account master WSCKL
      // Then Check if the vendor is setup for the Application code for this account
        visbEpay = isModEnabled('E-Pay');
        Chain (inCo#:scAcct) pck00;
        If CKePayVn = *blanks;
           visbEpay = *off;
        Endif;
        If visbEpay = *on;
           visbEpay = *off;
           Setll *loval pevm00;
           Read pevm00;
           Dow Not %Eof(WMEVML1);
              If EVMappl = BIappl and EVMVend = CKePayVn and strmode = 'C';
                 visbEpay = *on;
                 Leave;
              Endif;
              If EVMVend = CKePayVn and strmode = 'A';
                 visbEpay = *on;
                 Leave;
              Endif;
              Read pevm00;
           Enddo;
        Endif;

       // Set visibilty for Payment (reserve) types based on vendor
        isEpClmInd = *Off;
        isEpClmMed = *Off;
        isEpClmLeg = *Off;
        isEpClmExp = *Off;
        visEpayTyp = *Off;

        If visbEpay = *on;
           Setll *loval pevm00;
           Reade CKePayVn pevm00;
           Dow Not %Eof(wmevml1);
              If EVMpTyp = 'C';
                 isEpClmInd = *On;
              Endif;
              If EVMpTyp = 'M';
                 isEpClmMed   = *On;
              Endif;
              If EVMpTyp = 'L';
                 isEpClmLeg = *On;
              Endif;
              If EVMpTyp = 'E';
                 isEpClmExp = *On;
              Endif;
              Reade CKePayVn pevm00;
           Enddo;
           If isEpClmInd = *On or isEpClmMed = *On or isEpClmLeg = *On or
            isEpClmExp = *On;
              visEpayTyp = *On;
           Endif;
        Endif;

      // Set selection criteria for Vendor Pay Funding Account Code Table Selection
        scVpFacTCH = 'GTco# = ' + %EditC(scCo#:'X') + ' and ' +
           'GTtbid = ' + '''VPFA''' + ' and ' + 'GTstat <> ' + '''I''';

      // Set selection criteria for Bank Account Number
        scAcctCH = 'CKco# = ' + %EditC(scCo#:'X') + ' and ' +
         'CKstat <> ' + '''I''';

      // Set selection criteria for Application Type
          scApplCH = 'GTco# = ' + %EditC(scCo#:'X') + ' and ' +
           'GTtbid = ' + '''BTYP''' + ' and ' + 'GTstat <> ' + '''I''';

      // Set selection criteria for Signature
        scSignCH = 'SGco# = ' + %EditC(scCo#:'X') + ' and ' +
         'SGstat <> ' + '''I''';

      // Set visibilty for Text on Check
           visTxtOnCk = *off;
        If clientID = '126';
           visTxtOnCk = *on;
        Endif;

      // Set visibilty for Claim Check Print Values
        visCkpv = *off;
        If clientID = '025';
           visCkpv = *on;
        Endif;

      // Set visibilty for Print 2 blank lines if over $ limit (Checkbox)
        vis1A2 = *on;
        If clientID = '129';
           vis1A2 = *on;
        Endif;



       // Need to remove following later //
        // visTxtOnCk = *on;
        // visCkpv = *on;
        // vis1A2 = *on;




      /End-Free
     p InzFinInfScVal  e

       //===================================================================
       // InzGrpDtlScVal - Initialize the Screen fields and indicators for Group Details
       //===================================================================
     p InzGrpDtlScVal  b
     d InzGrpDtlScVal  pi
     d
      /Free
       scCo# = intCo#;
       scCltid = clientID;

       // Determine if client is Admin only or Claims only
       visbAdmin = *off;
       visbClaim = *off;
       Chain 'WC' SWC01;
       If %found();
          If WCfl08 = 'A';                          // Admin Only
             visbAdmin = *on;
          Endif;
          If WCfl08 = 'C';                          // Claims only
             visbClaim = *on;
          Endif;
       Endif;

      // Set selection criteria for Self-reporting Bill
        strSelCr1(1) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''B''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for New Business Payment
        strSelCr1(2) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Payment Transfer
        strSelCr1(3) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for S/R Discount Code
        strSelCr1(4) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''B''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Membership Dues Transfer
        strSelCr1(5) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''M''';

      // Set selection criteria for Safety Fees Transfer
        strSelCr1(6) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''S''';

      // Set selection criteria for Premium Transfer
        strSelCr1(7) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Deposit Transfer
        strSelCr1(8) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''I''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Expense Constant Transfer
        strSelCr1(9) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''E''';

      // Set selection criteria for First Assessment Transfer
        strSelCr1(10) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''1''';

      // Set selection criteria for Second Assessment Transfer
        strSelCr1(11) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''2''';

      // Set selection criteria for Third Assessment Transfer
        strSelCr1(12) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''3''';

      // Set selection criteria for Fourth Assessment Transfer
        strSelCr1(13) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''4''';

      // Set selection criteria for Fifth Assessment Transfer
        strSelCr1(14) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''5''';

      // Set selection criteria for Sixth Assessment Transfer
        strSelCr1(15) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''6''';

      // Set selection criteria for Seventh Assessment Transfer
        strSelCr1(16) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''7''';

      // Set selection criteria for State Tax Transfer
        strSelCr1(17) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''T''';

      // Set selection criteria for State Tax on Deposit Transfer
        strSelCr1(18) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Deposit Billing Offset
        strSelCr2(1) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''H''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Tax on Deposit Billing Offset
        strSelCr2(2) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''F''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Deposit Payment Offset
        strSelCr2(3) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''I''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Tax on Deposit Payment Offset
        strSelCr2(4) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Deposit to Premium Transfer
        strSelCr2(5) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Tax on Deposit to Premium Transfer
        strSelCr2(6) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''T''';

      // Set selection criteria for Deductible Tax Bill
        strSelCr2(7) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''F''' + 'and TTctyp=' +'''R''';

      // Set selection criteria for Deductible Tax Paid
        strSelCr2(8) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''R''';

      // Set selection criteria for Membership Fees
        strSelCr3(1) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''M''';

      // Set selection criteria for Safety Fees
        strSelCr3(2) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''S''';

      // Set selection criteria for State Tax
        strSelCr3(3) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''T''';

      // Set selection criteria for Premium Payment
        strSelCr3(4) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for 1st Assessment
        strSelCr3(5) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''1''';

      // Set selection criteria for 2nd Assessment
        strSelCr3(6) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''2''';

      // Set selection criteria for 3rd Assessment
        strSelCr3(7) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''3''';

      // Set selection criteria for 4th Assessment
        strSelCr3(8) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''4''';

      // Set selection criteria for 5th Assessment
        strSelCr3(9) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''5''';

      // Set selection criteria for 6th Assessment
        strSelCr3(10) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''6''';

      // Set selection criteria for 7th Assessment
        strSelCr3(11) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''7''';

      // Set selection criteria for Deductible
        strSelCr4(1) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''R''';

      // Set selection criteria for Expense Constant
        strSelCr4(2) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''E''';

      // Set selection criteria for General Fees
        strSelCr4(3) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''F''';

      // Set selection criteria for Late Fees
        strSelCr4(4) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''L''';

      // Set selection criteria for Membership Fees
        strSelCr4(5) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''M''';

      // Set selection criteria for Deposit
        strSelCr4(6) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''I''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Safety Fees
        strSelCr4(7) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''S''';

      // Set selection criteria for Tax
        strSelCr4(8) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''T''';

      // Set selection criteria for Tax on Deposit
        strSelCr4(9) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for 1st Assessment
        strSelCr4(10) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''1''';

      // Set selection criteria for 2nd Assessment
        strSelCr4(11) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''2''';

      // Set selection criteria for 3rd Assessment
        strSelCr4(12) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''3''';

      // Set selection criteria for 4th Assessment
        strSelCr4(13) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''4''';

      // Set selection criteria for 5th Assessment
        strSelCr4(14) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''5''';

      // Set selection criteria for 6th Assessment
        strSelCr4(15) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''6''';

      // Set selection criteria for 7th Assessment
        strSelCr4(16) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''7''';

      // Set selection criteria for Premium Transfer
        strSelCr4(17) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Deposit Billing Offset SR
        strSelCr5(1) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''H''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Tax on Deposit Billing Offset SR
        strSelCr5(2) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''F''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Deposit Paid Offset SR
        strSelCr5(3) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''I''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Tax on Deposit Paid Offset SR
        strSelCr5(4) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Deposit to Premium Transfer SR
        strSelCr5(5) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Tax on Deposit to Premium Transfer SR
        strSelCr5(6) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''T''';

      // Set selection criteria for Deposit Premium Offset SR
        strSelCr5(7) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''B''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Tax on Deposit Premium Offset SR
        strSelCr5(8) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''F''' + 'and TTctyp=' +'''T''';

      // Set selection criteria for Collateral Billed
        strSelCr6(1) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''H''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Collateral A/R Paid
        strSelCr6(2) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''I''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Collateral A/R Transferred
        strSelCr6(3) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''I''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for Dividend Billed
        strSelCr7(1) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''B''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Dividend Paid
        strSelCr7(2) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for Dividend Payment Transfer
        strSelCr7(3) = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';



      // Set Headings for A/R Codes
        strHeads1(1) = 'Self-reporting Bill';
        strHeads1(2) = 'New Business Payment';
        strHeads1(3) = 'Payment Transfer';
        strHeads1(4) = 'S/R Discount Code';
        strHeads1(5) = 'Membership Dues Transfer';
        strHeads1(6) = 'Safety Fees Transfer';
        strHeads1(7) = 'Premium Transfer';
        strHeads1(8) = 'Deposit Transfer';
        strHeads1(9) = 'Expense Constant Transfer';
        strHeads1(10) = 'First Assessment Transfer';
        strHeads1(11) = 'Second Assessment Transfer';
        strHeads1(12) = 'Third Assessment Transfer';
        strHeads1(13) = 'Fourth Assessment Transfer';
        strHeads1(14) = 'Fifth Assessment Transfer';
        strHeads1(15) = 'Sixth Assessment Transfer';
        strHeads1(16) = 'Seventh Assessment Transfer';
        strHeads1(17) = 'State Tax Transfer';
        strHeads1(18) = 'State Tax on Deposit Transfer';

        strHeads2(1) = 'Deposit Billing Offset';
        strHeads2(2) = 'Tax on Deposit Billing Offset';
        strHeads2(3) = 'Deposit Payment Offset';
        strHeads2(4) = 'Tax on Deposit Payment Offset';
        strHeads2(5) = 'Deposit to Premium Transfer';
        strHeads2(6) = 'Tax on Deposit to Premium Transfer';
        strHeads2(7) = 'Deductible Tax Bill';
        strHeads2(8) = 'Deductible Tax Paid';

        strHeads3(1) = 'Membership Fees';
        strHeads3(2) = 'Safety Fees';
        strHeads3(3) = 'State Tax';
        strHeads3(4) = 'Premium Payment';
        strHeads3(5) = '1st Assessment';
        strHeads3(6) = '2nd Assessment';
        strHeads3(7) = '3rd Assessment';
        strHeads3(8) = '4th Assessment';
        strHeads3(9) = '5th Assessment';
        strHeads3(10) = '6th Assessment';
        strHeads3(11) = '7th Assessment';

        strHeads4(1) = 'Deductible';
        strHeads4(2) = 'Expense Constant';
        strHeads4(3) = 'General Fees';
        strHeads4(4) = 'Late Fees';
        strHeads4(5) = 'Membership Fees';
        strHeads4(6) = 'Deposit';
        strHeads4(7) = 'Safety Fees';
        strHeads4(8) = 'Tax';
        strHeads4(9) = 'Tax on Deposit';
        strHeads4(10) = '1st Assessment';
        strHeads4(11) = '2nd Assessment';
        strHeads4(12) = '3rd Assessment';
        strHeads4(13) = '4th Assessment';
        strHeads4(14) = '5th Assessment';
        strHeads4(15) = '6th Assessment';
        strHeads4(16) = '7th Assessment';
        strHeads4(17) = 'Premium Transfer';

        strHeads5(1) = 'Deposit Billed Offset';
        strHeads5(2) = 'Tax on Deposit Billed Offset';
        strHeads5(3) = 'Deposit Paid Offset';
        strHeads5(4) = 'Tax on Deposit Paid Offset';
        strHeads5(5) = 'Deposit to Premium Transfer';
        strHeads5(6) = 'Tax on Deposit to Premium Transfer';
        strHeads5(7) = 'Deposit Premium Offset';
        strHeads5(8) = 'Tax on Deposit Premium Offset';

        strHeads6(1) = 'Billed';
        strHeads6(2) = 'Paid';
        strHeads6(3) = 'Transferred';

        strHeads7(1) = 'Dividend Billed';
        strHeads7(2) = 'Dividend Paid';
        strHeads7(3) = 'Dividend Payment Transfer';


      // Set visibilty for Percentage of Manual Premium Target
        visbManp = *off;
        If clientID = '150';
           visbManp = *on;
        Endif;

      // Set visibilty for Bill Expense Constant on all Divisions
        visbExdv = *off;
        If clientID = '088' or clientID = '090';
           visbExdv = *on;
        Endif;

      // Set visibilty for Collateral A/R Codes fields
        visbCollAR = *off;
        If clientID = '150';
           visbCollAR = *on;
        Endif;

      // Set visibilty for Do not process 10/1/07 NY rate decrease
        visbNy07 = *off;
        If clientID = '129';
           visbNy07 = *on;
        Endif;

      // Set visibilty for Dividend Plan (3) fields
        visbCSIOPT  = *off;
        isOptaComp = *Off;
        isOptaComp = GetGrpAssFlg(FNco#:FNfnd);
        If isOptaComp or clientID = '114';
           visbCSIOPT = *On;
        Endif;

      // Set visibilty for Do not process 10/1/07 NY rate decrease
        visbScdt = *off;
        if WCfl44 = 'Y';
           visbScdt = *on;
        Endif;

       //*** Begin Add *** 05/14/24 ***************************************
       vis146    = *Off;
       If clientID = '146';
          vis146    = *On;
       Endif;
       //*** End   Add *** 05/14/24 ***************************************




       // Need to remove following later //
        // visbManp = *on;
        // visbExdv = *on;
        // visbCollAR = *on;
        // visbNy07 = *on;
        // visbCSIOPT = *On;
        // visbScdt = *on;





      /End-Free
     p InzGrpDtlScVal  e

       //===================================================================
       // InzGrpInfScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzGrpInfScVal  b
     d InzGrpInfScVal  pi
     d
      /Free
       scCo# = intCo#;
       scCltid = clientID;

      // Disable Group Number and Case Number
       GrpAddEdt = *On;

       // Determine if client is Admin only or Claims only
       visbAdmin = *off;
       visbClaim = *off;
       Chain 'WC' SWC01;
       If %found();
          If WCfl08 = 'A';                          // Admin Only
             visbAdmin = *on;
          Endif;
          If WCfl08 = 'C';                          // Claims only
             visbClaim = *on;
          Endif;
       Endif;

      // Set selection criteria for Default Vendor Number
        scVndrCH = 'VPco# = ' + %EditC(scCo#:'X');

      // Set selection criteria for Group Association
        scGrpAscCH = 'GTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'GTstat<>' + '''I''' + 'and GTtbid=' + '''GRPA''';

      // Set selection criteria for TPA Contact Code
        scTpacCH = 'TCco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TCstat<>' + '''I''';

      // Set selection criteria for Certification of Insurance Overlay
        scCeovCH = 'GTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'GTstat<>' + '''I''' + 'and GTtbid=' + '''COVL''';

      // Set selection criteria for A/R Deposit Payment Write-off Code
        scDbcdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''I''' + 'and TTctyp=' +'''D''';

      // Set selection criteria for A/R Premium Payment Write-off Code
        scPbcdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''P''' + 'and TTctyp=' +'''P''';

      // Set selection criteria for A/R Fee Payment Write-off Code
        scFbcdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''F''';

      // Set selection criteria for A/R Assessment Write-off Code (1 to 7)
        sc1acdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''1''';
        sc2acdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''2''';
        sc3acdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''3''';
        sc4acdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''4''';
        sc5acdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''5''';
        sc6acdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''6''';
        sc7acdCh = 'TTco# = ' + %EditC(scCo#:'X') + ' and ' +
         'TTstat=' + '''A''' + 'and TTtype=' + '''Q''' + 'and TTctyp=' +'''7''';

      // call API to Check the fund file to see if EDI Format ID is required of not
        visbEdif = *off;
        @@co# = scCo#;
        @@fnd = 0;
        @@rtcd = ' ';
        GetEDIF(@@co#:@@fnd:@@rtcd);
        If @@rtcd = 'F';
           visbEdif = *on;
        Endif;

      // Set visibilty for Max extended year policy (in months)
      // Set visibilty for Commercial Group
        visbMxe  = *off;
        visbCgfl = *off;
        if WCfl07 = 'Y';                               //WCfl07=Y Commercial Application Installed
           visbMxe  = *on;
           visbCgfl = *on;
        Endif;

      // Set visibilty for Include Group for CICenter
        visbCicf = *off;
        if WCfl19 = 'Y';
           visbCicf = *on;
        Endif;

      // Set visibilty for Group Association
        visbGrpAsc = *off;
        If clientID = '114' or clientID = '147' Or clientID = '152';
           visbGrpAsc = *on;
        Endif;

      // Set visibilty for Excess Insurance Group
        visbExIns = *off;
        If clientID = '146';
           visbExIns = *on;
        Endif;

      // Set visibilty for PEO Group - Policy Type
        visbPeof = *off;
        If WCfl30 = 'Y' or clientID = '114';
           visbPeof = *on;
        Endif;

      // Set visibilty for checkbox - Charge Expense Constant to Emp #1 for PEO Group
      // and Calculate Stock Discount Across Entire PEO Group
        visbPeoe = *off;
        visbPeoa = *off;
        If WCfl30 = 'Y';
           visbPeoe = *on;
           visbPeoa = *on;
        Endif;

      // Set visibilty and Heading for Portal Group fields
       isPortal = *off;
       isPortal = IsModEnabled('Portal Module');
        If isPortal = *on;
           scPortFlag = 'Include Group for ' +
            GetModuleProperty('Portal Module':'Name');
        Endif;

      // Set visibilty for Due Date for Weekly Interim Payments
        visbWidy = *off;
        If clientID = '148';
           visbWidy = *on;
        Endif;

      // Set visibilty for Claims Elig Flag
        visbClElig = *off;
        if isModEnabled('Claims Export');
           visbClElig = *on;
        Endif;

      // Set visibilty for WCPOLS electronic Reporting
        visbWcpl = *off;
        if WCfl03 = 'Y' and WCfl08 <> 'C';             //WCfl03=Y WCPOLS, WCfl08=C Claims only
           visbWcpl = *on;
        Endif;

      // Set visibilty for Free Form Text for Check Processing
        visbFref = *off;
        If clientID = '114';
           visbFref = *on;
        Endif;

      // Set visibilty for Disable SMS Texting
        visbDissms = *off;
        if isModEnabled('RelaySMS');
           visbDissms = *on;
        Endif;

      // Set visibilty for Output Logo File Name and Path
        visbLogo = *off;
        if isModEnabled('Scriptura-Documents')
         or isModEnabled('Scriptura-Various')
         or isModEnabled('3rd Party XML');
           visbLogo = *on;
        Endif;

      // Set visibilty for JobQ and Subsystem
        visbJobSbs = *off;
        if WCfl51 = 'Y';                               //JobQ/Subsystem
           visbJobSbs = *on;
        Endif;

      // Set visibilty for NCCI Carrier Group Code
        visbNCarGr = *off;
        If isModEnabled('IDC');
           visbNCarGr = *on;
        Endif;

      // Initialize Checkbox - Schedule of Operations Division & Print Order (A/D)
        scDepo = 'A';

      // Initialize Checkbox - Premium Verbiage
        scPrmv = 'P';

      // Initialize Default Subsystem for Billing
        // Read Date from JOBQSBS Data Area
        In SbsDA;
        scSbsD = strSbsD;





       // Need to remove following later //
        // visbGrpAsc = *on;
        // visbExIns = *on;
        // visbWidy = *on;
        // visbFref = *on;
        // visbDissms = *on;
        // visbJobSbs = *on;




      /End-Free
     p InzGrpInfScVal  e

       //===================================================================
       // Load Group Auxiliary Info
       //===================================================================
     p LoadAuxInfo     b
     d LoadAuxInfo     pi

      /free
       //***  Beg Add  ***  01/16/24  *** 153WAD ***********************
         If VisClt153 = *On;
          Clear scCxRsnCh;
          Clear scCxRsnVl;

            SetLL (inCo#) WTCRL;
            ReadE (inCo#) WTCRL;
            Dow Not %EoF(WTCRL);
               If CRstat = 'A';
                  If scCxRsnCh  = *Blanks;
                     scCxRsnCh  = ' ,' + %trim(CRKey) + ' - ' + %trim(CRDesc);
                     scCxRsnVl  = ' ,' + %trim(CRKey);
                  Else;
                     scCxRsnCh  = %trim(scCxRsnCh) + ',' + %trim(CRKey) +
                                  ' - ' + %Trim(CRDesc);
                     scCxRsnVl  = %trim(scCxRsnVl) + ',' + %trim(CRKey);
                  Endif;
               EndIf;
               ReadE (inCo#) WTCRL;
            EndDo;
         EndIf;
       //***  End Add  ***  01/16/24  *** 153WAD ***********************
       //*** begin add ***  09/25/24  *************************************
       //* Read Scriptura module file.
       Read(n) psc0p;
       isScriptDC = IsModEnabled('Design and Compose');
       //*** end add *****  09/25/24  *************************************

       Chain(n) (inCo#:inFnd) pf200;
       If %found(wdf2l);
          scFein = F2fein;
          scTcc  = F2tcc;
          scIaud = F2iaud;
          scSctc = F2sctc;

          If F2area <> *zeros or F2sph# <> *zeros;
             scSphone = %int(%editc(F2area:'X') + %editc(F2sph#:'X'));
          Endif;

          scCar# = F2car#;
          scWcpl = F2wcpl;
          scClck = F2clck;
          scAgck = F2agck;
          scMacc = F2macc;
          scPp3c = F2pp3c;
          scPpc3 = F2ppc3;
          scFold = F2fold;
          scDcid = F2dcid;

          scCtyp = F2ctyp;
          scTpa  = F2tpa;
          //*** Beg Del ***** 04/26/23 *****************************
          //*** scLbnm = F2lbnm;
          //*** End Del ***** 04/26/23 *****************************
          scLad1 = F2lad1;
          scLad2 = F2lad2;
          scLcty = F2lcty;
          scLst  = F2lst;
          scLzip = F2lzip;
          scBlog = F2blog;
          scCeov = F2ceov;
          scDocl = F2docl;
          scDocs = F2docs;
          scNaic = F2naic;
          scRre# = F2rre#;
          scTpac = F2tpac;
          scFysm = F2fysm;
          scSpsg = F2spsg;
          scJobq = F2jobq;
          scSbs  = F2sbs;
          scThrl = F2thrl;
          scThrh = F2thrh;
          scDbcd = F2dbcd;
          scPbcd = F2pbcd;
          scFbcd = F2fbcd;
          sc1acd = F21acd;
          sc2acd = F22acd;
          sc3acd = F23acd;
          sc4acd = F24acd;
          sc5acd = F25acd;
          sc6acd = F26acd;
          sc7acd = F27acd;
          scDissms = F2dissms;
          scSlogo = F2slogo;
          scSpath = F2spath;
       //*** begin add ***  09/25/24  *************************************
          scSURL  = F2SURL;
       //*** end add *****  09/25/24  *************************************
          scFref = F2fref;
          scBsi# = F2bsi#;
          scNcargrp = F2nCarGrp;
          sc1099fein = F21099fein;
          //*** Beg Add ***** 11/17/23 *****************************
          scDxtr = F2dxtr;
          //*** End Add ***** 11/17/23 *****************************
          //***  Beg Add  ***  01/16/24  *** 153WAD ***********************
          scRnote  = F2Rnote;
          scRcxRsn = F2RcxRsn;
          //***  End Add  ***  01/16/24  *** 153WAD ***********************

          saveDT2 = F2dt;
          saveTM2 = F2time;
       Endif;

      /end-free
     p LoadAuxInfo     e

       //===================================================================
       // Load Bank Info Grid
       //===================================================================
     p LoadBankGrid    b
     d LoadBankGrid    pi

      /free
        // Clear the subfile
        rrnBank = 0;
        dspBank = *off;
        clrBank = *on;
        Write rcdFinInf;
        clrBank = *off;
        dspBank = *on;
        scBnRowSel = 1;

        If dspMode;
           inquiry_2 = *on;
        Endif;

        // Populate the subfile
        Setll (scCo#:scFnd) wmbil;
        Reade (scCo#:scFnd) wmbil;
        Dow not %eof(wmbil);
           rrnBank += 1;
           scBnkSel = *off;

           If svrrnBank = *zeros;
              If rrnBank = 1;
                 scBnkSel = *on;
              Endif;
           Else;
              If rrnBank = svrrnBank;
                 scBnkSel = *on;
              Endif;
           Endif;

           scBnkGrp = BIfnd;
           scAppl = BIappl;
           scCktp = BIcktp;
           scBnkEfdt = CvtToDate(BIefdt);
           scAcct = BIacct;
           scEftc = BIeftc;
           scBnkStat = *Blanks;

           //*** Begin Chg ***** 08/28/23 *****************************
           // scActivate = *Blanks;
           // If BIstat = 'A';
           //    scBnkStat = 'Active';
           //    scActivate = 'Inactivate';
           // Endif;
           // If BIstat = 'I';
           //    scBnkStat = 'Inactive';
           //    scActivate = 'Activate';
           // Endif;

           visActBnk = *off;                // New Hyperlink for Activate
           visIActBnk = *off;               // New Hyperlink for Inactivate
           If BIstat = 'A';
              scBnkStat = 'Active';
              visIActBnk = *on;
           Endif;
           If BIstat = 'I';
              scBnkStat = 'Inactive';
              visActBnk = *on;
           Endif;

           If dspMode;
              visActBnk = *off;
              visIActBnk = *off;
           Endif;
           //*** End Chg ***** 08/28/23 *****************************
           scBnkRRN  = dbrrn5;

           Write sflBank;
        Reade (scCo#:scFnd) wmbil;
        Enddo;

        rrnBank = 1;
        If svrrnBank <> 0;
           rrnBank = svrrnBank;
           scBnRowSel = svrrnBank;
           svrrnBank = 0;
        Endif;

      /end-free
     p LoadBankGrid    e

       //===================================================================
       // Load Group Carrier Info
       //===================================================================
     p LoadCarInfo     b
     d LoadCarInfo     pi

      /free

       Chain(n) (inCo#:inFnd) pgc00;
       If %found(wmgcl);
          scDcar = GCdcar;
          scPsym = GCpsym;
          scCcod = GCccod;
          scPgid = GCpgid;
          scCfmt = GCcfmt;
          scFrcn = GCfrcn;
          scTocn = GCtocn;
          scNacn = GCnacn;
          scCrfl = GCcrfl;
          scAcfl = GCacfl;
          scUcfl = GCucfl;
          scDcfl = GCdcfl;

          saveDT3 = GCdt;
          saveTM3 = GCtime;
       Endif;

      /end-free
     p LoadCarInfo     e

       //===================================================================
       // Load Chapter Info Grid
       //===================================================================
     p LoadChpGrid     b
     d LoadChpGrid     pi

      /free
        // Clear the subfile
        rrnChapter = 0;
        dspChapter = *off;
        clrChapter = *on;
        Write rcdAddInf;
        clrChapter = *off;
        dspChapter = *on;

        If dspMode;
           inquiry_1 = *on;
        Endif;

        // Populate the subfile
        Setll (scCo#:scFnd) wmcpl;
        Reade (scCo#:scFnd) wmcpl;
        Dow not %eof(wmcpl);
           If @@next = 'CHAPTER';
              visbChpSel = *on;                         //Show Select Button on Search screen
              visbChpPnl = *on;                         //Hide Edit and Activate link for Inquiry
              //*** Begin Add ***** 08/28/23 *****************************
              visChpSts = *off;                         //Hide Activate link for Inquiry
              visIChpSts = *off;                        //Hide Inactivate link for Inquiry
              //*** End   Add ***** 08/28/23 *****************************
           Endif;
           If dspMode and @@next <> 'CHAPTER';
              visbChpPnl = *on;                         //Hide Edit and Activate link for Inquiry
              //*** Begin Add ***** 08/28/23 *****************************
              visChpSts = *off;                         //Hide Activate link for Inquiry
              visIChpSts = *off;                        //Hide Inactivate link for Inquiry
              //*** End   Add ***** 08/28/23 *****************************
           Endif;

           rrnChapter += 1;
           scChpSel = *off;

           If svrrnChapter = *zeros;
              If rrnChapter = 1;
                 scChpSel = *on;
              Endif;
           Else;
              If rrnChapter = svrrnChapter;
                 scChpSel = *on;
              Endif;
           Endif;

           scChp# = CPchap;
           scChpDesc = CPdesc;
           scChpStat = *Blanks;

           //*** Begin Chg ***** 08/28/23 *****************************
           // scActivate = *Blanks;
           // If CPstat = 'A';
           //    scChpStat = 'Active';
           //    scActivate = 'Inactivate';
           // Endif;
           // If CPstat = 'I';
           //    scChpStat = 'Inactive';
           //    scActivate = 'Activate';
           // Endif;

           visChpSts = *off;                // New Hyperlink for Activate
           visIChpSts = *off;               // New Hyperlink for Inactivate
           If CPstat = 'A';
              scChpStat = 'Active';
              visIChpSts = *on;
           Endif;
           If CPstat = 'I';
              scChpStat = 'Inactive';
              visChpSts = *on;
           Endif;

           If dspMode or @@next = 'CHAPTER';
              visChpSts = *off;
              visIChpSts = *off;
           Endif;
           //*** End Chg ***** 08/28/23 *****************************
           scChpRRN  = dbrrn4;

           Write sflChapter;
        Reade (scCo#:scFnd) wmcpl;
        Enddo;

        rrnChapter = 1;
        If svrrnChapter <> 0;
           rrnChapter = svrrnChapter;
           svrrnChapter = 0;
        Endif;

      /end-free
     p LoadChpGrid     e

       //===================================================================
       // Load Flexible Heading Info
       //===================================================================
     p LoadFlxInfo     b
     d LoadFlxInfo     pi

      /free

       Chain(n) (inCo#:inFnd) pfh00;
       If %found(wdfhl);
          scFh#1 = FHfh#1;
          scFh1q = FHfh1q;
          scFh1w = FHfh1w;
          scF1tb = FHf1tb;
          scF1df = FHf1df;
          scFh#2 = FHfh#2;
          scFh2q = FHfh2q;
          scFh2w = FHfh2w;
          scF2tb = FHf2tb;
          scF2df = FHf2df;
          scFh#3 = FHfh#3;
          scFh3q = FHfh3q;
          scFh3w = FHfh3w;
          scF3tb = FHf3tb;
          scF3df = FHf3df;
          scFh#4 = FHfh#4;
          scFh4q = FHfh4q;
          scFh4w = FHfh4w;
          scFh#5 = FHfh#5;
          scFh5q = FHfh5q;
          scFh5w = FHfh5w;
          scFh#6 = FHfh#6;
          scFh6q = FHfh6q;
          scFh6w = FHfh6w;
          scF6tb = FHf6tb;
          scF6df = FHf6df;

          saveDT4 = FHdt;
          saveTM4 = FHtime;

      // Set selection criteria for Default Value 1
          scF1dfCH = 'GTco# = ' + %EditC(scCo#:'X') + ' and ' +
           'GTtbid = ''' + scF1tb + '''' + ' and ' +
           'GTstat <> ' + '''I''';

      // Set selection criteria for Default Value 2
          scF2dfCH = 'GTco# = ' + %EditC(scCo#:'X') + ' and ' +
           'GTtbid = ''' + scF2tb + '''' + ' and ' +
           'GTstat <> ' + '''I''';

      // Set selection criteria for Default Value 3
          scF3dfCH = 'GTco# = ' + %EditC(scCo#:'X') + ' and ' +
           'GTtbid = ''' + scF3tb + '''' + ' and ' +
           'GTstat <> ' + '''I''';

      // Set selection criteria for Default Value 6
          scF6dfCH = 'GTco# = ' + %EditC(scCo#:'X') + ' and ' +
           'GTtbid = ''' + scF6tb + '''' + ' and ' +
           'GTstat <> ' + '''I''';

       Endif;

      /end-free
     p LoadFlxInfo     e

       //===================================================================
       // Load Group Master info
       //===================================================================
     p LoadMstInfo     b
     d LoadMstInfo     pi

      /free

       Chain(n) (inCo#:inFnd) pfn00;
       If %found(wmfnl);
          scCo#   = FNco#;                 // Company Number
          scFnd   = FNfnd;                 // Group Number
          scName  = FNname;                // Name
          scShnm  = FNshnm;                // Short Name
          scMad1  = FNmad1;                // Mailing Addres 1
          scMad2  = FNmad2;                // Mailing Addres 2
          scMcty  = FNmcty;                // Mailing City
          scMst   = FNmst;                 // Mailing State
          scMzip  = FNmzip;                // Mailing Zipcode
          scPad1  = FNpad1;                // Physical Addres 1
          scPad2  = FNpad2;                // Physical Addres 2
          scPcty  = FNpcty;                // Physical City
          scPst   = FNpst;                 // Physical State
          scPzip  = FNpzip;                // Physical Zipcode
          scAnam  = FNanam;                // Alternate Company Name
          scAad1  = FNaad1;                // Alternate Addres 1
          scAad2  = FNaad2;                // Alternate Addres 2
          scActy  = FNacty;                // Alternate City
          scAst   = FNast;                 // Alternate State
          scAzip  = FNazip;                // Alternate Zipcode

          If FNph1a <> *zeros or FNph1 <> *zeros;
             scPhone1 = %int(%editc(FNph1a:'X') + %editc(FNph1:'X'));
          Endif;
          If FNarea <> *zeros or FNph2 <> *zeros;
             scPhone2 = %int(%editc(FNarea:'X') + %editc(FNph2:'X'));
          Endif;
          If FNfaxa <> *zeros or FNfax <> *zeros;
             scFax    = %int(%editc(FNfaxa:'X') + %editc(FNfax:'X'));
          Endif;

          scIadr  = FNiadr;                // Internet Address
          scefDta = dtMDYY(FNEfdt);        // Effective date
          scexDta = dtMDYY(FNExdt);        // Expiration date
          scMxex  = FNmxex;                // Max extended year policy (in months)
          scNacs  = FNnacs;                // Next Available Case Number
          scNaen  = FNnaen;                // Next Available Employer Number
          scNaqn  = FNnaqn;                // Next Available Quote Number
          scAute  = FNaute;                // Auto Assign Employer Number
          scAutd  = FNautd;                // Auto Assign Division Number
          scDiv0  = FNdiv0;                // Allow Division Zero

          scMetr  = FNmetr;                // Net Deductible Medical Transaction Code
          scMep1  = FNmep1;                // Net Deductible Medical PV1
          scExtr  = FNextr;                // Net Deductible Expense Transaction Code
          scExp1  = FNexp1;                // Net Deductible Expense PV1
          scCotr  = FNcotr;                // Net Deductible Compensation Transaction Code
          scCop1  = FNcop1;                // Net Deductible Compensation PV1
          scLetr  = FNletr;                // Net Deductible Legal Transaction Code
          scLep1  = FNlep1;                // Net Deductible Legal PV1
          scDfrc  = FNdfrc;                // Medical Default Reserve Code
          scDfra  = FNdfra;                // Medical Default Amount
          scDfxc  = FNdfxc;                // Expense Default Reserve Code
          scDfxa  = FNdfxa;                // Expense Default Amount
          scPortdft = FNportDft;           // Default Group for Connect
          scSfin  = FNsfin;                // Employers Self-insured
          scCicf  = FNcicf;                // Include Group for CICenter
          scEdi   = FNedi;                 // Exclude Group from EDI Processing
          scClelig = FNclElig;           // Exclude Group from Base Claims Eligibility Export
          scCgfl  = FNcgfl;                // Commercial Group
          //***** Beg Del ***** 06/18/21 ****************************
          //*** scSumDed = FNSumDed;             // Summarize A/R for Deductible Billing
          //***** End Del ***** 06/18/21 ****************************
          scTest  = FNtest;                // Test group
          scExins = FNexIns;               // Excess Insurance Group
          scPeoe  = FNpeoe;                // Charge Expense Constant to Emp #1 for PEO Group
          scPeoa  = FNpeoa;                // Calculate Stock Discount Across Entire PEO Group
          scPeof  = FNpeof;                // PEO Group - Policy Type
          scAer   = FNaer;                 // Auto Endorse Re-attach with Policy Page issue
          scPeos  = FNpeos;                // Mid-term Endorsements as Re-submissions
          scPde   = FNpde;             // Print Division and Entity/Location Numbers on Schedule
                                           // of Operations
         //*** Beg Add *** 05/08/24 ****************************
          scAsof  = FNasof;                // PEO/ASO Group
         //*** End Add *** 05/08/24 ****************************
         //*** Beg Add ***  04/29/24  *** WC2965 *************************
          scEmail = FNEmail;
         //*** End Add ***  04/29/24  *** WC2965 *************************
          scDftvend# = FNdftVend#;         // Default Vendor Number
          scSior  = FNsior;                // Self-insured Organization Type Code
          scSiau  = FNsiau;                // Self-insured Authorization Type Code
          scEdif  = FNedif;                // EDI Format Identifier
          scGrpasc = FNgrpAsc;             // Group Association
          scPrmv  = FNprmv;                // Premium Verbiage
          scDepo  = FNdepo;          // Schedule of Operations Division & Entity/Location Print
          scChk1  = FNchk1;                // Check# Range Allowable for Manual Claim Checks
          scChk2  = FNchk2;                // Check# Range Allowable for Manual Claim Checks
          scMidy  = FNmidy;                // Due Date for Monthly Interim Payments
          scWidy  = FNwidy;                // Due Date for Weekly Interim Payments
          scLost  = FNlost;                // Auto Update Quote to Lost Status, # of Days
          scAclr  = FNaclr;                // Auto-Create Entity/Location on Employer
          scAttn  = FNattn;                // Policy Notice Attention Name
          //*** Beg Add ***** 05/08/25 ******************************
          If FNaara <> *zeros and FNaphn <> *zeros;   // Alternate Company Phone #
             scACPhn# = %int(%editc(Fnaara:'X') + %editc(FNaphn:'X'));
          Endif;

          scACEmlAd = FNamail;             // Alternate Company Email Address
          //*** End Add ***** 05/08/25 ******************************

          // Valid Claim Reserve Types
          scRs01  = FNrs01;
          scRs02  = FNrs02;
          scRs03  = FNrs03;
          scRs04  = FNrs04;
          scRs05  = FNrs05;
          scRs06  = FNrs06;
          scRs07  = FNrs07;
          scRs08  = FNrs08;
          scRs09  = FNrs09;
          scRs10  = FNrs10;

          saveDT  = FNdt;
          saveTM  = FNtime;
       Endif;

     // Web Interface - Include Group for Connect
       Reset dsSMCIP;
       Exec sql
        select * into :dsSMCIP
         from SMCIP
         where CICO# = :FNCO#;

       Reset dsWMFIP;
       Exec sql
        select * into :dsWMFIP
         from WMFIP
         where FICO# = :FNCO# and FIFND = :FNFND;
       dsWMFIPsave = dsWMFIP;

       tpCmpDft = *blanks;
       tpCmpDftC = *blanks;
       vsCmpOvr = *off;
       isCmpDft = *off;

       tpCmpDftC = 'Clear both fields to reset to company information.';
       tpCmpDft = tpCmpDftC;
       If dsWMFIP.FIasOf = *zero and dsWMFIP.FIdays = *zero;
          tpCmpDft = 'Data shown is defaulted from company information.';
          tpCmpDft = %trim(tpCmpDft) + ' ' +
           'Click ''Override Company Defaults'' to set values for ' +
           'this group.';
          vsCmpOvr = *on;
          isCmpDft = *on;
          dsWMFIP.FIasOf = dsSMCIP.CIasOf;
          dsWMFIP.FIdays = dsSMCIP.CIdays;
       Endif;

       If isPortal = *off;
          vsCmpOvr = *off;   // Hide link when Connect Portal Block is Hidden
       Endif;
       //*** Begin Add *** 07/22/25 ***************************************
       If dspMode;
          vsCmpOvr = *off;   // Hide link in Inquiry Mode
       Endif;
       //*** End   Add *** 07/22/25 ***************************************

       scQuote  = dsWMFIP.FIquote;
       //*** Begin Add *** 11/04/21 ***************************************
       //*** Begin Del *** 09/09/25 ***************************************
       //*** scExcCmprt = dsWMFIP.FIexcCmpRt;
       //*** End   Del *** 09/09/25 ***************************************
       //*** End   Add *** 11/04/21 ***************************************
       scPolicy = dsWMFIP.FIpolicy;
       scClaim  = dsWMFIP.FIclaim;
       scSR     = dsWMFIP.FIsr;
       scFROI   = dsWMFIP.FIfroi;

       scAsOfP  = DtMDYY(dsWMFIP.FIasOf);
       scDaysP  = dsWMFIP.FIdays;
       udAsOfp = DtMDYY(dsSMCIP.CIasOf);
       udDaysP  = dsSMCIP.CIdays;
       If isCmpDft = *on;
          Clear dsWMFIP.FIasOf;
          Clear dsWMFIP.FIdays;
       Endif;

      /end-free
     p LoadMstInfo     e

       //===================================================================
       // Load Region Info Grid
       //===================================================================
     p LoadRegionGrid  b
     d LoadRegionGrid  pi

      /free
        // Clear the subfile
        rrnRegion = 0;
        dspRegion = *off;
        clrRegion = *on;
        Write rcdAddInf;
        clrRegion = *off;
        dspRegion = *on;

        If dspMode;
           inquiry_1 = *on;
        Endif;

        // Populate the subfile
        Setll (scCo#:scFnd) wmrel;
        Reade (scCo#:scFnd) wmrel;
        Dow not %eof(wmrel);
           If @@next = 'REGION';
              visbRegSel = *on;                         //Show Select Button on Search screen
              visbRegPnl = *on;                         //Hide Edit and Activate link for Inquiry
              //*** Begin Add ***** 08/28/23 *****************************
              visRegSts = *off;                         //Hide Activate link for Inquiry
              visIRegSts = *off;                        //Hide Inactivate link for Inquiry
              //*** End   Add ***** 08/28/23 *****************************
           Endif;
           If dspMode and @@next <> 'REGION';
              visbRegPnl = *on;                         //Hide Edit and Activate link for Inquiry
              //*** Begin Add ***** 08/28/23 *****************************
              visRegSts = *off;                         //Hide Activate link for Inquiry
              visIRegSts = *off;                        //Hide Inactivate link for Inquiry
              //*** End   Add ***** 08/28/23 *****************************
           Endif;

           rrnRegion += 1;
           scRegSel = *off;

           If svrrnRegion = *zeros;
              If rrnRegion = 1;
                 scRegSel = *on;
              Endif;
           Else;
              If rrnRegion = svrrnRegion;
                 scRegSel = *on;
              Endif;
           Endif;

           scRgn# = RErgn#;
           scRegDesc = REdesc;
           scRegStat = *Blanks;

           //*** Begin Chg ***** 08/28/23 *****************************
           // scActivate = *Blanks;
           // If REstat = 'A';
           //    scRegStat = 'Active';
           //    scActivate = 'Inactivate';
           // Endif;
           // If REstat = 'I';
           //    scRegStat = 'Inactive';
           //    scActivate = 'Activate';
           // Endif;

           visRegSts = *off;                // New Hyperlink for Activate
           visIRegSts = *off;               // New Hyperlink for Inactivate
           If REstat = 'A';
              scRegStat = 'Active';
              visIRegSts = *on;
           Endif;
           If REstat = 'I';
              scRegStat = 'Inactive';
              visRegSts = *on;
           Endif;

           If dspMode or @@next = 'REGION';
              visRegSts = *off;
              visIRegSts = *off;
           Endif;
           //*** End Chg ***** 08/28/23 *****************************
           scRegRRN  = dbrrn3;

           Write sflRegion;
        Reade (scCo#:scFnd) wmrel;
        Enddo;

        rrnRegion = 1;
        If svrrnRegion <> 0;
           rrnRegion = svrrnRegion;
           svrrnRegion = 0;
        Endif;

      /end-free
     p LoadRegionGrid  e

       //===================================================================
       // Load Policy Years Info Grid
       //===================================================================
     p LoadYearsGrid   b
     d LoadYearsGrid   pi

      /free
        // Clear the subfile
        rrnPolYr = 0;
        dspPolYr = *off;
        clrPolYr = *on;
        Write rcdGrpAdd;
        clrPolYr = *off;
        dspPolYr = *on;

        If dspMode;
           inquiry = *on;
        Endif;

        // Populate the subfile in Descending order
        Setgt (scCo#:scFnd) wdf1l;
        Readpe (scCo#:scFnd) wdf1l;
        Dow not %eof(wdf1l);
           rrnPolYr += 1;
           scpolyrSel = *off;

           If svrrnPolYr = *zeros;
              If rrnPolYr = 1;
                 scPolYrSel = *on;
              Endif;
           Else;
              If rrnPolYr = svrrnPolYr;
                 scPolYrSel = *on;
              Endif;
           Endif;

           scPolYr = F1fyr;
           scPolEfdt = CvtToDate(F1stpo);
           scPolExdt = CvtToDate(F1enpo);
           scPolRRN  = dbrrn2;

           Write sflPolYr;
        Readpe (scCo#:scFnd) wdf1l;
        Enddo;

        rrnPolYr = 1;
        If svrrnPolYr <> 0;
           rrnPolYr = svrrnPolYr;
           svrrnPolYr = 0;
        Endif;

      /end-free
     p LoadYearsGrid   e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
     d*
     d mode            s              1
      /free

       //LoadActionBar();

       BtnCancel = *Off;

       // Process until exit button is pressed.
       DoW BtnCancel = *Off;

          // display screen
          Exsr ExfmtScreens;

          error = *off;
          error1 = *off;

          // Perform actions on screen.
          Select;
          // do nothing - catch tab click event
          When TabClick = *on;
             TabClick = *off;

          // Refresh
          When btnRefresh = *On;
             //***  Begin Del  ***  05/23/24  *****************
             //***  btnRefresh = *Off;
             //***  End   Del  ***  05/23/24  *****************

          // Enter key or Save button
          When btnSave = *on;
             btnSave = *off;
             If @@mode <> 'D';
                // Check if WMFNP updated since screen displayed (Group Master)
                flgRcdUpdated = RcdUpdatedErr('WMFNP':dbrrn:'FNdt':'FNtime':
                 'FNuser':saveDT:saveTM);
                if flgRcdUpdated = '0'; // No error
                   // Check if WDF2P updated since screen displayed (Auxiliary info)
                   flgRcdUpdated = RcdUpdatedErr('WDF2P':dbrrn6:'F2dt':'F2time':
                    'F2user':saveDT2:saveTM2);
                EndIf;
                if flgRcdUpdated = '0'; // No error
                   // Check if WMGCP updated since screen displayed (Carrier info)
                   flgRcdUpdated = RcdUpdatedErr('WMGCP':dbrrn7:'GCdt':
                    'GCtime':'GCuser':saveDT3:saveTM3);
                EndIf;
                if flgRcdUpdated = '0' and saveDT4 <> 0; // No error
                   // Check if WDFHP updated since screen displayed (flexible Heading)
                   flgRcdUpdated = RcdUpdatedErr('WDFHP':dbrrn8:'FHdt':
                    'FHtime':'FHuser':saveDT4:saveTM4);
                EndIf;

                if flgRcdUpdated = '0'; // No error

                   // Validate Group Master Info (Tab 1)
                   If ValidateGroup();
                      Exsr UpdGroup;
                   Else;
                      error = *on;
                   EndIf;

                   //***  Begin Add  ***  08/06/24  *****************
                   // Validate only if user has visited Tab 2
                   t3 = 0;
                   t3 = %lookup(2:ClickTab);
                   If t3 <> 0;
                   //***  End   Add  ***  08/06/24  *****************
                   // Validate Additional Info (Tab 2)
                   If ValidAddInfo();
                      Exsr UpdAddInfo;
                   Else;
                      error1 = *on;
                   EndIf;
                   //***  Begin Add  ***  08/06/24  *****************
                   Endif;
                   //***  End   Add  ***  08/06/24  *****************

                Else;
                   error = *on;
                   error1 = *on;
                EndIf;

            // Show Data Saved Sucessfully Message
                If error = *off and error1 = *off;
                   PopDataSaved();

                   //*** Beg Add ***** 05/10/23 *****************************
                   Clear ClickTab;
                   Clear t2;
                   //*** End Add ***** 05/10/23 *****************************
                EndIf;
             Endif;

          // Add Policy Years Button
          When BtnYrAdd = *On;
             BtnYrAdd =*Off;
             ProcessAddYear();

          // Add Region Button
          When BtnRegAdd = *On;
             BtnRegAdd =*Off;
             ProcessAddReg();

          // Add Chapter Button
          When BtnChpAdd = *On;
             BtnChpAdd =*Off;
             ProcessAddChp();

          // Add Bank Button
          When BtnBnkAdd = *On;
             BtnBnkAdd =*Off;
             ProcessAddBank();

          //*** Beg Add ***** 01/17/22 *******************************
          // Copy Configuration
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'GS':txtWinData);
             btnShowWin = *off;
          //*** End Add ***** 01/17/22 *******************************

          //*** Beg Add *** 05/23/23 ******************************************
          // Add User Access Window Logic
          When btnUsrAcc = *on;
             inDspFileName = 'WTGRPPRI';
             inPgmName     = 'Primary Group Information';
             inTabName     =  arrTabNames(TabIndex + 1);
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;
          //*** End Add *** 05/23/23 ******************************************

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;


          Other;
             ProcOther2();                  // Policy Years Grid
             ProcOther3();                  // Region Grid
             ProcOther4();                  // Chapter Grid
             ProcOther5();                  // Bank Grid

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;
       When arrTabNames(TabIndex+1) = 'Group Information';
          pnlHeight = 1925;
          APnlHeight = pnlHeight + 60;

          //***  Begin Add  ***  05/10/23  *****************
          t3 = 0;
          t3 = %lookup(indexTab:ClickTab);
          //***  Begin Chg  ***  05/23/24  *****************
          //***  If t3 = 0;
          //***     t2 = t2 + 1;
          //***     ClickTab(t2) = indexTab;
          If t3 = 0 or btnRefresh = *On;
             btnRefresh = *Off;
             If t3 = 0;
                t2 = t2 + 1;
                ClickTab(t2) = indexTab;
             EndIf;
          //***  End   Chg  ***  05/23/24  *****************
          //***  End   Add  ***  05/10/23  *****************

          If error = *off;
             Clear rcdGrpAdd;

            // Loading initial values
             InzGrpInfScVal();

            // Load Group Master Info
             LoadMstInfo();

            // Load Group Auxiliary Info
            //***  Beg Add  ***  01/16/24  *** 153WAD ***********************
             If clientID = '153';
                VisClt153 = *On;
             Endif;
           //***  End Add  ***  01/16/24  *** 153WAD ***********************
             LoadAuxInfo();

            // Load Group Carrier Info
             LoadCarInfo();

            //***  Begin Del  ***  05/10/23  *****************
            // Load Policy Years Grid Info
            //***  LoadYearsGrid();
            //***  End   Del  ***  05/10/23  *****************
          EndIf;

          //***  Begin Add  ***  05/10/23  *****************
          Endif;

          // Load Policy Years Grid Info
          LoadYearsGrid();
          //***  End   Add  ***  05/10/23  *****************

          If dspMode;
             inquiry = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdGrpAdd;
          Read rcdGrpPnl;
          Read rcdPanel;

       When arrTabNames(TabIndex+1) = 'Additional Information';
          pnlHeight = 755;
          APnlHeight = pnlHeight + 60;

          //***  Begin Add  ***  05/10/23  *****************
          t3 = 0;
          t3 = %lookup(indexTab:ClickTab);
          //***  Begin Chg  ***  05/23/24  *****************
          //***  If t3 = 0;
          //***     t2 = t2 + 1;
          //***     ClickTab(t2) = indexTab;
          If t3 = 0 or btnRefresh = *On;
             btnRefresh = *Off;
             If t3 = 0;
                t2 = t2 + 1;
                ClickTab(t2) = indexTab;
             EndIf;
          //***  End   Chg  ***  05/23/24  *****************
          //***  End   Add  ***  05/10/23  *****************

          If error1 = *off;
             Clear rcdAddInf;

            // Loading initial values
             InzAddInfScVal();

            // Load Flexible Heading Info
             LoadFlxInfo();

            //***  Begin Del  ***  05/10/23  *****************
            // Load Region Info
            //***  LoadRegionGrid();

            // Load Chapter Info
            //***  LoadChpGrid();
            //***  End   Del  ***  05/10/23  *****************
          EndIf;

          //***  Begin Add  ***  05/10/23  *****************
          Endif;

          // Load Region Info
          LoadRegionGrid();

          // Load Chapter Info
          LoadChpGrid();
          //***  End   Add  ***  05/10/23  *****************

          If dspMode;
             inquiry_1 = *on;
          Endif;
          //*** Beg Chg ***** 04/18/23 *****************************
          //*** TopRegion = 415;                            // Top of Region Grid
          //*** TopChapter = 630;                           // Top of Chapter Grid
          TopRegion = 130;                            // Top of Region Grid
          TopChapter = 350;                           // Top of Chapter Grid
          //*** End Chg ***** 04/18/23 *****************************
          If not dspMode;
             visbRegAdd = *on;                        // Show Add Region button
             visbChpAdd = *on;                        // Show Add Chapter button
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdAddInf;
          Read rcdGrpPnl;
          Read rcdPanel;

       When arrTabNames(TabIndex+1) = 'Financial Controls';
          pnlHeight = 755;
          APnlHeight = pnlHeight + 60;
          //***  Begin Add  ***  05/23/24  *****************
          btnRefresh = *Off;
          //***  End   Add  ***  05/23/24  *****************
          Clear rcdFinInf;

          scCo# = intCo#;

          // Loading initial values
          mode = 'D';
          InzFinInfScVal(mode);

          // Load Bank Info Grid
          LoadBankGrid();

          If dspMode;
             inquiry_2 = *on;
          Endif;

          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdFinInf;
          Read rcdGrpPnl;
          Read rcdPanel;

       EndSl;

       Endsr;


       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        FNname  = scName;                // Name
        FNshnm  = scShnm;                // Short Name
        FNmad1  = scMad1;                // Mailing Addres 1
        FNmad2  = scMad2;                // Mailing Addres 2
        FNmcty  = scMcty;                // Mailing City
        FNmst   = scMst;                 // Mailing State
        FNmzip  = scMzip;                // Mailing Zipcode
        FNpad1  = scPad1;                // Physical Addres 1
        FNpad2  = scPad2;                // Physical Addres 2
        FNpcty  = scPcty;                // Physical City
        FNpst   = scPst;                 // Physical State
        FNpzip  = scPzip;                // Physical Zipcode
        FNanam  = scAnam;                // Alternate Company Name
        FNaad1  = scAad1;                // Alternate Addres 1
        FNaad2  = scAad2;                // Alternate Addres 2
        FNacty  = scActy;                // Alternate City
        FNast   = scAst;                 // Alternate State
        FNazip  = scAzip;                // Alternate Zipcode

        If scPhone1 > *zeros;
           FNph1a  = %dec(%subst(%editc(scPhone1:'X'):1:3):3:0); // Phone 1 Area Code
           FNph1   = %dec(%subst(%editc(scPhone1:'X'):4):7:0);   // Phone 1 Number
        EndIf;
        If scPhone1 = *zeros;
           FNph1a  = *zeros;
           FNph1   = *zeros;
        EndIf;
        If scPhone2 > *zeros;
           FNarea  = %dec(%subst(%editc(scPhone2:'X'):1:3):3:0); // Phone 2 Area Code
           FNph2   = %dec(%subst(%editc(scPhone2:'X'):4):7:0);   // Phone 2 Number
        EndIf;
        If scPhone2 = *zeros;
           FNarea  = *zeros;
           FNph2   = *zeros;
        EndIf;
        If scFax > *zeros;
           FNfaxa  = %dec(%subst(%editc(scFax:'X'):1:3):3:0); // Fax Area Code
           FNfax   = %dec(%subst(%editc(scFax:'X'):4):7:0);   // Fax Number
        EndIf;
        If scFax = *zeros;
           FNfaxa  = *zeros;
           FNfax   = *zeros;
        EndIf;

        FNiadr  = scIadr;                // Internet Address
        FNefdt  = dtCYMD(scEfDta);       // Effective date
        FNexdt  = dtCYMD(scExDta);       // Effective date
        FNmxex  = scMxex;                // Max extended year policy (in months)
        FNnacs  = scNacs;                // Next Available Case Number
        FNnaen  = scNaen;                // Next Available Employer Number
        FNnaqn  = scNaqn;                // Next Available Quote Number
        FNaute  = scAute;                // Auto Assign Employer Number
        FNautd  = scAutd;                // Auto Assign Division Number
        FNdiv0  = scDiv0;                // Allow Division Zero

        FNmetr  = scMetr;                // Net Deductible Medical Transaction Code
        FNmep1  = scMep1;                // Net Deductible Medical PV1
        FNextr  = scExtr;                // Net Deductible Expense Transaction Code
        FNexp1  = scExp1;                // Net Deductible Expense PV1
        FNcotr  = scCotr;                // Net Deductible Compensation Transaction Code
        FNcop1  = scCop1;                // Net Deductible Compensation PV1
        FNletr  = scLetr;                // Net Deductible Legal Transaction Code
        FNlep1  = scLep1;                // Net Deductible Legal PV1
        FNdfrc  = scDfrc;                // Medical Default Reserve Code
        FNdfra  = scDfra;                // Medical Default Amount
        FNdfxc  = scDfxc;                // Expense Default Reserve Code
        FNdfxa  = scDfxa;                // Expense Default Amount

        //*** Begin Add *** 11/04/21 ***************************************
        If scQuote <> 'Y';
           scPortDft = ' ';
        EndIf;
        //*** End   Add *** 11/04/21 ***************************************
        FNportdft  = scPortDft;          // Default Group for Connect
        FNsfin  = scSfin;                // Employers Self-insured
        FNcicf  = scCicf;                // Include Group for CICenter
        FNedi   = scEdi;                 // Exclude Group from EDI Processing
        FNclelig   = scClElig;           // Exclude Group from Base Claims Eligibility Export
        FNcgfl  = scCgfl;                // Commercial Group
        If FNcgfl = ' ';
           FNcgfl = 'N';
        Endif;
        //***** Beg Del ***** 06/18/21 ****************************
        //*** FNSumDed= scSumDed;              // Summarize A/R for Deductible Billing
        //***** End Del ***** 06/18/21 ****************************
        FNtest  = scTest;                // Test group
        FNexins = scExIns;               // Excess Insurance Group
        FNpeoe  = scPeoe;                // Charge Expense Constant to Emp #1 for PEO Group
        FNpeoa  = scPeoa;                // Calculate Stock Discount Across Entire PEO Group
        FNpeof  = scPeof;                // PEO Group - Policy Type
        FNaer   = scAer;                 // Auto Endorse Re-attach with Policy Page issue
        FNpeos  = scPeos;                // Mid-term Endorsements as Re-submissions
        FNpde   = scPde;                 // Print Division and Entity/Location Numbers on Schedule
                                         // of Operations
        //*** Beg Add *** 05/08/24 ****************************
        FNAsof  = scAsof;                // PEO/ASO Group
        //*** End Add *** 05/08/24 ****************************
         //*** Beg Add ***  04/29/24  *** WC2965 *************************
          FNEmail = scEmail;
         //*** End Add ***  04/29/24  *** WC2965 *************************
        FNdftvend# = scDftVend#;         // Default Vendor Number
        FNsior  = scSior;                // Self-insured Organization Type Code
        FNsiau  = scSiau;                // Self-insured Authorization Type Code
        FNedif  = scEdif;                // EDI Format Identifier
        FNgrpasc = scGrpAsc;             // Group Association
        FNprmv  = scPrmv;                // Premium Verbiage
        If FNprmv = ' ';
           FNPRMV = 'P';
        Endif;
        FNdepo  = scDepo;                // Schedule of Operations Division & Entity/Location Print
        If FNdepo = ' ';                 // Order (A/D)
           FNdepo = 'A';
        Endif;
        FNchk1  = scChk1;                // Check# Range Allowable for Manual Claim Checks
        FNchk2  = scChk2;                // Check# Range Allowable for Manual Claim Checks
        FNmidy  = scMidy;                // Due Date for Monthly Interim Payments
        FNwidy  = scWidy;                // Due Date for Weekly Interim Payments
        FNlost  = scLost;                // Auto Update Quote to Lost Status, # of Days
        FNaclr  = scAclr;                // Auto-Create Entity/Location on Employer
        FNattn  = scAttn;                // Policy Notice Attention Name
        //*** Beg Add ***** 05/08/25 ******************************
        FNaara  = %dec(%subst(%editc(scACPhn#:'X'):1:3):3:0); // Area Code
        FNaphn  = %dec(%subst(%editc(scACPhn#:'X'):4):7:0);  // Phone Number
        FNamail = scACEmlAd;
        //*** End Add ***** 05/08/25 ******************************

        FNuser  = q1user;                // User
        FNpgm   = q1pgm;                 // Program
        FNtime  = Timeto6(SystemTime()); // Time
        FNdt    = Dateto7(SystemDate()); // Date Mintained

        saveDT  = FNdt;
        saveTM  = FNtime;

        FNlhf   = 1.00;                  // L&H Load F type

        // Valid Claim Reserve Types
        FNrs01  = scRs01;
        FNrs02  = scRs02;
        FNrs03  = scRs03;
        FNrs04  = scRs04;
        FNrs05  = scRs05;
        FNrs06  = scRs06;
        FNrs07  = scRs07;
        FNrs08  = scRs08;
        FNrs09  = scRs09;
        FNrs10  = scRs10;

       // Web Interface - Include Group for Connect
       // Update WMFIP file fields
        dsWMFIP.FIquote  = scQuote;
        //*** Begin Add *** 11/04/21 ***************************************
        //*** Begin Del *** 09/09/25 ***************************************
        //*** If scQuote <> 'Y';
        //***   scExcCmprt = ' ';
        //*** EndIf;
        //*** dsWMFIP.FIexcCmpRt = scExcCmprt;
        //*** End   Del *** 09/09/25 ***************************************
        //*** End   Add *** 11/04/21 ***************************************
        dsWMFIP.FIpolicy = scPolicy;
        dsWMFIP.FIclaim  = scClaim;
        dsWMFIP.FIsr     = scSR;
        dsWMFIP.FIfroi   = scFROI;
        If isCmpDft = *off;
           dsWMFIP.FIasOf = DtCYMD(scAsOfP);
           dsWMFIP.FIdays = scDaysP;
        Endif;


       // Update WDF2P file fields - Auxillary file info
        F2fein = scFein;
        F2tcc  = scTcc;
        F2iaud = scIaud;
        F2sctc = scSctc;
        If scSphone > *zeros;
           F2area  = %dec(%subst(%editc(scSphone:'X'):1:3):3:0); // Phone Area Code
           F2sph#  = %dec(%subst(%editc(scSphone:'X'):4):7:0);   // Phone Number
        EndIf;
        If scSphone = *zeros;
           F2area  = *zeros;
           F2sph#  = *zeros;
        EndIf;
        F2car# = scCar#;
        F2wcpl = scWcpl;
        F2clck = scClck;
        F2agck = scAgck;
        F2macc = scMacc;
        F2pp3c = scPp3c;
        F2ppc3 = scPpc3;
        F2fold = scFold;
        F2dcid = scDcid;

        F2user = FNuser;
        F2pgm  = FNpgm;
        F2time = FNtime;
        F2dt   = FNdt;

        saveDT2 = F2dt;
        saveTM2 = F2time;

        F2ctyp = scCtyp;
        F2tpa  = scTpa;
        //*** Beg Del ***** 04/26/23 *****************************
        //*** F2lbnm = scLbnm;
        //*** End Del ***** 04/26/23 *****************************
        F2lad1 = scLad1;
        F2lad2 = scLad2;
        F2lcty = scLcty;
        F2lst  = scLst;
        F2lzip = scLzip;
        F2blog = scBlog;
        F2ceov = scCeov;
        F2docl = scDocl;
        F2docs = scDocs;
        F2naic = scNaic;
        F2rre# = scRre#;
        F2tpac = scTpac;
        F2fysm = scFysm;
        F2spsg = scSpsg;
        F2jobq = scJobq;
        F2sbs  = scSbs;
        F2thrl = scThrl;
        F2thrh = scThrh;
        F2dbcd = scDbcd;
        F2pbcd = scPbcd;
        F2fbcd = scFbcd;
        F21acd = sc1acd;
        F22acd = sc2acd;
        F23acd = sc3acd;
        F24acd = sc4acd;
        F25acd = sc5acd;
        F26acd = sc6acd;
        F27acd = sc7acd;
        F2dissms = scDissms;
        F2slogo = scSlogo;
       //*** begin chg ***  09/25/24  *************************************
       //***** F2spath = scSpath;
        F2SURL = scSURL;
        F2SPath = scSPath;
       //*** end chg *****  09/25/24  *************************************
        F2fref = scFref;
        F2bsi# = scBsi#;
        F2ncargrp = scNCarGrp;
        F21099fein = sc1099fein;
        //*** Beg Add ***** 11/17/23 *****************************
        F2dxtr = scDxtr;
        If scDxtr = *blank;
           F2dxtr = 'N';
        Endif;

        //***  Beg Add  ***  01/16/24  *** 153WAD ***********************
        F2Rnote  = scRnote;
        F2RcxRsn = scRcxRsn;
        //***  End Add  ***  01/16/24  *** 153WAD ***********************

       // Update WMGCP file fields - Group Carrier Information
        GCdcar = scDcar;
        GCpsym = scPsym;
        GCccod = scCcod;
        GCpgid = scPgid;
        GCcfmt = scCfmt;
        GCfrcn = scFrcn;
        GCtocn = scTocn;
        GCnacn = scNacn;
        GCcrfl = scCrfl;
        GCacfl = scAcfl;
        GCucfl = scUcfl;
        GCdcfl = scDcfl;

        GCpgm  = FNpgm;
        GCuser = FNuser;
        GCdt   = FNdt;
        GCtime = FNtime;

        saveDT3 = GCdt;
        saveTM3 = GCtime;

       EndSr;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb2;
        FHfh#1  = scFh#1;                // Flx hdg 1
        FHfh1q  = scFh1q;                // Flx hdg 1 in Qte?
        FHfh1w  = scFh1w;                // Flx hdg 1 in Wrtn?
        FHf1tb  = scF1tb;                // Flx hdg 1 Table Id
        FHf1df  = scF1df;                // Flx hdg 1 Default Value
        FHfh#2  = scFh#2;                // Flx hdg 2
        FHfh2q  = scFh2q;                // Flx hdg 2 in Qte?
        FHfh2w  = scFh2w;                // Flx hdg 2 in Wrtn?
        FHf2tb  = scF2tb;                // Flx hdg 2 Table Id
        FHf2df  = scF2df;                // Flx hdg 2 Default Value
        FHfh#3  = scFh#3;                // Flx hdg 3
        FHfh3q  = scFh3q;                // Flx hdg 3 in Qte?
        FHfh3w  = scFh3w;                // Flx hdg 3 in Wrtn?
        FHf3tb  = scF3tb;                // Flx hdg 3 Table Id
        FHf3df  = scF3df;                // Flx hdg 3 Default Value
        FHfh#4  = scFh#4;                // Flx hdg 4
        FHfh4q  = scFh4q;                // Flx hdg 4 in Qte?
        FHfh4w  = scFh4w;                // Flx hdg 4 in Wrtn?
        FHfh#5  = scFh#5;                // Flx hdg 5
        FHfh5q  = scFh5q;                // Flx hdg 5 in Qte?
        FHfh5w  = scFh5w;                // Flx hdg 5 in Wrtn?
        FHfh#6  = scFh#6;                // Flx hdg 6
        FHfh6q  = scFh6q;                // Flx hdg 6 in Qte?
        FHfh6w  = scFh6w;                // Flx hdg 6 in Wrtn?
        FHf6tb  = scF6tb;                // Flx hdg 6 Table Id
        FHf6df  = scF6df;                // Flx hdg 6 Default Value

       EndSr;

       // Update Additional Information subroutine
       BegSr UpdAddInfo;
        // Update Flexible Heading Info
        Chain (inCo#:inFnd) pfh00;
        Exsr MoveScDb2;
        FHuser  = q1user;                // User
        FHpgm   = q1pgm;                 // Program
        FHtime  = Timeto6(SystemTime()); // Time
        FHdt    = Dateto7(SystemDate()); // Date Mintained

        saveDT4 = FHdt;
        saveTM4 = FHtime;

        If %found(wdfhl);
           Update pfh00;
        Else;
           FHco#   = FNco#;
           FHfnd   = FNfnd;
           FHiusr  = q1user;                // Initial User
           FHipgm  = q1pgm;                 // Initial Program
           FHitim  = Timeto6(SystemTime()); // Initial Time
           FHidt   = Dateto7(SystemDate()); // Initial Date
           Write pfh00;
        EndIf;
       EndSr;

     C*===============================================================
     C* UpdateWMFIP - Update/Write records to WMFIP
     C*===============================================================
      /Free
       Begsr UpdateWMFIP;
        If dsWMFIP = dsWMFIPsave;
           Leavesr;
        Endif;
        dsWMFIP.FIuser = FNuser;
        dsWMFIP.FIpgm  = FNpgm;
        dsWMFIP.FIdt   = FNdt;
        dsWMFIP.FItime = FNtime;
        If dsWMFIP.FIfnd <> *zero;
           Exec sql
            update WMFIP
             set row = :dsWMFIP
             where FIco# = :dsWMFIP.FIco#
              and FIfnd = :dsWMFIP.FIfnd;
        Else;
           dsWMFIP.FIco#  = FNco#;
           dsWMFIP.FIfnd  = FNfnd;
           dsWMFIP.FIiUsr = q1user;
           dsWMFIP.FIiPgm = q1pgm;
           //*** Begin Chg *** 08/21/25 ***************************************
           //*** dsWMFIP.FIiDt  = Timeto6(SystemTime());
           //*** dsWMFIP.FIiTim = Dateto7(SystemDate());
           dsWMFIP.FIiDt  = Dateto7(SystemDate());
           dsWMFIP.FIiTim = Timeto6(SystemTime());
           //*** End   Chg *** 08/21/25 ***************************************
           Exec sql
            insert into WMFIP
             values(:dsWMFIP);
        Endif;
       Endsr;

       // Update Group Info subroutine
       BegSr UpdGroup;
        Chain (inCo#:inFnd) pfn00;
        Chain (inCo#:inFnd) pf200;
        Chain (inCo#:inFnd) pgc00;
        Exsr MoveScDb;
        Update pfn00;
        // Update WMFIP records
        Exsr UpdateWMFIP;
        // Update Group Auxillary file info
        If %found(wdf2l);
           Update pf200;
        Else;
           F2co#  = FNco#;
           F2fnd  = FNfnd;
           F2iusr = FNuser;
           F2ipgm = FNpgm;
           F2itim = FNtime;
           F2idt  = FNdt;
           Write pf200;
        EndIf;
        // Update Group Carrier Information file
        If %found(wmgcl);
           Update pgc00;
        Else;
           GCco#  = FNco#;
           GCfnd  = FNfnd;
           GCiusr = FNuser;
           GCipgm = FNpgm;
           GCidt  = FNdt;
           Write pgc00;
        EndIf;
        //*** Beg Add ***** 05/22/23 *****************************
        // Check if Managed Care Group Record Exists
        If ##MCIF = 'Y';
           @@rtcd = ' ';
           ##co#  = %editc(FNco#:'X');
           ##wcf# = %editc(FNfnd:'X');
           ##sub# = *zeros;
           ##fnd  = *zeros;
           FA700R(@@rtcd:##co#:##wcf#:##sub#:##fnd);
           If @@rtcd = 'F' or ##sub# = *zeros or ##fnd = *zeros;
              // WCMC Add Group Pompt
              @@kc = '0';
              @@kl = '0';
              @@fnd = FNfnd;
              F0062R(@@kc:@@kl:@@fnd);
           Else;
              // Add Managed Care Group
              @@sub# = %dec(##sub#:5:0);
              @@fnd = FNfnd;
              @@rrn1 = 0;
              FA027R(@@co#:@@sub#:@@grp:@@fnd:@@rrn1);
           EndIf;
        EndIf;
        //*** End Add ***** 05/22/23 *****************************
       EndSr;


      /end-free
     p Main            e

       //===================================================================
       // processAddBank - process addition of Bank
       //===================================================================
     P ProcessAddBank  B
     D ProcessAddBank  PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear RcdBnkDtl;
       mode = 'A';
       isexit = *off;
       BtnBnkCan = *off;

       Dow BtnBnkCan = *off and isexit = *off;

          // Loading initial values
          InzFinInfScVal(mode);

          scBnkHdg = 'Add Bank Informaion';
          scBnkStAE = 'A';                            // Bank Status
          scBnkFnd = scCo#;

          // Load Mailing Address from Fund Master file for Add mode
          sc40a1 = %Trim(FNname);
          sc40a2 = %Trim(FNmad1);
          sc40a3 = %Trim(FNmad2);
          sc40a4 = %Trim(FNmcty) + ', ' + FNmst + ' ' +
           %Subst(%EditC(FNmzip:'X'):1:5);
          If %Subst(%EditC(FNmzip:'X'):6:4) <> *Blanks;
             sc40a4 = %Trim(sc40a4) + '-' +
              %Subst(%EditC(FNmzip:'X'):6:4);
          EndIf;
          If sc40a3 = *Blanks;
             sc40a3 = sc40a4;
             sc40a4 = *Blanks;
          EndIf;


          Exfmt RcdBnkDtl;

          Select;
          // save Button
          When BtnBnkSave = *on;
             BtnBnkSave = *off;
             If ValidateBnkDtl(mode);
                Exsr WrtBnkDtl;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnBnkCan = *on;
               BtnBnkCan = *off;
               isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        BIco#   = scCo#;                 // Company Number
        BIfnd   = scFnd;                 // Group Number
        BIstat  = scBnkStAE;             // Status
        BIacct  = scAcct;

       // Move Account Name and Address from from Bank Master File WSCKL
        Chain (inCo#:scAcct) pck00;
        If %found(wsckl);
           BIai01  = CKai01;
           BIai02  = CKai02;
           BIai03  = CKai03;
       // Move Other info as well
           BIrtup  = CKrtup;
           BIrtlw  = CKrtlw;
           BItrns  = CKtrns;
           BIbnk#  = CKbnk#;
           BIckdg  = %char(CKckdg);
           BIaxou  = CKaxou;
           BIonus  = CKonus;
        EndIf;
        BIappl  = scAppl;
        BIcktp  = scCktp;
        BIefdt  = dtCYMD(scBkEfDt);
        BIftyp  = scFtyp;
        BI$lmt  = sc$lmt;
        BIccid  = %trim(scCcid);
        BIdcid  = %trim(scBidcid);
        BI50n3  = sc50n3;
        BImacr  = scMacr;
        BIoutq  = scOutq;
        BIcpyq  = scCpyq;
        BIckpv  = scCkpv;
        BItxtOnCk  = %subst(scTxtOnCk:1:20);
        BItxtOnCk2 = %subst(scTxtOnCk:21:20);
        BIhdcy  = scHdcy;
        BItrst  = scTrst;
        BI1A2   = sc1A2;
        BIlogo  = scLogo;
        BI40A1  = sc40A1;
        BI40A2  = sc40A2;
        BI40A3  = sc40A3;
        BI40A4  = sc40A4;
        BInam1  = scNam1;                // Signature 1
        BInam2  = scNam2;                // Signature 2
        If scNam1 = *Blanks;
           BI10a1  = *Blanks;
           BIcmd1  = *Blanks;
        EndIf;
        If scNam2 = *Blanks;
           BI10a2  = *Blanks;
           BIcmd2  = *Blanks;
        EndIf;
        // Load Signature Font and Command from WMSGP
        If scNam1 <> *Blanks;
           Chain(n) (scCo#:scNam1) psg00;
           If %found(wmsgl1);
              BI10a1  = SGfont;          // Signature 1 Font/String
              BIcmd1  = SGcmd;           // Signature 1 PCL5 Command
           EndIf;
        EndIf;
        If scNam2 <> *Blanks;
           Chain(n) (scCo#:scNam2) psg00;
           If %found(wmsgl1);
              BI10a2  = SGfont;          // Signature 2 Font/String
              BIcmd2  = SGcmd;           // Signature 2 PCL5 Command
           EndIf;
        EndIf;

        BIuser = q1user;
        BIpgm  = q1pgm;
        BItime = Timeto6(SystemTime());
        BIdt   = Dateto7(SystemDate());

        // Load E-Pay Info
        If scEpVend <> ' ';
           B2co#   = scCo#;
           B2fnd   = scFnd;
           B2appl  = scAppl;
           B2acct  = scAcct;
           B2efdt  = dtCYMD(scBkEfDt);
           B2ckTP  = BIckTp;          // Bank Account Code

           B2ePvend   = scEpVend;
           B2vPfacTbl = scVpFACTbl;
           B2vPfacFf  = scVpFACff;
           B2ePclmInd = scEpClmInd;
           B2ePclmMed = scEpClmMed;
           B2ePclmExp = scEpClmExp;
           B2ePclmLeg = scEpClmLeg;
           B2ckTp     = BIckTp;
           B2user = q1user;
           B2pgm  = q1pgm;
           B2time = Timeto6(SystemTime());
           B2dt   = Dateto7(SystemDate());
           B2iUsr = B2user;
           B2iPgm = B2pgm;
           B2iDt  = B2dt;
           B2iTim = B2time;
           Write pb200;
        Endif;
       EndSr;

       // Writing Bank Info subroutine
       BegSr WrtBnkDtl;
        Clear pbi0p;
        Exsr MoveScDb;
        Write pbi0p;
       EndSr;

      /end-free
     P ProcessAddBank  E

       //===================================================================
       // processAddChp - process addition of Chapter
       //===================================================================
     P ProcessAddChp   B
     D ProcessAddChp   PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear RcdChpDtl;
       mode = 'A';
       isexit = *off;
       BtnChpCan = *off;

       Dow BtnChpCan = *off and isexit = *off;

          scChpHdg = 'Add Chapter Informaion';
          scChpStAE = 'A';
          visChpSvAd = *On;                           // Show Save/Add next Button

          Exfmt RcdChpDtl;

          Select;
          // save/Add Next Button
          When BtnChpSvAd = *on;
             BtnChpSvAd = *off;
             If ValidateChpDtl(mode);
                Exsr WrtChpDtl;
             EndIf;
             scChpAE = 0;
             scChpDesAE = *Blanks;

          // save Button
          When BtnChpSave = *on;
             BtnChpSave = *off;
             If ValidateChpDtl(mode);
                Exsr WrtChpDtl;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnChpCan = *on;
               BtnChpCan = *off;
               isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        CPco#   = scCo#;                 // Company Number
        CPfnd   = scFnd;                 // Group Number
        CPchap  = scChpAE;               // Chapter Number
        CPdesc  = scChpDesAE;            // Chapter Description
        CPstat  = scChpStAE;             // Status

        CPuser = q1user;
        CPpgm  = q1pgm;
        CPtime = Timeto6(SystemTime());
        CPdt   = Dateto7(SystemDate());
       EndSr;

       // Writing Chapter Details subroutine
       BegSr WrtChpDtl;
        Clear pcp0p;
        Exsr MoveScDb;
        Write pcp0p;
       EndSr;

      /end-free
     P ProcessAddChp   E

       //===================================================================
       // processAddReg - process addition of Region
       //===================================================================
     P ProcessAddReg   B
     D ProcessAddReg   PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear RcdRegDtl;
       mode = 'A';
       isexit = *off;
       BtnRegCan = *off;

       Dow BtnRegCan = *off and isexit = *off;

          scRegHdg = 'Add Region Informaion';
          scRegStAE = 'A';
          visRegSvAd = *On;                           // Show Save/Add next Button

          Exfmt RcdRegDtl;

          Select;
          // save/Add Next Button
          When BtnRegSvAd = *on;
             BtnRegSvAd = *off;
             If ValidateRegDtl(mode);
                Exsr WrtRegDtl;
             EndIf;
             scRegAE = 0;
             scRegDesAE = *Blanks;

          // save Button
          When BtnRegSave = *on;
             BtnRegSave = *off;
             If ValidateRegDtl(mode);
                Exsr WrtRegDtl;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnRegCan = *on;
               BtnRegCan = *off;
               isexit = *on;

          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        REco#   = scCo#;                 // Company Number
        REfnd   = scFnd;                 // Group Number
        RErgn#  = scRegAE;               // Region Number
        REdesc  = scRegDesAE;            // Region Description
        REstat  = scRegStAE;             // Status

        REuser = q1user;
        REpgm  = q1pgm;
        REtime = Timeto6(SystemTime());
        REdt   = Dateto7(SystemDate());
        REiUsr = q1user;
        REiPgm = q1pgm;
        REiTm  = Timeto6(SystemTime());
        REiDt  = Dateto7(SystemDate());
       EndSr;

       // Writing Region Details subroutine
       BegSr WrtRegDtl;
        Clear pre0p;
        Exsr MoveScDb;
        Write pre0p;
       EndSr;

      /end-free
     P ProcessAddReg   E


       //===================================================================
       // processAddYear - process addition of policy year/Group Detail
       //===================================================================
     P ProcessAddYear  B
     D ProcessAddYear  PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear RcdGrpDtl;
       mode = 'A';
       isexit = *off;
       reload = *on;
       BtnDtlCan = *off;
       scGrpHdg = 'Add Group Detail Information';

       Dow BtnDtlCan = *off and isexit = *off;

          // Loading initial values
          InzGrpDtlScVal();

          // Loading Default values from Prior years
          If reload = *on;
             Exsr MoveDbSc;
             reload = *off;
          Endif;

          // Load A/R Codes subfiles
          Exsr LoadARsfl;

          Exfmt RcdGrpDtl;

          Select;
          // save/Add Next Button
          When BtnDtlSvAd = *on;
             BtnDtlSvAd = *off;
             If ValidateGrpDtl(mode);
                reload = *on;
                Exsr WrtGrpDtl;
             EndIf;

          // save Button
          When BtnDtlSave = *on;
             BtnDtlSave = *off;
             If ValidateGrpDtl(mode);
                Exsr WrtGrpDtl;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnDtlCan = *on;
               BtnDtlCan = *off;
               isexit = *on;

          EndSl;
       Enddo;

       // Load A/R Codes subfile
       Begsr LoadARsfl;

        // Clear the subfile
        rrnSpecAR = 0;
        dspSpecAR = *off;
        clrSpecAR = *on;
        rrnDepPrm = 0;
        dspDepPrm = *off;
        clrDepPrm = *on;
        rrnFlatCx = 0;
        dspFlatCx = *off;
        clrFlatCx = *on;
        rrnCrAuBl = 0;
        dspCrAuBl = *off;
        clrCrAuBl = *on;
        rrnDepoSR = 0;
        dspDepoSR = *off;
        clrDepoSR = *on;
        rrnCollAR = 0;
        dspCollAR = *off;
        clrCollAR = *on;
        rrnDivPln = 0;
        dspDivPln = *off;
        clrDivPln = *on;

        Write rcdGrpDtl;

        clrSpecAR = *off;
        dspSpecAR = *on;
        clrDepPrm = *off;
        dspDepPrm = *on;
        clrFlatCx = *off;
        dspFlatCx = *on;
        clrCrAuBl = *off;
        dspCrAuBl = *on;
        clrDepoSR = *off;
        dspDepoSR = *on;
        clrCollAR = *off;
        dspCollAR = *on;
        clrDivPln = *off;
        dspDivPln = *on;

        If dspMode;
           inquiry_0 = *on;
        Endif;

        // Populate the subfile

        // Grid 1 -- Specific A/R Transaction Codes
        i=1;
        Dow i <= 18;
           rrnSpecAR += 1;
           scHdSpecAR = strHeads1(i);
           scVaSpecAR = strARcod1(i);
           scSpecARCH = strSelCr1(i);

           Write sflSpecAR;
           i +=1;
        Enddo;
        rrnSpecAR = 1;

        // Grid 2 -- Deposit to Premium Transfer
        i=1;
        Dow i <= 8;
           rrnDepPrm += 1;
           scHdDepPrm = strHeads2(i);
           scVaDepPrm = strARcod2(i);
           scDepPrmCH = strSelCr2(i);

           Write sflDepPrm;
           i +=1;
        Enddo;
        rrnDepPrm = 1;

        // Grid 3 -- A/R Transfer Codes for Flat Cancellation
        i=1;
        Dow i <= 11;
           rrnFlatCx += 1;
           scHdFlatCx = strHeads3(i);
           scVaFlatCx = strARcod3(i);
           scFlatCxCH = strSelCr3(i);

           Write sflFlatCx;
           i +=1;
        Enddo;
        rrnFlatCx = 1;

        // Grid 4 -- A/R Transfer Codes for Credit Audit Balance
        i=1;
        Dow i <= 17;
           rrnCrAuBl += 1;
           scHdCrAuBl = strHeads4(i);
           scVaCrAuBl = strARcod4(i);
           scCrAuBlCH = strSelCr4(i);

           Write sflCrAuBl;
           i +=1;
        Enddo;
        rrnCrAuBl = 1;

        // Grid 5 -- S/R Deposit Transfer A/R Codes (Mid-Year)
        i=1;
        Dow i <= 8;
           rrnDepoSR += 1;
           scHdDepoSR = strHeads5(i);
           scVaDepoSR = strARcod5(i);
           scDepoSRCH = strSelCr5(i);

           Write sflDepoSR;
           i +=1;
        Enddo;
        rrnDepoSR = 1;

        // Grid 6 -- Collateral A/R Codes
        i=1;
        Dow i <= 3;
           rrnCollAR += 1;
           scHdCollAR = strHeads6(i);
           scVaCollAR = strARcod6(i);
           scCollARCH = strSelCr6(i);

           Write sflCollAR;
           i +=1;
        Enddo;
        rrnCollAR = 1;

        // Grid 7 -- Dividend Plan
        i=1;
        Dow i <= 3;
           rrnDivPln += 1;
           scHdDivPln = strHeads7(i);
           scVaDivPln = strARcod7(i);
           scDivPlnCH = strSelCr7(i);

           Write sflDivPln;
           i +=1;
        Enddo;
        rrnDivPln = 1;
       EndSr;

       // Subrotine to move Database fields into Screen fields.
       BegSr MoveDbSc;

        scfyr = 999;
        setgt keyWDF1L pf100;
        Readpe (scCo#:scFnd) pf100;
        scfyr = 0;
        If Not %Eof(wdf1l);
           scfyr   = F1fyr + 1;
           scStrDt = DtMDYY(F1stpo) + 1;
           scEndDt = DtMDYY(F1enpo) + 1;
           scDps   = F1dps;
           scMfee  = F1mfee;
           scEmrt  = F1emrt;
           scDadj  = F1dadj;
           scCadj  = F1cadj;
           scNetd  = scStrDt;
           scScdt  = DtMDYY(F1scdt);
           scManp  = F1manp;

          // Flags
           scUpar = F1upar;
           scFaud = F1faud;
           scSpla = F1spla;
           scRlxc = F1rlxc;
           scClms = F1clms;
           scExdv = F1exdv;
           scPmfl = F1pmfl;
           scYrad = F1yrad;
           scCxad = F1cxad;
           scApfd = F1apfd;
           scBrfl = F1brfl;
           scBvfl = F1bvfl;
           scAced = F1aced;
           scNetr = F1netr;
           //*** Begin Add *** 01/07/22 ************************************
           If F1netSR = ' ';
              scNetSR = 'N';
           Else;
              scNetSR = F1netSR;
           EndIf;
           //*** End   Add *** 01/07/22 ************************************
           scAafl = F1aafl;
           scDpym = F1dpym;
           scApfl = F1apfl;
           scBlfl = F1blfl;
           scNy07 = F1ny07;
           scIdbd = F1idbd;
           //*** Begin Add *** 05/14/24 ************************************
           scSRAs = F1SRAs;
           //*** End   Add *** 05/14/24 ************************************

         // Set A/R Codes fields
           strARcod1(1)  = F1srbb;
           strARcod1(2)  = F1nbpt;
           strARcod1(3)  = F1nbtr;
           strARcod1(4)  = F1dscd;
           strARcod1(5)  = F1mfpt;
           strARcod1(6)  = F1sftx;
           strARcod1(7)  = F1prtr;
           strARcod1(8)  = F1nbdt;
           strARcod1(9)  = F1cdet;
           strARcod1(10) = F11stt;
           strARcod1(11) = F12ndt;
           strARcod1(12) = F13rdt;
           strARcod1(13) = F14tht;
           strARcod1(14) = F15tht;
           strARcod1(15) = F16tht;
           strARcod1(16) = F17tht;
           strARcod1(17) = F1txtx;
           strARcod1(18) = F1txpy;

           strARcod2(1)  = F1dbtr;
           strARcod2(2)  = F1tftr;
           strARcod2(3)  = F1dptr;
           strARcod2(4)  = F1tqtr;
           strARcod2(5)  = F1pptr;
           strARcod2(6)  = F1tptr;
           strARcod2(7)  = F1dtxb;
           strARcod2(8)  = F1dtxp;

           strARcod3(1)  = F1mftr;
           strARcod3(2)  = F1satr;
           strARcod3(3)  = F1Txtr;
           strARcod3(4)  = F1fctr;
           strARcod3(5)  = F11atr;
           strARcod3(6)  = F12atr;
           strARcod3(7)  = F13atr;
           strARcod3(8)  = F14atr;
           strARcod3(9)  = F15atr;
           strARcod3(10) = F16atr;
           strARcod3(11) = F17atr;

           strARcod4(1)  = F1axdd;
           strARcod4(2)  = F1axec;
           strARcod4(3)  = F1axgf;
           strARcod4(4)  = F1axlf;
           strARcod4(5)  = F1axmf;
           strARcod4(6)  = F1axdp;
           strARcod4(7)  = F1axsf;
           strARcod4(8)  = F1axtx;
           strARcod4(9)  = F1axtd;
           strARcod4(10) = F1ax1t;
           strARcod4(11) = F1ax2t;
           strARcod4(12) = F1ax3t;
           strARcod4(13) = F1ax4t;
           strARcod4(14) = F1ax5t;
           strARcod4(15) = F1ax6t;
           strARcod4(16) = F1ax7t;
           strARcod4(17) = F1axpt;

           strARcod5(1)  = F1shtr;
           strARcod5(2)  = F1sftr;
           strARcod5(3)  = F1sitr;
           strARcod5(4)  = F1sqtr;
           strARcod5(5)  = F1sptr;
           strARcod5(6)  = F1srtr;
           strARcod5(7)  = F1spof;
           strARcod5(8)  = F1srpo;

           strARcod6(1)  = F1colb;
           strARcod6(2)  = F1colp;
           strARcod6(3)  = F1colt;

           strARcod7(1)  = F1dvbl;
           strARcod7(2)  = F1dvpd;
           strARcod7(3)  = F1dvxr;

        EndIf;
       EndSr;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        F1co#   = scCo#;                 // Company Number
        F1fnd   = scFnd;                 // Group Number
        F1fyr   = scfyr;                 // Policy Year
        F1stpo  = dtCYMD(scStrDt);       // Policy Start date
        F1enpo  = dtCYMD(scEndDt);       // Policy End date
        F1dps   = scDps;                 // Deposit Style
        F1mfee  = scMfee;                // Membership Fee Amount
        F1emrt  = scEmrt;                // Employer Specific Retention
        F1dadj  = scDadj;                // Threshold Amount for Debit Adjustment
        F1cadj  = scCadj;                // Threshold Amount for Credit Adjustment
        F1netd  = dtCYMD(scNetd);        // Starting with Policies Effective
        F1scdt  = dtCYMD(scScdt);        // Stop Collateral Date
        F1manp  = scManp;                // % of Manual Premium

        //F1stat = 'A';
        F1user = q1user;
        F1pgm  = q1pgm;
        F1time = Timeto6(SystemTime());
        F1dt   = Dateto7(SystemDate());
        F1iUsr = q1user;
        F1iPgm = q1pgm;
        F1iTm  = Timeto6(SystemTime());
        F1iDt  = Dateto7(SystemDate());

       // Flags
        F1upar  = scUpar;
        F1faud  = scFaud;
        F1spla  = scSpla;
        F1rlxc  = scRlxc;
        F1clms  = scClms;
        F1exdv  = scExdv;
        F1pmfl  = scPmfl;
        F1yrad  = scYrad;
        F1cxad  = scCxad;
        F1apfd  = scApfd;
        F1brfl  = scBrfl;
        F1bvfl  = scBvfl;
        F1aced  = scAced;
        F1netr  = scNetr;
        //*** Begin Add *** 01/07/22 **************************************
        If scNetSR = ' ';
           F1netSR = 'N';
        Else;
           If scNetR  = 'Y';
              F1netSr = 'N';
           Else;
              F1netSR = scNetSR;
           EndIf;
        EndIf;
        //*** End   Add *** 01/07/22 **************************************
        F1aafl  = scAafl;
        F1dpym  = scDpym;
        F1apfl  = scApfl;
        F1blfl  = scBlfl;
        F1ny07  = scNy07;
        F1idbd  = scIdbd;
        //*** Begin Add *** 05/14/24 ************************************
        F1SRAs  = scSRAs;
        //*** End   Add *** 05/14/24 ************************************

       // Set A/R Codes fields
        F1srbb  =  strARcod1(1);
        F1nbpt  =  strARcod1(2);
        F1nbtr  =  strARcod1(3);
        F1dscd  =  strARcod1(4);
        F1mfpt  =  strARcod1(5);
        F1sftx  =  strARcod1(6);
        F1prtr  =  strARcod1(7);
        F1nbdt  =  strARcod1(8);
        F1cdet  =  strARcod1(9);
        F11stt  =  strARcod1(10);
        F12ndt  =  strARcod1(11);
        F13rdt  =  strARcod1(12);
        F14tht  =  strARcod1(13);
        F15tht  =  strARcod1(14);
        F16tht  =  strARcod1(15);
        F17tht  =  strARcod1(16);
        F1txtx  =  strARcod1(17);
        F1txpy  =  strARcod1(18);

        F1dbtr  =  strARcod2(1);
        F1tftr  =  strARcod2(2);
        F1dptr  =  strARcod2(3);
        F1tqtr  =  strARcod2(4);
        F1pptr  =  strARcod2(5);
        F1tptr  =  strARcod2(6);
        F1dtxb  =  strARcod2(7);
        F1dtxp  =  strARcod2(8);

        F1mftr  =  strARcod3(1);
        F1satr  =  strARcod3(2);
        F1Txtr  =  strARcod3(3);
        F1fctr  =  strARcod3(4);
        F11atr  =  strARcod3(5);
        F12atr  =  strARcod3(6);
        F13atr  =  strARcod3(7);
        F14atr  =  strARcod3(8);
        F15atr  =  strARcod3(9);
        F16atr  =  strARcod3(10);
        F17atr  =  strARcod3(11);

        F1axdd  =  strARcod4(1);
        F1axec  =  strARcod4(2);
        F1axgf  =  strARcod4(3);
        F1axlf  =  strARcod4(4);
        F1axmf  =  strARcod4(5);
        F1axdp  =  strARcod4(6);
        F1axsf  =  strARcod4(7);
        F1axtx  =  strARcod4(8);
        F1axtd  =  strARcod4(9);
        F1ax1t  =  strARcod4(10);
        F1ax2t  =  strARcod4(11);
        F1ax3t  =  strARcod4(12);
        F1ax4t  =  strARcod4(13);
        F1ax5t  =  strARcod4(14);
        F1ax6t  =  strARcod4(15);
        F1ax7t  =  strARcod4(16);
        F1axpt  =  strARcod4(17);

        F1shtr  =  strARcod5(1);
        F1sftr  =  strARcod5(2);
        F1sitr  =  strARcod5(3);
        F1sqtr  =  strARcod5(4);
        F1sptr  =  strARcod5(5);
        F1srtr  =  strARcod5(6);
        F1spof  =  strARcod5(7);
        F1srpo  =  strARcod5(8);

        F1colb  =  strARcod6(1);
        F1colp  =  strARcod6(2);
        F1colt  =  strARcod6(3);

        F1dvbl  =  strARcod7(1);
        F1dvpd  =  strARcod7(2);
        F1dvxr  =  strARcod7(3);

       EndSr;

       // Read A/R Codes subfile
       Begsr ReadARsfl;

        // Grid 1 -- Specific A/R Transaction Codes
        i = 1;
        ReadC sflSpecAR;
        Dow not %eof;
           strARcod1(i) = scVaSpecAR;
           i += 1;
           ReadC sflSpecAR;
        Enddo;

        // Grid 2 -- Deposit to Premium Transfer
        i = 1;
        ReadC sflDepPrm;
        Dow not %eof;
           strARcod2(i) = scVaDepPrm;
           i += 1;
           ReadC sflDepPrm;
        Enddo;

        // Grid 3 -- A/R Transfer Codes for Flat Cancellation
        i = 1;
        ReadC sflFlatCx;
        Dow not %eof;
           strARcod3(i) = scVaFlatCx;
           i += 1;
           ReadC sflFlatCx;
        Enddo;

        // Grid 4 -- A/R Transfer Codes for Credit Audit Balance
        i = 1;
        ReadC sflCrAuBl;
        Dow not %eof;
           strARcod4(i) = scVaCrAuBl;
           i += 1;
           ReadC sflCrAuBl;
        Enddo;

        // Grid 5 -- S/R Deposit Transfer A/R Codes (Mid-Year)
        i = 1;
        ReadC sflDepoSR;
        Dow not %eof;
           strARcod5(i) = scVaDepoSR;
           i += 1;
           ReadC sflDepoSR;
        Enddo;

        // Grid 6 -- Collateral A/R Codes
        i = 1;
        ReadC sflCollAR;
        Dow not %eof;
           strARcod6(i) = scVaCollAR;
           i += 1;
           ReadC sflCollAR;
        Enddo;

        // Grid 7 -- Dividend Plan
        i = 1;
        ReadC sflDivPln;
        Dow not %eof;
           strARcod7(i) = scVaDivPln;
           i += 1;
           ReadC sflDivPln;
        Enddo;
       EndSr;

       // Writing Policy Years/Group Details subroutine
       BegSr WrtGrpDtl;
        Clear pf10p;

        // Read A/R Codes subfiles
        Exsr ReadARsfl;

        Exsr MoveScDb;
        Write pf10p;
       EndSr;


      /end-free
     P ProcessAddYear  E

       //===================================================================
       // ProcOther2 - Process other Options for Policy Years Grid
       //===================================================================
     P ProcOther2      B
     D ProcOther2      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear RcdGrpDtl;
       isexit = *off;
       ReadC sflPolYr;

       Dow not %eof;

          // Processing for Display Years Link
          If scPolYrDsp = *On;
             scPolYrDsp = *Off;
             mode = 'D';
             scGrpHdg = 'Display Group Detail Information';

             // Loading initial values
             InzGrpDtlScVal();

             // Load Display Group Detail Screen
             Chain(n) scPolRRN WDF1P;
             Exsr MoveDbSc;

             rdFyr = *on;                             // Read only policy year
             inquiry_0 = *on;                         // Read all fields
             visDtlSav  = *on;                        // Hide Save Button
             visDtlSvAd = *on;                        // Hide Save/Add next Button

             // Load A/R Codes subfiles
             Exsr LoadARsfl;

             Exfmt RcdGrpDtl;
          EndIf;


          // Processing for Edit Years Link
          If scPolYrEdt = *On;
             scPolYrEdt = *Off;
             mode = 'C';
             scGrpHdg = 'Edit Group Detail Information';
             svrrnPolYr = rrnPolYr;                       //save rrn value

             // Loading initial values
             InzGrpDtlScVal();

             Chain(n) scPolRRN WDF1P;

             // Load Edit Group Detail Screen
             Exsr MoveDbSc;

             rdFyr = *on;                             // Read only policy year
             visDtlSvAd = *on;                        // Hide Save/Add next Button

             // Load A/R Codes subfiles
             Exsr LoadARsfl;

             Dow isexit = *off;
                Exfmt RcdGrpDtl;

                Select;
                // save Button
                When BtnDtlSave = *on;
                   BtnDtlSave = *off;
                   // Check if WDF1P updated since screen displayed (Years Details)
                   flgRcdUpdated = RcdUpdatedErr('WDF1P':dbrrn9:'F1dt':'F1time':
                    'F1user':saveDT5:saveTM5:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scPolRRN WDF1P;
                      Exsr MoveDbSc;
                      // Load A/R Codes subfiles
                      Exsr LoadARsfl;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateGrpDtl(mode);
                         // Read A/R Codes subfiles
                         Exsr ReadARsfl;
                         Chain scPolRRN WDF1P;
                         Exsr MoveScDb;
                         Update pf10p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cacel Button
                When BtnDtlCan = *on;
                     BtnDtlCan = *off;
                      isexit = *on;
                EndSl;
             EndDo;

             Unlock WDF1P;
          Endif;

          ReadC sflPolYr;
       Enddo;



       // Load A/R Codes subfile
       Begsr LoadARsfl;

        // Clear the subfile
        rrnSpecAR = 0;
        dspSpecAR = *off;
        clrSpecAR = *on;
        rrnDepPrm = 0;
        dspDepPrm = *off;
        clrDepPrm = *on;
        rrnFlatCx = 0;
        dspFlatCx = *off;
        clrFlatCx = *on;
        rrnCrAuBl = 0;
        dspCrAuBl = *off;
        clrCrAuBl = *on;
        rrnDepoSR = 0;
        dspDepoSR = *off;
        clrDepoSR = *on;
        rrnCollAR = 0;
        dspCollAR = *off;
        clrCollAR = *on;
        rrnDivPln = 0;
        dspDivPln = *off;
        clrDivPln = *on;

        Write rcdGrpDtl;

        clrSpecAR = *off;
        dspSpecAR = *on;
        clrDepPrm = *off;
        dspDepPrm = *on;
        clrFlatCx = *off;
        dspFlatCx = *on;
        clrCrAuBl = *off;
        dspCrAuBl = *on;
        clrDepoSR = *off;
        dspDepoSR = *on;
        clrCollAR = *off;
        dspCollAR = *on;
        clrDivPln = *off;
        dspDivPln = *on;

        If dspMode;
           inquiry_0 = *on;
        Endif;

        // Populate the subfile

        // Grid 1 -- Specific A/R Transaction Codes
        i=1;
        Dow i <= 18;
           rrnSpecAR += 1;
           scHdSpecAR = strHeads1(i);
           scVaSpecAR = strARcod1(i);
           scSpecARCH = strSelCr1(i);

           Write sflSpecAR;
           i +=1;
        Enddo;
        rrnSpecAR = 1;

        // Grid 2 -- Deposit to Premium Transfer
        i=1;
        Dow i <= 8;
           rrnDepPrm += 1;
           scHdDepPrm = strHeads2(i);
           scVaDepPrm = strARcod2(i);
           scDepPrmCH = strSelCr2(i);

           Write sflDepPrm;
           i +=1;
        Enddo;
        rrnDepPrm = 1;

        // Grid 3 -- A/R Transfer Codes for Flat Cancellation
        i=1;
        Dow i <= 11;
           rrnFlatCx += 1;
           scHdFlatCx = strHeads3(i);
           scVaFlatCx = strARcod3(i);
           scFlatCxCH = strSelCr3(i);

           Write sflFlatCx;
           i +=1;
        Enddo;
        rrnFlatCx = 1;

        // Grid 4 -- A/R Transfer Codes for Credit Audit Balance
        i=1;
        Dow i <= 17;
           rrnCrAuBl += 1;
           scHdCrAuBl = strHeads4(i);
           scVaCrAuBl = strARcod4(i);
           scCrAuBlCH = strSelCr4(i);

           Write sflCrAuBl;
           i +=1;
        Enddo;
        rrnCrAuBl = 1;

        // Grid 5 -- S/R Deposit Transfer A/R Codes (Mid-Year)
        i=1;
        Dow i <= 8;
           rrnDepoSR += 1;
           scHdDepoSR = strHeads5(i);
           scVaDepoSR = strARcod5(i);
           scDepoSRCH = strSelCr5(i);

           Write sflDepoSR;
           i +=1;
        Enddo;
        rrnDepoSR = 1;

        // Grid 6 -- Collateral A/R Codes
        i=1;
        Dow i <= 3;
           rrnCollAR += 1;
           scHdCollAR = strHeads6(i);
           scVaCollAR = strARcod6(i);
           scCollARCH = strSelCr6(i);

           Write sflCollAR;
           i +=1;
        Enddo;
        rrnCollAR = 1;

        // Grid 7 -- Dividend Plan
        i=1;
        Dow i <= 3;
           rrnDivPln += 1;
           scHdDivPln = strHeads7(i);
           scVaDivPln = strARcod7(i);
           scDivPlnCH = strSelCr7(i);

           Write sflDivPln;
           i +=1;
        Enddo;
        rrnDivPln = 1;
       EndSr;

       // Subrotine to move Database fields into Screen fields.
       BegSr MoveDbSc;

          scFyr   = F1Fyr;
          scStrDt = DtMDYY(F1stpo);
          scEndDt = DtMDYY(F1enpo);
          scDps   = F1dps;
          scMfee  = F1mfee;
          scEmrt  = F1emrt;
          scDadj  = F1dadj;
          scCadj  = F1cadj;
          scNetd  = DtMDYY(F1Netd);
          scScdt  = DtMDYY(F1scdt);
          scManp  = F1manp;

         // Flags
          scUpar = F1upar;
          scFaud = F1faud;
          scSpla = F1spla;
          scRlxc = F1rlxc;
          scClms = F1clms;
          scExdv = F1exdv;
          scPmfl = F1pmfl;
          scYrad = F1yrad;
          scCxad = F1cxad;
          scApfd = F1apfd;
          scBrfl = F1brfl;
          scBvfl = F1bvfl;
          scAced = F1aced;
          scNetr = F1netr;
          //*** Begin Add *** 01/07/22 ************************************
          If F1netSR = ' ';
             scNetSR = 'N';
          Else;
             scNetSR = F1netSR;
          EndIf;
          //*** End   Add *** 01/07/22 ************************************
          scAafl = F1aafl;
          scDpym = F1dpym;
          scApfl = F1apfl;
          scBlfl = F1blfl;
          scNy07 = F1ny07;
          scIdbd = F1idbd;
          //*** Begin Add *** 05/14/24 ************************************
          scSRAs = F1SRAs;
          //*** End   Add *** 05/14/24 ************************************

         // Set A/R Codes fields
           strARcod1(1)  = F1srbb;
           strARcod1(2)  = F1nbpt;
           strARcod1(3)  = F1nbtr;
           strARcod1(4)  = F1dscd;
           strARcod1(5)  = F1mfpt;
           strARcod1(6)  = F1sftx;
           strARcod1(7)  = F1prtr;
           strARcod1(8)  = F1nbdt;
           strARcod1(9)  = F1cdet;
           strARcod1(10) = F11stt;
           strARcod1(11) = F12ndt;
           strARcod1(12) = F13rdt;
           strARcod1(13) = F14tht;
           strARcod1(14) = F15tht;
           strARcod1(15) = F16tht;
           strARcod1(16) = F17tht;
           strARcod1(17) = F1txtx;
           strARcod1(18) = F1txpy;

           strARcod2(1)  = F1dbtr;
           strARcod2(2)  = F1tftr;
           strARcod2(3)  = F1dptr;
           strARcod2(4)  = F1tqtr;
           strARcod2(5)  = F1pptr;
           strARcod2(6)  = F1tptr;
           strARcod2(7)  = F1dtxb;
           strARcod2(8)  = F1dtxp;

           strARcod3(1)  = F1mftr;
           strARcod3(2)  = F1satr;
           strARcod3(3)  = F1Txtr;
           strARcod3(4)  = F1fctr;
           strARcod3(5)  = F11atr;
           strARcod3(6)  = F12atr;
           strARcod3(7)  = F13atr;
           strARcod3(8)  = F14atr;
           strARcod3(9)  = F15atr;
           strARcod3(10) = F16atr;
           strARcod3(11) = F17atr;

           strARcod4(1)  = F1axdd;
           strARcod4(2)  = F1axec;
           strARcod4(3)  = F1axgf;
           strARcod4(4)  = F1axlf;
           strARcod4(5)  = F1axmf;
           strARcod4(6)  = F1axdp;
           strARcod4(7)  = F1axsf;
           strARcod4(8)  = F1axtx;
           strARcod4(9)  = F1axtd;
           strARcod4(10) = F1ax1t;
           strARcod4(11) = F1ax2t;
           strARcod4(12) = F1ax3t;
           strARcod4(13) = F1ax4t;
           strARcod4(14) = F1ax5t;
           strARcod4(15) = F1ax6t;
           strARcod4(16) = F1ax7t;
           strARcod4(17) = F1axpt;

           strARcod5(1)  = F1shtr;
           strARcod5(2)  = F1sftr;
           strARcod5(3)  = F1sitr;
           strARcod5(4)  = F1sqtr;
           strARcod5(5)  = F1sptr;
           strARcod5(6)  = F1srtr;
           strARcod5(7)  = F1spof;
           strARcod5(8)  = F1srpo;

           strARcod6(1)  = F1colb;
           strARcod6(2)  = F1colp;
           strARcod6(3)  = F1colt;

           strARcod7(1)  = F1dvbl;
           strARcod7(2)  = F1dvpd;
           strARcod7(3)  = F1dvxr;

          saveDT5 = F1dt;
          saveTM5 = F1time;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        F1fyr   = scfyr;                 // Policy Year
        F1stpo  = dtCYMD(scStrDt);       // Policy Start date
        F1enpo  = dtCYMD(scEndDt);       // Policy End date
        F1dps   = scDps;                 // Deposit Style
        F1mfee  = scMfee;                // Membership Fee Amount
        F1emrt  = scEmrt;                // Employer Specific Retention
        F1dadj  = scDadj;                // Threshold Amount for Debit Adjustment
        F1cadj  = scCadj;                // Threshold Amount for Credit Adjustment
        F1netd  = dtCYMD(scNetd);        // Starting with Policies Effective
        F1scdt  = dtCYMD(scScdt);        // Stop Collateral Date
        F1manp  = scManp;                // % of Manual Premium

        F1user = q1user;
        F1pgm  = q1pgm;
        F1time = Timeto6(SystemTime());
        F1dt   = Dateto7(SystemDate());

        saveDT5 = F1dt;
        saveTM5 = F1time;

       // Flags
        F1upar  = scUpar;
        F1faud  = scFaud;
        F1spla  = scSpla;
        F1rlxc  = scRlxc;
        F1clms  = scClms;
        F1exdv  = scExdv;
        F1pmfl  = scPmfl;
        F1yrad  = scYrad;
        F1cxad  = scCxad;
        F1apfd  = scApfd;
        F1brfl  = scBrfl;
        F1bvfl  = scBvfl;
        F1aced  = scAced;
        F1netr  = scNetr;
        //*** Begin Add *** 01/07/22 **************************************
        If scNetSR = ' ';
           F1netSR = 'N';
        Else;
           If scNetR  = 'Y';
              F1netSr = 'N';
           Else;
              F1netSR = scNetSR;
           EndIf;
        EndIf;
        //*** End   Add *** 01/07/22 **************************************
        F1aafl  = scAafl;
        F1dpym  = scDpym;
        F1apfl  = scApfl;
        F1blfl  = scBlfl;
        F1ny07  = scNy07;
        F1idbd  = scIdbd;
        //*** Begin Add *** 05/14/24 ************************************
        F1SRAs  = scSRAs;
        //*** End   Add *** 05/14/24 ************************************

       // Set A/R Codes fields
        F1srbb  =  strARcod1(1);
        F1nbpt  =  strARcod1(2);
        F1nbtr  =  strARcod1(3);
        F1dscd  =  strARcod1(4);
        F1mfpt  =  strARcod1(5);
        F1sftx  =  strARcod1(6);
        F1prtr  =  strARcod1(7);
        F1nbdt  =  strARcod1(8);
        F1cdet  =  strARcod1(9);
        F11stt  =  strARcod1(10);
        F12ndt  =  strARcod1(11);
        F13rdt  =  strARcod1(12);
        F14tht  =  strARcod1(13);
        F15tht  =  strARcod1(14);
        F16tht  =  strARcod1(15);
        F17tht  =  strARcod1(16);
        F1txtx  =  strARcod1(17);
        F1txpy  =  strARcod1(18);

        F1dbtr  =  strARcod2(1);
        F1tftr  =  strARcod2(2);
        F1dptr  =  strARcod2(3);
        F1tqtr  =  strARcod2(4);
        F1pptr  =  strARcod2(5);
        F1tptr  =  strARcod2(6);
        F1dtxb  =  strARcod2(7);
        F1dtxp  =  strARcod2(8);

        F1mftr  =  strARcod3(1);
        F1satr  =  strARcod3(2);
        F1Txtr  =  strARcod3(3);
        F1fctr  =  strARcod3(4);
        F11atr  =  strARcod3(5);
        F12atr  =  strARcod3(6);
        F13atr  =  strARcod3(7);
        F14atr  =  strARcod3(8);
        F15atr  =  strARcod3(9);
        F16atr  =  strARcod3(10);
        F17atr  =  strARcod3(11);

        F1axdd  =  strARcod4(1);
        F1axec  =  strARcod4(2);
        F1axgf  =  strARcod4(3);
        F1axlf  =  strARcod4(4);
        F1axmf  =  strARcod4(5);
        F1axdp  =  strARcod4(6);
        F1axsf  =  strARcod4(7);
        F1axtx  =  strARcod4(8);
        F1axtd  =  strARcod4(9);
        F1ax1t  =  strARcod4(10);
        F1ax2t  =  strARcod4(11);
        F1ax3t  =  strARcod4(12);
        F1ax4t  =  strARcod4(13);
        F1ax5t  =  strARcod4(14);
        F1ax6t  =  strARcod4(15);
        F1ax7t  =  strARcod4(16);
        F1axpt  =  strARcod4(17);

        F1shtr  =  strARcod5(1);
        F1sftr  =  strARcod5(2);
        F1sitr  =  strARcod5(3);
        F1sqtr  =  strARcod5(4);
        F1sptr  =  strARcod5(5);
        F1srtr  =  strARcod5(6);
        F1spof  =  strARcod5(7);
        F1srpo  =  strARcod5(8);

        F1colb  =  strARcod6(1);
        F1colp  =  strARcod6(2);
        F1colt  =  strARcod6(3);

        F1dvbl  =  strARcod7(1);
        F1dvpd  =  strARcod7(2);
        F1dvxr  =  strARcod7(3);

       EndSr;

       // Read A/R Codes subfile
       Begsr ReadARsfl;

        // Grid 1 -- Specific A/R Transaction Codes
        i = 1;
        ReadC sflSpecAR;
        Dow not %eof;
           strARcod1(i) = scVaSpecAR;
           i += 1;
           ReadC sflSpecAR;
        Enddo;

        // Grid 2 -- Deposit to Premium Transfer
        i = 1;
        ReadC sflDepPrm;
        Dow not %eof;
           strARcod2(i) = scVaDepPrm;
           i += 1;
           ReadC sflDepPrm;
        Enddo;

        // Grid 3 -- A/R Transfer Codes for Flat Cancellation
        i = 1;
        ReadC sflFlatCx;
        Dow not %eof;
           strARcod3(i) = scVaFlatCx;
           i += 1;
           ReadC sflFlatCx;
        Enddo;

        // Grid 4 -- A/R Transfer Codes for Credit Audit Balance
        i = 1;
        ReadC sflCrAuBl;
        Dow not %eof;
           strARcod4(i) = scVaCrAuBl;
           i += 1;
           ReadC sflCrAuBl;
        Enddo;

        // Grid 5 -- S/R Deposit Transfer A/R Codes (Mid-Year)
        i = 1;
        ReadC sflDepoSR;
        Dow not %eof;
           strARcod5(i) = scVaDepoSR;
           i += 1;
           ReadC sflDepoSR;
        Enddo;

        // Grid 6 -- Collateral A/R Codes
        i = 1;
        ReadC sflCollAR;
        Dow not %eof;
           strARcod6(i) = scVaCollAR;
           i += 1;
           ReadC sflCollAR;
        Enddo;

        // Grid 7 -- Dividend Plan
        i = 1;
        ReadC sflDivPln;
        Dow not %eof;
           strARcod7(i) = scVaDivPln;
           i += 1;
           ReadC sflDivPln;
        Enddo;
       EndSr;

      /end-free
     P ProcOther2      E

       //===================================================================
       // ProcOther3 - Process other Options for Region Grid
       //===================================================================
     P ProcOther3      B
     D ProcOther3      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear RcdRegDtl;
       isexit = *off;
       ReadC sflRegion;

       Dow not %eof;

          // Processing for Edit Region Link
          If scRegEdt = *On;
             scRegEdt = *Off;
             mode = 'C';

             Chain(n) scRegRRN WMREP;
             scRegAE = RErgn#;
             scRegDesAE = REdesc;
             scRegStAE = REstat;
             saveDT6 = REdt;
             saveTM6 = REtime;

             scRegHdg = 'Edit Region Informaion';
             rdRegAE = *on;                           // Read only Region
             svrrnRegion = rrnRegion;                 //save rrn value

             Dow isexit = *off;
                Exfmt RcdRegDtl;

                Select;
                // save Button
                When BtnRegSave = *on;
                   BtnRegSave = *off;
                   // Check if WMREP updated since screen displayed (Region)
                   flgRcdUpdated=RcdUpdatedErr('WMREP':dbrrn10:'REdt':'REtime':
                    'REuser':saveDT6:saveTM6:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRegRRN WMREP;
                      scRegAE = RErgn#;
                      scRegDesAE = REdesc;
                      scRegStAE = REstat;
                      saveDT6 = REdt;
                      saveTM6 = REtime;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateRegDtl(mode);
                         Chain scRegRRN WMREP;
                         Exsr MoveScDb;
                         Update pre0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cacel Button
                When BtnRegCan = *on;
                     BtnRegCan = *off;
                      isexit = *on;
                EndSl;
             EndDo;

             Unlock WMREP;
          EndIf;

          ReadC sflRegion;
       Enddo;


       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        RErgn#  = scRegAE;               // Region Number
        REdesc  = scRegDesAE;            // Region Description
        REstat  = scRegStAE;             // Status

        REuser = q1user;
        REpgm  = q1pgm;
        REtime = Timeto6(SystemTime());
        REdt   = Dateto7(SystemDate());
        saveDT6 = REdt;
        saveTM6 = REtime;
       EndSr;

      /end-free
     P ProcOther3      E

       //===================================================================
       // ProcOther4 - Process other Options for Chapter Grid
       //===================================================================
     P ProcOther4      B
     D ProcOther4      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear RcdChpDtl;
       isexit = *off;
       ReadC sflChapter;

       Dow not %eof;

          // Processing for Edit Chapter Link
          If scChpEdt = *On;
             scChpEdt = *Off;
             mode = 'C';

             Chain(n) scChpRRN WMCPP;
             scChpAE = CPchap;
             scChpDesAE = CPdesc;
             scChpStAE = CPstat;
             saveDT7 = CPdt;
             saveTM7 = CPtime;

             scChpHdg = 'Edit Chapter Informaion';
             rdChpAE = *on;                           // Read only Chapter
             svrrnChapter = rrnChapter;               //save rrn value

             Dow isexit = *off;
                Exfmt RcdChpDtl;

                Select;
                // save Button
                When BtnChpSave = *on;
                   BtnChpSave = *off;
                   // Check if WMCPP updated since screen displayed (Chapter)
                   flgRcdUpdated=RcdUpdatedErr('WMCPP':dbrrn11:'CPdt':'CPtime':
                    'CPuser':saveDT7:saveTM7:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scChpRRN WMCPP;
                      scChpAE = CPchap;
                      scChpDesAE = CPdesc;
                      scChpStAE = CPstat;
                      saveDT7 = CPdt;
                      saveTM7 = CPtime;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateChpDtl(mode);
                         Chain scChpRRN WMCPP;
                         Exsr MoveScDb;
                         Update pcp0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cacel Button
                When BtnChpCan = *on;
                     BtnChpCan = *off;
                      isexit = *on;
                EndSl;
             EndDo;

             Unlock WMCPP;
          EndIf;

          ReadC sflChapter;
       Enddo;


       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        CPchap  = scChpAE;               // Chapter Number
        CPdesc  = scChpDesAE;            // Chapter Description
        CPstat  = scChpStAE;             // Status

        CPuser = q1user;
        CPpgm  = q1pgm;
        CPtime = Timeto6(SystemTime());
        CPdt   = Dateto7(SystemDate());
        saveDT7 = CPdt;
        saveTM7 = CPtime;
       EndSr;

      /end-free
     P ProcOther4      E

       //===================================================================
       // ProcOther5 - Process other Options for Bank Grid
       //===================================================================
     P ProcOther5      B
     D ProcOther5      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear RcdBnkDtl;
       isexit = *off;
       ReadC sflBank;

       Dow not %eof;

          // Processing for Edit Bank Link
          If scBnkEdt = *On;
             scBnkEdt = *Off;
             mode = 'C';
             scBnkFnd = scCo#;
             sveftc = scEftc;                     //Save EFT Flag

             Chain(n) scBnkRRN WMBIP;

             Exsr MoveDbSc;

             // Loading initial values
             InzFinInfScVal(mode);

             scBnkHdg = 'Edit Bank Informaion';
             rdBnkAE = *on;                                      // Read Only Bank Account
             svrrnBank = rrnBank;                                //save rrn value

             Dow isexit = *off;
                Exfmt RcdBnkDtl;

                Select;
                // save Button
                When BtnBnkSave = *on;
                   BtnBnkSave = *off;
                   // Check if WMBIP updated since screen displayed (Bank Info)
                   flgRcdUpdated=RcdUpdatedErr('WMBIP':dbrrn12:'BIdt':'BItime':
                    'BIuser':saveDT8:saveTM8:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scBnkRRN WMBIP;
                      Exsr MoveDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateBnkDtl(mode);
                         Chain scBnkRRN WMBIP;
                         Exsr MoveScDb;
                         Update pbi0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cacel Button
                When BtnBnkCan = *on;
                     BtnBnkCan = *off;
                      isexit = *on;
                EndSl;
             EndDo;

             Unlock WMBIP;
          EndIf;

          ReadC sflBank;
       Enddo;


       // Subrotine to move Database fields into Screen fields
       BegSr MoveDbSc;
        scBnkStAE = BIstat;              // Status
        scAcct  = BIacct;
        scAi01  = BIai01;
        scAi02  = BIai02;
        scAi03  = BIai03;
        scAppl  = BIappl;
        scCktp  = BIcktp;
        scEftc  = BIeftc;
        scBkEfDt = dtMDYY(BIEfdt);
        scFtyp  = BIftyp;
        sc$lmt  = BI$lmt;
        scCcid  = %trim(BIccid);
        scBidcid  = %trim(BIdcid);
        sc50n3  = BI50n3;
        scMacr  = BImacr;
        scOutq  = BIoutq;
        scCpyq  = BIcpyq;
        scCkpv  = BIckpv;
        SCTxtOnCk = %trim(BItxtOnCk) + %trim(BItxtOnck2);
        scHdcy  = BIhdcy;
        scTrst  = BItrst;
        sc1A2   = BI1A2;
        scLogo  = BIlogo;
        sc40A1  = BI40A1;
        sc40A2  = BI40A2;
        sc40A3  = BI40A3;
        sc40A4  = BI40A4;
        scRtup  = BIrtup;
        scRtlw  = BIrtlw;
        scTrns  = BItrns;
        scBnk#  = BIbnk#;
        scCkdg  = BIckdg;
        scAxou  = BIaxou;
        scOnus  = BIonus;
        scNam1  = BInam1;                // Signature 1
        scNam2  = BInam2;                // Signature 2
        saveDT8 = BIdt;
        saveTM8 = BItime;

       // Load E-Pay Info
        Chain(N) (BIco#:BIappl:BIfnd:BIacct:BIckTp:BIefDt) pb200;
        If %Found(Wdb2l1);
           scEpVend   = B2ePvend;
           scVpFacTbl = B2vPfacTbl;
           scVpFacFf  = B2vPfacff;
           scEpClmInd = B2ePclmInd;
           scEpClmMed = B2ePclmMed;
           scEpClmExp = B2ePclmExp;
           scEpClmLeg = B2ePclmLeg;
        Endif;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        BIstat  = scBnkStAE;             // Status
        BIeftc  = scEftc;
        BIai01  = scAi01;
        BIai02  = scAi02;
        BIai03  = scAi03;
        BIftyp  = scFtyp;
        BI$lmt  = sc$lmt;
        BIccid  = %trim(scCcid);
        BIdcid  = %trim(scBidcid);
        BI50n3  = sc50n3;
        BImacr  = scMacr;
        BIoutq  = scOutq;
        BIcpyq  = scCpyq;
        BIckpv  = scCkpv;
        BItxtOnCk  = %subst(scTxtOnCk:1:20);
        BItxtOnCk2 = %subst(scTxtOnCk:21:20);
        BIhdcy  = scHdcy;
        BItrst  = scTrst;
        BI1A2   = sc1A2;
        BIlogo  = scLogo;
        BI40A1  = sc40A1;
        BI40A2  = sc40A2;
        BI40A3  = sc40A3;
        BI40A4  = sc40A4;
        BIrtup  = scRtup;
        BIrtlw  = scRtlw;
        BItrns  = scTrns;
        BIbnk#  = scBnk#;
        BIckdg  = scCkdg;
        BIaxou  = scAxou;
        BIonus  = scOnus;
        BInam1  = scNam1;                // Signature 1
        BInam2  = scNam2;                // Signature 2
        // Load Signature Font and Command from WMSGP
        If scNam1 <> *Blanks;
           Chain(n) (scCo#:scNam1) psg00;
           If %found(wmsgl1);
              BI10a1  = SGfont;          // Signature 1 Font/String
              BIcmd1  = SGcmd;           // Signature 1 PCL5 Command
           EndIf;
        EndIf;
        If scNam2 <> *Blanks;
           Chain(n) (scCo#:scNam2) psg00;
           If %found(wmsgl1);
              BI10a2  = SGfont;          // Signature 2 Font/String
              BIcmd2  = SGcmd;           // Signature 2 PCL5 Command
           EndIf;
        EndIf;

        BIuser = q1user;
        BIpgm  = q1pgm;
        BItime = Timeto6(SystemTime());
        BIdt   = Dateto7(SystemDate());
        saveDT8 = BIdt;
        saveTM8 = BItime;

        // Load E-Pay Info
        Chain (BIco#:BIappl:BIfnd:BIacct:BIckTp:BIefDt) pb200;

        B2ePvend   = scEpVend;
        B2vPfacTbl = scVpFACTbl;
        B2vPfacFf  = scVpFACff;
        B2ePclmInd = scEpClmInd;
        B2ePclmMed = scEpClmMed;
        B2ePclmExp = scEpClmExp;
        B2ePclmLeg = scEpClmLeg;
        B2user = q1user;
        B2pgm  = q1pgm;
        B2time = Timeto6(SystemTime());
        B2dt   = Dateto7(SystemDate());

        If %Found(wdb2l1);
           Update pb200;
        Else;
           If scEpVend <> ' ';
              B2co#   = scCo#;
              B2fnd   = scFnd;
              B2appl  = scAppl;
              B2acct  = scAcct;
              B2efdt  = dtCYMD(scBkEfDt);
              B2ckTP  = BIckTp;          // Bank Account Code

              B2iUsr = B2user;
              B2iPgm = B2pgm;
              B2iDt  = B2dt;
              B2iTim = B2time;
              Write pb200;
           Endif;
        Endif;

       EndSr;

      /end-free
     P ProcOther5      E

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free

       // Program Mode.
       If q1prms >= 2;
          //*** Beg Add ***** 01/17/22 *****************************
          scActMode = @@mode;
          //*** End Add ***** 01/17/22 *****************************
          Select;
          When @@mode = 'D';
             dspMode = *on;
          When @@mode = 'C';
             chgMod  = *on;
          Endsl;
       Endif;

       // Set Screen Heading
       scHeading = 'Primary Group Information';

       //*** Beg Del *** 05/23/23 ******************************************
       // Set Tab Names
       //*** strTabNames += AddToList('Group Information':arrTabNames:n1);
       //*** strTabNames += AddToList('Additional Information':arrTabNames:n1);
       //*** strTabNames += AddToList('Financial Controls':arrTabNames:n1);
       //*** TbNmRcdPnl = strTabNames;
       //*** End Del *** 05/23/23 ******************************************

       apShowMenu = *on;

       // Set Menu options
       scMenuChoi = 'Primary Group Information';
       scMenuValu = 'WTGRPPRI';

       scMenuChoi = %trim(scMenuChoi) + ',Administration / DARTS Configuration';
       scMenuValu = %trim(scMenuValu) + ',WTGRPADMIN';

       scMenuChoi = %trim(scMenuChoi) + ',Policy Configuration';
       scMenuValu = %trim(scMenuValu) + ',WTGRPPOLCG';

       scMenuChoi = %trim(scMenuChoi) + ',Claims Configuration';
       scMenuValu = %trim(scMenuValu) + ',WTGRPCLMCG';

       scMenuChoi = %trim(scMenuChoi) + ',State Specific Information';
       scMenuValu = %trim(scMenuValu) + ',WTGRPSSI';

       scMenuChoi = %trim(scMenuChoi) + ',Data Reporting';
       scMenuValu = %trim(scMenuValu) + ',WTGRPDATA';

       aMenHeight = 145;

       apFootHght = aMenHeight + 125;

       // get the system values
       GetSystem(##Wcif:##Mcif:##Wcin);

       //*** Beg Add ***** 09/18/23 *****************************
       // Retrieve Client Suffix
       RtvCltSuffix(suffix);
       //*** End Add ***** 09/18/23 *****************************

       // Tab Index
       tabIndex = 0;
       If q1prms >= 4;
          If @@TabInd <> ' ';
             tabIndex = %int(@@TabInd);
             If tabIndex < 0 or tabIndex > 2;
                TabIndex = 0;
             Endif;
          Endif;
          @@TabInd = '00000';
       Endif;

       intCo# = GetUserInfo(q1user);
     // Update Action Panel Info (Rrn, Group Name, Group Number)
       scActRrn = xxRrn;
       scCompName = %trim(FNname);
       scGrpNum = FNfnd;
       If scCompName = ' ';
          Chain intCo# pco00;
          If COname <> ' ';
             scCompName = COname;
          Endif;
       Endif;

       scCo# = intCo#;

     // Check Portal Module
       isPortal = *off;
       isPortal = IsModEnabled('Portal Module');

       // Get Flags value from Workers Comp control file
       // Determine if client is Admin only or Claims only
          visbAdmin = *off;
          visbClaim = *off;
          Chain 'WC' SWC01;
          If %found();
             If WCfl08 = 'A';                          // Admin Only
                visbAdmin = *on;
             Endif;
             If WCfl08 = 'C';                          // Claims only
                visbClaim = *on;
             Endif;
          Endif;

       processMe = *on;

       //*** Beg Add *** 05/23/23 ******************************************
       isNoAccess = *off;

       // Set User Access button visibility indicator based on Users Security
       Chain (Q1user) pup01;
       //*** Beg Chg *** 06/27/23 ******************************************
       //*** If %Found(SMUPL) and UPlvl = 999 and UPtype = '*';
       // based on Users Security Type and Level defined in Application Confguration
       If CheckSecLevel('GrpCfgAut':UPtype:UPlvl);
       //*** End Chg *** 06/27/23 ******************************************
          isUsrAcc = *on;
       Else;
          isUsrAcc = *off;
       Endif;

       //*** Beg Add *** 06/23/23 ******************************************
       // Display Group Information in Inquiry Mode even if Users Security
       // level does not allow them to Edit Group Information.
       If dspMode = *on;
          isUsrAcc = *on;
       Endif;
       //*** End Add *** 06/23/23 ******************************************

       // Determine if user is in the Group Config Tab file for each tab.  This
       // will determine which tabs are displayed.  If the user has 999 * then
       // all tabs are displayed

       // Group Information tab
       Chain (intCo#:'WTGRPPRI':'Primary Group Information':
        'Group Information':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Group Information':arrTabNames:n1);
       Endif;

       // Additional Information tab
       Chain (intCo#:'WTGRPPRI':'Primary Group Information':
        'Additional Information':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Additional Information':arrTabNames:n1);
       Endif;

       // Financial Controls tab
       Chain (intCo#:'WTGRPPRI':'Primary Group Information':
        'Financial Controls':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Financial Controls':arrTabNames:n1);
       Endif;

       TbNmRcdPnl = strTabNames;
       //*** End Add *** 05/23/23 ******************************************

       Return;


      /end-free
     p ProgramStart    e

      //===================================================================
      // Validate the Additional information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidAddInfo    b
     d ValidAddInfo    pi              n
     d isError3        s               n   inz
      /Free

        isError3 = *off;
       // Initialize all the messageId and error indicators.
        erFh#1 = *off;
        msFh#1 = *blanks;
        erFh1w = *off;
        msFh1w = *blanks;
        erFh#2 = *off;
        msFh#2 = *blanks;
        erFh2w = *off;
        msFh2w = *blanks;
        erFh#3 = *off;
        msFh#3 = *blanks;
        erFh3w = *off;
        msFh3w = *blanks;
        erFh#4 = *off;
        msFh#4 = *blanks;
        erFh4w = *off;
        msFh4w = *blanks;
        erFh#5 = *off;
        msFh#5 = *blanks;
        erFh5w = *off;
        msFh5w = *blanks;
        erFh#6 = *off;
        msFh#6 = *blanks;
        erFh6w = *off;
        msFh6w = *blanks;

      // Validate Flexible Heading Info fields

      // Heading field #1 can't blank if value in other fields entered
        If scFh#1 = ' ' and (scF1tb <> ' ' or scF1df <> ' '
         or scFh1q <> ' ' or scFh1w <> ' ');
           erFh#1 = *on;
           msFh#1 = 'WCR0974';
           isError3 = *on;
        Endif;

      // Required Written should be same as Required Quote.
        If scFh1q <> ' ' or scFh1w <> ' ';
           If scFh1w <> scFh1q;
              erFh1w = *on;
              msFh1w = 'WC40067';
              isError3 = *on;
           Endif;
        Endif;

      // Heading field #2 can't blank if value in other fields entered
        If scFh#2 = ' ' and (scF2tb <> ' ' or scF2df <> ' '
         or scFh2q <> ' ' or scFh2w <> ' ');
           erFh#2 = *on;
           msFh#2 = 'WCR0974';
           isError3 = *on;
        Endif;

      // Required Written should be same as Required Quote.
        If scFh2q <> ' ' or scFh2w <> ' ';
           If scFh2w <> scFh2q;
              erFh2w = *on;
              msFh2w = 'WC40067';
              isError3 = *on;
           Endif;
        Endif;

      // Heading field #3 can't blank if value in other fields entered
        If scFh#3 = ' ' and (scF3tb <> ' ' or scF3df <> ' '
         or scFh3q <> ' ' or scFh3w <> ' ');
           erFh#3 = *on;
           msFh#3 = 'WCR0974';
           isError3 = *on;
        Endif;

      // Required Written should be same as Required Quote.
        If scFh3q <> ' ' or scFh3w <> ' ';
           If scFh3w <> scFh3q;
              erFh3w = *on;
              msFh3w = 'WC40067';
              isError3 = *on;
           Endif;
        Endif;

      // Heading field #4 can't blank if value in other fields entered
        If scFh#4 = ' ' and (scFh4q <> ' ' or scFh4w <> ' ');
           erFh#4 = *on;
           msFh#4 = 'WCR0974';
           isError3 = *on;
        Endif;

      // Required Written should be same as Required Quote.
        If scFh4q <> ' ' or scFh4w <> ' ';
           If scFh4w <> scFh4q;
              erFh4w = *on;
              msFh4w = 'WC40067';
              isError3 = *on;
           Endif;
        Endif;

      // Heading field #5 can't blank if value in other fields entered
        If scFh#5 = ' ' and (scFh5q <> ' ' or scFh5w <> ' ');
           erFh#5 = *on;
           msFh#5 = 'WCR0974';
           isError3 = *on;
        Endif;

      // Required Written should be same as Required Quote.
        If scFh5q <> ' ' or scFh5w <> ' ';
           If scFh5w <> scFh5q;
              erFh5w = *on;
              msFh5w = 'WC40067';
              isError3 = *on;
           Endif;
        Endif;

      // Heading field #6 can't blank if value in other fields entered
        If scFh#6 = ' ' and (scF6tb <> ' ' or scF6df <> ' '
         or scFh6q <> ' ' or scFh6w <> ' ');
           erFh#6 = *on;
           msFh#6 = 'WCR0974';
           isError3 = *on;
        Endif;

      // Required Written should be same as Required Quote.
        If scFh6q <> ' ' or scFh6w <> ' ';
           If scFh6w <> scFh6q;
              erFh6w = *on;
              msFh6w = 'WC40067';
              isError3 = *on;
           Endif;
        Endif;




      // Return *on indicator when No error exists.
        If isError3 = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidAddInfo    e

      //===================================================================
      // Validate Bank addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateBnkDtl  b
     d ValidateBnkDtl  pi              n
     d strmode                        1
     d isError6        s               n   inz
      /Free

        isError6 = *off;
       // Initialize all the messageId and error indicators.
        erAcct = *off;
        msAcct = *blanks;
        erAppl = *off;
        msAppl = *blanks;
        erBkEfDt = *off;
        msBkEfDt = *blanks;
        erBkEfDt2 = *off;
        mgBkEfDt = *blanks;
        erCktp = *off;
        msCktp = *blanks;
        erEftc = *off;
        msEftc = *blanks;
        erFtyp = *off;
        msFtyp = *blanks;
        erMacr = *off;
        msMacr = *blanks;
        erOutq = *off;
        msOutq = *blanks;
        erCpyq = *off;
        msCpyq = *blanks;
        erEpVend = *off;
        mgEpVend = *blanks;
        erVpFacTbl = *off;
        msVpFacTbl = *blanks;
        erVpFacFf = *off;
        msVpFacFf = *blanks;

       // Read Bank Master file WSCKL
        Chain (inCo#:scAcct) pck00;

        If strmode <> 'C';                            // Add mode
          // Account Number cannot be Blank
           If scAcct = *Blanks;
              erAcct = *on;
              msAcct = 'WCR0503';
              isError6 = *on;
           Endif;

          // If Payment type of Group is "Voucher" in Auxiliary file WDF2L
          // then Bank account must be of "Voucher" payment type
           Chain(n) (inCo#:inFnd) pf200;
           If %found(wdf2l);
              If F2ctyp = 'V' and scVchr <> 'Y';
                 erAcct = *on;
                 msAcct = 'WC30085';
                 isError6 = *on;
              Endif;
           Endif;


          // Application Type cannot be Blank
           If scAppl = *Blanks;
              erAppl = *on;
              msAppl = 'WCR0503';
              isError6 = *on;
           Endif;
          // Validate Application Type
           If erAppl = *off;
              If (scAppl = 'WP' and WCfl24 <> 'Y') or
                 (scAppl = 'RP' and WCfl25 <> 'Y') or
                 (scAppl = 'IC' and WCfl36 <> 'Y');
                 erAppl = *on;
                 msAppl = 'WC30114';
                 isError6 = *on;
              Endif;
           Endif;


          // Validation Check for Effective Date
           erBkEfDt= ValidDate(scBkEfDt:charMsg);
           If erBkEfDt = *on;
              msBkEfDt = charMsg;
              isError6 = *on;
           Endif;
          // If Effective Date Less than Today, Check for Duplicate Bank Account
           If erBkEfDt = *off and dtCYMD(scBkEfDt) < Dateto7(SystemDate());
              ##co# = %char(inCo#);
              ##fnd = %char(inFnd);
              ##srdt = %char(SystemDate());
              ##hdcy = *Blanks;
              ##chkq = *Blanks;
              ##ftyp = *Blanks;
              ##rtcd = *Blanks;
              ##dt   = *Zeros ;
              ##cpyq = *Blanks;
              ##coid = *Blanks;
              ##acct = *Blanks;
              Select;
              When CKvchr = 'Y';
                 ##call = 'V';
              When CKvchr = 'N';
                 ##call = 'C';
              Other;
                 ##call = CKvchr;
              EndSl;

              // Call API to access "Current" Bank Account based on ##call value
              ##acct= GetBankAcc(##co#: ##fnd: scAppl: ##srdt: ##call:
                       ##hdcy: ##chkq: ##ftyp: ##dt: ##cpyq:
                       scCktp: scEftc: ##coid: ##rtcd);

              If ##rtcd = *Blanks;
              // Bank Info found for Voucher/Check, with effective date prior to today.
                 erBkEfDt = *on;
                 msBkEfDt = 'WC30102';
                 isError6 = *on;
              Endif;
           Endif;
          // Effective Date should not past the Bank Master Acct Expiry Date
           If erBkEfDt = *off;
              If dtCYMD(scBkEfDt) > CKexdt and CKexdt <> 0;
                 erBkEfDt2 = *on;
                 mgBkEfDt = 'Bank Account is Expired as of this Date';
                 isError6 = *on;
              Endif;
           Endif;
          // Effective Date should not be prior to Bank Master Acct Effective Date
           If erBkEfDt = *off and erBkEfDt2 = *off;
              If dtCYMD(scBkEfDt) < CKefdt and CKefdt <> 0;
                 erBkEfDt2 = *on;
                 mgBkEfDt = 'Bank Master Record is Not Effective as of this -
                            Date';
                 isError6 = *on;
              Endif;
           Endif;


          // Bank Account Code cannot be 'EFT'
           If scCktp = %trim('EFT');
              erCktp = *on;
              msCktp = 'W400064';
              isError6 = *on;
           Endif;
          // Validate the bank Account code by procedure call ValidBankCode
           If erCktp = *off and isError6 = *off;
              ##co# = %char(inCo#);
              ##fnd = %char(inFnd);
              ##fyr =  *Zeros;
              ##case = *Zeros;
              ##data1 = *Blanks;
              If Not ValidBankCode(##co#:##fnd:##fyr:##case:scAppl:
                 ##data1:scCktp);
                 erCktp = *on;
                 msCktp = 'WCR0505';
                 isError6 = *on;
              Endif;
           Endif;


          // Validate EFT flag for MICR value
           If scEftc = *Blanks and (CKbnk# = ' ' or CKtrns = ' ' or
              CKonus = ' ' or CKrtup = ' ' or CKrtlw = ' ');
              erEftc = *on;
              msEftc = 'W400066';
              isError6 = *on;
           Endif;
          // Validate EFT flag for EFT value
           If scEftc <> *Blanks and (CKidsa = *zeros or CKiora = *zeros);
              erEftc = *on;
              msEftc = 'W400067';
              isError6 = *on;
           Endif;
          // Check for any Duplicate record in WMBIP for current EFT flag
           If erEftc = *off;
              inBkEfDt  = dtCYMD(scBkEfDt);
              Setll keyWMBIL6 pbi06;
              Reade keyWMBIL6 pbi06;
              Dow Not %EOF(WMBIL6);
                 // Only check flags if existing bank info is active
                 If BIstat = 'A';
                    Select;
                    When scEftc = *blanks and BIeftc = 'E';
                       erEftc = *on;
                       msEftc = 'WCR4804';
                       isError6 = *on;
                    When sceftc = 'E' and BIeftc = *blanks;
                       erEftc = *on;
                       msEftc = 'WCR4805';
                       isError6 = *on;
                    Other;
                       erEftc = *on;
                       msEftc = 'WC30104';
                       isError6 = *on;

                       Leave;
                    EndSl;
                 Endif;
                 Reade keyWMBIL6 pbi06;
              Enddo;
           Endif;


        Endif;                                // End of strmode <> 'C'


       // Validation applicable only to Edit Mode
        If strmode = 'C';                            // Edit mode

          // Validate EFT flag changed value
           If scEftc = *blank and (sveftc = 'E' or sveftc = 'B');
              inBkEfDt  = dtCYMD(scBkEfDt);
              hasEft = GetEFTExist(inCo#:inFnd:inBkEfDt:scAppl:scAcct:scCktp);
              Select;
              When not(hasEFT);
              When sveftc = 'E';
                 erEftc = *on;
                 msEftc = 'W400068';
                 isError6 = *on;
              When sveftc = 'B';
                 erEftc = *on;
                 msEftc = 'W400069';
                 isError6 = *on;
              Endsl;
           Endif;
          // Validate EFT flag for MICR value
           If scEftc = 'B' and (CKbnk# = ' ' or CKtrns = ' ' or
              CKonus = ' ' or CKrtup = ' ' or CKrtlw = ' ');
              erEftc = *on;
              msEftc = 'W400066';
              isError6 = *on;
           Endif;
          // Validate EFT flag for EFT value
           If scEftc <> *Blanks and (CKidsa = *zeros or CKiora = *zeros);
              erEftc = *on;
              msEftc = 'W400067';
              isError6 = *on;
           Endif;

        Endif;                                // End of strmode = 'C'


      // Validation applicable to both Add and Edit Mode
      // Following fields are Mandatory if Print Hardcopy of Check/Voucher is 'Y'
        If scHdcy = 'Y';
         // Formtype cannot be Blank
           If scFtyp = *Blanks;
              erFtyp = *on;
              msFtyp = 'WCR0503';
              isError6 = *on;
           Endif;
         // Macro ID cannot be Zero
           If scMacr = *Zeros;
              erMacr = *on;
              msMacr = 'WCR0502';
              isError6 = *on;
           Endif;
         // Check Output Queue cannot be Blank
           If scOutq = *Blanks;
              erOutq = *on;
              msOutq = 'WCR0503';
              isError6 = *on;
           Endif;
         // File Copy Output Queue cannot be Blank
           If scCpyq = *Blanks;
              erCpyq = *on;
              msCpyq = 'WCR0503';
              isError6 = *on;
           Endif;

        Endif;

      // Validate Vendor Pay Funding Account Code
       // These fields are mutually exclusive
        If scVpFacTbl <> ' ' and scVpFacFf <> ' ';
           erVpFacTbl = *on;
           msVpFacTbl = 'WCR1022';
           erVpFacFf = *on;
           msVpFacFf = 'WCR1022';
           isError6 = *on;
        Endif;
       // This field can not be blank
        If scEpVend <> ' ';
           If scVpFacTbl = ' ' and scVpFacFf = ' ';
              erVpFacTbl = *on;
              msVpFacTbl = 'WCR0503';
              erVpFacFf = *on;
              msVpFacFf = 'WCR0503';
              isError6 = *on;
           Endif;
        Endif;
       // This field must be blank
        If scEpVend = ' ';
           If scVpFacTbl <> ' ';
              erVpFacTbl = *on;
              msVpFacTbl = 'WCR0450';
              isError6 = *on;
           Endif;
           If scVpFacFf <> ' ';
              erVpFacFf = *on;
              msVpFacFf = 'WCR0450';
              isError6 = *on;
           Endif;
        Endif;

       // Validation applicable only to Add Mode
        If strmode = 'A';                            // Add mode
          // validate Application Type for New E-Pay Vendor record
           chkappl = 'N';
           If scEpVend <> ' ';
              Setll *loval pevm00;
              Read pevm00;
              Dow Not %Eof(WMEVML1);
                 If EVMappl = scappl and EVMVend = scEpVend;
                    chkappl = 'Y';
                    Leave;
                 Endif;
              Read pevm00;
              Enddo;
              If chkappl <> 'Y';
                 erEpVend = *on;
                 mgEpVend = 'E-Pay Vendor is not Setup for this Bank Account +
                  and Application Type';
                 isError6 = *on;
              Endif;
           Endif;
        Endif;


      // Return *on indicator when No error exists.
        If isError6 = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateBnkDtl  e

      //===================================================================
      // Validate Chapter Details addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateChpDtl  b
     d ValidateChpDtl  pi              n
     d strmode                        1
     d isError5        s               n   inz
      /Free

        isError5 = *off;
       // Initialize all the messageId and error indicators.
        erChpAE = *off;
        msChpAE = *blanks;
        erChpDesAE = *off;
        msChpDesAE = *blanks;

        If strmode <> 'C';
          // Validate Chapter
           If scChpAE <= *zero;
              erChpAE = *on;
              msChpAE = 'WCR0502';
              isError5 = *on;
           Endif;
          // Duplicate Chapter
           If scChpAE <> *zero;
              inChp = scChpAE;
              chain keyWMCPL pcp00;
              If %found(wmcpl);
                 erChpAE = *on;
                 msChpAE = 'WC30005';
                 isError5 = *on;
              Endif;
           Endif;
        Endif;

       // Validate Chapter Description
        If scChpDesAE = *Blanks;
           erChpDesAE = *on;
           msChpDesAE = 'WCR0520';
           isError5 = *on;
        Endif;


      // Return *on indicator when No error exists.
        If isError5 = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateChpDtl  e

      //===================================================================
      // Validate the Group Master information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateGroup   b
     d ValidateGroup   pi              n
     d isError         s               n   inz
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erName = *off;
        msName = *blanks;
        erShnm = *off;
        msShnm = *blanks;
        erMad1 = *off;
        msMad1 = *blanks;
        erMcty = *off;
        msMcty = *blanks;
        erMst  = *off;
        msMst  = *blanks;
        erMzip = *off;
        msMzip = *blanks;
        erPad1 = *off;
        msPad1 = *blanks;
        erPcty = *off;
        msPcty = *blanks;
        erPst  = *off;
        msPst  = *blanks;
        erPzip = *off;
        msPzip = *blanks;
        erPortDft = *off;
        msPortDft = *blanks;
        erAsOfP = *off;
        msAsOfP = *blanks;
        erEfDta = *off;
        msEfDta = *blanks;
        erExDta = *off;
        msExDta = *blanks;
        erMxex = *off;
        msMxex = *blanks;
        erNacs = *off;
        msNacs = *blanks;
        erNaen = *off;
        msNaen = *blanks;
        erNaqn = *off;
        msNaqn = *blanks;
        erMetr = *off;
        msMetr = *blanks;
        erMep1 = *off;
        msMep1 = *blanks;
        erExtr = *off;
        msExtr = *blanks;
        erExp1 = *off;
        msExp1 = *blanks;
        erCotr = *off;
        msCotr = *blanks;
        erCop1 = *off;
        msCop1 = *blanks;
        erLetr = *off;
        msLetr = *blanks;
        erLep1 = *off;
        msLep1 = *blanks;
        erDfrc = *off;
        msDfrc = *blanks;
        erDfra = *off;
        msDfra = *blanks;
        erDfxc = *off;
        msDfxc = *blanks;
        erDfxa = *off;
        msDfxa = *blanks;
        erPeoe = *off;
        msPeoe = *blanks;
        erPeoa = *off;
        msPeoa = *blanks;
        //*** Beg Add *** 05/08/24 ****************************
        erAsofg = *off;
        msAsofg = *blanks;
        //*** End Add *** 05/08/24 ****************************
        erChk1 = *off;
        msChk1 = *blanks;
        erChk2 = *off;
        msChk2 = *blanks;
        erSior = *off;
        msSior = *blanks;
        erSiau = *off;
        msSiau = *blanks;
        erEdif = *off;
        msEdif = *blanks;
        erMidy = *off;
        msMidy = *blanks;
        erWidy = *off;
        msWidy = *blanks;
        erAnam = *off;
        msAnam = *blanks;
        erAad1 = *off;
        msAad1 = *blanks;
        erActy = *off;
        msActy = *blanks;
        erAst  = *off;
        msAst  = *blanks;
        erAzip = *off;
        msAzip = *blanks;
       //*** Beg Add ***  04/29/24  *** WC2965 *************************
        erEmail = *off;
        msEmail = *blanks;
       //*** End Add ***  04/29/24  *** WC2965 *************************
       //*** Beg Add ***** 05/08/25 ************************************
        erACphn# = *off;
        msACphn# = *blanks;
        erACEmlAd = *off;
        msACEmlAd = *blanks;
       //*** End Add ***** 05/08/25 ************************************

        clear dsErClmRsv;
        clear dsMsClmRsv;
       //*** begin add ***  09/25/24  *************************************
       //* Read Scriptura module file.
       Read(n) psc0p;
       //*** end add *****  09/25/24  *************************************

       // Group name cannot be blank.
        If scName = *blanks;
           erName = *on;
           msName = 'WCR0503';
           isError = *on;
        Endif;

       // Short name cannot be blank.
        If scShnm = *blanks;
           erShnm = *on;
           msShnm = 'WCR0503';
           isError = *on;
        Endif;

       // Enter complete info for Mailing address, if any of the field is entered
        If scMad1 <> *blanks or scMad2 <> *blanks or
         scMcty <> *blanks or scMst <> *blanks or scMzip <> 0;
           If scMad1 = *Blanks;
              erMad1 = *on;
              msMad1 = 'WCR0503';
              isError = *on;
           Endif;
           If scMcty = *blanks;
              erMcty = *on;
              msMcty = 'WCR0503';
              isError = *on;
           Endif;
           If scMst  = *blanks;
              erMst  = *on;
              msMst  = 'WCR0503';
              isError = *on;
           Endif;
           If scMzip = *zeros;
              erMzip = *on;
              msMzip = 'WCR0556';
              isError = *on;
           Endif;
        Endif;

       // Enter complete info for Physical address, if any of the field is entered
        If scPad1 <> *blanks or scPad2 <> *blanks or
         scPcty <> *blanks or scPst <> *blanks or scPzip <> 0;
           If scPad1 = *Blanks;
              erPad1 = *on;
              msPad1 = 'WCR0503';
              isError = *on;
           Endif;
           If scPcty = *blanks;
              erPcty = *on;
              msPcty = 'WCR0503';
              isError = *on;
           Endif;
           If scPst  = *blanks;
              erPst  = *on;
              msPst  = 'WCR0503';
              isError = *on;
           Endif;
           If scPzip = *zeros;
              erPzip = *on;
              msPzip = 'WCR0556';
              isError = *on;
           Endif;
        Endif;

       // Check for portal default flag set for another group
        If scPortDft = 'Y';
           Reset decFnd;
           Exec sql
           select FNfnd into :decFnd
           from WMFNP
            where FNco# = :scCo#
             and FNfnd <> :scFnd
             and FNportDft = 'Y';
           If decFnd <> 0;
              erPortDft = *on;
              msPortDft = 'Group ' + %editc(decFnd:'Z') +
               ' is already defined as the default group.';
              isError = *on;
           Endif;
        Endif;

       // Validation Check for Portal As of Date, if entered
        If isPortal = *on and isCmpDft = *off;
           If scAsOfP <> 0;
              erAsOfP = ValidDate(scAsOfP:charMsg);
              If erAsOfP = *on;
                 msAsOfP = charMsg;
                 isError = *on;
              Endif;
           Endif;
        Endif;

       // Validation Check for Effective Date
        erEfDta = ValidDate(scEfDta:charMsg);
        If erEfDta = *on;
           msEfDta = charMsg;
           isError = *on;
        Endif;

       // Validation Check for Expiration Date
        If scExDta <> 0;
           erExDta = ValidDate(scExDta:charMsg);
           If erExDta = *on;
              msExDta = charMsg;
              isError = *on;
           Endif;
        Endif;

       // Dates must be in Ascending order
        If scEfDta <> 0 and scExDta <> 0;
           If erExDta = *off and erEfDta = *off;
              erExDta = AscendDates(scEfDta:scExDta:charMsg);
              If erExDta = *on;
                 erEfDta = *on;
                 msExDta = charMsg;
                 msEfDta = charMsg;
                 isError = *on;
              Endif;
           Endif;
        Endif;

       // Max extended year policy (in months) must be > 12
        If scMxex <> 0 and scMxex < 12;
           erMxex = *on;
           msMxex = 'WCR2376';
           isError = *on;
        Endif;

       // Next Available Case Number must be greater than 0
        If scNacs <= *zeros;
           erNacs = *on;
           msNacs = 'WCR0502';
           isError = *on;
        Endif;

       // Next Available Employer Number must be greater than 0
        If scNaen <= *zeros;
           erNaen = *on;
           msNaen = 'WCR0502';
           isError = *on;
        Endif;

       // Next Available Quote Number must be greater than 0
        If scNaqn <= *zeros;
           erNaqn = *on;
           msNaqn = 'WCR0502';
           isError = *on;
        Endif;


       // Validate Net Deuctible Transaction Code and PV1
       // Medical
        If (scMetr <> *blanks and scMep1 = *blanks) or
         (scMetr = *blanks and scMep1 <> *blanks);
           erMetr = *on;
           msMetr = 'WCR0544';
           erMep1 = *on;
           msMep1 = 'WCR0544';
           isError = *on;
        Endif;

       // Expense
        If (scExtr <> *blanks and scExp1 = *blanks) or
         (scExtr = *blanks and scExp1 <> *blanks);
           erExtr = *on;
           msExtr = 'WCR0544';
           erExp1 = *on;
           msExp1 = 'WCR0544';
           isError = *on;
        Endif;

       // Compensation
        If (scCotr <> *blanks and scCop1 = *blanks) or
         (scCotr = *blanks and scCop1 <> *blanks);
           erCotr = *on;
           msCotr = 'WCR0544';
           erCop1 = *on;
           msCop1 = 'WCR0544';
           isError = *on;
        Endif;

       // Legal
        If (scLetr <> *blanks and scLep1 = *blanks) or
         (scLetr = *blanks and scLep1 <> *blanks);
           erLetr = *on;
           msLetr = 'WCR0544';
           erLep1 = *on;
           msLep1 = 'WCR0544';
           isError = *on;
        Endif;


       // Validate Default Medical Reserve Code and Amount
        If (scDfrc <> *blanks and scDfra = *zeros) or
         (scDfrc = *blanks and scDfra <> *zeros);
           erDfrc = *on;
           msDfrc = 'WCR0544';
           erDfra = *on;
           msDfra = 'WCR0544';
           isError = *on;
        Endif;

       // Validate Default Expense Reserve Code and Amount
        If (scDfxc <> *blanks and scDfxa = *zeros) or
         (scDfxc = *blanks and scDfxa <> *zeros);
           erDfxc = *on;
           msDfxc = 'WCR0544';
           erDfxa = *on;
           msDfxa = 'WCR0544';
           isError = *on;
        Endif;


       // Validate Checkbox - Charge Expense Constant to Emp #1 for PEO Group
        If scPeoe <> *blanks and scPeof = *Blanks;
           erPeoe = *on;
           msPeoe = 'WC31539';
           isError = *on;
        Endif;

       // Validate Checkbox - Calculate Stock Discount Across Entire PEO Group
        If scPeoa <> *blanks and scPeof = *Blanks;
           erPeoa = *on;
           msPeoa = 'WC31539';
           isError = *on;
        Endif;

        //*** Beg Add *** 05/08/24 ****************************
        If scAsof <> *blanks and scPeof = *Blanks;
           erAsofg = *on;
           msAsofg = 'WC31539';
           isError = *on;
        Endif;
        //*** End Add *** 05/08/24 ****************************

        //*** Beg Add ***  04/29/24  *** WC2965 *************************
       // Validate Default Email Address Entered.
        If scEmail <> *blanks;
           erEmail = Check_Email(scEmail:msEmail);
           If erEmail = *On;
              isError = *on;
           EndIf;
        Endif;
        //*** End Add ***  04/29/24  *** WC2965 *************************

       // Test the Check number range
        If scChk1 > scChk2;
           erChk1 = *on;
           msChk1 = 'WCR0559';
           erChk2 = *on;
           msChk2 = 'WCR0559';
           isError = *on;
        Endif;

       // Validate Self-insured Organization Type Code
        If scSior <> *blanks and scSfin <> 'Y';
           erSior = *on;
           msSior = 'WC31348';
           isError = *on;
        Endif;

       // Validate Self-insured Authorization Type Code
        If scSiau <> *blanks and scSfin <> 'Y';
           erSiau = *on;
           msSiau = 'WC31348';
           isError = *on;
        Endif;

       // Validate If EDI Format Identifier is required
        If visbEdif = *on and scEdif = *blanks;
           erEdif = *on;
           msEdif = 'WC31531';
           isError = *on;
        Endif;

       // Due Date for Monthly Interim Payments must be from 1 to 28
        If scMidy < 0 or scMidy > 28;
           erMidy = *on;
           msMidy = 'WC30125';
           isError = *on;
        Endif;

       // Due Date for Weekly Interim Payments must be from 1 to 28
        If scWidy < 0 or scWidy > 28;
           erWidy = *on;
           msWidy = 'WC30125';
           isError = *on;
        Endif;

        //*** Beg Add ***** 05/08/25 ******************************
       // Alternate Company Email Address
        If scACEmlAd <> *Blanks;
           erACEmlAd = Check_Email(scACEmlAd:msACEmlAd);
           If erACEmlAd = *On;
              isError = *on;
           Endif;
        EndIf;

       // Alternate Company Phone #
        If %subst(%editc(scACPhn#:'X'):1:3) <> '000' and
            %subst(%editc(scACPhn#:'X'):4:7) = '0000000';
           erACphn# = *on;
           msACphn# = 'Phone Number required.';
           isError = *on;
        Endif;

        If %subst(%editc(scACPhn#:'X'):1:3) = '000' and
            %subst(%editc(scACPhn#:'X'):4:7) <> '0000000';
           erACphn# = *on;
           msACphn# = 'Area Code required.';
           isError = *on;
        Endif;
        //*** End Add ***** 05/08/25 ******************************

       // Enter complete info for Alternate Name and address,
       // if any of the field is entered
        If scAnam <> *blanks or scAad1 <> *blanks or scAad2 <> *blanks or
         scActy <> *blanks or scAst <> *blanks or scAzip <> 0;
           If scAnam = *Blanks;
              erAnam = *on;
              msAnam = 'WCR0503';
              isError = *on;
           Endif;
           If scAad1 = *Blanks;
              erAad1 = *on;
              msAad1 = 'WCR0503';
              isError = *on;
           Endif;
           If scActy = *blanks;
              erActy = *on;
              msActy = 'WCR0503';
              isError = *on;
           Endif;
           If scAst  = *blanks;
              erAst  = *on;
              msAst  = 'WCR0503';
              isError = *on;
           Endif;
           If scAzip = *zeros;
              erAzip = *on;
              msAzip = 'WCR0556';
              isError = *on;
           Endif;
        Endif;


       // Validate WDF2P file fields - Auxillary file info
        erFein = *off;
        msFein = *blanks;
        erTcc  = *off;
        msTcc  = *blanks;
        erCtyp = *off;
        mgCtyp = *blanks;
        erCtyp2 = *off;
        mgCtyp2 = *blanks;
        //*** Beg Del ***** 04/26/23 *****************************
        //*** erLbnm = *off;
        //*** msLbnm = *blanks;
        //*** End Del ***** 04/26/23 *****************************
        erLad1 = *off;
        msLad1 = *blanks;
        erLcty = *off;
        msLcty = *blanks;
        erLst  = *off;
        msLst  = *blanks;
        erLzip = *off;
        msLzip = *blanks;
        erBlog = *off;
        msBlog = *blanks;
        erCeov = *off;
        mgCeov = *blanks;
        erSpsg = *off;
        mgSpsg = *blanks;
        erFold = *off;
        msFold = *blanks;
        erFold2 = *off;
        msFold2 = *blanks;
        erDcid = *off;
        msDcid = *blanks;
        erSlogo = *off;
        mgSlogo = *blanks;
        erSpath = *off;
        mgSpath = *blanks;
        //*** begin add ***  09/25/24  *************************************
        Clear erSURL;
        Clear mgSURL;
        //*** end add *****  09/25/24  *************************************
        erFysm = *off;
        mgFysm = *blanks;
        erJobq = *off;
        msJobq = *blanks;
        //***  Begin Add ***** 09/18/23 *****************************
        // Variables for Replacement Data to pass in Error Message Description
        rdJobQ = *blanks;
        rdSbs  = *blanks;
        //***  End   Add ***** 09/18/23 *****************************
        erSbs  = *off;
        msSbs  = *blanks;
        erThrl = *off;
        msThrl = *blanks;
        erThrh = *off;
        msThrh = *blanks;
       //***  Beg Add  ***  01/16/24  *** 153WAD ***********************
        erRCXRSN = *off;
        mgRCXRSN = *blanks;
       //***  End Add  ***  01/16/24  *** 153WAD ***********************

       // Federal Tax Id must be greater than 0
        If scFein <= *zeros;
           erFein = *on;
           msFein = 'WCR0502';
           isError = *on;
        Endif;

       // IRS Transmittal Code cannot be blank.
        If scTcc  = *blanks;
           erTcc  = *on;
           msTcc  = 'WCR0503';
           isError = *on;
        Endif;

       // Validate Type of Payment
        If scClck = 'Y' and scCtyp = *Blanks;
           erCtyp = *on;
           mgCtyp = 'Type of Payment required.';
           If clientID = '114';
              erCtyp2 = *on;
              mgCtyp2 = 'Type of Payment required.';
           Endif;
           isError = *on;
        Endif;

       // Enter complete info for Lock Box Name and address,
       // if any of the field is entered
        //*** Beg Chg ***** 04/26/23 *****************************
        //*** If scLbnm <> *blanks or scLad1 <> *blanks or scLad2 <> *blanks or
        //***  scLcty <> *blanks or scLst <> *blanks or scLzip <> 0;
        //***    If scLbnm = *Blanks;
        //***       erLbnm = *on;
        //***       msLbnm = 'WCR0503';
        //***       isError = *on;
        //***    Endif;
        If scLad1 <> *blanks or scLad2 <> *blanks or
         scLcty <> *blanks or scLst <> *blanks or scLzip <> 0;
        //*** End Chg ***** 04/26/23 *****************************
           If scLad1 = *Blanks;
              erLad1 = *on;
              msLad1 = 'WCR0503';
              isError = *on;
           Endif;
           If scLcty = *blanks;
              erLcty = *on;
              msLcty = 'WCR0503';
              isError = *on;
           Endif;
           If scLst  = *blanks;
              erLst  = *on;
              msLst  = 'WCR0503';
              isError = *on;
           Endif;
           If scLzip = *zeros;
              erLzip = *on;
              msLzip = 'WCR0556';
              isError = *on;
           Endif;
        Endif;

       // Billing Logo must be valid page segment object if entered
        If scBlog <> *blanks;
           @@objname = scBlog;
           @@objtype = '*PAGSEG';
           @@err = '0';
           CheckObj(@@objname:@@objtype:@@err);
           If @@err = '1';
              erBlog = *on;
              msBlog = 'WC30292';
              isError = *on;
           Endif;
        Endif;

       // Certification of Insurance Overlay must exist if entered
        If scCeov <> *blanks;
           @@objname = scCeov;
           @@objtype = '*OVL   ';
           @@err = '0';
           CheckObj(@@objname:@@objtype:@@err);
           If @@err = '1';
              erCeov = *on;
              mgCeov = 'Overlay object not found on system.';
              isError = *on;
           Endif;
        Endif;

       // Certification of Insurance Page segment must exist if entered
        If scSpsg <> *blanks;
           @@objname = scSpsg;
           @@objtype = '*PAGSEG';
           @@err = '0';
           CheckObj(@@objname:@@objtype:@@err);
           If @@err = '1';
              erSpsg = *on;
              mgSpsg = 'Page segment not found on system.';
              isError = *on;
           Endif;
        Endif;

       // Folder must be valid if entered and user must have authority to folder
        If scFold <> *blanks;
           @@folder = scFold;
           @@rtcd = ' ';
           ValidFlr(@@folder:@@rtcd);
           If @@rtcd = '1';
              erFold = *on;
              msFold = 'WCR0826';
              isError = *on;
           Endif;
           If @@rtcd = '2';
              erFold2 = *on;
              msFold2 = 'SCM0517';
              isError = *on;
           Endif;
        Endif;

       // Document must be valid if entered
        If scDcid <> *blanks;
           @@docid  = scDcid;
           @@folder = scFold;
           @@rtcd = ' ';
           ValidDoc(@@docid:@@folder:@@rtcd);
           If @@rtcd = '1';
              erDcid = *on;
              msDcid = 'WCR0825';
              isError = *on;
           Endif;
        Endif;

       // Validate Output Logo File Name and Path
        If scSlogo <> *blanks;
           If %scan('.':scSLogo) = 0;
              erSlogo = *on;
              mgSlogo = 'Logo file name must have a file name extension.';
              isError = *on;
           Endif;
        //*** begin add ***  09/25/24  *************************************
         If SCFlg6 <> 'Y';
        //*** begin add ***  09/25/24  *************************************
          // Image path must be entered.
           If scSpath = *blanks;
              erSpath = *on;
              mgSpath = 'Logo image file path must be entered.';
              isError = *on;
           Endif;
          // Path must begin and end with a '/' character.
           If scSpath <> *blanks;
              If %subst(scSpath:1:1) <> '/' OR
               %subst(scSpath:%len(%trim(scSpath)):1) <> '/';
                 erSpath = *on;
                 mgSpath = 'Logo image file path must begin and end ' +
                  'with a ''/''.';
                 isError = *on;
              Endif;
           Endif;
         Else;
           //*** begin add ***  09/25/24  *************************************
           //* Design and Compose enabled.
           If scSURL = ' ';
              mgSURL = 'Logo image URL must be entered.';
              erSURL = *on;
              isError = *on;
           Endif;
           If scSURL <> *blanks and SCFlg6 = 'Y';
              If %subst(scSURL:1:7) <> 'http://' OR
               %subst(scSURL:%len(%trim(scSURL)):1) <> '/';
                 erSURL = *on;
                 mgSURL = 'Logo image URL must begin with http:// and must' +
                  ' end with a ''/''.';
                 isError = *on;
              Endif;
           Endif;
         Endif;
           //*** end add *****  09/25/24  *************************************
       //*** begin chg ***  09/25/24  *************************************
       //*****Endif;
       Else;
          // Do not allow image path/url if no logo.
          If scSPath <> ' ';
             mgSPath = 'Logo image file path must be blank.';
             erSPath = *on;
             isError = *on;
          Endif;
          If scSURL <> ' ';
             mgSURL = 'Logo image URL must be blank.';
             erSURL = *on;
             isError = *on;
          Endif;
       Endif;
       //*** end chg *****  09/25/24  *************************************

       // Validate CEU Fiscal Year Start Month
        If scFysm < 0 or scFysm > 12;
           erFysm = *on;
           mgFysm = 'Value must be 1 thru 12, if populated.';
           isError = *on;
        Endif;

        //***  Beg Add  ***  01/16/24  *** 153WAD ***********************
       // Validate Cancel Reason Code
        If scRnote = 'Y' and scRcxRsn = ' ';
           erRcxRsn = *on;
             mgRcxRsn = 'Select Cancellation Reason Code.';
           isError = *on;
        EndIf;
        //***  End Add  ***  01/16/24  *** 153WAD ***********************

       // Threshold values must be in ascending order, if entered.
        If scThRl > scThRh;
           erThrl = *on;
           msThrl = 'WCR0750';
           erThrh = *on;
           msThrh = 'WCR0750';
           isError = *on;
        Endif;

       // Check for valid JobQ name
        If scJobq <> *blanks;
           @@objname = scJobq;
           @@err = '0';
           CheckObjName(@@objname:@@err);
           If @@err = '1';
              erJobq = *on;
              msJobq = 'WCR3384';
              isError = *on;
           Endif;
        Endif;

       // Subsystem/JobQ are Mutually Inclusive
        If (scJobq <> *blanks and scSbs = *blanks)
         OR (scJobq = *blanks and scSbs <> *blanks);
           erJobq = *on;
           msJobq = 'WCR3381';
           erSbs  = *on;
           msSbs  = 'WCR3381';
           isError = *on;
        Endif;

       // Check existence of Subsystem entered
        If scSbs  <> *blanks;
           @@objname = scSbs ;
           @@objtype = '*SBSD  ';
           @@err = '0';
           CheckObj(@@objname:@@objtype:@@err);
           If @@err = '1';
              erSbs  = *on;
              msSbs  = 'WCR3380';
              isError = *on;
           Endif;
        Endif;

        //***  Begin Add ***** 09/18/23 *****************************
       // Retrieve Object Library
        ##lib = *blanks;
        If scSbs <> *blanks and erSbs = *off;
           ##obj = scSbs;
           ##type = '*SBSD  ';
           RtvObjLib(##obj:##type:##lib);
        Endif;
        //***  End   Add ***** 09/18/23 *****************************

       // Check whether JobQ already assigned to any other subsystem
        //***  Begin Chg ***** 09/18/23 *****************************
        //*** If scJobQ <> *blanks and scSbs <> *blanks;
       // There are 4 scenarios in old program W0010R07, add other 3 now
        // 1. Jobq/Subsystem entered first time at screen.
        If F2jobq = *blanks and scJobQ <> *blanks
         and erJobq = *off and erSbs = *off;
        //***  End   Chg ***** 09/18/23 *****************************
           @@objname = scJobQ;
           @@objtype = '*JOBQ  ';
           @@err = '0';
           //***  Begin Chg ***** 09/18/23 *****************************
           // CheckObj(@@objname:@@objtype:@@err);
           @@lib = ##lib;
           CheckObj2(@@lib:@@objname:@@objtype:@@err);
           //***  End   Chg ***** 09/18/23 *****************************
           If @@err = '0';
              @@jobq = scJobQ;
              //***  Begin Chg ***** 09/18/23 *****************************
              // @@jobqlib = 'QGPL';
              @@jobqlib = ##lib;
              //***  End   Chg ***** 09/18/23 *****************************
              @@jobqsts = ' ';
              @@sbs = ' ';
              @@nojobs = 0;
              RtvJobQA(@@jobq:@@jobqlib:@@jobqsts:@@sbs:@@nojobs);
              // If JobQ already assigned (returned subsystem different from screen)
              If scSbs <> @@Sbs;
                 //***  Begin Add ***** 09/18/23 *****************************
                 rdJobQ = @@Sbs;
                 //***  End   Add ***** 09/18/23 *****************************
                 erJobQ = *on;
                 msJobQ = 'WCR3385';
                 isError = *on;
              Endif;
           Endif;
        Endif;

        //***  Begin Add ***** 09/18/23 *****************************
        // 2. & 3. JobQ/Subsystem is changed at screen.
        If (F2jobq <> *blanks and scJobQ <> F2jobq)
         OR (F2sbs <> *blanks and scSbs <> F2sbs)
         and erJobq = *off and erSbs = *off;
           @@objname = F2jobq;
           @@objtype = '*JOBQ  ';
           @@err = '0';
           @@lib = ##lib;
           // Check whether Old JobQ already exist
           CheckObj2(@@lib:@@objname:@@objtype:@@err);
           If @@err = '0';                       // If Old JobQ already exist.
              @@jobq = F2jobq;
              @@jobqlib = ##lib;
              @@jobqsts = ' ';
              @@sbs = ' ';
              @@nojobs = 0;
              RtvJobQA(@@jobq:@@jobqlib:@@jobqsts:@@sbs:@@nojobs);
              // If JobQ/Subsystem changed and Old JobQ is in use (has any Job).
              If @@nojobs <> 0;
                 rdJobQ = F2jobq;
                 rdSbs  = F2sbs;
                 If scJobQ <> F2jobq;              // Job Changed
                    erJobQ = *on;
                    msJobQ = 'WCR3386';
                    isError = *on;
                 Endif;
                 If scSbs <> F2sbs;                // Subsystem changed.
                    erSbs  = *on;
                    msSbs  = 'WCR3387';
                    isError = *on;
                 Endif;
              Endif;
           Endif;

        // 4. If JobQ changed and Old JobQ not in use
           If scJobQ <> F2jobq
            and erJobq = *off and erSbs = *off;
              @@objname = scJobQ;
              @@objtype = '*JOBQ  ';
              @@err = '0';
              @@lib = ##lib;
              // Check whether New JobQ already exist
              CheckObj2(@@lib:@@objname:@@objtype:@@err);
              If @@err = '0';
                 @@jobq = scJobQ;
                 @@jobqlib = ##lib;
                 @@jobqsts = ' ';
                 @@sbs = ' ';
                 @@nojobs = 0;
                 RtvJobQA(@@jobq:@@jobqlib:@@jobqsts:@@sbs:@@nojobs);
                 // If JobQ already assigned (returned subsystem different from screen)
                 If scSbs <> @@Sbs;
                    rdJobQ = @@Sbs;
                    erJobQ = *on;
                    msJobQ = 'WCR3385';
                   isError = *on;
                 Endif;
              Endif;
           Endif;

        Endif;

        // Call Program to Create JobQ
        If isError = *off;
           // Do not create JobQ at Data Center Clients
           If suffix = '   ';
              outCo = %editc(inCo#:'X');
              outFnd# = %editc(inFnd:'X');
              outJobQ = F2jobq;
              outName = scJobq;
              outSbs  = F2sbs;
              outSbs1 = scSbs;
              outMsg  = ' ';
              outRtcd = ' ';
              crtJobQ(outCo:outFnd#:outJobQ:outName:outSbs:outSbs1:outMsg:
               outRtcd);
              If outRtcd <> ' ';
                 rdJobQ  = outMsg;
                 erJobQ  = *On;
                 msJobQ  = 'WCR3382';
                 isError = *On;
              Endif;
           Endif;
        Endif;

        //***  End   Add ***** 09/18/23 *****************************


       // Validate WMGCP file fields - Group Carrier Information
        erDcar = *off;
        msDcar = *blanks;
        erFrcn = *off;
        msFrcn = *blanks;
        erTocn = *off;
        msTocn = *blanks;

       // Default Carrier Code cannot be blank.
        If scDcar = *blanks;
           erDcar = *on;
           msDcar = 'WCR0503';
           isError = *on;
        Endif;

       // Valid Carrier Number Range must be in ascending order, if entered
        If scFrcn > scTocn;
           erFrcn = *on;
           msFrcn = 'WCR0750';
           erTocn = *on;
           msTocn = 'WCR0750';
           isError = *on;
        Endif;

      // Validate Claim Reserve Code fields
       // Check for Duplicate Claim Reserve Code
       For i = 1 to 9;
          For j = 2 to 10;
             If i<>j and dsScClmRsv(i)<>*blanks and dsScClmRsv(i)=dsScClmRsv(j);
                dsErClmRsv(i) = *on;
                dsErClmRsv(j) = *on;
                dsMsClmRsv(i) = 'WC30122';
                dsMsClmRsv(j) = 'WC30122';
                isError = *on;
             Endif;
          Endfor;
       Endfor;


       // If No Error, for saving in the database file
      // Re-arrange Claim Reserve Codes to Eliminate Blank code
       If isError = *off;
          For i = 1 to 9;
             For j = i+1 to 10;
                If dsScClmRsv(i) = *blanks and dsScClmRsv(j) <> *blanks;
                   dsScClmRsv(i) = dsScClmRsv(j);
                   dsScClmRsv(j) = *blanks;
                Endif;
             Endfor;
          Endfor;
       Endif;

















      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateGroup   e

      //===================================================================
      // Validate the Group Details addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateGrpDtl  b
     d ValidateGrpDtl  pi              n
     d strmode                        1
     d isError2        s               n   inz
      /Free

        isError2 = *off;
       // Initialize all the messageId and error indicators.
        erFyr  = *off;
        msFyr  = *blanks;
        erStrDt = *off;
        msStrDt = *blanks;
        erEndDt = *off;
        msEndDt = *blanks;
        erDps  = *off;
        msDps  = *blanks;
        erNetD = *off;
        msNetD = *blanks;
        erScdt = *off;
        msScdt = *blanks;

        If strmode <> 'C';
          // Policy Year must be > 0.
           If scFyr <= *zero;
              erFyr = *on;
              msFyr = 'WCR0502';
              isError2 = *on;
           Endif;
          // Duplicate Policy Year
           If scFyr <> *zero;
              chain keyWDF1L pf100;
              If %found(wdf1l);
                 erFyr = *on;
                 msFyr = 'WC30004';
                 isError2 = *on;
              Endif;
           Endif;
        Endif;

       // Validation Check for Policy Year Start Date
        erStrDt = ValidDate(scStrDt:charMsg);
        If erStrDt = *on;
           msStrDt = charMsg;
           isError2 = *on;
        Endif;
       // Validation Check for Policy Year End Date
        If scEndDt <> 0;
           erEndDt = ValidDate(scEndDt:charMsg);
           If erEndDt = *on;
              msEndDt = charMsg;
              isError2 = *on;
           Endif;
        Endif;
       // Dates must be in Ascending order
        If scStrDt <> 0 and scEndDt <> 0;
           If erEndDt = *off and erStrDt = *off;
              erEndDt = AscendDates(scStrDt:scEndDt:charMsg);
              If erEndDt = *on;
                 erStrDt = *on;
                 msEndDt = charMsg;
                 msStrDt = charMsg;
                 isError2 = *on;
              Endif;
           Endif;
        Endif;
       // Policy Year Start Date Must Greater than or Equal Group Effective Date
        If scStrDt <> 0 and erStrDt = *off;
           If dtCYMD(scStrDt) < dtCYMD(scEfDta);
              erStrDt = *on;
              msStrDt = 'WC31479';
              isError2 = *on;
           Endif;
        Endif;
       // Policy Year Date Range Must not Overlap with other policy years
        If scStrDt <> 0 and scEndDt <> 0;
           If erEndDt = *off and erStrDt = *off;
              Setll (scCo#:scFnd) pf100;
              Reade (scCo#:scFnd) pf100;
              Dow Not %Eof(wdf1l);
                 If scFyr <> F1fyr and
                  dtCYMD(scStrDt) < F1enpo and
                  dtCYMD(scEndDt) > F1stpo;
                    erEndDt = *on;
                    erStrDt = *on;
                    msEndDt = 'WC31414';
                    msStrDt = 'WC31414';
                    isError2 = *on;
                 Endif;
                 Reade (scCo#:scFnd) pf100;
              Enddo;
           Endif;
        Endif;

       // Validate Deposite Style.
        If scDps = *Blanks;
           erDps = *on;
           msDps = 'WC30341';
           isError2 = *on;
        Endif;

       // Validation Check for Starting with Policies Effective Date
        If scNetD <> 0;
           erNetD = ValidDate(scNetD:charMsg);
           If erNetD = *on;
              msNetD = charMsg;
              isError2 = *on;
           Endif;
        Endif;
       // Starting with Policies Effective Date must be within Policy Year Dates
        If scNetD <> 0 and erNetD = *off;
           If dtCYMD(scNetD) < dtCYMD(scStrDt) OR
            dtCYMD(scNetD) > dtCYMD(scEndDt);
              erNetD = *on;
              msNetD = 'WCR1145';
              isError2 = *on;
           Endif;
        Endif;

       // Validation Check for Stop Collateral Effective Date
        If scScDt <> 0;
           erScDt = ValidDate(scScDt:charMsg);
           If erScDt = *on;
              msScDt = charMsg;
              isError2 = *on;
           Endif;
        Endif;


      // Return *on indicator when No error exists.
        If isError2 = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateGrpDtl  e

      //===================================================================
      // Validate Region Details addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateRegDtl  b
     d ValidateRegDtl  pi              n
     d strmode                        1
     d isError4        s               n   inz
      /Free

        isError4 = *off;
       // Initialize all the messageId and error indicators.
        erRegAE = *off;
        msRegAE = *blanks;
        erRegDesAE = *off;
        msRegDesAE = *blanks;

        If strmode <> 'C';
          // Validate Region#
           If scRegAE <= *zero;
              erRegAE = *on;
              msRegAE = 'WCR0502';
              isError4 = *on;
           Endif;
          // Duplicate Region
           If scRegAE <> *zero;
              inReg = scRegAE;
              chain keyWMREL pre00;
              If %found(wmrel);
                 erRegAE = *on;
                 msRegAE = 'WC30005';
                 isError4 = *on;
              Endif;
           Endif;
        Endif;

       // Validate Region Description
        If scRegDesAE = *Blanks;
           erRegDesAE = *on;
           msRegDesAE = 'WCR0511';
           isError4 = *on;
        Endif;


      // Return *on indicator when No error exists.
        If isError4 = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateRegDtl  e


      // utility procedure definitions
      /COPY SCOPYCOMPI

