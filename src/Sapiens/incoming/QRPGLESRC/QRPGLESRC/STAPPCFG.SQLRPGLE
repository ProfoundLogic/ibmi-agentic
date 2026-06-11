     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
      //*********************************************************************
     h/title STAPPCFG - Application Configuration(Profound)
     h dftname(STAPPCFG)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STAPPCFG': 'SPIEMAIL':'STBNDDIR')
      //*********************************************************************
      //===============================================================*
      //  Created   : 03/12/15                                         *
      //  Programmer: C:65                                             *
      //  Project   : SY251 - Create system level Application Config   *
      //                      program based on the WTAPPCFG.           *
      //                      WTAPPCFG will be changed as a wrapper to *
      //                      call this program.                       *
      //                                                               *
      //  *Special Notes:                                              *
      //                                                               *
      //  06/07/2024: If the row count in the sflItemCur subfile       *
      //  increases, the value of intMaxRows also needs to be changed. *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      // 02/18/25 C:300 Project: SY433 (Profound UI Upgrade)           *
      //                Fix - Getting Decimal Data Error on clicking   *
      //                Add button in Image Logo grid in Output Control tab
      //                                                               *
      // 09/25/24 C:65  Project: SY446 (Design and Compose revamp)     *
      //                Add a checkbox to enable the new Design and    *
      //                Compose processing for interactive Scriptura.  *
      //                File extensions will be hidden and edits       *
      //                disabled for all interactive docs.  This       *
      //                applies to the LiveDoc template and the input  *
      //                template for Interactive Forms - in both       *
      //                Documents tab and interactive flagged process- *
      //                es in Output Control tab.  Finally the logo    *
      //                path will be changed to logo URL for inter-    *
      //                ctive flagged processes in Output Control.     *
      //                                                               *
      // 09/24/24 C:342 Ticket: 117552                                 *
      //                1.Increase top position of Cancel and Save     *
      //                  button, increase height  of panel.           *
      //                DSPF Changes                                   *
      //                1.Increase width of Distribution Type and      *
      //                  and Staff Roles multi-select box on Output   *
      //                  Control screen  and Document screen.         *
      //                2.Increase width of Distribution Type field of *
      //                  rcdSubInfo record format.                    *
      //                3.Onload changes for rcdDocInfo dated          *
      //                  09/24/24.                                    *
      //                                                               *
      // 09/23/24 C:344 Ticket: 117617                                 *
      //                Create new Module Setting container and then   *
      //                position it accordingly on the panel based on  *
      //                whether Scriptura and or the 3rd Party XML     *
      //                Container is present on the Output Control     *
      //                Tab panel.                                     *
      //                                                               *
      // 09/10/24 C:342 Ticket: 116978/WC2826                          *
      //                Filtering of Email Tokens in Application Config*
      //                DSPF Changes                                   *
      //                1. Change database file for SflEmlTokn subfile *
      //                   to load data from STETP file.               *
      //                2. Change database file for sflEmlTok2 subfile *
      //                   to load data from STETP file.               *
      //                                                               *
      // 09/09/24 C:291 Ticket: 117091                                 *
      //                FTP failure directory is only required if      *
      //                threshold information has been entered.        *
      //                Display file change: Removed default value '25'*
      //                from scSize field under record rcdOutput.      *
      //                Fixed Position of module setting that was      *
      //                showing out of tab panel.                      *
      //                                                               *
      // 08/13/24 C:333 Ticket: 117288                                 *
      //                Agent Commission Stmt -Application Config      *
      //                Entering Client template & Path Not Saving     *
      //                                                               *
      // 08/12/24 C:79 Ticket: 99/115952                               *
      //               Fix positioning on Documents grid               *
      //                                                               *
      //07/30/24 C:342 WC2826/115874/116975/116976                     *
      //               Claims Contacts.                                *
      //               DSPF changes:                                   *
      //               1. Added Staff Roles field on Document Tab in   *
      //                  application configuration                    *
      //               1. Changes Dropdown selection criteria for      *
      //                  Policy/Claims/Agency                         *
      //                                                               *
      //  07/16/24 C:340 Project: WC2972                               *
      //                 Add Status Column.                            *
      //                 Add Group Grid.                               *
      //                                                               *
      //  06/19/24 C:344 Support: 115584                               *
      //                 Set the trigger event parm value prior to     *
      //                 executing the GETTRIGGERNAME procedure.       *
      //                                                               *
      // 06/07/24 C:344 Support: 115578                                *
      //                - The number of rows in the Current User Items *
      //                subfile was increased from 9 to 15 to hold the *
      //                newly added items and plan for future growth.  *
      //                - Also fixed issue to clear remaining rows in  *
      //                the Current User Items subfile. Created the    *
      //                intMaxRows field to hold the row count for the *
      //                sflItemCur subfile.                            *
      //
      // 03/04/24 C:342 Project: WC2826                                *
      //                Claims Contacts                                *
      //                DSPF Changes:                                  *
      //                1. Added Staff Roles multi-select box on       *
      //                   rcdOutCfg                                   *
      //                                                               *
      // 03/22/24 C:291 Project: WC2760                                *
      //                Add new parameter to LtrWindow to show Renewal *
      //                Template fields based on Category.             *
      //                Added new field scLtrCols for column headings  *
      //                on Letter Templates grid.                      *
      //                                                               *
      // 01/16/24 C:340 Project: 153WAD                                *
      //                Add field for copy to user folder if not Email *
      //                                                               *
      // 01/10/24 C:340 Project: WC2670                                *
      //                Enable filtering and sorting on all of the     *
      //                grids within the output control setup screens. *
      //                                                               *
      // 09/11/23 C:344 Project: WC2907                                *
      //                Add 'From Email Address' field to Application  *
      //                Configuration display file.                    *
      //                                                               *
      // 08/21/23 C:333 Project: WC2876                                *
      //                Output Control Functionality Enhancements      *
      //                                                               *
      // 07/25/23 C:340 Project: WC2850                                *
      //                Scriptura output Control Default # Copies.     *
      //                                                               *
      // 06/26/23 C:300 Project: WC2910                                *
      //                Add Security / User Access Controls for Group  *
      //                Configuration screens                          *
      //                1.Changed tab name from Search Security to Security
      //                DSPF: Changes in rcdSearch record format       *
      //                2.Add section heading "Search" and "Access Controls"
      //                3.Add new heading and type for Group Configuration
      //                                                               *
      // 06/08/23 C:342 Project: WC2747                                *
      //                1.Display file changes as introduced Dropdown  *
      //                  for required field and removed Checkboxes    *
      //                2.Add copy option in Service Request setup and *
      //                  Maintenance grid.                            *
      //                3.Remove edit for "completion date cannot be   *
      //                  prior to the visit date".                    *
      //                4.Add an edit for "Completion Date cannot be   *
      //                  a future date".                             *
      //                                                               *
      // 05/25/23 C:291 Project: WC2875                                *
      //                Pass Category instead of Category description  *
      //                to GetTokensEmail procedure.                   *
      //                                                               *
      // 05/24/23 C:062 Project: WC2838                                *
      //                Add ftp failure directory.                     *
      //                                                               *
      // 05/18/23 C:291 Project: WC2838                                *
      //                Added grid sflAgtETok on rcdOutCfg. This grid  *
      //                is visible when "FTP" Radio button is clicked. *
      //                                                               *
      // 05/12/23 C:342 Support: 110389                                *
      //                Fixed positioning issue for cancel and save/   *
      //                add next button on Printer setup screen.       *
      //                                                               *
      // 05/10/23 C:291 Support: 108937                                *
      //                Fixed "Email Subject" field tooltip on record  *
      //                rcdDocInfo, rcdSubInfo and rcdOutCfg.          *
      //                                                               *
      // 03/27/23 C:300 Support: 107719 (UBIC)                         *
      //                Issue with SAVE button on output Control tab   *
      //                -- When a user updates the wording for output  *
      //                'Audit Request' it changes an unrelated output *
      //                of 'SR Late Notice' (last record of Grid)      *
      //                                                               *
      // 02/24/23 C:300 Support: 107395 (InHouse)                      *
      //                Not showing Category in Service Request Tracking
      //                grid column                                    *
      //                                                               *
      // 11/29/22 C:300 Support: 104032 (CoveRisk)                     *
      //                In DSPF: in Email Body Section on rcdOutCfg and*
      //                rcdDocInfo record formats -- there is extra "r"*
      //                in "overrride".                                *
      //                                                               *
      // 11/26/22 C:340 Project: WC2838                                *
      //                - Upgrade OutPut Control look and add addition *
      //                  al Email Subject/Body.                       *
      //                                                               *
      // 10/04/22 C:344 Project: WC2838                                *
      //                - Change value of the xxKey field which is     *
      //                  part of the keySTGT key list to the value    *
      //                  of the Document Category (DLCAT) field so    *
      //                  that the correct description value is used   *
      //                  in the GETTOKENSEMAIL procedure call.        *
      //
      // 09/16/22 C:291 Project: 129WXJ                                *
      //                Add Loss Time Flags grid and select box on     *
      //                record rcdAutoLet and rcdAutiCfg.              *
      //                                                               *
      // 06/22/22 C:079 Support: 102638                                *
      //                - Load Action Panel Items using service pgm    *
      //                                                               *
      // 06/20/22 C:333 Support: 102623                                *
      //                - Application Configuration - Output Control   *
      //                  Categories                                   *
      //                - added logic to fetch Descrption from STGTP.  *
      //                    DSPF Changes:                              *
      //                - Changed Output & Category column width.      *
      //                - Increased scCat length from 5 to 40.         *
      //                                                               *
      // 06/15/22 C:33  - Only show Status Description for Claims      *
      //                                                               *
      // 05/09/22 C:079 Project: 129WWV                                *
      //                - Add User Access Security                     *
      //                                                               *
      // 04/11/22 C:333 Project: SY434                                 *
      //                - Update Grids for Sorting Filtering Upgrade   *
      //                                                               *
      // 02/21/22 C:333 Support: 98523 (In-House)                      *
      //               - In Application Configuration Screen>> Document*
      //                 Tab , not able to save mutilple Distribution  *
      //                 Types                                         *
      //                                                               *
      // 09/14/21 C:33  Project: WC2278  #98078                        *
      //               -Update email overrride flags                   *
      //                                                               *
      *  06/21/21 C:298 Project: SY426                                 *
      *                 Re-Brand and Config for Visit Tracking         *
      *                                                                *
      // 05/10/21 C:65  Project: WC2760                                *
      //                Add report template records to replacement     *
      //                templates grid.  If a process has a 1/1/1999   *
      *                 report template record then add checkbox to    *
      *                 add/edit window to allow report templates to   *
      *                 be added.                                      *
      *                                                                *
      *  05/27/21 C:318 Support:95270                                  *
      *                 Changes for inquiry mode to make fields disable*
      *                 and links invisible                            *
      *                                                                *
      *  04/19/21 C:53  Project WC2278
      *                 Changes for contact enhancement
      *                                                                *
      *  03/16/21 C:62  Project: WC2663                                *
      *                 Add QTE3PRTPMT to condition                    *
      *                                                                *
      *  02/19/21 C:65  Project: Phw#94651 (LUBA)                      *
      *                 Correct popup window for replacement docs.     *
      *                                                                *
      *  11/04/19 C:33  Project: 146WJJ                                *
      *                 Add Split Wage fields                          *
      *
      *  10/17/19 C:291 Project: 133WRI                                *
      *                 Add Status Description Box on Rules tab for    *
      *                 Claims under detail box and on Add/Edit Rule   *
      *                 Configuration.                                 *
      *                                                                *
      *  09/27/19  C:295 Support: 82403                                *
      *                  Export to Excel does not work in Rules        *
      *                  tab for auto diaries.Changes in display file. *
      *                                                                *
      *  09/24/19 C:291 Project: 129WVN                                *
      *                 Add "Allow duplicate entries" checkbox on Visit*
      *                 Configuration window and detail element below  *
      *                 the Visit Type grid.                           *
      *                                                                *
      *  05/22/19 C:33  Support: 79577
      *                 Use more global routines
      *                                                                *
      *  05/13/19  C:295 Support: 77937                                *
      *                  Application Configuration / Document -        *
      *                  Filtering by Category Issue.                  *
      *                  Subcatagory values are initialized if catagory*
      *                  is changed in display file.                   *
      *                                                                *
      *  05/10/19  C:300 Support: 77946 (SCS)
      *                  Auto Letters configuration for setting up a NEW
      *                  claim when it is Med Only, Indemnity or Record Only
      *                                                                    *
      *  03/04/19  C:33  Project SY390
      *                  Soft-code 'Portal'
      *                                                                    *
      *  01/30/19  C:33  Support: 76701
      *                  Update frequency record's statu when updating
      *                                                                *
      *  10/02/2018 c:79 Support: 99/70482                             *
      *                  On crtdupobj do not add triggers              *
      *                                                                *
      *  08/23/2018 c:62 Project SY390                                 *
      *                  Add portal menu option                        *
      //                                                               *
      //  08/10/18 C:267 Support: 72338                                *
      //                 Modify RPG statement with regards to subset;  *
      //                 error on starting position of the command.    *
      //                                                               *
      //  08/08/18 C:062 Project: 150WCS                               *
      //                 Show Policy Config menu all the time.         *
      //                                                               *
      //  06/20/18 C:291 Project: WC2600                               *
      //                 Add Allow Inquiry Flags for Claims and policy.*
      //                                                               *
      //  05/14/18 C:291 Project: 150WCR                               *
      //                 Add Policy Configuration option to the menu.  *
      //                                                               *
      //  05/08/18 C:65  Project: SY395                                *
      //                 Add setup to Documents to allow interactive   *
      //                 forms - a new interactive scriptura document  *
      //                 different from LiveDocs.                      *
      //                                                               *
      //  11/16/17 C:65  Project: 102WHK                               *
      //                 Add schema type to document setup - allowing  *
      //                 documents to have different schema types.     *
      //                                                               *
      //  10/30/17 C:33  Project: WC2405                               *
      //                -Allow for multiple selections on trigger      *
      //                  status filter.                               *
      //                -Add digest flag in recipient grids            *
      //                -Include compare information in detail grid    *
      //                -Expand 'When' block                           *
      //                -Add 'Save/Edit Another' to comparisons        *
      //                                                               *
      //  10/26/17 C:65  Project: SY371                                *
      //                 Add secondary signature entry for processes   *
      //                 and redo output fields on display grid to     *
      //                 have path/name concatenated.                  *
      //                                                               *
      //  09/11/17 C:33  Support: 48670                                *
      //                 Acknowledge CRTDUPOBJ not allowed.            *
      //                                                               *
      //  09/06/17 C:33  Project: WC2405                               *
      //                -Account for Inactive triggers                 *
      //                -Add Exit Program as an action                 *
      //                                                               *
      //  08/04/17 C:291 Project: 150WAV                               *
      //                -Add Letter Template Grid on the process setup *
      //                 window on Output Control tab.                 *
      //                                                               *
      //  07/24/17 C:33  Project: WC2405                               *
      //                -Add fields for external locations (Rules)     *
      //                                                               *
      //  06/15/17 C:33  Project: WC2405                               *
      //                -Allow for SQL statement as an action          *
      //                -Allow for Email Digest                        *
      //                                                               *
      //  05/10/17 C:33  Project: WC2405                               *
      //                -Allow for 'WHEN' to be included in trigger    *
      //                -Include status for frequency record           *
      //                -Include field conditionals                    *
      //                                                               *
      //  04/12/17 C:180 Project: MC325                                *
      //                -Add Manage Care Configuration option          *
      //                                                               *
      //  04/10/17 C:136 Project: 129WUL                               *
      //                 Add Compare Date and Compare Operator for     *
      //                 ExpModDate rule                               *
      //                                                               *
      //  04/05/17 C:33  Project: SY388                                *
      //                -Include an alternate Full Page URL            *
      //                -Include tags                                  *
      //                                                               *
      //  03/31/17 C:33  Project: WC2405                               *
      //                -Use pending status for generic rules until    *
      //                  trigger can be created.                      *
      //                -Include field/file descriptions in textual    *
      //                -Seperate Rules from Triggers                  *
      //                -Turn off Textual                              *
      //                -Include file/field descriptions in detail     *
      //                -Include report fields in detail               *
      //                -Trim the screen fields for 'fields'           *
      //                -Add flag for SMS actions                      *
      //                                                               *
      //  03/13/17 C:136 Support: 46111                                *
      //                 Check boxes under Allow User... were          *
      //                 covered up by 3rd part XML container when     *
      //                 no Template Information grid entries exist    *
      //                                                               *
      //  01/31/17 C:33  Project: WC2405                               *
      //                 Add generic file rules                        *
      //                                                               *
      //  12/22/16 C:65  Project: SY371                                *
      //                 Add screen fields for output print settings   *
      //                 on processes plus checkbox for no logo.       *
      //                                                               *
      //  12/16/16 C:33  Project: SY388                                *
      //                 Add Dashboard Configuration                   *
      //                                                               *
      //  12/06/16 C:65  Project: SY371                                *
      //                 Update the save button on the output control  *
      //                 config window to display "Next" instead of    *
      //                 "Save" if there is an additional setup screen.*
      //                 Check new setting flag in STSPP before        *
      //                 displaying the replacement docs grid.  If the *
      //                 is not to be displayed move up subsequent     *
      //                 fields on the screen.  Add grids for logo     *
      //                 and signature setups as well.                 *
      //                                                               *
      //  11/01/16 C:33  Project: WC2405                               *
      //                 Add text representation of Rules              *
      //                                                               *
      //  09/09/16 C:65  Project: SY371                                *
      //                 Move client template fields to the template   *
      //                 grid, which is renamed from replacement       *
      //                 template grid.  Move the secure signature     *
      //                 fields there as well and rearrange the top    *
      //                 portion of the window.  Finally write a       *
      //                 record for the "initial" template if no       *
      //                 records exist in the template file.           *
      //                                                               *
      //  08/08/16 C:65  Project: SY371                                *
      //                 Add more fields to output control edit window.*
      //                                                               *
      //  07/06/16 C:33  Project: WC2405                               *
      //                 Use Auto-Diaries as Rules Engine              *
      //                                                               *
      //  06/28/16 C:65  Project: SY371                                *
      //                 Change subfile # of records on output control *
      //                 to prevent it from only displaying 10 rcds.   *
      //                                                               *
      //  06/10/16 C:65  Project: SY371                                *
      //                 Add more fields to output control edit window.*
      //                                                               *
      //  06/09/16 C:33  Project: 114WKA                               *
      //                -Add group expiration date.                    *
      //                -Allow expired groups to be removed from       *
      //                  selection.                                   *
      //                                                               *
      //  05/24/16 C:302 Project: 114WYS                               *
      //                 Retrospective Settlement changes for Single   *
      //                 Employer Run                                  *
      //                                                               *
      //  04/28/16 C:65  Project: SY371                                *
      //                 Only display active system process records.   *
      //                                                               *
      //  03/01/16 C:62  Project: SY251                                *
      //                 Add Auto Letters                              *
      //                                                               *
      //  02/25/16 C:65  Project: SY251                                *
      //                 Check for duplicate template/live doc setup   *
      //                 based on category/sub category/template or    *
      //                 live doc id/language.                         *
      //                                                               *
      //  01/20/16 C:242 PW38697                                       *
      //                 Add Group Filter                              *
      //                                                               *
      //  11/05/15  C:33  Project: SY371                               *
      //                  Add Output Control                           *
      //                                                               *
      //  10/12/15 C:65  Project: SY251                                *
      //                 Add path field to Documents tab/setup window. *
      //                 Add editing for valid file extensions on      *
      //                 templates and Live Doc ID's.                  *
      //                                                               *
      //  08/19/15 C:65  Project: SY251                                *
      //                 Add printer setup grid to Documents tab.      *
      //                                                               *
      //  05/22/15 C:279 Project: 114WKA                               *
      //                 Add Auto Diary Triggers Module                *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fstappcfg  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflVstTrk:rrnVst)
     f                                     Sfile(sflDocGrid:rrnDoc)
     f                                     Sfile(sflSubGrid:rrnSub)
     f                                     Sfile(sflPrinter:rrnPrin)
     f                                     sfile(sflAutoTrg:rrnAutoTrg)
     f                                     sfile(sflTrigTxt:rrnAutoTrg)
     f                                     sfile(sflTrigGrp:rrnTrigGrp)
     f                                     sfile(sflTrgRcpt:rrnTrgRcpt)
     f                                     sfile(sflTrgJst:rrnTrgJst)
     f                                     sfile(sflTrgInd:rrnTrgInd)
     f                                     sfile(sflTrgNCCI:rrnTrgNCCI)
     f                                     sfile(sflTrgNtrC:rrnTrgNtrC)
     f                                     sfile(sflTrgPrtC:rrnTrgPrtC)
     f                                     sfile(sflTrgCaus:rrnTrgCaus)
     f                                     sfile(sflTrgPmtC:rrnTrgPmtC)
     f                                     sfile(sflTrgRsvC:rrnTrgRsvC)
     f                                     sfile(sflQteSts:rrnQteSts)
     f                                     sfile(sflTrgRcp:rrnTrgRcpt)
     f                                     sfile(sflTrgRcpF:rrnTrgRcpt)
     f                                     sfile(sflRuleAct:rrnActGrd)
     f                                     Sfile(sflOutCtl:rrnOut)
     f                                     sfile(sflTray:rrnTray)
     f                                     Sfile(sflAutlGrp:rrnAutlGrp)
     f                                     Sfile(sflAutlJst:rrnAutlJst)
     f                                     Sfile(sflAutlOff:rrnAutlOff)
     f                                     Sfile(sflAutoLet:rrnAutoLet)
     f                                     sfile(sflReplace:rrnRepl)
     f                                     sfile(sflLogos:rrnLogo)
     f                                     sfile(sflSignat:rrnSig)
     f                                     sfile(sflDashCfg:rrnDashCfg)
     f                                     sfile(sflDashUsr:rrnDashUsr)
     f                                     sfile(sflUsrItem:rrnUsrItem)
     f                                     sfile(sflItemCur:rrnItemCur)
     f                                     sfile(sflItemAvl:rrnItemAvl)
     f                                     sfile(sflFileFld:rrnFileFld)
     f                                     sfile(sflFldRpt:rrnFldRpt)
     f                                     sfile(sflFileCmp:rrnFileCmp)
     f                                     sfile(sflLtrTmpl:rrnLtr)
     f                                     sfile(sflTrgStDs:rrnTrgStDs)
     f                                     sfile(sflBrand:rrnBrand)
     f                                     sfile(sflBGroup:rrnBGroup)
      *** Beg Add ***** 09/16/22 ***************************************
     f                                     Sfile(sflAutlLTF:rrnAutlLTF)
      *** End Add ***** 09/16/22 ***************************************
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     f                                     Sfile(SflGrpSel:rrnGrpSel)
      *** End Add ***** 07/16/24 *** WC2972*****************************
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdSysTbl)
     fsPiEmail  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     rename(QSFLCTL001:QSFLCTL901)
     fstdllcrt  uf a e           k disk    rename(pdl01:pdl0crt)
     f                                     prefix(crt)
      *** Beg Add ***** 07/30/24 ***************************************
     fstdllcrt1 uf a e           k disk    rename(pd101:pd101crt)
     f                                     prefix(crt)
      *** End Add ***** 07/30/24 ***************************************
     fstdepcrt  uf a e           k disk    rename(pde01:pde0crt)
     f                                     prefix(ecr)
     fstsppcrt  uf a e           k disk    rename(psp01:psp0crt)
     f                                     prefix(pcr)
     fstlgl     uf a e           k disk    infds(dbfdsf)
     fstlgp     uf a e             disk    rename(plg01:plg0p)
     fstlglgrp  uf a e           k disk    rename(plg01:plg0grp)
     f                                     prefix(grp)
     fstlglgrp1 uf a e           k disk    rename(plg01:plg1grp)
     f                                     prefix(gr1)
     fstdll     if   e           k disk    rename(pdl01:pdl00)
     f                                     infds(dbfds2)
     fstdll2    if   e           k disk    rename(pdl01:pdl02)
     fstdlp     uf a e             disk    rename(pdl01:pdl0p)
     fstgtl     if a e           k disk    rename(pgt01:pgt00)
     fstscp     uf   e             disk    rename(psc01:psc0p)
     fstdel     if   e           k disk    rename(pde01:pde00)
     f                                     infds(dbfds3)
     fstdep     uf a e             disk    rename(pde01:pde0p)
     fwtvil1    if   e           k disk    rename(pvi01:pvi01)
     f*** Beg Add *** 02/24/23 *************************************************
     f                                     prefix(L_)
     f*** End Add *** 02/24/23 *************************************************
     f                                     infds(dbfds4)
     fwtvip     uf a e             disk    rename(pvi01:pvi0p)
     fsdacp     uf a e             disk    rename(pac01:pac0p)
     fsdacl     if   e           k disk    rename(pac01:pac00)
     f                                     infds(dbfds)
     fsmcol     if   e           k disk    rename(pco01:pco00)
     fstp1l     if   e           k disk    rename(pp101:pp100)
     f                                     infds(dbfds8)
     fstp1p     uf a e             disk    rename(pp101:pp10p)
     fststl1    if   e           k disk
     fstp2l     uf   e           k disk    rename(pp201:pp200)
     f                                     infds(dbfds9)
     fstp2p     uf a e             disk    rename(pp201:pp20p)
     fwtqsl1    if   e           k disk
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     f                                     usropn
      //*** Beg Add *** 08/21/23 ***********************************
     f                                     infds(dbfd14)
      //*** End Add *** 08/21/23 ***********************************
     fstgil     if   e           k disk    rename(pgi01:pgi00)
     fstctl1    if   e           k disk
     fsmtrp     uf a e             disk    rename(ptr01:ptr0p)
     f                                     prefix(m)
     fsmtrl     if   e           k disk    rename(ptr01:ptr00)
     f                                     prefix(m)
     f                                     infds(dbfds5)
     fsmtrpgrp  uf a e           k disk    rename(ptr01:ptr0grp)
     f                                     prefix(g)
     Fsmtrpact  uf a e           k DISK    rename(ptr01:ptr0act)
     f                                     prefix(a)
     Fsmtrptxt  uf a e           k DISK    rename(ptr01:ptr0txt)
     f                                     prefix(x)
     fsmtrprec  uf a e             disk    rename(ptr01:ptr0rec)
     fsmtrlrec  if   e           k disk    rename(ptr01:ptr0rc3)
     f                                     infds(dbfds6)
     fsmtrprc1  uf a e             disk    extfile('QTEMP/SMTRPREC')
     f                                     extdesc('SMTRPREC')
     f                                     rename(ptr01:ptr0rc1)
     f                                     usropn
     fsmtrlrc1  if   e           k disk    extfile('QTEMP/SMTRLREC')
     f                                     extdesc('SMTRLREC')
     f                                     rename(ptr01:ptr0rc2)
     f                                     infds(dbfds7)
     f                                     usropn
     fsmtrpclm  uf a e           k disk    rename(ptr01:ptr0clm)
     f                                     prefix(c)
     fsmtrppol  uf a e           k disk    rename(ptr01:ptr0pol)
     f                                     prefix(p)
     fsmtrptbl  uf a e           k disk    rename(ptr01:ptr0tbl)
     f                                     prefix(t)
     fsmtrpfil  uf a e           k disk    rename(ptr01:ptr0fil)
     f                                     prefix(f)
     fsmtrpfld  uf a e           k disk    rename(ptr01:ptr0fld)
     f                                     prefix(d)
     fsmtrpfrq  uf a e           k disk    rename(ptr01:ptr0frq)
     f                                     prefix(q)
     fsmtrpwhn  uf a e           k disk    rename(ptr01:ptr0whn)
     f                                     prefix(w)
     fsmtrpsql  uf a e           k disk    rename(ptr01:ptr0sql)
     f                                     prefix(sql)
     fsmtrplog  uf a e           k disk    rename(ptr01:ptr0log)
     f                                     prefix(l)
     fwtabl     if   e           k disk    rename(pna01:pna00)
     f                                     rename(ppa01:ppa00)
     f                                     rename(pca01:pca00)
     f                                     rename(pb201:pb200)
     f                                     rename(prt01:prt00)
     f                                     ignore(ptr01)
     fwtgtl     if   e           k disk    rename(pgt01:pgtw0)
     f                                     prefix(w)
     fstspl     if   e           k disk    rename(psp01:psp00)
     f                                     infds(spfds)
     fstspp     uf a e             disk    rename(psp01:psp0p)
     fsaclp     uf a e             disk    rename(pcl01:pcl0p)
     f                                     prefix(m)
     fsacll     if   e           k disk    rename(pcl01:pcl00)
     f                                     prefix(m)
     f                                     infds(dbfdsA)
     fsacll1    if   e           k disk    prefix(m)
     fsacllgrp  uf a e           k disk    rename(pcl01:pcl0grp)
     f                                     prefix(g)
     fsaclljst  uf a e           k disk    rename(pcl01:pcl0jst)
     f                                     prefix(s)
     fsaclloff  uf a e           k disk    rename(pcl01:pcl0off)
     f                                     prefix(o)
     fwmofl     if   e           k disk    rename(pof01:pof00)
     fstsp1l    if   e           k disk    rename(psp011:psp11l)
     f                                     infds(dbfdsB)
     fstsp1p    uf a e             disk    rename(psp011:psp11p)
     f*** Beg Chg *** 08/21/23 *************************************************
     f*stsp2l    if   e           k disk    rename(psp012:psp12l)
     fstsp2l    uf a e           k disk    rename(psp012:psp12l)
     f*** End Chg *** 08/21/23 *************************************************
     f                                     infds(dbfdsC)
     fstsp2p    uf a e             disk    rename(psp012:psp12p)
     f*** Beg Chg *** 08/21/23 *************************************************
     f*stsp3l    if   e           k disk    rename(psp013:psp13l)
     fstsp3l    uf a e           k disk    rename(psp013:psp13l)
     f*** End Chg *** 08/21/23 *************************************************
     f                                     infds(dbfdsD)
     fstsp3p    uf a e             disk    rename(psp013:psp13p)
     fsmdbp     uf a e             disk    rename(pdb01:pdb0p)
     f                                     infds(dbFdsDB)
     fsmdbl     if   e           k disk    rename(pdb01:pdb00)
     f                                     infds(dbFdsDB0)
     f*** Beg Chg *** 08/21/23 *************************************************
     f*stsp4l    if   e           k disk    rename(psp014:psp14l)
     fstsp4l    uf a e           k disk    rename(psp014:psp14l)
     f*** End Chg *** 08/21/23 *************************************************
     f                                     infds(dbfdsE)
     fstsp4p    uf a e             disk    rename(psp014:psp14p)
     fwtvipcfg  uf a e           k disk    rename(pvi01:pvi0p_cfg)
     f                                     prefix(cfg_)
     f*** Beg Add *** 05/09/22 *************************************************
     fsmupl     if   e           k disk
     fsdtbl1    if   e           k disk
     f*** End Add *** 05/09/22 *************************************************
     f*** Beg Add *** 08/21/23 *************************************************
     fwdf2l     if   e           k disk    rename(pf201:pf200)
     f                                     usropn
     f*** End Add *** 08/21/23 *************************************************
     f*** Beg Add *** 03/04/24 *************************************************
     fstsppcrt1 uf a e           k disk    rename(ps101:ps10crt)
     f                                     prefix(pcr1)
     f*** End Add *** 03/04/24 *************************************************
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     fWXGSPDSH  UF A E           K Disk    rename(pgs01:pgs00)
      *** End Add ***** 07/16/24 *** WC2972*****************************

     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     d dbfds4          ds
     d  dbrrn4               397    400b 0
     d dbfds5          ds
     d  dbrrn5               397    400b 0
     d dbfds6          ds
     d  dbrrn6               397    400b 0
     d dbfds7          ds
     d  dbrrn7               397    400b 0
     d dbfds8          ds
     d  dbrrn8               397    400b 0
     d dbfds9          ds
     d  dbrrn9               397    400b 0
     d dbfdsA          ds
     d  dbrrnA               397    400b 0
     d dbfdsB          ds
     d  dbrrnB               397    400b 0
     d dbfdsC          ds
     d  dbrrnC               397    400b 0
     d dbfdsD          ds
     d  dbrrnD               397    400b 0
     d dbFdsDB         ds
     d  dbRrnDB              397    400b 0
     d dbFdsDB0        ds
     d  dbRrnDB0             397    400b 0
     d spfds           ds
     d  sprrn                397    400b 0
     D  spNbrRcds            156    159I 0                                      * Num of records
     ddsMsgInfo        ds                  likeds(dsRtnMsgInfo)
     d dbfdsE          ds
     d  dbrrnE               397    400b 0
     d dbfdsF          ds
     d  dbrrnF               397    400b 0
     f*** Beg Add *** 08/21/23 *************************************************
     d dbfd14          ds
     d  dbrrn14              397    400b 0
     f*** End Add *** 08/21/23 *************************************************
     D
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')

      // constant
      //*** Beg Chg *** 06/26/23 ******************************************
     d*** intArrDim       c                   60
     d intArrDim       c                   70
      //*** Beg Chg *** 06/26/23 ******************************************
     d intFlgDim       c                   10

      // Database file fields for security type
     d                 ds
     d dsSecTypFl              1     10    dim(intFlgDim)
     d ACstp1                  1      1
     d ACstp2                  2      2
     d ACstp3                  3      3
     d ACstp4                  4      4
     d ACstp5                  5      5
     d ACstp6                  6      6
     d ACstp7                  7      7
     d ACstp8                  8      8
     d ACstp9                  9      9
     d ACstp0                 10     10

      // Database file fields for security level
     d                 ds
     d dsSecLvlFl              1     30  0 dim(intFlgDim)
     d ACslv1                  1      3  0
     d ACslv2                  4      6  0
     d ACslv3                  7      9  0
     d ACslv4                 10     12  0
     d ACslv5                 13     15  0
     d ACslv6                 16     18  0
     d ACslv7                 19     21  0
     d ACslv8                 22     24  0
     d ACslv9                 25     27  0
     d ACslv0                 28     30  0

      // Security Type
     d                 ds
      //*** Beg Chg *** 06/26/23 ******************************************
     d*** dsSecTyp                1     60
     d dsSecTyp                1     70
      //*** End Chg *** 06/26/23 ******************************************
     d                                     dim(intArrDim)
     d  scClInqTp1             1      1
     d  scClInqTp2             2      2
     d  scClInqTp3             3      3
     d  scClInqTp4             4      4
     d  scClInqTp5             5      5
     d  scClInqTp6             6      6
     d  scClInqTp7             7      7
     d  scClInqTp8             8      8
     d  scClInqTp9             9      9
     d  scClInqTp0            10     10
     d  scClUpdTp1            11     11
     d  scClUpdTp2            12     12
     d  scClUpdTp3            13     13
     d  scClUpdTp4            14     14
     d  scClUpdTp5            15     15
     d  scClUpdTp6            16     16
     d  scClUpdTp7            17     17
     d  scClUpdTp8            18     18
     d  scClUpdTp9            19     19
     d  scClUpdTp0            20     20
     d  scPlInqTp1            21     21
     d  scPlInqTp2            22     22
     d  scPlInqTp3            23     23
     d  scPlInqTp4            24     24
     d  scPlInqTp5            25     25
     d  scPlInqTp6            26     26
     d  scPlInqTp7            27     27
     d  scPlInqTp8            28     28
     d  scPlInqTp9            29     29
     d  scPlInqTp0            30     30
     d  scPlUpdTp1            31     31
     d  scPlUpdTp2            32     32
     d  scPlUpdTp3            33     33
     d  scPlUpdTp4            34     34
     d  scPlUpdTp5            35     35
     d  scPlUpdTp6            36     36
     d  scPlUpdTp7            37     37
     d  scPlUpdTp8            38     38
     d  scPlUpdTp9            39     39
     d  scPlUpdTp0            40     40
     d  scQtInqTp1            41     41
     d  scQtInqTp2            42     42
     d  scQtInqTp3            43     43
     d  scQtInqTp4            44     44
     d  scQtInqTp5            45     45
     d  scQtInqTp6            46     46
     d  scQtInqTp7            47     47
     d  scQtInqTp8            48     48
     d  scQtInqTp9            49     49
     d  scQtInqTp0            50     50
     d  scQtUpdTp1            51     51
     d  scQtUpdTp2            52     52
     d  scQtUpdTp3            53     53
     d  scQtUpdTp4            54     54
     d  scQtUpdTp5            55     55
     d  scQtUpdTp6            56     56
     d  scQtUpdTp7            57     57
     d  scQtUpdTp8            58     58
     d  scQtUpdTp9            59     59
     d  scQtUpdTp0            60     60
      //*** Beg Add *** 06/26/23 ******************************************
     d  scGrpCgTp1            61     61
     d  scGrpCgTp2            62     62
     d  scGrpCgTp3            63     63
     d  scGrpCgTp4            64     64
     d  scGrpCgTp5            65     65
     d  scGrpCgTp6            66     66
     d  scGrpCgTp7            67     67
     d  scGrpCgTp8            68     68
     d  scGrpCgTp9            69     69
     d  scGrpCgTp0            70     70
      //*** End Add *** 06/26/23 ******************************************

      // Security Level
     d                 ds
      //*** Beg Chg *** 06/26/23 ******************************************
     d*** dsSecLvl                1    180  0
     d dsSecLvl                1    210  0
      //*** End Chg *** 06/26/23 ******************************************
     d                                     dim(intArrDim)
     d  scClInqLv1             1      3  0
     d  scClInqLv2             4      6  0
     d  scClInqLv3             7      9  0
     d  scClInqLv4            10     12  0
     d  scClInqLv5            13     15  0
     d  scClInqLv6            16     18  0
     d  scClInqLv7            19     21  0
     d  scClInqLv8            22     24  0
     d  scClInqLv9            25     27  0
     d  scClInqLv0            28     30  0
     d  scClUpdLv1            31     33  0
     d  scClUpdLv2            34     36  0
     d  scClUpdLv3            37     39  0
     d  scClUpdLv4            40     42  0
     d  scClUpdLv5            43     45  0
     d  scClUpdLv6            46     48  0
     d  scClUpdLv7            49     51  0
     d  scClUpdLv8            52     54  0
     d  scClUpdLv9            55     57  0
     d  scClUpdLv0            58     60  0
     d  scPlInqLv1            61     63  0
     d  scPlInqLv2            64     66  0
     d  scPlInqLv3            67     69  0
     d  scPlInqLv4            70     72  0
     d  scPlInqLv5            73     75  0
     d  scPlInqLv6            76     78  0
     d  scPlInqLv7            79     81  0
     d  scPlInqLv8            82     84  0
     d  scPlInqLv9            85     87  0
     d  scPlInqLv0            88     90  0
     d  scPlUpdLv1            91     93  0
     d  scPlUpdLv2            94     96  0
     d  scPlUpdLv3            97     99  0
     d  scPlUpdLv4           100    102  0
     d  scPlUpdLv5           103    105  0
     d  scPlUpdLv6           106    108  0
     d  scPlUpdLv7           109    111  0
     d  scPlUpdLv8           112    114  0
     d  scPlUpdLv9           115    117  0
     d  scPlUpdLv0           118    120  0
     d  scQtInqLv1           121    123  0
     d  scQtInqLv2           124    126  0
     d  scQtInqLv3           127    129  0
     d  scQtInqLv4           130    132  0
     d  scQtInqLv5           133    135  0
     d  scQtInqLv6           136    138  0
     d  scQtInqLv7           139    141  0
     d  scQtInqLv8           142    144  0
     d  scQtInqLv9           145    147  0
     d  scQtInqLv0           148    150  0
     d  scQtUpdLv1           151    153  0
     d  scQtUpdLv2           154    156  0
     d  scQtUpdLv3           157    159  0
     d  scQtUpdLv4           160    162  0
     d  scQtUpdLv5           163    165  0
     d  scQtUpdLv6           166    168  0
     d  scQtUpdLv7           169    171  0
     d  scQtUpdLv8           172    174  0
     d  scQtUpdLv9           175    177  0
     d  scQtUpdLv0           178    180  0
      //*** Beg Add *** 06/26/23 ******************************************
     d  scGrpCgLv1           181    183  0
     d  scGrpCgLv2           184    186  0
     d  scGrpCgLv3           187    189  0
     d  scGrpCgLv4           190    192  0
     d  scGrpCgLv5           193    195  0
     d  scGrpCgLv6           196    198  0
     d  scGrpCgLv7           199    201  0
     d  scGrpCgLv8           202    204  0
     d  scGrpCgLv9           205    207  0
     d  scGrpCgLv0           208    210  0
      //*** End Add *** 06/26/23 ******************************************

      // Security Type Error Indicator
     d                 ds
      //*** Beg Chg *** 06/26/23 ******************************************
     d*** dsSecTypEr              1     60
     d dsSecTypEr              1     70
      //*** End Chg *** 06/26/23 ******************************************
     d                                     dim(intArrDim)
     d  erClInqTp1             1      1n
     d  erClInqTp2             2      2n
     d  erClInqTp3             3      3n
     d  erClInqTp4             4      4n
     d  erClInqTp5             5      5n
     d  erClInqTp6             6      6n
     d  erClInqTp7             7      7n
     d  erClInqTp8             8      8n
     d  erClInqTp9             9      9n
     d  erClInqTp0            10     10n
     d  erClUpdTp1            11     11n
     d  erClUpdTp2            12     12n
     d  erClUpdTp3            13     13n
     d  erClUpdTp4            14     14n
     d  erClUpdTp5            15     15n
     d  erClUpdTp6            16     16n
     d  erClUpdTp7            17     17n
     d  erClUpdTp8            18     18n
     d  erClUpdTp9            19     19n
     d  erClUpdTp0            20     20n
     d  erPlInqTp1            21     21n
     d  erPlInqTp2            22     22n
     d  erPlInqTp3            23     23n
     d  erPlInqTp4            24     24n
     d  erPlInqTp5            25     25n
     d  erPlInqTp6            26     26n
     d  erPlInqTp7            27     27n
     d  erPlInqTp8            28     28n
     d  erPlInqTp9            29     29n
     d  erPlInqTp0            30     30n
     d  erPlUpdTp1            31     31n
     d  erPlUpdTp2            32     32n
     d  erPlUpdTp3            33     33n
     d  erPlUpdTp4            34     34n
     d  erPlUpdTp5            35     35n
     d  erPlUpdTp6            36     36n
     d  erPlUpdTp7            37     37n
     d  erPlUpdTp8            38     38n
     d  erPlUpdTp9            39     39n
     d  erPlUpdTp0            40     40n
     d  erQtInqTp1            41     41n
     d  erQtInqTp2            42     42n
     d  erQtInqTp3            43     43n
     d  erQtInqTp4            44     44n
     d  erQtInqTp5            45     45n
     d  erQtInqTp6            46     46n
     d  erQtInqTp7            47     47n
     d  erQtInqTp8            48     48n
     d  erQtInqTp9            49     49n
     d  erQtInqTp0            50     50n
     d  erQtUpdTp1            51     51n
     d  erQtUpdTp2            52     52n
     d  erQtUpdTp3            53     53n
     d  erQtUpdTp4            54     54n
     d  erQtUpdTp5            55     55n
     d  erQtUpdTp6            56     56n
     d  erQtUpdTp7            57     57n
     d  erQtUpdTp8            58     58n
     d  erQtUpdTp9            59     59n
     d  erQtUpdTp0            60     60n
      //*** Beg Add *** 06/26/23 ******************************************
     d  erGrpCgTp1            61     61n
     d  erGrpCgTp2            62     62n
     d  erGrpCgTp3            63     63n
     d  erGrpCgTp4            64     64n
     d  erGrpCgTp5            65     65n
     d  erGrpCgTp6            66     66n
     d  erGrpCgTp7            67     67n
     d  erGrpCgTp8            68     68n
     d  erGrpCgTp9            69     69n
     d  erGrpCgTp0            70     70n
      //*** End Add *** 06/26/23 ******************************************

      // Security Level Error Indicator
     d                 ds
      //*** Beg Chg *** 06/26/23 ******************************************
     d*** dsSecLvlEr              1     60
     d dsSecLvlEr              1     70
      //*** End Chg *** 06/26/23 ******************************************
     d                                     dim(intArrDim)
     d  erClInqLv1             1      1n
     d  erClInqLv2             2      2n
     d  erClInqLv3             3      3n
     d  erClInqLv4             4      4n
     d  erClInqLv5             5      5n
     d  erClInqLv6             6      6n
     d  erClInqLv7             7      7n
     d  erClInqLv8             8      8n
     d  erClInqLv9             9      9n
     d  erClInqLv0            10     10n
     d  erClUpdLv1            11     11n
     d  erClUpdLv2            12     12n
     d  erClUpdLv3            13     13n
     d  erClUpdLv4            14     14n
     d  erClUpdLv5            15     15n
     d  erClUpdLv6            16     16n
     d  erClUpdLv7            17     17n
     d  erClUpdLv8            18     18n
     d  erClUpdLv9            19     19n
     d  erClUpdLv0            20     20n
     d  erPlInqLv1            21     21n
     d  erPlInqLv2            22     22n
     d  erPlInqLv3            23     23n
     d  erPlInqLv4            24     24n
     d  erPlInqLv5            25     25n
     d  erPlInqLv6            26     26n
     d  erPlInqLv7            27     27n
     d  erPlInqLv8            28     28n
     d  erPlInqLv9            29     29n
     d  erPlInqLv0            30     30n
     d  erPlUpdLv1            31     31n
     d  erPlUpdLv2            32     32n
     d  erPlUpdLv3            33     33n
     d  erPlUpdLv4            34     34n
     d  erPlUpdLv5            35     35n
     d  erPlUpdLv6            36     36n
     d  erPlUpdLv7            37     37n
     d  erPlUpdLv8            38     38n
     d  erPlUpdLv9            39     39n
     d  erPlUpdLv0            40     40n
     d  erQtInqLv1            41     41n
     d  erQtInqLv2            42     42n
     d  erQtInqLv3            43     43n
     d  erQtInqLv4            44     44n
     d  erQtInqLv5            45     45n
     d  erQtInqLv6            46     46n
     d  erQtInqLv7            47     47n
     d  erQtInqLv8            48     48n
     d  erQtInqLv9            49     49n
     d  erQtInqLv0            50     50n
     d  erQtUpdLv1            51     51n
     d  erQtUpdLv2            52     52n
     d  erQtUpdLv3            53     53n
     d  erQtUpdLv4            54     54n
     d  erQtUpdLv5            55     55n
     d  erQtUpdLv6            56     56n
     d  erQtUpdLv7            57     57n
     d  erQtUpdLv8            58     58n
     d  erQtUpdLv9            59     59n
     d  erQtUpdLv0            60     60n
      //*** Beg Add *** 06/26/23 ******************************************
     d  erGrpCgLv1            61     61n
     d  erGrpCgLv2            62     62n
     d  erGrpCgLv3            63     63n
     d  erGrpCgLv4            64     64n
     d  erGrpCgLv5            65     65n
     d  erGrpCgLv6            66     66n
     d  erGrpCgLv7            67     67n
     d  erGrpCgLv8            68     68n
     d  erGrpCgLv9            69     69n
     d  erGrpCgLv0            70     70n
      //*** End Add *** 06/26/23 ******************************************

      // work variables
     d clientID        s              3    inz('000')
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d error           s               n   inz(*off)
     d isRetroI        s               n   inz(*off)
     D hasReport       s               n
     d flgRcdUpdated   s              1
     d strMsg          s            200
     d strSlash        s              1
     d strSlash2       s              1
     d intCount        s              3  0
     d x               s              2  0
     d intRrn          s              9  0
     d intRrn1         s              9  0
     d intRrn2         s              9  0
     d intRrn3         s              9  0
     d intRrn4         s              9  0
     d intRrn5         s              9  0
      //*** Beg Add *** 06/26/23 ******************************************
     d intRrn6         s              9  0
      //*** End Add *** 06/26/23 ******************************************
     d intCo#          s              3  0
     d intStart        s              3  0
     d isDigestDone    s              1n   inz('1')
     d isCompareDone   s              1n   inz('1')
     d isPendChg       s              1n
     d isRulesEng      s              1n
     d isRulesDone     s              1n   inz('1')
     d isTextualDone   s              1n   inz('0')
     disUseDashConfig  s              1n   inz('0')
     d isUseScrp       s              1n   inz('0')
     d isUse3rd        s              1n   inz('0')
     D stateKey        s                   like(STkey)
     D indexTab        s              5  0
     D category        s                   like(mCLcat)
     D subCategory     s                   like(mCLsubC)
     D documentID      s                   like(mCLdcid)
     D language        s                   like(mCLlang)
     d strKey          s             10
     d strName         s             40
     d strTrgEvent     s                   like(qTRtrgEvent)
     d strField        s                   like(dTRfield)
     d strFile         s                   like(scFile)
     d strFltDlg       s                   like(scFltDlg)
     d strFltLbl       s                   like(scFltLbl)
     d strLblCtl       s                   like(scLblCtl)
     d strLblHdg       s                   like(scLblHdg)
     d strLblSetup     s                   like(scLblSetup)
     d strRuleHdg      s                   like(scRuleHdg)
     d strRuleHdgs     s                   like(scRuleHdgs)
     d strRuleTrg      s             50
     d process         s               n   inz(*off)
     d saveDT          s                   like(ACdt)
     d saveTM          s                   like(ACtime)
     d saveDT1         s                   like(ACdt)
     d saveTM1         s                   like(ACtime)
     d saveDT2         s                   like(ACdt)
     d saveTM2         s                   like(ACtime)
     d saveDT3         s                   like(ACdt)
     d saveTM3         s                   like(ACtime)
     d saveDT4         s                   like(ACdt)
     d saveTM4         s                   like(ACtime)
     d saveDT5         s                   like(ACdt)
     d saveTM5         s                   like(ACtime)
      //*** Beg Add *** 06/26/23 ******************************************
     d saveDT6         s                   like(ACdt)
     d saveTM6         s                   like(ACtime)
      //*** End Add *** 06/26/23 ******************************************
     D xxKey           s                   like(GTKey)
     D xxTbid          s                   like(GTTbid)
     D xxCat           s                   like(DECat)
     D xxSubC          s                   like(DESubC)
     D xxBegPos        s              3  0
     d svChgEvent      s                   like(scChgEvent)
     d svFieldChg      s                   like(scFieldChg)
     d svFieldRpt      s                   like(scFieldRpt)
     D svFiltCatg      s                   like(scFiltCatg)
     D svFiltDept      s                   like(scFiltDept)
     D svFiltStat      s                   like(scFiltStat)
     D svFiltEmai      s                   like(scFiltEmai)
     D strSelDistT     s            525
     D conDist         C                   CONST('Distribution')
     D distArr         s             50    DIM(30)
       //*** begin Del ***  02/21/22  ***********************************
       //*** end Del *****  02/21/22  ***********************************
     D svFiltIntr      s                   like(scFiltIntr)
     D svFiltLang      s                   like(scFiltLang)
     D svFiltAuto      s                   like(scFiltAuto)
     D svSubFilCt      s                   like(scSubFilCt)
     D svSubFilSt      s                   like(scSubFilSt)
     d svWhen          s                   like(scWhen)
     d saveLang        s                   like(DLLang)
     d scrituraFlag    s              1    inz(' ')
     d index           s              3  0 inz(1)
      //
     D namesArr        s            100    DIM(20)
     d visitFilter     s             10    INZ
     d fltrVstTyp      s                   like(scVstTyp)
     d fltrVstCat      s                   like(scVstCat)
     d fltrVstSts      s                   like(scVstSts)
     d fltrType        s                   like(ssType)
     d fltrSplO        s                   like(ssSplO)
     d fltrSplR        s                   like(ssSplR)
     d fltrScrO        s                   like(ssScrO)
     d fltr3Xml        s                   like(ss3Xml)
     D ##Wcif          s              1
     D ##Mcif          s              1
     D ##Wcin          s              1
     d intRecCnt       s              5  0 inz
     d intRecCntE      s              5  0 inz
     d intRecCntI      s              5  0 inz
     d intTmpCo#       s              3  0 inz(0)
     d intTmpSeq       s              5  0 inz(0)
     d flgTrgMode      s              1    inz
     D flgAutMode      s              1    inz
     d flgRecMode      s              1    inz
     d strCommand      s            255
     d strTRName       s             10    inz(' ')
     D strState        s              2    inz
     d fltrTrig        s                   like(scFiltTrg)
     d fltrTrgSts      s                   like(scFltTrgSt)
     d fltrTrgRl       s                   like(scFiltRole)
     d fltrTrgUsr      s                   like(scFiltUser)
     d #ofTrgJst       s              2  0
     d fltrTrgGrp      s                   like(scFltTrgGr)
     d*** Beg Add *** 05/09/22 *************************************************
       dcl-s  inDspFileName char(10);
       dcl-s  inPgmName     char(50);
       dcl-s  inTabName     char(50);
     d*** End Add *** 05/09/22 *************************************************
     d*** Beg Add *** 08/21/23 *************************************************
       dcl-s  inData char(500) Inz;
       dcl-s xxrrn packed(9:0);
       dcl-s inLeft packed(5:0)Inz(290);
       dcl-s inTop packed(5:0)Inz(210);
     d*** End Add *** 08/21/23 *************************************************
     d*** Beg Add *** 06/07/24 *** PHW115578 ***********************************
       dcl-s  intMaxRows zoned(2:0);
     d*** End Add *** 06/07/24 *** PHW115578 ***********************************
     ddsEmail        e ds                  extname(sPiEmail:RcdEmails)
     dptrEmail         s               *   inz
     d*** Beg Add *** 06/22/22 ************************************************
     d strMenuChoi     s            500    Varying
     d strMenuValu     s            100    Varying
     d*** End Add *** 06/22/22 ************************************************
      //*** Beg Add ***** 09/16/22 ************************
     d intStrPos       s              3  0
     d strLstFlg       s              1
      //*** End Add ***** 09/16/22 ************************
     d*** Begin Add *** 05/24/23 *******************************************
     d ChkIfsObj       pr                  extPgm('CHKIFSOBJ')
     d  ifsObjName                  256    const
     d  ifsObjExists                   n

     d  ifsObjName     s            256a
     d  ifsObjExists   s               n
     d*** End   Add *** 05/24/23 *******************************************
      //*** Beg Add ***** 06/08/23 ************************
     d isCopyMode      s               n   inz(*off)
      //*** End Add ***** 06/08/23 ************************
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     d intSelGrp       s              3  0 inz
      *** End Add ***** 07/16/24 *** WC2972 ****************************

     D                 ds
     D  Lang                   1    800
     D                                     DIM(20)
     D  SCLang01               1     40
     D  SCLang02              41     80
     D  SCLang03              81    120
     D  SCLang04             121    160
     D  SCLang05             161    200
     D  SCLang06             201    240
     D  SCLang07             241    280
     D  SCLang08             281    320
     D  SCLang09             321    360
     D  SCLang10             361    400
     D  SCLang11             401    440
     D  SCLang12             441    480
     D  SCLang13             481    520
     D  SCLang14             521    560
     D  SCLang15             561    600
     D  SCLang16             601    640
     D  SCLang17             641    680
     D  SCLang18             681    720
     D  SCLang19             721    760
     D  SCLang20             761    800

     d strTypes        s             25
     d strCat          s             25
     d Count           s              5  0
     d sqlQuery        s            300
      // Proto-types
      //*** Beg Add ***** 03/04/24 ************************
     d strStaffAsn     s             19
     d strSqlCursor    s           1000
     d strSelStafR     s           1024
     d conStaf         c                   CONST('Staff Roles')
     d Count1          s              3  0
      //*** End Add ***** 03/04/24 ************************
      //*** Beg Add ***** 07/30/24 ************************
     d PolicyCh        s           1000
     d ClaimCh         s           1000
     d AgencyCh        s           1000
      //*** End Add ***** 07/30/24 ************************
     d*** begin add ***  09/25/24  ********************************************
     d docName         s            100
     d*** end add *****  09/25/24  ********************************************

      // Clear Error Indicator
     d ClearError      pr

      // Edit Check
     d EditCheck       pr

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     DCheckObjInLIBL   pr                  extPgm('SRCHKOBJ')
     D outObject                     10    const
     D outObjectType                  7    const
     D inError                        1

     DRtvObjectLib     pr                  extPgm('RTVOBJLIB')
     D outObject                     10    const
     D outObjectType                  7    const
     D inLib                         10

      // Load Application Configuration info
     d LoadConfInfo    pr

       // Load Dashboard Tab
     d LoadDashboard   pr

      // Load Data
     d LoadData        pr
     d intStart                            like(intStart)

      // Load Documents tab
     d LoadDocuments   pr

      // Clear Auto Diary Trigger Filter
     d ClearTrgFilter  pr

     d GetSystem       pr                  extpgm('SA001R')
     d ##Wcif                         1
     d ##Mcif                         1
     d ##Wcin                         1

      // Get Fund name
     d GetFndName      pr            60
     d intGrp##                       3  0
     d expDate                         d   options(*nopass)

     DBuildClmTypText  pr                  like(xTRcond)
     D inClmTypes                          like(cTRclmType) const

     DBuildPolStsText  pr            15
     D inPolSts                            like(pTRpolStsTp) const

     DBuildRulesText   pr                  like(xTRcond)
     D inString                            like(xTRcond) const
     D inPrefix                     100                  const
     D inConjunction                 10                  const
     D inValue                             like(xTRcond) const

     DBuildTableText   pr                  like(xTRcond)
     D inTable                             like(tTRtable) const
     D inPrefix                     100                  const

      // Load Claim Trigger control
     d LoadClmInfo     pr

      // Load file trigger control Info
     dLoadFileInfo     pr
     d inMode                        10    const

      // Load Policy trigger control
     d LoadPolInfo     pr

      // Load Auto trigger grid
     d LoadTrgGrid     pr

      // Load Recipient grid
     d LoadRecipient   pr

      // Load External role choices
     D LoadExtRoles    pr
     d  inType                        1    const
     d  outRecCh                           like(scRecCh)
     d  outRecChVal                        like(scRecChVal)

     dPendTrigNotice   pr
     d inType                         1    const options(*nopass)

      // Load Recipient add/edit
     d ProcRecpt       pr

     dSaveRuleText     pr

      // Load Common elements for auto trigger
     d LoadTrgDtl      pr

      // Check for filter
     d MeetFilter      pr              n

      // Procee Add/Edit Trigger
     d ProcAddEdt      pr

     d ProcDashCfg     pr
     d  inMode                        1    const
     d  inRRN                         9  0 const options(*nopass)

     d ProcDashUsr     pr
     d  inMode                        1    const
     d  inRole                       25    const options(*nopass)
     d ProcBrandCfg    pr
     d  inMode                        1    const
     d  inRRN                         9  0 const options(*nopass)

     d QCmdExc         pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const

      // Load Auto Diary Trigger Filter
     d SetTrgFilter    pr

      // Add Document Setup record
     d DocWindow       pr
     D inDocRRN                       9  0 const options(*nopass)

      // Add Sub Category Setup record
     d SubWindow       pr
     D inSubRRN                       9  0 const options(*nopass)

      // Add Printer Setup record
     d PrtWindow       pr
     D inSubRRN                       9  0 const options(*nopass)

      // Add Printer Tray record
     d TrayWindow      pr
     D inPrtKey                      10    const
     D inTrayRRN                      9  0 const options(*nopass)

     D strPgm          s             10

     d ExtraSetupPgm   pr                  ExtPgm(strPgm)
     d   inCancel                     1

      // Replacement Doc record
     d ReplaceWindow   pr
     D inReplType                   100    const
     D inReplInter                    1    const
     D inReplRRN                      9  0 const options(*nopass)
      //*** Beg Add ***** 08/21/23 *********************************
     D inCopyMode                      n   const options(*nopass)
      //*** End Add ***** 08/21/23 *********************************
     //
     DValidDate        PR             1
     D date                           8  0
     D messageID                    100
     //
      // Logo record
     d LogoWindow      pr
     D inLogoType                   100    const
     d*** begin add ***  09/25/24  ********************************************
     D inLogoInter                    1    const
     d*** end add *****  09/25/24  ********************************************
     D inLogoRRN                      9  0 const options(*nopass)
      //*** Beg Add ***** 08/21/23 *********************************
     D inCopyMode                      n   const options(*nopass)
      //*** End Add ***** 08/21/23 *********************************
      // Signature record
     d SigWindow       pr
     D inSigType                    100    const
     D inSigRRN                       9  0 const options(*nopass)
      //*** Beg Add ***** 08/21/23 *********************************
     D inCopyMode                      n   const options(*nopass)
      //*** End Add ***** 08/21/23 *********************************

      // Signature record
     d LtrWindow       pr
     D inLtrType                    100    const
      //*** Beg Add ***** 03/22/24  ********************************
     D inLtrCat                       5    const
      //*** End Add ***** 03/22/24  ********************************
     D inLtrRRN                       9  0 const options(*nopass)
      //*** Beg Add ***** 08/21/23 *********************************
     D inCopyMode                      n   const options(*nopass)
      //*** End Add ***** 08/21/23 *********************************

      // System Flag API
     d GetSystemFlag   pr                  extpgm('SA047R')
     d   outSysFlag                   1

      // Configre (Add/Edit) Visit Type record
     d CfgVisitType    pr
     D inVstRRN                       9  0 const options(*nopass)

      // Clear Visit Typer Filter
     d ClearVstFilter  pr

      // Load Visit Tracking tab
     d LoadVisit       pr

      // Load Visit Typer Filter
     d SetVstFilter    pr

      // Configre (Edit) Output Control record
     d CfgOutputCtl    pr
     D inOutRRN                       9  0 const options(*nopass)

      // Clear Output Control Filter
     d ClearOutFilter  pr

      // Load Output Control tab
     d LoadOutput      pr

      // Load Output Control Filter
     d SetOutFilter    pr
      // Check for auto letter filter
     d ChkDupAutoLet   pr              n
     d  AutoLetSeq#                   9  0

      // Clear Auto Letter Trigger Filter
     d ClearAutFilter  pr

     dCreateWorkfile   pr             1n
     d inFile                        10    const
     d inClrFile                      1n   const options(*nopass)

      // Load Grid for auto letter trigger
     d LoadAutoLet     pr

      // Load Common elements for auto letter trigger
     d LoadAutLetDtl   pr

      // Check for auto letter filter
     d FiltAutoLet     pr              n

     d GetExtRoleDesc  pr            50
     d  inRole                       10

     d GetRecipDesc    pr            50
     d  inRole                             like(TRrecip)
     d  inType                             like(TRtype)

      // Procee Add/Edit Trigger
     d ProcAutlAddEdt  pr

      // Load Auto Diary Trigger Filter
     d SetAutlFilter   pr

      // Main Processing
     d Main            pr

      // Program Start
     d ProgramStart    pr
     d processMe                       n

      // Save Configuration setup
     d SaveDetail      pr

      // Save Data
     d SaveData        pr
     d intStart                            like(intStart)

     d DftFltrStat     pr
      //*** Beg Add ***** 08/21/23 *******************************
      *
     d GetCarrier#     pr             5s 0
     d infnd                               like(f2fnd)
      *
      //*** End Add ***** 08/21/23 *******************************
     d*** begin add ***  09/25/24  ********************************************
     DListDocs         PR             1n
     D inDoc                        100    const
     D outID                        100    options(*nopass)
     d*** end add *****  09/25/24  ********************************************

     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDashBrd
     d/copy sPrDate
     d/copy sPrFiles
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
     d/copy sPrEmail
     d*** Beg Add *** 05/09/22 ************************************************
     d/copy sPrSecure
     d*** End Add *** 05/09/22 ************************************************
     d*** Beg Add *** 06/22/22 ************************************************
     d/copy sPrActPnl
     d*** End Add *** 06/22/22 ************************************************
      //** Beg Add  ***  11/26/22 WC2838 **************************************
     d/copy sPrGenErr
      //** End Add  ***  11/26/22 WC2838 ***************************************

      //
      //   Key List for the Documents Table File
      //
     C     keySTDLL      Klist
     C                   Kfld                    intCo#
     C     key1STDLL     Klist
     C                   Kfld                    intCo#
     C                   Kfld                    scDocCatg
     C                   Kfld                    scDocSubC
     C                   Kfld                    scDocID
     C                   Kfld                    scDocLang
     C     keySTDLL2     Klist
     C                   Kfld                    intCo#
     C                   Kfld                    scDocCatg
     C                   Kfld                    scDocSubC
     C                   Kfld                    scDocID
     C                   Kfld                    scDocLang
      //
      // Generic Table File Key List
      //
     C     keySTGTL      Klist
     C                   Kfld                    intCo#
     C                   Kfld                    xxTbid
     C                   Kfld                    xxKey
      //
      // Department Code Table File Key List
      //
     C     keySTDEL      Klist
     C                   Kfld                    intCo#
     C                   Kfld                    xxCat
     C                   Kfld                    xxSubC
      //
     C     key1STDEL     Klist
     C                   Kfld                    intCo#

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9            tab index
     c                   parm                    @@mode            1            tab index
     c                   parm                    @@next           10            tab index
     c                   parm                    @@TabInd          5            tab index

      /free

       GetClientID(clientID);

       ProgramStart(process);

       If process;
          //*** Beg Add *** 05/09/22 ******************************************
          If index <> 0;
          //*** End Add *** 05/09/22 ******************************************
             Main();
          //*** Beg Add *** 05/09/22 ******************************************
          Else;
             LoadActionBar();
             isNoAccess = *on;
             TabNames = 'Security';
             pnlHeight = 560;
             aPnlHeight = 620;
             Write rcdSysTbl;
             Exfmt rcdPanel;
             Read rcdSysTbl;
             If scSelOpt <> *blanks;
                @@next = scSelOpt;
             Endif;
          Endif;
          //*** End Add *** 05/09/22 ******************************************
       Endif;


       *InLr = *On;
      /end-free

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free
       //*** Beg Add *** 05/09/22 **********************************************
       intCo# = GetUserInfo(q1user);
       isNoAccess = *off;
       //*** End Add *** 05/09/22 **********************************************

       // Program Mode.
       If q1prms >= 2;
          scActMode = @@mode;
          Select;
          When @@mode = 'D';
             dspMode = *on;
          When @@mode = 'C';
             chgMode = *on;
          Endsl;
       Endif;

       apShowMenu = *on;

       // Find applicable options
       //Endsl;
       //*** Beg Chg *** 06/22/22 ********************************************
       //***scMenuChoi = 'Application Configuration,Claims Configuration';
       //***scMenuValu = 'WTAPPCFG,WTCLMCFG';

       //***If isModEnabled('Managed Care');
       //***   scMenuChoi = %trim(scMenuChoi) + ',Managed Care Configuration';
       //***   scMenuValu = %trim(scMenuValu) + ',MTINTCFG';
       //***Endif;

       //***scMenuChoi = %trim(scMenuChoi) + ',Policy Configuration';
       //***scMenuValu = %trim(scMenuValu) + ',WTPOLCFG';
       // Add Portal Configuration
       //***If isModEnabled('Portal Module');
       //***   scMenuChoi = %trim(scMenuChoi) + ',' +
       //***    GetModuleProperty('Portal Module':'Name') + ' Configuration';
       //***   scMenuValu = %trim(scMenuValu) + ',WICONFIG';
       //***Endif;
       GetActPnlItems(strMenuChoi:strMenuvalu);
       scMenuChoi = strMenuChoi;
       scMenuValu = strMenuValu;
       //*** End Chg *** 06/22/22 ********************************************

       aMenHeight = 120;

       apFootHght = aMenHeight + 125;

       //*** Beg Add *** 05/09/22 **********************************************
       // Set User Access button visibility indicator based on Users Security
       Chain (Q1user) pup01;
       If %Found(SMUPL) and UPlvl = 999 and UPtype = '*';
          isUsrAcc = *on;
       Else;
          isUsrAcc = *off;
       Endif;

       // Determine if user is in the App Config Tab file for each tab.  This
       // will determine which tabs are displayed.  If the user has 999 * then
       // all tabs are displayed

       // Search Security tab
       index = 0;
       //*** Beg Chg *** 06/26/23 ******************************************
       //*** Chain (intCo#:'STAPPCFG':'Application Configuration':'Search Security'
       Chain (intCo#:'STAPPCFG':'Application Configuration':'Security'
       //*** End Chg *** 06/26/23 ******************************************
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
       //*** End Add *** 05/09/22 **********************************************
          //*** Beg Chg *** 06/26/23 ******************************************
          //*** TabNames = 'Search Security';
          //*** index = 1;
          //*** namesArr(index) = 'Search Security';
          TabNames = 'Security';
          index = 1;
          namesArr(index) = 'Security';
          //*** End Chg *** 06/26/23 ******************************************
       //*** Beg Add *** 05/09/22 **********************************************
       Endif;
       //*** End Add *** 05/09/22 **********************************************

       GetSystemFlag(scrituraFlag);

       // Documents tab
       If scrituraFlag = 'Y';
          //*** Beg Chg *** 05/09/22 ******************************************
          //***TabNames = %trim(TabNames) + ', Documents';
          //***index = index + 1;
          //***namesArr(index) = 'Documents';
          Chain (intCo#:'STAPPCFG':'Application Configuration':'Documents'
           :Q1user) pTB01;
          If %Found(SDTBL1) or isUsrAcc = *on;
             If index = 0;
                TabNames = 'Documents';
             Else;
                TabNames = %trim(TabNames) + ', Documents';
             Endif;
             index = index + 1;
             namesArr(index) = 'Documents';
          Endif;
          //*** End Chg *** 05/09/22 ******************************************
       Endif;

       // Service Request Tracking tab
       //*** Beg Chg *** 05/09/22 ******************************************
       //***TabNames = %trim(TabNames) + ', Service Request Tracking';
       //***index = index + 1;
       //***namesArr(index) = 'Service Request Tracking';
       //***isShowVst = *off;
       //***showVstClr = *on;
       //***isShowVst = IsModEnabled('VISIT TRACKING');
          Chain (intCo#:'STAPPCFG':'Application Configuration':
            'Service Request Tracking':Q1user) ptb01;
          If %Found(SDTBL1) or isUsrAcc = *on;
             isShowVst = *off;
             isShowVst = IsModEnabled('VISIT TRACKING');
             If index = 0;
                TabNames = 'Service Request Tracking';
             Else;
                TabNames = %trim(TabNames) + ', Service Request Tracking';
             Endif;
             index = index + 1;
             namesArr(index) = 'Service Request Tracking';
             showVstClr = *on;
          Endif;
       //*** End Chg *** 05/09/22 ******************************************

       // Rules/Auto Diary Triggers tab
       //*** Beg Add *** 05/09/22 ******************************************
       Chain (intCo#:'STAPPCFG':'Application Configuration':
         'Auto Diary Triggers':Q1user) ptb01;
       If %Found(SDTBL1) or isUsrAcc = *on;
       //*** End Add *** 05/09/22 ******************************************
       //*** Beg Del *** 05/09/22 ******************************************
       //***   index = index + 1;
       //***   namesArr(index) = 'Auto Diary Triggers';
       //*** End Del *** 05/09/22 ******************************************
          isAutoTrig = *off;
          isPolTrig  = *off;
          isClmTrig  = *off;
          isFileTrig  = *off;
          isAutoTrig = IsModEnabled('Auto-Diary Triggers');
          isRulesEng = IsModEnabled('Rules Engine');
          Select;
          When isRulesEng = *off and isAutoTrig = *on;
             strRuleTrg = 'Auto Diary Trigger';
          When not isRulesDone;
             isRulesEng = *off;
             strRuleTrg = 'Auto Diary Trigger';
          Other;
             strRuleTrg = 'Rule';
          Endsl;
          //*** Beg Chg *** 05/09/22 ******************************************
          //***   TabNames = %trim(TabNames) + ',' + %trim(strRuleTrg) + 's';
          If index = 0;
             TabNames = %trim(strRuleTrg) + 's';
          Else;
             TabNames = %trim(TabNames) + ',' + %trim(strRuleTrg) + 's';
          Endif;
          //*** End Chg *** 05/09/22 ******************************************
          index = index + 1;
          namesArr(index) = 'Auto Diary Triggers';
          strRuleHdgs = %trim(strRuleTrg) + ',Sequence #,Description,' +
           'Diary Code,Status,,';
          strRuleHdg = %trim(strRuleTrg) + ' Configuration';
          strLblSetup = %trim(strRuleTrg) + ' Setup and Maintenance';
          strLblHdg = %trim(strRuleTrg);
          strLblCtl = %trim(strRuleTrg) + ' Controls';
          strFltDlg = %trim(strRuleTrg) + ' Filter';
          strFltLbl = %trim(strRuleTrg) + '(s)';
          scRuleHdgs = strRuleHdgs;
          scRuleHdg = strRuleHdg;
          scLblSetup = strLblSetup;
          scLblHdg = strLblHdg;
          scLblCtl = strLblCtl;
          scFltDlg = strFltDlg;
          scFltLbl = strFltLbl;
          isRules = isRulesEng;
          scTrigType = 'C';
          // There is on-load javascript using this indicator
          isRuleOnly = *off;
          Select;
          When isRulesEng = *on and isAutoTrig = *off;
             scTrigType = 'X';
             isRuleOnly = *on;
             isAutoTrig = *on;
          Endsl;
          scTrigDtl = 'Detail';
          isTextual = isTextualDone;

          // Initialize auto diary trigger filter
          ClearTrgFilter();
          fltrTrgSts = 'A,P,Q,i';
          showTrgClr = *on;
          If isAutoTrig = *off;
             showTrgClr = *off;
             fltrTrgSts = ' ';
          Endif;
          scFltTrgSt = fltrTrgSts;

          dspTrgAdd = *on;
          If isAutoTrig = *off or dspMode = *on;
             dspTrgAdd = *off;
          Endif;
       //*** Beg Add *** 05/09/22 ********************************************
       Endif;
       //*** End Add *** 05/09/22 ********************************************

       // get the system values
       GetSystem(##Wcif:##Mcif:##Wcin);


       // Output Control tab.  Only disply tab if records exist in STSPP
       //*** Beg Chg *** 05/09/22 ******************************************
       //***If spNbrRcds > 0;
       Chain (intCo#:'STAPPCFG':'Application Configuration':
         'Output Control':Q1user) ptb01;
       If spNbrRcds > 0 and (%Found(SDTBL1) or isUsrAcc = *on);
       //*** End Add *** 05/09/22 ******************************************
          isUseScrp = IsModEnabled('Scriptura-Various');
          isUse3rd  = IsModEnabled('3rd Party XML');
          //*** Beg Chg *** 05/09/22 ******************************************
          //***TabNames = %trim(TabNames) + ', Output Control';
          If index = 0;
             TabNames = 'Output Control';
          Else;
             TabNames = %trim(TabNames) + ', Output Control';
          Endif;
          //*** End Chg *** 05/09/22 ******************************************
          isShowOut = *on;
          index = index + 1;
          namesArr(index) = 'Output Control';
       Endif;

       // Auto Letters tab
       //*** Beg Chg *** 05/09/22 ******************************************
       //***If scrituraFlag = 'Y';
       //***   TabNames = %trim(TabNames) + ', Auto Letters';
       Chain (intCo#:'STAPPCFG':'Application Configuration':
         'Auto Letters':Q1user) ptb01;
       If scrituraFlag = 'Y' and (%Found(SDTBL1) or isUsrAcc = *on);
          If index = 0;
             TabNames = 'Auto Letters';
          Else;
             TabNames = %trim(TabNames) + ', Auto Letters';
          Endif;
       //*** End Chg *** 05/09/22 ******************************************
          index = index + 1;
          namesArr(index) = 'Auto Letters';
       Endif;

       // Dashboard tab
       isUseDashConfig = IsModEnabled('Dashboard Config');
       // Dashboard Configuration
       //*** Beg Chg *** 05/09/22 ******************************************
       //***If isUseDashConfig;
       //***   TabNames = %trim(TabNames) + ', Dashboard';
       Chain (intCo#:'STAPPCFG':'Application Configuration':
         'Dashboard':Q1user) ptb01;
       If isUseDashConfig and (%Found(SDTBL1) or isUsrAcc = *on);
          If index = 0;
             TabNames = 'Dashboard';
          Else;
             TabNames = %trim(TabNames) + ', Dashboard';
          Endif;
       //*** End Chg *** 05/09/22 ******************************************
          index = index + 1;
          namesArr(index) = 'Dashboard';
       Endif;

       // Branding tab
       //*** Beg Chg *** 05/09/22 ******************************************
       //***TabNames = %trim(TabNames) + ', Branding';
       Chain (intCo#:'STAPPCFG':'Application Configuration':
         'Branding':Q1user) ptb01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          If index = 0;
             TabNames = 'Branding';
          Else;
             TabNames = %trim(TabNames) + ', Branding';
          Endif;
       //*** End Chg *** 05/09/22 ******************************************
          index = index + 1;
          namesArr(index) = 'Branding';
       //*** Beg Add *** 05/09/22 ******************************************
       Endif;
       //*** End Add *** 05/09/22 ******************************************

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

       //*** Beg Del *** 05/09/22 **********************************************
       //***intCo# = GetUserInfo(q1user);
       //*** End Del *** 05/09/22 **********************************************
       Chain intCo# pco00;
       If COname <> ' ';
          scCompName = COName;
       Endif;

       scCO# = intCO#;
       dspOnly = dspMode;
       dspAutlAdd = not dspMode;
       dspAutlEdt = dspMode;

       clear dsSecTypEr;
       clear dsSecLvlEr;

       //* Read Scriptura module file.
       Read(n) psc0p;

       //* Add default file extension types for Scriptura to system table file.
       Exsr WriteScripDef;

       processMe = *on;

       isRetroI = IsModEnabled('Incurred Loss');
       isRetro = isRetroI;

       Return;

       //===================================================================
       // Write default file extensions for Scriptura.
       //===================================================================
       BegSr WriteScripDef;

       //* Interactive
       xxTBID = 'FILI';
       xxKey  = '.SLD';
       // Retrieve from Generic Table File
       Chain(n) keySTGTL pgt00;
       If not %found;
          Clear pgt00;
          GTCo#  = intCo#;
          GTTBID = xxTBID;
          GTKEY  = xxKey;
          GTSTAT = 'A';
          GTDESC = 'Default Live Docs File Extension - ' + %trim(xxKey);
          GTPgm  = Q1PGM;
          GTUser = Q1USER;
          GTTime = Timeto6(SystemTime());
          GTDt   = Dateto7(SystemDate());
          Write pgt00;
       Endif;

       //* Non-Interactive
       xxTBID = 'FILN';
       xxKey  = '.SCD';
       Chain(n) keySTGTL pgt00;
       If not %found;
          Clear pgt00;
          GTCo#  = intCo#;
          GTTBID = xxTBID;
          GTKEY  = xxKey;
          GTSTAT = 'A';
          GTDESC = 'Default Live Docs File Extension - ' + %trim(xxKey);
          GTPgm  = Q1PGM;
          GTUser = Q1USER;
          GTTime = Timeto6(SystemTime());
          GTDt   = Dateto7(SystemDate());
          Write pgt00;
       Endif;
       //*** begin add ***  09/25/24  *************************************
       //* Interactive Form
       xxTBID = 'FILF';
       xxKey  = '.SDT';
       Chain(n) keySTGTL pgt00;
       If not %found;
          Clear pgt00;
          GTCo#  = intCo#;
          GTTBID = xxTBID;
          GTKEY  = xxKey;
          GTSTAT = 'A';
          GTDESC = 'Default Interactive Form Extension - ' + %trim(xxKey);
          GTPgm  = Q1PGM;
          GTUser = Q1USER;
          GTTime = Timeto6(SystemTime());
          GTDt   = Dateto7(SystemDate());
          Write pgt00;
       Endif;
       //*** end add *****  09/25/24  *************************************

       Endsr;
       //===================================================================

      /end-free
     p ProgramStart    e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /free

       LoadActionBar();
       Exsr LoadDocFilter;
       Exsr LoadDocDefaults;
       //** Beg Add  ***  11/26/22 WC2838 ************************
       Exsr LoadOutDefaults;
       //** End Add  ***  11/26/22 WC2838 ************************
       LoadDocuments();

       // Load Application Configuration Setup Info
       LoadConfInfo();

       fltrVstSts = 'A';

       LoadOutput();

       // Process until exit button is pressed.
       DoU exitNow = *On;
          // display screen
          Exsr ExfmtScreens;

          error = *off;
          clear dsSecTypEr;
          clear dsSecLvlEr;

          // Perform actions on screen.
          Select;

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;

          // Exit the program
          When exitNow = *on;
             Leave;

          // Do nothing - Catch tab click event
          When tabClick = *On;
             tabClick = *Off;
             fltrTrig   = *blanks;
             scFiltTrg  = fltrTrig;
             showTrgClr = *off;
             If scFiltTrg <> ' ' or fltrTrgRl <> ' ' or fltrTrgUsr <> ' ' or
              fltrTrgSts <> ' ';
                showTrgClr = *on;
             Endif;

          // Refresh
          When btnRefresh = *On;
             btnRefresh =*Off;
             intCo# = GetUserInfo(q1user);
             Chain intCo# pco00;
             LoadActionBar();
             Exsr LoadDocFilter;
             Exsr LoadDocDefaults;
             LoadDocuments();

             LoadVisit();
             LoadOutput();
             // Load auto letter grid
             LoadAutoLet();

             // Load Application Configuration Setup Info
             LoadConfInfo();

          // Edit/Save the appropriate screen.
          When save = *on;

             Select;

             // Edit Setup Detail for Errors (Enter key or Save button)
             //*** Beg Chg *** 06/26/23 ******************************************
             //*** When namesArr(TabIndex +1) = 'Search Security';
             When namesArr(TabIndex +1) = 'Security';
             //*** End Chg *** 06/26/23 ******************************************
                If not dspMode;
                   editCheck();

                   // Save setup Detail
                   If error = *off;
                      SaveDetail();
                   Endif;
                Endif;

             When namesArr(TabIndex +1) = 'Documents';
                // Edit Document Defaults Section (Enter key or Save button)
                If not dspMode;
                   Exsr EditDocDefaults;

                   // Save setup Detail
                   If error = *off;
                      Exsr SaveDocDefaults;
                   Endif;
                Endif;

             When namesArr(TabIndex +1) = 'Service Request Tracking';
                If not dspMode;
                   // Save setup Detail
                      DftFltrStat();
                Endif;

             When namesArr(TabIndex +1) = 'Output Control';
                // Edit Output Control Section (Enter key or Save button)
                If not dspMode;
                   Exsr EditOutputGrid;
             //** Beg Add  ***  11/26/22 WC2838 ************************
                   Exsr EditOutDefaults ;
             //** End Add  ***  11/26/22 WC2838 ************************
                   // Save setup Detail
                   If error = *off;
             //** Beg Add  ***  11/26/22 WC2838 ************************
                      Exsr SaveOutDefaults ;
             //** End Add  ***  11/26/22 WC2838 ************************
             //** Beg Del  ***  03/27/23 WC2838 ************************
             // SaveOutputGrid subroutine always chain last record of STSPP file
             // and Grid info is alreay saved/updated through EDIT link
             // Hence Not Needed for SAVE button of System toolbar
             //*****  Exsr SaveOutputGrid;
             //** End Del  ***  03/27/23 WC2838 ************************
                   Endif;
                Endif;

             When namesArr(TabIndex +1) = 'Auto Diary Triggers';
                // Edit Output Control Section (Enter key or Save button)
                If not dspMode;
                   Exsr RulesTabEdit;
                   // Save setup Detail
                   If error = *off;
                      Exsr RulesTabSave;
                   Endif;
                Endif;
             Endsl;
             save = *off;

          // Add a new document setup record
          When addLetter = *on;
             DocWindow();
             LoadDocuments();

          // Add a new document setup record
          When addSub = *on;
             SubWindow();
             LoadDocuments();

          // Add a new printer setup record
          When addPrint = *on;
             PrtWindow();
             LoadDocuments();

          // Filter the document setup grid
          When docFilter = *on;
             Exsr ProcessFilter;
             LoadDocuments();

          // Filter the document setup grid
          When clrDocFilt = *on;
             scFiltCatg = ' ';
             scFiltDept = ' ';
             scFiltStat = ' ';
             scFiltEmai = ' ';
             scFiltIntr = 'B';
             scFiltLang = 'All';
             scFiltAuto = 'B';
             showClrFil = *off;
             LoadDocuments();

          // Filter the sub category setup grid
          When subFilter = *on;
             Exsr ProcessSubFilter;
             LoadDocuments();

          // Filter the sub category setup grid
          When clrSubFilt = *on;
             scSubFilCt = ' ';
             scSubFilSt = ' ';
             showClrSub = *off;
             LoadDocuments();

          When vstTypFlt = *On;
             SetVstFilter();

          When clrVstFlt = *On;
             ClearVstFilter();

          When addVstTyp = *On;
             CfgVisitType();

          // process auto trigger filter
          When AutoDryFlt = *On;
             SetTrgFilter();

          // process auto trigger clear filter
          When clrTrigFlt = *On;
             ClearTrgFilter();

          // add a new auto diary trigger
          When BtnTrgAdd = *on;
             BtnTrgAdd = *off;
             FlgTrgMode = 'A';
             ProcAddEdt();

          When outFlt = *On;
             SetOutFilter();

          When clrOutFlt = *On;
             ClearOutFilter();
          // process auto trigger filter
          When AutoLetFlt = *On;
             SetAutlFilter();

          // process auto trigger clear filter
          When clrAutlFlt = *On;
             ClearAutFilter();

          // add a new auto diary trigger
          When BtnAutlAdd = *on;
             BtnAutlAdd = *off;
             FlgAutMode = 'A';
             ProcAutlAddEdt();

          // Add a dashboard item
          When addDashCfg = *on;
             addDashCfg = *off;
             ProcDashCfg('A');
             LoadDashboard();

          // Add a dashboard user
          When addDashUsr = *on;
             addDashUsr = *off;
             ProcDashUsr('A');
             LoadDashboard();
          // Add a Branding item
          When addBrand = *on;
             addBrand = *off;
             ProcBrandCfg('A');
             LoadBrand();
          //*** Beg Add *** 05/09/22 ******************************************
          // Add User Access Window Logic
          When btnUsrAcc = *on;
             inDspFileName = 'STAPPCFG';
             inPgmName     = 'Application Configuration';
             inTabName     =  namesArr(TabIndex + 1);
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;
          //*** End Add *** 05/09/22 ******************************************
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
          When scGrpSave = *On;
               scGrpSave = *off;
               SaveGrpSel( );
      *** End Add ***** 07/16/24 *** WC2972 ****************************
          Other;

             //*  Check for clicked links on grid.
             Readc sflDocGrid;
             If not %eof();

                //*  Release the notification.
                If editDoc = *on;
                   editDoc = *off;
                   Update sflDocGrid;
                   DocWindow(outDocRRN);
                   LoadDocuments();
                Endif;

             Endif;

             //*  Check for clicked links on grid.
             Readc sflSubGrid;
             If not %eof();

                //*  Release the notification.
                If editSub = *on;
                   editSub = *off;
                   Update sflSubGrid;
                   SubWindow(outSubRRN);
                   LoadDocuments();
                Endif;

             Endif;

             //*  Check for clicked links on grid.
             Readc sflPrinter;
             If not %eof();

                If editPrint = *on;
                   editPrint = *off;
                   Update sflPrinter;
                   PrtWindow(outPrtRRN);
                   LoadDocuments();
                Endif;

                If delPrint = *on;
                   delPrint = *off;
                   Update sflPrinter;
                   strMsg = 'Press Confirm to remove this printer destination.';
                   If not PopTwoBtn(strmsg:'Delete Printer - ' +
                    %trim(outPrtKey):'Cancel':'Confirm');
                      Chain outPrtRRN pp10p;
                      If %found;
                         Delete pp10p;
                         LoadDocuments();
                      Endif;
                   Endif;
                Endif;

             Endif;

             Readc sflVstTrk;
             If not %eof();
                If editVstTyp = *on;
                   editVstTyp = *off;
                   Update sflVstTrk;
                   CfgVisitType(OutVstRRN);
                   LoadVisit();
                Endif;
                //*** Beg Add ***** 06/08/23 ************************
                If copyVstTyp = *on;
                   isCopyMode = *on;
                   copyVstTyp = *off;
                   Update sflVstTrk;
                   CfgVisitType(OutVstRRN);
                   LoadVisit();
                Endif;
                //*** End Add ***** 06/08/23 ************************
             Endif;

             // check for auto diary trigger
             Readc sflAutoTrg;
             If not %eof();
                If LnkTrgEdt = *on;
                   LnkTrgEdt = *off;
                   flgTrgMode = 'E';
                   TrgNxtChg = *off;
                   Update sflAutoTrg;
                   ProcAddEdt();
                Endif;

                If LnkTrgSts = *on;
                   LnkTrgSts = *off;
                   TrgNxtChg = *off;
                   Update sflAutoTrg;
                   Exsr ProcStsChg;
                Endif;
             Endif;
             // check for auto diary trigger
             Readc sflTrigTxt;
             If not %eof();
                If LnkTrgEdt = *on;
                   LnkTrgEdt = *off;
                   flgTrgMode = 'E';
                   TrgNxtChg = *off;
                   Update sflTrigTxt;
                   ProcAddEdt();
                Endif;

                If LnkTrgSts = *on;
                   LnkTrgSts = *off;
                   TrgNxtChg = *off;
                   Update sflTrigTxt;
                   Exsr ProcStsChg;
                Endif;
             Endif;

             Readc sflOutCtl;
             If not %eof();
                If editOutCtl = *on;
                   editOutCtl = *off;
                   Update sflOutCtl;
                   CfgOutputCtl(scOutRRN);
                   LoadOutput();
                Endif;
             Endif;
          // check for auto diary trigger
          Readc sflAutoTrg;
          If not %eof();
             If LnkTrgEdt = *on;
                LnkTrgEdt = *off;
                flgTrgMode = 'E';
                TrgNxtChg = *off;
                Update sflAutoTrg;
                ProcAddEdt();
             Endif;

             If LnkTrgSts = *on;
                LnkTrgSts = *off;
                TrgNxtChg = *off;
                Update sflAutoTrg;
                Exsr ProcStsChg;
             Endif;
          Endif;
          Readc sflTrigTxt;
          If not %eof();
             If LnkTrgEdt = *on;
                LnkTrgEdt = *off;
                flgTrgMode = 'E';
                TrgNxtChg = *off;
                Update sflTrigTxt;
                ProcAddEdt();
             Endif;

             If LnkTrgSts = *on;
                LnkTrgSts = *off;
                TrgNxtChg = *off;
                Update sflTrigTxt;
                Exsr ProcStsChg;
             Endif;
          Endif;
          // check for auto letter
          Readc sflAutoLet;
          If not %eof();
             If LnkAutlEdt = *on;
                LnkAutlEdt = *off;
                flgAutMode = 'E';
                AutlNxtChg = *off;
                Update sflAutoLet;
                ProcAutlAddEdt();
             Endif;

             If LnkAutlSts = *on;
                LnkAutlSts = *off;
                FlgAutMode = 'S';
                AutlNxtChg = *off;
                Update sflAutoLet;
                ProcAutlAddEdt();
             Endif;
          Endif;

             //  Check for clicked links on grid.
             Readc sflDashCfg;
             If not %eof();
                If edtDashCfg = *on;
                   edtDashCfg = *off;
                   Update sflDashCfg;
                   ProcDashCfg('E':scDashRrn);
                   LoadDashboard();
                Endif;
             Endif;

             //  Check for clicked links on grid.
             Readc sflDashUsr;
             If not %eof();
                If edtDashUsr = *on;
                   edtDashUsr = *off;
                   Update sflDashUsr;
                   ProcDashUsr('E':scRole);
                   LoadDashboard();
                Endif;
             Endif;
             //  Check for clicked links on grid.
             Readc sflBrand;
             If not %eof();
                If editBrand = *on;
                   editBrand = *off;
                   Update sflBrand;
                   ProcBrandCfg('E':scBrnRrn);
                   LoadBrand();
                Endif;
             Endif;
             If not %eof();
                If delBrand = *on;
                   delBrand = *off;
                   Update sflBrand;
                   ProcBrandCfg('D':scBrnRrn);
                   LoadBrand();
                Endif;
             Endif;

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;
       scSMS = IsModEnabled('SMS Enabled');

       isClmTrig = *off;
       isPolTrig = *off;
       isFileTrig  = *off;
       indexTab = TabIndex + 1;

       Select;
       //*** Beg Chg *** 06/26/23 ******************************************
       //*** When namesArr(indexTab) = 'Search Security';
       //***    pnlHeight = 560;
       When namesArr(indexTab) = 'Security';
          pnlHeight = 690;
       //*** End Chg *** 06/26/23 ******************************************
          APnlHeight = pnlHeight + 60;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt rcdSearch;
          Read  rcdSysTbl;
          Read rcdPanel;
       When namesArr(indexTab) = 'Documents';
          //*** Beg Add 08/12/24 ********************************************
          If rrnPosDoc <> 0 and rrnPosDoc <= rrnDocRow;
             rrnDoc = rrnPosDoc;
          Else;
             rrnDoc = 1;
          Endif;
          //*** End Add 08/12/24 ********************************************
          pnlHeight = 1050;
          APnlHeight = pnlHeight + 60;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt rcdLetters;
          Read  rcdSysTbl;
          Read rcdPanel;
       When namesArr(indexTab) = 'Service Request Tracking';
          LoadVisit();
          If isShowVst = *off;
             pnlHeight =  560;
             showVstClr = *off;
             showVstAdd = *off;
          Else;
             //*** Beg Chg ***** 06/08/23 ************************
             //*** pnlHeight =  850;
             pnlHeight =  1000;
             //*** End Chg ***** 06/08/23 ************************
             If dspMode = *off;
                showVstAdd = *on;
             Endif;
          Endif;

          APnlHeight = pnlHeight + 60;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt rcdVstTrk;
          Read  rcdSysTbl;
          Read  rcdPanel;
       When namesArr(indexTab) = 'Auto Diary Triggers';
          Select;
          When isAutoTrig = *off;
             pnlHeight = 770;
             isPolTrig = *off;
             isClmTrig = *off;
             isFileTrig = *off;
          When scTrigType = 'C';
             If isAutoTrig = *on;
                isClmTrig = *on;
             Endif;
             pnlHeight = 1670;
          When scTrigType = 'P';
             If isAutoTrig = *on;
                isPolTrig = *on;
             Endif;
             pnlHeight = 1045;
             scPolTop  = 840;
          When scTrigType = 'X';
             isFileTrig = *on;
             pnlHeight = 970;
          Endsl;

          LoadTrgGrid();
          LoadTrgDtl();
          If rrnPosTrg <> 0 and rrnPosTrg <= rrnTrgRow;
             rrnAutoTrg = rrnPosTrg;
          Else;
             rrnAutoTrg = 1;
          Endif;

          isGenFile = *off;
          isGenFile = *on;
          If isRulesEng = *off;
             isGenFile = *off;
          Endif;
          scRuleHdgs = strRuleHdgs;
          Select;
          When isFileTrig = *on;
             scRuleHdgs = ScanReplace(',Diary Code,':',,':strRuleHdgs);
          Other;
             scRuleHdgs = strRuleHdgs;
          Endsl;
          scRuleHdg = strRuleHdg;
          scLblSetup = strLblSetup;
          scLblHdg = strLblHdg;
          scLblCtl = strLblCtl;
          isRules = isRulesEng;
          APnlHeight = pnlHeight + 60;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt rcdAutoTrg;
          Read  rcdSysTbl;
          Read  rcdPanel;
       When namesArr(indexTab) = 'Output Control';
          // Assume all containers will be displayed
        //** Beg Chg  ***  11/26/22 WC2838 ************************
        //pnlHeight = 675;
          //**** Beg Chg **** 09/23/24 ************************************
          //***pnlHeight = 755;
          pnlHeight = 795;
          //**** End Chg **** 09/23/24 ************************************
        //** End Chg  ***  11/26/22 WC2838 ************************
          //**** Beg Add **** 09/23/24 ************************************
          topModSet = 740; // Initial Module Setting Top Coordinate
          //**** End Add **** 09/23/24 *************************************
          sc3rdTop  = 630;
          If isUseScrp = *off;
             // Adjust based on the height of the Scriptura container
             pnlHeight -= 190;
             sc3rdTop  -= 190;
          //**** Beg Add **** 09/23/24 *******************************
             topModSet -= 190;
             isVisScrp = *Off;
          Else;
             isVisScrp = *On;
          //**** End Add **** 09/23/24 *******************************
          Endif;
          //*** Beg Del ***** 09/09/24 *****************************
          //***If isUse3rd = *off;
          //***   // Adjust based on the height of the 3rd Party container
          //***   pnlHeight -= 105;
          //***Endif;
          //*** End Del ***** 09/09/24 *****************************
          //**** Beg Add **** 09/23/24 **********************************
          If isUse3rd = *off;
             // Adjust based on the height of the 3rd Party container
             pnlHeight -= 105;
             topModSet -= 105;
             isVis3rd = *Off;
          Else; // 3rd Party XML & Module Setting
             isVis3rd = *On;
          Endif;
          //**** End Add **** 09/23/24 ***********************************
          // Minimum panel height is 560
          If pnlHeight < 560;
          //**** Beg Chg **** 09/23/24 *************************************
             //***pnlHeight = 560;
             pnlHeight = 500;
          //**** End Chg **** 09/23/24 *************************************
          Endif;
          APnlHeight = pnlHeight + 60;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt rcdOutput;
          Read  rcdSysTbl;
          Read  rcdPanel;
       When namesArr(indexTab) = 'Auto Letters';
          //*** Beg Chg ***** 09/16/22 ****************************
          //pnlHeight = 680;
          pnlHeight = 840;
          //*** End Chg ***** 09/16/22 ****************************

          LoadAutoLet();
          LoadAutLetDtl();
          If rrnPosAutL <> 0 and rrnPosAutL <= rrnAutlRow;
             rrnAutoLet = rrnPosAutL;
          Else;
             rrnAutoLet = 1;
          Endif;

          APnlHeight = pnlHeight + 60;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt rcdAutoLet;
          Read  rcdSysTbl;
          Read  rcdPanel;
       When namesArr(indexTab) = 'Dashboard';
          LoadDashboard();
          pnlHeight = 680;
    *** Beg Add ***** 07/16/24 *** WC2972 ****************************
          pnlHeight = 1020;
     *** End Add ***** 07/16/24 *** WC2972*****************************
          APnlHeight = pnlHeight + 60;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt rcdDashBrd;
          Read  rcdSysTbl;
          Read  rcdPanel;
       When namesArr(indexTab) = 'Branding';
          LoadBrand();
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt rcdBrand;
          Read  rcdSysTbl;
          Read rcdPanel;
       EndSl;

       Endsr;

       //===================================================================
       // Load the document defaults for scriptura module.
       //===================================================================
       BegSr LoadDocDefaults;

       scDocDef = SCFlg4;
       If scDocDef = ' ';
          scDocDef = 'I';
       Endif;

       scMaxDoc = SC1101;
       //*** begin add ***  09/25/24  *************************************
       scScriptDC = SCFlg6;
       visScrpDC = IsModEnabled('Design and Compose');
       //*** end add *****  09/25/24  *************************************

       scClmInq = ' ';
       scPolInq = ' ';
       If  %scan('C':SCkey3) <> 0;                //SCkey contain value as 'CP'
           scClmInq = 'C';                        //1st pos = Claims, 2nd pos = Policy
       Endif;
       If  %scan('P':SCkey3) <> 0;
           scPolInq = 'P';
       Endif;

       Endsr;

       //===================================================================
       // Edit the document defaults for scriptura module.
       //===================================================================
       BegSr EditDocDefaults;

       Clear msMaxDoc;
       Clear erMaxDoc;
       Clear error;

       If scMaxDoc <= 0;
          error = *on;
          msMaxDoc = 'Maximum # of documents open cannot be less than one.';
          erMaxDoc = *on;
       Endif;

       Endsr;

       //===================================================================
       // Edit the Auto Letter defaults for scriptura module.
       //===================================================================
       BegSr EditAutoLetters;


       Endsr;
       //** Beg Add  ***  11/26/22 WC2838 ************************
       //===================================================================
       // Load the document defaults for scriptura module.
       //===================================================================
       BegSr LoadOutDefaults;

       scSize   = SC1104;
       scCount  = SC1105;
       scErrNot = SCURL5;
       //*** Begin Add *** 05/24/23 ****************************************
       scFtpFailD = SCurl1;
       //*** End   Add *** 05/24/23 ****************************************

       Endsr;
       //===================================================================
       // Edit the document defaults for scriptura module.
       //===================================================================
       BegSr EditOutDefaults;

       Clear msSize;
       Clear erSize;
       Clear msCount;
       Clear erCount;
       Clear msErrNot;
       Clear erErrNot;
       //*** Begin Add *** 05/24/23 ****************************************
       Clear msFtpFailD;
       Clear erFtpFailD;
       //*** End   Add *** 05/24/23 ****************************************
       Clear error;

       If scSize < 0;
          error = *on;
          msSize = 'Email Size cannot be less than zero.';
          erSize = *on;
       Endif;

       If scCount < 0;
          error = *on;
          msCount = 'Email Count cannot be less than zero.';
          erCount = *on;
       Endif;

          If %len(%trim(scErrNot)) > 0;
             msErrNot = 'Email Count cannot be less than zero.';
               erErrNot = Check_Email(scErrNot:msErrNot);
             If erErrNot = *On;
                error = *on;
             EndIf;
          EndIf;

          //*** Begin Add *** 05/24/23 *************************************
          // Ftp Failure Directory must be entered.
          If scFtpFailD = ' ';
             //*** Beg Chg ***** 09/09/24 ********************************
             //***error = *on;
             //***msFtpFailD = 'Ftp Failure Directory must be entered.';
             //***erFtpFailD = *on;
             If scSize <> 0 or scCount <> 0;
                error = *on;
                msFtpFailD = 'Ftp Failure Directory must be entered.';
                erFtpFailD = *on;
             Endif;
             //*** End Chg ***** 09/09/24 ********************************
          Else;
             // Directory must exist
             ifsObjName   = scFtpFailD;
             ifsObjExists = *Off;
             ChkIfsObj(ifsObjName:ifsObjExists);
             If Not(ifsObjExists);
                error = *on;
                erFtpFailD = *On;
                msFtpFailD = 'Document does not exist or authority ' +
                             'is restricted.';
             Else;
                // Path must begin with / and end with /
                If (%Subst(scFtpFailD:1:1) <> '/' Or
                    %Subst(scFtpFailD:%Len(%Trim(scFtpFailD)):1) <> '/');
                   error = *on;
                   erFtpFailD = *On;
                   msFtpFailD = 'Directory must begin with a / ' +
                                'and end with a /.';
                EndIf;
             EndIf;
          Endif;
          //*** End   Add *** 05/24/23 *************************************

       Endsr;
       //** End Add  ***  11/26/22 WC2838 ************************
       //===================================================================
       // Edit the Output Control check boxes - Grid
       //===================================================================
       Begsr EditOutputGrid;

         error = *off;

         Dou %eof;
            Readc sflOutCtl;
            If not %eof();
               outNxtChg = *on;
               Update sflOutCtl;
               If scSplO = 'N' and scScrO = 'N' and sc3Xml = 'N';
                  strMsg = %trim(scType) +
                   ' - At least one output type must be selected.';
                  PopOneBtn(strMsg:'Output Control Error');
                  error = *on;
                  Leave;
               Endif;
            Endif;
         Enddo;

       Endsr;

       //===================================================================
       // Edit the Rules Tab
       //===================================================================
       Begsr RulesTabEdit;
       Endsr;

       //===================================================================
       // Save the Rules Tab
       //===================================================================
       Begsr RulesTabSave;
         Clear sprrn;
         Chain ('Rule Report') psp00;
         Chain sprrn psp0p;
         SPpathExt  = scPathExt;
         SPpgm  = q1pgm;
         SPuser = q1user;
         SPtime = Timeto6(SystemTime());
         SPdt   = Dateto7(SystemDate());
         If not %found;
            SPco#  = intCo#;
            SPtype = 'Rule Report';
            SPsplO = 'N';
            SPsplR = 'Y';
            SPstat = 'X';
            SPiUsr = SPuser;
            SPiPgm = SPpgm;
            SPiTim = SPtime;
            SPiDt = SPdt;
            Write psp0p;
         Else;
            Update psp0p;
         Endif;

         Exfmt rcdSave;
       Endsr;

       //** Beg Add  ***  11/26/22 WC2838 ************************
       //===================================================================
       // Save the Output defaults Module Settings.
       //===================================================================
       BegSr SaveOutDefaults;

       Setll 1 psc0p;
       Read psc0p;

       SC1104 = scSize;
       SC1105 = scCount;
       SCURL5 = %trim(scErrNot);
       //*** Begin Add *** 05/24/23 ****************************************
       SCurl1 = scFtpFailD;
       //*** End   Add *** 05/24/23 ****************************************

       SCPgm  = Q1PGM;
       SCUser = Q1USER;
       SCTime = Timeto6(SystemTime());
       SCDt   = Dateto7(SystemDate());

       Update psc0p;

       Exfmt rcdSave;

       Endsr;

       //** End Add  ***  11/26/22 WC2838 ************************
       //===================================================================
       // Save the Ouput Control record - Grid
       //===================================================================
       BegSr SaveOutputGrid;

         Chain scOutRRN psp0p;

         SP3DtQS  = sc3DtQS;
         SP3DtQR  = sc3DtQR;
         SP3XmlF  = sc3XmlF;
         SPEmlSub = scEmlSub2;
         SPEmlBody = scEmlBody2;
         SPEmlUsro = scEmlUsro2;

       //** Beg Add  ***  11/26/22 WC2838 ************************

         SPEmlSubM  = scEmlSubM;
         SPEmlBodyM = scEmlBodyM;
         SPEmlSubF  = scEmlSubF;
         SPEmlBodyF = scEmlBodyF;

         SPPKeyI  = scSPrintMI ;
         SPTrayI  = scSTrayMI  ;
         SPOverPI = scSOverPMI ;
         SPPKeyA  = scSPrintMA ;
         SPTrayA  = scSTrayMA  ;
         SPOverPA = scSOverPMA ;

         If scFilePref <> ' ';
            SPFnPreFix = %Trim(scFilePref);
         EndIf;

         SPFnData = %Trim(scSFileNm);

       //** End Add  ***  11/26/22 WC2838 ************************
       //** Beg Add  ***  07/25/23 WC2850 ************************
         If scCopiesS = 0;
            SPCpyS = 1;
         Else;
            SPCpyS = scCopiesS;
         EndIf;
         If scCopiesM = 0;
            SPCpyM = 1;
         Else;
            SPCpyM = scCopiesM;
         EndIf;
       //** End Add  ***  07/25/23 WC2850 ************************
       //** Beg Add  ***  01/16/24 *** 153WAD ********************
            SPCopyE = scCopyE;
       //** End Add  ***  01/16/24 *** 153WAD ********************
         SPuser   = q1user;
         SPpgm    = q1pgm;
         SPdt     = Dateto7(SystemDate());
         SPtime   = Timeto6(SystemTime());

         If %found;
            Update psp0p;
         Endif;

       Endsr;

       //===================================================================
       // Save the document defaults for scriptura module.
       //===================================================================
       BegSr SaveDocDefaults;

       Setll 1 psc0p;
       Read psc0p;

       SC1101 = scMaxDoc;
       //*** begin add ***  09/25/24  *************************************
       SCFlg6 = scScriptDC;
       //*** end add *****  09/25/24  *************************************
       SCFlg4 = scDocDef;
       Clear SCkey3;
       SCkey3 = %trim(scClmInq) + %trim(scPolInq);

       SCPgm  = Q1PGM;
       SCUser = Q1USER;
       SCTime = Timeto6(SystemTime());
       SCDt   = Dateto7(SystemDate());

       Update psc0p;

       Exfmt rcdSave;

       Endsr;

       //===================================================================
       // Display Filter window for documents grid.
       //===================================================================
       BegSr LoadDocFilter;

       showClrFil = *on;
       showClrSub = *on;
       scFiltStat = 'A';
       scFiltEmai = ' ';
       scSubFilSt = 'A';

       scFiltIntr = 'B';
       scFiltLang = 'All';
       scFiltAuto = 'B';
       Clear scFiltLngC;

       If Lang(1) <> ' ';

          scFiltLngC = 'All';
          x = 1;

          Dow x <= 20;
             If Lang(x) <> ' ';
                scFiltLngC = %trim(scFiltLngC) + ',' + %trim(Lang(x));
             Endif;
             x = x + 1;
          Enddo;

       Else;
          scFiltLngC = 'All,English';
       Endif;

       Clear scCatChoic;
       Clear scCatOpts;

       // Load the choices for document filter.

       Setll keySTDLL pdl00;
       Reade keySTDLL pdl00;

       Dow not %eof;

          If %scan(DLCat:scCatChoic) = 0;
             xxTBID = 'DOCS';
             xxKey  = DLCat;
             Chain(n) keySTGTL pgt00;
             If scCatChoic = ' ';
                scCatChoic = ' ,' + %trim(DLCat);
                scCatOpts  = ' ,' + %trim(GTDesc);
             Else;
                scCatChoic = %trim(scCatChoic) + ',' + %trim(DLCat);
                scCatOpts  = %trim(scCatOpts) + ',' + %trim(GTDesc);
             Endif;
          Endif;

          Reade keySTDLL pdl00;
       Enddo;

       Endsr;
       //===================================================================
       // Display Filter window for documents grid.
       //===================================================================
       BegSr ProcessFilter;

       svFiltCatg = scFiltCatg;
       svFiltDept = scFiltDept;
       svFiltStat = scFiltStat;
       svFiltEmai = scFiltEmai;
       svFiltIntr = scFiltIntr;
       svFiltLang = scFiltLang;
       svFiltAuto = scFiltAuto;

       Dou filtCancel = *on or filtApply = *on;

          scFiltDepC = 'DECO# = ' + %char(scCo#) + ' AND DECAT = ''' +
           %trim(scFiltCatg) + '''';

          Exfmt rcdFilterD;

       Enddo;

       If FiltCancel = *on;
          scFiltCatg = svFiltCatg;
          scFiltDept = svFiltDept;
          scFiltStat = svFiltStat;
          scFiltEmai = svFiltEmai;
          scFiltIntr = svFiltIntr;
          scFiltLang = svFiltLang;
          scFiltAuto = svFiltAuto;
       Endif;

       If scFiltCatg <> ' ' or scFiltDept <> ' ' or scFiltStat <> ' '
        or scFiltIntr <> 'B' or scFiltLang <> 'All' or scFiltAuto <> 'B'
        or scFiltEmai <> ' ';
          showClrFil = *on;
       Else;
          showClrFil = *off;
       Endif;

       Endsr;
       //===================================================================
       // Display Filter window for sub categories grid.
       //===================================================================
       BegSr ProcessSubFilter;

       svSubFilCt = scSubFilCt;
       svSubFilSt = scSubFilSt;

       Dou filSCancel = *on or filSApply = *on;

          Exfmt rcdFilterS;

       Enddo;

       If FilSCancel = *on;
          scSubFilCt = svSubFilCt;
          scSubFilSt = svSubFilSt;
       Endif;

       If scSubFilCt <> ' ' or scSubFilSt <> ' ';
          showClrSub = *on;
       Else;
          showClrSub = *off;
       Endif;

       Endsr;
       //===================================================================

       //===================================================================
       // Process Status change of Auto Trigger
       //===================================================================
       BegSr ProcStsChg;

       // Read from subfile
       Chain rrnAuToTrg sflAutoTrg;

       If %found;
          Chain scTrgRrn ptr0p;
          If %found(SMTRP);
             Select;
             When mTRstat = 'I'  and mTRname <> 'FILE RULES';
                mTRstat = 'A';
             When mTRstat = 'A'  and mTRname <> 'FILE RULES';
                mTRstat = 'I';
             When mTRstat = 'I' and TriggerExists(mTRco#:mTRname:mTRseq#);
                mTRstat = 'A';
             When mTRstat = 'I';
                mTRstat = 'P';
                Chain (mTRco#:mTRname:mTRseq#) ptr0frq;
                If %found;
                   BuildFileTrg(qTRfile:qTRlibrary:'*SUBMIT');
                Endif;
                PendTrigNotice();
             When mTRstat = 'A' and not TriggerExists(mTRco#:mTRname:mTRseq#);
                mTRstat = 'I';
             When mTRstat = 'A';
                mTRstat = 'i';
                Chain (mTRco#:mTRname:mTRseq#) ptr0frq;
                If %found;
                   BuildFileTrg(qTRfile:qTRlibrary:'*SUBMIT');
                Endif;
                PendTrigNotice('I');
             Other;
                mTRstat = 'I';
             Endsl;
             mTRpgm  = q1Pgm;
             mTRuser = q1User;
             mTRtime = Timeto6(SystemTime());
             mTRdt   = Dateto7(SystemDate());
             Update ptr0p;

             Exsr ProcStsChgFrq;

             LTRco#  = intCo#;
             LTRiPgm = q1Pgm;
             LTRiUsr = q1User;
             LTRiTim = Timeto6(SystemTime());
             LTRiDt  = Dateto7(SystemDate());
             LTRseq# = mTRseq#;
             LTRname = mTRname;
             If mTRstat = 'I';
                LTRaction = 'Inactivate';
             Else;
                LTRaction = 'Activate';
             Endif;
             Write ptr0log;
          Endif;
       Endif;

       Endsr;

       //===================================================================
       // Process Status change of Auto Trigger - Frequency
       //===================================================================
       Begsr ProcStsChgFrq;
        Setll (mTRco#:mTRname:mTRseq#) ptr0frq;
        Dou %eof;
           Reade (mTRco#:mTRname:mTRseq#) ptr0frq;
           Select;
           When %eof;
              Iter;
           Endsl;
           Select;
           When mTRstat = 'P';
              qTRstat = 'A';
           Other;
              qTRstat = Upper(mTRstat);
           Endsl;
           qTRpgm  = mTRpgm;
           qTRuser = mTRuser;
           qTRtime = mTRtime;
           qTRdt   = mTRdt;
           Update ptr0frq;
        Enddo;
       Endsr;

      /end-free
     p Main            e
     P*====================================================================
     P* Build claim type text for Rules
     P*====================================================================
     PBuildClmTypText  b
     DBuildClmTypText  pi                  like(xTRcond)
     D inClmTypes                          like(cTRclmType) const

     DconClmTypes      c                   'IMR'
     DstrClmTypes      s                   like(cTRclmType)
     DstrText          s                   like(xTRcond)
      /Free
        strClmTypes = inClmTypes;
        If strClmTypes = *blanks;
           strClmTypes = conClmTypes;
        Endif;
        Clear strText;
        If %scan('I':strClmTypes) > 0;
           strText = BuildRulesText(strText:*blanks:'or'
            :'Indemnity');
        Endif;
        If %scan('M':strClmTypes) > 0;
           strText = BuildRulesText(strText:*blanks:'or'
            :'Medical Only');
        Endif;
        If %scan('R':strClmTypes) > 0;
           strText = BuildRulesText(strText:*blanks:'or'
            :'Record Only');
        Endif;
        Return strText;
      /End-free
     PBuildClmTypText  e

     P*====================================================================
     P* Build policy status text for Rules
     P*====================================================================
     PBuildPolStsText  b
     DBuildPolStsText  pi            15
     D inPolSts                            like(pTRpolStsTp) const

     DstrPolSts        s             15    inz
      /Free
        Select;
        When inPolSts = 'W';
           strPolSts = 'Live';
        When inPolSts = 'Q';
           strPolSts = 'Quote';
        Endsl;
        Return strPolSts;
      /End-free
     PBuildPolStsText  e

     P*====================================================================
     P* Build textual detail for Rules
     P*====================================================================
     PBuildRulesText   b
     DBuildRulesText   pi                  like(xTRcond)
     D inString                            like(xTRcond) const
     D inPrefix                     100                  const
     D inConjunction                 10                  const
     D inValue                             like(xTRcond) const

     DstrText          s                   like(xTRcond)
     DstrValue         s                   like(xTRcond) inz
      /Free
       strText = %trim(inString);
       Select;
       When inValue = *blanks;
       When inPrefix <> *blank and %scan(%trim(inPrefix):strText) = 0;
          strValue = %trim(inPrefix) + ' ' + %trim(inValue);
       Other;
          strValue = %trim(inValue);
       Endsl;
       Select;
       When inValue = *blanks;
       When strText = *blanks;
         // strText = %trim(inPrefix);
       Other;
          strText = %trim(strText) + ' ' + %trim(inConjunction);
       Endsl;
       strText = %trim(strText) + ' ' + %trim(strValue);
       Return strText;
      /End-free
     PBuildRulesText   e

     P*====================================================================
     P* Build textual detail for Rules tables
     P*====================================================================
     PBuildTableText   b
     DBuildTableText   pi                  like(xTRcond)
     D inTable                             like(tTRtable) const
     D inPrefix                     100                  const

     DstrText          s                   like(xTRcond)
     DstrValue         s                   like(xTRcond)
      /Free
        Clear strText;
        Setll (mTRco#:mTRname:mTRseq#:inTable) smtrptbl;
        Dou %eof;
           Reade(n) (mTRco#:mTRname:mTRseq#:inTable) smtrptbl;
           If %eof;
              Iter;
           Endif;
           Exsr GetDescription;
           strText = BuildRulesText(strText:inPrefix:'or'
            :%trim(strValue));
        Enddo;
        Return strText;

        //=================================================================
        Begsr GetDescription;
         Clear strValue;
         Select;
         When inTable = 'Claim Indicator';
            strValue = GetSysGenDesc(mTRco#:'AXCI':%trim(tTRkey));
         When inTable = 'Nature';
            strValue = NatureCdDs(mTRco#:%trim(tTRkey));
         When inTable = 'Part';
            strValue = PartCdDs(mTRco#:%trim(tTRkey));
         When inTable = 'Cause';
            strValue = CauseCdDs(mTRco#:%trim(tTRkey));
         When inTable = 'Payment';
            strValue = BR2CodeDs(mTRco#:%trim(tTRkey));
         When inTable = 'Reserve';
            strValue = ResTypCdDs(mTRco#:%trim(tTRkey));
         When inTable = 'States';
            Clear STname;
            Chain tTRkey pst01;
            strValue = STname;
         When inTable = 'NCCI Injury';
            xxTbId = '  NC';
            If #ofTrgJSt = 1;
               xxTbId = %trim(strState) + 'NC';
               Chain (mTRco#:xxTbId) pgtw0;
               If not %found;
                  xxTbId = '  NC';
               Endif;
            Endif;
            strValue = GetGenCodeDs(mTRco#:xxTbId:%trim(tTRkey));
         When inTable = 'Quote Status';
          //strValue = ResTypeCdDs(mTRco#:%trim(tTRkey));
         Endsl;
         If strValue = *blanks;
            strValue = %trim(tTRkey);
         Endif;
        Endsr;
      /End-free
     PBuildTableText   e

     p DftFltrStat     b
     d DftFltrStat     pi

      /Free
       Exec sql
       set option commit = *none;
       sqlQuery = 'Update wtvipcfg +
                   set VIstat = ''I'' +
                   where VIcfgid = ''FilterVisitStatus'' +
                   and VIco# = '+%char(intCo#)+' +
                   and VIcat in (''Claims'',''Policy'',''Agent'','' '')';

       Exec sql execute immediate : sqlQuery;

       If scPolicy <> ' ';
          Dow scPolicy <> ' ';
             Clear strTypes;
             Clear strCat;
             strTypes = ParseString(scPolicy:',':1:'1');
             strCat = 'Policy';
             Exsr Write_VstRec;
          Enddo;
       Endif;
       If scClm <> ' ';
          Dow scClm <> ' ';
             Clear strTypes;
             Clear strCat;
             strTypes = ParseString(scClm:',':1:'1');
             strCat = 'Claims';
             Exsr Write_VstRec;
          Enddo;
       Endif;
       If scAgt <> ' ';
          Dow scAgt <> ' ';
             Clear strTypes;
             Clear strCat;
             strTypes = ParseString(scAgt:',':1:'1');
             strCat = 'Agent';
              Exsr Write_VstRec;
          Enddo;
       Endif;
       If scOthr <> ' ';
          Dow scOthr <> ' ';
             Clear strTypes;
             Clear strCat;
             strTypes = ParseString(scOthr:',':1:'1');
             strCat = ' ';
              Exsr Write_VstRec;
          Enddo;
       Endif;

       Begsr Write_VstRec;
          Exec SQL select count(*) into :count from wtvipcfg
           where VICO# = :intco# and VIcat = :strCat and
           VIcfgVal = :strTypes;
           If count > 0;
              Exec sql Update wtvipcfg set VIstat = 'A' where
              VIco# = :intco# and VIcat = :strCat and
              VIcfgVal = :strTypes
              and VIcfgid = 'FilterVisitStatus';
           Else;
              Cfg_VIcat = strCat;
              Cfg_VIcfgVal = strTypes;
              Cfg_VIcfgid = 'FilterVisitStatus';
              Cfg_VIco#    = intCo#;
              Cfg_VItype   = VItype;
              Cfg_VIuser   = Q1user;
              Cfg_VIpgm    = Q1pgm;
              Cfg_VIdt     = Dateto7(SystemDate());
              Cfg_VItime   = Timeto6(SystemTime());
              Cfg_VIiusr   = Q1user;
              Cfg_VIipgm   = Q1PGM;
              Cfg_VIidt    = Dateto7(SystemDate());
              Cfg_VIitim   = Timeto6(SystemTime());
              Cfg_VIstat   = VIstat;
              Write pvi0p_cfg;
           Endif;
       Endsr;
      /End-free
     p DftFltrStat     e

       //===================================================================
       // Check for Auto Letter Trigger Filter
       //===================================================================
     p FiltAutoLet     b
     d FiltAutoLet     pi              n

      /free

       // Auto Letter Document
       If ssAutlDoc <> ' ' and %scan(%trim(ssAutlDoc):%Trim(mCLdcid)) = 0;
          Return *off;
       Endif;

       // Auto Letter Status
       If ssAutlSts <> ' ' and ssAutlSts <> mCLstat;
          Return *off;
       Endif;

       // Auto Letter Group
       If ssAutlGrp <> 0;
          Chain (mCLco#: mCLalsq) pcl0grp;
          If %Found(SACLLGRP);
             DoU %EOF(SACLLGRP);
                If ssAutlGrp = gCLfnd;
                   Leave;
                EndIf;
                ReadE (mCLco#: mCLalsq) pcl0grp;
                If %EOF(SACLLGRP);
                   Return *Off;
                EndIf;
             EndDo;
          Endif;
       Endif;

       // Auto Letter Jurisdiction State
       If ssAutlJst <> ' ';
          Chain (mCLco#: mCLalsq) pcl0jst;
          If %Found(SACLLJST);
             DoU %EOF(SACLLJST);
                If ssAutlJst = sCLjst;
                   Leave;
                EndIf;
                ReadE (mCLco#: mCLalsq) pcl0jst;
                If %EOF(SACLLJST);
                   Return *Off;
                EndIf;
             EndDo;
          Endif;
       Endif;

       // Auto Letter Office #
       If ssAutlOff <> ' ';
          Chain (mCLco#: mCLalsq) pcl0off;
          If %Found(SACLLOFF);
             DoU %EOF(SACLLOFF);
                If ssAutlOff = oCLoff#;
                   Leave;
                EndIf;
                ReadE (mCLco#: mCLalsq) pcl0off;
                If %EOF(SACLLOFF);
                   Return *Off;
                EndIf;
             EndDo;
          Endif;
       Endif;

       Return *on;
      /end-free

     p FiltAutoLet     e

       //===================================================================
       // Get external role description
       //===================================================================
     p GetExtRoleDesc  b
     d GetExtRoleDesc  pi            50
     d  inRole                       10

     doutDesc          s             50    inz
      /Free
       Chain(n) (intCo#:'AXRO':inRole) pgt00;
       Select;
       When %found;
          outDesc = %trim(GTDesc);
       Other;
          outDesc = %trim(inRole);
       Endsl;
       Return outDesc;
      /End-free
     p GetExtRoleDesc  e

       //===================================================================
       // Get recipient description
       //===================================================================
     p GetRecipDesc    b
     d GetRecipDesc    pi            50
     d  inRole                             like(TRrecip)
     d  inType                             like(TRtype)

     doutDesc          s             50    inz
      /Free
       Chain(n) (intCo#:'AXRO':inRole) pgt00;
       Select;
       When inType = 'Individual';
          outDesc = %trim(GetUserName(inRole));
       When %found;
          outDesc = %trim(GTDesc);
       Other;
          outDesc = %trim(inRole);
       Endsl;
       Return outDesc;
      /End-free
     p GetRecipDesc    e

       //===================================================================
       // Load Documents Grid/Tab.
       //===================================================================
     p LoadAutoLet     b
     d LoadAutoLet     pi

      /free

       // Clear the subfile.
       dspAutoLet = *off;
       clrAutoLet = *on;
       Write rcdAutoLet;
       clrAutoLet = *off;
       dspAutoLet = *on;
       rrnAutoLet = 0;
       clear dbrrnA;

       // Load the Auto Trigger subfile.
       Setll intCo# pcl00;
       Reade intCo# pcl00;
       Dow not %eof(SACLL);

          If FiltAutoLet() = *on;
             scAutlSeq# = mCLalSq;

             xxTBID = 'DOCS';
             xxKey  = mCLcat;
             Chain(n) keySTGTL pgt00;
             If %found;
                scAutlCat  = %trim(GTDesc);
             Else;
                scAutlCat  = %trim(DLCat);
             Endif;

             xxCat  = %trim(mCLcat);
             xxSubC = %trim(mCLsubC);
             Chain(n) keySTDEL pde00;
             If %found;
                scAutlSub = %trim(DEDesc);
             Else;
                scAutlSub = %trim(mCLSubC);
             Endif;

             scAutlDoc  = mCLdcid;
             scAutlLang = mCLlang;

             If mCLstat = 'A';
                scAutlStat = 'Active';
                scChgStsT = 'Inactivate';
             Else;
                scAutlStat = 'Inactive';
                scChgStsT = 'Activate';
             Endif;

             scAutlRrn = dbrrnA;

             rrnAutoLet = rrnAutoLet + 1;

             clear scAutRwSel;
             If (rrnAutoLet = rrnAutlRow) or (rrnAutoLet = 1 and
                rrnAutlRow =    0);
                scAutRwSel = 'A';
             Endif;

             Write sflAutoLet;
          Endif;

          Reade intCo# pcl00;
       Enddo;

      /end-free

     p LoadAutoLet     e
       //===================================================================
       // Load Application Configuration Setup info
       //===================================================================
     p LoadConfInfo    b
     d LoadConFinfo    pi

      /free

       // Application Configuration setup
       dsbAppCfg = *off;

       clear rcdSearch;

       If dspMode = *on;
          dsbAppCfg = *on;
       Endif;
       // Claim Inquiry
       strKey = 'ClmInqAut';
       Chain strKey pac00;
       If not %found;
          clear *all pac00;
          intRrn = 0;
       Else;
          intRrn = dbrrn;
       Endif;
       saveDT = ACdt;
       saveTM = ACtime;
       intStart = 0;
       LoadData(intStart);

       // Claim Update
       strKey = 'ClmUpdAut';
       Chain strKey pac00;
       If not %found;
          clear *all pac00;
          intRrn1 = 0;
       Else;
          intRrn1 = dbrrn;
       Endif;
       saveDT1 = ACdt;
       saveTM1 = ACtime;
       intStart = 10;
       LoadData(intStart);

       // Policy Inquiry
       strKey = 'PolInqAut';
       Chain strKey pac00;
       If not %found;
          clear *all pac00;
          intRrn2 = 0;
       Else;
          intRrn2 = dbrrn;
       Endif;
       saveDT2 = ACdt;
       saveTM2 = ACtime;
       intStart = 20;
       LoadData(intStart);

       // Policy Update
       strKey = 'PolUpdAut';
       Chain strKey pac00;
       If not %found;
          clear *all pac00;
          intRrn3 = 0;
       Else;
          intRrn3 = dbrrn;
       Endif;
       saveDT3 = ACdt;
       saveTM3 = ACtime;
       intStart = 30;
       LoadData(intStart);

       // Quote Inquiry
       strKey = 'QteInqAut';
       Chain strKey pac00;
       If not %found;
          clear *all pac00;
          intRrn4 = 0;
       Else;
          intRrn4 = dbrrn;
       Endif;
       saveDT4 = ACdt;
       saveTM4 = ACtime;
       intStart = 40;
       LoadData(intStart);

       // Quote Update
       strKey = 'QteUpdAut';
       Chain strKey pac00;
       If not %found;
          clear *all pac00;
          intRrn5 = 0;
       Else;
          intRrn5 = dbrrn;
       Endif;
       saveDT5 = ACdt;
       saveTM5 = ACtime;
       intStart = 50;
       LoadData(intStart);

       //*** Beg Add *** 06/26/23 ******************************************
       // Group Configuration Security / User Access Controls
       strKey = 'GrpCfgAut';
       Chain strKey pac00;
       If not %found;
          clear *all pac00;
          intRrn6 = 0;
       Else;
          intRrn6 = dbrrn;
       Endif;
       saveDT6 = ACdt;
       saveTM6 = ACtime;
       intStart = 60;
       LoadData(intStart);
       //*** End Add *** 06/26/23 ******************************************

       Clear sprrn;
       Clear SPpathExt;
       Chain ('Rule Report') psp00;
       scPathExt  = SPpathExt;

      /end-free
     p LoadConfInfo    e

       //===================================================================
       // Load Dashboard Tab
       //===================================================================
     p LoadDashboard   b
     d LoadDashboard   pi

     dstrSqlCursor     s           1000

     ddsSMDBP          ds
     D DBdesc                       100
     D DBitem                        25
     D DBurl                        256
     D DBsizeH                        3  0
     D DBsizeW                        3  0
     D DBtag                        100
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
     D DBStat                         1
      *** End Add ***** 07/16/24 *** WC2972 ****************************
     D DBrrn                          9  0

     ddsSMDRP          ds
     D DRrole                        25
     D*DRrrn                          9  0

      /free
       // Clear the subfiles
       dspDashCfg = *off;
       dspDashUsr = *off;
       dspUsrItem = *off;
       clrDashCfg = *on;
       clrDashUsr = *on;
       clrUsrItem = *on;
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       dspGrpSel  = *off;
       clrGrpSel  = *on;
      *** End Add ***** 07/16/24 *** WC2972 ****************************
       Write rcdDashBrd;
       dspDashCfg = *on;
       dspDashUsr = *on;
       dspUsrItem = *on;
       clrDashCfg = *off;
       clrDashUsr = *off;
       clrUsrItem = *off;
       rrnDashCfg = 0;
       rrnDashUsr = 0;
       rrnUsrItem = 0;
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       dspGrpSel  = *on;
       clrGrpSel  = *off;
       rrnGrpSel  = 0;

       //strSqlCursor = 'select DBdesc, DBitem, DBurl, DBsizeH, DBsizeW, ' +
       // 'DBtag, ' +
       // 'rrn(SMDBP) from SMDBP ' +
       // 'where DBtype = ''iFrame'' and DBstat <> ''E''';

       strSqlCursor = 'select DBdesc, DBitem, DBurl, DBsizeH, DBsizeW, ' +
        'DBtag, DBStat, ' +
        'rrn(SMDBP) from SMDBP ' +
        'where DBtype = ''iFrame'' and DBstat <> ''E'' Order by DBStat';
      *** End Add ***** 07/16/24 *** WC2972 ****************************
       Exec SQL Prepare sqlDB From :strSqlCursor;
       Exec SQL Declare cursorDB Cursor For sqlDB;
       Exec SQL Open cursorDB;
       Dou SQLSTT <> '00000';
          Exec SQL Fetch next From cursorDB into :dsSMDBP;
          Select;
          When SQLSTT <> '00000';
             Iter;
          Endsl;
          scDesc = DBdesc;
          scTag  = DBtag;
          scItem = DBitem;
          scURL  = DBurl;
          scSizeHgt = %editc(GetWidgetHeight('iFrame':DBsizeH):'Z')+'px';
          scSizeWth = %editc(GetWidgetWidth('iFrame':DBsizeW):'Z')+'px';
          scDashRRN = DBrrn;
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
          Select;
          When DBstat = 'A';
             scDBStatus = 'Active';
          When DBstat = 'I';
             scDBStatus = 'Inactive';
          EndSl;
      *** End Add ***** 07/16/24 *** WC2972 ****************************
          If dspMode = *on;
             visDashb = *on;
          Endif;
          rrnDashCfg += 1;
          Write sflDashCfg;
       Enddo;
       Exec SQL Close cursorDB;
       rrnDashCfg = 1;

       scRole = 'Claims';
       rrnDashUsr += 1;
       Write sflDashUsr;
       scRole = 'Policy';
       rrnDashUsr += 1;
       Write sflDashUsr;
       scRole = 'Executive';
       rrnDashUsr += 1;
       Write sflDashUsr;
       strSqlCursor = 'select distinct DRrole ' +
        'from SMDRP ' +
        'where DRrole not in (''Claims'',''Policy'',''Executive'') ' +
        'order by DRrole ';
       Exec SQL Prepare sqlDR From :strSqlCursor;
       Exec SQL Declare cursorDR Cursor For sqlDR;
       Exec SQL Open cursorDR;
       Dou SQLSTT <> '00000';
          Exec SQL Fetch next From cursorDR into :dsSMDRP;
          Select;
          When SQLSTT <> '00000';
             Iter;
          Endsl;
          scRole = DRrole;
          rrnDashUsr += 1;
          Write sflDashUsr;
       Enddo;
       Exec SQL Close cursorDR;
       rrnDashUsr = 1;
      /end-free

      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
      // Load Group Grid

       Chain(n) intCo# pgs00;
       If %Found(WXGSPDSH);
          Open WMFNL;
          DoU %Eof(WXGSPDSH);
             scGroup# = GSfnd;
             scIEfl = GSIEfl;
             Chain (intCo#:GSfnd) pfn00;
             If %found(WMFNL);
                scGrpName = FNname;
                scGrpTest = FNtest;
                scGrpExpDt = CvtToDate(FNExDt);
             EndIf;

             Select;
             When GSIEfl = 'I';
                scStatus = 'Include';
                scIncExcT = 'Exclude';
             When GSIEfl = 'E';
                scStatus = 'Exclude';
                scIncExcT = 'Include';
             EndSl;

             mgIncExcV = ' ';
             erIncExcV = *Off;
             rrnGrpSel  = rrnGrpSel + 1;
             write sflGrpSel;

             ReadE(n) intCo# pgs00;
          EndDo;
           Close WMFNL;
       Else;
          Open WMFNL;
          Chain (intCo#) pfn00;
          If %found(WMFNL);
             DoU %Eof(WMFNL);
                scGroup# = FNfnd;
                scGrpName = FNname;
                scGrpTest = FNtest;
                scGrpExpDt = CvtToDate(FNExDt);

                mgIncExcV = ' ';
                erIncExcV = *Off;
                rrnGrpSel  = rrnGrpSel + 1;
                write sflGrpSel;

                ReadE(n) intCo# pfn00;
             EndDo;
          EndIf;
          Close WMFNL;
       EndIf;

          intSelGrp = rrnGrpSel;
          Select;
          When scIncAll = *On;
               scIncAll = *off;
               UpdGrpSel('I');

          When scExclAll = *On;
               scExclAll = *off;
               UpdGrpSel('E');
          EndSl;


      *** End Add ***** 07/16/24 *** WC2972 ****************************

     p LoadDashboard   e

       //===================================================================
       // Load Documents Grid/Tab.
       //===================================================================
     p LoadDocuments   b
     d LoadDocuments   pi

      /free

       // Clear the subfile.
       dspDoc = *off;
       dspDocCtl = *off;
       clrDoc = *on;
       dspSub = *off;
       dspSubCtl = *off;
       clrSub = *on;
       dspPrin = *off;
       dspPrinCtl = *off;
       clrPrin = *on;
       Write rcdLetters;
       clrDoc = *off;
       dspDoc = *on;
       dspDocCtl = *on;
       rrnDoc = 0;
       clrSub = *off;
       dspSub = *on;
       dspSubCtl = *on;
       rrnSub = 0;
       dspPrin = *on;
       dspPrinCtl = *on;
       clrPrin = *off;
       rrnPrin = 0;

       // Load the documents subfile.

       Setll keySTDLL pdl00;
       Reade keySTDLL pdl00;

       Dow not %eof;


          If (%scan(%trim(DLCat):scFiltCatg) <> 0 or scFiltCatg = ' ')
           and (%scan(%trim(DLSubC):scFiltDept) <> 0  or scFiltDept = ' ')
           and (DLStat = scFiltStat or scFiltStat = ' ')
           and (DLemlClmt = scFiltEmai or scFiltEmai = ' '
             or (DLemlClmt = ' ' and scFiltEmai = 'N '))
           and (DLIntr = scFiltIntr or scFiltIntr = 'B')
           and (DLLang = scFiltLang or scFiltLang = 'All')
           and (DLauto = scFiltAuto or scFiltAuto = 'B');

             xxTBID = 'DOCS';
             xxKey  = DLCat;
             Chain(n) keySTGTL pgt00;
             If %found;
                OutDocCat  = %trim(GTDesc);
             Else;
                OutDocCat  = %trim(DLCat);
             Endif;

             xxCat  = %trim(DLCat);
             xxSubC = %trim(DLSubC);
             Chain(n) keySTDEL pde00;
             If %found;
                OutDocSubC = %trim(DEDesc);
             Else;
                OutDocSubC = %trim(DLSubC);
             Endif;
             OutDocType = %trim(DLType);

             OutDocDesc = %trim(DLDesc);
             OutDocID   = %trim(DLDcid);
             OutDocLang = %trim(DLLang);
             OutDocINID = %trim(DLINID);

             Clear OutDocPrin;
             Chain DLPrin pp100;
             If %found;
                OutDocPrin = %trim(P1Name);
             Endif;

             If DLIntr = 'Y';
                OutDocIntr = 'LiveDoc';
                OutDocName = %trim(DLINID);
             //*** begin add ***  09/25/24  *********************************
                xxBegPos = %scan('.':DLINID);
                If xxBegPos <> 0;
                   OutDocName = %trim(%subst(DLINID:1:xxBegPos-1));
                Endif;
             //*** end add *****  09/25/24  *********************************
             Else;
                OutDocName = %trim(DLPath) + %trim(DLDcID);
                Clear OutDocIntr;
                If DLIntr = 'F';
                   OutDocIntr = 'Form';
                Endif;
             Endif;

             Clear OutDocTurn;
             If DLTurn = 'Y';
                OutDocTurn = 'Yes';
             Endif;

             If DLStat = 'A';
                OutDocStat = 'Active';
             Else;
                OutDocStat = 'Inactive';
             Endif;

             OutDocAuto = 'No';
             If DLauto = 'Y';
                OutDocAuto = 'Yes';
             EndIf;

             OutDocCopy = DLCopy;
             OutDocRMCD = DLRMCD;

             OutDocRRN  = dbrrn2;

             rrnDoc = rrnDoc + 1;

             If rrnDoc = 1;
               scDocGridS = *On;
             Else;
               scDocGridS = *Off;
             Endif;
             //*** Beg Add 08/12/24 ********************************************
             scDocGrids = *off;
             If (rrnDoc = rrnDocRow) or (rrnDoc = 1 and
                rrnDoc =    0);
                scDocGrids = *on;
             Endif;
             //*** End Add 08/12/24 ********************************************

             OutEmailC = DLEmlClmt;
NP01         OutCopyE  = DLCopyE;

             Write sflDocGrid;

          Endif;

          Reade keySTDLL pdl00;
       Enddo;

       rrnDoc = 1;

       // Load the departments subfile.

       Setll key1STDEL pde00;
       Reade key1STDEL pde00;

       Dow not %eof;

          If (%scan(%trim(DECat):scSubFilCt) <> 0
           or scSubFilCt = ' ') and (DEStat = scSubFilSt or scSubFilSt = ' ');

             xxTBID = 'DOCS';
             xxKey  = DECat;
             Chain(n) keySTGTL pgt00;
             If %found;
                OutSubCat  = %trim(GTDesc);
             Else;
                OutSubCat  = %trim(DECat);
             Endif;

             OutSubDesc = %trim(DEDesc);

             OutSubCode = %trim(DESubC);
             If DEStat = 'A';
                OutSubStat = 'Active';
             Else;
                OutSubStat = 'Inactive';
             Endif;

             OutSubRRN  = dbrrn3;

             rrnSub = rrnSub + 1;
             Write sflSubGrid;

          Endif;

          Reade key1STDEL pde00;
       Enddo;

       rrnSub = 1;

       // Load the printers subfile.

       Setll *loval pp100;
       Read pp100;

       Dow not %eof;


             OutPrtKey  = %trim(P1Key);
             OutPrtName = %trim(P1Name);
             OutPrtDesc = %trim(P1Desc);
             OutPrtUNCN = %trim(P1UNCN);

             Select;
             When P1Type = 'N';
                OutPrtType = 'Network';
             When P1Type = 'O';
                OutPrtType = 'OutQueue';
             Other;
                Clear OutPrtType;
             Endsl;

             If P1Stat = 'A';
                OutPrtStat = 'Active';
             Else;
                OutSubStat = 'Inactive';
             Endif;

             OutPrtRRN  = dbrrn8;

             rrnPrin = rrnPrin + 1;
             Write sflPrinter;

          Read pp100;
       Enddo;

       rrnPrin = 1;

      /end-free

     p LoadDocuments   e

       //===================================================================
       // Add Letters to document setup file.
       //===================================================================
     p DocWindow       b
     d DocWindow       pi
     D inDocRRN                       9  0 const options(*nopass)

     D xxRRN           s              9  0 inz(0)
     d error           s               n   inz(*off)
     d docAddMode      s               n   inz(*off)
     d str100          s            100    inz(*blanks)

      /free

       If %parms >= 1;
          xxRRN = inDocRRN;
       Endif;

       scCo# = intCo#;
       docAddMode = *off;
       docEdtMode = *off;

       IF SCFlg3 = 'Y';
          disbPrint = *off;
          Clear tipDocPrin;
         tipDocPrin = 'This is the printer defaulted for the document if no +
          printer is specified in user parameters.';
          Clear tipDocCopy;
          Clear tipDocHard;
       Else;
          disbPrint = *on;
          tipDocPrin = 'The print module for Scriptura must be enabled to +
           access this field.';
          tipDocCopy = 'The print module for Scriptura must be enabled to +
           access this field.';
          tipDocHard = 'The print module for Scriptura must be enabled to +
           access this field.';
       Endif;

       //* Set the options for the language dropdown from the module file.
       Clear scLangChoi;

       If Lang(1) <> ' ';

          scLangChoi = %trim(Lang(1));
          saveLang = %trim(Lang(1));
          x = 2;

          Dow x <= 20;
             If Lang(x) <> ' ';
                scLangChoi = %trim(scLangChoi) + ',' + %trim(Lang(x));
             Endif;
             x = x + 1;
          Enddo;

       Else;
          scLangChoi = 'English';
          saveLang = 'English';
       Endif;

       If xxRRN <> 0;
          Chain(n) xxRRN pdl0p;
          docEdtMode = *on;
       Else;
          docAddMode = *on;
       Endif;

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou docAddMode = *off;

          //* Clear screen fields prior to display if in Add Mode.
          If docAddMode = *on;
             Clear scDocCatg;
             Clear scDocSubC;
             Clear scDocSchem;
             Clear scFormPath;
             Clear scFormID;
             Clear scDocDesc;
             Clear scDocTurn;
             Clear scDocID;
             Clear scDocPath;
             scDocCopy = 1;
             Clear scDocRMCD;
             scDocType = SCFlg4;
             Clear scDocHard;
             Clear scDocPrin;
             scDocLang = saveLang;
             Clear scDocAuto;
             Clear scDocTray;
             Clear scEmlSubs;
             Clear scEmlBodys;
             Clear scDistTyp;
             Clear scEmlClmt;
             Clear scEmlUsro;
             scDocStat = 'A';
             dspAddNext = *on;
             posCancel = 555;
             posAddNext = 665;
             //*** Beg Add ***** 01/16/24 *** 153WAD ********************
             Clear scCopyE;
             //*** Beg Add ***** 01/16/24 *** 153WAD ********************

             //** Begin Add  ***  10/04/22 WC2838 ************************
             Clear DLCat;
             Clear dsEmail;
             ptrEmail = %addr(dsEmail);
             str100 = '@@@INITIALIZE@@@';
             //*** Beg Chg ***** 09/10/24 *********************************
             //*** scTokenCrt = 'job = ''' + %char(q1JobN) + '''';
             //*** GetTokensEmail(ptrEmail:str100);
             FltEmailToken('Documents');
             //*** End Chg ***** 09/10/24 *********************************
             //** End   Add  ***  10/04/22 WC2838 ************************

             //*** Beg Add *** 07/30/24 *************************************
             Clear scStfRole2;
             //*** End Add *** 07/30/24 *************************************
          Else;
             scDocCatg = DLCat;
             scDocSubC = DLSubC;
             scDocSchem = DLType;
             scFormPath = DLFormP;
             scFormID = DLFormT;
          //*** begin add ***  09/25/24  *********************************
             xxBegPos = %scan('.':DLFormT);
             If xxBegPos <> 0;
                scFormID = %trim(%subst(DLFormT:1:xxBegPos-1));
             Endif;
          //*** end add *****  09/25/24  *********************************
             scDocDesc = DLDesc;
             scDocTurn = DLTurn;
             scDocCopy = DLCopy;
             scDocRMCD = DLRMCD;
             scDocPath = DLPath;
             Clear dsEmail;
             ptrEmail = %addr(dsEmail);
             xxTBID = 'DOCS';

             //** Begin Del  ***  10/04/22 WC2838 ************************
             //*****xxKey  = 'DLDcId';
             //** End   Del  ***  10/04/22 WC2838 ************************

             //** Begin Add  ***  10/04/22 WC2838 ************************
             xxKey  = DLCat;
             //** End   Add  ***  10/04/22 WC2838 ************************

             Chain(n) keySTGTL pgt00;
             //*** Beg Chg ***** 05/25/23 *******************************
             //***If %found;
             //***   str100 = %trim(GTdesc);
             //** Begin Add  ***  10/04/22 WC2838 ************************
             //***Else;
             //***   Clear str100;
             //** End   Add  ***  10/04/22 WC2838 ************************
             //***Endif;
             Clear str100;
             str100 = DLCat;
             //*** End Chg ***** 05/25/23 *******************************

             //*** Beg Add ***** 01/16/24 *** 153WAD ********************
             scCopyE = DLCopyE;
             //*** Beg Add ***** 01/16/24 *** 153WAD ********************
             //*** Beg Chg ***** 09/10/24 *********************************
             //*** scTokenCrt = 'job = ''' + %char(q1JobN) + '''';
             //*** GetTokensEmail(ptrEmail:str100);
             FltEmailToken('Documents');
             //*** End Chg ***** 09/10/24 *********************************
             scEmlSubs = DLEmlSub;
             scEmlBodys = DLEmlBody;
             scEmlClmt = DLEmlClmt;
             scEmlUsro = DLEmlUsro;
             scMail = DLmail;
             If DLcat = 'PS';
                visEmlClmt = *off;
             Else;
                visEmlClmt = *on;
             Endif;
             // Load the Actions
             Clear crtDLCritVal;
             Clear scDisttyp;
             Setll (DLco#:DLcat:DLsubC:DLdcid:DLinid:DLlang:conDist)
                pdl0crt;
             Dou %eof(STDLLCRT);
             Reade(n) (DLco#:DLcat:DLsubC:DLdcid:DLinid:DLlang:
                conDist) pdl0crt;
                   If scDisttyp <> *blank;
                      scDisttyp = %trim(scDisttyp) + ',';
                   Endif;
                   scDistTyp = %trim(scDistTyp) + crtDLCritVal;
             Enddo;

             //*** Beg Add *** 07/30/24 *************************************
             // Load Staff Selection
             Clear crtD1CritVal;
             Clear scStfRole2;
             Setll (DLco#:DLcat:DLsubC:DLdcid:DLinid:DLlang:conStaf)
                pd101crt;
             Dou %eof(STDLLCRT1);
             Reade(n) (DLco#:DLcat:DLsubC:DLdcid:DLinid:DLlang:
                conStaf) pd101crt;
                   If scStfRole2 <> *blank;
                      scStfRole2 = %trim(scStfRole2) + ',';
                   Endif;
                   scStfRole2 = %trim(scStfRole2) + crtD1CritVal;
             Enddo;

             //*** End Add *** 07/30/24 *************************************
             If DLIntr = 'Y';
                scDocType = 'I';
                scDocID   = DLInID;
             //*** begin add ***  09/25/24  *********************************
                xxBegPos = %scan('.':DLINID);
                If xxBegPos <> 0;
                   scDocID = %trim(%subst(DLINID:1:xxBegPos-1));
                Endif;
             //*** end add *****  09/25/24  *********************************
             Else;
                scDocType = 'N';
                scDocID   = DLDcid;
                If DLIntr = 'F';
                   scDocType = 'F';
                Endif;
             Endif;
             scDocHard = DLHard;
             scDocPrin = DLPrin;
             scDocLang = DLLang;
             scDocAuto = DLauto;
             scDocTray = DLTray;
             scDocStat = DLStat;
             dspAddNext = *off;
             posCancel = 665;
          Endif;

          Exsr ClearErrors;
          Clear docSave;
          Clear docCancel;
          Clear docAdd;
          Clear scDeptChoi;
          scSchmChoi = 'TSCAT = ''' + '  ' + '''';
          Clear scTrayChoi;

       //* Display popup window for the Document Setup record.

          Dou docSave = *on or docCancel = *on or docAdd = *on;

             Dou error = *off or docCancel = *on;

                If scDocCatg <> ' ';
                   scDeptChoi = 'DECO# = ' + %char(scCO#) + ' AND DECAT = ''' +
                    %trim(scDocCatg) + '''';
                   scSchmChoi = 'TSCAT = ''' + %trim(scDocCatg) + '''';
                Endif;

                If scDocPrin <> ' ';
                   scTrayChoi = 'P2KEY = ''' + %trim(scDocPrin) + '''';
                Endif;

                If scDocType = 'I';
                   scDocLabel = 'LiveDoc ID';
                Else;
                   scDocLabel = 'Template ID';
                Endif;
                scDocTypeH = scDocType;
                //** Begin Add  ***  10/04/22 WC2838 ***************
                If catSelect = *on; // When a Category Selected
                   catSelect = *off; // Reset Category Selected Indicator
                //*** Beg Chg ***** 05/25/23 *******************************
                   //***xxKey  = scDocCatg;
                   //***Chain(n) keySTGTL pgt00;
                   //***If %found;
                   //***   str100 = %trim(GTdesc);
                   //***Else;
                   //***  Clear str100;
                   //***Endif;
                   Clear str100;
                   str100 = scDocCatg;
                //*** End Chg ***** 05/25/23 *******************************

                   //*** Beg Chg ***** 09/10/24 *********************************
                   //*** scTokenCrt = 'job = ''' + %char(q1JobN) + '''';
                   //*** GetTokensEmail(ptrEmail:str100);
                   FltEmailToken('Documents');
                   //*** End Chg ***** 09/10/24 *********************************
                Endif;
                //** End   Add  ***  10/04/22 WC2838 ***************

                //*** Beg Add *** 07/30/24 *************************************
                SetStaffCH();
                //*** End Add *** 07/30/24 *************************************
                Exfmt rcdDocInfo;

                Exsr ClearErrors;

                //** Begin Add  ***  10/04/22 WC2838 ***************
                If catSelect = *on; // When a Category Selected
                   error = *on; // Force error
                   Iter; // Force to iterate Do Loop
                Endif;
                //** End   Add  ***  10/04/22 WC2838 ***************

                If docCancel = *off;
                   Exsr DocCheck;
                Endif;

             Enddo;

          Enddo;

          If docSave = *on or docAdd = *on;
             Exsr DocumentSave;
          Endif;

          If docCancel = *on or docSave = *on;
             docAddMode = *off;
          Endif;

       Enddo;

       //===================================================================
       // Clear the error fields for window.
       //===================================================================
       BegSr ClearErrors;

       error = *off;

       Clear erDocCatg;
       Clear erDocSubC;
       Clear erDocID;
       Clear erDocDesc;
       Clear erDocCopy;
       Clear erDocRMCD;
       Clear erDocPath;
       Clear erDocLang;
       Clear erFormPath;
       Clear erFormID;

       Clear msDocCatg;
       Clear msDocSubC;
       Clear msDocID;
       Clear msDocDesc;
       Clear msDocCopy;
       Clear msDocRMCD;
       Clear msDocPath;
       Clear msDocLang;
       Clear msFormPath;
       Clear msFormID;

       Endsr;
       //===================================================================

       //===================================================================
       // Edit the document setup fields.
       //===================================================================
       BegSr DocCheck;

       // Category must be selected.
       If scDocCatg = ' ';
          msDocCatg = 'A category must be selected.';
          erDocCatg = *on;
          error = *on;
       Endif;

       // Sub Category must be selected.
       If scDocSubC = ' ';
          msDocSubC = 'A sub category must be selected.';
          erDocSubC = *on;
          error = *on;
       Endif;

       // Document ID must be entered.
       If scDocID = ' ';
          msDocID = 'Template ID/LiveDoc ID must be entered.';
          erDocID = *on;
          error = *on;
       Else;

       // Document ID must have a valid file extension.
          If scDocType <> 'I';
             xxTBID = 'FILN';
          Else;
             xxTBID = 'FILI';
          Endif;

       //*** begin add ***  09/25/24  *********************************
         If scDocType <> 'I';
       //*** end add *****  09/25/24  *********************************
          xxBegPos = %scan('.':scDocID);
          If xxBegPos = 0;
             msDocID = 'Template ID/LiveDoc ID must have a valid file ' +
              'extension.';
             erDocID = *on;
             error = *on;
          Else;

             xxKey = %xlate(lo:up:%trim(%subst(scDocID:xxBegPos:4)));
             Chain(n) keySTGTL pgt00;
             If not %found;
                msDocID = 'Template ID/LiveDoc ID must have a valid file ' +
                 'extension.';
                erDocID = *on;
                error = *on;
             Endif;

          Endif;
       //*** begin add ***  09/25/24  *********************************
         Else;
         //* Verify LiveDoc template exists in D&C Cloud
             xxBegPos = %scan('.':scDocID);
             docName = scDocID;
             If xxBegPos <> 0;
                docName = %trim(%subst(scDocID:1:xxBegPos-1));
             Endif;

            If SCFlg6 = 'Y' and not ListDocs(docName);
               msDocID = 'LiveDoc ID is not setup in Design and Compose.';
               erDocID = *on;
               error = *on;
            Endif;

         Endif;
       //*** end add *****  09/25/24  *********************************

       Endif;

       // Check for template/live doc id already exists.
       If error = *off and docAddMode = *on;

          If scDocType = 'N';
             Chain(n) key1STDLL pdl00;
          Else;
             Chain(n) keySTDLL2 pdl02;
          Endif;

          If %found;
             msDocID = 'A record already exists for this Category/Sub Categor' +
              'y/Document/Language.  Edit the existing document or activate a' +
              ' previously inactivated document to allow for any necessary ch' +
              'anges.';
             erDocID = *on;
             error = *on;
             LeaveSR;
          Endif;

       Endif;

       // Description must be entered.
       If scDocDesc = ' ';
          msDocDesc = 'A description must be entered.';
          erDocDesc = *on;
          error = *on;
       Endif;

       // Remark Code must be selected.
       If scDocRMCD = ' ';
          msDocRMCD = 'A remark code must be selected.';
          erDocRMCD = *on;
          error = *on;
       Endif;

       // # of copies cannot be less than or equal to zero.
       If scDocCopy <= 0 and disbPrint = *off;
          msDocCopy = '# of copies cannot be less than or equal to zero.';
          erDocCopy = *on;
          error = *on;
       Endif;

       // Document path must be entered for non-interactive documents.
       If scDocType <> 'I';
          If scDocPath = ' ';
             msDocPath = 'Document path must be entered.';
             erDocPath = *on;
             error = *on;
          Else;

          // Document path must begin and end with a '/' character.
             strSlash  = %subst(scDocPath:1:1);
             strSlash2 = %subst(scDocPath:%len(%trim(scDocPath)):1);

             If strSlash <> '/' or strSlash2 <> '/';
                msDocPath = 'Document path must begin and end with a ''/''.';
                erDocPath = *on;
                error = *on;
             Endif;

          Endif;

       Endif;

       // Edits for interactive form path and template ID.
       If scDocType = 'F';

          // Interactive Form Template ID must be entered.
          If scFormID = ' ';
             msFormID = 'Interactive Form Template ID must be entered.';
             erFormID = *on;
             error = *on;
          Else;

          // Form ID must have a valid file extension.
       //*** begin del ***  09/25/24  *************************************
       //****xxTBID = 'FILN';
       //****
       //****If %scan('.':scFormID) = 0;
       //****   msFormID = 'Interactive Form Template ID must have a valid ' +
       //****    'file extension.';
       //****   erFormID = *on;
       //****   error = *on;
       //****Else;
       //****
       //****   xxKey = %xlate(lo:up:%trim(%subst(scFormID:%scan('.':
       //****           scFormID):10)));
       //****   Chain(n) keySTGTL pgt00;
       //****   If not %found;
       //****      msFormID = 'Interactive Form Template ID must have a valid' +
       //****       ' file extension.';
       //****      erFormID = *on;
       //****      error = *on;
       //****   Endif;
       //****
       //****Endif;
       //*** end del *****  09/25/24  *************************************

          Endif;

          // Interactive Form path must be entered.
          If scFormPath = ' ';
             msFormPath = 'Interactive Form path must be entered.';
             erFormPath = *on;
             error = *on;
          Else;

          // Interactive Form path must begin and end with a '/' character.
             strSlash  = %subst(scFormPath:1:1);
             strSlash2 = %subst(scFormPath:%len(%trim(scFormPath)):1);

             If strSlash <> '/' or strSlash2 <> '/';
                msFormPath = 'Interactive Form path must begin and end ' +
                 'with a ''/''.';
                erFormPath = *on;
                error = *on;
             Endif;

          Endif;

       Endif;
       // Document path must be entered for non-interactive documents.
       mgDocAuto = ' ';
       erDocAuto = *Off;
       If scDocType = 'I' And scDocAuto <> ' ';
          mgDocAuto = 'Interactive Letters cannot be Auto Letters';
          erDocAuto = *On;
          error = *On;
       EndIf;

       Endsr;
       //===================================================================

       //===================================================================
       // Save the document setup record.
       //===================================================================
       BegSr DocumentSave;

       If xxRRN <> 0;
          Chain xxRRN pdl0p;
       Else;
          Clear pdl0p;
          DLCo#  = intCo#;
          DLCat  = %trim(scDocCatg);
          DLSubC = %trim(scDocSubC);
          DLIPgm = Q1PGM;
          DLIUsr = Q1USER;
          DLITim = Timeto6(SystemTime());
          DLIDt  = Dateto7(SystemDate());
       Endif;

       DLType = %trim(scDocSchem);
       DLDesc = %trim(scDocDesc);
       If scDocType = 'I';
          DLIntr = 'Y';
          DLINID = %trim(scDocID);
          Clear DLPath;
          Clear DLFormP;
          Clear DLFormT;
       Else;
          DLIntr = ' ';
          DLDcid = %trim(scDocID);
          DLPath = scDocPath;
          Clear DLFormP;
          Clear DLFormT;
          If scDocType = 'F';
             DLIntr = 'F';
             DLFormP = %trim(scFormPath);
             DLFormT = %trim(scFormID);
          Endif;
       Endif;

       DLLang = scDocLang;
       DLauto = scDocAuto;
       DLPrin = scDocPrin;
       DLTray = scDocTray;
       DLCopy = scDocCopy;
       DLHard = scDocHard;
       DLRMCD = scDocRMCD;
       DLStat = scDocStat;
       DLTurn = scDocTurn;
       DLEmlClmt = scEmlClmt;
       DLEmlUsrO = scEmlUsrO;
       DLEmlSub = scEmlSubs;
       DLEmlBody = scEmlBodys;
       DLmail = scMail;
       //*** Beg Add ***** 01/16/24 *** 153WAD ********************
       DLCopyE = scCopyE;
       //*** Beg Add ***** 01/16/24 *** 153WAD ********************

       DLPgm  = Q1PGM;
       DLUser = Q1USER;
       DLTime = Timeto6(SystemTime());
       DLDt   = Dateto7(SystemDate());

       If xxRRN <> 0;
          Update pdl0p;
       Else;
          Write pdl0p;
       Endif;
       crtDLco#  = DLCo#;
       crtDLcat  = DLcat;
       crtDLsubC = DLsubC;
       crtDLdcid = DLdcid;
       crtDLinid = DLinid;
       crtDLlang = DLlang;
       crtDLcritid = %trim('Distribution');
       crtDLpgm  = q1Pgm;
       crtDLuser = q1User;
       crtDLtime = Timeto6(SystemTime());
       crtDLdt   = Dateto7(SystemDate());
       crtDLiPgm = q1Pgm;
       crtDLiUsr = q1User;
       crtDLiTim = Timeto6(SystemTime());
       crtDLiDt  = Dateto7(SystemDate());

       // Clear then add old Distribution Types
       Setll (DLco#:DLcat:DLsubC:DLdcid:DLinid:DLlang:conDist)
          pdl0crt;
       Reade (DLco#:DLcat:DLsubC:DLdcid:DLinid:DLlang:conDist)
          pdl0crt;
       Dow not %eof(STDLLCRT);
          Delete pdl0crt;
          Reade (DLco#:DLcat:DLsubC:DLdcid:DLinid:DLlang:conDist)
             pdl0crt;
       Enddo;
       strSelDistT = scDistTyp;
       //*** Beg Chg *****  02/21/22  ****************************************
       //***indexD = 1;
       //***Dow strSelDistT <> ' ';
          //***distArr(indexD) = ParseString(strSelDistT:',':1:'1');
          //***crtDLCritVal = distArr(indexD);
          //***If indexD <> 1;
          //***   indexL = %lookup(crtDLCritVal:distArr);
          //***   If indexL <> 0;
          //***      iter;
          //***   Endif;
          //***Endif;
          //***indexD = indexD + 1;
       Dow strSelDistT <> ' ';
          crtDLCritVal = ParseString(strSelDistT:',':1:'1');
       //*** End Chg *****  02/21/22  ****************************************
          write pdl0crt;
       Enddo;

       //*** Beg Add *** 07/30/24 *************************************
       // Clear then add staff Roles to STDLPCRT1
       Setll (DLco#:DLcat:DLsubC:DLdcid:DLinid:DLlang:conStaf)
          pd101crt;
       Reade (DLco#:DLcat:DLsubC:DLdcid:DLinid:DLlang:conStaf)
          pd101crt;
       Dow not %eof(STDLLCRT1);
          Delete pd101crt;
          Reade (DLco#:DLcat:DLsubC:DLdcid:DLinid:DLlang:conStaf)
             pd101crt;
       Enddo;

       strSelStafR = scStfRole2;

       // write record for staff selection
       crtD1co#  = DLCo#;
       crtD1cat  = DLcat;
       crtD1subC = DLsubC;
       crtD1dcid = DLdcid;
       crtD1inid = DLinid;
       crtD1lang = DLlang;
       crtD1critid = %trim('Staff Roles');
       crtD1pgm  = q1Pgm;
       crtD1user = q1User;
       crtD1time = Timeto6(SystemTime());
       crtD1dt   = Dateto7(SystemDate());
       crtD1iPgm = q1Pgm;
       crtD1iUsr = q1User;
       crtD1iTim = Timeto6(SystemTime());
       crtD1iDt  = Dateto7(SystemDate());
       crtD1stat = scDocStat;
       Dow strSelStafR <> ' ';
          crtD1critval = ParseString(strSelStafR:',':1:'1');
          write pd101crt;
       Enddo;
       //*** End Add *** 07/30/24 *************************************
       Endsr;
       //===================================================================

      /end-free

     p DocWindow       e
       //===================================================================
       // Add Sub Categories to department setup file.
       //===================================================================
     p SubWindow       b
     d SubWindow       pi
     D inSubRRN                       9  0 const options(*nopass)

     D xxRRN           s              9  0 inz(0)
     d error           s               n   inz(*off)
     d subAddMode      s               n   inz(*off)
     d str100          s            100    inz(*blanks)

      /free

       If %parms >= 1;
          xxRRN = inSubRRN;
       Endif;

       scCo# = intCo#;
       subAddMode = *off;
       subEdtMode = *off;

       If xxRRN <> 0;
          Chain(n) xxRRN pde0p;
          subEdtMode = *on;
       Else;
          subAddMode = *on;
       Endif;

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou subAddMode = *off;

          //* Clear screen fields prior to display if in Add Mode.
          If subAddMode = *on;
             Clear scSubCAT;
             Clear scSubSUBC;
             Clear scSubDESC;
             scSubSTAT = 'A';
             dspAddNext = *on;
             posCancel = 840;
             posAddNext = 950;
             Clear scEmlSub1;
             Clear scEmlBody1;
             Clear scEmlUsro1;
             Clear scDisttyp1;
          Else;
             scSubCAT  = DECAT;
             scSubSUBC = DESUBC;
             scSubDESC = DEDESC;
             scSubSTAT = DESTAT;
             dspAddNext = *off;
             posCancel = 950;
             Clear dsEmail;
             ptrEmail = %addr(dsEmail);
             xxTBID = 'DOCS';

             //** Begin Del  ***  10/04/22 WC2838 ************************
             //***** xxKey = 'DLDcId';
             //** End   Del  ***  10/04/22 WC2838 ************************

             //** Begin Add  ***  10/04/22 WC2838 ************************
             xxKey = DLCat;
             //** End   Add  ***  10/04/22 WC2838 ************************

             Chain(n) keySTGTL pgt00;
             //*** Beg Chg ***** 05/25/23 ********************************
             //***If %found;
             //***   str100 = %trim(GTdesc);
             //***Endif;
             str100 = DLCat;
             //*** End Chg ***** 05/25/23 ********************************
             scTokenCrt = 'job = ''' + %char(q1JobN) + '''';
             GetTokensEmail(ptrEmail:str100);
             // LoadDefaultEmaildata(ptrEmail:str100);
             scEmlSub1 = DEEmlSub;
             scEmlBody1 = DEEmlBody;
             scEmlUsro1 = DEEmlUsro;
             If DEcat = 'PS';
                visEmlClmt = *off;
             Else;
                visEmlClmt = *on;
             Endif;
             // Load the Actions
             Clear ecrDECritVal;
             Clear scDisttyp1;
             Setll (DEco#:DEcat:DEsubC:conDist) pde0crt;
             Dou %eof(STDEPCRT);
             Reade(n) (DEco#:DEcat:DEsubC:conDist) pde0crt;
                If scDisttyp1 <> *blank;
                   scDisttyp1 = %trim(scDisttyp1) + ',';
                Endif;
                scDistTyp1 = %trim(scDistTyp1) + ecrDECritVal;
             Enddo;

          Endif;

          Exsr ClearErrors;
          Clear subSave;
          Clear subCancel;
          Clear subAdd;

       //* Display popup window for the Sub Category Setup record.

          Dou subSave = *on or subCancel = *on or subAdd = *on;

             Dou error = *off or subCancel = *on;

                Exfmt rcdSubInfo;

                Exsr ClearErrors;

                If subCancel = *off;
                   Exsr SubCheck;
                Endif;

             Enddo;

          Enddo;

          If subSave = *on or subAdd = *on;
             Exsr SubCategorySave;
          Endif;

          If subCancel = *on or subSave = *on;
             subAddMode = *off;
          Endif;

       Enddo;

       //===================================================================
       // Clear the error fields for window.
       //===================================================================
       BegSr ClearErrors;

       error = *off;
       Clear erSubCat;
       Clear msSubCat;
       Clear erSubSubC;
       Clear msSubSubC;
       Clear erSubDesc;
       Clear msSubDesc;
       Clear erSubSTAT;
       Clear msSubSTAT;

       Endsr;
       //===================================================================

       //===================================================================
       // Edit the document setup fields.
       //===================================================================
       BegSr SubCheck;

       If subEdtMode = *off;

             If scSubCat  = ' ';
                msSubCat  = 'Category cannot be blank.';
                erSubCat  = *on;
             error = *on;
             Leavesr;
          Endif;

          If scSubSubC = ' ';
             msSubSubC = 'Sub category code cannot be blank.';
             erSubSubC = *on;
             error = *on;
          Else;
             Chain(n) (intCo#:scSubCat:scSubSubC) pde00;
             If %found;
                msSubSubC = 'Record already exists for sub category code.';
                erSubSubC = *on;
                error = *on;
             Endif;
          Endif;

       Endif;

       If scSubDesc = ' ';
          msSubDesc = 'Sub category description cannot be blank.';
          erSubDesc = *on;
          error = *on;
       Endif;

       Endsr;
       //===================================================================

       //===================================================================
       // Save the sub category setup record.
       //===================================================================
       BegSr SubCategorySave;

       If xxRRN <> 0;
          Chain xxRRN pde0p;
       Else;
          Clear pde0p;
          DECo#  = intCo#;
          DECat  = scSubCat;
          DESubC = scSubSubC;
          DEIUsr = Q1USER;
          DEIDt  = Dateto7(SystemDate());
       Endif;

       DEDesc = scSubDesc;
       DESTAT = scSubSTAT;

       //*** beg add *****  04/19/21  ***********************************
       DEEmlSub = scEmlSub1;
       DEEmlBody = scEmlBody1;
       DEEmlUsro = scEmlUsro1;

       ecrDEco#  = DECo#;
       ecrDEcat  = DEcat;
       ecrDEsubC = DEsubC;
       ecrDEcritid = %trim('Distribution');
       ecrDEpgm  = q1Pgm;
       ecrDEuser = q1User;
       ecrDEtime = Timeto6(SystemTime());
       ecrDEdt   = Dateto7(SystemDate());
       ecrDEiPgm = q1Pgm;
       ecrDEiUsr = q1User;
       ecrDEiTim = Timeto6(SystemTime());
       ecrDEiDt  = Dateto7(SystemDate());
       // Clear then add old Distribution Types
       Setll (DEco#:DEcat:DEsubC:conDist) pde0crt;
       Reade (DEco#:DEcat:DEsubC:conDist) pde0crt;
       Dow not %eof(STDEPCRT);
          Delete pde0crt;
          Reade (DEco#:DEcat:DEsubC:conDist) pde0crt;
       Enddo;
       strSelDistT = scDistTyp1;
       Dow strSelDistT <> ' ';
          ecrDECritVal = ParseString(strSelDistT:',':1:'1');
          write pde0crt;
       Enddo;
       //*** end add *****  04/19/21  ***********************************

       DEPgm  = Q1PGM;
       DEUser = Q1USER;
       DETime = Timeto6(SystemTime());
       DEDt   = Dateto7(SystemDate());

       If xxRRN <> 0;
          Update pde0p;
       Else;
          Write pde0p;
       Endif;

       Endsr;
       //===================================================================

      /end-free

     p SubWindow       e

       //===================================================================
       // Add Printer to Network Printer file.
       //===================================================================
     p PrtWindow       b
     d PrtWindow       pi
     D inPrtRRN                       9  0 const options(*nopass)

     D xxRRN           s              9  0 inz(0)
     d error           s               n   inz(*off)
     d prtAddMode      s               n   inz(*off)

      /free

       If %parms >= 1;
          xxRRN = inPrtRRN;
       Endif;

       prtAddMode = *off;
       prtEdtMode = *off;

       If xxRRN <> 0;
          Chain(n) xxRRN pp10p;
          prtEdtMode = *on;
       Else;
          prtAddMode = *on;
       Endif;

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou prtAddMode = *off;

          //* Clear screen fields prior to display if in Add Mode.
          If prtAddMode = *on;
             Clear scPrtKey;
             Clear scPrtName;
             Clear scPrtDesc;
             Clear scPrtUNCN;
             scPrtStat = 'A';
             dspAddNext = *on;
             //*** Beg Chg *** 05/12/23 ******************************************
             //*** posCancel = 555;
             //*** posAddNext = 665;
             posCancel = 440;
             posAddNext = 550;
             //*** End Chg *** 05/12/23 ******************************************
             scPrtType = 'N';
          Else;
             scPrtKey  = P1Key;
             scPrtName = P1Name;
             scPrtDesc = P1Desc;
             scPrtUNCN = P1UNCN;
             scPrtStat = P1Stat;
             scPrtType = P1Type;
             dspAddNext = *off;
             //*** Beg Chg *** 05/12/23 ******************************************
             //*** posCancel = 665;
             posCancel = 550;
             //*** End Chg *** 05/12/23 ******************************************
          Endif;

          Select;
          When scPrtType = 'N';
             scPrtLabel = 'Printer Key';
          When scPrtType = 'N';
             scPrtLabel = 'OutQueue Name';
          Endsl;

          Exsr LoadTray;

          Exsr ClearErrors;
          Clear prtSave;
          Clear prtCancel;
          Clear prtAdd;

       //* Display popup window for the Printer Setup record.

          Dou prtSave = *on or prtCancel = *on or prtAdd = *on;

             Dou error = *off or prtCancel = *on;

                scPrtTypeH = scPrtType;

                Exfmt rcdPrinter;

                Exsr ClearErrors;

                If prtCancel = *off;
                   Exsr PrtCheck;
                //* Process printer tray actions only if no errors present for printer setup.
                   If error = *off;

                   //* Add printer tray.
                      If addTray = *on;
                         TrayWindow(scPrtKey);
                         Exsr LoadTray;
                         addTray = *off;
                      Endif;

                   //* Edit printer tray.
                      Readc sflTray;
                      If not %eof();
                         If editTray = *on;
                            editTray = *off;
                            Update sflTray;
                            TrayWindow(scPrtKey:outTrayRRN);
                            Exsr LoadTray;
                         Endif;
                      Endif;

                   Endif;

                Endif;

             Enddo;

          Enddo;

          If prtSave = *on or prtAdd = *on;
             Exsr PrinterSave;
          Endif;

          If prtCancel = *on or prtSave = *on;
             prtAddMode = *off;
          Endif;

          If prtCancel = *on and xxRRN = 0;
             Exsr DeleteTrays;
          Endif;

       Enddo;

       //===================================================================
       // Clear the error fields for window.
       //===================================================================
       BegSr ClearErrors;

       error = *off;
       Clear erPrtKey;
       Clear msPrtKey;
       Clear erPrtName;
       Clear msPrtName;
       Clear erPrtDesc;
       Clear msPrtDesc;
       Clear erPrtUNCN;
       Clear msPrtUNCN;

       Endsr;
       //===================================================================

       //===================================================================
       // Edit the document setup fields.
       //===================================================================
       BegSr PrtCheck;

       If prtEdtMode = *off;

          If scPrtKey  = ' ';
             msPrtKey  = 'OutQueue name/Printer key cannot be blank.';
             erPrtKey  = *on;
             error = *on;
          Else;
             Chain(n) scPrtKey pp100;
             If %found;
                msPrtKey = 'Record already exists for OutQueue Name/Printer '+
                 'key.';
                erPrtKey = *on;
                error = *on;
             Else;

                If scPrtType = 'O';
                   CheckObjInLIBL(scPrtKey:'*OUTQ':erPrtKey);
                   If erPrtKey = '1';
                      msPrtKey = 'Output queue does not exist on iSeries.';
                      error = *on;
                   Endif;
                Endif;

             Endif;
          Endif;

       Endif;

       If scPrtName = ' ';
          msPrtName = 'Printer name cannot be blank.';
          erPrtName = *on;
          error = *on;
       Endif;

       If scPrtDesc = ' ';
          msPrtDesc = 'Printer description cannot be blank.';
          erPrtDesc = *on;
          error = *on;
       Endif;

       Endsr;
       //===================================================================

       //===================================================================
       // Save the printer setup record.
       //===================================================================
       BegSr PrinterSave;

       If xxRRN <> 0;
          Chain xxRRN pp10p;
       Else;
          Clear pp10p;
          P1Key  = scPrtKey;
          P1IUsr = Q1USER;
          P1IDt  = Dateto7(SystemDate());
       Endif;

       P1Name = scPrtName;
       P1Desc = scPrtDesc;
       P1Stat = scPrtStat;
       P1UNCN = scPrtUNCN;
       P1Type = scPrtType;

       Clear P1OLib;
       If P1Type = 'O';
          RtvObjectLib(P1Key:'*OUTQ':P1OLib);
       Endif;

       P1Pgm  = Q1PGM;
       P1User = Q1USER;
       P1Time = Timeto6(SystemTime());
       P1Dt   = Dateto7(SystemDate());

       If xxRRN <> 0;
          Update pp10p;
       Else;
          Write pp10p;
       Endif;

       Endsr;
       //===================================================================


       //===================================================================
       // Load the printer tray grid.
       //===================================================================
       BegSr LoadTray;

       // Clear the subfile.
       dspTray = *off;
       dspTrayCtl = *off;
       clrTray = *on;
       Write rcdPrinter;
       clrTray = *off;
       dspTray = *on;
       dspTrayCtl = *on;
       rrnTray = 0;

       // Load the printer tray subfile.

       If scPrtKey = ' ';
          Leavesr;
       Endif;

       Setll scPrtKey pp200;
       Reade(n) scPrtKey pp200;

       Dow not %eof;

          outTrayTry = %trim(P2Tray);
          outTrayDsc = %trim(P2Desc);

          If P2Stat = 'A';
             outTraySts = 'Active';
          Else;
             outTraySts = 'Inactive';
          Endif;

          outTrayRRN  = dbrrn9;

          rrnTray = rrnTray + 1;

          Write sflTray;

          Reade(n) scPrtKey pp200;
       Enddo;

       rrnTray = 1;

       Endsr;
       //===================================================================

       //===================================================================
       // Delete any trays added when printer to be added is cancelled.
       //===================================================================
       BegSr DeleteTrays;

       Setll scPrtKey pp200;
       Reade scPrtKey pp200;

       Dow not %eof;
          Delete pp200;
          Reade scPrtKey pp200;
       Enddo;

       Endsr;
       //===================================================================

      /end-free

     p PrtWindow       e

       //===================================================================
       // Add Printer Trays to printer tray setup file.
       //===================================================================
     p TrayWindow      b
     d TrayWindow      pi
     D inPrtKey                      10    const
     D inTrayRRN                      9  0 const options(*nopass)

     D xxRRN           s              9  0 inz(0)
     d error           s               n   inz(*off)
     d trayAddMode     s               n   inz(*off)

      /free

       If %parms >= 2;
          xxRRN = inTrayRRN;
       Endif;

       scCo# = intCo#;
       trayAddMode = *off;
       trayEdtMde = *off;

       If xxRRN <> 0;
          Chain(n) xxRRN pp20p;
          trayEdtMde = *on;
       Else;
          trayAddMode = *on;
       Endif;

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou trayAddMode = *off;

          //* Clear screen fields prior to display if in Add Mode.
          If trayAddMode = *on;
             Clear scTrayTray;
             Clear scTrayDesc;
             scTraySTAT = 'A';
             dspAddNext = *on;
             posCancel = 555;
             posAddNext = 665;
          Else;
             scTrayTray = P2Tray;
             scTrayDesc = P2Desc;
             scTraySTAT = P2Stat;
             dspAddNext = *off;
             posCancel = 665;
          Endif;

          Exsr ClearErrors;
          Clear traySave;
          Clear trayCancel;
          Clear trayAdd;

       //* Display popup window for the Tray Setup record.

          Dou traySave = *on or trayCancel = *on or trayAdd = *on;

             Dou error = *off or trayCancel = *on;

                Exfmt rcdTray;

                Exsr ClearErrors;

                If trayCancel = *off;
                   Exsr TrayCheck;
                Endif;

             Enddo;

          Enddo;

          If traySave = *on or trayAdd = *on;
             Exsr TrayInfoSave;
          Endif;

          If trayCancel = *on or traySave = *on;
             trayAddMode = *off;
          Endif;

       Enddo;

       //===================================================================
       // Clear the error fields for window.
       //===================================================================
       BegSr ClearErrors;

       error = *off;
       Clear erTrayTray;
       Clear msTrayTray;
       Clear erTrayDesc;
       Clear msTrayDesc;
       Clear erTrayStat;
       Clear msTrayStat;

       Endsr;
       //===================================================================

       //===================================================================
       // Edit the tray setup fields.
       //===================================================================
       BegSr TrayCheck;

       If trayEdtMde = *off;

          If scTrayTray = ' ';
             msTrayTray = 'Printer tray number cannot be blank.';
             erTrayTray = *on;
             error = *on;
             Leavesr;
          Else;
             Chain(n) (inPrtKey:scTrayTray) pp200;
             If %found;
                msTrayTray = 'Record already exists for printer tray number.';
                erTrayTray = *on;
                error = *on;
                Leavesr;
             Endif;
          Endif;

       Endif;

       If scTrayDesc = ' ';
          msTrayDesc = 'Printer tray description cannot be blank.';
          erTrayDesc = *on;
          error = *on;
       Endif;

       Endsr;
       //===================================================================

       //===================================================================
       // Save the printer tray setup record.
       //===================================================================
       BegSr TrayInfoSave;

       If xxRRN <> 0;
          Chain xxRRN pp20p;
       Else;
          Clear pp20p;
          P2Key  = inPrtKey;
          P2Tray = scTrayTray;
          P2IUsr = Q1User;
          P2IDt  = Dateto7(SystemDate());
       Endif;

       P2Desc = scTrayDesc;
       P2Stat = scTrayStat;

       P2Pgm  = Q1Pgm;
       P2User = Q1User;
       P2Time = Timeto6(SystemTime());
       P2Dt   = Dateto7(SystemDate());

       If xxRRN <> 0;
          Update pp20p;
       Else;
          Write pp20p;
       Endif;

       Endsr;
       //===================================================================

      /end-free

     p TrayWindow      e

       //===================================================================
       // Edit Detail Data.
       //===================================================================
     p EditCheck       b
     d EditCheck       pi

      /free

       // Clear errors then process each tab's set of edits.
       error = *off;

       // Check to be sure screen has not been updated since entry

       // Check if SDACP updated since screen displayed
       flgRcdUpdated = RcdUpdatedErr('SDACP':intRrn:'ACdt':'ACtime':
        'ACuser':saveDT:saveTM);

       If flgRcdUpdated = '0'; // No error
          flgRcdUpdated = RcdUpdatedErr('SDACP':intRrn1:'ACdt':'ACtime':
           'ACuser':saveDT1:saveTM1);
       Endif;
       If flgRcdUpdated = '0'; // No error
          flgRcdUpdated = RcdUpdatedErr('SDACP':intRrn2:'ACdt':'ACtime':
           'ACuser':saveDT2:saveTM2);
       Endif;
       If flgRcdUpdated = '0'; // No error
          flgRcdUpdated = RcdUpdatedErr('SDACP':intRrn3:'ACdt':'ACtime':
           'ACuser':saveDT3:saveTM3);
       Endif;
       If flgRcdUpdated = '0'; // No error
          flgRcdUpdated = RcdUpdatedErr('SDACP':intRrn4:'ACdt':
          //*** Beg Chg *** 06/26/23 ******************************************
          //*** 'ACtime':'ACuser':saveDT3:saveTM3);
           'ACtime':'ACuser':saveDT4:saveTM4);
          //*** Beg Chg *** 06/26/23 ******************************************
       Endif;
       If flgRcdUpdated = '0'; // No error
          flgRcdUpdated = RcdUpdatedErr('SDACP':intRrn5:'ACdt':
          //*** Beg Chg *** 06/26/23 ******************************************
          //*** 'ACtime':'ACuser':saveDT3:saveTM3);
           'ACtime':'ACuser':saveDT5:saveTM5);
          //*** Beg Chg *** 06/26/23 ******************************************
       Endif;
       //*** Beg Add *** 06/26/23 ******************************************
       If flgRcdUpdated = '0'; // No error
          flgRcdUpdated = RcdUpdatedErr('SDACP':intRrn6:'ACdt':
           'ACtime':'ACuser':saveDT6:saveTM6);
       Endif;
       //*** End Add *** 06/26/23 ******************************************
       Select;
       When flgRcdUpdated = '0'; // No error
       When flgRcdUpdated = '1'; // Error
          error = *on;
       When flgRcdUpdated = '2'; // Error with refresh
       Endsl;

       If error = *off;

          For intCount = 1 to intArrDim;
             If dsSecTyp(intCount) = *blanks and dsSecLvl(intCount) > 0;
                dsSecTypEr(intCount) = *on;
                error = *on;
             Endif;

             If dsSecTyp(intCount) <> *blanks and dsSecLvl(intCount) = 0;
                dsSecLvlEr(intCount) = *on;
                error = *on;
             Endif;

          Endfor;

       Endif;

      /end-free

     p EditCheck       e
       //===================================================================
       // Save Data.
       //===================================================================
     p SaveDetail      b
     d SaveDetail      pi

      /free

       // Claim Inquiry
       Chain intRrn pac0p;

       intStart = 0;
       SaveData(intStart);

       strKey = 'ClmInqAut';
       Exsr srWrtUpd;
       saveDT = ACdt;
       saveTM = ACtime;

       // Claim Update
       Chain intRrn1 pac0p;

       intStart = 10;
       SaveData(intStart);

       strKey = 'ClmUpdAut';
       Exsr srWrtUpd;
       saveDT1 = ACdt;
       saveTM1 = ACtime;

       // Policy Inquiry
       Chain intRrn2 pac0p;

       intStart = 20;
       SaveData(intStart);

       strKey = 'PolInqAut';
       Exsr srWrtUpd;
       saveDT2 = ACdt;
       saveTM2 = ACtime;

       // Policy Update
       Chain intRrn3 pac0p;

       intStart = 30;
       SaveData(intStart);

       strKey = 'PolUpdAut';
       Exsr srWrtUpd;
       saveDT3 = ACdt;
       saveTM3 = ACtime;

       // Quote Inquiry
       Chain intRrn4 pac0p;

       intStart = 40;
       SaveData(intStart);

       strKey = 'QteInqAut';
       Exsr srWrtUpd;
       saveDT4 = ACdt;
       saveTM4 = ACtime;

       // Quote Update
       Chain intRrn5 pac0p;

       intStart = 50;
       SaveData(intStart);

       strKey = 'QteUpdAut';
       Exsr srWrtUpd;
       saveDT5 = ACdt;
       saveTM5 = ACtime;

       //*** Beg Add *** 06/26/23 ******************************************
       // Group Configuration Security / User Access Controls
       Chain intRrn6 pac0p;

       intStart = 60;
       SaveData(intStart);

       strKey = 'GrpCfgAut';
       Exsr srWrtUpd;
       saveDT6 = ACdt;
       saveTM6 = ACtime;
       //*** End Add *** 06/26/23 ******************************************

       Exfmt rcdSave;

       //===================================================================
       // Write/Update Record
       //===================================================================
       BegSr SrWrtUpd;

       ACpgm  = Q1pgm;
       ACuser = Q1user;
       ACtime = Timeto6(SystemTime());
       ACdt   = Dateto7(SystemDate());

       If not %found;
          ACapid = strKey;
          ACipgm = Q1pgm;
          ACiusr = Q1user;
          ACitim = Timeto6(SystemTime());
          ACidt  = Dateto7(SystemDate());
          Write pac0p;
       Else;
          Update pac0p;
       Endif;
       unlock SDACP;

       Endsr;
       //===================================================================

      /end-free

     p SaveDetail      e
       //===================================================================
       // Load Data.
       //===================================================================
     p LoadData        b
     d LoadData        pi
     d intStrCnt                      3  0

      /free

       For intCount = 1 to intFlgDim;
          dsSecTyp(intStrCnt + intCount) = dsSecTypFl(intCount);
          dsSecLvl(intStrCnt + intCount) = dsSecLvlFl(intCount);
       Endfor;

      /end-free

     p LoadData        e
       //===================================================================
       // Save Data.
       //===================================================================
     p SaveData        b
     d SaveData        pi
     d intStrCnt                      3  0

      /free

       For intCount = 1 to intFlgDim;
          dsSecTypFl(intCount) = Upper(dsSecTyp(intStrCnt + intCount));
          dsSecLvlFl(intCount) = dsSecLvl(intStrCnt + intCount);
       Endfor;

      /end-free

     p SaveData        e
      // utility procedure definitions
      /COPY SCOPYCOMPI
       //===================================================================
       // Load Auto Diary Trigger Filter
       //===================================================================
     p SetTrgFilter    b
     d SetTrgFilter    pi
      /free

       Select;
       When isClmTrig = *on;
          selFltTrg = 'GTtbId = ''AXKY''' + ' And (GTfg01 = ''C''' + ' or +
           GTfg01 = '' ''' + ') And GTstat = ''A''';
       When isPolTrig = *on;
          selFltTrg = 'GTtbId = ''AXKY''' + ' And (GTfg01 = ''P''' + ' or +
           GTfg01 = '' ''' + ') And GTstat = ''A''';
       Endsl;
       selFltTrg = %trim(selFltTrg) + ' and GTco# = ' + %editc(intCo#:'X');

       selFltRole = 'GTstat <> ''I''' + ' And GTtbid = ''AXRO''';
       If isRules = *off;
          selFltRole = %trim(selFltRole) + ' and GTfg02 <> ''E''';
       Endif;
       selFltRole = %trim(selFltRole) + ' and GTco# = ' + %editc(intCo#:'X');

       selRecUser = 'UPco# = ' + %editc(intCo#:'X') + ' And UPstat = ''A''';

       filtAplTrg = *off;
       fltCanTrg  = *off;
       AutoDryFlt = *off;

       Exfmt rcdTrgFltr;

       If filtAplTrg = *on;
          fltrTrig   = scFiltTrg;
          fltrTrgSts = scFltTrgSt;
          fltrTrgGrp = scFltTrgGr;
          fltrTrgRl  = scFiltRole;
          fltrTrgUsr = scFiltUser;
          //rrnTrgRow  = 0;
          rrnTrgRow  = 1;

          If fltrTrig <> ' ' or fltrTrgSts <> ' ' or fltrTrgGrp <> 0 or
             ssFltScope <> *blank or
             fltrTrgRl <> ' ' or fltrTrgUsr <> ' ';
             showTrgClr = *on;
          Endif;
       Else;
          scFiltTrg  = fltrTrig;
          scFltTrgSt = fltrTrgSts;
          scFltTrgGr = fltrTrgGrp;
          scFiltRole = fltrTrgRl;
          scFiltUser = fltrTrgUsr;
       Endif;

      /end-free
     p SetTrgFilter    e
       //===================================================================
       // Clear Auto Diary Trigger Filter
       //===================================================================
     p ClearTrgFilter  b
     d ClearTrgFilter  pi
      /free

       fltrTrig   = *blanks;
       fltrTrgSts = ' ';
       fltrTrgRl  = *blanks;
       fltrTrgUsr = *blanks;
       fltrTrgGrp = *zero;
       scFltTrgGr = fltrTrgGrp;
       scFiltTrg  = fltrTrig;
       scFltTrgSt = fltrTrgSts;
       scFiltRole = fltrTrgRl;
       scFiltUser = fltrTrgUsr;
       Clear ssFltScope;

       clrTrigFlt = *off;
       showTrgClr = *off;

      /end-free
     p ClearTrgFilter  e
       //===================================================================
       // Get Group Name
       //===================================================================
     p GetFndName      b
     d GetFndName      pi            60
     d intGrp#                        3  0
     d expDate                         d   options(*nopass)

     d strGrpName      s             60
     d dtGrpExp        s               d
      /free

       strGrpName = %editc(intGrp#:'X');
       If ##Wcif = 'Y';
          Open WMFNL;

          Clear dtGrpExp;
          Chain (intCo#:intGrp#) pfn00;
          If %found(WMFNL);
             strGrpName = %trim(strGrpName) + ' - ' + %trim(FNname);
             dtGrpExp = CvtToDate(FNexDt);
          Endif;
          Close WMFNL;
       Endif;
       If %parms >= 2;
          expDate = dtGrpExp;
       Endif;
       Return strGrpName;

      /end-free
     p GetFndName      e

       //===================================================================
       // Check For Overlapping Auto Letters
       //===================================================================
     p ChkDupAutoLet   b
     d ChkDupAutoLet   pi              n
     d  autoLetSeq#                        Like(mCLalSq)

     d  inAutoLetSeq#  s                   Like(mCLalSq)
     d  groupMatch     s               n   Inz(*Off)
     d  stateMatch     s               n   Inz(*Off)
     d  officeMatch    s               n   Inz(*Off)
     d  strSelection   s            525
     d  groupKey       s                   Like(gCLfnd)
     d  stateKey       s                   Like(sCLjst)
     d  officeKey      s                   Like(oCLoff#)
      /free

       // If there is a match in group, jurisdiction and office# then
       // there is an overlap and return error.

       inAutoLetSeq# = autoLetSeq#;

       // Lets Check Group
       If edAddGrpAL = ' ';
          groupMatch = *On;
       Else;
          strSelection = edAddGrpAL;
          Dow strSelection <> ' ';
             groupKey = %int(%subst(strSelection:1:3));
             Chain (intCo#:inAutoLetSeq#:groupKey) pcl0grp;
             If %Found(SACLLGRP);
                groupMatch = *On;
                Leave;
             Else;
                Chain (intCo#:inAutoLetSeq#) pcl0grp;
                If Not %Found(SACLLGRP);
                   groupMatch = *On;
                   Leave;
                EndIf;
             EndIf;
             strSelection = %subst(strSelection:5);
          Enddo;
       EndIf;

       // Lets Check Jurisdiction state
       If edAddJstAL = ' ';
          stateMatch = *On;
       Else;
          strSelection = edAddJstAL;
          Dow strSelection <> ' ';
             stateKey = %subst(strSelection:1:2);
             Chain (intCo#:inAutoLetSeq#:stateKey) pcl0jst;
             If %Found(SACLLJST);
                stateMatch = *On;
                Leave;
             Else;
                Chain (intCo#:inAutoLetSeq#) pcl0grp;
                If Not %Found(SACLLJST);
                   stateMatch = *On;
                   Leave;
                EndIf;
             EndIf;
             strSelection = %subst(strSelection:4);
          Enddo;
       EndIf;

       // Lets Check Group
       If edAddOffAL = ' ';
          officeMatch = *On;
       Else;
          strSelection = edAddOffAL;
          Dow strSelection <> ' ';
             officeKey = %subst(strSelection:2:5);
             If officeKey <> ' ';
                Chain (intCo#:inAutoLetSeq#:officeKey) pcl0off;
                If %Found(SACLLOFF);
                   officeMatch = *On;
                   Leave;
                Else;
                   Chain (intCo#:inAutoLetSeq#) pcl0off;
                   If Not %Found(SACLLOFF);
                      officeMatch = *On;
                      Leave;
                   EndIf;
                EndIf;
             EndIf;
             strSelection = %subst(strSelection:9);
          Enddo;
       EndIf;

       // A match was found in Group, Jurisdiction State and Office#
       // This means thee is an auto letter that overlaps
       If groupMatch = *On And stateMatch = *On and officeMatch = *On;
          Return *On;
       EndIf;

       Return *Off;

      /end-free
     p ChkDupAutoLet   e
       //===================================================================
       // Clear Auto Letter Filter
       //===================================================================
     p ClearAutFilter  b
     d ClearAutFilter  pi
      /free

       ssAutlDoc = ' ';
       ssAutlSts = ' ';
       ssAutlGrp = 0;
       ssAutlJst = ' ';
       ssAutlOff = ' ';

       clrAutlFlt = *off;
       shwAutlClr = *off;

      /end-free
     p ClearAutFilter  e

       //===================================================================
       // Create workfile in QTEMP
       //===================================================================
     pCreateWorkfile   b
     dCreateWorkfile   pi             1n
     d inFile                        10    const
     d inClrFile                      1n   const options(*nopass)

     d isClrFile       s              1n   inz('1')
     d strRtn          s              1    inz

     dCreateWrkF       pr                  ExtPgm('SA710C')
     d outRtn                         1
     d inFromName                    10    const
     d inToName                      10    const
     d inClrFile                      1n   const options(*nopass)
      /free
       If %parms >= 2;
          isClrFile = inClrFile;
       Endif;
       CreateWrkF(strRtn:inFile:inFile:isClrFile);
       Select;
       When strRtn <> *blanks;
          Return *off;
       Other;
          Return *on;
       Endsl;
      /end-free
     pCreateWorkfile   e

       //===================================================================
       // Load Common elements for auto letters
       //===================================================================
     p LoadAutLetDtl   b
     d LoadAutLetDtl   pi

      /free

       // Process common elements

       If rrnAutlRow = 0;
          rrnAutlRow = 1;
       Endif;

       // Read from subfile
       Chain rrnAutlRow sflAutoLet;
       If %found;
          Chain(n) scAutlRrn pcl0p;
          If not %found(SACLP);
             clear pcl0p;
          Endif;
       Else;
          clear pcl0p;
       Endif;

       // Load the Auto Letter Detail
       Exsr LodAutlDtl;

       // Load the Auto Letter Groups
       Exsr LodAutlGrpGrid;

       // Load the Auto Letter Jurisdiction State
       Exsr LodAutlJstGrid;

       // Load the Auto letter Office #
       Exsr LodAutlOffGrid;

       //*** Beg Add ***** 09/16/22 ************************
       // Load the Auto Letter Lost time Flag
       Exsr LodAutlLTFGrid;
        //*** End Add ***** 09/16/22 ************************

       //===================================================================
       // Load Auto Letter Detail
       //===================================================================
       BegSr LodAutlDtl;

          dtAutlSeq# = mCLalSq;

          xxTBID = 'DOCS';
          xxKey  = mCLcat;
          Chain(n) keySTGTL pgt00;
          If %found;
             dtAutlCat  = %trim(GTDesc);
          Else;
             dtAutlCat  = %trim(DLCat);
          Endif;

          xxCat  = %trim(mCLcat);
          xxSubC = %trim(mCLsubC);
          Chain(n) keySTDEL pde00;
          If %found;
             dtAutlSub = %trim(DEDesc);
          Else;
             dtAutlSub = %trim(mCLSubC);
          Endif;
          dtAutlLang = mCLlang;
          dtAutlDoc  = mCLdcid;

          dtAutlStat = 'Active';
          If mCLstat = 'I';
             dtAutlStat = 'Inactive';
          EndIf;

          dtBgClmSts = ' ';
          Select;
          When mCLcsFr = 'C';
             dtBgClmSts = 'Closed';
          When mCLcsFr = 'O';
             dtBgClmSts = 'Open';
          When mCLcsFr = 'N';
             dtBgClmSts = 'New';
          EndSl;

          dtEnClmSts = ' ';
          Select;
          When mCLcsto = 'C';
             dtEnClmSts = 'Closed';
          When mCLcsTo = 'O';
             dtEnClmSts = 'Open';
          EndSl;

          dtBgClmTyp = ' ';
          Select;
          When mCLltFr = 'I';
             dtBgClmTyp = 'Indemnity';
          When mCLltFr = 'M';
             dtBgClmTyp = 'Medical Only';
          When mCLltFr = 'R';
             dtBgClmTyp = 'Record Only';
          EndSl;

          dtEnClmTyp = ' ';
          Select;
          When mCLltTo = 'I';
             dtEnClmTyp = 'Indemnity';
          When mCLltTo = 'M';
             dtEnClmTyp = 'Medical Only';
          When mCLltTo = 'R';
             dtEnClmTyp = 'Record Only';
          EndSl;

          dtAutlCond = ' ';
          Select;
          When mCLandr = 'A';
             dtAutlCond = 'And';
          When mCLandr = 'O';
             dtAutlCond = 'Or';
          EndSl;

       Endsr;
       //===================================================================
       // Load Auto Letter Group Grid
       //===================================================================
       BegSr LodAutlGrpGrid;

       // clear the group subfile.
       dspAutlGrp = *off;
       clrAutlGrp = *on;
       Write rcdAutoLet;
       clrAutlGrp = *off;
       dspAutlGrp = *on;
       rrnAutlGrp = 0;

       Setll (mCLco#:mCLalSq) pcl0grp;
       Reade(n) (mCLco#:mCLalSq) pcl0grp;
       Dow not %eof(SACLLGRP);
          scAutlGrps = GetFndName(gCLfnd);
          rrnAutlGrp = rrnAutlGrp + 1;
          Write sflAutlGrp;

          Reade(n) (mCLco#:mCLalSq) pcl0grp;
       Enddo;
       rrnAutlGrp = 1;

       Endsr;
       //===================================================================
       // Load Auto Letter Jurisdiction State Grid
       //===================================================================
       BegSr LodAutlJstGrid;

       // clear the group subfile.
       dspAutlJst = *off;
       clrAutlJst = *on;
       Write rcdAutoLet;
       clrAutlJst = *off;
       dspAutlJst = *on;
       rrnAutlJst = 0;

       Setll (mCLco#:mCLalSq) pcl0jst;
       Reade(n) (mCLco#:mCLalSq) pcl0jst;
       Dow not %eof(SACLLJST);
          stateKey = sCLjst;
          Chain (stateKey) pst01;
          If %found(STSTL1) and STname <> ' ';
             scAutlJsts = %trim(STname);
          Else;
             scAutlJsts = %trim(sCLjst);
          Endif;
          rrnAutlJst = rrnAutlJst + 1;
          Write sflAutlJst;

          Reade(n) (mCLco#:mCLalSq) pcl0jst;
       Enddo;
       rrnAutlJst = 1;

       Endsr;
       //===================================================================
       // Load Auto Letter Office Grid
       //===================================================================
       BegSr LodAutlOffGrid;

       // clear the group subfile.
       dspAutlOff = *off;
       clrAutlOff = *on;
       Write rcdAutoLet;
       clrAutlOff = *off;
       dspAutlOff = *on;
       rrnAutlOff = 0;

       Setll (mCLco#:mCLalSq) pcl0off;
       Reade(n) (mCLco#:mCLalSq) pcl0off;
       Dow not %eof(SACLLOFF);
          Chain (oCLco#:oCLoff#) pof00;
          If %found(WMOFL) and OFname <> ' ';
             scAutlOffs = %trim(OFname);
          Else;
             scAutlOffs = %trim(oCLoff#);
          Endif;
          rrnAutlOff = rrnAutlOff + 1;
          Write sflAutlOff;

          Reade(n) (mCLco#:mCLalSq) pcl0off;
       Enddo;
       rrnAutlOff = 1;

       Endsr;

       //*** Beg Add ***** 09/16/22 ****************************************
       //===================================================================
       // Load Auto Letter Lost Time Flag Grid
       //===================================================================
       BegSr LodAutlLTFGrid;

       // clear the Lost time flag subfile.
       dspAutlLTF = *off;
       clrAutlLTF = *on;
       Write rcdAutoLet;
       clrAutlLTF = *off;
       dspAutlLTF = *on;
       rrnAutlLTF = 0;

       intStrPos = 1;
       For x = 1 to 3;
          If %Subst(mCLLTFlLst:intStrPos:1) <> ' ';
             strLstFlg = %Subst(mCLLTFlLst:intStrPos:1);
             intStrPos += 1;
             Select;
             When strLstFlg = 'I';
                scAutlLTF = 'Indemnity';
             When strLstFlg = 'M';
                scAutlLTF = 'Medical';
             When strLstFlg = 'R';
                scAutlLTF = 'Record Only';
             Endsl;
             rrnAutlLTF = rrnAutlLTF + 1;
             Write sflAutlLTF;
          Endif;
       Endfor;
       rrnAutlLTF = 1;

       Endsr;
       //*** End Add ***** 09/16/22 ****************************************

      /end-free
     p LoadAutLetDtl   e
       //===================================================================
       // Process Auto Letter Add/Edit
       //===================================================================
     p ProcAutlAddEdt  b
     d ProcAutlAddEdt  pi

     d isNotValid      s               n   inz(*off)
     d intSeq          s              5  0
     d strSelection    s            525
     d saveDate        s              7  0
     d saveTime        s              6  0

      /free

       clear rcdAutlCfg;

       Select;
       // Add Auto Letter mode
       When flgAutMode = 'A';
          Exsr MoveDbScAutoL;

       // Edit Auto Letter mode
       When flgAutMode = 'E';
          Exsr MoveDbScAutoL;

       // Change Auto Letter Status mode
       When flgAutMode = 'S';
          Exsr ProcSaveAutoL;
          Return;
       Endsl;

       BtnAutlSav = *off;
       BtnAutlCan = *off;
       isNotValid = *off;

       Dou BtnAutlSav = *on and isNotValid = *off;

          Exfmt rcdAutlCfg;

          ExSr InitAutoL;

          Select;
          When BtnAutlSav = *on;
             Exsr ValidateAutoL;
             If isNotValid = *off;
                Exsr ProcSaveAutoL;
             Endif;

          When BtnAutlCan = *on;
             BtnTrigCan = *off;
             Leave;
          Other;
          Endsl;
       Enddo;
       //===================================================================
       // InitAutoL - Initialize Error Messages and other fields
       //===================================================================
       Begsr InitAutoL;

          isNotValid = *off;
          erAutlDoc  = *off;
          erBegSts   = *off;
          erEndSts   = *off;
          erCond     = *off;
          erBegTyp   = *off;
          erEndTyp   = *off;
          //*** Beg Add ***** 09/16/22 ******************
          erAutlLTF  = *off;
          //*** End Add ***** 09/16/22 ******************

       EndSr;
       //===================================================================
       // MoveDbScAutoL - Populate Auto Letter Screen Fields
       //===================================================================
       Begsr MoveDbscAutoL;

       Select;
       // Add Auto Letter mode
       When flgAutMode = 'A';
       // Add Auto Letter mode
          autlAddMod  = *On;
          autlNotAdd  = *Off;
       When flgAutMode = 'E';
          // Read from subfile
          autlAddMod  = *Off;
          autlNotAdd  = *On;
          Chain rrnAuToLet sflAutoLet;

          If %found;
             Chain(n) scAutlRrn pcl0p;
             If %found(SACLP);
                edAutlSeq# = mCLalSq;
                edAutlDoc  = mCLcat + mCLsubc + mCLdcid + mCLlang;

                Select;
                When mCLstat = 'I';
                   edAutlSts = 'Inactive';
                Other;
                   edAutlSts = 'Active';
                EndSl;

                edBegSts   = mCLcsFr;
                edEndSts   = mCLcsTo;
                edCond     = mCLandr;
                edBegTyp   = mCLltFr;
                edEndTyp   = mCLltTo;

                // Load the Groups
                Setll (mCLco#:mCLalSq) pcl0grp;
                Reade(n) (mCLco#:mCLalsq) pcl0grp;
                Dow not %eof(SACLLGRP);
                   If edAddGrpAL = ' ';
                      edAddGrpAL = %editc(gCLfnd:'X');
                   Else;
                      edAddGrpAL = %trim(edAddGrpAL) + ',' + %editc(gCLfnd:'X');
                   Endif;
                   Reade(n) (mCLco#:mCLalsq) pcl0grp;
                Enddo;

                // Load the Jurisdiction States
                Setll (mCLco#:mCLalSq) pcl0jst;
                Reade(n) (mCLco#:mCLalsq) pcl0jst;
                Dow not %eof(SACLLJST);
                   If edAddJstAL = ' ';
                      edAddJstAL = sCLjSt;
                   Else;
                      edAddJstAL = %trim(edAddJstAL) + ',' + sCLjSt;
                   Endif;
                   Reade(n) (mCLco#:mCLalsq) pcl0jst;
                Enddo;

                // Load the Offices
                Setll (mCLco#:mCLalSq) pcl0off;
                Reade(n) (mCLco#:mCLalsq) pcl0off;
                Dow not %eof(SACLLOFF);
                   If edAddOffAL = ' ';
                      edAddOffAL = ':' + oCLoff# + ':';
                   Else;
                      edAddOffAL = %trim(edAddOffAL) + ',' + ':' + oCLoff# +
                                   ':';
                   Endif;
                   Reade(n) (mCLco#:mCLalsq) pcl0off;
                Enddo;

                //*** Beg Add ***** 09/16/22 ************************
                // Load Claim Loss Time Flag
                intStrPos = 1;
                For x = 1 to 3;
                   strLstFlg = %Subst(mCLLTFlLst:intStrPos:1);
                   If strLstFlg <>  ' ';
                      If edAddLTFAL = ' ';
                         edAddLTFAL = strLstFlg;
                      Else;
                         edAddLTFAL = %trim(edAddLTFAL) + ',' + strLstFlg;
                      Endif;
                   Endif;
                   intStrPos += 1;
                Endfor;
                //*** End Add ***** 09/16/22 ************************

             Endif;
          Endif;
       EndSl;

       Endsr;
       //===================================================================
       // Validate Auto Letter
       //===================================================================
       Begsr ValidateAutoL;

       isNotValid = *off;

       // Document is required
       If edAutlDoc = ' ';
          mgAutlDoc = 'Please select a document.';
          erAutlDoc = *On;
          isNotValid = *On;
       EndIf;

       // Make sure there aren't any overlapped letters.
       // Check to see if Document is being used in another auto letter
       category    = %Subst(edAutlDoc:1:5);
       subCategory = %Subst(edAutlDoc:6:5);
       documentID  = %Subst(edAutlDoc:11:60);
       language    = %Subst(edAutlDoc:71:40);

       Chain (intCo#:category:subCategory:documentID:language) pcl01;
       If %Found(SACLL1);
          DoU %Eof(SACLL1);
             If (flgAutMode = 'E' And edAutlSeq# <> mCLalSq) Or
                 flgAutMode = 'A';
                erAutlDoc = ChkDupAutoLet(mCLalSq);
                If erAutlDoc = *On;
                   mgAutlDoc = 'This auto letter overlaps with an existing ' +
                                'auto letter.';
                   isNotValid = *On;
                   Leave;
                EndIf;
             EndIf;
             ReadE (intCo#:category:subCategory:documentID:language) pcl01;
          EndDo;
       EndIf;

       //*** Beg Add ***** 09/16/22 **********************************
       // Lost timm Flags validation
       If edAddLTFAL <> *Blank and (edBegTyp <> *Blank or edEndTyp <> *Blank);
          mgAutlLTF = 'Only the lost time Multi-select box or ' +
           'the Lost Time Begin/End select boxes can be filled, not both.';
          erAutlLTF = *On;
          If edBegTyp <> *Blank;
             mgBegTyp = 'Only the Lost Time Begin/End select boxes or ' +
              'the Lost Time Multi-select box can be filled, not both.';
             erBegTyp = *On;
          Endif;
          If edEndTyp <> *Blank;
             mgEndTyp = 'Only the Lost Time Begin/End select boxes or ' +
              'the Lost Time Multi-select box can be filled, not both.';
             erEndTyp = *On;
          Endif;
          isNotValid = *On;
       Endif;
       //*** End Add ***** 09/16/22 **********************************

       // Validate Triggers

       // Begin Status must be entered if End status entered.
       If edBegSts = ' ' and edEndSts <> ' ';
          mgBegSts = 'From status must be entered.';
          erBegSts = *On;
          isNotValid = *On;
       EndIf;

       // Begin Status must be different than End Status.
       If edBegSts <> ' ' and edBegSts = edEndSts;
          mgBegSts = 'From and To Statuses must be different.';
          erBegSts = *On;
          isNotValid = *On;
       EndIf;

       // End Status must be entered if Begin status entered.
       If edEndSts = ' ' and edBegSts <> ' ';
          mgEndSts = 'From status must be entered.';
          erEndSts = *On;
          isNotValid = *On;
       EndIf;

       // End Status must be set to Open if Begin status is New
       If edEndSts <> 'O' and edBegSts = 'N';
          mgEndSts = 'End status must be set to Open if begin status is New';
          erEndSts = *On;
          isNotValid = *On;
       EndIf;

       // Begin Lost Time must be entered if End Lost Time entered.
       // Skip this Validation to allow setting up a new claim
       // when it is Med Only, Indemnity or Record Only
       If edBegTyp = ' ' and edEndTyp <> ' ' and edBegSts <> 'N';
          mgBegTyp = 'From lost time must be entered.';
          erBegTyp = *On;
          isNotValid = *On;
       EndIf;

       // Begin Lost Time must be different than End Lost Time.
       If edBegTyp <> ' ' and edBegTyp = edEndTyp;
          mgBegTyp = 'From and To Lost Time Values must be different.';
          erBegTyp = *On;
          isNotValid = *On;
       EndIf;

       // End Lost Time must be entered if Begin Lost Time entered.
       If edEndTyp = ' ' and edBegTyp <> ' ';
          mgEndTyp = 'From lost time must be entered.';
          erEndTyp = *On;
          isNotValid = *On;
       EndIf;

       // Lost time or Claim Status Triggers must be entered.
       If edBegSts = ' ' And edBegTyp = ' ' And edEndSts = ' ' and
          edEndTyp = ' ';
          mgBegSts = 'Claim Status and/or Lost Time Trigger must be entered.';
          erBegSts = *On;
          isNotValid = *On;
       EndIf;

       // If checking for new claims then lost time condition can't be set.
       If edBegSts = 'N' And edBegTyp <> ' ';
          mgBegTyp = 'You can''t add Lost Time Trigger if checking' +
                     ' for new claims.';
          erBegTyp = *On;
          mgEndTyp = 'You can''t add Lost Time Trigger if checking' +
                     ' for new claims.';
          erEndTyp = *On;
          isNotValid = *On;
       EndIf;

       // If Lost time or Status trigger not set then condition can't be set.
       // Skip this Validation to allow setting up a new claim
       // when it is Med Only, Indemnity or Record Only
       If (edBegSts = ' ' Or edBegTyp = ' ') And edCond <> ' '
        and edBegSts <> 'N';
          mgCond = 'Condition can''t be set since both status and ' +
                   'lost time triggers have not been set.';
          erCond = *On;
          isNotValid = *On;
       EndIf;

       // If checking for new claims then lost time condition can't be set.
       // Skip this Validation to allow setting up a new claim
       // when it is Med Only, Indemnity or Record Only

       Endsr;
       //===================================================================
       // Save the Auto Letter record.
       //===================================================================
       Begsr ProcSaveAutoL;

       If flgAutMode = 'E' Or flgAutMode = 'S';
          Chain scAutlRrn pcl0p;
       Else;
          // get sequence #
          SetGT (intCo#:*hival) pcl00;
          ReadPE (intCo#) pcl00;
          If not %eof(SACLL);
             intSeq = mCLalSq + 1;
          Else;
             intSeq = 1;
          Endif;

          clear pcl0p;
          mCLco#  = intCo#;
          mCLalSq = intSeq;
       EndIf;

       If flgAutMode = 'S';
          Select;
          When mCLstat = 'A';
             mCLstat = 'I';
          Other;
             mCLstat = 'A';
          EndSl;
       Else;
          mCLcat  = %Subst(edAutlDoc:1:5);
          mCLsubC = %Subst(edAutlDoc:6:5);
          mCLdcid = %Subst(edAutlDoc:11:60);
          mCLlang = %Subst(edAutlDoc:71:40);
          mCLcsFr = edBegSts;
          mCLcsTo = edEndSts;
          mCLandr = edCond;
          mCLltFr = edBegTyp;
          mCLltTo = edEndTyp;
          //*** Beg Add ***** 09/16/22 ********************
          Clear mCLLTFlLst;
          strSelection = edAddLTFAL;
          Dow strSelection <> ' ';
             mCLLTFlLst = %Trim(mCLLTFlLst) + %subst(strSelection:1:1);
             strSelection = %subst(strSelection:3);
          Enddo;
          //*** End Add ***** 09/16/22 ********************
       EndIf;

       mCLPgm  = q1Pgm;
       mCLUser = q1User;
       mCLtime = Timeto6(SystemTime());
       mCLdt   = Dateto7(SystemDate());

       If flgAutMode = 'E' Or flgAutMode = 'S';
          Update pcl0p;
       Else;
          mCLstat = 'A';
          mCLiPgm = q1Pgm;
          mCLiUsr = q1User;
          mCLiTim = Timeto6(SystemTime());
          mCLiDt  = Dateto7(SystemDate());
          Write pcl0p;
       Endif;

       Unlock SACLP;

       If flgAutMode <> 'S';

          // clear the old group selection
          Setll (mCLco#:mCLalSq) pcl0grp;
          Reade (mCLco#:mCLalSq) pcl0grp;
          Dow not %eof(SACLLGRP);
             delete pcl0grp;
             Reade (mCLco#:mCLalSq) pcl0grp;
          Enddo;

          // Add new group selection
          gCLco#  = intCo#;
          gCLalSq = mCLalSq;
          gCLpgm  = q1Pgm;
          gCLuser = q1User;
          gCLtime = Timeto6(SystemTime());
          gCLdt   = Dateto7(SystemDate());
          gCLiPgm = q1Pgm;
          gCLiUsr = q1User;
          gCLiTim = GTRtime;
          gCLiDt  = GTRdt;

          strSelection = edAddGrpAL;

          Dow strSelection <> ' ';
             gCLfnd = %int(%subst(strSelection:1:3));
             strSelection = %subst(strSelection:5);
             write pcl0grp;
          Enddo;

          // clear the old jurisdiction state selection
          Setll (mCLco#:mCLalSq) pcl0jst;
          Reade (mCLco#:mCLalSq) pcl0jst;
          Dow not %eof(SACLLJST);
             delete pcl0jst;
             Reade (mCLco#:mCLalSq) pcl0jst;
          Enddo;

          // Add new group selection
          sCLco#  = intCo#;
          sCLalSq = mCLalSq;
          sCLpgm  = q1Pgm;
          sCLuser = q1User;
          sCLtime = Timeto6(SystemTime());
          sCLdt   = Dateto7(SystemDate());
          sCLiPgm = q1Pgm;
          sCLiUsr = q1User;
          sCLiTim = GTRtime;
          sCLiDt  = GTRdt;

          strSelection = edAddJstAL;

          Dow strSelection <> ' ';
             sCLjSt = %subst(strSelection:1:2);
             strSelection = %subst(strSelection:4);
             write pcl0jst;
          Enddo;

          // clear the old office # selection
          Setll (mCLco#:mCLalSq) pcl0off;
          Reade (mCLco#:mCLalSq) pcl0off;
          Dow not %eof(SACLLOFF);
             delete pcl0off;
             Reade (mCLco#:mCLalSq) pcl0off;
          Enddo;

          // Add new group selection
          oCLco#  = intCo#;
          oCLalSq = mCLalSq;
          oCLpgm  = q1Pgm;
          oCLuser = q1User;
          oCLtime = Timeto6(SystemTime());
          oCLdt   = Dateto7(SystemDate());
          oCLiPgm = q1Pgm;
          oCLiUsr = q1User;
          oCLiTim = GTRtime;
          oCLiDt  = GTRdt;

          strSelection = edAddOffAL;

          Dow strSelection <> ' ';
             oCLoff# = %subst(strSelection:2:5);
             If oCLoff# <> ' ';
                write pcl0off;
             EndIf;
             strSelection = %subst(strSelection:9);
          Enddo;
       EndIf;

       Endsr;

      /end-free
     p ProcAutlAddEdt  e
       //===================================================================
       // Load Auto Letter Filter
       //===================================================================
     p SetAutlFilter   b
     d SetAutlFilter   pi
      /free

       ssAutlOK   = *off;
       ssAutlCan  = *off;
       AutoLetFlt = *off;

       Exfmt rcdAutlFlt;

       If ssAutlOK = *on;
          If ssAutlDoc <> ' ' or ssAutlSts <> ' ' or ssAutlGrp <> 0 or
             ssAutlJst <> ' ' or ssAutlOff <> ' ';
             shwAutlClr = *on;
          Endif;
       Endif;

      /end-free
     p SetAutlFilter   e
       //===================================================================
       // Load Auto Trigger Grid
       //===================================================================
     p LoadTrgGrid     b
     d LoadTrgGrid     pi

      /free

       // Clear the subfile.
       dspAutoTrg = *off;
       clrAutoTrg = *on;
       Write rcdAutoTrg;
       clrAutoTrg = *off;
       dspAutoTrg = *on;
       rrnAutoTrg = 0;
       clear dbrrn5;

       // Load the Auto Trigger subfile.
       Setll intCo# ptr00;
       Reade intCo# ptr00;

       Dow not %eof(SMTRL);

          If meetFilter() = *on;
             scAutoTrg = mTRname;
             scTrgSeq# = mTRseq#;
             if mTRdesc <> *blanks;
                scTrgDesc = %trim(mTRdesc);
             else;
                xxTBID = 'AXKY';
                xxKey  = mTRname;
                Chain(n) keySTGTL pgt00;
                scTrgDesc = %trim(GTdesc);
             endif;
             scTrgDrCd = mTRdart;
             Select;
             When mTRstat = 'A';
                scTrgSts  = 'Active';
                scLnkText = 'Inactivate';
             When mTRstat = 'P';
                scTrgSts  = 'Pending';
                scLnkText = '';
             When mTRstat = 'X';
                scTrgSts  = 'Invalid';
                scLnkText = '';
             When mTRstat = 'Q';
                scTrgSts  = 'Pending Change';
                scLnkText = '';
             When mTRstat = 'i';
                scTrgSts  = 'Pending Inactive';
                scLnkText = '';
             Other;
                scTrgSts = 'Inactive';
                scLnkText = 'Activate';
             Endsl;

             scTrgRrn = dbrrn5;

             rrnAutoTrg = rrnAutoTrg + 1;

             clear scTrgRwSel;
          If (rrnAutoTrg = rrnTrgRow) or (rrnAutoTrg = 1 and rrnTrgRow =    0);
                scTrgRwSel = 'A';
             Endif;

             Write sflAutoTrg;
             scTrgText = 'My text ' + %editc(rrnAutoTrg:'Z');
             // Load textual detail
             Clear scTrgText;
             Chain(n) (mTRco#:mTRname:mTRseq#) smtrptxt;
             If not %found;
                SaveRuleText();
                Chain(n) (mTRco#:mTRname:mTRseq#) smtrptxt;
             Endif;
             Select;
             When mTRname = *blanks;
                scTrgText = 'No textual data available.  No rule selected.';
             When not %found;
                scTrgText = 'No textual data available.  Record not found.';
             When %found;
                scTrgText = %trim(xTRcond) + ' ... ' + %trim(xTRactions);
             Endsl;
             Write sflTrigTxt;
          Endif;

          Reade intCo# ptr00;
       Enddo;

      /end-free
     p LoadTrgGrid     e

       //===================================================================
       // Load Recipient Grid
       //===================================================================
     p LoadRecipient   b
     d LoadRecipient   pi

      /free

       clear dbrrn6;
       intRecCnt = 0;
       intRecCntE = 0;
       intRecCntI = 0;

       If flgtrgMode = 'A';
          Exsr LoadRecpAdd;
       Else;
          Exsr LoadRecpEdt;
       Endif;


       intRecCnt  = rrnTrgRcpt;
       rrnTrgRcpt = 1;

       //===================================================================
       // Load Data
       //===================================================================
       BegSr LoadDataRec;

       scRcptRole = ' ';
       scAltnRole = ' ';
       scRcpHdg = 'Role/User,Alternate Role/User';
       scShowDgst = *off;

       Select;
       When TRtype = 'Role';
          xxTBID = 'AXRO';
          xxKey  = TRrecip;
          Chain(n) keySTGTL pgt00;
          If %found;
             scRcptRole = %trim(GTDesc);
          Else;
             scRcptRole = %trim(TRrecip);
          Endif;
          intRecCntI += 1;
       When TRtype = 'Individual';
          scRcptRole = %trim(GetUserName(TRrecip));
          intRecCntI += 1;
       When TRtype = 'External';
          scRcptRole = %trim(GetExtRoleDesc(TRrecip));
          intRecCntE += 1;
       When TRtype = 'Email';
          scRcpHdg = 'Email Address,Text (SMS) Number,Digest';
          scShowDgst = *on;
          scDigest = TRdigest;
          scEmail = TRemail;
          scText  = TRtext;
          scRcptRole = TRemail;
          scAltnRole = %trim(%editc(TRtext:'Z'));
          intRecCntE += 1;
       Endsl;

       Select;
       When TRaltType = 'Role';
          xxTBID = 'AXRO';
          xxKey  = TRaltRecip;
          Chain(n) keySTGTL pgt00;
          If %found;
             scAltnRole = %trim(GTdesc);
          Else;
             scAltnRole = %trim(TRaltRecip);
          Endif;
       When TRaltType = 'Individual';
          scAltnRole = %trim(GetUserName(TRaltRecip));
       When TRtype = 'External';
          scAltnRole = %trim(GetExtRoleDesc(TRaltRecip));
       Endsl;

       rrnTrgRcpt = rrnTrgRcpt + 1;

       clear scRecRwSel;
       If rrnTrgRcpt = 1;
          scRecRwSel = 'A';
       Endif;

       Select;
       When flgTrgMode = ' ';
          Write sflTrgRcpt;
       When isFileTrig = *on;
          Write sflTrgRcpF;
       Other;
          Write sflTrgRcp;
       Endsl;

       Endsr;
       //===================================================================
       // Load Recipient based on mode add
       //===================================================================
       BegSr LoadRecpAdd;

       clear dbrrn7;

       Setll (intTmpCo#:strTRName:intTmpSeq) SMTRLRC1;
       Reade(n) (intTmpCo#:strTRName:intTmpSeq) SMTRLRC1;

       Dow not %eof(SMTRLRC1);
          scRecptRrn = dbrrn7;
          Exsr LoadDataRec;

          Reade(n) (intTmpCo#:strTRName:intTmpSeq) SMTRLRC1;
       Enddo;

       Endsr;
       //===================================================================
       // Load Recipient based on mode edit
       //===================================================================
       BegSr LoadRecpEdt;

       Setll (mTRco#:mTRname:mTRseq#) ptr0rc3;
       Reade(n) (mTRco#:mTRname:mTRseq#) ptr0rc3;

       Dow not %eof(SMTRLREC);
          scRecptRrn = dbrrn6;
          Exsr LoadDataRec;

          Reade(n) (mTRco#:mTRname:mTRseq#) ptr0rc3;
       Enddo;

       Endsr;

      /end-free
     p LoadRecipient   e

       //===================================================================
       // Load External Role choices
       //===================================================================
     P LoadExtRoles    b
     d LoadExtRoles    pi
     d  inType                        1    const
     d  outRecCh                           like(scRecCh)
     d  outRecChVal                        like(scRecChVal)
      /Free
       Clear outRecCh;
       Clear outRecChVal;
       Setll (intCo#:'AXRO') stgtl;
       Dou %eof;
          Reade (intCo#:'AXRO') stgtl;
          Select;
          When %eof;
             Iter;
          When GTstat = 'I';
             Iter;
          When GTfg02 <> 'E';
             Iter;
          When GTfg01 <> inType;
             Iter;
          Endsl;
          outRecCh = %trim(outRecCh) + ',' + %trim(GetExtRoleDesc(GTkey));
          outRecChVal = %trim(outRecChVal) + ',' + GTkey;
       Enddo;
       Return;
      /End-free
     P LoadExtRoles    e
       //===================================================================
       // Load Common elements for auto triggers
       //===================================================================
     p LoadTrgDtl      b
     d LoadTrgDtl      pi

      /free

       // Process common elements

       If rrnTrgRow = 0;
          rrnTrgRow = 1;
       Endif;

       // Read from subfile
       Chain rrnTrgRow sflAutoTrg;

       If %found;
          Chain(n) scTrgRrn ptr0p;
          If not %found(SMTRP);
             clear ptr0p;
          Endif;
       Else;
          clear ptr0p;
       Endif;

       vsDtlStDsc = *off;
       xxTBID = 'OCST';
       Chain (mTRco#:xxTbId) pgtw0;
       If %Found();
          vsDtlStDsc = *on;
          scStCdGTop = 570;
          scStCdRlTp = 740;
       Else;
          scStCdRlTp = 570;
       Endif;

       visSpltWg = *off;
       If clientId = '146';
          visSpltWg = *on;
       Endif;

       scDtlTrig = mTRname;
       scDtlSeq# = mTRseq#;
       scDtlDesc = %trim(mTRdesc);

       // Load textual detail
       Clear scTrgText;
       Chain(n) (mTRco#:mTRname:mTRseq#) smtrptxt;
       Select;
       When mTRname = *blanks;
          scTrgText = 'No textual data available.  No rule selected.';
       When not %found;
          scTrgText = 'No textual data available.  Record not found.';
       When %found;
          scTrgText = %trim(xTRcond) + ' ... ' + %trim(xTRactions);
       Endsl;

       // Load DARTS information
       Chain mTRdart pct01;
       If %found(STCTL1);
          scDiaryCd = mTRdart + ' - ' + %trim(CTdesc);
       Else;
          scDiaryCd = mTRdart;
       Endif;

       // diary date
       xxTBID = 'AXDI';
       xxKey  = mTRdiaryDt;
       Chain(n) keySTGTL pgt00;
       If %found and GTstat <> 'I';
          scDiaryDt = %trim(GTDesc);
       Else;
          scDiaryDt = %trim(xxKey);
       Endif;

       // number of occurences
       xxTBID = 'AXOC';
       xxKey  = mTR#occur;
       Chain(n) keySTGTL pgt00;
       If %found and GTstat <> 'I';
          scNumOcc = %trim(GTDesc);
       Else;
          scNumOcc = %trim(xxKey);
       Endif;

       scDtlDays = mTRaddlDays;
       scSecLvl  = mTRsecLvl;

       // Load the Groups
       Exsr LoadGrpGrid;

       // Load the Action(s)
       Exsr LoadActionGrid;

       // Load Recipient Grid info
       Exsr LoadRecInfo;

       Select;
       When isClmTrig = *on;
          // Load Claim trigger control info
          LoadClmInfo();
       When isPolTrig = *on;
        //***  Begin Add  ***  06/15/22  *************
          vsDtlStDsc = *off;
        //***  End   Add  ***  06/15/22  *************
          // Load Policy trigger control info
          LoadPolInfo();
       When isFileTrig = *on;
        //***  Begin Add  ***  06/15/22  *************
          vsDtlStDsc = *off;
        //***  End   Add  ***  06/15/22  *************
          // Load file trigger control info
          LoadFileInfo('Detail');
          dspFileFld = *on;
          clrFileFld = *off;
          rrnFileFld = 1;
          dspFldRpt = *on;
          clrFldRpt = *off;
          rrnFldRpt = 1;
       Endsl;

       //===================================================================
       // Load Action(s) Grid
       //===================================================================
       BegSr LoadActionGrid;

        // clear the action subfile
        dspActGrd = *off;
        clrActGrd = *on;
        Write rcdAutoTrg;
        clrActGrd = *off;
        dspActGrd = *on;
        rrnActGrd = 0;

        Clear aTRaction;
        Setll (mTRco#:mTRname:mTRseq#) ptr0act;
        Dou %eof(SMTRPACT);
           Reade(n) (mTRco#:mTRname:mTRseq#) ptr0act;
         //If %eof;
         //   Iter;
         //Endif;
           Select;
           When %eof and aTRaction = *blank and isFileTrig = *on;
              aTRaction = 'XML';
              aTRexternal = 'E';
           When %eof and aTRaction = *blank;
              aTRaction = 'Send Diary';
              aTRexternal = 'I';
           When %eof;
              Iter;
           Endsl;
           scAction = ATRaction;
           Select;
           When ATRexternal = 'E';
              scExternal = 'External';
           When ATRexternal = 'I';
              scExternal = 'Internal';
           Endsl;
           rrnActGrd = rrnActGrd + 1;
           Write sflRuleAct;
        Enddo;
        rrnActGrd = 1;

       Endsr;

       //===================================================================
       // Load Group Grid
       //===================================================================
       BegSr LoadGrpGrid;

       // clear the group subfile.
       dspTrigGrp = *off;
       clrTrigGrp = *on;
       Write rcdAutoTrg;
       clrTrigGrp = *off;
       dspTrigGrp = *on;
       rrnTrigGrp = 0;

       Setll (mTRco#:mTRname:mTRseq#) ptr0grp;
       Reade(n) (mTRco#:mTRname:mTRseq#) ptr0grp;

       Dow not %eof(SMTRPGRP);
          scTrgGrps = GetFndName(GTRfnd:scTrgGrpEx);
          rrnTrigGrp = rrnTrigGrp + 1;
          Write sflTrigGrp;

          Reade(n) (mTRco#:mTRname:mTRseq#) ptr0grp;
       Enddo;
       rrnTrigGrp = 1;

       Endsr;
       //===================================================================
       // Load Recipient Info
       //===================================================================
       BegSr LoadRecInfo;

       // clear the group subfile.
       dspTrgRcpt = *off;
       clrTrgRcpt = *on;
       Write rcdAutoTrg;
       clrTrgRcpt = *off;
       dspTrgRcpt = *on;
       rrnTrgRcpt = 0;

       flgTrgMode = ' ';
       LoadRecipient();

       Endsr;

      /end-free
     p LoadTrgDtl      e

       //===================================================================
       // Load Brand
       //===================================================================
     p LoadBrand       b
     d LoadBrand       pi
      /free
       // Clear the subfile.
       rrnBrand = 0;
       sfdBrand = *off;
       clrBrand = *on;
       Write rcdBrand;
       clrBrand = *off;
       sfdBrand = *on;

       // Load the Output Control subfile
       Setll *loval plg01;

       Dou %eof;
          Read  plg01;
          If not %eof;

             ScProcess = LGProcess;
             ScPath = LGPath;
             ScBSeq#= LGSeq#;
             ScBCo#= LGCo#;

             If LGStat = 'A';
                ScStat = 'Active';
             Else;
                ScStat = 'Inactive';
             Endif;

             scBrnRRN  = dbrrnF;

           //isVisEdit = (isUseScrp = *on or isUse3rd = *on) and not dspMode;
           //****  visedtBrn = *on;
           //****  visdelBrn = *on;
             If Not Dspmode;
                visedtBrn = *on;
                visdelBrn = *on;
             Else;
                visedtBrn = *off;
                visdelBrn = *off;
             Endif;

             rrnBrand = rrnBrand + 1;
             Write sflBrand;
          Endif;

       Enddo;

       rrnBrand = 1;

      /end-free
     p LoadBrand       e

       //===================================================================
       // Load Claim trigger control Info
       //===================================================================
     p LoadClmInfo     b
     d LoadClmInfo     pi

     d strState        s             10    inz
      /free

        // Claim specific
        Exsr LoadClmSpcfc;

        // Load Jurisdiction state
        Exsr LoadJrstSt;

        // Load Claim Indicators
        Exsr LoadClmInd;

        // Load NCCI Injury codes
        Exsr LoadNCCICd;

        // Load Nature Codes
        Exsr LoadNtrCd;

        // Load Part Codes
        Exsr LoadPartCd;

        // Load Cause Code
        Exsr LoadCauseCd;

        // Load Payment Code
        Exsr LoadPmtCd;

        // Load Rsv Codes
        Exsr LoadRsvCd;

        // Load Status Descriptions
        If vsDtlStDsc = *on;
           Exsr LoadStsDesc;
        Endif;

       //===================================================================
       // Load Claim specific info
       //===================================================================
       BegSr LoadClmSpcfc;

       Chain(n) (mTRco#:mTRname:mTRseq#) ptr0clm;
       If %found(SMTRPCLM);

          scClmSts = CTRclmSts;
          If CTRclmSts = ' ';
             scClmSts = 'Both';
          Endif;

          scClmType = ' ';
          scClmTyp1 = ' ';
          scClmTyp2 = ' ';
          If %scan('I':CTRclmType) > 0 or CTRclmType = ' ';
             scClmType = 'Indemnity';
          Endif;
          If %scan('M':CTRclmType) > 0 or CTRclmType = ' ';
             If scClmType = ' ';
                scClmType = 'Medical Only';
             Else;
                scClmTyp1 = 'Medical Only';
             Endif;
          Endif;
          If %scan('R':CTRclmType) > 0 or CTRclmType = ' ';
             Select;
             When scClmType = ' ';
                scClmType = 'Record Only';
             When scClmTyp1 = ' ';
                scClmTyp1 = 'Record Only';
             Other;
                scClmTyp2 = 'Record Only';
             Endsl;
          Endif;

          scClmAmt   = CTRamt;
          scPmtCnt   = CTRpayCount;
          scSpcfcRet = CTRreten;
          scTotPaid  = CTRtotPaid;
          scTotIcur  = CTRtotInc;
          scCmpWksPd = CTRweeksPd;

          Select;
             When CTRrset = 'Y';
                scRetroF = 'Yes';
             Other;
                scRetroF = 'No';
          Endsl;

          scSplitWg0 = 'No';
          Select;
          When CTRspltWg0 = 'Y';
             scSplitWg0 = 'Yes';
          Endsl;

          scSplitWg = 'No';
          Select;
          When CTRspltWg = 'Y';
             scSplitWg = 'Yes';
          Endsl;

       Else;
          scClmSts = ' ';
          scClmType = ' ';
          scClmTyp1 = ' ';
          scClmTyp2 = ' ';
          scClmAmt   = 0;
          scPmtCnt   = 0;
          scSpcfcRet = 0;
          scTotPaid  = 0;
          scTotIcur  = 0;
          scCmpWksPd = 0;
       Endif;

       Endsr;
       //===================================================================
       // Load Jurisdiction State
       //===================================================================
       BegSr LoadJrstSt;

       // clear the subfile.
       dspTrgJst = *off;
       clrTrgJst = *on;
       Write rcdAutoTrg;
       clrTrgJst = *off;
       dspTrgJst = *on;
       rrnTrgJst = 0;
       strState  = ' ';

       Setll (mTRco#:mTRname:mTRseq#:'States') ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:'States') ptr0tbl;

       Dow not %eof(SMTRPTBL);
          Chain TTRkey pst01;
          If %found(STSTL1) and STname <> ' ';
             scJurstSt = %trim(STname);
          Else;
             scJurstSt = %trim(TTRkey);
          Endif;
          strState = TTRkey;

          rrnTrgJst = rrnTrgJst + 1;
          Write sflTrgJst;

          Reade(n) (mTRco#:mTRname:mTRseq#:'States') ptr0tbl;
       Enddo;
       #ofTrgJst = rrnTrgJst;
       rrnTrgJst = 1;

       Endsr;
       //===================================================================
       // Load Claim Indicators
       //===================================================================
       BegSr LoadClmInd;

       // clear the subfile.
       dspTrgInd = *off;
       clrTrgInd = *on;
       Write rcdAutoTrg;
       clrTrgInd = *off;
       dspTrgInd = *on;
       rrnTrgInd = 0;

       Setll (mTRco#:mTRname:mTRseq#:'Claim Indicator') ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:'Claim Indicator') ptr0tbl;

       Dow not %eof(SMTRPTBL);

          xxTbId = 'AXCI';
          xxKey  = TTRkey;
          Chain(n) keySTGTL pgtw0;
          If %found;
             scClmInd = %trim(wGTDesc);
          Else;
             scClmInd = %trim(TTRkey);
          Endif;

          rrnTrgInd = rrnTrgInd + 1;
          Write sflTrgInd;

          Reade(n) (mTRco#:mTRname:mTRseq#:'Claim Indicator') ptr0tbl;
       Enddo;
       rrnTrgInd = 1;

       Endsr;

       //===================================================================
       // Load NCCI Injury Codes
       //===================================================================
       BegSr LoadNCCICd;

       // clear the subfile.
       dspTrgNCCI = *off;
       clrTrgNCCI = *on;
       Write rcdAutoTrg;
       clrTrgNCCI = *off;
       dspTrgNCCI = *on;
       rrnTrgNCCI = 0;

       Setll (mTRco#:mTRname:mTRseq#:'NCCI Injury') ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:'NCCI Injury') ptr0tbl;

       Dow not %eof(SMTRPTBL);

          If #ofTrgJst = 1;
             xxTbId = %trim(strState) + 'NC';
             Chain (mTRco#:xxTbId) pgtw0;
             If not %found;
                xxTbId = '  ' + 'NC';
             Endif;
          Else;
             xxTbId = '  ' + 'NC';
          Endif;

          xxKey  = TTRkey;
          Chain(n) keySTGTL pgtw0;
          If %found;
             scNCCIInj = %trim(wGTDesc);
          Else;
             scNCCIInj = %trim(TTRkey);
          Endif;

          rrnTrgNCCI = rrnTrgNCCI + 1;
          Write sflTrgNCCI;

          Reade(n) (mTRco#:mTRname:mTRseq#:'NCCI Injury') ptr0tbl;
       Enddo;
       rrnTrgNCCI = 1;

       Endsr;
       //===================================================================
       // Load Nature Codes
       //===================================================================
       BegSr LoadNtrCd;

       // clear the subfile.
       dspTrgNtrC = *off;
       clrTrgNtrC = *on;
       Write rcdAutoTrg;
       clrTrgNtrC = *off;
       dspTrgNtrC = *on;
       rrnTrgNtrC = 0;

       Setll (mTRco#:mTRname:mTRseq#:'Nature') ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:'Nature') ptr0tbl;

       Dow not %eof(SMTRPTBL);

          Chain(n) (mTRco#:TTRkey) pna00;
          If %found;
             scNatrCd = %trim(NAdesc);
          Else;
             scNatrCd = %trim(TTRkey);
          Endif;

          rrnTrgNtrC = rrnTrgNtrC + 1;
          Write sflTrgNtrC;

          Reade(n) (mTRco#:mTRname:mTRseq#:'Nature') ptr0tbl;
       Enddo;
       rrnTrgNtrC = 1;

       Endsr;

       //===================================================================
       // Load Part Codes
       //===================================================================
       BegSr LoadPartCd;

       // clear the subfile.
       dspTrgPrtC = *off;
       clrTrgPrtC = *on;
       Write rcdAutoTrg;
       clrTrgPrtC = *off;
       dspTrgPrtC = *on;
       rrnTrgPrtC = 0;

       Setll (mTRco#:mTRname:mTRseq#:'Part') ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:'Part') ptr0tbl;

       Dow not %eof(SMTRPTBL);

          Chain(n) (mTRco#:TTRkey) ppa00;
          If %found;
             scPartCd = %trim(PAdesc);
          Else;
             scPartCd = %trim(TTRkey);
          Endif;

          rrnTrgPrtC = rrnTrgPrtC + 1;
          Write sflTrgPrtC;

          Reade(n) (mTRco#:mTRname:mTRseq#:'Part') ptr0tbl;
       Enddo;
       rrnTrgPrtC = 1;

       Endsr;

       //===================================================================
       // Load Cause Codes
       //===================================================================
       BegSr LoadCauseCd;

       // clear the subfile.
       dspTrgCaus = *off;
       clrTrgCaus = *on;
       Write rcdAutoTrg;
       clrTrgCaus = *off;
       dspTrgCaus = *on;
       rrnTrgCaus = 0;

       Setll (mTRco#:mTRname:mTRseq#:'Cause') ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:'Cause') ptr0tbl;

       Dow not %eof(SMTRPTBL);

          Chain(n) (mTRco#:TTRkey) pca00;
          If %found;
             scCausCd = %trim(CAdesc);
          Else;
             scCausCd = %trim(TTRkey);
          Endif;

          rrnTrgCaus = rrnTrgCaus + 1;
          Write sflTrgCaus;

          Reade(n) (mTRco#:mTRname:mTRseq#:'Cause') ptr0tbl;
       Enddo;
       rrnTrgCaus = 1;

       Endsr;

       //===================================================================
       // Load Payment Codes
       //===================================================================
       BegSr LoadPmtCd;

       // clear the subfile.
       dspTrgPmtC = *off;
       clrTrgPmtC = *on;
       Write rcdAutoTrg;
       clrTrgPmtC = *off;
       dspTrgPmtC = *on;
       rrnTrgPmtC = 0;

       Setll (mTRco#:mTRname:mTRseq#:'Payment') ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:'Payment') ptr0tbl;

       Dow not %eof(SMTRPTBL);

          Chain(n) (mTRco#:TTRkey) pb200;
          If %found;
             scPmtCd = %trim(B2desc);
          Else;
             scPmtCd = %trim(TTRkey);
          Endif;

          rrnTrgPmtC = rrnTrgPmtC + 1;
          Write sflTrgPmtC;

          Reade(n) (mTRco#:mTRname:mTRseq#:'Payment') ptr0tbl;
       Enddo;
       rrnTrgPmtC = 1;

       Endsr;

       //===================================================================
       // Load Reserve Codes
       //===================================================================
       BegSr LoadRsvCd;

       // clear the subfile.
       dspTrgRsvC = *off;
       clrTrgRsvC = *on;
       Write rcdAutoTrg;
       clrTrgRsvC = *off;
       dspTrgRsvC = *on;
       rrnTrgRsvC = 0;

       Setll (mTRco#:mTRname:mTRseq#:'Reserve') ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:'Reserve') ptr0tbl;

       Dow not %eof(SMTRPTBL);

          Chain(n) (mTRco#:TTRkey) prt00;
          If %found;
             scRsvCd = %trim(RTdesc);
          Else;
             scRsvCd = %trim(TTRkey);
          Endif;

          rrnTrgRsvC = rrnTrgRsvC + 1;
          Write sflTrgRsvC;

          Reade(n) (mTRco#:mTRname:mTRseq#:'Reserve') ptr0tbl;
       Enddo;
       rrnTrgRsvC = 1;

       Endsr;

       //===================================================================
       // Load Status Descriptions
       //===================================================================
       BegSr LoadStsDesc;

       // clear the subfile.
       dspTrgStDs = *off;
       clrTrgStDs = *on;
       Write rcdAutoTrg;
       clrTrgStDs = *off;
       dspTrgStDs = *on;
       rrnTrgStDs = 0;

       Setll (mTRco#:mTRname:mTRseq#:'Status Description') ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:'Status Description') ptr0tbl;

       Dow not %eof(SMTRPTBL);

          xxTbId = 'OCST';
          xxKey  = TTRkey;
          Chain(n) keySTGTL pgtw0;
          If %found;
             scStatDesc = %trim(wGTDesc);
          Else;
             scStatDesc = %trim(TTRkey);
          Endif;

          rrnTrgStDs = rrnTrgStDs + 1;
          Write sflTrgStDs;

          Reade(n) (mTRco#:mTRname:mTRseq#:'Status Description') ptr0tbl;
       Enddo;
       rrnTrgStDs = 1;

       Endsr;

      /end-free
     p LoadClmInfo     e

       //===================================================================
       // Load file trigger control Info
       //===================================================================
     pLoadFileInfo     b
     dLoadFileInfo     pi
     d inMode                        10    const

     d delimiter       s              1    inz
     dstrSqlCursor     s           1000
     dstrType          s             10

     disColumnsEOF     s              1n

     ddsColumns        ds                  qualified
     d label                        102
     d type                          10
     d length                         9  0
     d dec                            9  0
     d field                         10
     daryColumnsNull   s              5i 0 dim(5)
     ddsFields         ds                  qualified inz
     d field                         10
     d type                          25
     d operCur                       25
     d compValueCur                 100
     d compTypeCur                  100
     d operPrv                       25
     d compValuePrv                 100
     d compTypePrv                  100
     daryFieldsNull    s              5i 0 dim(8)
     ddsFieldsChg      ds                  likeds(dsFields) inz
     ddsFieldsRpt      ds                  likeds(dsFields) inz
     ddsFiles          ds                  qualified
     d co#                            3  0
     d name                          10
     d seq#                           5  0
     d file                          10
     d library                       10
     daryFilesNull     s              5i 0 dim(5)
      /Free
       Clear scLibrary;
       Clear scLib;
       Clear scFile;
       Chain(n) (mTRco#:mTRname:mTRseq#) ptr0fil;
       scLibrary = fTRlibrary;
       scLib = fTRlibrary;
       scFile = fTRfile;
       strFile = scFile;
       scFileDtl = GetFileDesc(fTRfile:fTRlibrary:'*BEFORE');
       Clear delimiter;
       Clear scChgEvent;
       Clear scExitPgm;
       // Load trigger requirements
       Setll (mTRco#:mTRname:mTRseq#) ptr0frq;
       Dou %eof(SMTRPFRQ);
          Reade(n) (mTRco#:mTRname:mTRseq#) ptr0frq;
          Select;
          When %eof;
             Iter;
          When qTRstat = 'I';
             Iter;
          Endsl;
          If scChgEvent <> *blanks;
             delimiter = ',';
          Endif;
          scChgEvent = %trim(scChgEvent) + %trim(delimiter) + qTRtrgEvent;
          scExitPgm = qTRexitPgm;
       Enddo;
       Select;
       When inMode = 'Detail';
          dspFileFld = *off;
          clrFileFld = *on;
          dspFldRpt = *off;
          clrFldRpt = *on;
          Write rcdAutoTrg;
          clrFileFld = *off;
          dspFileFld = *on;
          rrnFileFld = 0;
          clrFldRpt = *off;
          dspFldRpt = *on;
          rrnFldRpt = 0;
       Endsl;
       Clear delimiter;
       Clear scFieldChg;
       scCond = *off;
       // Load fields
       Exsr LoadFieldDetail;
       Setll (mTRco#:mTRname:mTRseq#:'Change') ptr0fld;
       Dou %eof(SMTRPFLD);
          Reade(n) (mTRco#:mTRname:mTRseq#:'Change') ptr0fld;
          Select;
          When %eof;
             Iter;
          When dTRstat = 'I';
             Iter;
          Endsl;
          If scFieldChg <> *blanks;
             delimiter = ',';
          Endif;
          Select;
          When inMode = 'Detail';
          When inMode = 'Detail';
             scFileFld = GetFieldDesc(dTRfield:fTRfile:fTRlibrary:'*BEFORE');
             rrnFileFld = rrnFileFld + 1;
             Write sflFileFld;
          Endsl;
          scFieldChg = %trim(scFieldChg) + delimiter + dTRfield;
          If dTRoperCur <> *blank or dTRoperPrv <> *blank;
             scCond = *on;
          Endif;
       Enddo;
       scFieldChg = %trim(scFieldChg);
       rrnFileFld = 1;
       Clear delimiter;
       Clear scFieldRpt;
       Setll (mTRco#:mTRname:mTRseq#:'Report') ptr0fld;
       Dou %eof(SMTRPFLD);
          Reade(n) (mTRco#:mTRname:mTRseq#:'Report') ptr0fld;
          Select;
          When %eof;
             Iter;
          When dTRstat = 'I';
             Iter;
          Endsl;
          If scFieldRpt <> *blanks;
             delimiter = ',';
          Endif;
          Select;
          When inMode = 'Detail';
             scFileFld = GetFieldDesc(dTRfield:fTRfile:fTRlibrary:'*BEFORE');
             rrnFldRpt = rrnFldRpt + 1;
             Write sflFldRpt;
          Endsl;
          scFieldRpt = %trim(scFieldRpt) + delimiter + dTRfield;
       Enddo;
       scFieldRpt = %trim(scFieldRpt);
       // Load when conditions
       Clear wTRwhen;
       Chain(n) (mTRco#:mTRname:mTRseq#) ptr0whn;
       scWhen = wTRwhen;
       visWhenDtl = *off;
       If scWhen <> *blank;
          visWhenDtl = *on;
       Endif;
       // Load SQL action
       Clear sqlTRsqlAct;
       Chain(n) (mTRco#:mTRname:mTRseq#) ptr0sql;
       scSqlAct = sqlTRsqlAct;
       //===================================================================
       // Load the field detail grid
       //===================================================================
       Begsr LoadFieldDetail;
        dspFileFld = *off;
        clrFileFld = *on;
        Write rcdAutoTrg;
        clrFileFld = *off;
        rrnFileFld = 0;
        Exec SQL
         select TRco#, TRname, TRseq#, TRfile, TRlibrary
          into :dsFiles :aryFilesNull
          from SMTRPFIL
          where TRco# = :mTRco#
           and  TRname = :mTRname
           and  TRseq# = :mTRseq#;
        If SQLSTT <> '00000';
           Reset dsFiles;
        Endif;
        strSqlCursor = 'select LABELTEXT, COLTYPE, LENGTH, SCALE, ' +
         ' SYS_CNAME ' +
         ' from SYSCOLUMNS ' +
         ' where sys_dname=''' + %trim(upper(dsFiles.library)) + '''' +
         '  and  sys_tname=''' + %trim(upper(dsFiles.file)) + '''';
        Exec SQL Prepare sqlColumns From :strSqlCursor;
        Exec SQL Declare cursorColumns Cursor For sqlColumns;
        Exec SQL Open cursorColumns;
        Dou isColumnsEOF;
           Exec SQL Fetch next From cursorColumns
            into :dsColumns :aryColumnsNull;
           isColumnsEOF = SQLSTT <> '00000';
           Select;
           When isColumnsEOF;
              Iter;
           Endsl;
           strType = 'Report';
           Exsr ReadFieldData;
           dsFieldsRpt = dsFields;
           Select;
           When SQLSTT = '00000';
              scFldRpt = *on;
           Other;
              scFldRpt = *off;
              Reset dsFieldsRpt;
           Endsl;
           strType = 'Change';
           Exsr ReadFieldData;
           dsFieldsChg = dsFields;
           Select;
           When SQLSTT = '00000';
              scFldChg = *on;
           Other;
              scFldChg = *off;
              Reset dsFieldsChg;
           Endsl;
           Select;
           When scFldChg = *off and scFldRpt = *off;
              Iter;
           When scFldChg = *off and scFldRpt = *on;
              dsFields = dsFieldsRpt;
           Other;
              dsFields = dsFieldsChg;
           Endsl;
           scFldInfo = %trim(dsColumns.field) + ' - ' + dsColumns.label;
           scField = %trim(dsColumns.field) + ' - ' + dsColumns.label;
           scFileFld = %trim(dsColumns.field) + ' - ' + dsColumns.label;
           Select;
           When dsFields.type <> *blanks;
              scFieldTyp = dsFields.type;
           When dsColumns.length = 7 and dsColumns.dec = 0;
              scFieldTyp = '7.0';
           Other;
              scFieldTyp = dsColumns.type;
           Endsl;
           scOperCur = dsFields.operCur;
           scCmpValC = dsFields.compValueCur;
           scCmpTypC = dsFields.compTypeCur;
           scOperPrv = dsFields.operPrv;
           scCmpValP = dsFields.compValuePrv;
           scCmpTypP = dsFields.compTypePrv;
           rrnFileFld = rrnFileFld + 1;
           Write sflFileFld;
        Enddo;
        Exec SQL Close cursorColumns;
       Endsr;
       //===================================================================
       // Read the field data via SQL
       //===================================================================
       Begsr ReadFieldData;
        Reset dsFields;
        Exec SQL
         select TRfield, TRfieldTyp,
          TRoperCur, TRcmpValC, TRcmpTypC,
          TRoperPrv, TRcmpValP, TRcmpTypP
          into :dsFields :aryFieldsNull
          from SMTRPFLD
          where TRco# = :dsFiles.co#
           and  TRname = :dsFiles.name
           and  TRseq# = :dsFiles.seq#
           and  TRfield = :dsColumns.field
           and  TRtype = :strType
           and  TRstat <> 'I';
       Endsr;
      /End-free
     pLoadFileInfo     e
       //===================================================================
       // Load Policy trigger control Info
       //===================================================================
     p LoadPolInfo     b
     d LoadPolInfo     pi

     d charPolTyp      s              1    inz
     d charQteSts      s              1    inz
      /free

        // Load policy detail box info
       Chain(n) (mTRco#:mTRname:mTRseq#) ptr0pol;
       If %found(SMTRPPOL);
          Select;
          When PTRpolStsTp = 'W';
             scStsType  = 'Live';
          When PTRpolStsTp = 'Q';
             scStsType  = 'Quote';
          Other;
             scStsType  = 'Both';
          Endsl;

          Select;
          When PTRpolSts = ' ';
             scPolSts  = 'Both';
          Other;
             scPolSts  = PTRpolSts;
          Endsl;

          scPremLvl  = PTRpremLvl;
          scQteTyp   = PTRqteType;
          charPolTyp = %trim(PTRpolStsTp);
          scPolEffDt = PTR#daysPr;
          scPremAmt  = PTRpremAmt;
          scValChg   = PTRchgVal;
          scPerChg   = PTRchg;
          scCompDate = PTRCompDate;
          scCompOper = PTRCompOper;
          If scCompDate <> *blank;
             isCompVis = *On;
          Else;
             isCompVis = *Off;
          Endif;
       Else;
          scStsType  = ' ';
          scPolSts   = ' ';
          scPremLvl  = ' ';
          scQteTyp   = ' ';
          charPolTyp = ' ';
          scPolEffDt = 0;
          scPremAmt  = 0;
          scValChg   = 0;
          scPerChg   = 0;
          scCompDate = *blank;
          scCompOper = *blank;
          isCompVis = *Off;
       Endif;

        // Load Quote Status
        Exsr LoadQteSts;

       //===================================================================
       // Load Quote Status
       //===================================================================
       BegSr LoadQteSts;

       // clear the subfile.
       dspQteSts = *off;
       clrQteSts = *on;
       Write rcdAutoTrg;
       clrQteSts = *off;
       dspQteSts = *on;
       rrnQteSts = 0;

       Setll (mTRco#:mTRname:mTRseq#:'Quote Status') ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:'Quote Status') ptr0tbl;

       Dow not %eof(SMTRPTBL);

          charQteSts = %trim(TTRkey);

          If charPolTyp = ' ';
             Chain(n) (mTRco#:charQteSts) pqs01;
          Else;
             Chain(n) (mTRco#:charQteSts:charPolTyp) pqs01;
          Endif;

          If %found;
             scQteSts = %trim(QSdesc);
          Else;
             scQteSts = %trim(TTRkey);
          Endif;

          rrnQteSts = rrnQteSts + 1;
          Write sflQteSts;

          Reade(n) (mTRco#:mTRname:mTRseq#:'Quote Status') ptr0tbl;
       Enddo;
       rrnQteSts = 1;

       Endsr;

      /end-free
     p LoadPolInfo     e

       //===================================================================
       // Notify user of pending trigger
       //===================================================================
     pPendTrigNotice   b
     dPendTrigNotice   pi
     d inType                         1    const options(*nopass)

     dstrMsg           s            256
     dstrTitle         s             50
     dstrType          s              1    inz
      /Free
       If %parms >= 1;
          strType = inType;
       Endif;
       strTitle = 'Rule Pending';
       strMsg = 'The rule you are attempting to activate will remain in ' +
        'a Pending status until the trigger can be added.  This may take ' +
        'several hours.  This rule will not be available until active.';
       Select;
       When strType = 'I';
          strMsg = 'The rule you are attempting to inactivate will remain in ' +
           'a Pending Inactivate status until the trigger can be removed.  ' +
           'This may take several hours.  ' +
           'This rule will still be available until inactive.';
       Endsl;
       PopOneBtn(strMsg:strTitle);
      /End-free
     pPendTrigNotice   e

       //===================================================================
       // Add/Edit Auto Diary Trigger processing
       //===================================================================
     p ProcAddEdt      b
     d ProcAddEdt      pi

     d isDiaryAction   s               n   inz(*off)
     d isEOF           s               n
     d isNotValid      s               n   inz(*off)
     d isReload        s               n   inz(*on)
     d intSeq          s              5  0
     dstrActions       s                   like(xTRactions)
     dstrCond          s                   like(xTRcond)
     dstrGroup         s                   like(xTRcond)
     d strSelection    s            525
     d strSql          s           1000
     d strSqlCursor    s           2500
     dstrUsers         s                   like(xTRactions)
     d saveDate        s              7  0
     d saveTime        s              6  0
     d strTable        s                   like(tTrTable)
     d intUsaDate      s              8  0
     d dsColumns       ds                  qualified inz
     d  label                       102
     d  type                         10
     d  length                        9  0
     d  dec                           9  0
     d aryColumnsNull  s              5i 0 dim(4)
     d dsColumnsNull   ds                  qualified inz
     d  label                         5i 0
     d  type                          5i 0
     d  length                        5i 0
     d  dec                           5i 0
     d dsFieldComp     ds                  qualified inz
     d  rrn                           9  0
     d  field                        10
     d  type                         25
     d  operatorCur                  25
     d  valueCur                    100
     d  typeCur                      25
     d  operatorPrv                  25
     d  valuePrv                    100
     d  typePrv                      25
     d  file                         10
     d  lib                          10
     d  seq#                          5  0
     d  name                         10
     d dsAddCmpDt      ds            10
     d  dsCompMth                     2    overlay(dsAddCmpDt:1)
     d  dsSlash1                      1    overlay(dsAddCmpDt:3)
     d  dsCompDay                     2    overlay(dsAddCmpDt:4)
     d  dsSlash2                      1    overlay(dsAddCmpDt:6)
     d  dsCompCentury                 2    overlay(dsAddCmpDt:7)
     d  dsCompYear                    2    overlay(dsAddCmpDt:9)

      /free
       clear rcdTrigCfg;
       Clear rcdTrigFil;
       scTrgMode = flgTrgMode;
       scSMS = IsModEnabled('SMS Enabled');
       isSql = IsModEnabled('Rule SQL Action');
       isExitPgm = '1';
       scRuleHdgs = strRuleHdgs;
       scRuleHdg = strRuleHdg;
       scLblSetup = strLblSetup;
       scLblHdg = strLblHdg;
       scLblCtl = strLblCtl;
       isRules = isRulesEng;
       isRetro = isRetroI;
       isClmTrg = isClmTrig;
       isPolTrg = isPolTrig;
       isFileTrg = isFileTrig;
       Clear strFile;
       // Do not show expired groups
       scGrpSel1 = 'FNexDt = 0 or FNexDt >= ' + %char(%date:*cymd0);
       // Do not show expired groups
       scGrpSel2 = *blanks;
       visSpltWg = *off;
       If clientId = '146';
          visSpltWg = *on;
       Endif;

       Select;
       // Add trigger mode
       When flgTrgMode = 'A';
          dsbAddTrig = *off;
          scAddPrmLv = 'Normal';
          scTrigSts = 'A';
          scChgEvent = '*INSERT,*UPDATE,*DELETE';
          If %open(SMTRPRC1);
             Close SMTRPRC1;
          Endif;
          If %open(SMTRLRC1);
             Close SMTRLRC1;
          Endif;

          // In add trigger mode create temporary files in QTEMP library
          Monitor;
             clear strCommand;
             strCommand = 'DLTF FILE(QTEMP/SMTRLREC)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          Monitor;
             clear strCommand;
             strCommand = 'DLTF FILE(QTEMP/SMTRPREC)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
             If not CreateWorkfile('SMTRPREC') and
              PopOneBtn('Unable to create holding file for recipients. +
               Action can not be completed.':'Unable to Complete Action');
                Return;
             Endif;
          Endmon;

          Monitor;
             clear strCommand;
             strCommand = 'CRTDUPOBJ OBJ(SMTRPREC) FROMLIB(*LIBL) +
              OBJTYPE(*FILE) TOLIB(QTEMP) TRG(*NO)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
             If not CreateWorkfile('SMTRLREC') and
              PopOneBtn('Unable to create holding file for recipients. +
               Action can not be completed.':'Unable to Complete Action');
                Return;
             Endif;
          Endmon;

          Monitor;
             clear strCommand;
             strCommand = 'CRTDUPOBJ OBJ(SMTRLREC) FROMLIB(*LIBL) +
              OBJTYPE(*FILE) TOLIB(QTEMP) TRG(*NO)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;
          Open SMTRPRC1;
          Open SMTRLRC1;

       // Edit trigger mode
       When flgTrgMode = 'E';
          dsbAddTrig = *on;
          Exsr MoveDbSc;
       Endsl;

       isGrpVis = *on;
       isGenFile = *off;
       isGenFile = *on;
       If isRulesEng = *off;
          isGenFile = *off;
       Endif;
       Select;
       When isClmTrig = *on;
          BtnCanTop = 1130;
          scAddTrgHg = 1085;
          selAddTrg = 'GTtbId = ''AXKY''' + ' And (GTfg01 = ''C''' + ' or +
           GTfg01 = '' ''' + ') And GTstat = ''A'' And +
           GTco# = ' + %editc(intCo#:'X');
          chAddDryDt = 'GTtbId = ''AXDI''' + ' And (GTfg01 = ''C''' + ' or +
           GTfg01 = '' ''' + ') And GTstat = ''A''';
           scTrigType = 'C';
       When isPolTrig = *on;
          BtnCanTop = 835;
          scAddTrgHg = 785;
          scAddPlTp  = 620;
          selAddTrg = 'GTtbId = ''AXKY''' + ' And (GTfg01 = ''P''' + ' or +
           GTfg01 = '' ''' + ') And GTstat = ''A''';
          chAddDryDt = 'GTtbId = ''AXDI''' + ' And (GTfg01 = ''P''' + ' or +
           GTfg01 = '' ''' + ') And GTstat = ''A''';
           scTrigType = 'P';
       When isFileTrig = *on;
          scTrigType = 'X';
          isGrpVis = *off;
       Endsl;

       isExpVis = *off;
       If isGrpVis = *on and dsbAddTrig = *on;
          isExpVis = *on;
       Endif;
       Clear scOpenDlg;
       BtnTrigSav = *off;
       BtnTrigCan = *off;
       BtnCondSav = *off;
       BtnRecpAdd = *off;
       isNotValid = *off;
       isReload   = *on;
       Exsr setDefaults;
       Exsr setQteSts;

       //Dou BtnTrigSav = *on and isNotValid = *off;
       Dou (BtnTrigSav = *on or BtnCondSav = *on)and isNotValid = *off;
          BtnTrigSav = *off;
          BtnCondSav = *off;

          If isReload = *on;
             dspTrgRcp = *off;
             clrTrgRcp = *on;
             Write rcdTrigCfg;
             Write rcdTrigFil;
             clrTrgRcp = *off;
             dspTrgRcp = *on;
             rrnTrgRcpt = 0;

             // Load Recipient Grid info
             LoadRecipient();
          Endif;

          Select;
          When isFileTrig = *on;
             scAddTrig = 'FILE RULES';
             strFile = scFile;
             svChgEvent = scChgEvent;
             svFieldChg = scFieldChg;
             svFieldRpt = scFieldRpt;
             svWhen = scWhen;
             isFieldCmp = isCompareDone;
             Exfmt rcdTrigFil;
             If strFile <> scFile and scFile <> *blank;
                strFile = scFile;
             Endif;
          Other;
             Exfmt rcdTrigCfg;
          Endsl;
          Clear scOpenDlg;
          isNotValid = *off;
          isReload   = *off;
          erAddTrig  = *off;
          erAddAct   = *off;
          erAddDryCd = *off;
          erAddNmOcc = *off;
          erAddEffDt = *off;
          erAddPrmLv = *off;
          erAddPAmt  = *off;
          erAddVlChg = *off;
          erAddPrChg = *off;
          flgRecMode = ' ';

          Select;
          When BtnTrigSav = *on or BtnCondSav = *on;
             Exsr ValidateTrg;
             If isNotValid = *off;
                Exsr ProcSave;
              //Endif;
             Endif;

          When BtnTrigCan = *on;
             BtnTrigCan = *off;
             Leave;

          When BtnRecpAdd = *on;
             isReload = *on;
             flgRecMode = 'A';
             ProcRecpt();

          // Verify field defaults if Trigger is changed
          When BtnTrgChg = *on;
             BtnTrgChg = *off;
             clear scAddQtSts;
             Exsr setDefaults;
             Exsr setQteSts;

          // Verify/reload quote status if Status Type/ Quote Type is changed
          When BtnStsChg = *on;
             BtnStsChg = *off;
             clear scAddQtSts;
             Exsr setQteSts;

          Other;
             //  Check for clicked links on recipient grid.
             Dou isEOF;
                Select;
                When isFileTrig = *on;
                   Readc sflTrgRcpF;
                   isEOF = %eof;
                Other;
                   Readc sflTrgRcp;
                   isEOF = %eof;
                Endsl;
                If isEOF;
                   Iter;
                Endif;
                //  Edit recipient
                If scEdtRecpt = *on;
                   scEdtRecpt = *off;
                   flgRecMode = 'E';
                   isReload = *on;
                   ProcRecpt();
                Endif;

                //  delete recipient
                If scDelRecpt = *on;
                   scDelRecpt = *off;
                   isReload = *on;
                   strMsg = 'Do you want to continue with the deletion?';
                   If PopTwoBtn(strmsg:'Confirm':'Continue':'Cancel');
                      Exsr DelRecpt;
                   Endif;
                Endif;

             Enddo;
          Endsl;
       Enddo;

       If flgTrgMode = 'A';
          If %open(SMTRLRC1);
             Close SMTRLRC1;
          Endif;
          If %open(SMTRPRC1);
             Close SMTRPRC1;
          Endif;

          // In add trigger mode remove temporary files from QTEMP library
          Monitor;
             clear strCommand;
             strCommand = 'DLTF FILE(QTEMP/SMTRLREC)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-Error;
          Endmon;
          Monitor;
             clear strCommand;
             strCommand = 'DLTF FILE(QTEMP/SMTRPREC)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-Error;
          Endmon;
       Endif;

       //===================================================================
       // Load the comparison grid
       //===================================================================
       Begsr LoadCompGrid;
        dspFileCmp = *off;
        clrFileCmp = *on;
        Write rcdFileCmp;
        clrFileCmp = *off;
        rrnFileCmp = 0;
        strSqlCursor = 'select RRN(fld), fld.TRFIELD, fld.TRFIELDTYP, ' +
         'fld.TROPERCUR, fld.TRCMPVALC, fld.TRCMPTYPC, ' +
         'fld.TROPERPRV, fld.TRCMPVALP, fld.TRCMPTYPP, ' +
         'fil.TRFILE, fil.TRLIBRARY, ' +
         'fld.TRSEQ#, fld.TRNAME ' +
         'from SMTRPFLD as fld ' +
         'join SMTRPFIL as fil ' +
         ' on fld.TRCO#=fil.TRCO# and fld.TRNAME=fil.TRNAME and' +
         '    fld.TRSEQ#=fil.TRSEQ# ' +
         'where fld.TRCO#=' + %editc(mTRco#:'X') +
         ' and fld.TRNAME=''' + %trim(mTRname) + '''' +
         ' and fld.TRSEQ#=' + %editc(mTRseq#:'X') +
         ' and fld.TRTYPE=''Change''';
        Exec SQL Prepare sqlComp From :strSqlCursor;
        Exec SQL Declare cursorComp Cursor For sqlComp;
        Exec SQL Open cursorComp;
        Dou SQLSTT <> '00000';
           Exec SQL Fetch next From cursorComp into :dsFieldComp;
           Select;
           When SQLSTT <> '00000';
              Iter;
           Endsl;
           Exec SQL
            select LABELTEXT, COLTYPE, LENGTH, SCALE
             into :dsColumns :aryColumnsNull
             from SYSCOLUMNS
             where upper(:dsFieldComp.lib)=sys_dname
              and  upper(:dsFieldComp.file)=sys_tname
              and  upper(:dsFieldComp.field)=sys_cname;
           If SQLSTT <> '00000';
              Clear dsColumns;
           Endif;
           SQLSTT = '00000';
           scFldInfo = %trim(dsFieldComp.field) + ' - ' + dsColumns.label;
           scField = dsFieldComp.field;
           Select;
           When dsFieldComp.type <> *blanks;
              scFieldTyp = dsFieldComp.type;
           When dsColumns.length = 7 and dsColumns.dec = 0;
              scFieldTyp = '7.0';
           Other;
              scFieldTyp = dsColumns.type;
           Endsl;
           scOperCur = dsFieldComp.operatorCur;
           scCmpValC = dsFieldComp.valueCur;
           scCmpTypC = dsFieldComp.typeCur;
           scOperPrv = dsFieldComp.operatorPrv;
           scCmpValP = dsFieldComp.valuePrv;
           scCmpTypP = dsFieldComp.typePrv;
           scRRN = dsFieldComp.rrn;
           rrnFileCmp += 1;
           Write sflFileCmp;
        Enddo;
        Exec SQL Close cursorComp;
        dspFileCmp = *on;
       Endsr;

       //===================================================================
       // MoveDbSc
       //===================================================================
       Begsr MoveDbsc;

       // Read from subfile
       Chain rrnAuToTrg sflAutoTrg;

       If %found;
          Chain(n) scTrgRrn ptr0p;
          If %found(SMTRP);
             scAddTrig  = UPPER(mTRname);
             scAddDesc  = %trim(mTRdesc);
             scTrigSts = mTRstat;
             scAddDryCd = mTRdart;
             scAddDryDt = mTRdiaryDt;
             scAddNmOcc = mTR#occur;
             scAddDays  = mTRaddlDays;
             scAddScLvl = mTRsecLvl;
             scAddSeq#  = mTRseq#;
             SaveDate   = mTRdt;
             SaveTime   = mTRtime;

             // Load the Groups
             Setll (mTRco#:mTRname:mTRseq#) ptr0grp;
             Reade(n) (mTRco#:mTRname:mTRseq#) ptr0grp;
             Dow not %eof(SMTRPGRP);
                If scAddGrp = ' ';
                   scAddGrp = %editc(GTRFnd:'X');
                Else;
                   scAddGrp = %trim(scAddGrp) + ',' + %editc(GTRFnd:'X');
                Endif;
                Reade(n) (mTRco#:mTRname:mTRseq#) ptr0grp;
             Enddo;

             // Load the Actions
             Clear ATRaction;
             Setll (mTRco#:mTRname:mTRseq#) ptr0act;
             Dou %eof(SMTRPACT);
                Reade(n) (mTRco#:mTRname:mTRseq#) ptr0act;
                Select;
                When %eof and ATRaction = *blank;
                   ATRaction = 'Send Diary';
                When %eof;
                   Iter;
                Endsl;
                Select;
                When ATRexternal = 'E';
                   If scAddActE <> *blank;
                      scAddActE = %trim(scAddActE) + ',';
                   Endif;
                   scAddActE = %trim(scAddActE) + ATRaction;
                Other;
                   If scAddActI <> *blank;
                      scAddActI = %trim(scAddActI) + ',';
                   Endif;
                   scAddActI = %trim(scAddActI) + ATRaction;
                Endsl;
             Enddo;

             Select;
             When isClmTrig = *on;
                // Load Claim trigger control info
                Exsr LoadClaims;
             When isPolTrig = *on;
                // Load Policy trigger control info
                Exsr LoadPolicy;
             When isFileTrig = *on;
                // Load file trigger control info
                LoadFileInfo('Edit');
             Endsl;
          Endif;
       Endif;

       Endsr;

       //===================================================================
       // Validate Auto Diary Trigger
       //===================================================================
       Begsr ValidateFileRule;
         // At least one action must be selected
         Select;
         When scAddActE = *blank;
            mtAddAct = 'At least one action ' +
             'must be selected.';
            erAddAct = *on;
            isNotValid = *on;
         When scAddActE = 'Execute SQL';
            If GetUserEmail(q1user) = *blanks;
               mtAddAct = 'You must have an email address established in ' +
                'user configuration to add/edit a rule with an SQL statement.';
               erAddAct = *on;
               isNotValid = *on;
            Endif;
         Endsl;
         // At least one change field must be selected
         Select;
         When scFieldChg = *blank;
            mtFieldChg = 'At least one field must be selected.';
            erFieldChg = *on;
            isNotValid = *on;
         Endsl;
         // At least one report field must be selected
         Select;
         When scFieldRpt = *blank;
            mtFieldRpt = 'At least one field must be selected.';
            erFieldRpt = *on;
            isNotValid = *on;
         Endsl;
         If scWhen <> *blanks;
            strSql = 'CREATE or REPLACE TRIGGER testSQL ' +
             'AFTER INSERT on testLibSQL.testFilSQL ' +
             'REFERENCING NEW ROW AS N OLD ROW AS O ' +
             'FOR EACH ROW MODE DB2SQL WHEN (' +
             %trim(ScanReplace('WHEN':'':%trim(Upper(scWhen)):1:1)) + ') ' +
             'BEGIN END';
            If not ValidSqlSyntax(strSql:'*SQL');
               dsMsgInfo = RtvMsg('*');
               mtWhen = dsMsgInfo.msgText;
               erWhen = *on;
               isNotValid = *on;
            Endif;
         Endif;
         If scSqlAct <> *blanks;
            If GetUserEmail(q1user) = *blanks;
               mtSqlAct = 'You must have an email address established in ' +
                'user configuration to add/edit a rule with an SQL statement.';
               erSqlAct = *on;
               isNotValid = *on;
            Endif;
            If not ValidSqlSyntax(%trim(scSqlAct):'*SQL');
               dsMsgInfo = RtvMsg('*');
               mtSqlAct = dsMsgInfo.msgText;
               erSqlAct = *on;
               isNotValid = *on;
            Endif;
         Endif;
         If isNotValid;
            Leavesr;
         Endif;
         // At least one recipient must be added.
         Select;
         When intRecCnt = 0 and %scan('Send':scAddActE) > 0;
            scOpenDlg = 'Y';
            PopOneBtn('At least one recipient must be added to the list.'
             :'Missing Recipients');
            isNotValid = *on;
            scOpenDlg = 'Y';
         Endsl;

       Endsr;

       //===================================================================
       // Validate Auto Diary Trigger
       //===================================================================
       Begsr ValidateTrg;

       isNotValid = *off;

       If flgTrgMode = 'E';
          flgRcdUpdated = RcdUpdatedErr('SMTRP':scTrgRrn:'TRdt':
           'TRtime':'TRuser':saveDate:saveTime);
          Select;
          When flgRcdUpdated = '0'; // No error
          When flgRcdUpdated = '1'; // Error
             isNotValid = *on;
          When flgRcdUpdated = '2'; // Error with refresh
          Endsl;
       Endif;

       // Trigger must be selected.
       If scAddTrig  = ' ';
          msAddTrig  = 'WCR0503';
          erAddTrig  = *on;
          isNotValid = *on;
       Endif;

       If isFileTrig = *on;
          Exsr ValidateFileRule;
          Leavesr;
       Endif;

       // At least one action must be selected
       Select;
       When isRules = *on and scAddActI = *blank and scAddActE = *blank;
          mtAddAct = 'At least one action (internal and/or external) ' +
           'must be selected.';
          erAddAct = *on;
          isNotValid = *on;
       When isRules = *off and scAddActI = *blanks;
          scAddActI = 'Send Diary';
       Endsl;

       // Diary code must be selected.
       isDiaryAction = %scan('Send Diary':scAddActI) > 0;
       If scAddDryCd = ' ' and isDiaryAction;
          msAddDryCd = 'WCR0503';
          erAddDryCd = *on;
          isNotValid = *on;
       Endif;

       // # of occurences must be entered.
       If scAddNmOcc = ' ' and isDiaryAction;
          msAddNmOcc = 'WCR0503';
          erAddNmOcc = *on;
          isNotValid = *on;
       Endif;

       // At least one recipient must be added.
       Select;
       When intRecCnt = 0 and isRules = *off;
          PopOneBtn('At least one recipient must be added to the list.'
           :'Missing Recipients');
          isNotValid = *on;
       When scAddActI <> *blank and intRecCntI = *zero;
          PopOneBtn('At least one Internal recipient must be added to the list.'
           :'Missing Recipients');
          isNotValid = *on;
       When scAddActE <> *blank and intRecCntE = *zero;
          PopOneBtn('At least one External recipient must be added to the list.'
           :'Missing Recipients');
          isNotValid = *on;
       Endsl;

       // Claim status is require if Status description is selected
       If scAddClSts = *Blanks and scAddStDs <> *blanks;
          msAddClSts = 'Claim Status is required when Status Description '+
           'is selected.';
          erAddClsts = *on;
          isNotValid = *on;
       Endif;

       // # of days prior to effective date must be entered.
       If vsAddEffDt = *on and scAddEffDt = 0;
          msAddEffDt = 'WCR0503';
          erAddEffDt = *on;
          isNotValid = *on;
       Endif;

       // premium amount must be entered.
       If vsAddPAmt  = *on and scAddPAmt = 0;
          msAddPAmt  = 'WCR0503';
          erAddPAmt  = *on;
          isNotValid = *on;
       Endif;

       // $ value change must be entered.
       If vsAddVlChg = *on and scAddVlChg = 0;
          msAddVlChg = 'WCR0503';
          erAddVlChg = *on;
          isNotValid = *on;
       Endif;

       // % change must be entered.
       If vsAddPrChg = *on and scAddPrChg = 0;
          msAddPrChg = 'WCR0503';
          erAddPrChg = *on;
          isNotValid = *on;
       Endif;

       // $ value change, premium amount and % change are mutually exclusive.
       If (scAddPAmt <> 0 and (scAddPrChg <> 0 or scAddVlChg <> 0)) or
        (scAddPrChg <> 0 and scAddVlChg <> 0);
          msAddPrChg = 'WCR0590';
          msAddVlChg = 'WCR0590';
          msAddPAmt  = 'WCR0590';
          erAddPrChg = *on;
          erAddVlChg = *on;
          erAddPAmt  = *on;
          isNotValid = *on;
       Endif;

       erAddCmpDt = *Off;
       msAddCmpDt = *blanks;
       erAddCmpOp = *Off;
       msAddCmpOp = *blanks;
       Select;
       When scAddTrig = 'EXPMODDATE';
          If scAddCmpDt = *blanks;
             erAddCmpDt = *On;
             msAddCmpDt = 'An experience mod date is required for this -
              trigger selection.';
             isNotValid = *On;
          EndIf;
          If scAddCmpOp = *blanks;
             erAddCmpOp = *On;
             msAddCmpOp = 'A compare operator is required for this -
              trigger selection.';
             isNotValid = *On;
          Endif;
          If %Scan('*':scAddCmpDt)= 0;
             Monitor;
                intUsaDate = %dec(%char(%date(scAddCmpDt:*usa/):*usa0):8:0);
             On-Error;
                erAddCmpDt = *On;
                msAddCmpDt = 'Date is invalid and must be MM/DD/CCYY format. -
                 See tooltip for more information.';
                isNotValid = *On;
             EndMon;
          Endif;
          If %Scan('*':scAddCmpDt) <> 0 and scAddCmpOp <> 'equal to';
             erAddCmpOp = *On;
             msAddCmpOp = 'The use of wild card values requires the use -
              of the ''Equal To'' compare operator';
             isNotValid = *On;
          Endif;
          dsAddCmpDt = scAddCmpDt;
          If scAddCmpDt <> *blanks and (dsSlash1 <> '/' or
              dsSlash2 <> '/');
             erAddCmpDt = *On;
             msAddCmpDt = 'The date must be formatted as MM/DD/CCYY. -
              See tool tip for more information.';
             isNotValid = *On;
          Endif;
          If dsCompMth = '**' and dsCompDay = '**' and dsCompYear = '**'
           and dsCompCentury = '**';
             erAddCmpDt = *On;
             msAddCmpDt = 'The date cannot be all wild card values. -
              See tool tip.';
             isNotValid = *On;
          Endif;
          If %Scan('*':dsCompMth) <> 0 and dsCompMth <> '**';
             erAddCmpDt = *On;
             msAddCmpDt = 'The month must be numeric or ''**''. -
              See tool tip.';
             isNotValid = *On;
          Endif;
          If %Scan('*':dsCompDay) <> 0 and dsCompDay <> '**';
             erAddCmpDt = *On;
             msAddCmpDt = 'The day must be numeric or ''**''. -
              See tool tip.';
             isNotValid = *On;
          Endif;
          If %Scan('*':dsCompCentury) <> 0 and dsCompCentury <> '**';
             erAddCmpDt = *On;
             msAddCmpDt = 'The century must be numeric or ''**''. -
              See tool tip.';
             isNotValid = *On;
          Endif;
          If %Scan('*':dsCompYear) <> 0 and dsCompYear <> '**';
             erAddCmpDt = *On;
             msAddCmpDt = 'The year portion must be numeric or ''**''. -
              See tool tip.';
             isNotValid = *On;
          Endif;
          Select;
          When isNotValid = *On;
          When %Scan('*':scAddCmpDt) = 0;
          Other;
             If dsCompMth = '**';
                dsCompMth = '01';
             Endif;
             If dsCompDay = '**';
                dsCompDay = '01';
             Endif;
             If dsCompCentury = '**';
                dsCompCentury = '20';
             Endif;
             If dsCompYear = '**';
                dsCompYear = '01';
             Endif;
             Monitor;
                intUsaDate = %dec(%char(%date(dsAddCmpDt:*usa/):*usa0):8:0);
             On-Error;
                erAddCmpDt = *On;
                msAddCmpDt = 'Invalid numbers in the date. See tool tip.';
                isNotValid = *On;
             EndMon;
          Endsl;
       Endsl;

       If (vsClmInd = *on) and (scAddClInd = *blanks);
          msClmInd = 'WCR0503';
          erClmInd = *on;
          isNotValid = *on;
       Endif;

       If (vsNCCICd = *on) and (scAddNCCI = *blanks);
          msNCCICd = 'WCR0503';
          erNCCICd = *on;
          isNotValid = *on;
       Endif;

       If (vsNtCd = *on) and (scAddNtrCd = *blanks and
           scAddPrtCd = *blanks and scAddCauCd = *blanks);
          msClmNPC = 'Atlease one Nature/Part/Cause code required';
          erClmNPC = *on;
          isNotValid = *on;
       Endif;

       erRetroFin = *Off;
       If scRetro = 'Y' and scAddClSts <> 'Open' and isRetroI = *on;
          msRetroFin = 'Claim status must be set to Open';
          erRetroFin = *on;
          isNotValid = *on;
       Endif;

       erSpltWg  = *off;
       erSpltWg0 = *off;
       // Fields are mutually exclusive
       If visSpltWg = *on and scSpltWg = 'Y' and scSpltWg0 = 'Y';
          isNotValid = *on;
          msSpltWg  = 'WCR0590';
          mfSpltWg  = 'WMESGG';
          erSpltWg  = *on;
          msSpltWg0 = 'WCR0590';
          mfSpltWg0 = 'WMESGG';
          erSpltWg0 = *on;
       Endif;

       Endsr;

       //===================================================================
       // Delete recipient
       //===================================================================
       Begsr DelRecpt;

       If flgTrgMode = 'E';
          Delete scRecptRrn SMTRPREC;
       Else;
          Delete scRecptRrn SMTRPRC1;
       Endif;

       Endsr;

       //===================================================================
       // Load Policy info in Add/Edit grid
       //===================================================================
       Begsr LoadPolicy;

        // Load policy info
       Chain(n) (mTRco#:mTRname:mTRseq#) ptr0pol;
       If %found(SMTRPPOL);
          scAddStTyp = PTRpolStsTp;
          scAddPlSts = PTRpolSts;
          scAddQtTyp = PTRqteType;
          scAddPrmLv = PTRpremLvl;
          scAddEffDt = PTR#daysPr;
          scAddPAmt  = PTRpremAmt;
          scAddVlChg = PTRchgVal;
          scAddPrChg = PTRchg;
          scAddCmpDt = PTRCompDate;
          scAddCmpOp = PTRCompOper;
       Endif;

       // Load the Quote status
       Setll (mTRco#:mTRname:mTRseq#:'Quote Status') ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:'Quote Status') ptr0tbl;

       Dow not %eof(SMTRPTBL);

          If scAddQtSts = ' ';
             scAddQtSts = %trim(TTRkey);
          Else;
             scAddQtSts = %trim(scAddQtSts) + ',' + %trim(TTRkey);
          Endif;
          Reade(n) (mTRco#:mTRname:mTRseq#:'Quote Status') ptr0tbl;
       Enddo;

       Endsr;

       //===================================================================
       // Load Claims info in Add/Edit grid
       //===================================================================
       Begsr LoadClaims;

        // Load claims info
       Chain(n) (mTRco#:mTRname:mTRseq#) ptr0clm;
       If %found(SMTRPCLM);
          scAddClSts = CTRclmSts;
          scAddClTyp = CTRclmType;
          scAddAmt   = CTRamt;
          scAddPmtCn = CTRpayCount;
          scAddSpcfc = CTRreten;
          scAddTtPd  = CTRtotPaid;
          scAddTtcur = CTRtotInc;
          scAddCWkPd = CTRweeksPd;
          scRetro  = CTRrset;
          scSpltWg  = CTRspltWg;
          scSpltWg0 = CTRspltWg0;
       Endif;

       // Load the Jurisdiction State
       scAddJrsSt = *blanks;
       strSelection = *blanks;
       strTable = 'States';
       Setll (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          strSelection = %trim(strSelection) + ',' + %trim(TTRkey);
          Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Enddo;
       scAddJrsSt= %subst(strSelection:2);

       // Load the Claim Indicator
       scAddClInd = *blanks;
       strSelection = *blanks;
       strTable = 'Claim Indicator';
       Setll (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          strSelection = %trim(strSelection) + ',' + %trim(TTRkey);
          Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Enddo;
       scAddClInd = %subst(strSelection:2);

       // Load the NCCI Injury Code
       scAddNCCI = *blanks;
       strSelection = *blanks;
       strTable = 'NCCI Injury';
       Setll (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          strSelection = %trim(strSelection) + ',' + %trim(TTRkey);
          Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Enddo;
       scAddNCCI = %subst(strSelection:2);

       // Load the Nature Code
       scAddNtrCd = *blanks;
       strSelection = *blanks;
       strTable = 'Nature';
       Setll (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          strSelection = %trim(strSelection) + ',' + %trim(TTRkey);
          Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Enddo;
       scAddNtrCd = %subst(strSelection:2);

       // Load the Part Code
       scAddPrtCd = *blanks;
       strSelection = *blanks;
       strTable = 'Part';
       Setll (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          strSelection = %trim(strSelection) + ',' + %trim(TTRkey);
          Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Enddo;
       scAddPrtCd = %subst(strSelection:2);

       // Load the Cause Code
       scAddCauCd = *blanks;
       strSelection = *blanks;
       strTable = 'Cause';
       Setll (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          strSelection = %trim(strSelection) + ',' + %trim(TTRkey);
          Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Enddo;
       scAddCauCd = %subst(strSelection:2);

       // Load the Payment Code
       scAddPmtCd = *blanks;
       strSelection = *blanks;
       strTable = 'Payment';
       Setll (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          strSelection = %trim(strSelection) + ',' + %trim(TTRkey);
          Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Enddo;
       scAddPmtCd = %subst(strSelection:2);

       // Load the Reserve Code
       scAddRsvCd = *blanks;
       strSelection = *blanks;
       strTable = 'Reserve';
       Setll (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          strSelection = %trim(strSelection) + ',' + %trim(TTRkey);
          Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Enddo;
       scAddRsvCd = %subst(strSelection:2);

       // Load Status Description
       scAddStDs = *blanks;
       strSelection = *blanks;
       strTable = 'Status Description';
       Setll (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          strSelection = %trim(strSelection) + ',' + %trim(TTRkey);
          Reade(n) (mTRco#:mTRname:mTRseq#:strTable) ptr0tbl;
       Enddo;
       scAddStDs = %subst(strSelection:2);

       Endsr;

       //===================================================================
       // Set defaults
       //===================================================================
       Begsr setDefaults;

       vsNumOcc   = *off;
       vsAddStTyp = *off;
       vsAddPlSts = *off;
       vsAddQtTyp = *off;
       vsAddEffDt = *off;
       vsAddPrmLv = *off;
       vsAddPAmt  = *off;
       vsAddVlChg = *off;
       vsAddPrChg = *off;
       dsbAddQtSt = *off;
       vsClmInd   = *off;
       vsNCCICd   = *off;
       vsNtCd     = *off;
       vsPartCd   = *off;
       vsCauseCd  = *off;
       vsStatDesc = *off;

       If scAddTrig = 'NQTESTS' or scAddTrig = 'RQTESTS' or
        scAddTrig = 'QTEENTRY' or scAddTrig = 'QTERVW' or
         scAddTrig = 'POLPEND' or scAddTrig = 'LOWRSV';
          vsNumOcc = *on;
          scAddNmOcc = 'OneOpen';
       Endif;

       If scAddTrig = 'WEEKSPD' or scAddTrig = 'MULTIEMP' or scAddTrig =
        'POLCONC' or scAddTrig = 'POLPANEL'
        or scAddTrig = 'LARGERSV' or scAddTrig = 'TOTALINC';
          vsNumOcc = *on;
          scAddNmOcc = 'One';
       Endif;

       If scAddTrig = 'AUDPREM$' or scAddTrig = 'AUDPREM' or scAddTrig =
        'CALCPREM' or scAddTrig = 'POLPEND' or scAddTrig = 'POLCONC' or
        scAddTrig = 'POLPANEL';
          vsAddStTyp = *on;
          scAddStTyp = 'W';
       Endif;

       If scAddTrig = 'QTEENTRY' or scAddTrig = 'QTERVW' or scAddTrig =
        'NQTESTS' or scAddTrig = 'RQTESTS' or scAddTrig = 'QTE3PRTPMT';
          vsAddStTyp = *on;
          scAddStTyp = 'Q';
       Endif;

       If scAddTrig = 'POLPEND';
          vsAddPlSts = *on;
          scAddPlSts = 'Active';
       Endif;

       If scAddTrig = 'QTEENTRY' or scAddTrig = 'NQTESTS';
          vsAddQtTyp = *on;
          scAddQtTyp = 'New';
       Endif;

       If scAddTrig = 'RQTESTS' or scAddTrig = 'QTERVW';
          vsAddQtTyp = *on;
          scAddQtTyp = 'Renewal';
       Endif;

       If scAddTrig = 'NQTESTS' and FlgTrgMode = 'A';
          scAddQtSts = 'L,D,S,A';
       Endif;

       If scAddTrig = 'RQTESTS' and FlgTrgMode = 'A';
          scAddQtSts = 'l,d,s';
       Endif;

       If scAddTrig = 'POLPEND' or scAddTrig = 'RQTESTS' or scAddTrig =
        'NQTESTS';
          vsAddEffDt = *on;
       Endif;

       If scAddTrig = 'AUDPREM$' or scAddTrig = 'AUDPREM' or scAddTrig =
        'CALCPREM' or scAddTrig = 'POLCONC';
          vsAddPrmLv = *on;
       Endif;

       If scAddTrig = 'POLCONC';
          vsAddPAmt = *on;
       Endif;

       If scAddTrig = 'AUDPREM$';
          vsAddVlChg = *on;
       Endif;

       If scAddTrig = 'AUDPREM' or scAddTrig = 'CALCPREM';
          vsAddPrChg = *on;
       Endif;

       If scAddTrig = 'CLMCHG03';
          vsClmInd = *on;
       Endif;

       If scAddTrig = 'CLMNCCI';
          vsNCCICd = *on;
       Endif;

       If scAddTrig = 'CLMNPC';
          vsNtCd = *on;
          vsPartCd = *on;
          vsCauseCd = *on;
       Endif;

       xxTBID = 'OCST';
       Chain (mTRco#:xxTbId) pgtw0;
       If %Found();
          vsStatDesc = *on;
          scStDsTop = 390;
          scpolRlTop = 510;
       Else;
          scpolRlTop = 390;
       Endif;

       //***  Begin Add  ***  06/15/22  *************
       Select;
       When isClmTrig = *on;
       When isPolTrig = *on;
          vsStatDesc = *off;
       When isFileTrig = *on;
          vsStatDesc = *off;
       Endsl;
       //***  End   Add  ***  06/15/22  *************

       Endsr;
       //===================================================================
       // Set Quote Status on Trigger/Status Type/Quote Type change
       //===================================================================
       Begsr setQteSts;

       clear chAddQtSts;
       chAddQtSts = 'QSco# = ' + %editc(intCo#:'X') + ' And QSstat = ''A''';

       Select;
       // Quote
       When scAddStTyp = 'Q';
          chAddQtSts = %trim(chAddQtSts) + ' And QSqte = ''' +
           %trim(scAddStTyp) + '''';
          Select;
          When %trim(scAddQtTyp) = 'New';
             chAddQtSts = %trim(chAddQtSts) + ' And QSrenew =  '' ''';
          When %trim(scAddQtTyp) = 'Renewal';
             chAddQtSts = %trim(chAddQtSts) + ' And QSrenew = ''Y''';
          Endsl;

       // Live
       When scAddStTyp = 'W';
          chAddQtSts= %trim(chAddQtSts) + ' And QSqte = ''W''' +' And (QSkey = +
           ''W''' + ' or QSkey = ''C''' + ' or QSkey = ''R''' + ')';

       // Both
       Other;
          chAddQtSts=%trim(chAddQtSts)+ ' And ((QSqte = ''W''' +' And (QSkey = +
           ''W''' + ' or QSkey = ''C''' + ' or QSkey = ''R''' + '))';
          Select;
          When %trim(scAddQtTyp) = 'New';
             chAddQtSts = %trim(chAddQtSts) + ' or (QSqte = ''Q''' +
              ' and QSrenew =  '' ''' + '))';
          When %trim(scAddQtTyp) = 'Renewal';
             chAddQtSts = %trim(chAddQtSts) + ' or (QSqte = ''Q''' +
              ' and QSrenew =  ''Y''' + '))';
          Other;
             chAddQtSts = %trim(chAddQtSts) + ' or QSqte = ''Q''' + ')';
          Endsl;
       Endsl;

       Endsr;

       //===================================================================
       // Save the Trigger textual detail - claim
       //===================================================================
       Begsr SaveRuleTextClm;

        // Claim Status
        strCond = BuildRulesText(strCond:'has a status of':'and':cTRclmSts);

        // Claim Type
        strCond = BuildRulesText(strCond:'has claim type':'and'
         :BuildClmTypText(cTRclmType));

        // Jurisdiction states (STSTL)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('States':'has jursidiction state'));

        // Claim Indicators (STGTL - AXCI)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('Claim Indicator':'has claim indicator'));

        // NCCI Indjury Code (WTGTL - xxNC)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('NCCI Injury':'has injury code'));

        // Nature Code (WTABL - pna00)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('Nature':'has nature code'));

        // Part Code (WTABL - ppa00)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('Part':'has part code'));

        // Cause Code (WTABL - pca00)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('Cause':'has cause code'));

        // PV1 (WTABL - pb200)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('Payment':'has payment value 1 code'));

        // Reserve (WTABL - prt00)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('Reserve':'has reserve code'));

        // Retrospective Settlement
        If cTRrSet = 'Y';
           strCond = BuildRulesText(strCond
            :'has a finalized retrospective settlement':'and':cTRrSet);
        Endif;

        // Amount
        If cTRamt <> *zero;
           strCond = BuildRulesText(strCond
            :'has a reserve change greater than or equal to $':'and'
            :%editc(cTRamt:'J'));
        Endif;

        // Payment Count
        If cTRpayCount <> *zero;
           strCond = BuildRulesText(strCond
            :'has a payment count greater than or eqaul to':'and'
            :%editc(cTRpayCount:'X'));
        Endif;

        // % of Specific Retention
        If cTRreten <> *zero;
           strCond = BuildRulesText(strCond
            :'has a specific retention greater than or eqaul to':'and'
            :%editc(cTRreten:'J')+'%');
        Endif;

        // Total Paid
        If cTRtotPaid <> *zero;
           strCond = BuildRulesText(strCond
            :'has total paid greater than or equal to $':'and'
            :%editc(cTRtotPaid:'J'));
        Endif;

        // Total Incurred
        If cTRtotInc <> *zero;
           strCond = BuildRulesText(strCond
            :'has total incurred greater than or equal to $':'and'
            :%editc(cTRtotInc:'J'));
        Endif;

        // Compensation Weeks Paid
        If cTRweeksPd <> *zero;
           strCond = BuildRulesText(strCond
            :'has total weeks paid greater than or equal to':'and'
            :%editc(cTRweeksPd:'X'));
        Endif;

       Endsr;
       //===================================================================
       // Save the Trigger textual detail - policy
       //===================================================================
       Begsr SaveRuleTextPol;

        // Policy Status Type
        strCond = BuildRulesText(strCond:'exists in':'and'
         :BuildPolStsText(pTRpolStsTp));

        // Policy Status
        strCond = BuildRulesText(strCond:'has a status of':'and':pTRpolSts);

        // Quote Type
        If pTRqteType <> *blank;
           strCond = BuildRulesText(strCond:'is a':'and'
            :%trim(pTRqteType)+' policy');
        Endif;

        // Quote Status (WTQSL)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('Quote Status':'has quote status'));

        // Premium Amount
        If pTRpremAmt <> *zero;
           strCond = BuildRulesText(strCond
            :'has a premium amount greater than or equal to $':'and'
            :%editc(pTRpremAmt:'J'));
           strCond = BuildRulesText(strCond:'(at the':*blank
            :%trim(pTRpremLvl)+' premium level)');
        Endif;

        // $ Value change
        If pTRchgVal <> *zero;
           strCond = BuildRulesText(strCond
            :'has a premium change greater than or equal to $':'and'
            :%editc(pTRchgVal:'J'));
           strCond = BuildRulesText(strCond:'(at the':*blank
            :%trim(pTRpremLvl)+' premium level)');
        Endif;

        // % change
        If pTRchg <> *zero;
           strCond = BuildRulesText(strCond
            :'has a premium change greater than or equal to':'and'
            :%editc(pTRchg:'J')+'%');
           strCond = BuildRulesText(strCond:'(at the':*blank
            :%trim(pTRpremLvl)+' premium level)');
        Endif;

       Endsr;
       //===================================================================
       // Save the Trigger record.
       //===================================================================
       Begsr ProcSave;

       If flgTrgMode = 'E';
          Chain scTrgRrn ptr0p;
          Select;
          When isClmTrig = *on;
             Chain (mTRco#:mTRname:mTRseq#) ptr0clm;
             if %found;
                strtable = 'found';
             endif;
          When isPolTrig = *on;
             Chain (mTRco#:mTRname:mTRseq#) ptr0pol;
          Endsl;
       Else;
          // get sequence #
          Setgt (intCo#:scAddTrig:*hival) ptr00;
          Readpe (intCo#:scAddTrig) ptr00;
          If not %eof(SMTRL);
             intSeq = mTRseq# + 1;
          Else;
             intSeq = 0;
          Endif;

          clear ptr0p;
          mTRco#  = intCo#;
          mTRname = scAddTrig;
          mTRseq# = intSeq;
          mTRstat = 'A';
          mTRiPgm = q1Pgm;
          mTRiUsr = q1User;
          mTRiTim = Timeto6(SystemTime());
          mTRiDt  = Dateto7(SystemDate());

          LTRco#  = mTRco#;
          LTRname = mTRname;
          LTRseq# = mTRseq#;
          LTRiPgm = mTRiPgm;
          LTRiUsr = mTRiUsr;
          LTRiTim = mTRiTim;
          LTRiDt  = mTRiDt;
          LTRaction = 'Activate';

          Select;
          When isClmTrig = *on;
             mTRtype = 'Claims';
             cTRco#  = intCo#;
             cTRname = mTRname;
             cTRseq# = mTRseq#;
             cTRiPgm = mTRiPgm;
             cTRiUsr = mTRiUsr;
             cTRiTim = mTRiTim;
             cTRiDt  = mTRiDt;
          When isPolTrig = *on;
             mTRtype = 'Policy';
             PTRco#  = intCo#;
             PTRiPgm = q1Pgm;
             PTRiUsr = q1User;
             PTRiTim = Timeto6(SystemTime());
             PTRiDt  = Dateto7(SystemDate());
             PTRseq# = mTRseq#;
             PTRname = scAddTrig;
          When isFileTrig = *on;
             mTRtype = 'File';
             mTRname = 'FILE RULES';
          Endsl;
       Endif;

       Select;
       When scChgEvent <> svChgEvent;
          isPendChg = *on;
       When scFieldChg <> svFieldChg;
          isPendChg = *on;
       When scFieldRpt <> svFieldRpt;
          isPendChg = *on;
       When scWhen <> svWhen;
          isPendChg = *on;
       When mTRstat = 'Q';
          isPendChg = *on;
       Other;
          isPendChg = *off;
       Endsl;

       mTRdesc   = scAddDesc;
       mTRstat = scTrigSts;
       Select;
       When mTRname <> 'FILE RULES';
       When scTrigSts = 'A' and flgTrgMode = 'A';
          mTRstat = 'P';
       When scTrigSts = 'A' and isPendChg;
          mTRstat = 'Q';
       When scTrigSts = 'A' and not TriggerExists(mTRco#:mTRname:mTRseq#);
          mTRstat = 'P';
       When scTrigSts = 'I' and TriggerExists(mTRco#:mTRname:mTRseq#);
          mTRstat = 'i';
       Endsl;
       mTRdart   = scAddDryCd;
       mTR#occur = scAddNmOcc;
       mTRsecLvl = scAddScLvl;
       mTRdiaryDt  = scAddDryDt;
       mTRaddlDays = scAddDays;
       mTRpgm    = q1Pgm;
       mTRuser   = q1User;
       mTRtime   = Timeto6(SystemTime());
       mTRdt     = Dateto7(SystemDate());

       // Save policy info
       Select;
       When isPolTrig = *on;
          PTRpolStsTp = scAddStTyp;
          PTRpolSts   = scAddPlSts;
          PTRqteType  = scAddQtTyp;
          PTRpremLvl  = scAddPrmLv;
          PTR#daysPr  = scAddEffDt;
          PTRpremAmt  = scAddPAmt;
          PTRchgVal   = scAddVlChg;
          PTRchg      = scAddPrChg;
          PTRCompDate = scAddCmpDt;
          PTRCompOper = scAddCmpOp;
          PTRpgm      = q1Pgm;
          PTRuser     = q1User;
          PTRtime     = Timeto6(SystemTime());
          PTRdt       = Dateto7(SystemDate());
       When isClmTrig = *on;
          cTRclmSts   =  scAddClSts;
          cTRclmType  =  scAddClTyp;
          cTRamt      =  scAddAmt  ;
          cTRpayCount =  scAddPmtCn;
          cTRreten    =  scAddSpcfc;
          cTRtotPaid  =  scAddTtPd ;
          cTRtotInc   =  scAddTtcur;
          cTRweeksPd  =  scAddCWkPd;
          cTRpgm      =  mTRpgm;
          cTRuser     =  mTRuser;
          cTRtime     =  mTRtime;
          cTRdt       =  mTRdt;
          cTRrset     =  scRetro ;
          cTRspltWg   =  scspltWg;
          cTRspltWg0  =  scspltWg0;
       Endsl;


       If flgTrgMode = 'E';
          Update ptr0p;

          Select;
          When isPolTrig = *on;
             // update policy trigger info
             Update ptr0pol;
          When isClmTrig = *on;
             // update claim trigger info
             Update ptr0clm;
          When isFileTrig = *on;
             // process generic file trigger info
             Exsr ProcSaveFileTrig;
          Endsl;
       Else;
          Write ptr0p;

          // write log history info
          Write ptr0log;

          Select;
          When isPolTrig = *on;
             // write policy trigger info
             Write ptr0pol;
          When isClmTrig = *on;
             // write claim trigger info
             Write ptr0clm;
          When isFileTrig = *on;
             // process generic file trigger info
             Exsr ProcSaveFileTrig;
          Endsl;
       Endif;
       Unlock SMTRP;

       // clear the old group selection
       Setll (mTRco#:mTRname:mTRseq#) ptr0grp;
       Reade (mTRco#:mTRname:mTRseq#) ptr0grp;
       Dow not %eof(SMTRPGRP);
          delete ptr0grp;
          Reade (mTRco#:mTRname:mTRseq#) ptr0grp;
       Enddo;

       // Add new group selection
       GTRco#  = intCo#;
       GTRseq# = mTRseq#;
       GTRname = scAddTrig;
       GTRpgm  = q1Pgm;
       GTRuser = q1User;
       GTRtime = Timeto6(SystemTime());
       GTRdt   = Dateto7(SystemDate());
       GTRiPgm = q1Pgm;
       GTRiUsr = q1User;
       GTRiTim = GTRtime;
       GTRiDt  = GTRdt;

       strSelection = scAddGrp;

       Dow strSelection <> ' ';
          GTRfnd = %int(%subst(strSelection:1:3));
          strSelection = %subst(strSelection:5);
          write ptr0grp;
       Enddo;

       // Clear the old action(s)
       Dou not %found;
          Delete (mTRco#:mTRname:mTRseq#) ptr0act;
       Enddo;

       // Add action selection(s)
       ATRco#  = intCo#;
       ATRseq# = mTRseq#;
       ATRname = scAddTrig;
       ATRpgm  = q1Pgm;
       ATRuser = q1User;
       ATRtime = Timeto6(SystemTime());
       ATRdt   = Dateto7(SystemDate());
       ATRiPgm = q1Pgm;
       ATRiUsr = q1User;
       ATRiTim = GTRtime;
       ATRiDt  = GTRdt;

       // Internal Actions
       strSelection = scAddActI;
       Dow strSelection <> ' ';
          ATRaction = ParseString(strSelection:',':1:'1');
          ATRexternal = 'I';
          write ptr0act;
       Enddo;

       // External Actions
       strSelection = scAddActE;
       Dow strSelection <> ' ';
          ATRaction = ParseString(strSelection:',':1:'1');
          ATRexternal = 'E';
          write ptr0act;
       Enddo;

       // set values for smTrpTbl common fields.
       tTRco#  = mTRco# ;
       tTRname = mTRname;
       tTRseq# = mTRseq#;
       tTRpgm  = mTRpgm;
       tTRuser = mTRuser;
       tTRtime = mTRtime;
       tTRdt   = mTRdt;
       tTRiPgm = mTRiPgm;
       tTRiUsr = mTRiUsr;
       tTRiTim = mTRiTim;
       tTRiDt  = mTRiDt;

       // clear the old quote status selection
       tTRtable = 'Quote Status';
       Setll (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          delete ptr0tbl;
          Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Enddo;
       // Add new quote status selection
       strSelection = scAddQtSts;
       Dow strSelection <> ' ';
          tTRkey = %subst(strSelection:1:1);
          strSelection = %subst(strSelection:3);
          write ptr0tbl;
       Enddo;

       // clear the old Jurisdiction States
       tTRtable = 'States';
       Setll (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          delete ptr0tbl;
          Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Enddo;
       // Add new Jurisdiction States
       Clear #ofTrgJSt;
       strSelection = scAddJrsSt;
       Dow strSelection <> ' ';
          tTRkey = %subst(strSelection:1:2);
          strSelection = %subst(strSelection:4);
          write ptr0tbl;
          #ofTrgJSt += 1;
          strState = tTRkey;
       Enddo;

       // clear the old Claim Indicators
       tTRtable = 'Claim Indicator';
       Setll (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          delete ptr0tbl;
          Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Enddo;
       // Add new Claim Indicators
       strSelection = scAddClInd;
       Dow strSelection <> ' ';
          tTRkey = %subst(strSelection:1:2);
          strSelection = %subst(strSelection:4);
          write ptr0tbl;
       Enddo;

       // clear the old NCCI Injury Codes
       tTRtable = 'NCCI Injury';
       Setll (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          delete ptr0tbl;
          Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Enddo;
       // Add new NCCI Injury Codes
       strSelection = scAddNCCI;
       Dow strSelection <> ' ';
          tTRkey = %subst(strSelection:1:2);
          strSelection = %subst(strSelection:4);
          write ptr0tbl;
       Enddo;

       // clear the old Nature Codes
       tTRtable = 'Nature';
       Setll (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          delete ptr0tbl;
          Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Enddo;
       // Add new Nature Codes
       strSelection = scAddNtrCd;
       Dow strSelection <> ' ';
          tTRkey = %subst(strSelection:1:2);
          strSelection = %subst(strSelection:4);
          write ptr0tbl;
       Enddo;

       // clear the old Part Codes
       tTRtable = 'Part';
       Setll (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          delete ptr0tbl;
          Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Enddo;
       // Add new Part Codes
       strSelection = scAddPrtCd;
       Dow strSelection <> ' ';
          tTRkey = %subst(strSelection:1:2);
          strSelection = %subst(strSelection:4);
          write ptr0tbl;
       Enddo;

       // clear the old Cause Codes
       tTRtable = 'Cause';
       Setll (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          delete ptr0tbl;
          Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Enddo;
       // Add new Cause Codes
       strSelection = scAddCauCd;
       Dow strSelection <> ' ';
          tTRkey = %subst(strSelection:1:2);
          strSelection = %subst(strSelection:4);
          write ptr0tbl;
       Enddo;

       // clear the old Payment Codes
       tTRtable = 'Payment';
       Setll (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          delete ptr0tbl;
          Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Enddo;
       // Add new Payment Codes
       strSelection = scAddPmtCd;
       Dow strSelection <> ' ';
          tTRkey = %subst(strSelection:1:2);
          strSelection = %subst(strSelection:4);
          write ptr0tbl;
       Enddo;

       // clear the old Reserve Codes
       tTRtable = 'Reserve';
       Setll (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          delete ptr0tbl;
          Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Enddo;
       // Add new Reserve Codes
       strSelection = scAddRsvCd;
       Dow strSelection <> ' ';
          tTRkey = %subst(strSelection:1:2);
          strSelection = %subst(strSelection:4);
          write ptr0tbl;
       Enddo;

       // clear the old Status Descriptions
       tTRtable = 'Status Description';
       Setll (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Dow not %eof(SMTRPTBL);
          delete ptr0tbl;
          Reade (mTRco#:mTRname:mTRseq#:tTRtable) ptr0tbl;
       Enddo;
       // Add new Table Descriptions
       strSelection = scAddStDs;
       Dow strSelection <> ' ';
          tTRkey = %subst(strSelection:1:1);
          strSelection = %subst(strSelection:3);
          write ptr0tbl;
       Enddo;

       // Add new recipient
       If flgTrgMode = 'A';
          Exsr saveRecpt;
       Endif;

       SaveRuleText();

       If BtnCondSav = *on;
          Exsr LoadCompGrid;
          Exfmt rcdFileCmp;
       Endif;

       If scTrigSts = 'A' and mTRstat = 'P';
          PendTrigNotice();
       Endif;
       If scTrigSts = 'A' and mTRstat = 'Q';
          PendTrigNotice();
       Endif;
       If scTrigSts = 'I' and mTRstat = 'i';
          PendTrigNotice('I');
       Endif;

       Endsr;

       //===================================================================
       // Save generic file triggers
       //===================================================================
       Begsr ProcSaveFileTrig;
        If scFile = *blanks;
           scFile = strFile;
        Endif;
        If flgTrgMode = 'A';
           fTRco#  = intCo#;
           fTRname = mTRname;
           fTRseq# = mTRseq#;
           fTRlibrary = Upper(scLib);
           fTRfile = Upper(scFile);
           fTRpgm  = mTRiPgm;
           fTRuser = mTRiUsr;
           fTRtime = mTRiTim;
           fTRdt   = mTRiDt;
           fTRiPgm = mTRiPgm;
           fTRiUsr = mTRiUsr;
           fTRiTim = mTRiTim;
           fTRiDt  = mTRiDt;
           Write ptr0fil;
        Endif;
        // Clear the old fields
        // Add new fields
        // Clear the old trigger requirements
        // Inactivate the old fields
        Setll (mTRco#:mTRname:mTRseq#) ptr0fld;
        Dou %eof(SMTRPFLD);
           Reade (mTRco#:mTRname:mTRseq#) ptr0fld;
           Select;
           When %eof;
              Iter;
           Endsl;
           dTRstat = 'I';
           dTRpgm  = mTRpgm;
           dTRuser = mTRuser;
           dTRtime = mTRtime;
           dTRdt   = mTRdt;
           Update ptr0fld;
        Enddo;
        // Update current fields
        Dou strField = *blanks;
           strField = ParseString(scFieldChg:',':1:*on);
           Select;
           When strField = *blanks;
              Iter;
           Endsl;
           Setll (mTRco#:mTRname:mTRseq#) ptr0fld;
           Dou %eof(SMTRPFLD);
              Reade (mTRco#:mTRname:mTRseq#) ptr0fld;
              Select;
              When %eof;
                 Iter;
              When dTRtype <> 'Change';
                 Iter;
              When dTRField = strField;
                 Leave;
              Endsl;
           Enddo;
           dTRstat = 'A';
           dTRpgm  = mTRpgm;
           dTRuser = mTRuser;
           dTRtime = mTRtime;
           dTRdt   = mTRdt;
           Select;
           When %eof(SMTRPFLD);
              dTRco#  = intCo#;
              dTRname = mTRname;
              dTRseq# = mTRseq#;
              dTRfield = strField;
              dTRtype = 'Change';
              dTRiPgm = dTRpgm;
              dTRiUsr = dTRuser;
              dTRiTim = dTRtime;
              dTRiDt  = dTRdt;
              Write ptr0fld;
           Other;
              Update ptr0fld;
           Endsl;
        Enddo;
        Dou strField = *blanks;
           strField = ParseString(scFieldRpt:',':1:*on);
           Select;
           When strField = *blanks;
              Iter;
           Endsl;
           Setll (mTRco#:mTRname:mTRseq#) ptr0fld;
           Dou %eof(SMTRPFLD);
              Reade (mTRco#:mTRname:mTRseq#) ptr0fld;
              Select;
              When %eof;
                 Iter;
              When dTRtype <> 'Report';
                 Iter;
              When dTRField = strField;
                 Leave;
              Endsl;
           Enddo;
           dTRstat = 'A';
           dTRpgm  = mTRpgm;
           dTRuser = mTRuser;
           dTRtime = mTRtime;
           dTRdt   = mTRdt;
           Select;
           When %eof(SMTRPFLD);
              dTRco#  = intCo#;
              dTRname = mTRname;
              dTRseq# = mTRseq#;
              dTRfield = strField;
              dTRtype = 'Report';
              dTRiPgm = dTRpgm;
              dTRiUsr = dTRuser;
              dTRiTim = dTRtime;
              dTRiDt  = dTRdt;
              Write ptr0fld;
           Other;
              Update ptr0fld;
           Endsl;
        Enddo;
        // Inactivate the old trigger requirements
        Setll (mTRco#:mTRname:mTRseq#) ptr0frq;
        Dou %eof(SMTRPFRQ);
           Reade (mTRco#:mTRname:mTRseq#) ptr0frq;
           Select;
           When %eof;
              Iter;
           Endsl;
           qTRstat = 'I';
           qTRpgm  = mTRpgm;
           qTRuser = mTRuser;
           qTRtime = mTRtime;
           qTRdt   = mTRdt;
           Update ptr0frq;
        Enddo;
        Dou strTrgEvent = *blanks;
           strTrgEvent = ParseString(scChgEvent:',':1:*on);
           Select;
           When strTrgEvent = *blanks;
              Iter;
           Endsl;
           Setll (mTRco#:mTRname:mTRseq#) ptr0frq;
           Dou %eof(SMTRPFRQ);
              Reade (mTRco#:mTRname:mTRseq#) ptr0frq;
              Select;
              When %eof;
                 Iter;
              When qTRtrgEvent = strTrgEvent;
                 Leave;
              Endsl;
           Enddo;
           qTRstat = 'A';
           qTRtrgTime = '*AFTER';
           qTRtrgPgm = 'STTRIGGER';
           qTRexitPgm = scExitPgm;
           qTRpgm  = mTRpgm;
           qTRuser = mTRuser;
           qTRtime = mTRtime;
           qTRdt   = mTRdt;
           Select;
           When %eof(SMTRPFRQ);
              qTRco#  = intCo#;
              qTRname = mTRname;
              qTRseq# = mTRseq#;
              qTRlibrary = Upper(scLib);
              qTRfile = Upper(scFile);
              //*** Beg Add ***** 06/19/24 ********************************
              qTRtrgEvent = strTrgEvent;
              //*** End Add ***** 06/19/24 ********************************
              qTRtrgName = GetTriggerName(qTRlibrary:qTRfile:qTRtrgEvent);
              //*** Beg Del ***** 06/19/24 ********************************
              //***qTRtrgEvent = strTrgEvent;
              //*** End Del ***** 06/19/24 ********************************
              qTRiPgm = qTRpgm;
              qTRiUsr = qTRuser;
              qTRiTim = qTRtime;
              qTRiDt  = qTRdt;
              Write ptr0frq;
           Other;
              Update ptr0frq;
           Endsl;
        Enddo;
        Clear wTRwhen;
        Chain (mTRco#:mTRname:mTRseq#) ptr0whn;
        wTRco#  = intCo#;
        wTRname = mTRname;
        wTRseq# = mTRseq#;
        wTRwhen = scWhen;
        wTRpgm  = mTRpgm;
        wTRuser = mTRuser;
        wTRtime = mTRtime;
        wTRdt   = mTRdt;
        Select;
        When not %found;
           wTRiPgm = wTRpgm;
           wTRiUsr = wTRUser;
           wTRiTim = wTRtime;
           wTRiDt  = wTRdt;
           Write ptr0whn;
        Other;
           Update ptr0whn;
        Endsl;
        Clear sqlTRsqlAct;
        Chain (mTRco#:mTRname:mTRseq#) ptr0sql;
        sqlTRco#  = intCo#;
        sqlTRname = mTRname;
        sqlTRseq# = mTRseq#;
        sqlTRsqlAct = scSqlAct;
        sqlTRpgm  = mTRpgm;
        sqlTRuser = mTRuser;
        sqlTRtime = mTRtime;
        sqlTRdt   = mTRdt;
        Select;
        When not %found;
           sqlTRiPgm = sqlTRpgm;
           sqlTRiUsr = sqlTRUser;
           sqlTRiTim = sqlTRtime;
           sqlTRiDt  = sqlTRdt;
           Write ptr0sql;
        Other;
           Update ptr0sql;
        Endsl;
        BuildFileTrg(qTRfile:qTRlibrary:'*SUBMIT');
       Endsr;

       //===================================================================
       // Save recipient
       //===================================================================
       Begsr saveRecpt;

       Setll (intTmpCo#:strTRName:intTmpSeq) SMTRLRC1;
       Reade(n) (intTmpCo#:strTRName:intTmpSeq) SMTRLRC1;
       Dow not %eof(SMTRLRC1);
          TRco#  = mTRco#;
          TRName = mTRName;
          TRseq# = mTRseq#;
          write ptr0rec;

          Reade(n) (intTmpCo#:strTRName:intTmpSeq) SMTRLRC1;
       Enddo;

       Endsr;

      /end-free

     p ProcAddEdt      e

       //===================================================================
       // Add/Edit Branding
       //===================================================================
     p ProcBrandCfg    b
     d ProcBrandCfg    pi
     d  inMode                        1    const
     d  inRRN                         9  0 const options(*nopass)

     d decCount        s             10i 0 inz
     d decRRN          s              9  0 inz
     d intSeq          s             15  0 inz(0)
     d isError         s              1n   inz(*off)
     d strSelection    s            525
     d errSelection    s            525
     d errCkFnd        s                   like(grpLGfnd)

      /Free
       If %parms >= 2;
          decRRN = inRRN;
       Endif;
       Exsr ClearErr;
       Select;
       When inMode = 'A';
          Clear plg0p;
          LGco#  = intCo#;
          LGstat = 'A';
          Exsr LoadScreen;
       When inMode = 'E';
          Chain(n) decRRN STLGP;
          Exsr LoadScreen;
       When inMode = 'D';
          strMsg = 'Press Confirm to remove the selected record.';
          If not PopTwoBtn(strmsg:'Delete Record - ' +
             %trim(scProcess):'Cancel':'Confirm');
          Chain decRRN STLGP;
             If %found;

             // Delete associate group selection records
                Setll (LGco#:LGseq#) plg0grp;
                Reade (LGco#:LGseq#) plg0grp;
                Dow not %eof(STLGLGRP);
                   delete plg0grp;
                   Reade (LGco#:LGseq#) plg0grp;
                Enddo;

                Delete plg0p;
                LoadDocuments();
             Endif;
          Endif;
       Endsl;
       If inMode <> 'D' and inMode <> 'F' and inMode <> 'C';
       Dou not isError or LogoCancel = *on;
          LogoSave = *off;
          LogoCancel = *off;
          Exfmt rcdBrnInfo;
          Select;
          When LogoCancel = *on;
          When LogoSave = *on;
             Exsr SaveDB;
          Endsl;
       Enddo;
       Endif;

       Return;

       //=====================================================================
       Begsr ClearErr;
       clear msBranLogo;
       erBranLogo = *off;
       isError = *off;
       msBranProc = *off;
       erBranProc = *off;
       Endsr;

       //=====================================================================
       Begsr ErrorChk;
        Exsr ClearErr;
        If scLgPath = *blank;
           msBranLogo = 'Logo Address cannot be blank.';
           erBranLogo = *on;
           isError = *on;
        Endif;

       // Edit for duplicate groups

       errSelection = scLogoGrp;

       Dow errSelection <> ' ';
          errCkFnd = %int(%subst(errSelection:1:3));
          errSelection = %subst(errSelection:5);
       // Chain(n) (intCo#:errCkFnd:scDropProc) plg1grp;
       // If %found;
          Reset decCount;
          Exec sql
           select count(*) into :decCount
            from STLGPGRP
            where LGco# = :intCo# and LGfnd = :errCkFnd
             and LGprocess = :scDropProc and LGseq# <> :LGseq#;
          If decCount > 0;
             msBranProc =
              'A selected group is already assigned to this process.';
             erBranProc = *on;
             isError = *on;
          Endif;
       Enddo;

       Endsr;

       //=====================================================================
       Begsr LoadScreen;
        If LGStat = 'A';
           ScLogoStat = 'Active';
        Else;
           ScLogoStat = 'Inactive';
        Endif;
        scDropProc = LGProcess;
        scLogoDesc = LGLogo;
        scLgPath   = LGPath;

        // Load the Groups
        // Do not show expired groups
        isExpVis = *off;
        If inMode = 'E';
           isExpVis = *on;
        Endif;
        scLGrpSel1 = 'FNexDt = 0 or FNexDt >= ' + %char(%date:*cymd0);
        scLGrpSel2 = *blanks;
        Clear scLogoGrp;
        Setll (LGco#:LGseq#) plg0grp;
        Reade(n) (LGco#:LGseq#) plg0grp;
        Dow not %eof(STLGLGRP);
           If scLogoGrp = ' ';
              scLogoGrp = %editc(grpLGfnd:'X');
           Else;
              scLogoGrp = %trim(scLogoGrp) + ',' + %editc(grpLGfnd:'X');
           Endif;
           Reade(n) (LGco#:LGseq#) plg0grp;
        Enddo;

       Endsr;

       //=====================================================================
       Begsr SaveDB;
        Exsr ErrorChk;
        If isError;
           Leavesr;
        Endif;
        // Get next seq#
        If inMode = 'A';
          SetGT (intCo#:*hival) plg01;
          ReadPe(n) (intCo#) plg01;
          If not %eof(STLGL);
             intSeq = LGseq# + 1;
          Else;
             intSeq = 1;
          Endif;
          LGSeq#    = intSeq;
        Else;
        Chain decRRN STLGP;
        Endif;
        LGStat    = scLogoStat;
        LGProcess = scDropProc;
        LGLogo    = scLogoDesc;
        LGPath    = scLgPath;

        LGuser     = q1User;
        LGpgm      = q1Pgm;
        LGdt       = DateTo7(SystemDate);
        LGtime     = TimeTo6(SystemTime);
        Select;
        When inMode = 'A';
           LGiUsr = LGuser;
           LGiPgm = LGpgm;
           LGiDt  = LGdt;
           LGiTm  = LGtime;
           Write plg0p;
        Other;
           Update plg0p;
        Endsl;


       // HANDLE GROUP LOAD
       // clear the old group selection
        Setll (LGco#:LGseq#) plg0grp;
        Reade (LGco#:LGseq#) plg0grp;
        Dow not %eof(STLGLGRP);
          delete plg0grp;
          Reade (LGco#:LGseq#) plg0grp;
        Enddo;

       // Add new group selection
       grpLGco#  = intCo#;
       grpLGseq# = LGseq#;
       grpLGpgm  = q1Pgm;
       grpLGuser = q1User;
       grpLGtime = Timeto6(SystemTime());
       grpLGdt   = Dateto7(SystemDate());
       grpLGiPgm = q1Pgm;
       grpLGiUsr = q1User;
       grpLGiTim = grpLGtime;
       grpLGiDt  = grpLGdt;
       grpLGProcess = LGProcess;

       strSelection = scLogoGrp;

       Dow strSelection <> ' ';
          grpLGfnd = %int(%subst(strSelection:1:3));
          strSelection = %subst(strSelection:5);
          write plg0grp;
       Enddo;


       Endsr;
      /End-free
     p ProcBrandCfg    e

       //===================================================================
       // Add/Edit Dashboard Items
       //===================================================================
     p ProcDashCfg     b
     d ProcDashCfg     pi
     d  inMode                        1    const
     d  inRRN                         9  0 const options(*nopass)

     d decRRN          s              9  0 inz
     d intItem         s             15  0 inz(1)
     d isError         s              1n   inz(*off)

     d dsBefore        ds                  likerec(pdb0p) inz
     d dsAfter         ds                  likerec(pdb0p) inz
      /Free
       If %parms >= 2;
          decRRN = inRRN;
       Endif;
       Exsr LoadScreen;
       Dou not isError or btnCancel = *on;
          btnSave = *off;
          btnCancel = *off;
          Exfmt rcdItems;
          Select;
          When btnCancel = *on;
          When btnSave = *on;
             Exsr SaveDB;
          Endsl;
       Enddo;

       Return;

       //=====================================================================
       Begsr ClearErr;
        isError = *off;
        erItem = *off;
        erDesc = *off;
        erUrl  = *off;
       Endsr;

       //=====================================================================
       Begsr ErrorChk;
        Exsr ClearErr;
        Chain(n) decRRN SMDBP dsAfter;
        If dsBefore <> dsAfter;
           If PopTwoBtn('This record has been updated by another user.':
            'Record Changed':'Refresh':'Cancel');
              Exsr LoadScreen;
              isError = *on;
              Leavesr;
           Endif;
           btnCancel = *on;
           Leavesr;
        Endif;
        If scAutoGen <> 'Y';
           If scItem = *blank;
              msItem = 'SYM0503';
              mfItem = 'SYMESG';
              erItem = *on;
              isError = *on;
           Endif;
           Setll scItem SMDBL;
           Dou %eof;
              Reade(n) scItem SMDBL;
              If not %eof and DBstat <> 'I' and decRRN <> dbRrnDB0;
                 msItem = 'SYM0520';
                 mfItem = 'SYMESG';
                 erItem = *on;
                 isError = *on;
              Endif;
           Enddo;
        Endif;
        If scDesc = *blank;
           msDesc = 'SYM0503';
           mfDesc = 'SYMESG';
           erDesc = *on;
           isError = *on;
        Endif;
        If scUrl = *blank;
           msUrl = 'SYM0503';
           mfUrl = 'SYMESG';
           erUrl = *on;
           isError = *on;
        Endif;
       Endsr;

       //=====================================================================
       Begsr LoadScreen;
        Chain(n) decRRN SMDBP dsBefore;
        Clear rcdItems;
        Clear pdb0p;
        visAutoGen = *off;
        If inMode = 'A';
           visAutoGen = *on;
           scAutoGen = 'Y';
           scAutoGen = 'Y';
        Endif;
        DBtype = 'iFrame';
        DBstat = 'A';
        DBsizeH = 4;
        DBsizeW = 4;
        Clear DBfullPage;
        Chain(n) decRRN SMDBP;
        If DBfullPage = *blanks;
           DBfullPage = 'visible';
        Endif;
        scDBstat   = DBstat;
        scItemType = DBtype;
        scItem     = DBitem;
        scDesc     = DBdesc;
        scTag      = DBtag;
        scSizeH    = DBsizeH;
        scSizeW    = DBsizeW;
        scURL      = DBurl;
        scUrlFull = DBurlFull;
        scFullPage = DBfullPage;
        scProc     = DBproc;
        scTeamLead = DBteamLead;
       Endsr;

       //=====================================================================
       Begsr SaveDB;
        Exsr ErrorChk;
        If isError;
           Leavesr;
        Endif;
        If scAutoGen = 'Y';
           intItem = *hival;
           scItem = %editc(intItem:'X');
           %subst(scItem:1:%len(%trim(scItemType))) = %trim(scItemType);
           Setgt scItem SMDBL;
           Readp SMDBL;
           Select;
           When not %eof and
            %subst(DBitem:1:%len(%trim(scItemType))) = scItemType;
              intItem = %int(%subst(DBitem:%len(%trim(scItemType))+1))+1;
           Other;
              intItem = 1;
           Endsl;
           scItem = %editc(intItem:'X');
           %subst(scItem:1:%len(%trim(scItemType))) = %trim(scItemType);
        Endif;
        Chain decRRN SMDBP;
        DBstat     = scDBstat;
        DBtype     = scItemType;
        DBitem     = scItem;
        DBdesc     = scDesc;
        DBtag      = scTag;
        DBsizeH    = scSizeH;
        DBsizeW    = scSizeW;
        DBurl      = scUrl;
        DBfullPage = scFullPage;
        DBurlFull  = scUrlFull;
        DBproc     = scProc;
        DBteamLead = scTeamLead;
        DBuser     = q1User;
        DBpgm      = q1Pgm;
        DBdt       = DateTo7(SystemDate);
        DBtime     = TimeTo6(SystemTime);
        Select;
        When inMode = 'A';
           DBiUsr = DBuser;
           DBiPgm = DBpgm;
           DBiDt  = DBdt;
           DBiTim = DBtime;
           Write pdb0p;
        Other;
           Update pdb0p;
        Endsl;
       Endsr;
      /End-free
     p ProcDashCfg     e

       //===================================================================
       // Add/Edit Dashboard Users
       //===================================================================
     p ProcDashUsr     b
     d ProcDashUsr     pi
     d  inMode                        1    const
     d  inRole                       25    const options(*nopass)

     d decStart        s              5  0
     d decEnd          s              5  0
     d i1              s              5  0
     d strRole         s             25    inz
     d strSqlCursor    s           1000
     d isExit          s              1n

     ddsSMDRP        e ds                  extname(SMDRP)
     ddsSMDBP          ds
     d DBitem                        25
     d DBdesc                       100
     d DBtag                        100
      /Free
       If %parms >= 2;
          strRole = inRole;
       Endif;
       isAddUserI = *off;
       Clear scMode;
       Select;
       When inMode = 'E';
          scMode = 'Edit';
       When inMode = 'A';
          Clear strRole;
          isAddUserI = *on;
          scMode = 'Add';
       Endsl;
       scRoleUser = strRole;

       // Clear the subfiles
       clrItemCur = *on;
       clrItemAvl = *on;
       Write rcdItemUsr;
       clrItemCur = *off;
       clrItemAvl = *off;
       rrnItemCur = 0;
       rrnItemAvl = 0;

       strSqlCursor = 'select DISTINCT DBitem, DBdesc ' +
        ', DBtag ' +
        'from SMDBP left join SMDRP on DBITEM = DRITEM ' +
        'where DBstat <> ''I'' and DBtype = ''iFrame'' ' +
        ' and DBitem not in ' +
        ' (select DBitem ' +
        '   from SMDBP left join SMDRP on DBITEM = DRITEM ' +
        '   where DRROLE = ''' + %trim(strRole) + ''')';
       Exec SQL Prepare sqlAvl From :strSqlCursor;
       Exec SQL Declare cursorAvl Cursor For sqlAvl;
       Exec SQL Open cursorAvl;
       Dou SQLSTT <> '00000';
          Exec SQL Fetch next From cursorAvl into :dsSMDBP;
          Select;
          When SQLSTT <> '00000';
             Iter;
          Endsl;
          scDesc = DBdesc;
          scTag  = DBtag;
          scItem = DBitem;
          rrnItemAvl += 1;
          scGridRRN = rrnItemAvl;
          Write sflItemAvl;
       Enddo;
       Exec SQL Close cursorAvl;
       scLastRRNA = rrnItemAvl;

       strSqlCursor = 'select DBitem, DBdesc ' +
        ', DBtag ' +
        'from SMDBP inner join SMDRP on DBITEM = DRITEM ' +
        'where DRROLE = ''' + %trim(strRole) + ''' ' +
        ' and DBtype = ''iFrame'' ' +
        'order by DRposX';
       Exec SQL Prepare sqlCur From :strSqlCursor;
       Exec SQL Declare cursorCur Cursor For sqlCur;
       Exec SQL Open cursorCur;
       Dou SQLSTT <> '00000';
          Exec SQL Fetch next From cursorCur into :dsSMDBP;
          Select;
          When SQLSTT <> '00000';
             Iter;
          Endsl;
          scDesc = DBdesc;
          scTag  = DBtag;
          scItem = DBitem;
          rrnItemCur += 1;
          scGridRRN = *zero;
          Write sflItemCur;
       Enddo;
       Exec SQL Close cursorCur;
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       If rrnItemCur = 0;
          scDesc = 'Charts & Grids';
          scTag  = ' ';
          scItem = 'ChartsGrids';
          rrnItemCur += 1;
          scGridRRN = *zero;
          Write sflItemCur;
       EndIf;
      *** End Add ***** 07/16/24 *** WC2972 ****************************
       scLastRRNC = rrnItemCur;

       // Include enough records in the subfiles for movement
       //*** Beg Chg ***** 06/07/24 ********************************
       //***decEnd   = rrnItemCur + rrnItemAvl;
       intMaxRows = 15; // Maximum rows in subfile
       decEnd   = rrnItemCur + (intMaxRows - rrnItemCur);
       //*** End Chg ***** 06/07/24 ********************************
       decStart = rrnItemCur + 1;
       For i1 = decStart to decEnd;
          Clear scDesc;
          Clear scTag;
          Clear scItem;
          Clear scGridRRN;
          rrnItemCur += 1;
          Write sflItemCur;
       Endfor;
       decStart = rrnItemAvl + 1;
       For i1 = decStart to decEnd;
          Clear scDesc;
          Clear scTag;
          Clear scItem;
          Clear scGridRRN;
          rrnItemAvl += 1;
          Write sflItemAvl;
       Endfor;

       rrnItemAvl = 1;
       rrnItemCur = 1;

       Dou isExit;
          isExit = *off;
          btnCancel = *off;
          btnSave = *off;
          scSrcId = *blank;
          scSrcRRN  = 0;
          scTgtId = *blank;
          scTgtRRN = 0;
          Exfmt rcdItemUsr;
          Select;
          When btnCancel = *on;
             isExit = *on;
          When btnSave = *on;
             Exsr SaveDashUsr;
             isExit = *on;
          Other;
             Exsr DragDropAction;
          Endsl;
       Enddo;

       //=====================================================================
       Begsr DragDropAction;
        // Drag Action being handled in javascript
        Leavesr;
       Endsr;

       //=====================================================================
       Begsr SaveDashUsr;
        // Save being handled in javascript via STAPPCFG05
        Leavesr;
       Endsr;

      /End-free
     p ProcDashUsr     e
       //***  End   Add  ***  12/16/16  ***********************************

       //===================================================================
       // Add/Edit Recipient
       //===================================================================
     p ProcRecpt       b
     d ProcRecpt       pi

     d isNotError      s               n   inz(*off)
     d saveDate1       s              7  0
     d saveTime1       s              6  0

      /free

       clear rcdTrigRec;
       clear rcdRuleRec;
       isUseDgst = isDigestDone;
       scRuleAdd = 'Internal';
       If scAddActI = *blanks and scAddActE <> *blanks;
          scRuleAdd = 'External';
       Endif;
       vsbAddNxt  = *on;
       lftPosCan  = 710;
       selRecUser = 'UPco# = ' + %editc(intCo#:'X') + ' And UPstat = ''A''';
       Select;
       When isClmTrg = *on;
          chRoleRec = 'GTstat <> ''I'' and GTtbid = ''AXRO'' and ' +
           '(GTfg01 = ''C'' or GTfg01 = ''P'') and GTfg02 <> ''E'' and ' +
           'GTco# = ' + %editc(intCo#:'X');
          LoadExtRoles('C':scRecCh:scRecChVal);
       When isPolTrg = *on;
          chRoleRec = 'GTstat <> ''I'' and GTtbid = ''AXRO'' and ' +
           '(GTfg01 = '' '' or GTfg01 = ''P'') and GTfg02 <> ''E'' and ' +
           'GTco# = ' + %editc(intCo#:'X');
          LoadExtRoles('P':scRecCh:scRecChVal);
       When isFileTrg = *on;
          scRuleAdd = 'Email';
       Endsl;

       If flgRecMode = 'E';
          vsbAddNxt = *off;
          lftPosCan = 830;
          Exsr LoadRecp;
       Endif;

       FltCanRec  = *off;
       FltNxtTrg  = *off;
       filtSavTrg = *off;
       isNotError = *off;

       Dou filtSavTrg = *on and isNotError = *off;

          FltCanRec  = *off;
          FltNxtTrg  = *off;
          filtSavTrg = *off;

          scOpenDlg = 'Y';
          Select;
          When isRules = *on;
             Exfmt rcdRuleRec;
          Other;
             Exfmt rcdTrigRec;
          Endsl;
          scOpenDlg = ' ';

          isNotError = *off;
          erRoleRec  = *off;
          erRecUser  = *off;
          erAltRole  = *off;
          erAltRcUsr = *off;
          erRoleRecE = *off;
          erAltRoleE = *off;
          erEmail = *off;
          erText  = *off;

          Select;
          When filtSavTrg = *on or FltNxtTrg = *on;
             Exsr ValidateRec;
             If isNotError = *off;
                Exsr ProcSaveRec;
                If FltNxtTrg = *on;
                   clear scRoleRec;
                   clear scAltRole;
                   clear scRecUser;
                   clear scAltRcUsr;
                   Clear scRoleRecE;
                   Clear scAltRoleE;
                   Clear scEmail;
                   Clear scText;
                   Clear scDigest;
                   Clear scDigestAl;
                Endif;
             Endif;

          When FltCanRec = *on;
             FltCanRec = *off;
             Leave;
          Endsl;
       Enddo;

       //===================================================================
       // LoadRecp
       //===================================================================
       Begsr LoadRecp;

       If flgTrgMode = 'A';
          Chain(n) scRecptRrn SMTRPRC1;
       Else;
          Chain(n) scRecptRrn SMTRPREC;
       Endif;
       If %found;
          scRuleAdd = 'Internal';
          Select;
          When TRtype = 'Email';
             scEmail = TRemail;
             scText  = TRtext;
             scRuleAdd = 'Email';
          When TRtype  = 'External';
             scRoleRecE = TRrecip;
             scRuleAdd = 'External';
          When TRtype  = 'Role';
             scRolerec = TRrecip;
          When TRtype  = 'Individual';
             scRecUser = TRrecip;
          Endsl;

          Select;
          When TRtype  = 'External';
             scAltRoleE  = TRaltRecip;
          When TRaltType = 'Role';
             scAltRole   = TRaltRecip;
          When TRaltType = 'Individual';
             scAltRcUsr  = TRaltRecip;
          Endsl;
          scDigest = TRdigest;
          scDigestAl = TRdigestAl;
          SaveDate1 = TRdt;
          SaveTime1 = TRtime;
       Endif;

       Endsr;

       //===================================================================
       // Validate Recipient
       //===================================================================
       Begsr ValidateRec;

       isNotError = *off;

       If flgRecMode = 'E' and flgTrgMode = 'E';
          flgRcdUpdated = RcdUpdatedErr('SMTRPREC':scRecptRrn:'TRdt':
           'TRtime':'TRuser':saveDate1:saveTime1);
          Select;
          When flgRcdUpdated = '0'; // No error
          When flgRcdUpdated = '1'; // Error
             isNotError = *on;
          When flgRcdUpdated = '2'; // Error with refresh
          Endsl;
       Endif;

       Select;
       When scRuleAdd = 'Email';
          If scEmail = *blank and scText = *zero;
             msEmail = 'SYM0521';
             msText  = 'SYM0521';
             erEmail = *on;
             erText  = *on;
             isNotError = *on;
          Endif;
          Leavesr;
       When scRuleAdd = 'External';
          If scRoleRecE = *blank;
             msRoleRecE = 'WCR0503';
             mfRoleRecE = 'WMESGG';
             erRoleRecE = *on;
             isNotError = *on;
          Endif;
          Leavesr;
       Endsl;

       // either of Role or user must be selected.
       If scRoleRec  = ' ' and scRecUser = ' ';
          msRoleRec  = 'WCR1324';
          msRecUser  = 'WCR1324';
          erRoleRec  = *on;
          erRecUser  = *on;
          isNotError = *on;
       Endif;

       // Role and user are mutually exclusive.
       If scRoleRec <> ' ' and scRecUser <> ' ';
          msRoleRec  = 'WCR0590';
          msRecUser  = 'WCR0590';
          erRoleRec  = *on;
          erRecUser  = *on;
          isNotError = *on;
       Endif;

       // alternate Role and user are mutually exclusive.
       If scAltRole <> ' ' and scAltRcUsr <> ' ';
          msAltRole  = 'WCR0590';
          msAltRcUsr = 'WCR0590';
          erAltRole  = *on;
          erAltRcUsr = *on;
          isNotError = *on;
       Endif;

       Endsr;

       //===================================================================
       // Save the Recipient
       //===================================================================
       Begsr ProcSaveRec;

       If flgRecMode = 'E';
          If flgTrgMode = 'E';
             Chain scRecptRrn SMTRPREC;
          Else;
             Chain scRecptRrn SMTRPRC1;
          Endif;
       Else;
          If flgTrgMode = 'E';
             clear ptr0rec;
             TRco#  = mTRCo#;
             TRname = mTRName;
             TRseq# = mTRSeq#;
          Else;
             clear ptr0rc1;
             TRco#  = intTmpCo#;
             TRname = strTRName;
             TRseq# = intTmpSeq;
          Endif;
          TRiPgm = q1Pgm;
          TRiUsr = q1User;
          TRiTim = Timeto6(SystemTime());
          TRiDt  = Dateto7(SystemDate());
       Endif;

       Select;
       When scRuleAdd = 'Email';
          TRtype = 'Email';
          TRemail = scEmail;
          TRtext  = scText;
       When scRuleAdd = 'External';
          TRrecip = scRoleRecE;
          TRtype = 'External';
       When scRoleRec <> ' ';
          TRrecip = scRoleRec;
          TRtype  = 'Role';
       When scRecUser <> ' ';
          TRrecip = scRecUser;
          TRtype  = 'Individual';
       Endsl;

       Select;
       When scRuleAdd = 'External';
          TRaltRecip = scAltRoleE;
          TRaltType = 'External';
       When scAltRole <> ' ';
          TRaltRecip = scAltRole;
          TRaltType  = 'Role';
       When scAltRcUsr <> ' ';
          TRaltRecip = scAltRcUsr;
          TRaltType  = 'Individual';
       Endsl;
       TRdigest = scDigest;
       TRdigestAl = scDigestAl;

       TRpgm  = q1Pgm;
       TRuser = q1User;
       TRtime = Timeto6(SystemTime());
       TRdt   = Dateto7(SystemDate());

       If flgRecMode = 'E';
          If flgTrgMode = 'E';
             Update ptr0rec;
          Else;
             Update ptr0rc1;
          Endif;
       Else;
          If flgTrgMode = 'E';
             Write ptr0rec;
          Else;
             Write ptr0rc1;
          Endif;
       Endif;

       Endsr;

      /end-free

     p ProcRecpt       e
       //===================================================================
       // Save the Trigger textual detail.
       //===================================================================
     pSaveRuleText     b
     dSaveRuleText     pi

     dstrActions       s                   like(xTRactions)
     dstrCond          s                   like(xTRcond)
     dstrGroup         s                   like(xTRcond)
     dstrField         s                   like(xTRcond)
     dstrUsers         s                   like(xTRactions)

      /Free
        Clear strCond;
        Chain(n) (mTRco#:mTRname:mTRseq#) smtrl;
        Select;
        When mTRtype = 'Claims';
           Clear ptr0clm;
           Chain(n) (mTRco#:mTRname:mTRseq#) smtrpclm;
        When mTRtype = 'Policy';
           Clear ptr0pol;
           Chain(n) (mTRco#:mTRname:mTRseq#) smtrppol;
        When mTRtype = 'File';
           Clear ptr0fil;
           Clear ptr0fld;
           Clear ptr0frq;
           Chain(n) (mTRco#:mTRname:mTRseq#) smtrpfil;
           Chain(n) (mTRco#:mTRname:mTRseq#) smtrpfld;
           Chain(n) (mTRco#:mTRname:mTRseq#) smtrpfrq;
        Endsl;

        // Groups
        Clear strGroup;
        Setll (mTRco#:mTRname:mTRseq#) smtrpgrp;
        Dou %eof;
           Reade(n) (mTRco#:mTRname:mTRseq#) smtrpgrp;
           If %eof;
              Iter;
           Endif;
           strGroup = BuildRulesText(strGroup:'in group':'or'
            :%editc(gTRfnd:'Z'));
        Enddo;
        strCond = BuildRulesText(strCond:*blanks:'and':strGroup);

        Select;
        When mTRtype = 'Claims';
           Exsr SaveRuleTextClm;
           strCond = BuildRulesText('When a claim':*blanks:*blank:strCond);
        When mTRtype = 'Policy';
           Exsr SaveRuleTextPol;
           strCond = BuildRulesText('When a policy':*blanks:*blank:strCond);
        When mTRtype = 'File';
           Exsr SaveRuleTextFile;
           strCond = BuildRulesText('When a record in file, '
            +%trim(GetFileDesc(fTRfile:fTRlibrary:'*AFTER':'('))+', '
            :*blanks:*blank:strCond);
        Endsl;

        Clear strActions;
        Setll (mTRco#:mTRname:mTRseq#) smtrpact;
        Dou %eof(smtrpact);
           Reade (mTRco#:mTRname:mTRseq#) smtrpact;
           Select;
           When %eof and strActions = *blank;
              aTRaction = 'Send Diary';
           When %eof;
              Iter;
           Endsl;
           strActions = BuildRulesText(strActions:*blanks:'and':aTRaction);
           Clear strUsers;
           Setll (mTRco#:mTRname:mTRseq#) smtrlrec;
           Dou %eof(smtrlrec);
              Reade (mTRco#:mTRname:mTRseq#) smtrlrec;
              Select;
              When %eof;
                 Iter;
              When aTRexternal = 'E' and aTRaction = 'Send Email'
                and TRtype = 'Email' and TRemail <> *blank;
                 strUsers = BuildRulesText(strUsers:*blanks:'and'
                  :TRemail);
                 Iter;
              When aTRexternal = 'E' and aTRaction = 'Send Text'
                and TRtype = 'Email' and TRtext <> *zero;
                 strUsers = BuildRulesText(strUsers:*blanks:'and'
                  :%editc(TRtext:'Z'));
                 Iter;
              When aTRexternal = 'E' and TRtype <> 'External';
                 Iter;
              When aTRexternal <> 'E' and TRtype = 'External';
                 Iter;
              Endsl;
              strUsers = BuildRulesText(strUsers:*blanks:'and'
               :GetRecipDesc(TRrecip:TRtype));
              If TRaltRecip <> *blanks;
                 strUsers = BuildRulesText(strUsers:*blanks:'(or'
                  :%trim(GetRecipDesc(TRaltRecip:TRaltType))+')');
              Endif;
           Enddo;
           If strUsers <> *blanks;
              strActions = BuildRulesText(strActions:*blanks:'to':strUsers);
           Endif;
        Enddo;

        Chain (mTRco#:mTRname:mTRseq#) smtrptxt;
        xTRco#  = mTRco#;
        xTRname = mTRname;
        xTRseq# = mTRseq#;
        xTRtype = mTRtype;
        xTRcond = strCond;
        xTRactions = strActions;
        xTRuser = q1User;
        xTRpgm  = q1Pgm;
        xTRtime = TimeTo6(SystemTime);
        xTRdt   = DateTo7(SystemDate);
        If %found;
           Update ptr0txt;
        Else;
           xTRiUsr = xTRuser;
           xTRiPgm = xTRpgm;
           xTRiTim = xTRtime;
           xTRiDt  = xTRdt;
           Write ptr0txt;
           Feod smtrptxt;
        Endif;

        Return;
       //===================================================================
       // Save the Trigger textual detail - claim
       //===================================================================
       Begsr SaveRuleTextClm;

        // Claim Status
        strCond = BuildRulesText(strCond:'has a status of':'and':cTRclmSts);

        // Claim Type
        strCond = BuildRulesText(strCond:'has claim type':'and'
         :BuildClmTypText(cTRclmType));

        // Jurisdiction states (STSTL)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('States':'has jursidiction state'));

        // Claim Indicators (STGTL - AXCI)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('Claim Indicator':'has claim indicator'));

        // NCCI Indjury Code (WTGTL - xxNC)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('NCCI Injury':'has injury code'));

        // Nature Code (WTABL - pna00)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('Nature':'has nature code'));

        // Part Code (WTABL - ppa00)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('Part':'has part code'));

        // Cause Code (WTABL - pca00)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('Cause':'has cause code'));

        // PV1 (WTABL - pb200)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('Payment':'has payment value 1 code'));

        // Reserve (WTABL - prt00)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('Reserve':'has reserve code'));

        // Retrospective Settlement
        If cTRrSet = 'Y';
           strCond = BuildRulesText(strCond
            :'has a finalized retrospective settlement':'and':cTRrSet);
        Endif;

        // Amount
        If cTRamt <> *zero;
           strCond = BuildRulesText(strCond
            :'has a reserve change greater than or equal to $':'and'
            :%editc(cTRamt:'J'));
        Endif;

        // Payment Count
        If cTRpayCount <> *zero;
           strCond = BuildRulesText(strCond
            :'has a payment count greater than or eqaul to':'and'
            :%editc(cTRpayCount:'X'));
        Endif;

        // % of Specific Retention
        If cTRreten <> *zero;
           strCond = BuildRulesText(strCond
            :'has a specific retention greater than or eqaul to':'and'
            :%editc(cTRreten:'J')+'%');
        Endif;

        // Total Paid
        If cTRtotPaid <> *zero;
           strCond = BuildRulesText(strCond
            :'has total paid greater than or equal to $':'and'
            :%editc(cTRtotPaid:'J'));
        Endif;

        // Total Incurred
        If cTRtotInc <> *zero;
           strCond = BuildRulesText(strCond
            :'has total incurred greater than or equal to $':'and'
            :%editc(cTRtotInc:'J'));
        Endif;

        // Compensation Weeks Paid
        If cTRweeksPd <> *zero;
           strCond = BuildRulesText(strCond
            :'has total weeks paid greater than or equal to':'and'
            :%editc(cTRweeksPd:'X'));
        Endif;

       Endsr;
       //===================================================================
       // Save the Trigger textual detail - file
       //===================================================================
       Begsr SaveRuleTextFile;

        // Trigger Actions
        Setll (mTRco#:mTRname:mTRseq#) smtrpfrq;
        Dou %eof(SMTRPFRQ);
           Reade(n) (mTRco#:mTRname:mTRseq#) smtrpfrq;
           Select;
           When %eof;
              Iter;
           When qTRtrgEvent = '*INSERT';
              qTRtrgEvent = %trim(qTRtrgEvent) + 'E';
           Endsl;
           qTRtrgEvent = %trim(%replace('':qTRtrgEvent:1:1)) + 'D';
           strCond = BuildRulesText(strCond:'has been ':'or'
            :Lower(qTRtrgEvent));
        Enddo;

        // Fields
        Clear strField;
        Setll (mTRco#:mTRname:mTRseq#:'Change') smtrpfld;
        Dou %eof(SMTRPFLD);
           Reade(n) (mTRco#:mTRname:mTRseq#:'Change') smtrpfld;
           Select;
           When %eof;
              Iter;
           When dTRfield = '*ALL';
              dTRfield = Lower(%trim(%replace('':dTRfield:1:1)) + ' fields');
           Endsl;
           strField = BuildRulesText(strField:*blank:'or'
            :GetFieldDesc(dTRfield:qTRfile:qTRlibrary:'*AFTER':'('));
        Enddo;
        strCond = BuildRulesText(strCond:'to impact ':*blank
         :strField);

       Endsr;
       //===================================================================
       // Save the Trigger textual detail - policy
       //===================================================================
       Begsr SaveRuleTextPol;

        // Policy Status Type
        strCond = BuildRulesText(strCond:'exists in':'and'
         :BuildPolStsText(pTRpolStsTp));

        // Policy Status
        strCond = BuildRulesText(strCond:'has a status of':'and':pTRpolSts);

        // Quote Type
        If pTRqteType <> *blank;
           strCond = BuildRulesText(strCond:'is a':'and'
            :%trim(pTRqteType)+' policy');
        Endif;

        // Quote Status (WTQSL)
        strCond = BuildRulesText(strCond:*blanks:'and'
         :BuildTableText('Quote Status':'has quote status'));

        // Premium Amount
        If pTRpremAmt <> *zero;
           strCond = BuildRulesText(strCond
            :'has a premium amount greater than or equal to $':'and'
            :%editc(pTRpremAmt:'J'));
           strCond = BuildRulesText(strCond:'(at the':*blank
            :%trim(pTRpremLvl)+' premium level)');
        Endif;

        // $ Value change
        If pTRchgVal <> *zero;
           strCond = BuildRulesText(strCond
            :'has a premium change greater than or equal to $':'and'
            :%editc(pTRchgVal:'J'));
           strCond = BuildRulesText(strCond:'(at the':*blank
            :%trim(pTRpremLvl)+' premium level)');
        Endif;

        // % change
        If pTRchg <> *zero;
           strCond = BuildRulesText(strCond
            :'has a premium change greater than or equal to':'and'
            :%editc(pTRchg:'J')+'%');
           strCond = BuildRulesText(strCond:'(at the':*blank
            :%trim(pTRpremLvl)+' premium level)');
        Endif;

        If pTRCompDate <> *blanks;
           Select;
           When pTRName = 'EXPMODDATE';
              strCond = BuildRulesText(strCond:
               'has a exp mod date ' + pTRCompOper:
               'and':
               pTRCompDate);
           EndSl;
        Endif;

        If pTRName = 'POLADDRCHG';
           strCond = BuildRulesText(strCond:
            *blank:
            'and':
            'has a policy mailing or physical address change.');
        Endif;

       Endsr;
      /End-free
     pSaveRuleText     e
       //===================================================================
       // Check for Auto Diary Trigger Filter
       //===================================================================
     p MeetFilter      b
     d MeetFilter      pi              n

     d isMeetFilter1   s               n
     d isMeetFilter2   s               n
     d isMeetFilter3   s               n
      /free

       // display records based on trigger type selection.
       Select;
       When isClmTrig = *on and mTRtype <> 'Claims';
          Return *off;
       When isPolTrig = *on and mTRtype <> 'Policy';
          Return *off;
       When isFileTrig = *on and mTRtype <> 'File';
          Return *off;
       Endsl;

       // Auto diary trigger
       If scFiltTrg <> ' ' and %scan(%trim(mTRname):scFiltTrg) = 0;
          Return *off;
       Endif;

       // Auto diary trigger status
       If scFltTrgSt <> ' ' and %scan(%trim(mTRstat):scFltTrgSt) = 0;
          Return *off;
       Endif;

       // Check for Scope
       If ssFltScope <> *blank;
          Chain (mTRco#:mTRname:mTRseq#:ssFltScope) ptr0act;
          Select;
          When not %found and ssFltScope = 'I';
             // Check for where no update has been made since 'Rules' implemented
             Chain (mTRco#:mTRname:mTRseq#) ptr0act;
             If %found;
                Return *off;
             Endif;
          When not %found;
             Return *off;
          Endsl;
       Endif;

       isMeetFilter1 = *off;
       isMeetFilter2 = *off;
       // Check for Role,Aletrnate Role,User,Alternate user
       Setll (mTRco#:mTRname:mTRseq#) ptr0rc3;
       Reade(n) (mTRco#:mTRname:mTRseq#) ptr0rc3;
       If %eof(SMTRLREC);
          isMeetFilter1 = *on;
          isMeetFilter2 = *on;
       Endif;

       Dow not %eof(SMTRLREC);
          // Filter Role
          If scFiltRole = ' ' or ((TRtype = 'Role' and TRrecip <> ' ' and
           %scan(%trim(TRrecip):scFiltRole) <> 0) or (TRaltType = 'Role' and
           TRaltRecip <> ' ' and %scan(%trim(TRaltRecip):scFiltRole) <> 0));
             isMeetFilter1 = *on;
          Endif;

          // Filter user
          If scFiltUser = ' ' or ((TRtype = 'Individual' and TRrecip <> ' ' and
           %scan(%trim(TRrecip):scFiltUser) <> 0) or (TRaltType = 'Individual'
           and TRaltRecip <> ' ' and %scan(%trim(TRaltRecip):scFiltUser) <> 0));
             isMeetFilter2 = *on;
          Endif;

          If isMeetFilter1 = *on and isMeetFilter2 = *on;
             Leave;
          Endif;
          Reade(n) (mTRco#:mTRname:mTRseq#) ptr0rc3;
       Enddo;

       // Check for Group
       isMeetFilter3 = *off;
       Setll (mTRco#:mTRname:mTRseq#) ptr0grp;
       Reade(n) (mTRco#:mTRname:mTRseq#) ptr0grp;
       IF %eof(SMTRPGRP);
          isMeetFilter3 = *on;
       Endif;
       Dow not %eof(SMTRPGRP);
          If scFltTrgGr = *Zero or GTRfnd = scFltTrgGr;
             isMeetFilter3 = *on;
             Leave;
          Endif;
          Reade(n) (mTRco#:mTRname:mTRseq#) ptr0grp;
       Enddo;

       If isMeetFilter1 = *on and isMeetFilter2 = *on and
          isMeetFilter3 = *on;
          Return *on;
       Endif;

       Return *off;
      /end-free

     p MeetFilter      e
       //===================================================================
       // Load Output Control Grid
       //===================================================================
     p LoadOutput      b
     d LoadOutput      pi
      /free
       // Clear the subfile.
       rrnOut = 0;
       dspOut = *off;
       dspOutCtl = *off;
       clrOut = *on;
       Write rcdOutput;
       clrOut = *off;
       dspOut = *on;
       dspOutCtl = *on;

       // Load the Output Control subfile
       Setll *loval psp00;

       Dou %eof;
          Read  psp00;
          If not %eof and SPStat = 'A';
             // Check filter criteria
             Select;
             // Output contains
             When fltrType <> *blanks
              and (%scan(%trim(Upper(fltrType)):Upper(SPtype))=0);
                Iter;
             // Hardcopy
             When fltrSplO <> *blanks and fltrSplO <> SPsplO;
                Iter;
             // Hardcopy retired
             When fltrSplR <> *blanks and fltrSplR <> SPsplR;
                Iter;
             // Scriptura
             When fltrScrO <> *blanks and fltrScrO <> SPscrO;
                Iter;
             // 3rd Party XML
             When fltr3Xml <> *blanks and fltr3Xml <> SP3Xml;
                Iter;
             Endsl;

             scType  = SPtype;
             scSplO  = SPsplO;
             scScrO  = SPscrO;
             sc3Xml  = SP3Xml;
             scCat   = SPCat;
        //*** Beg ADD *** 06/20/22 ******************************
             xxTbid = 'DOCS';
             xxKey  = SPCat;
             Chain keySTGTL pgt00;
             if %found;
                if GTDesc <> '' ;
                   scCat = GTDesc;
                endif;
             Endif;
        //*** End ADD *** 06/20/22 ******************************

             scOutRRN  = sprrn;

             isDisSpool = *off;
             Clear scToolSpl;
             If SPsplR = 'Y';
                isDisSpool = *on;
                scToolSpl = 'Hardcopy output has been retired for this item ' +
                 'and is not avaliable.';
             Endif;
             isDisScrp = *off;
             Clear scToolScrp;
             If isUseScrp = *off;
                isDisScrp = *on;
                scToolScrp = 'Scriptura output is not availble for this item.';
             Endif;
             isDis3rd = *off;
             Clear scTool3rd;
             If isUse3rd = *off;
                isDis3rd = *on;
                scTool3rd = '3rd party XML output is not availble for this ' +
                 'item.';
             Endif;
             isVisEdit = (isUseScrp = *on or isUse3rd = *on) and not dspMode;
             If dspMode = *on;
                isDis3rd = *on;
                isDisScrp = *on;
                isDisSpool = *on;
             Endif;

             rrnOut = rrnOut + 1;
             Write sflOutCtl;
          Endif;

       Enddo;

       isVisScrp = isUseScrp;
       isVis3rd  = isUse3rd;

       rrnOut = 1;

      /end-free
     p LoadOutput      e
       //===================================================================
       // Load Output Control filter
       //===================================================================
     p SetOutFilter    b
     d SetOutFilter    pi
      /free
       isVisScrp = isUseScrp;
       isVis3rd  = isUse3rd;
       showOutClr = *Off;
       ssType = fltrType;
       ssSplO = fltrSplO;
       ssSplR = fltrSplR;
       ssScrO = fltrScrO;
       ss3Xml = fltr3Xml;
       Dou (outFltrCx = *on) or (outFltrSv = *on);
         Exfmt rcdOutFltr;
         if (outFltrSv = *on);
            fltrType = ssType;
            fltrSplO = ssSplO;
            fltrSplR = ssSplR;
            fltrScrO = ssScrO;
            fltr3Xml = ss3Xml;
            LoadOutput();
         Endif;
       Enddo;
       If fltrType <> *blank
        or fltrSplO <> *blank
        or fltrSplR <> *blank
        or fltrScrO <> *blank
        or fltr3Xml <> *blank;
          showOutClr = *On;
       Endif;
      /end-free
     p SetOutFilter    e
       //===================================================================
       // Clear Output Control Filter
       //===================================================================
     p ClearOutFilter  b
     d ClearOutFilter  pi
      /free
       Clear fltrType;
       Clear fltrSplO;
       Clear fltrSplR;
       Clear fltrScrO;
       Clear fltr3Xml;
       LoadOutput();
       showOutClr = *Off;
      /end-free
     p ClearOutFilter  e
       //===================================================================
       // Configure (Add/Edit) Output Control
       //===================================================================
     p CfgOutputCtl    b
     d CfgOutputCtl    pi
     D inOutRRN                       9  0 const options(*nopass)

     D xxRRN           s              9  0 inz(0)
     d error           s               n   inz(*off)
     D xxCancel        s              1
     D topOffset       s              5  0 inz(0)
     D reportType      s                   like(S1Type)
     d str100          s            100    inz(*blanks)
      //*** Beg Add ***** 05/18/23 ********************************
     d strRunType2     s            100    inz(*blanks)
      //*** End Add ***** 05/18/23 ********************************
      /free
       If %parms >= 1;
          xxRRN = inOutRRN;
       Endif;

       //* Read Scriptura module file.
       Read(n) psc0p;

       Clear rcdOutCfg;

       If xxRRN <> 0;
          Exsr LoadOutputCtl;
       Else;
          Return;
       Endif;

       outCfgCx = *off;
       outCfgSv = *off;

       Exsr ClearErrors;

       isVisScrp = isUseScrp;
       isVis3rd  = isUse3rd;

       //*** Beg Add *** 03/04/24 *******************************************
       //*** Beg Chg *** 07/30/24 *******************************************
       //*** Exsr SetStaffCH;
       SetStaffCH();
       //*** End Chg *** 07/30/24 *******************************************
       //*** End Add *** 03/04/24 *******************************************
       Dou (outCfgCx = *on) or (outCfgSv = *on);
         Exfmt rcdOutCfg;
         If outCfgCx = *on;
            Leave;
         Endif;

         Exsr ClearErrors;
         Exsr CfgOutCheck;

         //* Process replacement doc actions only if no errors present for output setup.
         If error = *off;

            //* Add replacement doc record.
            If btnReplAdd = *on;
               ReplaceWindow(SPType:SPInter);
               Exsr LoadReplaceGrid;
               btnReplAdd = *off;
            Endif;

            //* Edit printer tray.
            Readc sflReplace;
            If not %eof();
               If replEdit = *on;
                  replEdit = *off;
                  Update sflReplace;
                  ReplaceWindow(SPType:SPInter:outReplRRN);
                  Exsr LoadReplaceGrid;
               Endif;
               //***** Beg Add *** 08/21/23 ****************************
               If replCopy = *on;
                  replCopy = *off;
                  Update sflReplace;
                  ReplaceWindow(SPType:SPInter:outReplRRN:*on);
                  Exsr LoadReplaceGrid;
               Endif;
               //***** End Add *** 08/21/23 ****************************
            Endif;

            //* Add logo record.
            If btnLogoAdd = *on;
            //*** begin chg ***  09/25/24  ********************************************
            //*****   LogoWindow(SPType);
               LogoWindow(SPType:SPInter);
            //*** end chg *****  09/25/24  ********************************************
               Exsr LoadReplaceGrid;
               btnLogoAdd = *off;
            Endif;

            Readc sflLogos;
            If not %eof();

            //* Edit logo record.
               If logoEdit = *on;
                  logoEdit = *off;
                  Update sflLogos;
            //*** begin chg ***  09/25/24  ********************************************
            //*****      LogoWindow(SPType:outLRRN);
                  LogoWindow(SPType:SPInter:outLRRN);
            //*** end chg *****  09/25/24  ********************************************
                  Exsr LoadReplaceGrid;
               Endif;

            //* Delete logo record.
               If logoDelete = *on;
                  logoDelete = *off;
                  Update sflLogos;
                   strMsg = 'Press Confirm to remove this logo setup record.';
                   If not PopTwoBtn(strmsg:'Delete logo image record.':
                    'Cancel':'Confirm');
                      Chain outLRRN psp12p;
                      If %found;
                         Delete psp12p;
                         Exsr LoadReplaceGrid;
                      Endif;
                   Endif;
               Endif;
               //***** Beg Add *** 08/21/23 ****************************
               If logoCopy = *on;
                  logoCopy = *off;
                  Update sflLogos;
            //*** begin chg ***  09/25/24  ********************************************
            //*****      LogoWindow(SPType:outLRRN:*on);
                  LogoWindow(SPType:SPInter:outLRRN:*on);
            //*** end chg *****  09/25/24  ********************************************
                  Exsr LoadReplaceGrid;
               Endif;
               //***** End Add *** 08/21/23 ****************************

            Endif;

            //* Add signature record.
            If btnSigAdd = *on;
               SigWindow(SPType);
               Exsr LoadReplaceGrid;
               btnSigAdd = *off;
            Endif;

            Readc sflSignat;
            If not %eof();

            //* Edit signature record.
               If sigEdit = *on;
                  sigEdit = *off;
                  Update sflSignat;
                  SigWindow(SPType:outSRRN);
                  Exsr LoadReplaceGrid;
               Endif;

            //* Delete Signature record.
               If sigDelete = *on;
                  sigDelete = *off;
                  Update sflSignat;
                   strMsg = 'Press Confirm to remove this signature setup '+
                    'record.';
                   If not PopTwoBtn(strmsg:'Delete signature image record.':
                    'Cancel':'Confirm');
                      Chain outSRRN psp13p;
                      If %found;
                         Delete psp13p;
                         Exsr LoadReplaceGrid;
                      Endif;
                   Endif;
               Endif;
               //***** Beg Add *** 08/21/23 ****************************
               If sigCopy = *on;
                  sigCopy = *off;
                  Update sflSignat;
                  SigWindow(SPType:outSRRN:*on);
                  Exsr LoadReplaceGrid;
               Endif;
               //***** End Add *** 08/21/23 ****************************

            Endif;

            //*** Beg Add ***  08/04/17  ************************************
            //* Add Letter Template record.
            If btnLtrAdd = *on;
               //*** Beg Chg ***** 03/22/24 *********************************
               //***LtrWindow(SPType);
               LtrWindow(SPType:SpCat);
               //*** End Chg ***** 03/22/24 *********************************
               Exsr LoadReplaceGrid;
               btnLtrAdd = *off;
            Endif;

            Readc sflLtrTmpl;
            If not %eof();

            //* Edit Letter Template record.
               If LtrEdit = *on;
                  LtrEdit = *off;
                  Update sflLtrTmpl;
                  //*** Beg Chg ***** 03/22/24 **************************
                  //***LtrWindow(SPType:outLTRRN);
                  LtrWindow(SPType:SPCat:outLTRRN);
                  //*** End Chg ***** 03/22/24 **************************
                  Exsr LoadReplaceGrid;
               Endif;

            //* Delete Signature record.
               If LtrDelete = *on;
                  LtrDelete = *off;
                  Update sflLtrTmpl;
                   strMsg = 'Press Confirm to remove this Letter Template '+
                    'setup record.';
                   If not PopTwoBtn(strmsg:'Delete Letter Template record.':
                    'Cancel':'Confirm');
                      Chain outLTRRN psp14p;
                      If %found;
                         Delete psp14p;
                         Exsr LoadReplaceGrid;
                      Endif;
                   Endif;
               Endif;

               //***** Beg Add *** 08/21/23 ****************************
               If LtrCopy = *on;
                  LtrCopy = *off;
                  Update sflLtrTmpl;
                  //*** Beg Chg ***** 03/22/24 ************************
                  //***LtrWindow(SPType:outLTRRN:*on);
                  LtrWindow(SPType:SPCat:outLTRRN:*on);
                  //*** End Chg ***** 03/22/24 ************************
                  Exsr LoadReplaceGrid;
               Endif;
               //***** End Add *** 08/21/23 ****************************
            Endif;
            //*** End Add ***  08/04/17  ************************************

         Endif;
         If (outCfgSv = *on);
            If not error;
               Exsr SaveOutputCtl;
            Else;
               outCfgSv = *off;
            Endif;
         Endif;
         If outCfgSv = *on and SPSetup <> ' ';

             Monitor;
                strPgm = SPSetup;
                Clear xxCancel;
                ExtraSetupPgm(xxCancel);
                If xxCancel = *on;
                   outCfgSv = *off;
                Endif;
             On-error;
             Endmon;

         Endif;
       Enddo;
       //===================================================================
       // Clear the error fields for Output Control
       //===================================================================
       BegSr ClearErrors;

         error = *off;

       //*** Beg Add **** 09/11/23 **** WC2907 **************
       erFromEml = *Off;
       clear msFromEml;
       //*** End Add **** 09/11/23 **** WC2907 **************

       Endsr;
       //===================================================================
       // Edit the Output Control fields
       //===================================================================
       BegSr CfgOutCheck;

       //*** Beg Add **** 09/11/23 **** WC2907 **************
       // Validate the From Email Address field content
       If scFromEml <> ' ';
       erFromEml = Check_Email(scFromEml:msFromEml);
         If erFromEml = *on;
           error = *on;
          EndIf;
       EndIf;
       //*** End Add **** 09/11/23 **** WC2907 **************

       Endsr;
       //===================================================================
       // Save the Ouput Control record
       //===================================================================
       BegSr SaveOutputCtl;

         Chain xxRRN psp0p;

         SP3DtQS  = sc3DtQS;
         SP3DtQR  = sc3DtQR;
         SP3XmlF  = sc3XmlF;

         SPForm = scFormType;
         SPHold = scOutHold;
         SPSave = scOutSave;
         SPNoLogo = scNoLogo;
         SPEmlSub = scEmlSub2;
         SPEmlBody = scEmlBody2;
         SPEmlUsro = scEmlUsro2;
       //** Beg Add  ***  09/11/23 WC2907 ************************
         SPFemail = scFromEml;
       //** End Add  ***  09/11/23 WC2907 ************************

       //** Beg Add  ***  11/26/22 WC2838 ************************

         SPEmlSubM  = scEmlSubM;
         SPEmlBodyM = scEmlBodyM;
         SPEmlSubF  = scEmlSubF;
         SPEmlBodyF = scEmlBodyF;

         SPPKeyI  = scSPrintMI ;
         SPTrayI  = scSTrayMI  ;
         SPOverPI = scSOverPMI ;
         SPPKeyA  = scSPrintMA ;
         SPTrayA  = scSTrayMA  ;
         SPOverPA = scSOverPMA ;

         If scFilePref <> ' ';
            SPFnPreFix = %Trim(scFilePref);
         EndIf;

         SPFnData = %Trim(scSFileNm);

       //** End Add  ***  11/26/22 WC2838 ************************
       //** Beg Add  ***  01/16/24 *** 153WAD ********************
            SPCopyE = scCopyE;
       //** End Add  ***  01/16/24 *** 153WAD ********************
       //** Beg Add  ***  07/25/23 WC2850 ************************
         If scCopiesS = 0;
            SPCpyS = 1;
         Else;
            SPCpyS = scCopiesS;
         EndIf;
         If scCopiesM = 0;
            SPCpyM = 1;
         Else;
            SPCpyM = scCopiesM;
         EndIf;
       //** End Add  ***  07/25/23 WC2850 ************************
         pcrSPtype = SPtype;
         pcrSPcritid = %trim('Distribution');
         pcrSPpgm  = q1Pgm;
         pcrSPuser = q1User;
         pcrSPtime = Timeto6(SystemTime());
         pcrSPdt   = Dateto7(SystemDate());
         pcrSPiPgm = q1Pgm;
         pcrSPiUsr = q1User;
         pcrSPiTim = Timeto6(SystemTime());
         pcrSPiDt  = Dateto7(SystemDate());
         // Clear then add old Distribution Types
         Setll (SPtype:conDist) psp0crt;
         Reade (SPtype:conDist) psp0crt;
         Dow not %eof(STSPPCRT);
            Delete psp0crt;
            Reade (SPtype:conDist) psp0crt;
         Enddo;
         strSelDistT = scDistTyp2;
         Dow strSelDistT <> ' ';
            pcrSPCritVal = ParseString(strSelDistT:',':1:'1');
            write psp0crt;
         Enddo;

         //*** Beg Add *** 03/04/24 *******************************************
         // Clear then add old Staff Roles
         Setll (SPtype:conStaf) ps10crt;
         Reade (SPtype:conStaf) ps10crt;
         Dow not %eof(STSPPCRT1);
            Delete ps10crt;
            Reade (SPtype:conStaf) ps10crt;
         Enddo;
         strSelStafR = scStfRoles;

         // write record for staff selection
         pcr1S1type = SPtype;
         pcr1S1stat = 'A';
         pcr1S1critid = %trim('Staff Roles');
         pcr1S1pgm  = q1Pgm;
         pcr1S1user = q1User;
         pcr1S1time = Timeto6(SystemTime());
         pcr1S1dt   = Dateto7(SystemDate());
         pcr1S1iPgm = q1Pgm;
         pcr1S1iUsr = q1User;
         pcr1S1iTim = Timeto6(SystemTime());
         pcr1S1iDt  = Dateto7(SystemDate());
         Dow strSelStafR <> ' ';
            pcr1S1CritVal = ParseString(strSelStafR:',':1:'1');
            write ps10crt;
         Enddo;

         //*** End Add *** 03/04/24 *******************************************
         SPPKey   = scSPrint;
         SPPKeyM  = scSPrintM;
         SPOverP  = scSOverP;
         SPOverPM = scSOverPM;

         SPOutS = scSCopyS;
         SPOutM = scSCopyM;
         SPTray   = scSTray;
         SPTrayM  = scSTrayM;

         SPAllowD = scSAllowD;
         SPAllowP = scSAllowP;
         SPAllowC = scSAllowC;
         SPAllowE = scSAllowE;

         SPuser   = q1user;
         SPpgm    = q1pgm;
         SPdt     = Dateto7(SystemDate());
         SPtime   = Timeto6(SystemTime());

         If %found(STSPP);
            Update psp0p;
         Endif;

       Endsr;
       //===================================================================
       // Load Outut Control record
       //===================================================================
       BegSr LoadOutputCtl;

         Chain(n) xxRRN psp0p;

         scOutType = %trim(SPType);
         sc3DtQS  = SP3DtQS;
         sc3DtQR  = SP3DtQR;
         sc3XmlF  = SP3XmlF;
         xxTBID = 'DOCS';
         xxKey  = SPCat;
         //*** Beg Add ***** 05/18/23 ********************************
         visEmlTok = *On;
         //*** End Add ***** 05/18/23 ********************************
         //*** Beg Add *** 01/10/24 *** WC2670 **********************
         scCatagory = SPCat;
         //*** End Add *** 01/10/24 *** WC2670 **********************
         Chain(n) keySTGTL pgt00;
         If %found;
            scOutCat  = %trim(GTDesc);
         Else;
            scOutCat  = %trim(SPCat);
         Endif;

         Clear dsEmail;
         //*** Beg Chg ***** 05/25/23 ********************************
         //***str100 = scOutCat;
         str100 = SPCat;
         //*** End Chg ***** 05/25/23 ********************************
         //*** Beg Chg ***** 05/18/23 ********************************
         //***scTokenCrt = 'job = ''' + %char(q1JobN) + '''';
         //*** Beg Chg ***** 09/10/24 *********************************
         //*** scTokenCr1 = 'job = ' + %char(q1JobN) +
         //***  ' and Run_Type = '' ''';
         FltEmailToken('OutControl');
         //*** End Chg ***** 09/10/24 *********************************
         scTokCrtA = 'job = ' + %char(q1JobN) +
           ' and Run_Type = ''A''';
         strRunType2 = 'A';
         //*** End Chg ***** 05/18/23 ********************************
         ptrEmail = %addr(dsEmail);
         //*** Beg Chg ***** 05/18/23 ********************************
         //***GetTokensEmail(ptrEmail:str100);
         GetTokensEmail(ptrEmail:str100:' ':strRunType2);
         //*** End Chg ***** 05/18/23 ********************************
         ptrEmail = %addr(dsEmail);
         scEmlSub2 = SPEmlSub;
         scEmlBody2 = SPEmlBody;
         scEmlUsro2 = SPEmlUsro;
         //** Beg Add  ***  09/11/23 WC2907 ************************
         scFromEml = SPFemail;
         //** End Add  ***  09/11/23 WC2907 ************************
       //** Beg Add  ***  01/16/24 *** 153WAD ********************
            scCopyE = SPCopyE;
       //** End Add  ***  01/16/24 *** 153WAD ********************
         //** Beg Add  ***  11/26/22 WC2838 ************************

         If SPEmlSub <> ' ';
            If SPMass = 'Y' and SPEmlSubM = ' ';
               scEmlSubM  = SPEmlSub   ;
            EndIf;
            If SPEmlSubF = ' ';
               scEmlSubF  = SPEmlSub   ;
            EndIf;
         EndIf;
         If SPEmlBody <> ' ';
            If SPMass = 'Y' and SPEmlBodyM = ' ';
               scEmlBodyM = SPEmlBody  ;
            EndIf;
            If SPEmlBodyF = ' ';
               scEmlBodyF = SPEmlBody  ;
            EndIf;
         EndIf;
         scEmlSubM  = %Trim(SPEmlSubM)  ;
         scEmlBodyM = %Trim(SPEmlBodyM) ;
         scEmlSubF  = %Trim(SPEmlSubF)  ;
         scEmlBodyF = %Trim(SPEmlBodyF) ;

         scSPrintMI = SPPKeyI  ;
         scSTrayMI  = SPTrayI  ;
         scSOverPMI = SPOverPI ;
         scSPrintMA = SPPKeyA  ;
         scSTrayMA  = SPTrayA  ;
         scSOverPMA = SPOverPA ;

         If SPFnPreFix <> ' ';
            scFilePref = %Trim(SPFnPreFix);
         EndIf;

         scSFileNm = %Trim(SPFnData);

         //** End Add  ***  11/26/22 WC2838 ************************
         //** Beg Add  ***  07/25/23 WC2850 ************************
         If SPCpyS = 0;
            scCopiesS = 1;
         Else;
            scCopiesS = SPCpyS;
         EndIf;
         If SPCpyM = 0;
            scCopiesM = 1;
         Else;
            scCopiesM = SPCpyM;
         EndIf;
         //** End Add  ***  07/25/23 WC2850 ************************

         Clear pcrSPCritVal;
         Clear scDisttyp2;
         Setll (SPtype:conDist) psp0crt;
         Dou %eof(STSPPCRT);
         Reade(n) (SPtype:conDist) psp0crt;
            If scDisttyp2 <> *blank;
               scDisttyp2 = %trim(scDisttyp2) + ',';
            Endif;
            scDistTyp2 = %trim(scDistTyp2) + pcrSPCritVal;
         Enddo;

         //*** Beg Add *** 03/04/24 *******************************************
         Clear pcr1S1CritVal;
         Clear scStfRoles;
         Setll (SPtype:conStaf) ps10crt;
         Reade(n) (SPtype:conStaf) ps10crt;
         Dow not %eof(STSPPCRT1);
            If scStfRoles <> *blank;
               scStfRoles = %trim(scStfRoles) + ',';
            Endif;
            scStfRoles = %trim(scStfRoles) + pcr1S1CritVal;
            Reade(n) (SPtype:conStaf) ps10crt;
         Enddo;

         //*** End Add *** 03/04/24 *******************************************
         If SCFlg3 = 'Y';
            disbPrint = *off;
            Clear tipPrinter;
         Else;
            disbPrint = *on;
            tipPrinter = 'The print module for Scriptura must be enabled to +
             access this field.';
         Endif;
         If SCFlg2 = 'Y';
            disbEmail = *off;
            Clear tipEmail;
         Else;
            disbEmail = *on;
            tipEmail = 'The email module for Scriptura must be enabled to +
             access this field.';
         Endif;

         If SPInter = 'Y';
            scReplHead = 'LiveDoc Information';
            scReplCols = 'Effective Date,Report,Default LiveDoc ID, Client '+
             'Live DocID,Status, ';
         Else;
            scReplHead = 'Template Information';
            scReplCols = 'Effective Date,Report,Default Template,Client '+
             'Template,Status, ';
         Endif;

         //*** Beg Add ***** 03/22/24 *************************************
         If SPCat = 'PS';
            scLtrCols = 'Group,Carrier #,Effective Date,Type,' +
             'New Business Template,Renewal Business Template,Status, ';
         Else;
            scLtrCols = 'Group,Carrier #,Effective Date,Type,' +
             'Letter Template, ,Status, ';
         Endif;
         //*** End Add ***** 03/22/24 *************************************

         If SPHideRep = 'Y';
            showRepl = *off;
            topOffset = -185;
            topOffset = -170;
            //***** Beg Add *** 08/21/23 *****************************
            If sc3XML = 'N';
               vis3rdXml = *off;
               topOffset = -280;
            Else;
               vis3rdXml = *on;
            Endif;
            //***** End Add *** 08/21/23 *****************************
         Else;
            showRepl = *on;
            topOffset = 0;
            //***** Beg Add *** 08/21/23 *****************************
            If sc3XML = 'N';
               vis3rdXml = *off;
               topOffset = -110;
            Else;
               vis3rdXml = *on;
            Endif;
            //***** End Add *** 08/21/23 *****************************
         Endif;
       //** Beg Chg  ***  11/26/22 WC2838 ************************
       //xmlTop     =  525 + topOffset;
       //logoHdgTop =  645 + topOffset;
       //noLogoTop  =  650 + topOffset;
       //logoGrdTop =  675 + topOffset;
       //logoAddTop =  820 + topOffset;
       //sigHdgTop  =  850 + topOffset;
       //sigGrdTop  =  880 + topOffset;
       //sigAddTop  = 1025 + topOffset;
       //cancTop    = 1690 + topOffset;
       //saveTop    = 1690 + topOffset;
       //topEmail = 1255 + topOffset;
       //hgtOutput = 1570 + topOffset;
       //LtrHdgTop  = 1055 + topOffset;
       //LtrGrdTop  = 1085 + topOffset;
       //LtrAddTop  = 1230 + topOffset;

       //***** Beg Chg *** 08/21/23 *****************************
       //xmlTop     =  705 + topOffset;
         If vis3rdXml = *off;
            xmlTop =  0;
         Else;
            xmlTop =  705 + topOffset;
         Endif;
       //***** End Chg *** 08/21/23 *****************************
         logoHdgTop =  825 + topOffset;
         noLogoTop  =  830 + topOffset;
         logoGrdTop =  855 + topOffset;
         logoAddTop = 1000 + topOffset;
         sigHdgTop  = 1030 + topOffset;
         sigGrdTop  = 1060 + topOffset;
         sigAddTop  = 1205 + topOffset;
         //** Beg Chg  ***  09/24/24 *****************************
         //*** cancTop    = 1870 + topOffset;
         //*** saveTop    = 1870 + topOffset;
         cancTop    = 1940 + topOffset;
         saveTop    = 1940 + topOffset;
         //** End Chg  ***  09/24/24 *****************************
         topEmail = 1435 + topOffset;
         //** Beg Chg  ***  09/24/24 *****************************
         //*** hgtOutput = 1750 + topOffset;
         hgtOutput = 1820 + topOffset;
         //** End Chg  ***  09/24/24 *****************************
         LtrHdgTop  = 1235 + topOffset;
         LtrGrdTop  = 1265 + topOffset;
         LtrAddTop  = 1410 + topOffset;

       //** End Chg  ***  11/26/22 WC2838 ************************

         If SPSingle = 'Y';
            isSSingle = *on;
       //** Beg Add  ***  11/26/22 WC2838 ************************
            ssSEmail  = 'S';
       //** End Add  ***  11/26/22 WC2838 ************************
       //** Beg Add  ***  01/16/24 *** 153WAD ********************
            scCopyE = SPCopyE;
       //** End Add  ***  01/16/24 *** 153WAD ********************
         Else;
            isSSingle = *off;
         Endif;

         If SPMass = 'Y';
            isSMass = *on;
       //** Beg Add  ***  11/26/22 WC2838 ************************
            If SPSingle <> 'Y';
               ssSEmail  = 'M';
            EndIf;
       //** End Add  ***  11/26/22 WC2838 ************************
         Else;
            isSMass = *off;
         Endif;

         scSPrint  = SPPKey;
         scSPrintM = SPPKeyM;
         scSOverP  = SPOverP;
         scSOverPM = SPOverPM;

         scFormType = SPForm;
         scOutHold = SPHold;
         scOutSave = SPSave;
         scNoLogo = SPNoLogo;

         scSCopyS  = SPOutS;
         scSCopyM  = SPOutM;
         scSTray   = SPTray;
         scSTrayM  = SPTrayM;

         scSAllowD = SPAllowD;
         scSAllowP = SPAllowP;
         scSAllowC = SPAllowC;
         scSAllowE = SPAllowE;
         If SPSetup <> ' ';
            cfgSaveTxt = 'Next';
         Else;
            cfgSaveTxt = 'Save';
         Endif;

         //** Beg Add  ***  11/26/22 WC2838 ************************
         // Load Selection Box for File Name based on Category

         If %Trim(SPCat) = 'PS';
            isValCat = *On;
            scFlNmChoi  =  '     ,File Info,Carrier Policy,DBA Name';
            scFlNmVal   =  '     ,File Info,Carrier Policy,DBA Name';
            If isSMass = *On;
               isSMassIns = *On;
               isSMassAgt = *On;
            EndIf;
         Else;
          isSMassIns = *Off;
          isSMassAgt = *Off;
         EndIf;

         If %Trim(SPCat) = 'AS';
            isValCat = *On;
            scFlNmChoi  =  '     ,Fein/Mod,Agency Name';
            scFlNmVal   =  '     ,Fein/Mod,Agency Name';
         EndIf;

         If %Trim(SPCat) = 'CS' Or %Trim(SPCat) = 'MC';
            isValCat = *On;
            scFlNmChoi  =  '     ,Claim #,Accident Date/Last/first';
            scFlNmVal   =  '     ,Claim #,Accident Date/Last/first';
         EndIf;

         If %Trim(SPCat) = '  ' Or %Trim(SPCat) = 'RP';
            isValCat = *Off;
         EndIf;

         //** End Add  ***  11/26/22 WC2838 ************************

          Setll (scOutType) psp11l;
          Reade(n) (scOutType) psp11l;
          If %eof;

             Clear psp11p;
             S1Type    = scOutType;
             S1Efdt    = 990101;
             S1SDefT   = SPSDefT;
             S1PathD   = SPPathD;
             S1LangD   = SPLangD;
             S1SCltT   = SPSCltT;
             S1PathC   = SPPathC;
             S1LangC   = SPLangC;
             S1Sig     = SPSig;
             S1SigFold = SPSigFold;
             S1Stat    = 'A';
             S1IUsr    = Q1User;
             S1IDt     = Dateto7(SystemDate());
             S1Pgm     = Q1Pgm;
             S1User    = Q1User;
             S1Time    = Timeto6(SystemTime());
             S1Dt      = Dateto7(SystemDate());
             Write psp11p;

          Endif;

         Exsr LoadReplaceGrid;

       Endsr;
       //===================================================================
       // Load the replacement doc grid.
       //===================================================================
       BegSr LoadReplaceGrid;

       // Clear the subfile.
       dspRepl = *off;
       dspReplCtl = *off;
       clrRepl = *on;
       dspLogo = *off;
       dspLogoCtl = *off;
       clrLogo = *on;
       dspSig = *off;
       dspSigCtl = *off;
       clrSig = *on;
       dspLtr = *off;
       dspLtrCtl = *off;
       clrLtr = *on;
       //***** Beg Add *** 08/21/23 *********************************
       OutRepVis = *off;
       //***** End Add *** 08/21/23 *********************************
       Write rcdOutCfg;
       clrRepl = *off;
       dspRepl = *on;
       dspReplCtl = *on;
       rrnRepl = 0;
       dspLogo = *on;
       dspLogoCtl = *on;
       clrLogo = *off;
       rrnLogo = 0;
       dspSig = *on;
       dspSigCtl = *on;
       clrSig = *off;
       rrnSig = 0;
       dspLtr = *on;
       dspLtrCtl = *on;
       clrLtr = *off;
       rrnLtr = 0;

       // Load the replacement doc subfile.

       Setgt (scOutType) psp11l;
       Readpe(n) (scOutType) psp11l;

       Dow not %eof;

          outSRDate  = dtMDYY(S1Efdt);
          If SPInter = 'Y';
             outSRDef = %trim(S1SDefT);
             outSRClt = %trim(S1SCltT);
          //*** begin add ***  09/25/24  *********************************
             xxBegPos = %scan('.':S1SDefT);
             If xxBegPos <> 0;
                outSRDef = %trim(%subst(S1SDefT:1:xxBegPos-1));
             Endif;
             xxBegPos = %scan('.':S1SCltT);
             If xxBegPos <> 0;
                outSRClt = %trim(%subst(S1SCltT:1:xxBegPos-1));
             Endif;
          //*** end add *****  09/25/24  *********************************
          Else;
             outSRDef = %trim(S1PathD) + %trim(S1SDefT);
             outSRClt = %trim(S1PathC) + %trim(S1SCltT);
          Endif;

          If S1Stat = 'A';
             outSRStat = 'Active';
          Else;
             outSRStat = 'Inactive';
          Endif;
          outReport = ' ';
          //***** Beg Add *** 08/21/23 *******************************
          If outReport = ' ';
             OutRepVis = *off;
          Endif;
          //***** End Add *** 08/21/23 *******************************

          outReplRRN  = dbrrnB;

          rrnRepl = rrnRepl + 1;

          Write sflReplace;

          Readpe(n) (scOutType) psp11l;
       Enddo;
      //* Load replacement grid with report template records.
       hasReport = *off;
       reportType = %trim(scOutType) + ' Report';

       Setgt (reportType) psp11l;
       Readpe(n) (reportType) psp11l;

       Dow not %eof;

          hasReport = *on;
          outSRDate  = dtMDYY(S1Efdt);
          If SPInter = 'Y';
             outSRDef = %trim(S1SDefT);
             outSRClt = %trim(S1SCltT);
          Else;
             outSRDef = %trim(S1PathD) + %trim(S1SDefT);
             outSRClt = %trim(S1PathC) + %trim(S1SCltT);
          Endif;

          If S1Stat = 'A';
             outSRStat = 'Active';
          Else;
             outSRStat = 'Inactive';
          Endif;
          outReport = 'Y';
          //***** Beg Add *** 08/21/23 *******************************
          If outReport = 'Y';
             OutRepVis = *on;
          Endif;
          //***** End Add *** 08/21/23 *******************************

          outReplRRN  = dbrrnB;

          rrnRepl = rrnRepl + 1;

          Write sflReplace;

          Readpe(n) (reportType) psp11l;
       Enddo;

       rrnRepl = 1;

       // Load the Logo subfile.

       Setgt (scOutType) psp12l;
       Readpe(n) (scOutType) psp12l;

       Dow not %eof;

          If S2Fnd = 0;
             outLGroup = 'All';
          //***** Beg Chg *** 08/21/23 ***************************
          //***Else;
          //***   outLGroup = %editc(S2Fnd : 'X');
             outLCarr = ' ';
          Else;
             outLGroup = %editc(S2Fnd : 'X');
             outLCarr = %editc(GetCarrier#(S2Fnd) : 'X');
          //***** End Chg *** 08/21/23 ***************************
          Endif;

          outLDate   = dtMDYY(S2Efdt);
          outLFileNm = %trim(S2Logo);
          outLPath   = %trim(S2Path);
          //*** begin add ***  09/25/24  *************************************
          If SCFlg6 = 'Y';
             outLPath  = %trim(S2URL);
          Endif;
          //*** end add *****  09/25/24  *************************************

          If S2Stat = 'A';
             outLStat = 'Active';
          Else;
             outLStat = 'Inactive';
          Endif;

          outLRRN  = dbrrnC;

          rrnLogo = rrnLogo + 1;

          Write sflLogos;

          Readpe(n) (scOutType) psp12l;
       Enddo;

       rrnLogo = 1;

       // Load the Signature subfile.

       Setgt (scOutType) psp13l;
       Readpe(n) (scOutType) psp13l;

       Dow not %eof;

          If S3Fnd = 0;
             outSGroup = 'All';
          //***** Beg Chg *** 08/21/23 ***************************
          //***Else;
          //***   outSGroup = %editc(S3Fnd : 'X');
             outSCarr = ' ';
          Else;
             outSGroup = %editc(S3Fnd : 'X');
             outSCarr = %editc(GetCarrier#(S3Fnd) : 'X');
          //***** End Chg *** 08/21/23 ***************************
          Endif;

          outSDate   = dtMDYY(S3Efdt);
          outSFile = %trim(S3Path) + %trim(S3Sig);
          outSFile2 = %trim(S3Path2) + %trim(S3Sig2);

          If S3Stat = 'A';
             outSStat = 'Active';
          Else;
             outSStat = 'Inactive';
          Endif;

          outSRRN  = dbrrnD;

          rrnSig = rrnSig + 1;

          Write sflSignat;

          Readpe(n) (scOutType) psp13l;
       Enddo;

       rrnSig = 1;

       // Load Letter Template subfile.

       Setgt (scOutType) psp14l;
       Readpe(n) (scOutType) psp14l;

       Dow not %eof;

          If S4Fnd = 0;
             outLTGroup = 'All';
          //***** Beg Chg *** 08/21/23 ***************************
          //***Else;
          //***   outLTGroup = %editc(S4Fnd : 'X');
             outLTCarr = ' ';
          Else;
             outLTGroup = %editc(S4Fnd : 'X');
             outLTCarr = %editc(GetCarrier#(S4Fnd) : 'X');
          //***** End Chg *** 08/21/23 ***************************
          Endif;

          outLTDate   = dtMDYY(S4Efdt);
          outLPathNm  =  %trim(S4Path) + %trim(S4Doc);
          //*** Beg Add *** 01/10/24 *** WC2670 **********************
          outLPthNmR  =  %trim(S4RPath) + %trim(S4RDoc);
          //*** End Add *** 01/10/24 *** WC2670 **********************


          If S4Stat = 'A';
             outLTStat = 'Active';
          Else;
             outLTStat = 'Inactive';
          Endif;

          Clear outLType;
          Select;
          When S4LType = 'C';
             outLType = 'Cover Letter';
          When S4LType = 'N';
             outLType = 'New/Renewal Letter';
          Endsl;

          outLTRRN  = dbrrnE;

          rrnLtr = rrnLtr + 1;

          Write sflLtrTmpl;

          Readpe(n) (scOutType) psp14l;
       Enddo;

       rrnLtr = 1;

       Endsr;
       //*** Beg Del *** 07/30/24 *******************************************
       //*** Beg Add *** 03/04/24 *******************************************
       //===================================================================
       // Set Staff Multi select box choices
       //===================================================================
       //*** Begsr SetStaffCH;
       //***  scStaffCH  = *Blanks;
       //***  scStaffVal = *Blanks;
       //***  If scCat = 'Policy Services';
       //***     scStaffCH  = 'Audit Manager,Audit Reviewer,Claim Contact,+
       //***         Policy Services,Premium Accountant,Underwriter';
       //***     scStaffVal = 'Audit Manager,Audit Reviewer,Claim Contact,+
       //***         Policy Services,Premium Accountant,Underwriter';
       //***     strSqlCursor = 'Select Distinct EXahlc from WMEXP  +
       //***                     where EXstat <> ''I'' and EXahlc <> '' '' +
       //***                     ORDER BY EXahlc ASC';
       //***  Endif;
       //***  If scCat = 'Claims Services';
       //***     scStaffCH  = 'Account Manager,Case Nurse,Claim Manager,+
       //***         Outside Adjuster,Reinsurance Adjuster,+
       //***         Special Injury/Disability Fund Adjuster,+
       //***         Supervisor';
       //***     scStaffVal = 'Account Manager,Case Nurse,Claim Manager,+
       //***         Outside Adjuster,Reinsurance Adjuster,+
       //***         Special Injury/Disability Fund Adjuster,+
       //***         Supervisor';
       //***     strSqlCursor = 'Select Distinct CXadj from WMCXP  +
       //***                     where CXstat <> ''I'' and CXadj <> '' '' +
       //***                     ORDER BY CXadj ASC';
       //***  Endif;
       //***  If scCat = 'Agency Services';
       //***     scStaffCH  = 'Agent';
       //***     scStaffVal = 'Agent';
       //***  Endif;
       //***  If scCat = 'Policy Services' or scCat = 'Claims Services';
       //***     Exec Sql Prepare sqlDB2 From :strSqlCursor;
       //***     Exec Sql Declare StaffCur Cursor For sqlDB2;
       //***     Exec Sql
       //***      Open StaffCur;
       //***     count1 = 1;
       //***     Dow sqlStt = '00000';
       //***        Exec Sql
       //***         Fetch Next
       //***         From StaffCur
       //***         Into :strStaffAsn;
       //***        If sqlStt = '02000' and count1 = 1;
       //***           If scCat = 'Policy Services';
       //***              scStaffCH  = %trim(scStaffCH) + ',Loss Control Rep';
       //***              scStaffVal = %trim(scStaffVal) + ',Loss Control Rep';
       //***           Endif;
       //***           If scCat = 'Claims Services';
       //***              scStaffCH  = %trim(scStaffCH) + ',Adjuster';
       //***              scStaffVal = %trim(scStaffVal) + ',Adjuster';
       //***           Endif;
       //***           Leave;
       //***        Endif;
       //***        If sqlStt = '00000';
       //***           count1 = count1 + 1;
       //***           scStaffCH  = %trim(scStaffCH) + ',' + %trim(strStaffAsn);
       //***           scStaffVal = %trim(scStaffVal) + ',' + %trim(strStaffAsn);
       //***        Endif;
       //***     Enddo;
       //***     Exec Sql
       //***      Close StaffCur;
       //***  Endif;
       //*** Endsr;
       //*** End Add *** 03/04/24 *******************************************
       //*** End Del *** 07/30/24 *******************************************
       //===================================================================
      /end-free
     p CfgOutputCtl    e
       //===================================================================
       // Load Visit Types Grid.
       //===================================================================
     p LoadVisit       b
     d LoadVisit       pi

     dstrSqlCur        s           1000
     ddsWTVIPCFG       ds
     D VICat                         10
     D VIcfgid                       25
     D VIcfgval                      50

      /free
       // Clear the subfile.
       dspVst = *off;
       dspVstCtl = *off;
       clrVst = *on;
       Write rcdVstTrk;
       clrVst = *off;
       dspVst = *on;
       dspVstCtl = *on;
       rrnVst = 0;

       // Load the Visit Type subfile.
       Setll intCo# pvi01;
       Reade intCo# pvi01;

       Dow not %eof;

          visitFilter = 'Include';
          If (fltrVstTyp <> ' ');
          //*** Beg Chg ***** 02/24/23 ************************
             // If (%scan(%xlate(lo:up:%trim(fltrVstTyp)):%xlate(lo:up:VItype))=0);
             If (%scan(%xlate(lo:up:%trim(fltrVstTyp)):
              %xlate(lo:up:L_VItype))=0);
          //*** End Chg ***** 02/24/23 ************************
                visitFilter = 'Exclude';
             Endif;
          Endif;
          //*** Beg Chg ***** 02/24/23 ************************
          // If ((fltrVstCat<>VIcat) and (fltrVstCat<>' '));
          If ((fltrVstCat<>L_VIcat) and (fltrVstCat<>' '));
          //*** End Chg ***** 02/24/23 ************************
             visitFilter = 'Exclude';
          Endif;
          //*** Beg Chg ***** 02/24/23 ************************
          // If ((fltrVstSts<>VIstat) and (fltrVstSts<>'*') and (fltrVstSts<>' '));
          If ((fltrVstSts<>L_VIstat) and (fltrVstSts<>'*') and
           (fltrVstSts<>' '));
          //*** End Chg ***** 02/24/23 ************************
             visitFilter = 'Exclude';
          Endif;

          If (visitFilter = 'Include');
             //*** Beg Chg ***** 02/24/23 ************************
             // OutVstTyp  = VItype;
             // OutVstCat  = VIcat;
             OutVstTyp  = L_VItype;
             OutVstCat  = L_VIcat;
             //*** End Chg ***** 02/24/23 ************************
             Select;
             //*** Beg Chg ***** 02/24/23 ************************
             // When VIstat = 'A';
             When L_VIstat = 'A';
             //*** End Chg ***** 02/24/23 ************************
                OutVstSts  = 'Active';
             //*** Beg Chg ***** 02/24/23 ************************
             // When VIstat = 'I';
             When L_VIstat = 'I';
             //*** End Chg ***** 02/24/23 ************************
                OutVstSts  = 'Inactive';
             Other;
                OutVstSts  = *blanks;
             Endsl;
             OutVstRRN  = dbrrn4;

             rrnVst = rrnVst + 1;
             Write sflVstTrk;
          Endif;

          Reade intCo# pvi01;
       Enddo;

       rrnVst = 1;

       strSqlCur = 'select VIcat,VIcfgid,VIcfgval from wtvipcfg ' +
        'where VICO# = ' + %char(intCo#) + ' and VIstat = ''A''';
       Exec SQL Prepare sqlDB1 From :strSqlCur;
       Exec SQL Declare cursorDB1 Cursor For sqlDB1;
       Exec SQL Open cursorDB1;
       Dou SQLSTT <> '00000';
          Exec SQL Fetch next From cursorDB1 into :dsWTVIPCFG;
          Select;
          When SQLSTT <> '00000';
             Iter;
          Endsl;
          If (VIcat = 'Policy') and (VIcfgId = 'FilterVisitStatus');
             If scPolicy = ' ';
                scPolicy = VIcfgVal;
             Else;
                scPolicy = %trim(scPolicy) + ',' +
                 %trim(VIcfgVal);
             Endif;
          Endif;
          If (VIcat = 'Claims' and VIcfgId = 'FilterVisitStatus');
             If scClm = ' ';
                scClm = VIcfgVal;
             Else;
                scClm = %trim(scClm) + ',' + %trim(VIcfgVal);
             Endif;
          Endif;
          If (VIcat = 'Agent' and VIcfgId = 'FilterVisitStatus');
             If scAgt = ' ';
                scAgt = VIcfgVal;
             Else;
                scAgt = %trim(scAgt) + ',' + %trim(VIcfgVal);
             Endif;
          Endif;
          If (VIcat = ' ' and VIcfgId = 'FilterVisitStatus');
             If scOthr = ' ';
                scOthr = VIcfgVal;
             Else;
                scOthr = %trim(scOthr) + ',' + %trim(VIcfgVal);
             Endif;
          Endif;
       Enddo;
       Exec SQL Close cursorDB1;

      /end-free
     p LoadVisit       e
       //===================================================================
       // Load Visit Types Filter
       //===================================================================
     p SetVstFilter    b
     d SetVstFilter    pi
      /free
       scVsttyp = fltrVstTyp;
       scVstCat = fltrVstCat;
       scVstSts = 'A';
       Dou (vstFltrCx = *on) or (vstFltrSv = *on);
         If scVstCat <> ' ';
           selVstTyp = 'VIcat = ''' + %Trim(scVstCat) + '''';
         Else;
           selVstTyp = 'VIcat <> ''''';
         Endif;
         Exfmt rcdVstFltr;
         if (vstFltrSv = *on);
            fltrVsttyp = scVstTyp;
            fltrVstCat = scVstCat;
            fltrVstSts = scVstSts;
            LoadVisit();
         Endif;
       Enddo;
       vstTypFlt = *Off;
       showVstClr= *On;
      /end-free
     p SetVstFilter    e
       //===================================================================
       // Clear Visit Types Filter
       //===================================================================
     p ClearVstFilter  b
     d ClearVstFilter  pi
      /free
       fltrVsttyp = *blanks;
       fltrVstCat = *blanks;
       fltrVstSts = *blanks;
       scVsttyp = fltrVstTyp;
       scVstCat = fltrVstCat;
       scVstSts = fltrVstSts;
       LoadVisit();
       clrVstFlt = *Off;
       showVstClr= *Off;
      /end-free
     p ClearVstFilter  e
       //===================================================================
       // Configure (Add/Edit) Visit Types
       //===================================================================
     p CfgVisitType    b
     d CfgVisitType    pi
     D inVstRRN                       9  0 const options(*nopass)

     D xxRRN           s              9  0 inz(0)
     d error           s               n   inz(*off)
     d vstCfgMode      s             10    inz
      /free
       If %parms >= 1;
          xxRRN = inVstRRN;
       Endif;
       Clear rcdVstCfg;
       //*** Beg Add ***** 06/08/23 ************************
       If xxRRN <> 0 and isCopyMode = *on;
          vstCfgMode = 'Copy';
          vstCfgCpy = *on;
          vstCfgEdt = *off;
          Exsr LoadVisitCFG;
       Elseif xxRRN <> 0 and isCopyMode <> *on;
       //*** End Add ***** 06/08/23 ************************
       //*** Beg Del ***** 06/08/23 ************************
       //*** If xxRRN <> 0;
       //*** End Del ***** 06/08/23 ************************
          vstCfgMode = 'Edit';
          vstCfgEdt = *on;
          Exsr LoadVisitCFG;
       Else;
          vstCfgMode = 'Add';
          vstCfgEdt = *off;
          scVsttyp = *blanks;
          scVstCat = *blanks;
          scVstSts = 'A';
       Endif;
       vstCfgCx = *off;
       vstCfgSv = *off;
       //*** Beg Add ***** 06/08/23 ************************
       isCopyMode = *off;
       //*** End Add ***** 06/08/23 ************************

       Exsr ClearErrors;
       Dou (vstCfgCx = *on) or (vstCfgSv = *on);
         Exfmt rcdVstCfg;
         If (vstCfgSv = *on);
            Exsr ClearErrors;
            Exsr CfgVstCheck;
            If not error;
               exsr SaveVisitCfg;
               LoadVisit();
            Else;
               vstCfgSv = *off;
            Endif;
         Endif;
       Enddo;
       //===================================================================
       // Clear the error fields for Add Visit Type Window
       //===================================================================
       BegSr ClearErrors;

       error = *off;
       Clear erVstTyp;
       Clear msVstTyp;

       Endsr;
       //===================================================================
       // Edit the Visit Type Configuration fields.
       //===================================================================
       BegSr CfgVstCheck;

       If scVstTyp = ' ';
          msVstTyp = 'Visit Type / Description cannot be blank.';
          erVstTyp = *on;
          error = *on;
          Leavesr;
       Endif;

       If scVstCat = ' ';
          msVstCat = 'Visit Category cannot be blank.';
          erVstCat = *on;
          error = *on;
          Leavesr;
       Endif;

       If scVstSts = ' ';
          msVstSts = 'Visit Status cannot be blank.';
          erVstSts = *on;
          error = *on;
          Leavesr;
       Endif;

       //*** Beg Chg ***** 06/08/23 ************************
       //*** If vstCFGmode = 'Add';
       If vstCFGmode = 'Add' or vstCFGmode = 'Copy';
       //*** End Chg ***** 06/08/23 ************************
          Chain(n) (intCo#:scVsttyp) pvi01;
          If %found;
             msVstTyp = 'This Visit Type record already exists.';
             erVstTyp = *on;
             error = *on;
          Endif;
       Endif;

       Endsr;
       //===================================================================
       // Save the Visit Type Configuration record.
       //===================================================================
       BegSr SaveVisitCfg;

       //*** Beg Chg ***** 06/08/23 ************************
       //*** If vstCfgMode = 'Add';
       If vstCfgMode = 'Add' or vstCfgMode = 'Copy';
       //*** End Chg ***** 06/08/23 ************************
          Clear pvi0p;
          VIco#    = intCo#;
          VItype   = scVstTyp;
          VIcat    = scVstCat;
          //*** Beg Chg ***** 06/08/23 ************************
          //*** VIuser   = Q1USER;
          //*** VIpgm    = Q1PGM;
          //*** VIdt     = Dateto7(SystemDate());
          //*** VItime   = Timeto6(SystemTime());
          VIiusr   = Q1USER;
          VIipgm   = Q1PGM;
          VIidt    = Dateto7(SystemDate());
          VIitim   = Timeto6(SystemTime());
          //*** End Chg ***** 06/08/23 ************************
       Else;
          Chain xxRRN pvi0p;
       Endif;
          VIdtHd1  = scDte01;
          VIdtHd2  = scDte02;
          VIdtHd3  = scDte03;
          VIdtHd4  = scDte04;
          VIdtHd5  = scDte05;
          VIdtHd6  = scDte06;
          VIdtHd7  = scDte07;
          VIdtHd8  = scDte08;
          VIdtHd9  = scDte09;
          VIdtHd10 = scDte10;
          VIdtRq1  = scDte01Req;
          VIdtRq2  = scDte02Req;
          VIdtRq3  = scDte03Req;
          VIdtRq4  = scDte04Req;
          VIdtRq5  = scDte05Req;
          VIdtRq6  = scDte06Req;
          VIdtRq7  = scDte07Req;
          VIdtRq8  = scDte08Req;
          VIdtRq9  = scDte09Req;
          VIdtRq10 = scDte10Req;
          VIcbHd1  = scChkbx01;
          VIcbHd2  = scChkbx02;
          VIcbHd3  = scChkbx03;
          VIcbHd4  = scChkbx04;
          VIcbHd5  = scChkbx05;
          VIcbHd6  = scChkbx06;
          VIcbHd7  = scChkbx07;
          VIcbHd8  = scChkbx08;
          VIcbHd9  = scChkbx09;
          VIcbHd10 = scChkbx10;
          VIinHd1  = scItg01;
          VIinHd2  = scItg02;
          VIinHd3  = scItg03;
          VIinHd4  = scItg04;
          VIinHd5  = scItg05;
          VIinHd6  = scItg06;
          VIinHd7  = scItg07;
          VIinHd8  = scItg08;
          VIinHd9  = scItg09;
          VIinHd10 = scItg10;
          VIinRq1  = scItg01Req;
          VIinRq2  = scItg02Req;
          VIinRq3  = scItg03Req;
          VIinRq4  = scItg04Req;
          VIinRq5  = scItg05Req;
          VIinRq6  = scItg06Req;
          VIinRq7  = scItg07Req;
          VIinRq8  = scItg08Req;
          VIinRq9  = scItg09Req;
          VIinRq10 = scItg10Req;
          VIdcHd1  = scDec01;
          VIdcHd2  = scDec02;
          VIdcHd3  = scDec03;
          VIdcHd4  = scDec04;
          VIdcHd5  = scDec05;
          VIdcHd6  = scDec06;
          VIdcHd7  = scDec07;
          VIdcHd8  = scDec08;
          VIdcHd9  = scDec09;
          VIdcHd10 = scDec10;
          VIdcRq1  = scDec01Req;
          VIdcRq2  = scDec02Req;
          VIdcRq3  = scDec03Req;
          VIdcRq4  = scDec04Req;
          VIdcRq5  = scDec05Req;
          VIdcRq6  = scDec06Req;
          VIdcRq7  = scDec07Req;
          VIdcRq8  = scDec08Req;
          VIdcRq9  = scDec09Req;
          VIdcRq10 = scDec10Req;
          VItfHd1  = scTxt01;
          VItfHd2  = scTxt02;
          VItfHd3  = scTxt03;
          VItfHd4  = scTxt04;
          VItfHd5  = scTxt05;
          VItfHd6  = scTxt06;
          VItfHd7  = scTxt07;
          VItfHd8  = scTxt08;
          VItfHd9  = scTxt09;
          VItfHd10 = scTxt10;
          VItfRq1  = scTxt01Req;
          VItfRq2  = scTxt02Req;
          VItfRq3  = scTxt03Req;
          VItfRq4  = scTxt04Req;
          VItfRq5  = scTxt05Req;
          VItfRq6  = scTxt06Req;
          VItfRq7  = scTxt07Req;
          VItfRq8  = scTxt08Req;
          VItfRq9  = scTxt09Req;
          VItfRq10 = scTxt10Req;
          VItfTb6  = scTxttbl06;
          VItfTb7  = scTxttbl07;
          VItfTb8  = scTxttbl08;
          VItfTb9  = scTxttbl09;
          VItfTb10 = scTxttbl10;
          VIstat   = scVstSts;
          VIDupEntry  = scDupEntry;
          //*** Beg Chg ***** 06/08/23 ************************
          //*** VIIUSR   = Q1USER;
          //*** VIIPGM   = Q1PGM;
          //*** VIIDT    = Dateto7(SystemDate());
          //*** VIITIM   = Timeto6(SystemTime());
          VIuser   = Q1USER;
          VIpgm    = Q1PGM;
          VIdt     = Dateto7(SystemDate());
          VItime   = Timeto6(SystemTime());
          //*** End Chg ***** 06/08/23 ************************

       //*** Beg Chg ***** 06/08/23 ************************
       //*** If vstCfgMode = 'Add';
       If vstCfgMode = 'Add' or vstCfgMode = 'Copy';
       //*** End Chg ***** 06/08/23 ************************
          Write pvi0p;
       Else;
          Update pvi0p;
       Endif;

       Endsr;
       //===================================================================
       // Load Visit Type Configuration record.
       //===================================================================
       BegSr LoadVisitCfg;

          Chain(n) xxRRN pvi0p;

          scVstCat   = VIcat    ;
          //*** Beg Chg ***** 06/08/23 ************************
          //***scVstTyp   = VItype   ;
          If vstCfgMode = 'Copy';
          scVstTyp   = *blanks;
          Else;
          scVstTyp   = VItype   ;
          Endif;
          //*** End Chg ***** 06/08/23 ************************
          scDte01    = VIdtHd1  ;
          scDte02    = VIdtHd2  ;
          scDte03    = VIdtHd3  ;
          scDte04    = VIdtHd4  ;
          scDte05    = VIdtHd5  ;
          scDte06    = VIdtHd6  ;
          scDte07    = VIdtHd7  ;
          scDte08    = VIdtHd8  ;
          scDte09    = VIdtHd9  ;
          scDte10    = VIdtHd10 ;
          scDte01Req = VIdtRq1  ;
          scDte02Req = VIdtRq2  ;
          scDte03Req = VIdtRq3  ;
          scDte04Req = VIdtRq4  ;
          scDte05Req = VIdtRq5  ;
          scDte06Req = VIdtRq6  ;
          scDte07Req = VIdtRq7  ;
          scDte08Req = VIdtRq8  ;
          scDte09Req = VIdtRq9  ;
          scDte10Req = VIdtRq10 ;
          scChkbx01  = VIcbHd1  ;
          scChkbx02  = VIcbHd2  ;
          scChkbx03  = VIcbHd3  ;
          scChkbx04  = VIcbHd4  ;
          scChkbx05  = VIcbHd5  ;
          scChkbx06  = VIcbHd6  ;
          scChkbx07  = VIcbHd7  ;
          scChkbx08  = VIcbHd8  ;
          scChkbx09  = VIcbHd9  ;
          scChkbx10  = VIcbHd10 ;
          scItg01    = VIinHd1  ;
          scItg02    = VIinHd2  ;
          scItg03    = VIinHd3  ;
          scItg04    = VIinHd4  ;
          scItg05    = VIinHd5  ;
          scItg06    = VIinHd6  ;
          scItg07    = VIinHd7  ;
          scItg08    = VIinHd8  ;
          scItg09    = VIinHd9  ;
          scItg10    = VIinHd10 ;
          scItg01Req = VIinRq1  ;
          scItg02Req = VIinRq2  ;
          scItg03Req = VIinRq3  ;
          scItg04Req = VIinRq4  ;
          scItg05Req = VIinRq5  ;
          scItg06Req = VIinRq6  ;
          scItg07Req = VIinRq7  ;
          scItg08Req = VIinRq8  ;
          scItg09Req = VIinRq9  ;
          scItg10Req = VIinRq10 ;
          scDec01    = VIdcHd1  ;
          scDec02    = VIdcHd2  ;
          scDec03    = VIdcHd3  ;
          scDec04    = VIdcHd4  ;
          scDec05    = VIdcHd5  ;
          scDec06    = VIdcHd6  ;
          scDec07    = VIdcHd7  ;
          scDec08    = VIdcHd8  ;
          scDec09    = VIdcHd9  ;
          scDec10    = VIdcHd10 ;
          scDec01Req = VIdcRq1  ;
          scDec02Req = VIdcRq2  ;
          scDec03Req = VIdcRq3  ;
          scDec04Req = VIdcRq4  ;
          scDec05Req = VIdcRq5  ;
          scDec06Req = VIdcRq6  ;
          scDec07Req = VIdcRq7  ;
          scDec08Req = VIdcRq8  ;
          scDec09Req = VIdcRq9  ;
          scDec10Req = VIdcRq10 ;
          scTxt01    = VItfHd1  ;
          scTxt02    = VItfHd2  ;
          scTxt03    = VItfHd3  ;
          scTxt04    = VItfHd4  ;
          scTxt05    = VItfHd5  ;
          scTxt06    = VItfHd6  ;
          scTxt07    = VItfHd7  ;
          scTxt08    = VItfHd8  ;
          scTxt09    = VItfHd9  ;
          scTxt10    = VItfHd10 ;
          scTxt01Req = VItfRq1  ;
          scTxt02Req = VItfRq2  ;
          scTxt03Req = VItfRq3  ;
          scTxt04Req = VItfRq4  ;
          scTxt05Req = VItfRq5  ;
          scTxt06Req = VItfRq6  ;
          scTxt07Req = VItfRq7  ;
          scTxt08Req = VItfRq8  ;
          scTxt09Req = VItfRq9  ;
          scTxt10Req = VItfRq10 ;
          scTxttbl06 = VItfTb6  ;
          scTxttbl07 = VItfTb7  ;
          scTxttbl08 = VItfTb8  ;
          scTxttbl09 = VItfTb9  ;
          scTxttbl10 = VItfTb10 ;
          scVstSts   = VIstat   ;
          scDupEntry  = VIDupEntry;

       Endsr;
      /end-free
     p CfgVisitType    e
       //===================================================================
       // Replacement Doc setup file.
       //===================================================================
     p ReplaceWindow   b
     d ReplaceWindow   pi
     D inReplType                   100    const
     D inReplInter                    1    const
     D inReplRRN                      9  0 const options(*nopass)
      //*** Beg Add ***** 08/21/23 *********************************
     D inCopyMode                      n   const options(*nopass)
      //*** End Add ***** 08/21/23 *********************************

     D xxRRN           s              9  0 inz(0)
     D xxPathD         s            250
     D xxDocD          s             60
     d error           s               n   inz(*off)
     d replAddMode     s               n   inz(*off)
     D reportType      s                   like(S1Type)

      /free

       If %parms >= 3;
          xxRRN = inReplRRN;
       Endif;

       scCo# = intCo#;
       replAddMode = *off;
       replEdtMde = *off;
       replEdtMd1 = *off;
       visReport = hasReport;
       reportType = %trim(inReplType) + ' Report';

       If inReplInter = 'Y';
          scSRDocLbD = 'Default LiveDoc ID';
       Else;
          scSRDocLbD = 'Default Template ID';
       Endif;

       If xxRRN <> 0;
          Chain(n) xxRRN psp11p;
       //*** Beg Chg ***** 08/21/23 *********************************
       //***   replEdtMde = *on;
       //***Else;
       //***   replAddMode = *on;
       //***Endif;
          If inCopyMode = *on;
             replAddMode = *on;
             replEdtMd1 = *on;
             btnName = 'Proceed To Copy';
          Else;
             replEdtMde = *on;
             replEdtMd1 = *on;
             btnName = 'Save';
          Endif;
       Else;
          replAddMode = *on;
          btnName = 'Save';
       Endif;
       //*** End Chg ***** 08/21/23 *********************************

       If inReplInter = 'Y';
          scSRDocLbl = 'Client LiveDoc ID';
       Else;
          scSRDocLbl = 'Client Template ID';
       Endif;
       isNonInter = *off;
       If inReplInter <> 'Y';
          isNonInter = *on;
       Endif;

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou replAddMode = *off;

          //* Clear screen fields prior to display if in Add Mode.
          //*** Beg Chg ***** 08/21/23 *********************************
          //***If replAddMode = *on;
          If replAddMode = *on and inCopyMode <> *on;
          //*** End Chg ***** 08/21/23 *********************************
             Clear scSREfdt;
             Clear scSRCltNam;
             Clear scSRCltPth;
             Clear scSRDefNam;
             Clear scSRDefPth;
             Clear scReport;
             scSRStat = 'A';
          Else;
             scSREfdt   = dtMDYY(S1Efdt);
             scSRCltNam = S1SCltT;
             scSRCltPth = S1PathC;
             scSRStat   = S1Stat;
             scSRDefNam = S1SDefT;
             scSRDefPth = S1PathD;
          //*** begin add ***  09/25/24  *********************************
             xxBegPos = %scan('.':S1SDefT);
             If SCFlg6 = 'Y' and xxBegPos <> 0 and isNonInter = *off;
                scSRDefNam = %trim(%subst(S1SDefT:1:xxBegPos-1));
             Endif;
             xxBegPos = %scan('.':S1SCltT);
             If SCFlg6 = 'Y' and xxBegPos <> 0 and isNonInter = *off;
                scSRCltNam = %trim(%subst(S1SCltT:1:xxBegPos-1));
             Endif;
          //*** end add *****  09/25/24  *********************************
             If S1Type = reportType;
                scReport = 'Y';
             Else;
                Clear scReport;
             Endif;
          Endif;

          Exsr ClearErrors;
          Clear replSave;
          Clear replCanc;

       //* Display popup window for the Replacement Doc record.

          Dou replSave = *on or replCanc = *on;

             Dou error = *off or replCanc = *on;

                Exfmt rcdReplace;

                Exsr ClearErrors;

                If replCanc = *off;
                   Exsr ReplaceCheck;
                Endif;
             //***** Beg Del *** 08/13/24 ***************************
             //***** Beg Add *** 08/21/23 ***************************
             //*If inCopyMode <> *on;
             //*   clear scSRDefNam;
             //*   clear scSRDefPth;
             //*   clear scSRCltNam;
             //*   clear scSRCltPth;
             //*Endif;
             //***** End Add *** 08/21/23 ***************************
             //***** End Del *** 08/13/24 ***************************

             Enddo;
             //***** Beg Del *** 08/13/24 ***************************
             //***** Beg Add *** 08/21/23 ***************************
             //*If inCopyMode <> *on;
             //*   clear scSRDefNam;
             //*   clear scSRDefPth;
             //*   clear scSRCltNam;
             //*   clear scSRCltPth;
             //*Endif;

             //*If error <> *on and inCopyMode <> *on;
             //*   SetGT (inReplType:*hival) psp11l;
             //*   ReadPE(n) (inReplType) psp11l;
             //*   If scReport = 'Y';
             //*      SetGT (reportType:*hival) psp11l;
             //*      ReadPE(n) (reportType) psp11l;
             //*   Endif;
             //*   If %found;
             //*      scSRDefNam = s1SDEFT;
             //*      scSRDefPth = s1PATHD;
             //*      scSRCltNam = s1SCLTT;
             //*      scSRCltPth = s1PATHC;
             //*   Endif;
             //*Endif;
             //***** End Add *** 08/21/23 ***************************
             //***** End Del *** 08/13/24 ***************************

          Enddo;

          If replSave = *on;
             Exsr ReplaceSave;
          Endif;

          replAddMode = *off;

       Enddo;

       //===================================================================
       // Clear the error fields for window.
       //===================================================================
       BegSr ClearErrors;

       error = *off;
       Clear erSREfdt;
       Clear msSREfdt;
       Clear erSRStat;
       Clear msSRStat;
       Clear erSRCltNam;
       Clear msSRCltNam;
       Clear erSRCltPth;
       Clear msSRCltPth;

       Endsr;
       //===================================================================

       //===================================================================
       // Edit the replacement doc setup fields.
       //===================================================================
       BegSr ReplaceCheck;

       If replEdtMde = *off;

          erSREfdt = ValidDate(scSREfdt:msSREfdt);
          If erSREfdt = *on;
             msSREfdt = 'Effective Date must be a valid date.';
             error = *on;
             Leavesr;
          Endif;

          Chain(n) (inReplType:dtCymd(scSREfdt)) psp11l;
          If scReport = 'Y';
             Chain(n) (reportType:dtCymd(scSREfdt)) psp11l;
          Endif;
          If %found;
             msSREfdt = 'Record already exists for effective date.';
             erSREfdt = *on;
             error = *on;
             Leavesr;
          Endif;

       Endif;

       // Document ID must have a valid file extension.
       If inReplInter <> 'Y';
          xxTBID = 'FILN';
       Else;
          xxTBID = 'FILI';
       Endif;

       //*** begin add ***  09/25/24  *********************************
       If (SCFlg6 = 'Y' and inReplInter <> 'Y') or SCFlg6 <> 'Y';
       //*** end add *****  09/25/24  *********************************
       If scSRCltNam <> ' ';
          If %scan('.':scSRCltNam) = 0;
             If inReplInter = 'Y';
                msSRCltNam = 'Client LiveDoc ID must have a valid file ' +
                 'extension.';
             Else;
                msSRCltNam = 'Client Template ID must have a valid file ' +
                 'extension.';
             Endif;
             erSRCltNam = *on;
             error = *on;
             Leavesr;
          Else;

             xxKey = %xlate(lo:up:%trim(%subst(scSRCltNam:
              %scan('.':scSRCltNam):10)));
             Chain(n) keySTGTL pgt00;
             If not %found;
                If inReplInter = 'Y';
                   msSRCltNam = 'Client LiveDoc ID must have a valid file ' +
                    'extension.';
                Else;
                   msSRCltNam = 'Client Template ID must have a valid file ' +
                    'extension.';
                Endif;
                erSRCltNam = *on;
                error = *on;
                Leavesr;
             Endif;

          Endif;

       Endif;
       //*** begin add ***  09/25/24  *********************************
       Endif;
       //*** end add *****  09/25/24  *********************************

       // Document path must be entered for non-interactive documents.
       If inReplInter <> 'Y' and scSRCltNam <> ' ';
          If scSRCltPth = ' ';
             msSRCltPth = 'Document path must be entered.';
             erSRCltPth = *on;
             error = *on;
             LeaveSR;
          Else;

          // Document path must begin and end with a '/' character.
             strSlash  = %subst(scSRCltPth:1:1);
             strSlash2 = %subst(scSRCltPth:%len(%trim(scSRCltPth)):1);

             If strSlash <> '/' or strSlash2 <> '/';
                msSRCltPth = 'Document path must begin and end with a ''/''.';
                erSRCltPth = *on;
                error = *on;
                LeaveSR;
             Endif;

          Endif;

       Endif;

       Endsr;
       //===================================================================

       //===================================================================
       // Save the printer tray setup record.
       //===================================================================
       BegSr ReplaceSave;

       If xxRRN <> 0;
          Chain xxRRN psp11p;
       Else;
          Setll (inReplType:dtCymd(scSREfdt)) psp11l;
          Readpe(n) (inReplType) psp11l;
          If scReport = 'Y';
             Setll (reportType:dtCymd(scSREfdt)) psp11l;
             Readpe(n) (reportType) psp11l;
          Endif;
          If not %eof;
             xxDocD = S1SDefT;
             xxPathD = S1PathD;
          Else;
             xxDocD = SPSDefT;
             xxPathD = SPPathD;
          Endif;
          Clear psp11p;
          S1Type = inReplType;
          If scReport = 'Y';
             S1Type = reportType;
          Endif;
          S1Efdt = dtCymd(scSREfdt);
          S1SDefT = xxDocD;
          S1PathD = xxPathD;
          S1IUsr = Q1User;
          S1IDt  = Dateto7(SystemDate());
       Endif;

       //*** Beg Add ***** 08/21/23 *********************************
       If inCopyMode = *on;
          S1Efdt = dtCymd(scSREfdt);
       Endif;
       //*** End Add ***** 08/21/23 *********************************

       S1SCltT = scSRCltNam;
       S1PathC = scSRCltPth;
       S1Stat  = scSRStat;

       S1Pgm  = Q1Pgm;
       S1User = Q1User;
       S1Time = Timeto6(SystemTime());
       S1Dt   = Dateto7(SystemDate());

       //*** Beg Chg ***** 08/21/23 *********************************
       //***If xxRRN <> 0;
       If xxRRN <> 0 and inCopyMode <> *on;
       //*** Beg Chg ***** 08/21/23 *********************************
          Update psp11p;
       Else;
          Write psp11p;
       Endif;

       Endsr;
       //===================================================================

      /end-free

     p ReplaceWindow   e

       //===================================================================
       // Valid Date - Date passed in is valid and return formatted. If date passed
       //              in is not valid then value passed in is returned.
       //===================================================================
     P ValidDate       B
     D ValidDate       PI             1
     D date                           8  0
     D messageID                    100
      //
     C     Datchk        Plist
     C                   Parm                    Srdti             8 0
     C                   Parm      *BLANKS       Srffmt            8
     C                   Parm      *BLANKS       Srtfmt            8
     C                   Parm                    Srdto            10
     C                   Parm                    Srerr             1
      //
      /FREE
       Clear messageID;
       //
       //  EDIT DATE
       //
      /END-FREE
     C                   Z-ADD     date          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      //
      /FREE
       IF SRERR = *ON;
      /END-FREE
     C                   movel(p)  'WCR0500'     messageID
      /FREE
         return *ON;
       ELSE;
      /END-FREE
     C                   MOVEL     SRDTO         date
      /FREE
         return *OFF;
       ENDIF;                                                                 //SRERR=*ON
       //
      /END-FREE
     P ValidDate       E
       //===================================================================
       // Logo setup window.
       //===================================================================
     p LogoWindow      b
     d LogoWindow      pi
     D inLogoType                   100    const
     D*** begin add ***  09/25/24  ********************************************
     D inLogoInter                    1    const
     D*** end add *****  09/25/24  ********************************************
     D inLogoRRN                      9  0 const options(*nopass)
      //*** Beg Add ***** 08/21/23 *********************************
     D inCopyMode                      n   const options(*nopass)
      //*** End Add ***** 08/21/23 *********************************

     D xxRRN           s              9  0 inz(0)
     D xxPath          s            250
     D xxLogo          s            100
     d error           s               n   inz(*off)
     d logoAddMode     s               n   inz(*off)

      /free

       //*** Beg Chg ***** 02/18/25 *********************************
       //*** If %parms >= 2;
       If %parms > 2;
       //*** End Chg ***** 02/18/25 *********************************
          xxRRN = inLogoRRN;
       Endif;

       scCo# = intCo#;
       logoAddMode = *off;
       logoEdtMde = *off;
       //*** Beg Add ***** 08/21/23 *********************************
       logoEdtM1 = *off;
       logoEdtM2 = *off;
       //*** End Add ***** 08/21/23 *********************************
       //*** begin add ***  09/25/24  *************************************
       //* Read Scriptura module file.
       Read(n) psc0p;
       visScrpDC = IsModEnabled('Design and Compose');
       If visScrpDC = *on and inLogoInter = 'Y';
          isScriptDC = *on;
       Endif;
       //*** end add *****  09/25/24  *************************************

       If xxRRN <> 0;
          Chain(n) xxRRN psp12p;
       //*** Beg Chg ***** 08/21/23 *********************************
       //***   logoEdtMde = *on;
       //***Else;
       //***   logoAddMode = *on;
       //***Endif;
          If inCopyMode = *on;
             logoAddMode = *on;
             logoEdtMde = *on;
             logoEdtM1 = *on;
             btnName = 'Proceed To Copy';
          Else;
             logoEdtMde = *on;
             logoEdtM2 = *on;
             btnName = 'Save';
          Endif;
       Else;
          logoAddMode = *on;
          btnName = 'Save';
       Endif;
       //*** End Chg ***** 08/21/23 *********************************

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou logoAddMode = *off;

          //* Clear screen fields prior to display if in Add Mode.
          //*** Beg Chg ***** 08/21/23 *********************************
          //***If logoAddMode = *on;
          If logoAddMode = *on and inCopyMode <> *on;
          //*** End Chg ***** 08/21/23 *********************************
             Clear scLGroup;
             Clear scLEfdt;
             Clear scLLogo;
             Clear scLPath;
          //*** begin add ***  09/25/24  *************************************
             Clear scLURL;
          //*** end add *****  09/25/24  *************************************
             scLStat = 'A';
          Else;
             scLGroup = S2Fnd;
             scLEfdt  = dtMDYY(S2Efdt);
             scLStat  = S2Stat;
             scLLogo  = S2Logo;
             scLPath  = S2Path;
          //*** begin add ***  09/25/24  *************************************
             scLURL   = S2URL;
          //*** end add *****  09/25/24  *************************************
          Endif;

          Exsr ClearErrors;
          Clear logoSave;
          Clear logoCanc;

       //* Display popup window for the Logo image record.

          Dou logoSave = *on or logoCanc = *on;

             Dou error = *off or logoCanc = *on;

                Exfmt rcdLogo;

                Exsr ClearErrors;

                //*** Beg Chg ***** 08/21/23 ****************************
                //***If logoCanc = *off;
                If logoCanc = *off and inCopyMode <> *on;
                //*** End Chg ***** 08/21/23 ****************************
                   Exsr LogoCheck;
                Endif;

             Enddo;

          Enddo;

          If logoSave = *on;
             //*** Beg Chg ***** 08/21/23 ****************************
             //***Exsr LogoImageSave;
             If inCopyMode <> *on;
                Exsr LogoImageSave;
             Else;
                inData = 'Copy Logo Image';
                xxrrn = 0;
                If outLgroup = 'All';
                   Chain(n) (inLogoType:*Zeros:dtCymd(scLEfdt)) psp12l;
                   If %found;
                      PopOneBtn('Present for all Groups with effective date '+
                                 %editc(scLEfdt:'Y') + '.':'Duplicate');
                      leave;
                   Endif;
                Else;
                   Open WMFNL;
                   Chain(n) (intCo#:%dec(outLgroup:3:0)) pfn00;
                   If %found;
                      xxrrn = dbrrn14;
                   Endif;
                   Close WMFNL;
                Endif;
                ProceedToCopy(xxrrn:inData:scOutType:scLEfdt:outLRRN:
                               *omit:*omit:*omit:*omit:*omit:inLeft:inTop);
             Endif;
             //*** End Chg ***** 08/21/23 ****************************
          Endif;

          logoAddMode = *off;

       Enddo;

       //===================================================================
       // Clear the error fields for window.
       //===================================================================
       BegSr ClearErrors;

       error = *off;
       Clear erLGroup;
       Clear msLGroup;
       Clear erLEfdt;
       Clear msLEfdt;
       Clear erLStat;
       Clear msLStat;
       Clear erLLogo;
       Clear msLLogo;
       Clear erLPath;
       Clear msLPath;
       //*** begin add ***  09/25/24  *************************************
       Clear erLURL;
       Clear msLURL;
       //*** end add *****  09/25/24  *************************************

       Endsr;
       //===================================================================

       //===================================================================
       // Edit the Logo image setup fields.
       //===================================================================
       BegSr LogoCheck;

       //*** Beg Chg ***** 08/21/23 *********************************
       //***If logoEdtMde = *off;
       If logoEdtM2 = *off;
       //*** End Chg ***** 08/21/23 *********************************

          erLEfdt = ValidDate(scLEfdt:msLEfdt);
          If erLEfdt = *on;
             msLEfdt = 'Effective Date must be a valid date.';
             error = *on;
             Leavesr;
          Endif;

          Chain(n) (inLogoType:scLGroup:dtCymd(scLEfdt)) psp12l;
          If %found;
             msLGroup = 'Record already exists for group and effective date.';
             erLGroup = *on;
             error = *on;
             Leavesr;
          Endif;

       Endif;

       // Image file name must be entered/valid.
       If scLLogo <> ' ';
          If %scan('.':scLLogo) = 0;
             msLLogo = 'Image file name must have a file extension.';
             erLLogo = *on;
             error = *on;
          Endif;
       Else;
          msLLogo = 'An image file name must be entered.';
          erLLogo = *on;
          error = *on;
       Endif;

       //*** begin add ***  09/25/24  *************************************
       If SCFlg6 <> 'Y' or inLogoInter = ' ';
       //*** end add *****  09/25/24  *************************************
       // Image path must be entered.
       If scLPath = ' ';
          msLPath = 'Image file path must be entered.';
          erLPath = *on;
          error = *on;
       Else;

       // Path must begin and end with a '/' character.
          strSlash  = %subst(scLPath:1:1);
          strSlash2 = %subst(scLPath:%len(%trim(scLPath)):1);

          If strSlash <> '/' or strSlash2 <> '/';
             msLPath = 'Image file path must begin and end with a ''/''.';
             erLPath = *on;
             error = *on;
          Endif;

       Endif;
       //*** begin add ***  09/25/24  *************************************
       Else;
          // Image URL must be entered.
          If scLURL = ' ';
             msLURL = 'Logo image URL must be entered.';
             erLURL = *on;
             error = *on;
          Else;
          //* Design and Compose enabled.
             If %subst(scLURL:1:7) <> 'http://' OR
              %subst(scLURL:%len(%trim(scLURL)):1) <> '/';
                erLURL = *on;
                msLURL = 'Logo image URL must begin with http:// and must' +
                 ' end with a ''/''.';
                error = *on;
             Endif;
          Endif;
       Endif;
       //*** end add *****  09/25/24  *************************************

       Endsr;
       //===================================================================

       //===================================================================
       // Save the printer tray setup record.
       //===================================================================
       BegSr LogoImageSave;

       If xxRRN <> 0;
          Chain xxRRN psp12p;
       Else;
          Clear psp12p;
          S2Type = inLogoType;
          S2Fnd  = scLGroup;
          S2Efdt = dtCymd(scLEfdt);
          S2IUsr = Q1User;
          S2IDt  = Dateto7(SystemDate());
       Endif;

       S2Logo = scLLogo;
       S2Path = scLPath;
       //*** begin add ***  09/25/24  *************************************
       S2URL = scLURL;
       //*** end add *****  09/25/24  *************************************

       S2Stat  = scLStat;

       S2Pgm  = Q1Pgm;
       S2User = Q1User;
       S2Time = Timeto6(SystemTime());
       S2Dt   = Dateto7(SystemDate());

       If xxRRN <> 0;
          Update psp12p;
       Else;
          Write psp12p;
       Endif;

       Endsr;
       //===================================================================

      /end-free

     p LogoWindow      e
       //===================================================================
       // Signature setup window.
       //===================================================================
     p SigWindow       b
     d SigWindow       pi
     D inSigType                    100    const
     D inSigRRN                       9  0 const options(*nopass)
      //*** Beg Add ***** 08/21/23 *********************************
     D inCopyMode                      n   const options(*nopass)
      //*** End Add ***** 08/21/23 *********************************

     D xxRRN           s              9  0 inz(0)
     D xxPath          s            250
     D xxSig           s            100
     d error           s               n   inz(*off)
     d sigAddMode      s               n   inz(*off)

      /free

       If %parms >= 2;
          xxRRN = inSigRRN;
       Endif;

       scCo# = intCo#;
       sigAddMode = *off;
       sigEdtMde = *off;
       //*** Beg Add ***** 08/21/23 *********************************
       sigEdtM1 = *off;
       sigEdtM2 = *off;
       //*** End Add ***** 08/21/23 *********************************

       If xxRRN <> 0;
          Chain(n) xxRRN psp13p;
       //*** Beg Chg ***** 08/21/23 *********************************
       //***   sigEdtMde = *on;
       //***Else;
       //***   sigAddMode = *on;
       //***Endif;
          If inCopyMode = *on;
             sigAddMode = *on;
             sigEdtMde = *on;
             sigEdtM1 = *on;
             btnName = 'Proceed To Copy';
          Else;
             sigEdtMde = *on;
             sigEdtM2 = *on;
             btnName = 'Save';
          Endif;
       Else;
          sigAddMode = *on;
          btnName = 'Save';
       Endif;
       //*** End Chg ***** 08/21/23 *********************************

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou sigAddMode = *off;

          //* Clear screen fields prior to display if in Add Mode.
          //*** Beg Chg ***** 08/21/23 *********************************
          //***If sigAddMode = *on;
          If sigAddMode = *on and inCopyMode <> *on;
          //*** End Chg ***** 08/21/23 *********************************
             Clear scSGroup;
             Clear scSEfdt;
             Clear scSFile;
             Clear scSPath;
             Clear scSFile2;
             Clear scSPath2;
             scSStat = 'A';
          Else;
             scSGroup = S3Fnd;
             scSEfdt  = dtMDYY(S3Efdt);
             scSStat  = S3Stat;
             scSFile  = S3Sig;
             scSPath  = S3Path;
             scSFile2 = S3Sig2;
             scSPath2 = S3Path2;
          Endif;

          Exsr ClearErrors;
          Clear sigSave;
          Clear sigCanc;

       //* Display popup window for the Signature image record.

          Dou sigSave = *on or sigCanc = *on;

             Dou error = *off or sigCanc = *on;

                Exfmt rcdSignat;

                Exsr ClearErrors;

                //*** Beg Chg ***** 08/21/23 ****************************
                //***If sigCanc = *off;
                If sigCanc = *off and inCopyMode <> *on;
                //*** End Chg ***** 08/21/23 ****************************
                   Exsr SigCheck;
                Endif;

             Enddo;

          Enddo;

          If sigSave = *on;
             //*** Beg Chg ***** 08/21/23 ****************************
             //***Exsr SigImageSave;
             If inCopyMode <> *on;
                Exsr SigImageSave;
             Else;
                inData = 'Copy Signature Image';
                xxrrn = 0;
                If outSgroup = 'All';
                   Chain(n) (inSigType:*zeros:dtCymd(scSEfdt)) psp13l;
                   If %found;
                      PopOneBtn('Present for all Groups with effective date '+
                                 %editc(scSEfdt:'Y') + '.':'Duplicate');
                      leave;
                   Endif;
                Else;
                   Open WMFNL;
                   Chain(n) (intCo#:%dec(outSgroup:3:0)) pfn00;
                   If %found;
                      xxrrn = dbrrn14;
                   Endif;
                   Close WMFNL;
                Endif;
                ProceedToCopy(xxrrn:inData:scOutType:scSEfdt:outSRRN:
                               *omit:*omit:*omit:*omit:*omit:inLeft:inTop);
             Endif;
             //*** End Chg ***** 08/21/23 ****************************
          Endif;

          sigAddMode = *off;

       Enddo;

       //===================================================================
       // Clear the error fields for window.
       //===================================================================
       BegSr ClearErrors;

       error = *off;
       Clear erSGroup;
       Clear msSGroup;
       Clear erSEfdt;
       Clear msSEfdt;
       Clear erSStat;
       Clear msSStat;
       Clear erSFile;
       Clear msSFile;
       Clear erSPath;
       Clear msSPath;
       Clear erSFile2;
       Clear msSFile2;
       Clear erSPath2;
       Clear msSPath2;

       Endsr;
       //===================================================================

       //===================================================================
       // Edit the Signature image setup fields.
       //===================================================================
       BegSr SigCheck;

       //*** Beg Chg ***** 08/21/23 **********************************
       //***If sigEdtMde = *off;
       If sigEdtM2 = *off;
       //*** End Chg ***** 08/21/23 **********************************

          erSEfdt = ValidDate(scSEfdt:msSEfdt);
          If erSEfdt = *on;
             msSEfdt = 'Effective Date must be a valid date.';
             error = *on;
             Leavesr;
          Endif;

          Chain(n) (inSigType:scSGroup:dtCymd(scSEfdt)) psp13l;
          If %found;
             msSGroup = 'Record already exists for group and effective date.';
             erSGroup = *on;
             error = *on;
             Leavesr;
          Endif;

       Endif;

       // Image file name must be entered/valid.
       If scSFile <> ' ';
          If %scan('.':scSFile) = 0;
             msSFile = 'Image file name must have a file extension.';
             erSFile = *on;
             error = *on;
          Endif;
       Else;
          msSFile = 'An image file name must be entered.';
          erSFile = *on;
          error = *on;
       Endif;

       // Image path must be entered.
       If scSPath = ' ';
          msSPath = 'Image file path must be entered.';
          erSPath = *on;
          error = *on;
       Else;

       // Path must begin and end with a '/' character.
          strSlash  = %subst(scSPath:1:1);
          strSlash2 = %subst(scSPath:%len(%trim(scSPath)):1);

          If strSlash <> '/' or strSlash2 <> '/';
             msSPath = 'Image file path must begin and end with a ''/''.';
             erSPath = *on;
             error = *on;
          Endif;

       Endif;


       // Image 2 file name must be valid if entered.
       If scSFile2 <> ' ';
          If %scan('.':scSFile2) = 0;
             msSFile2 = 'Image file name must have a file extension.';
             erSFile2 = *on;
             error = *on;
          Endif;
       Endif;

       // Image path must be entered if image specified.
       If scSPath2 = ' ' and scSFile2 <> ' ';
          msSPath2 = 'Image file path must be entered.';
          erSPath2 = *on;
          error = *on;
       Endif;

       If scSPath2 <> ' ';
       // Path must begin and end with a '/' character.
          strSlash  = %subst(scSPath2:1:1);
          strSlash2 = %subst(scSPath2:%len(%trim(scSPath2)):1);

          If strSlash <> '/' or strSlash2 <> '/';
             msSPath2 = 'Image file path must begin and end with a ''/''.';
             erSPath2 = *on;
             error = *on;
          Endif;

       Endif;


       Endsr;
       //===================================================================

       //===================================================================
       // Save the printer tray setup record.
       //===================================================================
       BegSr SigImageSave;

       If xxRRN <> 0;
          Chain xxRRN psp13p;
       Else;
          Clear psp13p;
          S3Type = inSigType;
          S3Fnd  = scSGroup;
          S3Efdt = dtCymd(scSEfdt);
          S3IUsr = Q1User;
          S3IDt  = Dateto7(SystemDate());
       Endif;

       S3Sig  = scSFile;
       S3Path = scSPath;
       S3Sig2 = scSFile2;
       S3Path2 = scSPath2;

       S3Stat  = scSStat;

       S3Pgm  = Q1Pgm;
       S3User = Q1User;
       S3Time = Timeto6(SystemTime());
       S3Dt   = Dateto7(SystemDate());

       If xxRRN <> 0;
          Update psp13p;
       Else;
          Write psp13p;
       Endif;

       Endsr;
       //===================================================================

      /end-free

     p SigWindow       e

       //===================================================================
       // Letter Template setup window.
       //===================================================================
     p LtrWindow       b
     d LtrWindow       pi
     D inLtrType                    100    const
      //*** Beg Add ***** 03/22/24 *********************************
     D inLtrCat                       5    const
      //*** End Add ***** 03/22/24 *********************************
     D inLtrRRN                       9  0 const options(*nopass)
      //*** Beg Add ***** 08/21/23 *********************************
     D inCopyMode                      n   const options(*nopass)
      //*** End Add ***** 08/21/23 *********************************

     D xxRRN           s              9  0 inz(0)
     D xxPath          s            250
     D xxLtr           s            100
     d error           s               n   inz(*off)
     d LtrAddMode      s               n   inz(*off)

      /free

       //*** Beg Chg ***** 03/22/24 *********************************
       //***If %parms >= 2;
       If %parms >= 3;
       //*** End Chg ***** 03/22/24 *********************************
          xxRRN = inLtrRRN;
       Endif;

       scCo# = intCo#;
       //*** Beg Add ***** 03/22/24 **************************
       scLtrCat = inLtrCat;
       //*** End Add ***** 03/22/24 **************************
       ltrAddMode = *off;
       ltrEdtMde = *off;
       //*** Beg Add ***** 08/21/23 *********************************
       ltrEdtM1 = *off;
       ltrEdtM2 = *off;
       //*** End Add ***** 08/21/23 *********************************

       If xxRRN <> 0;
          Chain(n) xxRRN psp14p;
       //*** Beg Chg ***** 08/21/23 *********************************
       //***   ltrEdtMde = *on;
       //***Else;
       //***   ltrAddMode = *on;
       //***Endif;
          If inCopyMode = *on;
             ltrAddMode = *on;
             ltrEdtMde = *on;
             ltrEdtM1 = *on;
             btnName = 'Proceed To Copy';
          Else;
             ltrEdtMde = *on;
             ltrEdtM2 = *on;
             btnName = 'Save';
          Endif;
       Else;
          ltrAddMode = *on;
          btnName = 'Save';
       Endif;
       //*** End Chg ***** 08/21/23 *********************************

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou ltrAddMode = *off;

          //* Clear screen fields prior to display if in Add Mode.
          //*** Beg Chg ***** 08/21/23 *********************************
          //***If ltrAddMode = *on;
          If ltrAddMode = *on and inCopyMode <> *on;
          //*** End Chg ***** 08/21/23 *********************************
             Clear scLTGroup;
             Clear scLTEfdt;
             Clear scLTFlNm;
             Clear scLTPath;
             Clear scLTType;
             scLTStat = 'A';
NP01         Clear scLTFlNmRn;
NP01         Clear scLTPathRn;
          Else;
             scLTGroup = S4Fnd;
             scLTEfdt  = dtMDYY(S4Efdt);
             scLTStat  = S4Stat;
             scLTFlNm  = S4Doc;
             scLTPath  = S4Path;
             scLTType  = S4LType;
NP01         scLTFlNmRn = S4RDoc;
NP01         scLTPathRn = S4RPath;
          Endif;

          Exsr ClearErrors;
          Clear ltrSave;
          Clear ltrCancel;

       //* Display popup window for the Letter Template record.

          Dou ltrSave = *on or ltrCancel = *on;

             Dou error = *off or ltrCancel = *on;

                Exfmt rcdLtrTmpl;

                Exsr ClearErrors;

                //*** Beg Chg ***** 08/21/23 ****************************
                //***If ltrCancel = *off;
                If ltrCancel = *off and inCopyMode <> *on;
                //*** End Chg ***** 08/21/23 ****************************
                   Exsr LtrCheck;
                Endif;

             Enddo;

          Enddo;

          If ltrSave = *on;
             //*** Beg Chg ***** 08/21/23 ****************************
             //***Exsr ltrTmplSave;
             If inCopyMode <> *on;
                Exsr ltrTmplSave;
             Else;
                inData = 'Copy Letter Template';
                xxrrn = 0;
                If outLTgroup = 'All';
                   Chain(n) (inLtrType:*zeros:dtCymd(scLTEfdt)) psp14l;
                   If %found;
                      PopOneBtn('Present for all Groups with effective date '+
                                 %editc(scLTEfdt:'Y') + '.':'Duplicate');
                      leave;
                   Endif;
                Else;
                   Open WMFNL;
                   Chain(n) (intCo#:%dec(outLTgroup:3:0)) pfn00;
                   If %found;
                      xxrrn = dbrrn14;
                   Endif;
                   Close WMFNL;
                Endif;
                ProceedToCopy(xxrrn:inData:scOutType:scLTEfdt:outLTRRN:
                               *omit:*omit:*omit:*omit:*omit:inLeft:inTop);
             Endif;
             //*** End Chg ***** 08/21/23 ****************************
          Endif;

          ltrAddMode = *off;

       Enddo;

       //===================================================================
       // Clear the error fields for window.
       //===================================================================
       BegSr ClearErrors;

       error = *off;
       Clear erLTGroup;
       Clear msLTGroup;
       Clear erLTEfdt;
       Clear msLTEfdt;
       Clear erLTStat;
       Clear msLTStat;
       Clear erLTType;
       Clear msLTType;
       Clear erLTFlNm;
       Clear msLTFlNm;
       Clear erLTPath;
       Clear msLTPath;

       Endsr;
       //===================================================================

       //===================================================================
       // Edit the Letter Template setup fields.
       //===================================================================
       BegSr LtrCheck;

       //*** Beg Chg ***** 08/21/23 *********************************
       //***If ltrEdtMde = *off;
       If ltrEdtM2 = *off;
       //*** End Chg ***** 08/21/23 *********************************

          erLTEfdt = ValidDate(scLTEfdt:msLTEfdt);
          If erLTEfdt = *on;
             msLTEfdt = 'Effective Date must be a valid date.';
             error = *on;
             Leavesr;
          Endif;

          Chain(n) (inLtrType:scLTGroup:dtCymd(scLTEfdt)) psp14l;
          If %found;
             msLTGroup = 'Record already exists for group and effective date.';
             erLTGroup = *on;
             error = *on;
             Leavesr;
          Endif;

       Endif;

       // Letter Template Type must be selected.
       If scLTType = ' ';
          msLTType = 'Letter Template Type must be selected.';
          erLTType = *on;
          error = *on;
       Endif;

       // Letter Template file name must be entered/valid.
       If scLTFlNm <> ' ';
          If %scan('.':scLTFlNm) = 0;
             msLTFlNm = 'Letter Template file name must have a file extension.';
             erLTFlNm = *on;
             error = *on;
          Endif;
       Else;
          msLTFlNm = 'Letter Template file name must be entered.';
          erLTFlNm = *on;
          error = *on;
       Endif;

       // Letter Template path must be entered.
       If scLTPath = ' ';
          msLTPath = 'Letter Template file path must be entered.';
          erLTPath = *on;
          error = *on;
       Else;

       // Path must begin and end with a '/' character.
          strSlash  = %subst(scLTPath:1:1);
          strSlash2 = %subst(scLTPath:%len(%trim(scLTPath)):1);

          If strSlash <> '/' or strSlash2 <> '/';
             msLTPath = 'Letter Template file path must begin +
                        and end with a ''/''.';
             erLTPath = *on;
             error = *on;
          Endif;

       Endif;

       Endsr;
       //===================================================================

       //===================================================================
       // Save the Letter Template record.
       //===================================================================
       BegSr LtrTmplSave;

       If xxRRN <> 0;
          Chain xxRRN psp14p;
       Else;
          Clear psp14p;
          S4Type = inLtrType;
          S4Fnd  = scLTGroup;
          S4Efdt = dtCymd(scLTEfdt);
          S4IUsr = Q1User;
          S4IDt  = Dateto7(SystemDate());
          S4LangD= SPLangD;
       Endif;

       S4Doc  = scLTFlNm;
       S4Path = scLTPath;

       //*** Beg Add *** 04/28/22 *** WC2670 ******************
       S4RDoc  = scLTFlNmRn;
       S4RPath = scLTPathRn;
       S4RLangD= SPLangD;
       //*** End Add *** 04/28/22 *** WC2670 ******************

       S4Stat   = scLTStat;
       S4LType  = scLTType;

       S4Pgm  = Q1Pgm;
       S4User = Q1User;
       S4Time = Timeto6(SystemTime());
       S4Dt   = Dateto7(SystemDate());

       If xxRRN <> 0;
          Update psp14p;
       Else;
          Write psp14p;
       Endif;

       Endsr;
       //===================================================================

      /end-free

     p LtrWindow       e
      *

       //*** Beg Add ***** 08/21/23 **************************************
      //------------------------------------------
      // GetCarrier# - Getting Carrier
      //------------------------------------------
     p GetCarrier#     b
     d GetCarrier#     pi             5s 0
     d xxfnd                          3  0

      /Free
       If not %open(Wdf2l);
          Open Wdf2l;
       Endif;

       Chain (intCo#:xxfnd) pf200;
       Return F2car#;

       If %open(Wdf2l);
          Close Wdf2l;
       Endif;
      /End-Free

     p GetCarrier#     e
       //*** End Add ***** 08/21/23 **************************************
       //*** Beg Add *** 07/30/24 *******************************************
       //===================================================================
       // Set Staff Multi select box choices
       //===================================================================
     p SetStaffCH      b
     d SetStaffCH      pi
      /end-free
       scStaffCH  = *Blanks;
       scStaffVal = *Blanks;

       //Policy Services
       PolicyCh   = 'Audit Manager,Audit Reviewer,Claim Contact,+
           Policy Services,Premium Accountant,Underwriter';

       //Claims Services
       ClaimCh    = 'Account Manager,Case Nurse,Claim Manager,+
           Outside Adjuster,Reinsurance Adjuster,+
           Special Injury/Disability Fund Adjuster,+
           Supervisor';

       //Agency Services
       AgencyCh   = 'Agency,Agency Services Rep,Agency Marketing Rep';

       // Take Distict value for Policy Services
       strSqlCursor = 'Select Distinct EXahlc from WMEXP  +
                        where EXstat <> ''I'' and EXahlc <> '' '' +
                        ORDER BY EXahlc ASC';
       Exec Sql Prepare sqlDB2 From :strSqlCursor;
       Exec Sql Declare StaffCur Cursor For sqlDB2;
       Exec Sql
        Open StaffCur;
        count1 = 1;
        Dow sqlStt = '00000';
           Exec Sql
            Fetch Next
            From StaffCur
            Into :strStaffAsn;
           If sqlStt = '02000' and count1 = 1;
              PolicyCh   = %trim(PolicyCh) + ',Loss Control Rep';
              Leave;
           Endif;
           If sqlStt = '00000';
              count1 = count1 + 1;
              PolicyCh   = %trim(PolicyCh) + ',' + %trim(strStaffAsn);
           Endif;
        Enddo;
        Exec Sql
         Close StaffCur;

       // Take Distict value for Claim Services
       strSqlCursor = 'Select Distinct CXadj from WMCXP  +
                       where CXstat <> ''I'' and CXadj <> '' '' +
                       ORDER BY CXadj ASC';
       Exec Sql Prepare sqlDB3 From :strSqlCursor;
       Exec Sql Declare StaffCurClm Cursor For sqlDB3;
       Exec Sql
        Open StaffCurClm;
        count1 = 1;
        Dow sqlStt = '00000';
           Exec Sql
            Fetch Next
            From StaffCurClm
            Into :strStaffAsn;
           If sqlStt = '02000' and count1 = 1;
              ClaimCh   = %trim(ClaimCh) + ',Adjuster';
              Leave;
           Endif;
           If sqlStt = '00000';
              count1 = count1 + 1;
              ClaimCh  = %trim(ClaimCh) + ',' + %trim(strStaffAsn);
           Endif;
        Enddo;
       Exec Sql
        Close StaffCurClm;

       If namesArr(TabIndex +1) = 'Output Control';
          Select;
          When scCat = 'Policy Services';
             scStaffCH = %trim(PolicyCh)  + ',' + %trim(AgencyCh);
             scStaffVal= %trim(PolicyCh)  + ',' + %trim(AgencyCh);
          When scCat = 'Claims Services';
             scStaffCH = %trim(ClaimCh) + ',' + %trim(PolicyCh) + ',' +
                         %trim(AgencyCh);
             scStaffVal= %trim(ClaimCh) + ',' + %trim(PolicyCh) + ',' +
                         %trim(AgencyCh);
          When scCat = 'Agency Services';
             scStaffCH = %trim(PolicyCh)  + ',' + %trim(AgencyCh);
             scStaffVal= %trim(PolicyCh)  + ',' + %trim(AgencyCh);
          Endsl;
       Endif;

       If namesArr(TabIndex +1) = 'Documents';
          Select;
          When outDocCat = 'Policy Services';
             scStaffCH = %trim(PolicyCh)  + ',' + %trim(AgencyCh);
             scStaffVal= %trim(PolicyCh)  + ',' + %trim(AgencyCh);
          When outDocCat = 'Claims Services';
             scStaffCH = %trim(ClaimCh) + ',' + %trim(PolicyCh) + ',' +
                         %trim(AgencyCh);
             scStaffVal= %trim(ClaimCh) + ',' + %trim(PolicyCh) + ',' +
                         %trim(AgencyCh);
          When outDocCat = 'Agency Services';
             scStaffCH = %trim(PolicyCh)  + ',' + %trim(AgencyCh);
             scStaffVal= %trim(PolicyCh)  + ',' + %trim(AgencyCh);
          Endsl;
       Endif;
      /end-free
     p setStaffCH      e
       //*** End Add *** 07/30/24 *******************************************
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************
       //===================================================================
       // SaveGrpSel -  Save Selected Groups
       //===================================================================
     p SaveGrpSel      b
     d SaveGrpSel      pi
     d  CurRec         S              3S 0

       // Update All groups
       CurRec = 1;
       Dow CurRec <= intSelGrp;
          Chain CurRec sflGrpSel;
          If %Found;
             Chain (intCo# : scGroup#) WXGSPDSH;
             If %Found;
                GSIEFL = scIEfl;
                Update PGS00;
             Else;
                GSCO#  = intCo#;
                GSFND  = scGroup#;
                GSNAME = scGrpName;
                GSIEFL = scIEfl;
                Write PGS00;
             EndIf;
          EndIf;
          CurRec = CurRec + 1;
       EndDo;

     p SaveGrpSel      e
       //===================================================================
       // UpdGrpSel  -  Update WXGSPDSH Statuses
       //===================================================================
     p UpdGrpSel       b
     d UpdGrpSel       pi
     d  StatusIn                      1    Const

     d  CurRec         S              3S 0
       // Update All groups
       CurRec = 1;
       Dow CurRec <= intSelGrp;
          Chain CurRec sflGrpSel;
          If %Found;
             scIEfl = statusIn;
             Update sflGrpSel;
          EndIf;
          CurRec = CurRec + 1;
       EndDo;

     p UpdGrpSel       e
      *** Beg Add ***** 07/16/24 *** WC2972 ****************************

       //*** Beg Add ***** 09/10/24 *********************************
       //===================================================================
       // FltEmailToken - Filter Email Tokens based on Category
       //===================================================================
     p FltEmailToken   b
     d FltEmailToken   pi
     d CallType                      10    const
     d SelCriteria     s            200    Inz
      /free

       If CallType = 'OutControl';
          Category = SPcat;
          Clear ScTokenCr1;
       Endif;
       If CallType = 'Documents';
          Category = ScDocCatg;
          Clear scTokenCrt;
       Endif;
       If Category <> *blanks;
          Select;
          //Agency Services
          When Category   = 'AS';
             SelCriteria = 'ETco# = ' + %EditC(intCo#:'X') + ' and ' +
                '(' +  'ETpolicy=' + '''Y''' + ' or ' +
                'ETagent=' + '''Y''' + ')';

          //Claims Services
          When Category  = 'CS';
             SelCriteria = 'ETco# = ' + %EditC(intCo#:'X') + ' and ' +
                '(' +  'ETpolicy=' + '''Y''' + ' or ' +
                'ETclaim=' + '''Y''' + ' or ' +
                'ETagent=' + '''Y''' + ')';

          //Medical Case
          When Category  = 'MC';
             SelCriteria = 'ETco# = ' + %EditC(intCo#:'X') + ' and ' +
                '(' +  'ETmangc=' + '''Y''' + ')';

          //Pre-Certification
          When Category  = 'PC';
             SelCriteria = 'ETco# = ' + %EditC(intCo#:'X') + ' and ' +
                '(' +  'ETprecert =' + '''Y''' + ')';

          //Policy Services
          When Category  = 'PS';
             SelCriteria = 'ETco# = ' + %EditC(intCo#:'X') + ' and ' +
                '(' +  'ETpolicy=' + '''Y''' + ' or ' +
                'ETagent=' + '''Y''' + ')';

          //Report
          When Category  = 'RP';
             SelCriteria = 'ETco# = ' + %EditC(intCo#:'X') + ' and ' +
                '(' +  'ETreport =' + '''Y''' + ')';

          Endsl;

          If CallType = 'OutControl';
             ScTokenCr1 = SelCriteria;
          Endif;
          If CallType = 'Documents';
             scTokenCrt = SelCriteria;
          Endif;
       Endif;
      /end-free
     P FltEmailToken   e
       //*** End Add ***** 09/10/24 *********************************
