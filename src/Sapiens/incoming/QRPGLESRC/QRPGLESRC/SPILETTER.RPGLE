     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H/TITLE SPILETTER - API for documents/letters creation.
     H nomain
     H*************************************************************************
     H**   PROGRAM DESCRIPTION:                                               *
     H**                                                                      *
     H**    THIS PROGRAM IS USED TO DISPLAY A WINDOW TO SELECT AND            *
     H**    GENERATE LETTERS/DOCUMENTS FOR POLICIES/CLAIMS.                   *
     H**                                                                      *
     H**    There are four main procedures:                                   *
     H**                                                                      *
     H** DocWindow:      Display popup window to allow selection of           *
     H**                 documents to produce for the policy/claim.           *
     H**                 The popup window will have a tab panel where         *
     H**                 each tab contains all of the documents               *
     H**                 available for that sub category.                     *
     H**                                                                      *
     H** LaunchDoc:      Procedure to generate XML file, build URL,           *
     H**                 and display to screen to launch browser window.      *
     H*                                                                       *
     H** OutputWindow:   Procedure to display a window for output choices     *
     H**                 for Scriptura enabled processes.  It returns off     *
     H**                 for cancel and on for submit.                        *
     H**                                                                      *
     H** ListDocs:       Procedure to request list of D&C documents from      *
     H**                 Scriptura server and parse the resulting XML file    *
     H*                  into a physical file to verify if document avail.    *
     H*                                                                       *
     H* PROGRAMMER: C:65                                                      *
     H* CREATED:    03/20/15            PROJECT: SY251                        *
     H*                                                                       *
     H*************************************************************************
     H* REVISIONS:                                                            *
     H*                                                                       *
     H* 07/24/25 C:65  Project: Phw#118533 (CoveRisk)                         *
     H*                Change the "All" printer line in the "Policy/Mass"     *
     H*                output container to use its own bound program fields   *
     H*                instead of the same fields in the other container.     *
     H*                The duplicative fields arent updating based on screen. *
     H*                                                                       *
     H* 03/26/25 C:333 Support: 100389                                        *
     H*                PDSQA - iDARTS error when accessing when Documents     *
     H*                        window is up.                                  *
     H*                 - DSPF Changes: Changed scRRN ID from OutRRNLetter to *
     H*                   OutRRN.                                             *
     H*                 - Changes in runtime-bd8053f341.js under function     *
     H*                   draw_floating_toolbar().                            *
     H*                                                                       *
     H* 03/06/25 C:65  Project: WC2670 (Phw#118641 - Internal)                *
     H*                Fix the coding in OutputWindow procedure to clear the  *
     H*                To Email field of the default mass text when running   *
     H*                for a specific entity and the mass flag is not set on. *
     H*                Fix the display only property on the same To Email     *
     H*                field when the mass text is present since it should    *
     H*                never be edited.                                       *
     H*                                                                       *
     H*  01/23/25 C:65  Project: SY446 (Design and Compose revamp)            *
     H*                 When building live doc and interactive form URL's     *
     H*                 append/replace the file extension based on D&C status.*
     H*                 If live doc and D&C not enabled use .SLD extension,   *
     H*                 otherwise use .SDT extension.  For interactive forms  *
     H*                 use .SCD if D&C not enabled, otherwise use .SDT.      *
     H*                 Clear the in processing flag and data file when       *
     H*                 ListDoc does not get a response back in data queue.   *
     H*                 Use ccsid=best for the xml-into command.  Perform a   *
     H*                 CPY command to change the CCSID on file to 1252 for   *
     H*                 allowing the XML parsing to work properly.  Only      *
     H*                 check for template in cloud for LiveDoc requests.     *
     H*                 Store/return the DocumentID from ListDoc for the      *
     H*                 interactive LiveDoc calls.  Force file extensions     *
     H*                 being appended to be in lower case letters.           *
     H*                                                                       *
     H*  11/21/24 C:333 Support: 118045 (WC2826)                              *
     H*                 - Email Recipient is not showing.                     *
     H*                 scToEmail can hold up to 200A. Changes added if the   *
     H*                 email ids exceeds 200A, the overflow is stored in     *
     H*                 scToEmailA.                                           *
     H*                                                                       *
     H*  10/16/24 C:65  Project: SY446 (Design and Compose revamp)            *
     H*                 Add ListDocs procedure to allow D&C documents to be   *
     H*                 verified as available.  This procedure calls Scriptura*
     H*                 and parses the returned XML file as long as its not   *
     H*                 been called more recently than the time value in      *
     H*                 STSCP.  If the data is more recent than it checks     *
     H*                 the data already retrieved.                           *
     H*                                                                       *
     H*  09/25/24 C:65  Project: SY446 (Design and Compose revamp)            *
     H*                 When Design and Compose is enabled hide the file      *
     H*                 extension on the LiveDoc templates and when launching *
     H*                 LiveDocs or Interactive Forms build new URL's.        *
     H*                                                                       *
     H* 07/24/24 C:300 Support: 116983                                        *
     H*                Email Body Not Pulling Tokens                          *
     H*                Call ReplaceTokensEmail procedure                      *
     H*                                                                       *
     H*                Output Control Window just need to show the tokens     *
     H*                so, commenting these chanegs                           *
     H*                                                                       *
     H* 01/16/24 C:340 PROJECT: 153WAD                                        *
     H*                Populate XpCopyE Field on XMLPrep File.                *
     H*                                                                       *
     H*                                                                       *
     H* 07/25/23 C:340 Project: WC2850                                        *
     H*                Scriptura output Control Default # Copies.             *
     H*                Add # of Copies for Single/Mass.                       *
     H*                                                                       *
     H* 06/09/23 C:291 Project: SY436                                         *
     H*                Fix output control window size when Email checkbox     *
     H*                is unchecked. Changes are made in CheckEmail() function*
     H*                in JavaScript.                                         *
     H*                                                                       *
     H* 05/11/23 C:291 Project: WC2838                                        *
     H*                Fix printer on outut window to be editable when "Print *
     H*                Insured Copy" and "Print Agent Copy" boxes are checked.*
     H*                -Add disbPrintI disabled indicator on fields scPritI   *
     H*                 and scPrinterI.                                       *
     H*                -Add disbPrintA disabled indicator on fields scPritA   *
     H*                 and scPrinterA.                                       *
     H*                -Fixed Javascript on scPrint, scPrintI and scPrintA.   *
     H*                                                                       *
     H* 05/02/23 C:344 Project: WC2838                                        *
     H*                Allow email to be checked if recipients or additional  *
     H*                recipients box is populated for claims.                *
     H*                                                                       *
     H* 04/28/23 C:65  Project: WC2838                                        *
     H*                Allow email to be checked if recipients or additional  *
     H*                recipients box is populated.                           *
     H*                                                                       *
     H* 01/20/23 C:344 Project: WC2875                                        *
     H*                Update the call to LoadDefaultEmaildata with the       *
     H*                so that the correct RunInfo parameter is used.         *
     H*
     H* 11/16/22 C:300 Support: 104032 (Cove Risk)                            *
     H*                Edit E-mail Body Not Working                           *
     H*                Allow to change Email body when only                   *
     H*                'allow user override' checkbox is checked              *
     H*                                                                       *
     H* 11/08/22 C:333 Support: 103508(152)                                   *
     H*                Error message when generating Scriptura Letter         *
     H*                - Disply File changes.                                 *
     H*                  - Added 'scSubCat' textbox field.                    *
     H*                  - New column(scSubC) in sflletters grid with width 0.*
     H*                  - Added onload for rcdletters                        *
     H*                - RPGLE Changes                                        *
     H*                                                                       *
     H* 09/23/22 C:62  Project: WC2838                                        *
     H*                Add Contact Confinements.                              *
     H*                                                                       *
     H* 06/02/22 C:33  Support: 100993                                        *
     H*               -Allow for process to decide email availability         *
     H*               -Allow for process to decide DARTS availability         *
     H*               -DARTS/Folder edit updates                              *
     H*                                                                       *
     H* 04/28/22 C:65  Project: WC2278  #101948                               *
     H*                Clear additional email receipents so they do not carry *
     H*                over from a prior request.                             *
     H*                                                                       *
     H* 04/15/22 C:33  Project: WC2818                                        *
     H*                Allow for process to require DARTS                     *
     H*                                                                       *
     H* 09/14/21 C:33  Project: WC2278  #98078                                *
     H*               -Default email checkbox if subject/body defaulted       *
     H*               -Update email overrride flags                           *
     H*                                                                       *
     H* 09/10/21 C:53  Project: WC2278  #97839                                *
     H*          C:33  Use appropriate document id                            *
     H*                                                                       *
     H* 08/13/21 C:65  Project: WC2278                                        *
     H*                Add RRN to OutputWindow procedure to allow email       *
     H*                addresses to populate when called from single requests.*
     H*                Correct tooltips for emailing fields.                  *
     H*                                                                       *
     H* 05/11/21 C:65  Project: WC2278                                        *
     H*                Display new email section of output and document       *
     H*                windows.  Values will be loaded to/from new procs.     *
     H*                                                                       *
     H* 12/15/20 C:65  Project: SY371                                         *
     H*                Add two positions to LDA for multi-group processes and *
     H*                a checkbox for them to use group default remark code.  *
     H*                                                                       *
     H* 06/26/20 C:62  Ticket: 89840(136)                                     *
     H*                Expand date field to prevent overflow error.           *
     H*                                                                       *
     H* 05/08/18 C:65  Project: SY395                                         *
     H*                Add support for interactive forms.  Expand the URL     *
     H*                fields to 500 to avoid truncation of addresses.        *
     H*                                                                       *
     H* 04/18/18 C:62  Ticket: 70159(129)                                     *
     H*                Clear the names array.                                 *
     H*                                                                       *
     H* 11/16/17 C:65  Project: 102WHK                                        *
     H*                Update document schema type into grid for request.     *
     H*                                                                       *
     H* 11/16/17 C:65  Project: SY251/Phw#50620                               *
     H*                Change scan to lookup for building sub category tabs.  *
     H*                                                                       *
     H* 09/28/17 C:33  Support: 49209                                         *
     H*                Update tool tip for disabled check boxes.              *
     H*                                                                       *
     H* 09/14/17 C:65  Project: 25WUK/Phw#49246                               *
     H*                For TASB only, pass the old style XML file name to     *
     H*                to the prompt/exit program specified in SCKEY9.        *
     H*                                                                       *
     H* 03/23/17 C:136 Project: 148WAC                                        *
     H*                Add mode to OutputWindow procedure that bypasses       *
     H*                the EXFMT on the window and defaults the values        *
     H*                into the *LDA from App Config Output Control.          *
     H*                                                                       *
     H* 12/22/16 C:65  Project: SY371                                         *
     H*                Add checkboxes for holding/saving output to window.    *
     H*                                                                       *
     H* 12/01/16 C:136 Project: SY371                                         *
     H*                Add an optional parm (inCnlHide) to OutputWindow       *
     H*                that hides the Cancel button. Changes not commented.   *
     H*                                                                       *
     H* 10/07/16 C:65  Project: SY371                                         *
     H*                Update positions on LDA to prevent overlap with other  *
     H*                processes which utilize it.                            *
     H*                                                                       *
     H* 08/29/16 C:65  Project: SY371                                         *
     H*                Change interactive launch URLs to use the template     *
     H*                XML file name.                                         *
     H*                                                                       *
     H*  08/04/16 C:65  Project: SY371                                        *
     H*                 Remove directory field as well - it was unused.       *
     H*                                                                       *
     H* 07/28/16 C:65  Project: SY371                                         *
     H*                Add a procedure for displaying an "Output Choices"     *
     H*                window for Scriptura enabled processes.  This window   *
     H*                will allow selection of Darts, printer, email, etc.    *
     H*                Add email edits to Documents prompt as well.           *
     H*                                                                       *
     H* 06/02/16 C:136 Support: 41663                                         *
     H*                Don't let user open up more interactive windows than   *
     H*                allowed in Application Configuration                   *
     H*                                                                       *
     H* 06/01/16 C:65  Project: SY371                                         *
     H*                Change directory based on module file for LaunchDoc.   *
     H*                                                                       *
     H*  04/21/16 C:302 Project: 25WUK                                        *
     H*                 Update the documents window API to utilize the new    *
     H*                 functionality of the SCFLG5 and to call the TASB      *
     H*                 exit program.                                         *
     H*                                                                       *
     H* 04/04/16 C:65  Project: SY371                                         *
     H*                Use new language option from STLETTER.                 *
     H*                                                                       *
     H* 04/01/16 C:65  Project: SY371                                         *
     H*                Add LaunchDoc procedure to be used for launching       *
     H*                browser windows for interactive documents.             *
     H*                                                                       *
     H* 03/01/16 C:62  Project: SY251                                         *
     H*                Exclude Auto Letter only docuemnts.                    *
     H*                                                                       *
     H* 03/01/16 C:65  Project: SY251                                         *
     H*                Default printer from user parms if one is entered and  *
     H*                template is set up to allow user to change printer.    *
     H*                                                                       *
     H* 01/06/16 C:65  Project: SY251                                         *
     H*                Populate quote field for later use in Darts remark.    *
     H*                Change the language field to use a code value instead  *
     H*                and pass this value along in the interactive URL.      *
     H*                                                                       *
     H* 01/05/16 C:65  Project: SY251                                         *
     H*                Chain to employer master during mass policy runs.      *
     H*                Increase variable for submitting jobs to 5.0 to match  *
     H*                total possible size of array.                          *
     H*                                                                       *
     H* 11/24/15 C:65  Project: SY371                                         *
     H*                Populate DATA/DARTS flags in XML prep record for       *
     H*                calling work file build programs and writing Darts.    *
     H*                Remove unneccsary parms from S4400C call.              *
     H*                                                                       *
     H* 11/18/15 C:65  Project: SY251                                         *
     H*                Change printer tray dropdown to return/use tray field  *
     H*                instead of key field.                                  *
     H*                                                                       *
     H* 10/08/15 C:65  Project: SY371                                         *
     H*                Remove coding which generated the XML file name.       *
     H*                STLETTER now generats the name and write the XMLPREP   *
     H*                record to reserve it.                                  *
     H*                                                                       *
     H*                                                                       *
     H*************************************************************************
     fSPILETTER cf   e             workstn usropn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflLetters:docRRN)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     UsrOpn
     f                                     include(rcdSysTbl)
     FWDELP     IF   E             DISK    rename(pel01:pel0p) usropn           wcms
     FWMEML     IF   E           K DISK    rename(pem01:pem00) usropn           wcms
     FWMCMP     IF   E             DISK    rename(pcm01:pcm0p) usropn           wcms
     FWMCMLS    IF   E           K DISK    rename(pcm01:pcm0s) usropn           wcms
     F                                     infds(dbfds2)
     FWMCHL     IF   E           K DISK    rename(pch01:pch00) usropn           wcms
     FWMAGP     IF   E             DISK    rename(pag01:pag0p) usropn           wcms
     FWMFNL     IF   E           K DISK    rename(pfn01:pfn00) usropn           wcms
     F*** begin add ***  09/25/24  ************************************
     FWDF2L     IF   E           K DISK    rename(pf201:pf200) usropn           wcms
     F*** end add *****  09/25/24  ************************************
     FWDELL3    IF   E           K DISK    rename(pel01:pel03) usropn           wcms
     F                                     infds(dbfds)
     FFMCMP     IF   E             DISK    rename(pcm01:mcm0p) usropn           manc
     F                                     prefix(FM:2)
     FFMCML1    IF   E           K DISK    rename(pcm01:mcm01) usropn           manc
     F                                     prefix(FM:2)
     FUMPCP     IF   E             DISK    rename(ppc01:mpc0p) usropn           manc
     FFMSML     IF   E           K DISK    rename(psm01:msm00) usropn           manc
     FFMFNL     IF   E           K DISK    rename(pfn01:mfn00) usropn           manc
     FFMEML     IF   E           K DISK    rename(pem01:mem00) usropn           manc
     FSTDLL1    IF   E           K DISK    rename(pdl01:pdl01)
     FSTDEL     IF   E           K DISK    rename(pde01:pde00)
     FSTSCP     IF   E             DISK    rename(psc01:psc0p)
     FSTP1L     IF   E           K DISK    rename(pp101:pp100)
     FSMUPL     IF   E           K DISK    rename(pup01:pup00)
     FSMCOL     IF   E           K DISK    rename(pco01:pco00)
     F*** begin add ***  01/23/25  ****************************************
     fstgtl     if   e           k disk    rename(pgt01:pgt00)
     F*** end add *****  01/23/25  ****************************************
     FXMLPREL1  UF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSTSPL     IF   E           K DISK    rename(psp01:psp00)
     F*** begin add ***  10/16/24  ********************************************
     FSTLDP     UF A E             DISK    rename(pld01:pld0p) usropn

     dSleep            pr            10i 0 extproc('sleep')
     d seconds                       10u 0 value

     d GetObjLib       pr                  extpgm('RTVOBJLIB')
     d Object                        10    const
     d Type                           7    const
     d Library                       10

     dSendDTAQ         pr                  extpgm('QSNDDTAQ')
     d pDataq                        10a   const
     d pDataqLib                     10a   const
     d pEntLength                     5  0 const
     d pData                         50a   const

     d ReceiveDTAQ     pr                  extpgm('QRCVDTAQ')
     d ##DTAQ                        10    const
     d ##LIB                         10    const
     d ##LEN                          5p 0
     d ##DATA                       250
     d ##WAIT                         5p 0 const
     d ##KEYO                         2    const
     d ##KEYL                         3p 0 const
     d ##KEY                         14    const
     d ##SLEN                         3p 0 const
     d ##SDATA                       10    const

     D documents       DS                  DIM(9999) qualified
     D  documentName                100    varying
     D  documentID                  100    varying
     F*** end add *****  10/16/24  ********************************************

     d*** Begin Chg *** 09/23/22 *******************************************
     D*SVLDA           DS          1024    DTAARA(*LDA)
     D*xxMultiG                       1    overlay(svlda:127)
     D*xxDefDart                      1    overlay(svlda:128)
     D*xxHold                         1    overlay(svlda:189)
     D*xxSave                         1    overlay(svlda:190)
     D*xxDarts                        1    overlay(svlda:251)
     d*xxRMCD                         5    overlay(svlda:252)
     d*xxCopyOut                      1    overlay(svlda:257)
     d*xxPrint                        1    overlay(svlda:258)
     d*xxPrinter                     10    overlay(svlda:259)
     d*xxTray                        50    overlay(svlda:269)
     d*xx#CopiesA                     3    overlay(svlda:319)
     d*xxEmail                        1    overlay(svlda:322)
     d*xxToEmail                    200    overlay(svlda:323)
     d*xxSubject                    150    overlay(svlda:523)
     d*xxBody                       350    overlay(svlda:673)
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxMultiG                       1    overlay(svlda:127)
     D xxDefDart                      1    overlay(svlda:128)
     D xxHold                         1    overlay(svlda:189)
     D xxSave                         1    overlay(svlda:190)
     D xxDarts                        1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopyOut                      1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       200    overlay(svlda:673)
     d xxPrintI                       1    overlay(svlda:873)
     d xxPrinterI                    10    overlay(svlda:874)
     d xxTrayI                       50    overlay(svlda:884)
     d xxPrintA                       1    overlay(svlda:934)
     d xxPrinterA                    10    overlay(svlda:935)
     d xxTrayA                       50    overlay(svlda:945)
     d*** End   Chg *** 09/23/22 *******************************************
     d clientID        s              3    inz('000')

     d index           s              3  0 inz(1)
     d x               s              5  0 inz(1)
     d xxRRN           s              9  0
     d tabNameCodes    s           2000
     d xxTabName       s             50
     d xxOutDoc        s             50
     D namesArr        s             50    DIM(40)
     d urlCount        s              2  0 inz(1)
     d fldValue        s            100    inz('/SCR_DTM/')
     D svFiltDesc      s                   like(scFiltDesc)
     D strFound        s               n
     d strLang         s             60
     d strLangCode     s            250
     d message         s            250
     d messageType     s             30
     D recArr          s              1    DIM(6)
     d xxPolFrom       s              7  0
     d xxPolTo         s              7  0
     d xxSysDt         s              7  0
     d xxCxDt          s              7  0
     d maxIntRequests  s                   like(SC1101)
     d*** begin add ***  01/23/25  ****************************************
     d dcDocID         s            100    inz(' ')
     D xxKey           s                   like(GTKey)
     D xxTbid          s                   like(GTTbid)
     D strCommand      s           1000    inz(' ')

     d QCmdExc         pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const
     d*** end add *****  01/23/25  ****************************************

     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')

     D                 ds
     D  URL                    1  10000
     D                                     DIM(20)
     D  scURL01                1    500
     D  scURL02              501   1000
     D  scURL03             1001   1500
     D  scURL04             1501   2000
     D  scURL05             2001   2500
     D  scURL06             2501   3000
     D  scURL07             3001   3500
     D  scURL08             3501   4000
     D  scURL09             4001   4500
     D  scURL10             4501   5000
     D  scURL11             5001   5500
     D  scURL12             5501   6000
     D  scURL13             6001   6500
     D  scURL14             6501   7000
     D  scURL15             7001   7500
     D  scURL16             7501   8000
     D  scURL17             8001   8500
     D  scURL18             8501   9000
     D  scURL19             9001   9500
     D  scURL20             9501  10000

     D launchArr       s            400    DIM(41900)
     d launchCount     s              5  0 inz(1)

     D                 DS
     D  xxDocLaunch            1    400
     D  xdlRmcd                1      5
     D  xdlCat                 6     10
     D  xdlSubC               11     15
     D  xdlDcid               16     76
     D  xdlDtasrc             77    137
     D  xdlOutFile           138    198
     D  xdlOutFold           199    299
     D  xdlOutDir            300    400

     D                 DS
     D  scProvFein             1     12  0
     D  xxProvFein             1      9  0
     D  xxProvMod             10     12  0

     D psds          esds                  extname(szq1p)

     D dbfds           ds
     D  dbrrn                397    400b 0
     D dbfds2          ds
     D  dbrrn2               397    400b 0

     d S4400C          pr                  ExtPgm('S4400C')
     d   DTASRC_                     61    const

     d S4401C1         pr                  ExtPgm('S4401C1')
     d   DTASRC_                     61    const
     d   javamsg_                   250    const

     d STLETTER        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)

     d SNextDocID      pr                  ExtPgm('SNXTDCID')
     d   $#Fold_                     80
     d   $#Dcid_                     12
     d   QQPre_                       2    const
     d   QQCo#_                       3    const
     d   QQFnd_                       3    const
     d   QQRtcd_                      1    const
     d   QQSub_                       5    const
     d   QQPFlr_                     80

     d DocRoot         pr                  ExtPgm('RTVDOCROOT')
     d   $#Root_                     10

     d SA030R          pr                  ExtPgm('SA030R')
     d   $#SUFX_                      3
     d   $#FLRP_                     80

     d CheckQuote      pr                  ExtPgm('CHKQUOTE')
     d   quote_                       1

     d ExitProgram     pr                  ExtPgm(SCKey9)
     d   XMLFileName                 60

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      *** Beg Chg *** 01/20/2023
     D***strRRNType      s             25    inz(' ')
     D strRRNType      s            100    inz
      *** End Chg *** 01/20/2023
     ddsEmail        e ds                  extname(sPiEmail:RcdEmails)
     dptrEmail         s               *   inz
     D*** Beg Add *****  11/21/24  ************************************
     D ##TOemail       s           1000
     D tempEmail       s            100
     D curPos          s             10  0
     D nextComma       s             10  0
     D scemailL        s             10  0
     D scemailAL       s             10  0
     D*** End Add *****  11/21/24  ************************************

     d/copy sPrXML
     d/copy sPrEmail
     D/copy sPrDate
     D/copy sPrUsrPrms
     d/copy sPrGenPop
     D/copy sPrLetter
     d/copy sPrGenErr
     d/copy sPrString

     P*=====================================================================
     P* DocWindow - Display popup window allow documents/letters to be selected and
     P*             generated.
     P*          Parm 1 = Relative Record #             (9.0S)
     P*          Parm 2 = Category (PS=WDELP, CS=WMCMP) (  5A)
     P*          Parm 3 = Mode (mode)                   (  1A)  <optional>
     P*=====================================================================
     PDocWindow        B                   Export
     DDocWindow        PI
     D inRRN                          9  0 const
     D inCatg                         5    const
     D inMode                         1    const options(*nopass)
     D*
     D*
     D keyCo#          s                   like(ELCO#)
     D keyFnd          s                   like(ELFND)
     D keyEmp#         s                   like(ELEMP#)
     D keyDiv          s                   like(ELDIV)
     D keyFyr          s                   like(ELFYR)
     D keyCase         s                   like(CMCASE) inz(0)
     D keySub#         s                   like(FMSUB#)
     D keyCert         s                   like(PCCERT)
     D keyExt          s                   like(PCEXT)
     D*
     D xxMode          s              1
     D x#RRN           s              9  0
     D gridCount       s              3  0
     D*
     D string          s             10
     D xxPre           s              2
     D*
     d $#Mode          s              1    inz(*blank)
     d xmlCheck        s              1    inz(*off)
     d xxDate          s              8  0 inz
     d error           s              1    inz(*off)
     d errorMass       s               n   inz(*off)
     d massClaim       s               n   inz(*off)
     d MassPolicy      s               n   inz(*off)
     d clmStat         s              1    inz(' ')
     d noValid         s               n   inz(*off)
     d $#CAT           s              5    inz(*blanks)
     d $#SubC          s              5    inz(*blanks)
     d $#DCID          s             61    inz(*blanks)
     d DTASRC          s             61    inz(*blanks)
     d xmlFile         s             60    inz(*blanks)
     d xmlFileClt      s             60    inz(*blanks)
     d outFile         s             61    inz(*blanks)
     d templFile       s             60    inz(*blanks)
     d myDate          s               d
     d javaMsg         s            250    inz(*blanks)
     d strType         s            100    inz(*blanks)
     D*** begin add ***  09/25/24  ************************************
     D xxBegPos        s              3  0
     D*** end add *****  09/25/24  ************************************

     C*
      /Free

       If %parms >= 3;
          xxMode = inMode;
       Endif;
       GetClientID(clientID);
       Clear namesARR;
       //*** Begin Add *** 09/23/22 ***************************************
       isPolicy = *Off;
       massMode = *Off;
       //*** End   Add *** 09/23/22 ***************************************

       //** Establish key data based on incoming RRN# for single requests.
       If inRRN <> 0;
          xxRRN = inRRN;
          showWindow = *on;
          massMode   = *off;
          massPolicy = *off;
          massClaim  = *off;
          massWCMS   = *off;
          scCnlText = 'Cancel';

          Select;
          When inCatg = 'PS';
             Open WDELP;
             Chain inRRN pel0p;
             Close WDELP;
             If not %found;
                Return;
             Endif;
             keyCO#  = ELCO#;
             keyFND  = ELFND;
             keyEMP# = ELEMP#;
             keyDIV  = ELDIV;
             keyFYR  = ELFYR;
             $#Mode = 'E';
             showMC = *off;
             scDocHdg = 'Policy Documents';
             scPolClmHd = 'Policy:';
             Clear scPolClm;
             scPolClm = %EditC(ELFnd:'X') + '-' + %EditC(ELEmp#:'X') + '-' +
              %EditC(ELFyr:'X');
             Open WMEML;
             Chain (keyCo#:keyFnd:keyEmp#:keyDiv) pem00;
             Close WMEML;
             scPolClm = %trim(scPolClm) + ', ' + %trim(EMName + EMNam2);
             //*** Begin Add *** 09/23/22 *********************************
             isPolicy = *On;
             //*** End   Add *** 09/23/22 *********************************
          When inCatg = 'CS';
             Open WMCMP;
             Chain inRRN pcm0p;
             Close WMCMP;
             If not %found;
                Return;
             Endif;
             keyCO#  = CMCO#;
             keyFND  = CMFND;
             keyEMP# = CMEMP#;
             keyDIV  = CMDIV;
             keyFYR  = CMFYR;
             keyCASE = CMCASE;
             $#Mode = 'C';
             showMC = *off;
             scDocHdg = 'Claim Documents';
             scPolClmHd = 'Claim:';
             scPolClm = %EditC(CMfnd:'X') + '-' + %EditC(CMfYr:'X') + '-' +
              %EditC(CMcase:'X') + ', ' + %trim(CMLsn) + ', ' + %trim(CMFsn);
          When inCatg = 'MC';
             Open FMCMP;
             Chain inRRN mcm0p;
             Close FMCMP;
             If not %found;
                Return;
             Endif;
             keyCO#  = FMCO#;
             keySUB# = FMSUB#;
             keyFND  = FMFND;
             keyEMP# = FMEMP#;
             keyDIV  = FMDIV;
             keyFYR  = FMFYR;
             keyCASE = FMCASE;

             Exsr ProcessMCFiles;

             $#Mode = 'M';
             showMC = *on;
             scDocHdg = 'Medical Case Documents';
             scPolClmHd = 'Case#:';
             scPolClm = %EditC(keySub#:'X') + '-' + %EditC(keyFnd:'X') + '-' +
              %EditC(keyFyr:'X') + '-' + %EditC(keyCase:'X') + ', ' +
              %trim(FMLSN) + ', ' + %trim(FMFSN) + ' ' + %trim(FMMID);
          When inCatg = 'PC';
             Open UMPCP;
             Chain inRRN mpc0p;
             Close UMPCP;
             If not %found;
                Return;
             Endif;
             keyCO#  = PCCO#;
             keySUB# = PCSUB#;
             keyFND  = PCFND;
             keyFYR  = PCFYR;
             keyCASE = PCCASE;
             keyCERT = PCCERT;
             keyEXT  = PCEXT;

             Open FMCML1;
             Chain (keyCo#:keySub#:keyFnd:keyFyr:keyCase) mcm01;
             Close FMCML1;
             If not %found;
                Return;
             Endif;
             keyEMP# = FMEMP#;
             keyDIV  = FMDIV;

             Exsr ProcessMCFiles;

             $#Mode = 'P';
             showMC = *on;
             scDocHdg = 'Pre-Certification Documents';
             scPolClmHd = 'Pre-Cert:';
             scPolClm = %EditC(keySub#:'X') + '-' + %EditC(keyFnd:'X') + '-' +
              %EditC(keyFyr:'X') + '-' + %EditC(keyCase:'X') + ' Cert: ' +
              %trim(keyCert) + ' Ext: ' + %Editc(keyExt:'X');
          Other;
             Return;
          Endsl;

       Else;

       //** Set fields for mass requests.
          showWindow = *off;
          massMode   = *on;
          apShowMenu = *off;
          scCnlText = 'Exit';

          Select;
          When inCatg = 'PS';
             scDocHdg = 'Policy Documents';
             massPolicy = *on;
             massWCMS   = *on;
             $#Mode = 'E';
             showMC = *off;
             dtRangeHdg = 'Policy Effective Date Range';
             cxHeading = 'Include Cancelled Policies';
             messageType = 'Policies';
             //*** Begin Add *** 09/23/22 *********************************
             isPolicy = *On;
             //*** End   Add *** 09/23/22 *********************************
          When inCatg = 'CS';
             scDocHdg = 'Claim Documents';
             massClaim = *on;
             massWCMS  = *on;
             $#Mode = 'C';
             showMC = *off;
             dtRangeHdg = 'Claim Accident Date Range';
             cxHeading = 'Include Closed Claims';
             messageType = 'Claims';
          When inCatg = 'MC';
             scDocHdg = 'Medical Case Documents';
             $#Mode = 'M';
             showMC = *on;
             messageType = 'Medical Cases';
          When inCatg = 'PC';
             scDocHdg = 'Pre-Certification Documents';
             $#Mode = 'P';
             showMC = *on;
             messageType = 'Pre-Certifications';
          Endsl;

          Exsr ProcessMassFiles;

       Endif;

       //* Load the RRN into screen file.
       scRRN = inRRN;

       //* Read the Scriptura module file.
       Read psc0p;

       If SCFlg2 = 'Y';
          visEmail = *on;
       Else;
          visEmail = *off;
       Endif;

       Select;
       When showMC = *on and visEmail = *on;
          prmDlgHgt = 845;
          prmDlgWdt = 715;
          cnlLeft = 855;
          cnlTop = 940;
          sbmLeft = 975;
          sbmTop = 940;
          xRep = 385;
          yRep = 665;
       When visEmail = *on;
          prmDlgHgt = 585;
          prmDlgWdt = 715;
          cnlLeft = 855;
          cnlTop = 680;
          sbmLeft = 975;
          sbmTop = 680;
          xRep = 0;
          yRep = 0;
       When showMC = *on;
          prmDlgHgt = 545;
          prmDlgWdt = 545;
          cnlLeft = 670;
          cnlTop = 640;
          sbmLeft = 790;
          sbmTop = 640;
          xRep = 385;
          yRep = 340;
       Other;
          prmDlgHgt = 265;
          prmDlgWdt = 545;
          cnlLeft = 670;
          cnlTop = 360;
          sbmLeft = 790;
          sbmTop = 360;
          xRep = 0;
          yRep = 0;
       Endsl;

       If SCXMLF <> ' ';
          fldValue = %trim(SCXMLF);
       Endif;

       Open spiLetter;
       Open stActPnl;

       //* Populate field to enable iDarts button to be enabled for pop up.
       scShowIDRT = 'Y';

       Exsr LoadTabNames;

       Exsr LoadGrids;

       If noDocs = *on;

          Exfmt rcdLetters;

          Close spiLetter;
          Close stActPnl;
          Return;
       Endif;

       Dou btnCancel = *on;

          If massMode = *on;
             APnlHeight = 920;
             Write rcdSysTbl;
          Endif;

          Exfmt rcdLetters;

          If massMode = *on;
             Read rcdSysTbl;
          Endif;

          // Perform actions on screen.
          Select;

          When btnRefresh = *On or scChgUsrPr = *on;
             btnRefresh =*Off;
             If massMode = *on;
                Exsr ProcessMassFiles;
             Endif;
             Exsr LoadGrids;

          // Load the grid on tab click.
          When tabClick = *On;
             tabClick = *off;
          //***** Beg Chg *** 11/08/22 ******************************
          //***Exsr LoadGrids;
             scSubCat = namesarr(tabindex + 1);
             write rcdletters;
          //***** End Chg *** 11/08/22 ******************************

          // Filter the document grid
          When btnFilter = *on;
             Exsr ProcessFilter;
             Exsr LoadGrids;

          // Filter the document setup grid
          When btnClrFil = *on;
             scFiltDesc = ' ';
             showClrFil = *off;
             Exsr LoadGrids;

          // Process the selected document requests.
          When btnSelect = *On;
             btnSelect = *off;

             errorMass = *off;
             If massMode = *on;
                Exsr ErrorCheckMass;
             Endif;

             If not errorMass;
                Exsr ProcessSelection;
             Endif;

          // Load the grid on tab click.
          When btnCancel = *On;
             btnCancel = *off;
             Leave;

          Other;

             //* test window
             PopOneBtn('Select/Enter and Cancel have not been pressed.'
              :'Document Processing':'Ok');

          Endsl;

       Enddo;


       Close spiLetter;
       Close stActPnl;

       Return;
       //*********************************************************************
       //*
       //* Process Mass files.
       //*
       //*********************************************************************
       Begsr ProcessMassFiles;

          Chain Q1User pup00;
          keyCo# = UPCo#;
          keyFnd = UPFnd;

          Chain keyCo# pco00;
          If COname <> ' ';
             scCompName = COName;
          Endif;

          Select;
          When inCatg = 'MC' or inCatg = 'PC';
             keySub# = UPSub#;
             Open FMFNL;
             Chain (keyCo#:keySub#:keyFnd) mfn00;
             Close FMFNL;
          When inCatg = 'PS' or inCatg = 'CS';
             Open WMFNL;
             Chain (keyCo#:keyFnd) pfn00;
             Close WMFNL;
          Endsl;

          scGroup = %EditC(keyFnd:'X') + ', ' + %trim(FNName);

       Endsr;
       //*********************************************************************
       //*
       //* Process the MC related files.
       //*
       //*********************************************************************
       Begsr ProcessMCFiles;

          Open FMSML;
          Chain (keyCo#:keySub#) msm00;
          Close FMSML;

          Open FMFNL;
          Chain (keyCo#:keySub#:keyFnd) mfn00;
          Close FMFNL;

          Open FMEML;
          Chain (keyCo#:keySub#:keyFnd:keyEmp#:keyDiv) mem00;
          Close FMEML;

       Endsr;
       //*********************************************************************
       //*
       //* Error check mass request fields.
       //*
       //*********************************************************************
       Begsr ErrorCheckMass;

          Clear msPolFrom;
          Clear erPolFrom;
          Clear msPolTo;
          Clear erPolTo;

          Select;
          When massPolicy or massClaim;

             xxDate = dtCheck(scPolFrom);
             If xxDate = 0;
                msPolFrom = 'WCR0500';
                erPolFrom = *on;
                errorMass = *on;
             Else;
                scPolFrom = xxDate;
             Endif;

             xxDate = dtCheck(scPolTo);
             If xxDate = 0;
                msPolTo = 'WCR0500';
                erPolTo = *on;
                errorMass = *on;
             Else;
                scPolTo = xxDate;
             Endif;

             If erPolFrom <> *on and erPolTo <> *on;
                If dtCymd(scPolTo) < dtCymd(scPolFrom);
                   erPolTo = *on;
                   msPolTo = 'WCR0529';
                   errorMass = *on;
                Endif;
             Endif;

          Endsl;

       Endsr;
       //*********************************************************************
       //*
       //* Process Selection
       //*
       //*********************************************************************
       Begsr ProcessSelection;

          Clear dspWindow;
          Clear launchArr;
          launchCount = 1;
          Clear URL;
          urlCount = 1;
          clear scOutFld1;

          Readc sflLetters;

          If %eof();
             PopOneBtn('No documents have been selected for processing.'
              :'Document Selection':'Ok');
             Leavesr;
          Endif;

          Dow not %eof();

             //*  Display prompt for selected document.
             If scDocSelec = 'Y';
                scDocSelec = ' ';
                Update sflLetters;
                maxIntRequests = SC1101;
             //*** begin add  ***  09/25/24  ********************************
                If scDocIntr = 'LiveDoc' and SCFlg6 = 'Y' and
                 not ListDocs(scDocName:dcDocID);
                   PopOneBtn('Selected interactive document is not setup in ' +
                    'Design and Compose.':'Document - ' + %trim(scDoc#):'Ok');
                Else;
             //*** end add  *****  09/25/24  ********************************
                   If urlCount <= maxIntRequests and scDocIntr <> ' '
                    or scDocIntr = ' ';
                      Exsr Prompt;
                   Else;
                      scOutFld1 = 'More interactive documents -
                       were selected than the maximum of ' +
                       %trim(%char(maxIntRequests)) + ' allowed.';
                   Endif;
             //*** begin add  ***  09/25/24  ********************************
                Endif;
             //*** end add  *****  09/25/24  ********************************

             Endif;

             Readc sflLetters;
          Enddo;

          // Display window to launch URLs.
          If urlCount <> 1;
             Exfmt rcdInterac;
          Else;
             // Alert users documents have been submitted for processing.
             If launchCount <> 1;
                Exfmt rcdSubmit;
             Endif;
          Endif;

          If launchCount <> 1;
             Exsr SubmitLaunch;
          Endif;

       Endsr;
       //*********************************************************************
       //*
       //* Process the filter window for documents grid.
       //*
       //*********************************************************************
       Begsr ProcessFilter;

       svFiltDesc = scFiltDesc;

       Dou filtCancel = *on or filtApply = *on;

          Exfmt rcdFilter;

       Enddo;

       If filtCancel = *on;
          scFiltDesc = svFiltDesc;
       Endif;

       If scFiltDesc <> ' ';
          showClrFil = *on;
       Else;
          showClrFil = *off;
       Endif;

       Endsr;
       //*********************************************************************
       //*
       //* Process the prompt window for selected document.
       //*
       //*********************************************************************
       Begsr Prompt;

          Exsr LoadPrompt;

          //* Only display the prompt if some of the defaults can be changed.
          If disbDARTS = *off or disbEmail = *off or disbPrint = *off
           or showMC = *on;
             Dou btnPrmptCx = *on or btnPrmptSb = *on;
                Clear btnPrmptSb;
                Clear btnPrmptCx;

                If scPrinter <> ' ';
                   scTrayChoi = 'P2KEY = ''' + %trim(scDocPrin) + '''';
                Endif;

                If scPrint <> 'Y';
                   disbPrint1 = *on;
                Else;
                   If scDocPrtC <> 'Y';
                      disbPrint1 = *on;
                   Else;
                      disbPrint1 = *off;
                   Endif;
                Endif;

                Exfmt rcdPrompt;

                Exsr ClearErrors;

                If btnPrmptSb = *on;
                   Exsr PromptCheck;
                   If error = *on;
                      btnPrmptSb = *off;
                   Endif;
                Endif;

             Enddo;

          Else;

             //* If no fields are accessible then flag the prompt as passed.
             btnPrmptSb = *on;

          Endif;

          //* Load up the recipient array if submit has passed.
          Clear recArr;

          If btnPrmptSb = *on and showMC = *on;

             x = 1;

             If scRecClaim = 'Y';
                recArr(x) = 'C';
                x = x + 1;
             Endif;

             If scRecProv = 'Y';
                recArr(x) = 'P';
                x = x + 1;
             Endif;

             If scRecSub = 'Y';
                recArr(x) = 'S';
                x = x + 1;
             Endif;

             If scRecGrp = 'Y';
                recArr(x) = 'G';
                x = x + 1;
             Endif;

             If scRecEmp = 'Y';
                recArr(x) = 'E';
                x = x + 1;
             Endif;

             If scRecOther = 'Y';
                recArr(x) = 'O';
                x = x + 1;
             Endif;

          Endif;

          //* Process the request if submit has been pressed.
          If btnPrmptSb = *on;
             If massMode = *off;
                Exsr SubmitPrompt;
             Else;
                Exsr MassSubmit;
             Endif;
          Endif;

       Endsr;
       //*********************************************************************
       //*
       //* Load the prompt fields.
       //*
       //*********************************************************************
       Begsr LoadPrompt;

          scPrTitle = 'Document Submission Prompt';

          scDarts = 'Y';

          If SCflg5 = 'Y' or SCflg5 = *Blanks;
             scDarts = 'Y';
          Else;
             scDarts = ' ';
          Endif;

          scPrint = ' ';
          scEmail = ' ';
          Clear scPrinter;
          Clear scTray;
          Clear scToEmail;
          Clear scToEmailA;
          Clear scSubject;
          Clear scBody;
          Clear scCopies;
          Clear scHoldOut;
          Clear scSaveOut;
          scProvFein = 0;

          //* Allow Resolve to Darts changed if system flagged.
          If SCFlg1 = 'Y';
             disbDARTS = *off;
             Clear tipDARTS;
          Else;
             disbDARTS = *on;
             tipDARTS = 'The system is set to not allow changes to the +
              iDARTS resolve checkbox.';
          Endif;

          //* Display Email fields if system flagged.
          Clear scToEmailA;
          If visEmail = *on;
             Clear dsEmail;
             ptrEmail = %addr(dsEmail);
             If scDocID <> *blanks;
                dsRunInfo.docId = %trim(scDocID);
             Else;
                dsRunInfo.docId = %trim(scDoc#);
             Endif;
             dsRunInfo.docCat = scDocCat;
             dsRunInfo.docSub = scDocSubC;
             dsRunInfo.docLang = scDocLang;
             LoadDefaultEmaildata(ptrEmail:dsRunInfo);
             scSubject = scEmlSub;
             scBody = scEmlBody;
             disbEmail = *on;
             If scDocMail = 'Y';
                disbEmail = *off;
             Else;
                //*** Beg Del *** 11/16/22 ***************************************
                //*** scEmlUsrO = *blank;
                //*** End Del *** 11/16/22 ***************************************
             Endif;
             If scEmlUsrO = 'Y';
                disbEmailO = *off;
                disbEmailR = *off;
                tipToEmail = 'Multiple email recipients should be separated ' +
                 'by a comma or a semicolon.';
             Else;
                disbEmailO = *on;
                disbEmailR = *on;
                tipToEmail = 'The document does not allow for changes to ' +
                 'this field.';
                tipSubject = 'The document does not allow for changes to ' +
                 'this field.';
                tipBody = 'The document does not allow for changes to ' +
                 'this field.';
             Endif;
             Clear dsEmail;
             ptrEmail = %addr(dsEmail);
             Reset dsRunInfo;
             Select;
             When scDocID <> *blanks;
                dsRunInfo.docId = %trim(scDocID);
             Other;
                dsRunInfo.docId = %trim(scDoc#);
             Endsl;
             dsRunInfo.docCat = scDocCat;
             dsRunInfo.docSub = scDocSubC;
             dsRunInfo.docLang = scDocLang;
             LoadDefaultEmaildata(ptrEmail:dsRunInfo);
             scSubject = scEmlSub;
             scBody = scEmlBody;
             If scSubject <> *blank or scBody <> *blank;
                scEmail = 'Y';
             Endif;
             //* Allow editing if subject or body are blank otherise user
             //* cannot correct the blank values to proceed.
             If scSubject = ' ' or scBody = ' ';
                disbEmailO = *off;
                tipBody = ' ';
                tipSubject = ' ';
             Endif;
             If massMode = *on;
                disbEmailR = *on;
                scToEmail = '*Email Recipients Defined by Contact/Output ' +
                 'Control and Distribution Type Settings*';
             Else;
                Clear strRRNType;
                Select;
                When inCatG = 'PS';
                   strRRNType = 'Policy';
                When inCatG = 'CS';
                   strRRNType = 'Claim';
                When inCatG = 'MC';
                   strRRNType = 'Medical Case';
                When inCatG = 'PC';
                   strRRNType = 'Pre Cert';
                Endsl;
                Reset dsRunInfo;
                Select;
                When scDocID <> *blanks;
                   dsRunInfo.docId = %trim(scDocID);
                Other;
                   dsRunInfo.docId = %trim(scDoc#);
                Endsl;
                dsRunInfo.docCat = scDocCat;
                dsRunInfo.docSub = scDocSubC;
                dsRunInfo.docLang = scDocLang;
                scToEmail = %trim(GetDefaultEmailRecipients(dsRunInfo:xxRRN:
                 strRRNType));
             Endif;
          Endif;

          Chain Q1User pup00;

          //* Display Printing fields if system flagged.
          If SCFlg3 = 'Y';
             disbPrint = *off;
             scPrinter = scDocPrin;
             If (UPPKey <> ' ' and scDocPrtC = 'Y') or
              (UPPKey <> ' ' and scPrinter = ' ');
                scPrinter = UPPKey;
             Endif;
             scTray    = scDocTray;
             scCopies  = scDocCopy;
             If scPrinter <> ' ';
                scPrint = 'Y';
             Endif;
             Clear tipPrint;
             Clear tipPrinter;
             Clear tipCopies;
             If scDocPrtC <> 'Y';
                disbPrint = *on;
                tipPrint   = 'The requested document does not allow changes +
                 to the print settings.';
                tipPrinter = 'The requested document does not allow changes +
                 to the print settings.';
                tipCopies  = 'The requested document does not allow changes +
                 to the print settings.';
             Endif;
          Else;
             disbPrint = *on;
             tipPrint   = 'The print module for Scriptura must be enabled +
              to access this field.';
             tipPrinter = 'The print module for Scriptura must be enabled +
              to access this field.';
             tipCopies  = 'The print module for Scriptura must be enabled +
              to access this field.';
          Endif;

          //* Information for recipients portion of prompt.
          If showMC = *on;
             scRecClmL  = 'Claimant:  ' + %trim(FMLSN) + ', ' +
              %trim(FMFSN) + ' ' + FMMID;
             scRecProvL = 'Provider:  ';
             scRecSubL  = 'Subscriber:  ' + %editc(keySub#:'X') + ' ' +
               %trim(SMName);
             scRecGrpL  = 'Group:  ' + %editc(keyFnd:'X') + ' ' +
               %trim(FNName);
             scRecEmpL  = 'Employer:  ' + %editc(keyEmp#:'X') + ' ' +
               %trim(EMName);

          Endif;

       Endsr;
       //*********************************************************************
       //*
       //* Clear the prompt error fields.
       //*
       //*********************************************************************
       Begsr ClearErrors;

          Clear error;

          Clear msPrint;
          Clear msPrinter;
          Clear msCopies;

          Clear erPrint;
          Clear erPrinter;
          Clear erCopies;

          Clear msRecClaim;
          Clear msProvFein;

          Clear erRecClaim;
          Clear erProvFein;

          Clear msAttnProv;
          Clear msAttnSub;
          Clear msAttnGrp;
          Clear msAttnEmpl;
          Clear msAttnOth;

          Clear erAttnProv;
          Clear erAttnSub;
          Clear erAttnGrp;
          Clear erAttnEmpl;
          Clear erAttnOth;

          Clear msOthName;
          Clear msOthAdr1;
          Clear msOthAdr2;
          Clear msOthCity;
          Clear msOthSt;
          Clear msOthZip;

          Clear erOthName;
          Clear erOthAdr1;
          Clear erOthAdr2;
          Clear erOthCity;
          Clear erOthSt;
          Clear erOthZip;

          Clear msEmail;
          Clear msToEmail;
          Clear msSubject;
          Clear msBody;

          Clear erEmail;
          Clear erToEmail;
          Clear erSubject;
          Clear erBody;
          Clear erToEmailA;
          Clear msToEmailA;
       Endsr;
       //*********************************************************************
       //*
       //* Process edits on the prompt.
       //*
       //*********************************************************************
       Begsr PromptCheck;

          If scPrint = 'Y' and scPrinter = ' ';
             error = *on;
             erPrinter = *on;
             msPrinter = 'A printer must be selected when Print option ' +
              'is chosen.';
          Endif;

          If scCopies < 0;
             error = *on;
             erCopies = *on;
             msCopies = '# of copies cannot be less than zero.';
          Endif;

          If scCopies <= 0 and scPrint = 'Y';
             error = *on;
             erCopies = *on;
             msCopies = '# of copies must be greater than zero when printing.';
          Endif;


          If scEmail <> 'Y';
          //* Email fields should be blank if not emailing.


          Else;
          //* Email fields must be populated if emailing.
             //*** Begin Add *** 09/22/22 ******************************
             //*** Begin Chg *** 05/02/23 ******************************
             // If scToEmail = ' ';
             If scToEmail = ' ' and scToEmailA = ' ';
                msToEmail = 'Email address is required if sending an' +
                             ' email.';
             //*** End Chg *** 05/02/23 ******************************
                error = *on;
                erToEmail = *on;
             EndIf;
             //*** End   Add *** 09/22/22 ******************************

             If scToEmailA <> ' ';
                If CheckEmailList(%trim(scToEmailA):msToEmailA);
                   error = *on;
                   erToEmailA = *on;
                Endif;
             Endif;
             If scSubject = ' ';
                error = *on;
                erSubject = *on;
                msSubject = 'Email subject must not be blank if not email' +
                 'ing.';
             Endif;
             If scBody = ' ';
                error = *on;
                erBody = *on;
                msBody = 'Email body must not be blank if not email' +
                 'ing.';
             Endif;

          Endif;

       //* Perform edits on the recipient section of the prompt when needed.
          If showMC = *on;

       //* At least one recipient must be selected.

             If scRecClaim = ' ' and scRecProv = ' ' and scRecSub = ' ' and
              scRecGrp = ' ' and scRecEmp = ' ' and scRecOther = ' ';
                error = *on;
                erRecClaim = *on;
                msRecClaim = 'At least one recipient must be selected.';
             Endif;

       //* Provider must be entered when corresponding recipient selected.

             If scRecProv <> ' ' and scProvFein = 0;
                error = *on;
                erProvFein = *on;
                msProvFein = 'Provider must be entered when correspon' +
                 'ding recipient checkbox has been selected.';
             Endif;

       //* Attention names cannot be entered w/o corresponding recipient selected.

             If scRecProv = ' ' and scAttnProv <> ' ';
                error = *on;
                erAttnProv = *on;
                msAttnProv = 'Attention name cannot be entered if correspon' +
                 'ding recipient checkbox is not selected.';
             Endif;

             If scRecSub = ' ' and scAttnSub <> ' ';
                error = *on;
                erAttnSub = *on;
                msAttnSub = 'Attention name cannot be entered if correspon' +
                 'ding recipient checkbox is not selected.';
             Endif;

             If scRecGrp = ' ' and scAttnGrp <> ' ';
                error = *on;
                erAttnGrp = *on;
                msAttnGrp = 'Attention name cannot be entered if correspon' +
                 'ding recipient checkbox is not selected.';
             Endif;

             If scRecEmpl = ' ' and scAttnEmpl <> ' ';
                error = *on;
                erAttnEmpl = *on;
                msAttnEmpl = 'Attention name cannot be entered if correspon' +
                 'ding recipient checkbox is not selected.';
             Endif;

             If scRecOther = ' ' and scAttnOth <> ' ';
                error = *on;
                erAttnOth = *on;
                msAttnOth = 'Attention name cannot be entered if correspon' +
                 'ding recipient checkbox is not selected.';
             Endif;

       //* Other information must be entered when Other recipient has been selected.

             If scRecOther <> ' ' and scOthName = ' ';
                error = *on;
                erOthName = *on;
                msOthName = 'Name must be entered when Other recipient has' +
                 ' been selected.';
             Endif;

             If scRecOther <> ' ' and scOthAdr1 = ' ';
                error = *on;
                erOthAdr1 = *on;
                msOthAdr1 = 'Address must be entered when Other recipient has' +
                 ' been selected.';
             Endif;

             If scRecOther <> ' ' and scOthCity = ' ';
                error = *on;
                erOthCity = *on;
                msOthCity = 'City must be entered when Other recipient has' +
                 ' been selected.';
             Endif;

             If scRecOther <> ' ' and scOthSt = ' ';
                error = *on;
                erOthSt = *on;
                msOthSt = 'State must be entered when Other recipient has' +
                 ' been selected.';
             Endif;

             If scRecOther <> ' ' and scOthZip = 0;
                error = *on;
                erOthZip = *on;
                msOthZip = 'Zip code must be entered when Other recipient has' +
                 ' been selected.';
             Endif;

       //* Other information cannot be entered when Other recipient has not been selected.

             If scRecOther = ' ' and scOthName <> ' ';
                error = *on;
                erOthName = *on;
                msOthName = 'Name cannot be entered when Other recipient has' +
                 ' not been selected.';
             Endif;

             If scRecOther = ' ' and scOthAdr1 <> ' ';
                error = *on;
                erOthAdr1 = *on;
                msOthAdr1 = 'Address cannot be entered when Other recipient ' +
                 'has not been selected.';
             Endif;

             If scRecOther = ' ' and scOthAdr2 <> ' ';
                error = *on;
                erOthAdr2 = *on;
                msOthAdr2 = 'Address cannot be entered when Other recipient ' +
                 'has not been selected.';
             Endif;

             If scRecOther = ' ' and scOthCity <> ' ';
                error = *on;
                erOthCity = *on;
                msOthCity = 'City cannot be entered when Other recipient has' +
                 ' not been selected.';
             Endif;

             If scRecOther = ' ' and scOthSt <> ' ';
                error = *on;
                erOthSt = *on;
                msOthSt = 'State cannot be entered when Other recipient has' +
                 ' not been selected.';
             Endif;

             If scRecOther = ' ' and scOthZip <> 0;
                error = *on;
                erOthZip = *on;
                msOthZip = 'Zip code cannot be entered when Other recipient ' +
                 'has not been selected.';
             Endif;

          Endif;

       Endsr;
       //*********************************************************************
       //*
       //* Submission for Mass request.
       //*
       //*********************************************************************
       Begsr MassSubmit;

          noValid = *on;

          Select;
          When massPolicy;
             Open wdell3;
             Open wmeml;
             xxPolFrom = DtCymd(scPolFrom);
             xxPolTo   = DtCymd(scPolTo);
             xxSysDt   = Dateto7(SystemDate());

             Setll (keyCo#:keyFnd:xxPolFrom) pel03;
             Reade (keyCo#:keyFnd) pel03;

             //* Read over employer detail file for each policy within policy date range.
             Dow not %eof;

                If ELPfdt <= xxPolTo;
                   Chain (ELCo#:ELFnd:ELEmp#:ELDiv) pem00;
                   xxCxDt = 0;
                   Select;
                   When EMEfd5 <> 0;
                      xxCxDt = EMCAD5;
                   When EMEfd4 <> 0;
                      xxCxDt = EMCAD4;
                   When EMEfd3 <> 0;
                      xxCxDt = EMCAD3;
                   When EMEfd2 <> 0;
                      xxCxDt = EMCAD2;
                   When EMEfd1 <> 0;
                      xxCxDt = EMCAD1;
                   Endsl;

                   If scActive = 'Y' or xxCxDt = 0 or (xxCxDt > xxSysDt);
                      keyEmp# = ELEmp#;
                      keyDiv  = ELDiv;
                      keyFyr  = ELFyr;

                      xxRRN = dbrrn;

                      Exsr SubmitPrompt;
                      noValid = *off;
                   Endif;

                   Reade (keyCo#:keyFnd) pel03;
                Else;
                   Leave;
                Endif;

             Enddo;

             Close wmeml;
             Close wdell3;

          When massClaim;
             Open wmcmls;
             Open wmchl;
             xxPolFrom = DtCymd(scPolFrom);
             xxPolTo   = DtCymd(scPolTo);
             xxSysDt   = Dateto7(SystemDate());

             Setll (keyCo#:keyFnd:xxPolFrom) pcm0s;
             Reade (keyCo#:keyFnd) pcm0s;

             //* Read over claim master file for each claim within accident date range.
             Dow not %eof;

                If CMAcdt <= xxPolTo;

                   clmStat = ' ';
                   SetGt (CMCo#:CMFnd:CMFyr:CMCase:xxSysDt) pch00;
                   Readpe (CMCo#:CMFnd:CMFyr:CMCase) pch00;
                   If not %eof;
                      clmStat = CHOcst;
                   Else;
                      clmStat = CMStat;
                   Endif;

                   If scActive = 'Y' or clmStat <> 'C';
                      keyEmp# = CMEmp#;
                      keyDiv  = CMDiv;
                      keyFyr  = CMFyr;
                      keyCase = CMCase;

                      xxRRN = dbrrn2;

                      Exsr SubmitPrompt;
                      noValid = *off;
                   Endif;

                   Reade (keyCo#:keyFnd) pcm0s;
                Else;
                   Leave;
                Endif;

             Enddo;

             Close wmchl;
             Close wmcmls;
          Endsl;

          If noValid;
             PopOneBtn('No ' + %trim(messageType) + ' meet the criteria for ' +
              'processing.':'Document Selection':'Ok');
          Endif;

       Endsr;
       //*********************************************************************
       //*
       //* Submit the prompt choices.
       //*
       //*********************************************************************
       Begsr SubmitPrompt;

          For x = 1 to 6;
             If recArr(x) <> ' ' or showMC = *off;
                Exsr GenerateXML;

               If SCKey9 <> *Blanks;
                  ExitProgram(xmlFileClt);
               Endif;

                If scDocINTR <> ' ';
                   Exsr LoadURL;
                Else;
                   Exsr LoadLaunch;
                   dspWindow = *on;
                Endif;

             Endif;

             If showMC = *off;
                Leave;
             Endif;

          Endfor;

       Endsr;
       //*********************************************************************
       //*
       //* Generate XML File name and write record to preparation file.
       //*
       //*********************************************************************
       Begsr GenerateXML;

          Clear xmlFile;
          Clear message;
          Clear string;

          //* Set the prefix for using system folder control for output location.
          Select;
          When inCatg = 'PS';
             xxPre = 'DL';
          When inCatg = 'CS';
             xxPre = 'CL';
          When inCatg = 'MC' or inCatg = 'PC';
             xxPre = 'ML';
          Endsl;

          //* Call StLetter to request an XML document name and write record.

          StLetter('XMLREQUEST':xmlFile:message:$#Mode:%editc(xxRRN:'X'):xxPre:
           string:%editc(keyCo#:'X'):%editc(keyFnd:'X'));

          Chain (xmlFile) xp100;

          XPCat  = inCatg;
          XPSubc = scDocSubC;
          XPType = scDocType;
          XPDcid = %SUBST(scDocID:1:60);
          XPRMCD = scDocRMCD;
      ***  Beg Add  ***  01/16/24  *** 153WAD ***********************
          XPCopyE = scCopyE;
      ***  End Add  ***  01/16/24  *** 153WAD ***********************
          Select;
          When scDocIntr = 'LiveDoc';
             XPIntr = 'Y';
          When scDocIntr = 'Form';
             XPIntr = 'F';
          Other;
             XPIntr = ' ';
          Endsl;
          XPFormP = scDocFormP;
          XPFormT = scDocFormT;
          XPInid = scDoc#;

          strLang = scDocLang;
          Clear strLangCode;

          StLetter('LANGUAGE':strLang:strLangCode);

          XPLang = %trim(strLangCode);

          XPPath = scDocPath;
          XPScrp = 'Y';
          XPData = 'Y';
          XPDart = scDARTS;
          XPHold = scHoldOut;
          XPSave = scSaveOut;
          XPForm = '*STD';

          If scPrinter <> ' ' and scPrint = 'Y';
             Chain (scPrinter) pp100;
             If %found;
                XPPnam = %trim(P1Name);
                If P1Type = 'N';
                   XPPunc = %trim(P1UNCN);
                Else;
                   XPOutq = %trim(P1KEY);
                   XPOLib = %trim(P1OLIB);
                Endif;
                XPCopy = scCopies;
                XPTray = scTray;
             Endif;
          Endif;

          XPMail  = %trim(scToEmail);
          XPSubj  = %trim(scSubject);
          XPBody  = %trim(scBody);
          If massMode = *on;
             Clear strRRNType;
             Select;
             When massPolicy;
                strRRNType = 'Policy';
             When massClaim;
                strRRNType = 'Claim';
             Endsl;
             Reset dsRunInfo;
             dsRunInfo.docId = %trim(scDocID);
             dsRunInfo.docCat = scDocCat;
             dsRunInfo.docSub = scDocSubC;
             dsRunInfo.docLang = scDocLang;
             XPMail = %trim(GetDefaultEmailRecipients(dsRunInfo:xxRRN:
              strRRNType));
          Endif;
          XPMailAdd  = %trim(scToEmailA);
          If scEmail <> 'Y';
             Clear XPMail;
             Clear XPMailAdd;
             Clear XPBody;
             Clear XPSubj;
          Endif;

          //* Populate recipient info for managed care letters.
          If showMC = *on;

             XPRType = recArr(x);

             Select;
             When XPRType = 'P';
                XPPFein = xxProvFein;
                XPPMod  = xxProvMod;
                XPAttn  = scAttnProv;
             When XPRType = 'S';
                XPAttn  = scAttnSub;
             When XPRType = 'G';
                XPAttn  = scAttnGrp;
             When XPRType = 'E';
                XPAttn  = scAttnEmpl;
             When XPRType = 'O';
                XPAttn  = scAttnOth;
                XPOName = scOthName;
                XPOAdr1 = scOthAdr1;
                XPOAdr2 = scOthAdr2;
                XPOCity = scOthCity;
                XPOSt   = scOthSt;
                XPOZip  = scOthZip;
             Endsl;

          Endif;

          CheckQuote(XPQte);

       //*** begin add ***  09/25/24  ************************************
          Open wdf2l;
          Chain (XPCO#:XPFND) pf200;
          If %found and XPFND <> 0 and F2SLogo <> ' ';
             XPLogo = F2SLogo;
             XPLogoPth = F2SPath;
             If SCFlg6 = 'Y' and XPIntr <> ' ';
                XPLogoPth = F2SURL;
             Endif;
          Else;
             Chain XPCo# pco00;
             If %found and COSLogo <> ' ';
                XPLogo = COSLogo;
                XPLogoPth = COSPath;
                If SCFlg6 = 'Y' and XPIntr <> ' ';
                   XPLogoPth = COSURL;
                Endif;
             Endif;
          Endif;
          Close wdf2l;
       //*** end add *****  09/25/24  ************************************

          XPPgm  = Q1PGM;
          XPUser = Q1USER;
          XPTime = Timeto6(SystemTime());
          XPDt   = Dateto7(SystemDate());

          Update xp100;

          outFile = XPFile;
          DTASRC = xmlFile;
          templFile = XPXMLT;
          xmlFileClt = xmlFile;
          If clientID = '025' and %scan('.PDF':XPFile) <> 0;
             xmlFileClt = %subst(XPFile:1:%scan('.PDF':XPFile)) + 'XML';
          Endif;

       Endsr;
       //*********************************************************************
       //*
       //* Load the URL names
       //*
       //*********************************************************************
       Begsr LoadURL;

          If urlCount <= 20;

             //* Build workfile for xml data.
             StLetter('DATA':xmlFile);

             //* call java pgm here for xml
             Clear javaMsg;
             S4401C1(DTASRC:javaMsg);

             If %SUBST(javaMsg:1:7) <> 'Success';
                message = %trim(javaMsg);
             Else;

                //* Check for xml file existance
                Clear message;
                StLetter('CHKXMLM':xmlFile:message);
             Endif;

             If message <> ' ';
                PopOneBtn(message:'Interactive Processing':'Ok');
             Else;

          //*** begin chg ***  01/23/25  **************************************
          //****URL(urlCount) = %trim(SCSURL) +
          //**** %trim(scDoc#) + '&language=' + %trim(XPLang) +
          //**** '&input=' + %trim(fldValue) + %trim(templFile);
          //****
          //****//* Build different URL for interactive forms.
          //****If scDocIntr = 'Form';
          //****   URL(urlCount) = %trim(SCURL6) + %trim(fldValue) +
          //****    %trim(templFile) + '&formtemplate=' + %trim(scDocFormT) +
          //****    '&outtemplatepath=' + %trim(scDocPath) + '&outtemplate=' +
          //****    %trim(scDocID) + '&formtemplatepath=' + %trim(scDocFormP);
          //****Endif;
                Select;
                When SCFlg6 = ' ' and scDocIntr = 'Form';
                   xxTBID = 'FILN';
                When SCFlg6 = ' ';
                   xxTBID = 'FILI';
                Other;
                   xxTBID = 'FILF';
                Endsl;

                Chain(n) (XPCo#:xxTBID) pgt00;

             //* Design and Compose enabled.
                If SCFlg6 = 'Y';
                //* Build different URL for interactive forms.
                   If scDocIntr = 'Form';
                      URL(urlCount) = %trim(SCURL3) + %trim(fldValue) +
                       %trim(templFile) + '&formtemplate=' + %trim(scDocFormT) +
                       %trim(lower(GTKey)) + '&outtemplate=' + %trim(scDocID) +
                       '&outtemplatepath=' + %trim(scDocPath);
                   Else;
                      URL(urlCount) = %trim(SCURL2) + %trim(dcDocID) +
                       '&xmlfile=' + %trim(fldValue) + %trim(templFile);
                   Endif;
                Else;
                //* Build different URL for interactive forms.
                   If scDocIntr = 'Form';
                      URL(urlCount) = %trim(SCURL6) + %trim(fldValue) +
                       %trim(templFile) + '&formtemplate=' + %trim(scDocFormT) +
                       %trim(lower(GTKey)) + '&outtemplatepath=' +
                       %trim(scDocPath) + '&outtemplate=' + %trim(scDocID) +
                       '&formtemplatepath=' + %trim(scDocFormP);
                   Else;
                   URL(urlCount) = %trim(SCSURL) + %trim(scDocName) +
                    %trim(lower(GTKey)) + '&language=' + %trim(XPLang) +
                    '&input=' + %trim(fldValue) + %trim(templFile);
                   Endif;
                Endif;
          //*** end chg *****  01/23/25  **************************************

                urlCount = urlCount + 1;
             Endif;

          Endif;

       Endsr;

       //*********************************************************************
       //*
       //* Load the Launch parameters
       //*
       //*********************************************************************
       Begsr LoadLaunch;

          $#DCID = %SUBST(scDocID:1:60) + '$';
          $#CAT = inCatg;
          $#SubC = scDocSubC;
          DTASRC = %SUBST(DTASRC:1:60) + '$';
          OutFile = %SUBST(OutFile:1:60) + '$';

          If launchCount <= 41900;

             Clear xxDocLaunch;
             xdlRmcd   = scDocRMCD;
             xdlCat    = $#CAT;
             xdlSubC   = $#SubC;
             xdlDcid   = $#DCID;
             xdlDtasrc = DTASRC;
             xdlOutFile = OutFile;
             xdlOutFold = fldValue + '$';

             launchArr(launchCount) = xxDocLaunch;

             launchCount = launchCount + 1;
          Endif;

       Endsr;

       //*********************************************************************
       //*
       //* Submit the launched jobs.
       //*
       //*********************************************************************
       Begsr SubmitLaunch;

          x = 1;
          Dow x < LaunchCount;
             xxDocLaunch = launchArr(x);
             S4400C(xdlDtasrc);
             x = x + 1;
          Enddo;

       Endsr;

       //*********************************************************************
       //*
       //* Load the tab names based on the departments.
       //*
       //*********************************************************************
       Begsr LoadTabNames;

          TabIndex = 0;
          index = 0;
          Clear tabNames;
          Clear tabNameCodes;
          noDocs = *on;

          //* Load up two character fields - one for tab department codes and one
          //* for the department descriptions to display.  Load array for processing
          //* the selected tab.

          Setll (keyCo#:inCatg) pdl01;
          Reade(n) (keyCo#:inCatg) pdl01;
          Dow not %eof(stdll1);
             // Skip if Auto Letter
             If DLauto <> 'Y';

                Chain(n) (keyCo#:inCatg:DLSubC) pde00;
                If %found;
                   xxTabName = %trim(DEDesc);
                Else;
                   xxTabName = %trim(DLSubC);
                Endif;

                If %lookup(DLSubC:namesArr) = 0 and DLStat = 'A'
                  and DEStat = 'A';
                   If tabNames = ' ';
                      tabNameCodes = DLSubC;
                      tabNames = xxTabName;
                   Else;
                      tabNameCodes = %trim(tabNameCodes) + ',' + %trim(DLSubC);
                      tabNames = %trim(tabNames) + ',' + %trim(xxTabName);
                   Endif;
                   index = index + 1;
                   namesArr(index) = DLSubC;
                   noDocs = *off;
                Endif;
             EndIf;

             Reade(n) (keyCo#:inCatg) pdl01;
          Enddo;

       Endsr;

       //*********************************************************************
       //*
       //* Load the grids on each tab.
       //*
       //*********************************************************************
       Begsr LoadGrids;

          gridCount = 1;

          clrDocGrd = *on;
          dspDocGrd = *off;
          dspDocCntl = *off;
       //***** Beg Add *** 11/08/22 *********************************
          scSubCat = namesarr(1);
       //***** End Add *** 11/08/22 *********************************
          Write rcdLetters;
          clrDocGrd = *off;
          dspDocGrd = *on;
          dspDocCntl = *on;
          docRRN = 0;

          Setll (keyCo#:inCatg) pdl01;
          Reade(n) (keyCo#:inCatg) pdl01;
          Dow not %eof(stdll1);

             // Skip if Auto Letter
             If DLauto <> 'Y';
                strFound = *on;
                If scFiltDesc <> ' ';
                   If %scan(%xlate(lo:up:%trim(scFiltDesc)):%xlate(lo:up:
                    DLDesc)) = 0;
                      strFound = *off;
                   Endif;
                Endif;

           //***** Beg Chg *** 11/08/22 *****************************
            //***If DLSubC = namesArr(TabIndex + 1) and DLStat = 'A' and strFound
                If %lookup(DLSubC:namesArr) <> 0 and DLStat = 'A' and strFound
           //***** End Chg *** 11/08/22 *****************************
                 and (massMode = *off or DLIntr = ' ');
                   Clear scDocFormT;
                   Clear scDocFormP;

                   scDocDesc = %trim(DLDesc);
                   scDocID   = %trim(DLDcID);
                   scDocCat  = %trim(DLcat);
                   scDocSubC = %trim(DLSubC);
                   scDocType = %trim(DLType);
                   scDoc#    = %trim(DLINID);
                   scDocRMCD = DLRMCD;
                   scDocCopy = DLCopy;
                   If DLINTR = 'Y';
                      scDocINTR = 'LiveDoc';
                      scDocName = %trim(DLINID);
                  //*** begin add ***  09/25/24  *********************************
                  //* Design and Compose enabled.
                      xxBegPos = %scan('.':DLINID);
                   //*** begin chg ***  01/23/25  ********************************
                   //*****   If SCFlg6 = 'Y' and xxBegPos <> 0;
                   //*****      scDocName = %trim(%subst(DLINID:1:xxBegPos-1));
                   //*****   Endif;
                      If xxBegPos <> 0;
                         scDocName = %trim(%subst(DLINID:1:xxBegPos-1));
                      Endif;
                   //*** end chg *****  01/23/25  ********************************
                  //*** end add *****  09/25/24  *********************************
                   Else;
                      scDocINTR = ' ';
                      scDocName = %trim(DLPath) + %trim(DLDcID);
                      If DLINTR = 'F';
                         scDocINTR = 'Form';
                         scDocFormT = DLFormT;
                   //*** begin add ***  01/23/25  ********************************
                         xxBegPos = %scan('.':DLFormT);
                         If xxBegPos <> 0;
                            scDocFormT = %trim(%subst(DLFormT:1:xxBegPos-1));
                         Endif;
                   //*** end add *****  01/23/25  ********************************
                         scDocFormP = DLFormP;
                      Endif;
                   Endif;
                   scDocForm = 'PDF';
                   scDocLang = DLLang;
                   scDocPath = DLPath;
                   scDocPrin = DLPrin;
                   scDocPrtC = DLHard;
                   scDocTray = DLTray;
                   scDocMail = DLMail;
                   scEmlUsrO = DLemlUsrO;
          //***** Beg Add *** 11/08/22 ******************************
                   scSubC = DLSubC;
          //***** End Add *** 11/08/22 ******************************
                   scDocTURN = ' ';
                   docRRN = docRRN + 1;

      ***  Beg Add  ***  01/16/24  *** 153WAD ***********************
                   scCopyE = DLCopyE;
      ***  End Add  ***  01/16/24  *** 153WAD ***********************

                   Write sflLetters;

                Endif;
             EndIf;

             Reade(n) (keyCo#:inCatg) pdl01;
          Enddo;

          docRRN = 1;

       Endsr;
       //*********************************************************************


      /End-Free
     PDocWindow        E
      *
     P*=====================================================================
     P* LaunchDoc - Generate XML file, build URL, and display window to launch
     P*             browser window for interactive documents.
     P*          Parm 1 = XML File Name                 ( 60A)
     P*=====================================================================
     PLaunchDoc        B                   Export
     DLaunchDoc        PI           250
     D inXML                         60    const
     D*
     d DTASRC          s             61    inz(*blanks)
     d xmlFile         s             60    inz(*blanks)
     d templFile       s             60    inz(*blanks)
     d javaMsg         s            250    inz(*blanks)
     D*
      /Free

       Clear message;

       //* Read the Scriptura module file.
       Read psc0p;

       If SCXMLF <> ' ';
          fldValue = %trim(SCXMLF);
       Endif;

       Chain(n) (inXML) xp100;

       //* Bail if no XML preparation record is found.
       If not %found;
          message = 'No preparation record available.';
          Return message;
       Endif;

       xmlFile = inXML;
       templFile = XPXMLT;

       //* Build workfile for xml data.
       StLetter('DATA':xmlFile);

       DTASRC = inXML + '$';

       //* call java pgm here for xml
       Clear javaMsg;
       S4401C1(DTASRC:javaMsg);

       If %SUBST(javaMsg:1:7) <> 'Success';
          message = %trim(javaMsg);
       Else;
          Clear message;

          //* Check for xml file existance
          StLetter('CHKXMLM':xmlFile:message);
       Endif;

       //* Bail if XML file was not created.
       If message <> ' ';
          Return message;
       Else;
          Open spiLetter;

          Clear URL;
          URL(1) = %trim(SCSURL) +
           %trim(XPInid) + '&language=' + %trim(XPLang) +
           '&input=' + %trim(fldValue) + %trim(templFile);

          Exfmt rcdInterac;

          Close spiLetter;

       Endif;

       Return message;

      /End-Free
     PLaunchDoc        E
      *
     P*=====================================================================
     P* OutputWindow - Display a window for output choiuces for Scriptura
     P*                enabled processes.  Returns off for cancel, on for
     P*                submit.
     P*
     P*          Parm  1 = Type of Process               (100A)
     P*          Parm  2 = Mass Process Flag             (  1A)
     P*          Parm  3 = Hide Cancel Button            (  1A)
     P*          Parm  4 = Do not show Window            (  1A)
     P*          Parm  5 = RRN of Policy/Claim/Etc       (9.0S)
     P*
     P*=====================================================================
     POutputWindow     B                   Export
     DOutputWindow     PI             1n
     D inType                       100    const
     D inMass                         1    const
     D inCnlHide                      1    const options(*nopass)
     D inMode                         1    const options(*nopass)               'M' to bypass wdw
     D inRRN                          9  0 const options(*nopass)
     D*
     d error           s              1    inz(*off)
     disDartsRequired  s              1n   inz
     d rrnFound        s               n   inz(*off)
     d strDisabledTip  s            200
      /Free

       If %parms > 3 and inMode = 'M';
          Exsr LoadWindow;
          Exsr LoadParms;
          Return *Off;
       Endif;

       Open spiLetter;

       Exsr LoadWindow;

       btnCancel = *off;
       btnSubmit = *off;
       Exsr ClearErrors;

       //* Display and process the output window.
       Dou btnCancel = *on or btnSubmit = *on;

          Exfmt rcdOutput;

          If btnSubmit = *on;
             Exsr OutputCheck;
             If error = *on;
                btnSubmit = *off;
             Endif;
          Endif;

       Enddo;

       Close spiLetter;

       If btnSubmit = *on;
          Exsr LoadParms;
          Return *on;
       Else;
          Return *off;
       Endif;

       //*********************************************************************
       //*
       //* Load parameters into the screen fields.
       //*
       //*********************************************************************
       Begsr LoadWindow;

          //* Read the Scriptura module file.
          Read psc0p;
          rrnFound = *off;
          Clear strRRNType;
          disbEmailT = *on;
          //*** Begin Add *** 09/23/22 ************************************
          isPolicy = *Off;
          //*** End   Add *** 09/23/22 ************************************

          tipSubject = ' ';
          tipBody    = ' ';
          tipCopy    = ' ';
          tipDarts   = ' ';
          tipPrint   = ' ';
          tipPrinter = ' ';
          tipToEmail = 'Multiple email recipients should be separated by a +
           comma or a semicolon.';
          //*** Begin Add *** 09/23/22 *********************************
          scPrintI   = ' ';
          scPrinterI = ' ';
          scTrayI    = ' ';
          scPrintA   = ' ';
          scPrinterA = ' ';
          scTrayA    = ' ';
          //*** End   Add *** 09/23/22 *********************************
          Clear scToEmailA;
          If SCFlg2 = 'Y';
             visEmail = *on;
             wdwHeight = 555;
             wdwWidth = 710;
             //*** Beg Chg ***** 06/09/23 ********************************
             //***cnlLeft = 840;
             cnlLeft = 945;
             //*** End Chg ***** 06/09/23 ********************************
             cnlTop = 590;
             //*** Beg Chg ***** 06/09/23 ********************************
             //***sbmLeft = 960;
             sbmLeft = 1055;
             //*** End Chg ***** 06/09/23 ********************************
             sbmTop = 590;
          Else;
             visEmail = *off;
             wdwHeight = 250;
             wdwWidth = 540;
             //*** Beg Chg ***** 06/09/23 ********************************
             ///***cnlLeft = 670;
             cnlLeft = 945;
             //*** End Chg ***** 06/09/23 ********************************
             cnlTop = 290;
             //*** Beg Chg ***** 06/09/23 ********************************
             //***sbmLeft = 790;
             sbmLeft = 1055;
             //*** End Chg ***** 06/09/23 ********************************
             sbmTop = 290;
          Endif;

          disbDarts = *on;
          disbEmail = *on;
          disbPrint = *on;
          disbPrint1 = *on;
          disbCopy  = *on;
          disbEmailO = *on;
          //*** Beg Add ***** 05/11/23 ******************************
          disbPrintI = *On;
          disbPrintA = *On;
          //*** End Add ***** 05/11/23 ******************************

          If inType <> ' ';
             Chain inType psp00;
             If %found;
                //*** Begin Add *** 09/23/22 ************************
                If SPcat = 'PS' And inMass = 'Y';
                   isPolicy = *On;
                EndIf;
                //*** End   Add *** 09/23/22 ************************
                If SPAllowD = 'Y';
                   disbDarts = *off;
                Endif;
                If SPAllowE = 'Y';
                   disbEmail = *off;
                   If SPemlUsrO = 'Y';
                      disbEmailO = *off;
                   Endif;
                Endif;
                If SPAllowP = 'Y';
                   disbPrint = *off;
                   //*** Beg Add ***** 05/11/23 **********************
                   disbPrintI = *Off;
                   disbPrintA = *Off;
                   //*** End Add ***** 05/11/23 **********************
                Endif;
                If SPAllowC = 'Y';
                   disbCopy  = *off;
                Endif;
                //*** Beg Add *** 07/25/23 ** WC2850 ****************
                //*** begin del ***  03/06/25  ****************************
                //*****If inMass = 'Y';
                //*****   scCopies = SPCpyM;
                //*****Else;
                //*****   scCopies = SPCpyS;
                //*****Endif;
                //*** end del *****  03/06/25  ****************************
                //*** End Add *** 07/25/23 ** WC2850 ****************
                If %parms > 4;
                   Select;
                   When SPCat = 'PS';
                      Open WDELP;
                      Chain(n) inRRN pel0p;
                      rrnFound = %found;
                      strRRNType = 'Policy';
                      Close WDELP;
                   When SPCat = 'CS';
                      Open WMCMP;
                      Chain(n) inRRN pcm0p;
                      rrnFound = %found;
                      strRRNType = 'Claim';
                      Close WMCMP;
                   When SPCat = 'AS';
                      Open WMAGP;
                      Chain(n) inRRN pag0p;
                      rrnFound = %found;
                      strRRNType = 'Agency';
                      Close WMAGP;
                   When SPCat = 'MC';
                      Open FMCMP;
                      Chain(n) inRRN mcm0p;
                      rrnFound = %found;
                      strRRNType = 'Medical Case';
                      Close FMCMP;
                   When SPCat = 'PC';
                      Open UMPCP;
                      Chain(n) inRRN mpc0p;
                      rrnFound = %found;
                      strRRNType = 'Pre Cert';
                      Close UMPCP;
                   Endsl;
                Endif;
                //*** begin add ***  03/06/25  ****************************
                If inMass = 'Y' and rrnFound = *off;
                   scCopies = SPCpyM;
                Else;
                   scCopies = SPCpyS;
                Endif;
                //*** end add *****  03/06/25  ****************************
             Endif;
          Else;
             If SCFlg1 = 'Y';
                disbDarts = *off;
             Endif;
             If SCFlg2 = 'Y';
                disbEmail = *off;
             Endif;
             If SCFlg3 = 'Y';
                disbPrint = *off;
                //*** Beg Add ***** 05/11/23 ************************
                disbPrintI = *Off;
                disbPrintA = *Off;
                //*** End Add ***** 05/11/23 ************************
             Endif;
             disbCopy = *off;
          Endif;

          If %parms >= 3 and inCnlHide = 'Y';
             isCnlHide = *on;
          Else;
             isCnlHide = *off;
          Endif;

          //* Set the tool tip info for disabled areas.
          strDisabledTip = 'This process has been set up to not allow '+
              'changes to ***** settings.';
          If disbDarts = *on;
             tipDarts = ScanReplace('*****':'iDarts':strDisabledTip);
          Endif;

          If disbEmailO = *on;
             tipToEmail = ScanReplace('*****':'email':strDisabledTip);
             tipToEmail = tipToEmail;
             tipSubject = tipToEmail;
             tipBody = tipToEmail;
          Endif;

          If disbPrint = *on;
             tipPrint = ScanReplace('*****':'print':strDisabledTip);
             tipPrinter = tipPrint;
          Endif;

          If disbCopy = *on;
             tipCopy = ScanReplace('*****':'the copy to user folder'
              :strDisabledTip);
          Endif;

          //* Pull in any settings from local data area.
          In *Lock *dtaara;
          Out *dtaara;

          visGrpDef = *off;
          scGrpDef  = ' ';
          If xxMultiG = 'Y';
             visGrpDef = *on;
             scGrpDef  = xxDefDart;
          Endif;
          Select;
          When xxDarts = 'R';
             isDartsRequired = *on;
             xxDarts = 'Y';
          Endsl;
          Select;
          When xxDarts = 'N'; // Check box uses blank for not selected
             xxDarts = *blank;
          When xxDarts = '*'; // Darts not available per process
             xxDarts = *blank;
             disbDarts = *on;
             tipDarts = 'Process selections do not allow for iDARTS.';
             xxCopyOut = 'Y'; // User folder is required if Darts can't be selected
          Endsl;
          scDarts   = xxDarts;
          scRMCD    = xxRMCD;
          scCopyOut = xxCopyOut;
          scPrint   = xxPrint;
          scPrinter = xxPrinter;
          scTray    = xxTray;
          //*** begin add *** 07/24/25 *************************************
          scPrintM  = xxPrint;
          scPrinterM = xxPrinter;
          scTrayM   = xxTray;
          //*** end add ***** 07/24/25 *************************************
          //*** Begin Add *** 09/23/22 *************************************
          scPrintI   = xxPrintI;
          scPrinterI = xxPrinterI;
          scTrayI    = xxTrayI;
          scPrintA   = xxPrintA;
          scPrinterA = xxPrinterA;
          scTrayA    = xxTrayA;
          //*** End   Add *** 09/23/22 *************************************
          scHoldOut = xxHold;
          scSaveOut = xxSave;
          // *** Beg Chg *** 07/25/23 ** WC2850 ****************
          // If xx#CopiesA <> ' ';
          //    scCopies  = %int(xx#CopiesA);
          // Else;
          //    scCopies = 1;
          // Endif;
          If scCopies = 0;
             scCopies = 1;
          EndIf;
          // *** End Chg *** 07/25/23 ** WC2850 ****************
          scEmail   = xxEmail;
          If SCFlg2 = 'Y';
             Clear dsEmail;
             ptrEmail = %addr(dsEmail);
             //*** Beg Chg *** 01/20/23 *** WC2875 ***********************
             //***LoadDefaultEmaildata(ptrEmail:inType);
             dsRunInfo.runInfo = inType;
             LoadDefaultEmaildata(ptrEmail:dsRunInfo);
             //*** End Chg *** 01/20/23 *** WC2875 ***********************
             //*** Begin Chg *** 09/23/22 **********************************
             //*** scSubject = scEmlSub;
             //*** scBody = scEmlBody;
             If inMass = ' ';
                scSubject = scEmlSub;
                scBody = scEmlBody;
             Else;
                scSubject = SPemlSubM;
                scBody = SPemlBodyM;
             EndIf;
             //*** End   Chg *** 09/23/22 **********************************
             //*** Begin Add *** 07/24/24 **********************************
             // Call ReplaceTokensEmail procedure
             // scSubject = ReplaceTokensEmail(scSubject:dsRunInfo:inRRN:
             //  strRRNType:' ');
             // scBody = ReplaceTokensEmail(scBody:dsRunInfo:inRRN:strRRNType:' ');
             //*** End   Add *** 07/24/24 **********************************
             scToEmail = '*Email Recipients Defined by Contact/Output ' +
              'Control and Distribution Type Settings*';
             //*** begin add ***  03/06/25  ****************************
             If inMass = ' ';
                Clear scToEmail;
             Endif;
             //*** end add *****  03/06/25  ****************************
             //* Pull email recipients if valid RRN was passed.
             //*** begin chg ***  03/06/25  ****************************
             //*****If rrnFound;
             If rrnFound and inMass = ' ';
                disbEmailT = *off;
             //*** end chg *****  03/06/25  ****************************
                Reset dsRunInfo;
                dsRunInfo.runInfo = inType;
                //*** Beg Chg ***** 11/21/24 *******************************
                //***scToEmail = %trim(GetDefaultEmailRecipients(dsRunInfo:inRRN:
                //*** strRRNType));
                ##ToEmail = '';
                curPos = 1;
                scemailL = 0;
                scemailAL = 0;
                ##ToEmail = %trim(GetDefaultEmailRecipients(dsRunInfo:inRRN:
                 strRRNType));
                If %Len(%trim(##ToEmail)) > 0;
                   scToEmail = '';
                   scToEmailA = '';
                Endif;
                // Loop through the ##TOemail string
                DoW curPos <= %Len(%TrimR(##TOemail));
                   // Find the next email address
                   nextComma = %Scan(',' : ##TOemail : curPos);
                   If nextComma = 0;
                     nextComma = %Len(##TOemail) + 1; // Last email, no more commas
                   EndIf;
                   // Extract the email
                   tempEmail = %SubSt(##TOemail : curPos : nextComma - curPos);
                   curPos = nextComma + 1; // Move to the next email

                   // Check if the email fits in scToEmail
                   If scemailL + %Len(%trim(tempEmail)) <= %Len(scToEmail);

                      // Add to scToEmail (add comma if not first email)
                      If scemailL > 0;
                        scToEmail = %trim(scToEmail) + ',';
                        //scemailL += 1;
                      EndIf;
                      scToEmail = %trim(scToEmail) + tempEmail;
                      If scemailL > 0;
                         scemailL += 1;
                      EndIf;
                      scemailL += %Len(%trim(tempEmail));
                   ElseIf scemailAL + %Len(%trim(tempEmail))<= %Len(scToEmailA);

                       // Add to scemailA (add comma if not first email)
                      If scemailAL > 0;
                           scToEmailA = %trim(scToEmailA)+ ',';
                           //scemailAL += 1;
                      EndIf;
                      scToEmailA = %trim(scToEmailA) + tempEmail;
                      If scemailAL > 0;
                         scemailAL += 1;
                      EndIf;
                      scemailAL += %Len(%trim(tempEmail));
                   Else;
                        // Email too long for scToEmailA, log an error or handle accordingly
                   EndIf;
                EndDo;
                //*** End Chg ***** 11/21/24 *******************************
                disbEmailT = disbEmailO;
             Endif;
          Else;
             Clear scToEmail;
             Clear scToEmailA;
             Clear scSubject;
             Clear scBody;
          Endif;
          If scSubject <> *blank or scBody <> *blank;
             scEmail = 'Y';
          Endif;
          tipEmail = 'Check this box in order to email the addresses listed ' +
           'below.';
          Select;
          When xxEmail = '*'; // Email not available per process
             disbEmail = *on;
             xxEmail = *blanks;
             scEmail = *blanks;
             tipEmail = 'Process selections do not allow for email.';
          Endsl;
          //* Allow editing if subject or body are blank otherise user
          //* cannot correct the blank values to proceed.
          If scSubject = ' ' or scBody = ' ';
             disbEmailO = *off;
             tipBody = ' ';
             tipSubject = ' ';
          Endif;

          //* Default information if not passed in via local data area.

          If inType <> ' ';
             Chain(n) inType psp00;
             Chain(n) q1user pup00;
             If scHoldOut <> 'Y';
                scHoldOut = SPHold;
             Endif;
             If scSaveOut <> 'Y';
                scSaveOut = SPSave;
             Endif;

             If scCopyOut = ' ';
                If inMass = 'Y';
                   scCopyOut = SPOutM;
                Else;
                   scCopyOut = SPOutS;
                Endif;
             Endif;
             //*** Begin Add *** 09/23/22 ****************************
             // Set Insured printer settings for Mass prints
             If scPrinterI = ' ' And inMass = 'Y';
                If UPPKey = ' ' or SPOverPI = 'Y';
                   scPrinterI = SPPKeyI;
                   scTrayI    = SPTrayI;
                Else;
                   scPrinterI = UPPKey;
                   scTrayI    = ' ';
                Endif;
             Endif;

             // Set Agent printer settings for Mass prints
             If scPrinterA = ' ' And inMass = 'Y';
                If UPPKey = ' ' or SPOverPA = 'Y';
                   scPrinterA = SPPKeyA;
                   scTrayA    = SPTrayA;
                Else;
                   scPrinterA = UPPKey;
                   scTrayA    = ' ';
                Endif;
             Endif;

             // Set Insured print checkbox
             scPrintI = ' ';
             If scPrinterI <> ' ';
                scPrintI = 'Y';
             Endif;

             // Set Agent print checkbox
             scPrintA = ' ';
             If scPrinterA <> ' ';
                scPrintA = 'Y';
             Endif;
             //*** End   Add *** 09/23/22 ****************************

             If scPrinter = ' ';

                If inMass = 'Y';
                   If UPPKey = ' ' or SPOverPM = 'Y';
                      scPrinter = SPPKeyM;
                      scTray = SPTrayM;
                   Else;
                      scPrinter = UPPKey;
                      scTray = ' ';
                   Endif;
                Else;
                   If UPPKey = ' ' or SPOverP = 'Y';
                      scPrinter = SPPKey;
                      scTray = SPTray;
                   Else;
                      scPrinter = UPPKey;
                      scTray = ' ';
                  Endif;
                Endif;
          //*** begin add *** 07/24/25 *************************************
                scPrinterM = scPrinter;
                scTrayM   = scTray;
          //*** end add ***** 07/24/25 *************************************
             Endif;

             If scPrinter <> ' ';
                scPrint = 'Y';
             Else;
                scPrint = ' ';
             Endif;
          //*** begin add *** 07/24/25 *************************************
             scPrintM  = scPrint;
          //*** end add ***** 07/24/25 *************************************

          Endif;

          If scPrint <> 'Y';
             disbPrint1 = *on;
          Else;
             If disbPrint = *on;
                disbPrint1 = *on;
             Else;
                disbPrint1 = *off;
             Endif;
          Endif;

          //* Default values to prevent errors if fields are disabled for changes.

          If disbDarts = *on and scDarts = 'Y' and scRMCD = ' ';
             scDarts = ' ';
          Endif;

          If disbDarts = *on and disbCopy = *on and scDarts <> 'Y' and
           scCopyOut <> 'Y';
             scCopyOut = 'Y';
          Endif;

          If disbPrint = *on and scPrint = 'Y' and scPrinter = ' ';
             scPrint = ' ';
          //*** begin add *** 07/24/25 *************************************
             scPrintM  = scPrint;
          //*** end add ***** 07/24/25 *************************************
          Endif;

          If disbPrint = *on and scPrint = 'Y' and scCopies <= 0;
             scCopies = 1;
          Endif;

       Endsr;
       //*********************************************************************
       //*
       //* Load screen fields back to the paramters.
       //*
       //*********************************************************************
       Begsr LoadParms;

          In *Lock *dtaara;

          xxDarts   = scDarts;
          xxRMCD    = scRMCD;
          xxCopyOut = scCopyOut;
          xxPrint   = scPrint;
          xxPrinter = scPrinter;
          xxTray    = scTray;
          //*** begin add *** 07/24/25 *************************************
          If isPolicy = *on;
             xxPrint   = scPrintM;
             xxPrinter = scPrinterM;
             xxTray    = scTrayM;
          Endif;
          //*** end add ***** 07/24/25 *************************************
          xx#CopiesA = %editc(scCopies:'X');
          xxEmail   = scEmail;
          xxToEmail = %trim(scToEmail);
          xxSubject = %trim(scSubject);
          xxHold    = scHoldOut;
          xxSave    = scSaveOut;
          xxDefDart = scGrpDef;
          //*** Begin Add *** 09/23/22 *************************************
          xxPrintI   = scPrintI;
          xxPrinterI = scPrinterI;
          xxTrayI    = scTrayI;
          xxPrintA   = scPrintA;
          xxPrinterA = scPrinterA;
          xxTrayA    = scTrayA;
          //*** End   Add *** 09/23/22 *************************************
          If not rrnFound;
             xxToEmail = '%mass%';
          Endif;
          xxBody = %trim(scToEmailA);
          If xxEmail <> 'Y';
             Clear xxToEmail;
             Clear xxSubject;
             Clear xxBody;
          Endif;

          Out *dtaara;
          CreateKeyRecordXML(scBody);

       Endsr;
       //*********************************************************************
       //*
       //* Clear errors.
       //*
       //*********************************************************************
       Begsr ClearErrors;

          Clear error;

          Clear msDarts;
          Clear msRMCD;
          Clear msCopyOut;
          Clear msPrint;
          Clear msPrinter;
          Clear msTray;
          Clear msCopies;
          Clear msEmail;
          Clear msToEmail;
          Clear msSubject;
          Clear msBody;

          Clear erDarts;
          Clear erRMCD;
          Clear erCopyOut;
          Clear erPrint;
          Clear erPrinter;
          Clear erTray;
          Clear erCopies;
          Clear erEmail;
          Clear erToEmail;
          Clear erSubject;
          Clear erBody;
          Clear erToEmailA;
          Clear msToEmailA;

       Endsr;
       //*********************************************************************
       //*
       //* OutputCheck - Process edits on output window.
       //*
       //*********************************************************************
       Begsr OutputCheck;

          Exsr ClearErrors;

          If scDarts = ' ' and scCopyOut = ' ';
             error = *on;
             erDarts = *on;
             erCopyOut = *on;
             msDarts   = 'Either iDarts or Copying Output to User Folder ' +
              'must be selected.';
             msCopyOut = msDarts;
          Endif;

          //*** begin chg *** 07/24/25 *************************************
          //*****If scPrint = 'Y' and scPrinter = ' ';
          If (scPrint = 'Y' and scPrinter = ' ' and isPolicy = *off) or
             (scPrintM = 'Y' and scPrinterM = ' ' and isPolicy = *on);
          //*** end chg ***** 07/24/25 *************************************
             error = *on;
             erPrinter = *on;
             msPrinter = 'A printer must be selected when Print option ' +
              'is chosen.';
          Endif;

          If scCopies < 0;
             error = *on;
             erCopies = *on;
             msCopies = '# of copies cannot be less than zero.';
          Endif;

          //*** begin chg *** 07/24/25 *************************************
          //*****If scCopies <= 0 and scPrint = 'Y';
          If (scCopies <= 0 and scPrint = 'Y' and isPolicy = *off) or
             (scCopies <= 0 and scPrintM = 'Y' and isPolicy = *on);
          //*** end chg ***** 07/24/25 *************************************
             error = *on;
             erCopies = *on;
             msCopies = '# of copies must be greater than zero when printing.';
          Endif;

       // DARTS Required
       If isDartsRequired and scDarts <> 'Y';
          msDARTS = 'Process selections require iDarts to be selected.';
          erDARTS = *on;
          error = *on;
       Endif;

       // Remark Code must be selected if writing Darts remark.
       If scDarts = 'Y' and scRMCD = ' ';
          msRMCD = 'A remark code must be selected when writing to iDarts.';
          erRMCD = *on;
          error = *on;
       Endif;

       If scRMCD <> ' ';
          Chain(n) scRMCD pct01;
          If not %found or CTStat = 'I';
             msRMCD = 'Remark code must be valid.';
             erRMCD = *on;
             error = *on;
          Endif;
       Endif;

       If scEmail <> 'Y';
       //* Email fields should be blank if not emailing.

          If scToEmailA <> ' ';
             error = *on;
             erToEmailA = *on;
             msToEmailA = 'Additional email recipient(s) must be blank if ' +
              'not emailing.';
          Endif;

       Else;
       //* Email fields must be populated if emailing.
          //*** Begin Add *** 09/22/22 ******************************
          //*** begin chg *** 04/28/23 ******************************
          //*****If scToEmail = ' ';
          If scToEmail = ' ' and scToEmailA = ' ';
          //*** end chg ***** 04/28/23 ******************************
             msToEmail = 'Email address is required if sending an' +
                         ' email.';
             error = *on;
             erToEmail = *on;
          EndIf;
          //*** End   Add *** 09/22/22 ******************************

          If scToEmailA <> ' ';
             If CheckEmailList(%trim(scToEmailA):msToEmailA);
                error = *on;
                erToEmailA = *on;
             Endif;
          Endif;
          If scSubject = ' ';
             error = *on;
             erSubject = *on;
             msSubject = 'Email subject must not be blank if not email' +
              'ing.';
          Endif;
          If scBody = ' ';
             error = *on;
             erBody = *on;
             msBody = 'Email body must not be blank if not email' +
              'ing.';
          Endif;

       Endif;


       Endsr;
       //*********************************************************************

      /End-Free
     POutputWindow     E
      *
     P*** begin add ***  10/16/24  *********************************************
     P*=====================================================================
     P* ListDocs - Receives a document name and verifies if it exists in D&C.
     P*            This is done by calling Scriptura server and receiving an
     P*            XML file of all D&C documents, parsing them into a data file,
     P*            and then verifying document is one of documents available.
     P*
     P*            If data in data file is more recent than 30 seconds or value
     P*            specified in STSCP than just use the current data available.
     P*
     P*          Parm  1 = Document Name                 (100A)
     P*
     P*=====================================================================
     PListDocs         B                   Export
     DListDocs         PI             1n
     D inDoc                        100    const
     D outDocID                     100    options(*nopass)
     D*
     d foundDoc        s              1n   inz(*off)
     d timeVal         s             11  0 inz(30)
     d timeStamp       s             14  0 inz(0)
     d count           s              5  0 inz(0)
     d todayTime       s               z
     d dtaqLib         s             10    Inz(' ')
     d dtaqKey         s             14    Inz(' ')
     d dataLen         s              5p 0 inz(250)
     d dtaqData        s            250    Inz(' ')
     d sendData        s             50    Inz(' ')

       Open stldp;

       //* Clear out return documentID value if passed in.
       If %parms > 1;
          outDocID = ' ';
       Endif;

       //* Read the Scriptura module file.
       Setll 1 psc0p;
       Read psc0p;

       If SC1106 <> 0;
          timeVal = SC1106;
       Endif;

       todayTime = %timestamp(*sys) - %seconds(timeVal);
       timeStamp = %int(%subst(%char(todayTime:*iso0):1:14));

       Setll 1 pld0p;
       Read(n) pld0p;

       //* Wait 6 secs and try again if data is being updated.
       Dow not %eof and LDProc = 'Y' and count < 6;
          Sleep(6);
          count += 1;
          Setll 1 pld0p;
          Read(n) pld0p;
       Enddo;

       //* Build or Rebuild the data for documents.
       If %eof or LDStamp < timeStamp;

          //* Write record indicating processing is in place.
          If %eof;
             Clear pld0p;
             Eval LDStamp = %int(%subst(%char(%timestamp:*iso0):1:14));
             LDProc = 'Y';
             Write pld0p;
          Else;
             Setll 1 pld0p;
             Read pld0p;
             Clear pld0p;
             Eval LDStamp = %int(%subst(%char(%timestamp:*iso0):1:14));
             LDProc = 'Y';
             Update pld0p;
          Endif;

       //* Call Scriptura via data queue for ListDoc service.
          GetObjLib('DTQLDS':'*DTAQ':dtaqLib);
          dtaqKey = %editc(LDStamp:'X');
          sendData = %trim(dtaqKey) + %trim(SCXMLF) + 'ListDoc.xml';
          SendDTAQ('DTQLDS':dtaqLib:50:sendData);

          ReceiveDTAQ('DTQLDR':dtaqLib:dataLen:dtaqData:60:'EQ':14:
           dtaqKey:0:' ');

       //* Response back from data queue.
          If dataLen > 0;

             Clear documents;
          //* Parse XML file into array.
             Monitor;
                strCommand = 'DEL OBJLNK(''' +  %trim(SCXMLF) +
                 'ListDocTemp.xml' + ''')';
                Monitor;
                   Callp QCMDEXC(strCommand:%len(strCommand));
                On-error;
                Endmon;
                strCommand = 'CPY OBJ(''' + %trim(SCXMLF) + 'ListDoc.xml' +
                 ''')' + ' TOOBJ(''' + %trim(SCXMLF) + 'ListDocTemp.xml' +
                 ''') TOCCSID(1252)';
                Callp QCMDEXC(strCommand:%len(strCommand));

                Xml-into documents %xml(%trim(SCXMLF) + 'ListDocTemp.xml':
                 'path=data/documents/documents doc=file allowextra=yes ' +
                 'ccsid=best case=any');
             On-error;
          //* Clear out records when no response from Scriptura server.
                Setll 1 pld0p;
                Read pld0p;
                Dow not %eof;
                   Delete pld0p;
                   Read pld0p;
                Enddo;
                PopOneBtn('XML parsing of ListDoc.xml failed.  See ' +
                 'interactive job log. Do not close window.':
                 'Document Processing':'Ok');
                Close stldp;
                Return foundDoc;
             Endmon;

          //* Clear out existing data.
             timeStamp = LDStamp;
             Setll 1 pld0p;
             Read pld0p;
             Dow not %eof;
                If LDDoc <> ' ';
                   Delete pld0p;
                Endif;
                Read pld0p;
             Enddo;

          //* Write the parsed data into STLDP file.
             count = 1;
             Dow documents(count).documentName <> ' ' or
              documents(count).documentID <> ' ';

                Clear pld0p;
                If documents(count).documentName <> ' ';
                   LDStamp = timeStamp;
                   LDDoc = documents(count).documentName;
                   LDDocID = documents(count).documentID;
                   LDPgm  = Q1PGM;
                   LDUser = Q1USER;
                   LDTime = Timeto6(SystemTime());
                   LDDt   = Dateto7(SystemDate());
                   Write pld0p;

                   If LDDoc = inDoc and foundDoc = *off;
                      If %parms > 1;
                         outDocID = LDDocID;
                      Endif;
                      foundDoc = *on;
                   Endif;

                Endif;

                count += 1;
                If count = 9999;
                   Leave;
                Endif;
             Enddo;

          //* Clear out placeholder record.
             Setll 1 pld0p;
             Read pld0p;
             Dow not %eof;
                If LDDoc = ' ';
                   Delete pld0p;
                Endif;
                Read pld0p;
             Enddo;

          Else;
          //* Clear out records when no response from Scriptura server.
             Setll 1 pld0p;
             Read pld0p;
             Dow not %eof;
                Delete pld0p;
                Read pld0p;
             Enddo;
             PopOneBtn('Scriptura did not respond for ListDoc request.'
              :'Document Processing':'Ok');
          Endif;

       Else;
       //* Check existing data for document.
          Dow foundDoc = *off and not %eof;
             If LDDoc = inDoc;
                If %parms > 1;
                   outDocID = LDDocID;
                Endif;
                foundDoc = *on;
             Else;
                Read(n) pld0p;
             Endif;
          Enddo;
       Endif;

       Close stldp;

       Return foundDoc;

     PListDocs         E
     P*** end add *****  10/16/24  *********************************************
