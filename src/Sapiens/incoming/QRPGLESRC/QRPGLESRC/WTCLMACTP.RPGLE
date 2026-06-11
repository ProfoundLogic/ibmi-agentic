     hCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/title WTCLMACTP - Profound Claim Action Plan Tab
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:  This Program Will Display Claim Action Plan.
     h*
     h*  Called from: Claim Toolbar
     h*  Calls to:
     h*
     h*
     h*  Created on 7/28/2014 by C:279 for Project: 114WMD
     h**********************************************************************
     h* Revisions:                                                         *
     h*                                                                    *
     h* 05/26/23 C:65  Project: WC2875/SY371                               *
     h*                Add a call to OutputWindow if scriptura is flagged  *
   h*                in order to display the output options before       *
     h*                launcing the action plan live doc.                  *
     h*                                                                    *
     h*  06/09/20  C:298 Project: SY420
     h*                  Mask SSN
     h*
     h*  12/06/16 C:306 Support: 44002                                     *
     h*                 Added @@Call as new parameter to enable and disable*
     h*                 visiblity indicator of ADD button on the grid.     *
     h*                                                                    *
     h*  07/18/16 C:308 Project: 114WXY                                    *
     h*                 Removed sPrModule.                                 *
     h*                                                                    *
     h* 04/04/16 C:65  Project: WC2374/SY371                               *
     h*                Call Scriptura API's if output is flagged to use    *
   h*                Scriptura instead of CompSuite dialog.  Display a   *
     h*                a popup window to request description for the plan. *
     h*                                                                    *
     h*  03/23/16 C:308 Project: Elevate                                   *
     h*                 Genericize the toolbar programming                 *
     h*                                                                    *
     h* 01/07/16 C:65  Project: WC2401                                     *
     h*                Change the coding done under WC2254 to call an      *
   h*                API instead of directly calling the various         *
     h*                programs for windows here.                          *
     h*                                                                    *
     h*  11/25/15 C:33  38610                                              *
     h*                 Ensure path includes AO                            *
     h*                                                                    *
     h*  09/17/15 C:302 Project: wc2254                                    *
     h*                 Recompile Only                                     *
     h*                                                                    *
     h*  04/16/15 C:65  Project: SY251                                     *
     h*                 Add API call to launch new Department letters.     *
     h*
     h*  11/18/14 C:279 Redmine: 4759                                         *
     h*                 Modified to provide Key and Sequence# while building  *
     h*                 URL.                                                  *
     h*
     h*  10/21/14 C:267 Support: 87058  Project: 114WMD                       *
     h*                 Retrieve group and employer names from the file;      *
     h*                 avoid hard coding these fields on the report.         *
     h*
     h*  09/18/14 C:295 Project: 114WMD Priss 29                              *
     h*                 Word is getting spilted in report in the feilds       *
     h*                 Status, Action Plan, Reserve Rational, CC.            *
     h*                                                                       *
     h*  09/15/14 C:295 Project: 114WMD Priss 29                              *
     h*                 Injury field is not displaying the complete           *
     h*                 description in report and in Action Plan Tab.         *
     h*                                                                       *
     h*  09/08/14 C:295 Project: 114WMD Priss 27                              *
     h*                 The Action Plan Date is being updated with the current*
     h*                 It should get populated with actual date the action   *
     h*                 plan are entered.                                     *
     h*                                                                       *
     h*  09/08/14 C:295 Project: 114WMD Priss 26                              *
     h*                 Display the Defense Attorney Name and Plaintiff Attor *
     h*                 -ney Name, not FEIN #                                 *
     h*                                                                       *
     h*  09/08/14 C:295 Project: 114WMD Priss 28                              *
     h*                 Display only the description of the Department,Occupa *
     h*                 tion, Judge.                                          *
     h*                                                                       *
     h*  09/05/14 C:283 Project: 114WMD Priss 24                              *
     h*                 Image Type in the Darts should be 'O' for a pdf doc.  *
     h*
     h**********************************************************************
     fwtclmactp cf   e             workstn handler('PROFOUNDUI(HANDLER)')
     f                                     extfile('WTCLMACTP')
     f                                     sfile(sflActPlan:rrnClmAct)
     f                                     infds(fids01)
     fwtActPnl  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
      *****Begin Add*****  03/23/16  *************************************
     f                                     include(rcdClmPnl)
      ***** End Add *****  03/23/16  *************************************

     fwacal     if   e           k disk    rename(pca01:pca00)
     f                                     prefix(c1:2)
     f                                     infds(dbfds1)
     fwacap     uf a e             disk    rename(pca01:pca0p)
     f                                     prefix(c1:2)

     fwmall     if   e           k disk    rename(pal01:pal00)
     fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     f                                     infds(dbfds)
     fwmcdl     if   e           k disk    rename(pcd01:pcd00)
     fwmcxl     if   e           k disk    rename(pcx01:pcx00)
     fwmdpl     if   e           k disk    rename(pdp01:pdp00)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwmlll     if   e           k disk    rename(pll01:pll00)
     fwmtil     if   e           k disk    rename(pti01:pti00)
     fwmtpl1    if   e           k disk
     fwtabl     if   e           k disk    rename(ppa01:ppa00)
     f                                     rename(pna01:pna00)
     f                                     rename(pca01:pca02)
     fwtc2l     if   e           k disk    rename(pc201:pc200)
     fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     fwtocl     if   e           k disk    rename(poc01:poc00)
     fsmcol     if   e           k disk    rename(pco01:pco00)
     fsdcmla    uf   e           k disk    rename(pcm01:scm0a)
     f                                     prefix(s2:2)
     f***  Begin Add  ***  04/04/16  **************************************
     FXMLPREL1  UF   E           K DISK
     f***  End Add ******  04/04/16  **************************************

     fwtclmacto o    e             printer usropn

      *=============
      * proto-types
      *=============

      * build PDF url
     d***  Begin Chg  ***  11/25/15  **************************************
     d*BuildPDFUrl     pr           720
     d#BuildPDFUrl     pr           720
     d***  End   Chg  ***  11/25/15  **************************************
     d inDocId                       12
     d inFold                        80

      * clear subfiles
     d ClrSubFiles     pr

      * get remark code
     d GetRmkCd        pr             5

      * Get Next available document/folder id
     d GetNxtDoc       pr                  extpgm('SNXTDCID')
     d ##folder                      80
     d ##DocId                       12
     d ##Pre                          2
     d ##co                           3
     d ##fnd                          3
     d ##rtcd                         1

      * Get Next available document/folder id
     d GenrtPDF        pr                  extpgm('CRTRSLV')
     d ##folder                      80
     d ##DocId                       12
     d ##splf                        10
     d ##rtcd                         1

      * Load Action plans Grid
     d LoadClmAct      pr

      * Process Add Action Plan
     d ProcessAdd      pr

      * Process Other Operations
     d ProcOther       pr

      * Get Root
     d RtvRoot         pr                  extpgm('RTVDOCROOT')
     d ##root                        10

      * Write Darts
     d WriteDarts      pr                  extpgm('DA700R')
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


      * General Prototypes
     d/copy dPrRemark
     d/copy wPrUsrPrms
     d/copy sPrAtrium
     d/copy sPrString
     d/copy wPrGroup
     d/copy sPrDate
     d/copy wPrClmPop
     d/copy wPrTable
     d/copy wPrClaims
     d/copy wPrAgent
     d/copy wPrContact
     d/copy wPrLoc
     d/copy wPrEmplr
     d/copy wPrEdi
     d/copy wPrGenPop
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D***  Begin Add  ***  04/04/16  **************************************
      ***** Beg Del ***** 07/18/16 ****************************************
     D****/copy sPrModule
      ***** End Del ***** 07/18/16 ****************************************
     D/copy sPrXML
     D*
     D isUseXML        S              1n   inz
     d xmlFile         s             60    inz(*blanks)
     d message         s            250
     d strLang         s             60
     d strLangCode     s            250
     D string          s             10    inz(*blanks)
     D xxPre           s              2    inz('CP')
     d processType     c                   const('Claim Action Plan')
     d*** begin add ***  05/26/23  **********************************
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1

     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
     d*** end add *****  05/26/23  **********************************

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
     D***  End   Add  ***  04/04/16  **************************************

      * Load Action Panel procedure Prototype and variables
     d/copy sCopyClmPr
       //*** begin add ***  04/16/15  ****************************************
     d/copy sPrLetter
       //*** begin add ***  04/16/15  ****************************************

      * System Data Structures
     d dbfds           ds
     d  flstcm           *status
     d  dbrrn                397    400b 0
     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d psds          esds                  extname(szq1p)
     d fids01        e ds                  extname(szq2p)

     d                 ds                  inz
     d  rt                     1     20
     d                                     dim(10)
     d  CMrt01                 1      2
     d  CMrt02                 3      4
     d  CMrt03                 5      6
     d  CMrt04                 7      8
     d  CMrt05                 9     10
     d  CMrt06                11     12
     d  CMrt07                13     14
     d  CMrt08                15     16
     d  CMrt09                17     18
     d  CMrt10                19     20

     d                 ds                  inz
     d  rr                     1     90  2
     d                                     dim(10)
     d  CMrr01                 1      9  2
     d  CMrr02                10     18  2
     d  CMrr03                19     27  2
     d  CMrr04                28     36  2
     d  CMrr05                37     45  2
     d  CMrr06                46     54  2
     d  CMrr07                55     63  2
     d  CMrr08                64     72  2
     d  CMrr09                73     81  2
     d  CMrr10                82     90  2

     d                 ds                  inz
     d  ar                     1     90  2
     d                                     dim(10)
     d  CMar01                 1      9  2
     d  CMar02                10     18  2
     d  CMar03                19     27  2
     d  CMar04                28     36  2
     d  CMar05                37     45  2
     d  CMar06                46     54  2
     d  CMar07                55     63  2
     d  CMar08                64     72  2
     d  CMar09                73     81  2
     d  CMar10                82     90  2

      * work-Variables
     d ##co            s              3
     d ##fnd           s              3
     d ##pre           s              2
     d ##root          s             10
     d ##rtcd          s              1
     d ##splf          s             10
     d ##key           s             40
     d ##comm          s            256
     d ##cDat          s              8
     d ##dcId          s             12    inz(*blanks)
     d ##pNam          s             10    inz('W0070R')
     d ##secl          s              3    inz('000')
     d ##user          s             10
     d ##pgm           s             10
     d ##fold          s             80    inz(*blanks)
     d ##diFl          s              1    inz(*blanks)
     d ##iPgm          s             10
     d ##fl01          s              1    inz(*blanks)
     d ##fl02          s              1    inz(*blanks)
     d ##rmCd          s              5
     d ##seq#          s              5    inz('00000')
     d ##sbyl          s              1    inz('N')
     d ##com2          s             19    inz(*blanks)
     d ##apId          s              2    inz(*blanks)
     d ##iDoc          s             12    inz(*blanks)
     d ##iQue          s             10    inz(*blanks)
     d ##imgF          s              1    inz(*blanks)
     d ##iPth          s             10    inz(*blanks)
     d ##pIdx          s              5    inz('00000')
     d ##qPty          s              1    inz(*blanks)
     d ##dcTp          s              5    inz(*blanks)
     d ##csId          s              2    inz(*blanks)
     d ##code          s              2    inz(*blanks)
     d @@rrn           s              9
     d @@mode          s              1
     d @@TabIndex      s              5
      ****** beg add ******* 11/18/14 ******************************************
     d strKey#         s             40
     d strSeq#         s              5
      ****** end add ******* 11/18/14 ******************************************

     d inRrnClm        s              9  0
     d inMode          s              1
     d intCount        s              2  0
     d isMenuAvbl      s               n   inz(*on)
     d outClientID     s              3    inz('000')
     d strKey          s                   like(PAkey)
     d strTbId         s                   like(GTtbId)
     d strType         s              1
     d******** Beg Add ******** 09/15/14 *************************************
     d strInju         s            120
     d******** End Add ******** 09/15/14 *************************************
     d******** Beg Add ******** 09/18/14 *************************************
     d intpos          s              3  0
     d strBlanks       s              1
     d strString       s            200
     d strString1      s            256
     d incretval       s              3  0
     d retval          s              3  0
     d intlen          s              3  0
     d intCnt          s              3  0
     d intPrvVal       s              3  0
     d******** End Add ******** 09/18/14 *************************************
     d******** Beg Add ******** 10/21/14 *************************************
     d YYrrn           s              9  0
     d******** End Add ******** 10/21/14 *************************************
     d******** beg Add ******** 12/06/16 *************************************
     d @@Call          s              1a
     d******** end Add ******** 12/06/16 *************************************

     c     *entry        plist
     c                   parm                    @@rrn
     c                   parm                    @@mode
     c                   parm                    @@TabIndex
     d******** beg Add ******** 12/06/16 *************************************
     c                   parm                    @@Call
     d******** end Add ******** 12/06/16 *************************************

     c     keyWMEML      klist
     c                   kfld                    CMco#
     c                   kfld                    CMfnd
     c                   kfld                    CMemp#
     c                   kfld                    CMdiv

      /free

       Exsr  ProgramStart;

       // Load the action panel
       LoadActPan(isMenuAvbl);

       // Exit
       Dou exitNow = *on;

          // clear sub-file
          ClrSubfiles();

          // load data in the tab
          LoadClmAct();

          // display screen
          Exsr ExfmtScreens;

          // process screen selection
          Select;
          When TabClick = *On;
             TabClick = *Off;
       //*** begin chg ***  01/07/16  ****************************************
       //*** begin add ***  04/16/15  ****************************************
       //*****   When btnDocWndw = *on;
       //*****      btnDocWndw = *off;
       //*****      DocWindow(scActRRN:'CS');
       //*** end add *****  04/16/15  ****************************************
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'CS':txtWinData);
             btnShowWin = *off;
       //*** end chg *****  01/07/16  ****************************************

          // process Addition of Claim Action Plan
          When BtnActpAdd = *on;
             BtnActpAdd = *off;
       //*** begin chg ***  04/04/16  ***********************************
       //********       ProcessAdd();
             If isUseXML;
             //* Process using Scriptura LiveDocs via browser launch.

             //*** begin chg ***  05/26/23  *************************************
             //***** If GetRmkCd() = ' ';
             //*****    PopOneBtn('DARTS remark setup is required at the Group or +
             //*****     Company level to Save the Claim Action Plan':'Error');
             //***** Else;
             //*****    Exsr ProcessScriptura;
             //***** Endif;
                Exsr ProcessScriptura;
             //*** end chg *****  05/26/23  *************************************

             Else;
             //* Process using Compsuite dialog/output.
                ProcessAdd();
             Endif;
       //*** end chg *****  04/04/16  ***********************************

          Other;
             ProcOther();

          Endsl;
       Enddo;

       *inlr = *on;
       return;

       //===================================================================
       // ProgramStart - Start Processing
       //===================================================================
       Begsr ProgramStart;

       //Load default values or passed in values
       If %parms() > 0 and %int(@@rrn) <> 0;
          inRrnClm = %int(@@rrn);
          inMode = @@Mode;
       Else;
          inRrnClm = 41;
          inMode = 'C';
       EndIf;

       // Set the action panel mode
       scActMode = inMode;
       //***Begin Del*****  03/23/16  *************************************
       //scEnv = 'L';
       //***Begin Del*****  03/23/16  *************************************

       // Set the initial tab
       If %parms() > 2;
          tabIndex = %dec(@@TabIndex:5:0);
       Else;
          tabIndex=0;
       EndIf;

       // Access the claim record
       Chain inRrnClm pcm0p;

       // Get the client id
       GetClientID(outClientID);

       // Populate hidden fields
       scActCltId = outClientID;
       scActRrn = inRrnClm;

       scCo# = CMco#;
       tabClick = *Off;
       //*** begin add ***  04/04/16  ***********************************
       isUseXML = UseScriptura(processType)
        or Use3rdXml(processType);
       //*** end add *****  04/04/16  ***********************************

       Endsr;
       //===================================================================
       // Display Screen Formats
       //===================================================================
       Begsr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 760;
          APnlHeight = TabHeight + 50;
          Write rcdClmPnl;
          Write rcdPanel;
          Exfmt rcdActInfo;
          Read rcdClmPnl;
          Read rcdPanel;
       Endsl;

       Endsr;
       //*** begin add ***  04/04/16  ***********************************
       //===================================================================
       // Process Document via Scriptura
       //===================================================================
       Begsr ProcessScriptura;

          //* Display popup to request description for the plan.
          Clear scDesc;
          btnDesCanl = *off;
          btnDesOk   = *off;
          Clear erDesc;
          Clear msDesc;

          Dou btnDesCanl = *on or btnDesOk = *on;
             Exfmt rcdDocDesc;
             Clear erDesc;
             Clear msDesc;
             If btnDesOk = *on;
                If scDesc = ' ';
                   btnDesOk = *off;
                   erDesc = *on;
                   msDesc = 'Description cannot be blank.';
                Endif;
             Endif;
          Enddo;

          If btnDesCanl = *on;
             Leavesr;
          Endif;
       //*** begin add ***  05/26/23  *************************************
       //* Display output control window prior to doing browser launch.
          xxSubmit = *on;
          xxProcType = processType;
          xxMass = ' ';
          In *Lock *dtaara;

          Clear svLDA;

          xxRMCD = GetRmkCd();
          If xxRMCD <> ' ';
             xxDart = 'Y';
          Else;
             xxDart = 'N';
          Endif;

          Out *dtaara;
          xxSubmit = OutputWindow(xxProcType:xxMass:' ':' ':inRRNClm);

          If xxSubmit = *off;
             Leavesr;
          Endif;
       //*** end add *****  05/26/23  *************************************

          //* Proceed with requesting prep record and launching Scriptura.

          xmlFile = ' ';
          message = %trim(processType);

          StLetter('XMLREQUEST':xmlFile:message:'C':%editc(inRRNClm:'X'):
           xxPre:string:%editc(CMCo#:'X'):%editc(CMFnd:'X'));

          Chain (xmlFile) xp100;

          XPRMCD = GetRmkCd();
          XPData = 'Y';
          XPDart = 'Y';

          XPText1 = %trim(scDesc);

          XPPgm  = Q1PGM;
          XPUser = Q1USER;
          XPTime = Timeto6(SystemTime());
          XPDt   = Dateto7(SystemDate());

          Update xp100;

          message = ' ';
          //* Launch the browser window for interactive document.
          message = LaunchDoc(xmlFile);

          If message <> ' ';
             PopOneBtn(%trim(message):'Scriptura Processing':'Ok');
          Endif;

       Endsr;
       //*** end add *****  04/04/16  ***********************************
      /end-free

       //-------------------------------------------------------------------
       // Procedure Definition Section
       //-------------------------------------------------------------------

       //===================================================================
       // clears subfile
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi

      /free

       rrnClmAct = 0;
       clrClmAct = *On;
       dspClmAct = *Off;
       write rcdActInfo;

      /end-free

     p ClrSubfiles     e
       //===================================================================
       // Get Remark Code
       //===================================================================
     p GetRmkCd        b
     d GetRmkCd        pi             5

     d remarkCode      s              5    inz(*blanks)

      /free

       Chain (CMco#:CMfnd) pcx00;
       If %found;
          remarkCode = CXcapd;
       Endif;
       If not %found or remarkCode = ' ';
          Chain CMco# pco00;
          If %found;
             remarkCode = COcapd;
          Endif;
       EndIf;

       Return remarkCode;

      /end-free
     p GetRmkCd        e
       //===================================================================
       // LoadClmAct - Load the Action Plans data into Grid
       //===================================================================
     p LoadClmAct      b
     d LoadClmAct      pi

      /free

       dspClmAct = *on;
       clrClmAct = *off;
       dsbClAcDlt = *off;
       clear dbRrn1;

       //***** Beg Chg **** 12/06/16 *************************************
       //***If inMode='C';
       //***   dsbClAcDlt = *on;
       //***Endif;

       If @@Call = 'C';
          dsbClAcDlt = *on;
       Endif;
       //***** End Chg **** 12/06/16 *************************************

       Setll (CMco#:CMfnd:CMfyr:CMcase) pca00;
       Reade (CMco#:CMfnd:CMfyr:CMcase) pca00;
       Dow not %eof;
          //***** Beg Chg **** 09/08/14 *************************************
          //***** scActPlnDt = SystemDate();
          scActPlnDt =CvtToDate(C1acdt);
          //***** Beg Chg **** 09/08/14 *************************************
          scCrtUser  = GetUsrName(C1cusr);
          scActPlan  = %trim(%subst(C1apln:1:80));

          Chain(n) (C1key:C1seq#) scm0a;
          If %found;
             //****** beg add ****** 11/18/14 ******************************
             strKey# = C1Key;
             strSeq# = %Editc(C1seq#:'X');
             //****** end add ****** 11/18/14 ******************************
       //***  Begin Chg  ***  11/25/15  ***********************************
       //****scAcPlnUrl = BuildPDFUrl(S2dcid:S2fold);
             scAcPlnUrl = BuildPDFUrl(S2dcid:S2fold:'Claim Action Plan':
              C1key:C1seq#);
       //***  End   Chg  ***  11/25/15  ***********************************
          Endif;

          scClmAcRrn = dbRrn1;
          rrnClmAct = rrnClmAct + 1;
          write sflActPlan;

          Reade (CMco#:CMfnd:CMfyr:CMcase) pca00;
       Enddo;
       rrnClmAct = 1;

      /end-free
     p LoadClmAct      e
       //===================================================================
       // ProcessAdd - Process the Addition of Claim Action Plan
       //===================================================================
     p ProcessAdd      b
     d ProcessAdd      pi

     d isError         s               n   inz(*off)
     d strDciD         s             12
     d strFold         s             80

      /free

       open wtClmActO;
       Exsr LoadDefault;

       isError   = *off;
       BtnCancel = *off;
       BtnSave   = *off;
       BtnSvPrt  = *off;
       clear scVwPrtUrl;

       Dou btnCancel = *on or ((BtnSave = *on or BtnSvPrt = *on) and
        isError = *off);

          Exfmt rcdClmInfo;
          isError = *off;

          Select;
          When btnCancel = *On;
             btnCancel = *Off;
             close wtClmActO;
             Leave;

          When BtnSave = *On or BtnSvPrt = *on;
             Exsr Validate;
             If isError = *off;
                Exsr CreatePDF;
                //Write record to the (WACAP-Claim Action Plan File)
                Exsr WrtActPlnF;

                // Print
                If BtnSvPrt = *on;
                   Exfmt rcdPrtPDF;
                Endif;
             Endif;
          EndSl;
       Enddo;

       //===================================================================
       // Write to Claim Action Plan File
       //===================================================================
       Begsr wrtActPlnF;

       Exsr processDarts;

       C1co#  = CMco#;
       C1fnd  = CMfnd;
       C1fyr  = CMfyr;
       C1case = CMcase;
       C1acdt = DateTo7(SystemDate());
       C1cusr = q1user;
       C1apln = scActPlan;
       C1sts  = scStatus;
       C1rsvr = scRsvRat;
       C1cc   = scAddCC;
       C1key  = ##key;
       C1seq# = %int(##seq#);
       C1pgm  = q1pgm;
       C1time = TimeTo6(SystemTime());
       Write pca0p;

       //****** beg add ****** 11/18/14 ******************************
       strKey# = C1Key;
       strSeq# = %Editc(C1seq#:'X');
       //***  Begin Chg  ***  11/25/15  ***********************************
       //scVwPrtUrl = BuildPDFUrl(strDcId:strFold);
       scVwPrtUrl = BuildPDFUrl(S2dcid:S2fold:'Claim Action Plan':
        C1key:C1seq#);
       //***  End   Chg  ***  11/25/15  ***********************************
       //****** end add ****** 11/18/14 ******************************

       Endsr;
       //===================================================================
       // Validate
       //===================================================================
       Begsr Validate;

       If GetRmkCd() = ' ';
          isError = *on;
          PopOneBtn('DARTS remark setup is required at the Group or Company +
           level to Save the Claim Action Plan':'Error');
       Endif;

       Endsr;
       //===================================================================
       // CreatePDF
       //===================================================================
       Begsr CreatePDF;

       Exsr ProcPrint;

       Exsr ProcPDF;

       //****** beg del ****** 11/18/14 ******************************
       // scVwPrtUrl = BuildPDFUrl(strDcId:strFold);
       //****** end del ****** 11/18/14 ******************************

       Endsr;
       //===================================================================
       // Process PDF
       //===================================================================
       Begsr ProcPDF;

       ##pre   = 'CP';
       ##co    = %Editc(CMco#:'X');
       ##fnd   = %Editc(CMfnd:'X');
       ##rtcd  = 'P';
       strFold = ' ';
       strDcId = ' ';

       GetNxtDoc(strFold:strDcId:##pre:##co:##fnd:##rtcd);

       ##splf = 'WTCLMACTO';
       GenrtPDF(strFold:strDcId:##splf:##rtcd);

       Endsr;
       //===================================================================
       // Write Blank Line
       //===================================================================
       Begsr WritSpace;

       DStatS = ' ';
       DUStat = ' ';
       Write RStatS;

       Endsr;
       //***** Beg Add ************ 09/18/14 ***********************************
       //===================================================================
       // Scan for Last space
       //===================================================================
       Begsr scanSpace;
         strblanks = ' ' ;
         clear intpos;
         Dou intpos = 0;
             intpos = %scan(strblanks:strstring:intpos+1);
             If intpos > intlen;
                Leave;
             Endif;
             If intpos <> 0;
                retVal = intpos;
             Endif;
         Enddo;
         intCnt = intCnt + 1;

       Endsr;
       //***** End Add ************ 09/18/14 ***********************************
       //===================================================================
       // Process Spool file
       //===================================================================
       Begsr ProcPrint;

       //****** Beg Add ******** 10/21/14 ***************************  *******
       // get group and employer names from database files
       DGrpNam = %trim(GetGrpName(CMfnd));
       DEmpNam = %trim(GetEmpName(CMemp#: CMdiv: YYrrn: CMfnd: CMco#));
      /end-free
     C** center group name
     C                   MOVEL(P)  DGrpNam       SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      60            SRDLEN            3 0
     C                   MOVEL     SRDFLD        DGrpNam
     C** center emplyer name
     C                   MOVEL(P)  DEmpNam       SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD
     C                   PARM      60            SRDLEN
     C                   MOVEL     SRDFLD        DEmpNam
      /free
       //****** End Add ******** 10/21/14 ***************************  *******

       // Write Header
       Write HdrPln;

       // Load Details
       DClmnt  = %trim(CMlsn) + ' , ' + %trim(CMfsn) + ' ' + %trim(CMmid);
       DStAsO  = dtMDYY(DateTo7(SystemDate()));
       //***** Beg Chg ************ 10/21/14 ***********************************
       DClaim  = %Editc(CMfnd:'X') + '-' + %Editc(CMfyr:'X') +
        '-' + %Editc(CMcase:'X');
       //**** %Editc(CMcase:'X');
       //***** End Chg ************ 10/21/14 ***********************************
       DclTyp = %trim(scClmTyp);
       DInjur = dtMDYY(CMacdt);
       DAww   = CMaww;
       DCompr = CMcrt;
       DHired = dtMDYY(CMhidt);
       DLitiG = scLiti;
       DAttor = scAttorney;
       DCAtto = scClmAttny;
       DTrePh = scTrtPhys;
       DDob   = dtMDYY(CDdtob);
       DSubro = scSubro;
       DDDate = dtMDYY(CMdbdt);
       DAdjus = scAdjuster;
       DJusSt = CMjrst;

       Chain (CMco#:Cmfnd:CMemp#:CMdiv:CMctg1) pal00;
       If %found;
          DLocat = %trim(ALname);
       Endif;
       Write DtlClm;

       //***** Beg Chg ************ 09/15/14 ***********************************
       // Injury details
       //****** DiInju = %trim(CMacds);
       //******Write DtlInj;

       strInju = %Trim(CMacDs) + ' ' + %Trim(CMacD2);
       DStatS = 'Injury:';
       intlen = %len(DUstat);

       strString = %subst(strInju:1:intLen);
       strString1= strInju;

       Exsr BldTxtLn1;
       DUStat = %subst(strInju:1:intPrvVal);
       Write RStatS;

       If %subst(strInju:intPrvVal+1) <> ' ';
          DStatS = ' ';
          DUStat = %subst(strInju:intPrvVal+1);
          Write RStatS;
       Endif;

       //Write Injury details
       Write DtlInj;
       //***** End Chg ************ 09/15/14 ***********************************

       //Print Status
       DStatS = 'Status:';
       //***** Beg Chg ************ 09/18/14 ***********************************
       intlen = %len(DUStat);

       strString = %subst(scStatus:1:intLen);
       strString1= scStatus;

       Exsr BldTxtLn1;
       DUStat = %subst(scStatus:1:intPrvVal);
       Write RStatS;

       If %subst(strString1:intPrvVal+1:intLen) <> ' ';
          Exsr BldTxtLn2;
          DStatS = ' ';
          DUStat = %subst(strString1:intPrvVal+1:incRetVal-intPrvVal);
          Write RStatS;
       Endif;

       If incRetVal <> 0 and %subst(scStatus:incRetVal+1) <> ' ';
          DStatS = ' ';
          DUStat = %subst(scStatus:incRetVal+1);
          Write RStatS;
       Endif;
       //***** End Chg ************ 09/18/14 ***********************************

       Exsr WritSpace;

       //Print Action Plan
       DActPl = 'Action Plan:';
       //***** Beg Chg ************ 09/18/14 ***********************************
       intlen = %len(DUActP);

       strString = %subst(scActPlan:1:intLen);
       strString1= scActPlan;

       Exsr BldTxtLn1;
       DUActP = %subst(scActPlan:1:intPrvVal);
       Write RActPl;

       If %subst(strString1:intPrvVal+1:intLen) <> ' ';
          Exsr BldTxtLn2;
          DActPl = ' ';
          DUActP = %subst(scActPlan:intPrvVal+1:incRetVal-intPrvVal);
          Write RActPl;
       Endif;

       If incRetVal <> 0 and %subst(scActPlan:incRetVal+1) <> ' ';
          DActPl = ' ';
          DUActP = %subst(scActPlan:incRetVal+1);
          Write RActPl;
       Endif;
       //***** End Chg ************ 09/18/14 ***********************************
       Exsr WritSpace;

       //Print Financial Summary Header
       Write RFince;

       //Print Financial Summary Detail
       Evalr DTOtIn = 'Paid:';
       DTInde = scPdInd;
       DTMedi = scPdMed;
       DTLega = scPdLeg;
       DTOthr = scPdOth;
       DTTotA = scPdTot;
       Write RPaid;

       Evalr DTOtIn = 'Outstanding:';
       DTInde = scOtInd;
       DTMedi = scOtMed;
       DTLega = scOtLeg;
       DTOthr = scOtOth;
       DTTotA = scOtTot;
       Write RPaid;

       Evalr DTOtIn = 'Total Incurred:';
       DTInde = scTiInd;
       DTMedi = scTiMed;
       DTLega = scTiLeg;
       DTOthr = scTiOth;
       DTTotA = scTiTot;
       Write RPaid;

       //Print Reserve Rationale
       DResvr = 'Reserve Rationale:';
       //***** Beg Chg ************ 09/18/14 ***********************************
       intlen = %len(DUResv);

       strString = %subst(scRsvRat:1:intLen);
       strString1= scRsvRat;

       Exsr BldTxtLn1;
       DUResv = %subst(scRsvRat:1:intPrvVal);
       Write RResvr;

       If %subst(strString1:intPrvVal+1:intLen) <> ' ';
          Exsr BldTxtLn2;
          DResvr = ' ';
          DUResv = %subst(scRsvRat:intPrvVal+1:incRetVal-intPrvVal);
          Write RResvr;
       Endif;

       If incRetVal <> 0 and %subst(scRsvRat:incRetVal+1) <> ' ';
          DResvr = ' ';
          DUResv = %subst(scRsvRat:incRetVal+1);
          Write RResvr;
       Endif;
       //***** End Chg ************ 09/18/14 ***********************************
       Exsr WritSpace;

       //Print CC
       DCc  = 'CC:';
       intlen = %len(DUcc);
       //***** Beg Chg ************ 09/18/14 ***********************************

       strString = %subst(scAddCC:1:intLen);
       strString1= scAddCC;

       Exsr BldTxtLn1;
       DUCc = %subst(scAddCC:1:intPrvVal);
       Write RCc;

       If %subst(strString1:intPrvVal+1:intLen) <> ' ';
          Exsr BldTxtLn2;
          DCc  = ' ';
          DUCc = %subst(scAddCC:intPrvVal+1:incRetVal-intPrvVal);
          Write RCc;
       Endif;

       If incRetVal <> 0 and %subst(scAddCC:incRetVal+1) <> ' ';
          DCc  = ' ';
          DUCc = %subst(scAddCC:incRetVal+1);
          Write RCc;
       Endif;
       //***** End Chg ************ 09/18/14 ***********************************

       close wtClmActO;

       Endsr;
       //===================================================================
       // Write DARTS Remark
       //===================================================================
       Begsr processDarts;

       // Write Diary
       ##key = %EditC(CMco#:'X') + %EditC(CMfnd:'X') +
        %EditC(CMfYr:'X') + %EditC(CMcase:'X');
       ##comm = 'Action Plan Created';
       ##cDat = %EditC(DateTo8(systemDate()):'X');
       ##user = q1user;
       ##pgm  = q1pgm;
       ##iPgm = Q1pgm;
       ##rmcd = GetRmkCd();
       ##dcid = strDcId;
       ##fold = strFold;
       ##secl = '000';
       ##diFl = ' ';
       ##fl01 = ' ';
       ##fl02 = ' ';
       ##seq# = '00000';
       ##com2 = ' ';
       ##apId = ' ';
       ##iDoc = ' ';
       ##iQue = ' ';
       //*** Beg Chg *** 09/05/14 *************************
       //*********** ##imgF = ' ';
       ##imgF = 'O';
       //*** End Chg *** 09/05/14 *************************
       ##iPth = ' ';
       ##pIdx = ' ';
       ##qPty = ' ';
       ##dcTp = ' ';
       ##csId = ' ';
       ##code = ' ';

       WriteDarts(##key:##comm:##cDat:##dcId:##pNam:##secl:##user:
        ##pgm:##fold:##diFl:##iPgm:##fl01:##fl02:##rmCd:##seq#:
        ##sbyl:##com2:##apId:##iDoc:##iQue:##imgF:##iPth:##pIdx:
        ##qPty:##dcTp:##csId:##code);

       Endsr;
       //===================================================================
       // Load Default screen fields
       //===================================================================
       Begsr LoadDefault;

       clear rcdClmInfo;

       scStatusAs = SystemDate();
       scJurst    = CMjrst;
       scAWW      = CMaww;
       scCompRate = '$' + %trim(%Char(CMcrt)) + '/week';
       scHiredate = dtMDYY(CMhiDt);
       //****** Beg Chg **** 09/08/14 ***************************************
       //***** scAdjuster = GetUsrName(GetUserId(CMfead));
       scAdjuster = GetUsrName(GetUserId(CMclex));
       //****** Beg Chg **** 09/08/14 ***************************************
       scDisDate  = dtMDYY(CMdbdt);
       //***** Beg Chg ************ 09/15/14 ***********************************
       //****** scInjury   = CMacds;
       scInjury   = %Trim(CMacDs) + ' ' + %Trim(CMacD2);
       //***** End Chg ************ 09/15/14 ***********************************

       Select;
       When CMltfl = 'I';
          scClmtyp = 'Indemnity';
       When CMltfl = 'M';
          scClmtyp = 'Medical Only';
       When CMltfl = 'R';
          scClmtyp = 'Record Only';
       Endsl;

       Chain (CMco#:CMfnd:CMctg2) pc200;
       If %found and outClientID = '114';
          scCovType = %trim(CMctg2) + ' - ' + %trim(C2desc);
          DCovTp    = %trim(C2desc);
       Else;
          scCovType = %trim(CMctg2);
          DCovTp    = %trim(C2ctg2);
       Endif;

       Chain (CMco#:CMfnd:CMemp#:CMdiv:CMdept) pdp00;
       If %found;
       //***** Beg Chg *** 09/08/14 *************************************
       //***** scDept = %trim(CMdept) + ' - ' + %trim(DPdesc);
          scDept = %trim(DPdesc);
       //***** End chg *** 09/08/14 *************************************
          DDepar = %trim(DPdesc);
       Else;
          scDept = %trim(CMdept);
       Endif;

       Chain CMss# pcd00;
       If %found;
          scDob = dtMDYY(CDdtob);
       Endif;

       If CMoccd <> ' ';
          Chain (CMco#:CMoccd) poc00;
          If %found;
       //***** Beg Chg *** 09/08/14 *************************************
       //***** scOccption = %trim(CMoccd) + ' - ' + %trim(OCdesc);
             scOccption = %trim(OCdesc);
       //***** Beg Chg *** 09/08/14 *************************************
             DOccup = %trim(OCdesc);
          Else;
             scOccption = %trim(CMoccd);
          Endif;
       Else;
          scOccption = %trim(CMocds);
          DOccup     = %trim(CMocds);
       Endif;

       Select;
       When CMsubr = 'N';
          scSubro  = 'Not Subrogated';
       When CMsubr = 'P';
          scSubro  = 'Pending Investigation';
       When CMsubr = 'R';
          scSubro  = 'Subrogation Recovery';
       When CMsubr = 'Y';
          scSubro  = 'Subrograted';
       Endsl;

       strKey = CMnatr;
       Chain (CMco#:strKey) pna00;
       If %found;
          scNature = %trim(CMnatr) + ' - ' + %trim(NAdesc);
          DiNatr   = %trim(NAdesc);
       Else;
          scNature = %trim(CMnatr);
       Endif;

       strKey = CMpart;
       Chain (CMco#:strKey) ppa00;
       If %found;
          scPart = %trim(CMpart) + ' - ' + %trim(PAdesc);
          DiBody = %trim(PAdesc);
       Else;
          scPart = %trim(CMpart);
       Endif;

       strKey = CMcaus;
       Chain (CMco#:strKey) pca02;
       If %found;
          scCause = %trim(CMcaus) + ' - ' + %trim(CAdesc);
          DiCaus  = %trim(CAdesc);
       Else;
          scCause = %trim(CMcaus);
       Endif;

       Chain (CMco#:CMfnd:CMfyr:CMcase) pll00;
       If LLlitg = 'Y';
          scLiti = 'Yes';
       Else;
          scLiti = 'No';
       Endif;

       //***** Beg Chg *** 09/08/14 *****************************************
       //***** scAttorney =  %editc(LLdefa:'X') + '-' + %editc(LLdefm:'X');
       //***** scClmAttny =  %editc(LLplaa:'X') + '-' + %editc(LLplam:'X');
       scAttorney =  %trim(LLdefn);
       scClmAttny =  %trim(LLplan);
       //***** End Chg *** 09/08/14 *****************************************

       Chain (CMco#:CMfnd:CMfyr:CMcase) pti00;
       If %found;
          strKey  = TIjudg;
          strTbId = 'JUDG';
          Chain (CMco#:strTbId:strKey) pgt00;
          If %found;
       //***** Beg Chg *** 09/08/14 *****************************************
       //***** scJudge = %trim(TIjudg) + ' - ' + %trim(GTdesc);
             scJudge = %trim(GTdesc);
       //***** Beg Chg *** 09/08/14 *****************************************
             DJudge  = %trim(GTdesc);
          Else;
             scJudge = %trim(TIjudg);
          Endif;
       Endif;

       Setll (CMco#:CMfnd:CMfyr:CMcase) ptp01;
       Reade (CMco#:CMfnd:CMfyr:CMcase) ptp01;
       Dow not %eof;
          If TPprol = 'TR';
             scTrtPhys = TPphyn;
             leave;
          Endif;
          Reade (CMco#:CMfnd:CMfyr:CMcase) ptp01;
       Enddo;

       Exsr LodFinSum;

       Endsr;
       //**********************************************************
       // Load Finacial Summary
       //**********************************************************
       Begsr LodFinSum;

       For intCount = 1 to 10;
          strKey = RT(intCount);
          strType = GetRsvType(CMco#:strKey);

          Select;
          When strType = 'C';
             scPdInd = scPdInd +  AR(intCount);
             scOtInd = scOtInd +  RR(intCount);
             scTiInd = scTiInd +  AR(intCount) + RR(intCount);

          When strType = 'M';
             scPdMed = scPdMed +  AR(intCount);
             scOtMed = scOtMed +  RR(intCount);
             scTiMed = scTiMed +  AR(intCount) + RR(intCount);

          When strType = 'L';
             scPdLeg = scPdLeg +  AR(intCount);
             scOtLeg = scOtLeg +  RR(intCount);
             scTiLeg = scTiLeg +  AR(intCount) + RR(intCount);

          Other;
             scPdOth = scPdOth +  AR(intCount);
             scOtOth = scOtOth +  RR(intCount);
             scTiOth = scTiOth +  AR(intCount) + RR(intCount);
          Endsl;
       Endfor;

       scPdTot = scPdMed + scPdInd + scPdLeg + scPdOth;
       scOtTot = scOtInd + scOtMed + scOtLeg + scOtOth;
       scTiTot = scTiInd + scTiMed + scTiLeg + scTiOth;

       Endsr;
       //***** Beg Add ************ 09/18/14 *********************
       //**********************************************************
       // Build Text Line 1
       //**********************************************************
       Begsr BldTxtLn1;

       intCnt = 0;
       retVal = 0;
       incRetVal = 0;
       intPrvVal = 0;

       Exsr scanspace;
       If RetVal = intLen or %subst(strString1:intLen+1:1) = ' ';
          intPrvVal = intLen;
       Else;
          intPrvVal = retVal;
       Endif;

       Endsr;
       //**********************************************************
       // Build Text Line 2
       //**********************************************************
       Begsr BldTxtLn2;

       strString = %subst(strString1:intPrvVal+1:intLen);
       Exsr scanspace;

       If RetVal = intLen or %subst(strString1:(intLen+intPrvVal+1):1) = ' ';
          incRetVal = intPrvVal + intLen;
       Else;
          incRetVal = intPrvVal + retVal;
       Endif;

       Endsr;
       //***** End Add ************ 09/18/14 *********************

      /End-Free
     p ProcessAdd      e
       //===================================================================
       // View/Print PDF
       //===================================================================
     d***  Begin Chg  ***  11/25/15  **************************************
     p*BuildPDFUrl     b
     d*BuildPDFUrl     pi           720
     p#BuildPDFUrl     b
     d#BuildPDFUrl     pi           720
     d***  End   Chg  ***  11/25/15  **************************************
     d inDocId                       12
     d inFold                        80

     d strIgFlr        s             50
     d strImgTitle     s             50
     d strImgLnk       s            720

      /free

       RtvRoot(##root);

       strIgFlr = '/' + %trim(##root) + '/' + %trim(inFold);
       strImgTitle = 'Claim Action Plan';

       //****** beg chg ****** 11/18/14 *************************************
       //strImgLnk = strImgTitle + '/iDartsWeb/remarks/attachments.do?meth-
       //od=getAttachmentFile&key=&sequence=&docId=-
       //'+%trim(inDocId)+'&folder='+%trim(strIgFlr)+'&user='+%trim(q1user);

       strImgLnk = strImgTitle + '/iDartsWeb/remarks/attachments.do?method=get-
       AttachmentFile&key='+%trim(strKey#)+'&sequence='+%trim(strSeq#)+'&docId-
       ='+%trim(inDocId)+'&folder='+%trim(strIgFlr)+'^P&user='+%trim(q1user);
       //****** end chg ****** 11/18/14 *************************************

       return strImgLnk;

      /end-free
     d***  Begin Chg  ***  11/25/15  **************************************
     p*BuildPDFUrl     e
     p#BuildPDFUrl     e
     d***  End   Chg  ***  11/25/15  **************************************
       //===================================================================
       // ProcOther - process other options
       //===================================================================
     p ProcOther       b
     d ProcOther       pi

     d                 ds
     d intDltDt                       8  0
     d mm                      1      2
     d dd                      3      4
     d yy                      5      8

      /free

       ReadC sflActPlan;
       Dow not %eof;
          Select;
          // process for deletion of Claim Action Plan
          When LnkClAcDlt = *on;
             LnkClAcDlt = *off;

             If PopTwoBtn('Please confirm if you want to continue with deletion+
              .':'Confirm Deletion':'Confirm':'Cancel');
                Chain scClmAcRrn pca0p;
                If %found(WACAP);
                   Chain (C1key:C1seq#) scm0a;
                   If %found;
                      intDltDt = dtMDYY(Dateto7(SystemDate()));

                      S2comm='Claim Action Plan deleted by: ' + %trim(q1user) +
                       ' on ' + mm + '/' + dd + '/' + yy;
                      S2user = q1user;
                      S2pgm  = q1pgm;
                      S2dt   = DateTo7(SystemDate);
                      S2time = TimeTo6(SystemTime);
                      update scm0a;
                   Endif;
                   delete pca0p;
                Endif;
             Endif;
             unlock WACAP;
          Endsl;

          ReadC sflActPlan;
       Enddo;

      /end-free
     p ProcOther       e
       //===================================================================
       // * utility procedure definitions
       //===================================================================
      /copy sCopyClmPi
