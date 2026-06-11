     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WTCONFIG - Portal Configuration
     H DFTNAME(WICONFIG)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
      *====================================================================*
      *  Created on 08/23/2018 by c:62 for Project SY390                   *
      *====================================================================*
      *  Revisions:                                                        *
      *                                                                    *
      *  04/18/23 C:344 Support: 109069                                    *
      *                 Revised display file to generate a pui.click       *
      *                 onchange of Notification Type.                     *
      *                 Created subroutine DisplayTokens to generate       *
      *                 the email tokens list after the Category           *
      *                 and Type dropdown values have been selected.       *
      *                 Revised the notifications error procedure to       *
      *                 correctly verify redudant records from being added.*
      *                                                                    *
      *  01/20/23 C:344 Project: WC2875                                    *
      *                 Update the connect configuration program use the   *
      *                 GetTokensEmail routine to display the available    *
      *                 tokens.                                            *
      *                                                                    *
      *  01/06/23 C:342 Support: 103418                                    *
      *                 Removed Allow payment on Cancel Pending Policy     *
      *                 checkbox from display file on Connect configuration*
      *                 screen under Consumer Tab.                         *
      *                                                                    *
      *  06/22/22 C:079 Support: 102638                                    *
      *                 Load Action Panel Items using service pgm          *
      *                                                                    *
      *  05/09/22 C:079 Project: 129WWV                                    *
      *                 Application Configuration Security                 *
      *                                                                    *
      *  04/11/22 C:333 Project: SY434                                     *
      *                 - Update Grids for Sorting Filtering Upgrade       *
      *                 - Roles Tab reload Issue.                          *
      *                                                                    *
      *  01/26/22 C:079 Support: 99/100707                                 *
      *                 Condition Scoring edit on WC2820 being installed   *
      *                                                                    *
      *  08/02/21 C:298 Project: WC2820                                    *
      *                 Add Scoring Vendor selection information           *
      *                 to rcdConsume.                                     *
      *                                                                    *
      *  05/27/21 C:318 Support:95270                                      *
      *                 Changes for inquiry mode to make fields disable    *
      *                 and links invisible                                *
      *                                                                    *
      *  11/21/20 C:136 Project: SY407 (DE772)                             *
      *                 Change email address to 60A (display file change)  *
      *                 to fit 10 addresses in 600a field for SNDEMAIL     *
      *                 parm.                                              *
      *                                                                    *
      *  11/20/20 C:136 Project: SY407 (DE798)                             *
      *                 Remove Pending Payment flag (display file change)  *
      *                                                                    *
      *                                                                    *
      *====================================================================*
     fwiconfig  cf   e             workstn extfile('WICONFIG')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflClsTmp:rrnTmpClas)
     f                                     Sfile(sflTmplate:rrnTemplat)
     f                                     sfile(sflQuest  :rrnQuest)
     f                                     sfile(sflQstTmp :rrnQstTmp)
     f                                     sfile(sflNoGrid :rrnSfNotic)
     f                                     sfile(sflDfEmail:rrnSfEmail)
     f                                     sfile(sflRoles  :rrnRoles)
     f                                     sfile(sflRoleAsc:rrnRoleAsc)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwidftemlp uf a e             Disk    Rename(pem01:pem0p)
     fwidftemll if   e           k Disk    Rename(pem01:pem00)
     f                                     Infds(widftemlpfds)
     fwinotip1  uf a e           k Disk    Rename(pn101:pn10p)
     f                                     Infds(winotip1fds)
     fwinotip   uf a e             Disk    Rename(pno01:pno0p)
     fwinotil   if   e           k Disk    Rename(pno01:pno00)
     f                                     Infds(winotipfds)
     fwitmp     uf a e             Disk    Rename(ptm01:ptm0p)
     fwitml     if   e           k Disk    Rename(ptm01:ptm00)
     f                                     InfDS(witmpfds)
     fwitcp     uf a e             Disk    Rename(ptc01:ptc0p) usropn
     fwitcl     uf   e           k Disk    Rename(ptc01:ptc00) usropn
     f                                     InfDS(witcpfds)
     fwmprl1    if   e           k Disk
     f*** Beg Add *** 05/09/22 *************************************************
     fsmupl     if   e           k disk
     fsdtbl1    if   e           k disk
     f*** End Add *** 05/09/22 *************************************************

      // constant
     d intArrDim       c                   10
     d intFlgDim       c                   10

     d message         s            100
      * system data structure
     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0

     Dsirlp          e ds                  extname(SIRLP)    qualified
     Dsirlpprd       e ds                  extname(SIRLPPRD) qualified

     DdsSIRLP          ds                  qualified inz
     D rrn                            9s 0
     D co#                            3s 0
     D key                           40
     D desc                          40
     D status                         1     inz('A')

     DdsSIRLPPRD       ds                  qualified inz
     D rrn                            9s 0
     D co#                            3s 0
     D key                           40
     D product                       25
     D prodSub                       25
     D auth                          10
     D status                         1     inz('A')

     DdsStates         ds                  inz qualified
     D abbr                          10
     D name                          40

     d widftemlpfds...
     d                 ds
     d  WIDFTEMLPrrn...
     d                       397    400b 0

     d winotipfds      ds
     d  WINOTIPrrn           397    400b 0

     d winotip1fds     ds
     d  WINOTIP1rrn          397    400b 0

     D witmpfds        ds
     D  WITMPrrn             397    400b 0

     D witcpfds        ds
     D  WITCPrrn             397    400b 0

     Dwitmpqst       e ds                  extname(WITMPQST)

     D#ofExpCodes      c                   1000
     DdsWIQUP        e ds                  extname(WIQUP)    inz qualified
     DdsWIQUPACD     e ds                  extname(WIQUPACD) inz qualified
     DdsWIQUPEXP     e ds                  extname(WIQUPEXP) inz qualified
     D                                     dim(#ofExpCodes)
     DdsWIQUPSCD     e ds                  extname(WIQUPSCD) inz qualified
     DdsWIQUPin        ds                  likeds(dsWIQUP)    inz
     DdsWIQUPACDin     ds                  likeds(dsWIQUPACD) inz
     DdsWIQUPEXPin     ds                  likeds(dsWIQUPEXP) inz
     D                                     dim(#ofExpCodes)
     DdsWIQUPSCDin     ds                  likeds(dsWIQUPSCD) inz
     DdsWITCP        e ds                  extname(WITCP) inz qualified
     D                                     dim(#ofExpCodes)
     d*** Beg Add *** 05/09/22 *************************************************
     d namesArr        s            100    DIM(20)
     d*** End Add *** 05/09/22 *************************************************

      // List of Keys and RRN

      * work variables
     DdecCount         s             10i 0 inz
     Di1               s             10i 0 inz(1)
     Di2               s             10i 0
     d cltId           s              3
     d fileDate        s              7  0
     d fileRRN         s              9  0
     d fileTime        s              6  0
     d fileUpdated     s              1a
     d fileUser        s             10
     d intCo#          s              3  0
     d isLogoReady     s              1n   inz('1')
     d isTemplateAdd   s              1n
     d mode            s              1
     d strmode         s              1    inz
     d error           s              1n   inz(*off)
     D flgRcdUpdated   s              1
     d saveDate        s              7  0
     d saveTime        s              6  0
     d strBrand        s             80    inz
     d strCmd          s            255    inz varying
     d strKey          s             10
     d strLibWITCP     s             10
     d strLibWITMPQST  s             10
     d strJobN         s              6
     d strSql          s           5000    inz varying
     d intRrn          s              9  0
     d intStart        s              3  0
     d intCount        s              3  0
     D isRefresh       s               n   inz(*on)
     d process         s               n   inz(*off)

     Ddate7            s              7  0
     Dtime6            s              6  0
     d*** Beg Add *** 05/09/22 *************************************************
       dcl-s  inDspFileName char(10);
       dcl-s  inPgmName     char(50);
       dcl-s  inTabName     char(50);
       dcl-s  index         zoned(3:0);
     d*** End Add *** 05/09/22 *************************************************
     d*** Beg Add *** 06/22/22 ************************************************
     d strMenuChoi     s            500    Varying
     d strMenuValu     s            100    Varying
     d*** End Add *** 06/22/22 ************************************************
     d*** Beg Add *** 01/20/23 *** WC2875 *************************************
     ddsEmail        e ds                  extname(sPiEmail:RcdEmails)
     dptrEmail         s               *   inz
     d*** End Add *** 01/20/23 *** WC2875 *************************************

     DGetObjLib        pr                  ExtPgm('RTVOBJLIB')
     D object                        10    Const
     D type                           7    Const
     D library                       10

     dRtvCltId         pr                  ExtPgm('RTVCLTID')
     d cltId                          3

      * main Processing
     d Main            pr

      // Program Start
     d ProgramStart    pr
     d processMe                       n

      // Save Configuration setup
     d SaveDetail      pr

      // Save Data
     d SaveData        pr
     d intStart                            like(intStart)

      // Edit Check
     d EditCheck       pr

      // Load Data
     d LoadData        pr
     d intStart                            like(intStart)

     d/copy sPrCompany
     d/copy sPrDate
     d/copy sPrFiles
     d/copy sPrGenPop
     d/copy sPrModule
     d/copy sPrRtvBrnd
     d/copy sPrRunCmd
     d/copy sPrString
     d/copy sPrSql
     d/copy sPrTable
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrUsrPrms
     d/copy wprPolErr
     d*** Beg Add *** 05/09/22 ************************************************
     d/copy sPrSecure
     d*** End Add *** 05/09/22 ************************************************
     d*** Beg Add *** 06/22/22 ************************************************
     d/copy sPrActPnl
     d*** End Add *** 06/22/22 ************************************************
     d*** Beg Add *** 01/20/23 ***  WC2875  ***********************************
     d/copy sPrEmail
     d*** End Add *** 01/20/23 ***  WC2875  ***********************************

      *Entry of Program.
     dwiConfigPgm      pr                  extpgm('WICONFIG')
     d                                9
     d                                1
     d                               10
     d                                5
     dwiConfigPgm      pi
     d@@rrn                           9
     d@@mode                          1
     d@@next                         10
     d@@TabInd                        5

      /free
       Exec SQL set option commit = *NONE;
       RtvBrnd('I':strBrand);

       // start program processing
       ProgramStart(process);
       //*** Beg Add *** 05/09/22 *********************************************
       LoadTabInfo();
       //*** End Add *** 05/09/22 *********************************************
       // main processing
       If process;
          //*** Beg Chg *** 05/09/22 *********************************************
          //***Main();
          If index <> 0;
             Main();
          Else;
             scCompName = getCompNam(intCo#);
             isNoAccess = *on;
             TabNames = 'Security';
             TabHeight = 560;
             APnlHeight = TabHeight + 60;
             apShowMenu = *on;
             Write rcdSysTbl;
             Exfmt rcdPanel;
             Read rcdSysTbl;
             If scSelOpt <> *blanks;
                @@next = scSelOpt;
             Endif;
          Endif;
       Endif;
          //*** End Add *** 05/09/22 ******************************************

       *InLr = *On;
       Return;
      /end-free

       //===================================================================
       // AEDTemplates - Add/Edit/Delete Templates
       //===================================================================
     pAEDTemplates     b
     dAEDTemplates     pi
     d inRRN                          9  0
     d inMode                         1    Const

     ddecTmpClas       s                   like(rrnTmpClas)
     dtmpErrors        s               n   Inz(*Off)
     disAddTmp         s               n   Inz(*Off)
     disEofWITMPQST    s               n   inz(*off)
     dgridChange       s               n   Inz(*On)
     dsaveDate         s              7  0
     dsaveTime         s              6  0
     dstrSQL           s            500

     DdsWITMPQST       ds                  qualified inz
     D rrn                            9s 0
     D co#                                 like(TMco#)
     D name                                like(TMname)
     D questionId#                         like(TMque00001)
     D operator                            like(TMoperator)
     D operand                             like(TMoperand)

      /Free

       isTmpAED   = *On;
       isDltTmp   = *Off;
       isNoAddTmp = *Off;
       scTmpSave  = 'Save';
       btnTmpSave = *Off;
       btnTmpCanc = *Off;
       btnTmpSvRp = *Off;
       isTemplateAdd = inMode = 'A';

       Exsr WorkfilesRemove;

       Select;
       When inMode = 'D'; // Delete Mode
          isDltTmp    = *On;
          isNoAddTmp  = *On;
          scTmpSave   = 'Delete';
          scTmpHdg    = 'Delete Template';
          scTmpCnPos  = 120;
       When inMode = 'A'; // Add mode
          Exsr WorkfilesCreate;
          isAddTmp    = *On;
          scTmpHdg    = 'Add Template';
          scTmpCnPos  = 0;
       When inMode = 'E'; // Edit Mode
          isNoAddTmp  = *On;
          scTmpHdg    = 'Edit Template';
          scTmpCnPos  = 120;
       EndSl;

       Exsr WorkfilesOpen;

       ExSr  LoadTmpData;
       DoU BtnTmpCanc = *On or (BtnTmpSave = *On And tmpErrors = *Off);
          ExFmt RcdEdtTemp;
          Select;
          When BtnTmpCanc = *On;
             Exsr WorkfilesRemove;
             Leave;
          When BtnTmpSave = *On Or BtnTmpSvRp = *On;
             ExSr TmpErrorChk;
             If Not tmpErrors;
                ExSr SaveTmpData;
             Else;
                If isRefresh;
                   Exsr LoadTmpData;
                Endif;
                btnTmpSave = *Off;
             EndIf;
             btnTmpSvRp = *Off;
          When TmpClassAddEdit() = *On;
             gridChange = *On;
             ExSr LoadTmpClass;
          When TmpQuestAddEdit() = *On;
             gridChange = *On;
             ExSr LoadTmpQuest;
          EndSl;

       EndDo;

       //===================================================================
       // Clear Subfiles
       //===================================================================
       BegSr ClrSubFiles;

        rrnTmpClas = 0;
        clrTmpClas = *On;
        dspTmpClas = *Off;
        rrnQstTmp = 0;
        clrQstTmp = *on;
        dspQstTmp = *off;
        write rcdEdtTemp;

       Endsr;

       //===================================================================
       // Load Template data
       //===================================================================
       BegSr LoadTmpData;

          Clear ptm0p;
          Chain(n) inRRN ptm0p;

          If TMstat = *blank;
             TMstat = 'A';
          EndIf;
          If TMieClCd = *blank;
          EndIf;

          scTmpName  = TMname;
          scMinPrm   = TMminPrm;
          scMaxPrm   = TMmaxPrm;
          scMaxLra   = TMmaxLra;
          scMaxClr   = TMmaxClr;
          scMaxScore = TMmaxScore;
          scMinScore = TMminScore;
          scMaxMod   = TMmaxMod;
          scMinYrs   = TMminYrs;
          scReqHis   = TMreqLhs;
          scReqCon   = TMreqCon;
          scStat     = TMstat;
          scIncExcCC = TMieClCd;

          saveDate = TMdt;
          saveTime = TMtime;

          // Load the Class code grid
          ExSr LoadTmpClass;

          // Load the Questions grid
          ExSr LoadTmpQuest;

          Exsr TmpErrorClr;

       Endsr;
       //===================================================================
       // Load Template Class Grid
       //===================================================================
       BegSr LoadTmpClass;

          If gridChange = *Off;
             LeaveSR;
          Else;
             gridChange = *Off;
          EndIf;

          rrnTmpClas = 0;
          clrTmpClas = *On;
          dspTmpClas = *Off;
          write rcdEdtTemp;
          dspTmpClas = *On;
          clrTmpClas = *Off;

          scClasName = scTmpName;
          If isTemplateAdd;
             scClasName = %char(q1jobN);
          Endif;

          Chain(n) (intCo#:scClasName) ptc00;
          If %Found(witcl);
             DoU %Eof(witcl);
                scClassCd  = TCclas;
                scRateSt = TCrateSt;
                Select;
                When scRateSt = '**';
                   scRateSt = '*ALL';
                When scRateSt = *blanks;
                   scRateSt = '*ALL';
                Endsl;
                scTmpClRrn = WITCPrrn;

                If inMode = 'D';
                   isDltTmpCC = *On;
                Else;
                   isDltTmpCC = *Off;
                EndIf;

                rrnTmpClas = rrnTmpClas + 1;
                write sflClsTmp;
                ReadE(n) (intCo#:scClasName) ptc00;
             EndDo;
          EndIf;

          decTmpClas = rrnTmpClas;
       Endsr;

       //===================================================================
       // Load Template Questiion Grid
       //===================================================================
       BegSr LoadTmpQuest;

        rrnQstTmp= 0;
        clrQstTmp = *on;
        dspQstTmp = *off;
        write rcdEdtTemp;
        dspQstTmp = *on;
        clrQstTmp = *off;
        selQstTmp = *on;

        scQstName = scTmpName;
        If isTemplateAdd;
           scQstName = %char(q1jobN);
        Endif;

        Exec SQL close cursorQstTmp;
        Exec SQL declare cursorQstTmp cursor for
         select
          rrn(WITMPQST), TMco#, TMname, TMquestionId#, TMoperator, TMoperand
          from WITMPQST
          where TMco# = :intCo# and TMname = :scQstName
          order by TMco#, TMname, TMquestionId#;
        Exec SQL open cursorQstTmp;
        Reset isEofWITMPQST;
        Dou isEofWITMPQST;
           Exec SQL fetch next from cursorQstTmp into :dsWITMPQST;
           Select;
           When sqlStt <> '00000';
              isEofWITMPQST = *on;
              Iter;
           Endsl;
           Reset dsWIQUP;
           Exec SQL
            select *
             into :dsWIQUP
             from WIQUP
             where QUCO# = :intCo# and QUid# = :dsWITMPQST.questionId#;
           Reset dsWIQUPACD;
           Exec SQL
            select *
             into :dsWIQUPACD
             from WIQUPACD
             where QUCO# = :intCo# and QUid# = :dsWITMPQST.questionId#;
           scRRNQst   = dsWITMPQST.rrn;
           scQuestTyp = dsWIQUP.QUtype;
           Select;
           When dsWIQUPACD.QUacord# <> 0;
              scQuestTyp = %trim(scQuestTyp) +
               ' (' + %trim(%editc(dsWIQUPACD.QUacord#:'Z')) + ')';
           Endsl;
           scQuestion = dsWIQUP.QUquestion;
           scQstId#   = dsWITMPQST.questionId#;
           scOperator = dsWITMPQST.operator;
           scOperand  = dsWITMPQST.operand;
           isQuestEdt = *Off;
           isQuestDlt = *Off;
           If inMode = 'D';
              isDltTmpQ = *On;
           Else;
              isDltTmpQ = *Off;
           EndIf;
           rrnQstTmp += 1;
           Write sflQstTmp;
           Select;
           When selQstTmp = *on;
              selQstTmp = *off;
           Endsl;
        Enddo;
        Exec SQL close cursorQstTmp;

       Endsr;

       //===================================================================
       // Save Template data
       //===================================================================
       BegSr SaveTmpData;

          If inMode = 'D';
             Chain inRRN ptm0p;
             If %Found(witmp);
                // Remove Questions
                Exec SQL delete from WITMPQST
                 where TMCO# = :intCo# and TMNAME = :TMname;
                // Remove Class codes
                Dou not %found(witcl);
                   Delete (intCo#:TMname) ptc00;
                EndDo;
                // Remove Answers
                Delete ptm0p;
             EndIf;
          Else;
             If inMode = 'A';
                Clear ptm0p;
             Else;
                Chain inRRN ptm0p;
             EndIf;

             TMco#    = intCo#;
             TMname   = scTmpName;
             TMstat   = scStat;
             TMminPrm = scMinPrm;
             TMmaxPrm = scMaxPrm;
             TMmaxLra = scMaxLra;
             TMmaxClr = scMaxClr;
             TMmaxScore = scMaxScore;
             TMminScore = scMinScore;
             TMmaxMod = scMaxMod;
             TMminYrs = scMinYrs;
             TMreqLhs = scReqHis;
             TMreqCon = scReqCon;
             TMieClCd = scIncExcCC;
             TMuser   = q1user;
             TMpgm    = q1pgm;
             TMdt     = DateTo7(SystemDate());
             TMtime   = TimeTo6(SystemTime());

             If inMode = 'A';
                TMiUsr   = q1user;
                TMiPgm   = q1pgm;
                TMiDt    = DateTo7(SystemDate());
                TMiTim   = TimeTo6(SystemTime());
                Write ptm0p;
                Exsr WorkfilesUpdate;
             Else;
                Update ptm0p;
             EndIf;

          EndIf;

       Endsr;

       //===================================================================
       // Validate Template Data
       //===================================================================
       BegSr TmpErrorChk;

          Exsr TmpErrorClr;

          fileUpdated = RcdUpdatedErr('WITMP':inRRN:'TMdt':'TMtime':'TMuser'
           :saveDate:saveTime:'1');
          If fileUpdated <> '0';
             isRefresh = fileUpdated = '2';
             tmpErrors =  *on;
             Leavesr;
          Endif;

          If isAddTmp = *On;
             // Template Name can't be blank
             If scTmpName = ' ';
                mgTmpName = 'Template name must be entered.';
                erTmpName = *On;
                tmpErrors = *On;
             Else;
                Chain (intCo#:scTmpName) ptm00;
                If %Found(witml);
                   mgTmpName = 'Duplicate Template Name.';
                   erTmpName = *On;
                   tmpErrors = *On;
                EndIf;
             EndIf;
          EndIf;

          If scIncExcCC = *blank and decTmpClas > 0;
             erIncExcCC = *on;
             msIncExcCC = 'Include or Exclude must be selected.';
             tmpErrors = *on;
          Endif;

       Endsr;

       //===================================================================
       // Clear Template Data Error Indicators
       //===================================================================
       BegSr TmpErrorClr;
          isRefresh = *off;
          tmpErrors = *Off;
          mgTmpName = ' ';
          erTmpName = *Off;
          msIncExcCC = *blank;
          erIncExcCC = *off;
       Endsr;

       //===================================================================
       // Create temporary workfile(s)
       //===================================================================
       BegSr WorkfilesCreate;
        // with no data;
        RunCmd('SYDUPFILE FROM(WITCP) TO(QTEMP/*SAME) DATA(*NO) LGL(*YES)':57);
        Open WITCL;
        Open WITCP;
       Endsr;

       //===================================================================
       // Close workfile(s)
       //===================================================================
       BegSr WorkfilesClose;
        If %open(WITCP);
           Close WITCL;
           Close WITCP;
        Endif;
       Endsr;

       //===================================================================
       // Open workfile(s)
       //===================================================================
       BegSr WorkfilesOpen;
        Exsr WorkfilesClose;
        If not %open(WITCP);
           Open WITCL;
           Open WITCP;
        Endif;
       Endsr;

       //===================================================================
       // Remove temporary workfile(s)
       //===================================================================
       BegSr WorkfilesRemove;
        Exsr WorkfilesClose;
        //Exec SQL drop table QTEMP/WITMPQST;
        Exec SQL delete from WITMPQST
         where TMCO# = :intCo# and TMNAME = :strJobN;
        Monitor;
           RunCmd('DLTDEPLGL PHYFILE(QTEMP/WITCP)':30);
           RunCmd('DLTOBJ OBJ(QTEMP/WITCP) OBJTYPE(*FILE)':38);
        On-error;
        Endmon;
       Endsr;

       //===================================================================
       // Update temporary workfile(s)
       //===================================================================
       BegSr WorkfilesUpdate;
        Exec SQL update WITMPQST
         set TMNAME = :scTmpName
         where TMCO# = :intCo# and TMNAME = :scQstName;
        Exec SQL update QTEMP/WITCP
         set TCNAME = :scTmpName;
        strSQL = 'insert into ' + %trim(strLibWITCP) + '/WITCP ' +
         'select * from QTEMP/WITCP';
        Exec SQL execute immediate :strSQL;
        Exsr WorkfilesRemove;
       Endsr;

      /End-free

     pAEDTemplates     e
       //===================================================================
       // AEDTmpClassCD - Add/Edit/Delete Templates
       //===================================================================
     pAEDTmpClassCD    b
     dAEDTmpClassCD    pi
     d inRRN                          9  0
     d inMode                         1    Const

     dclsErrors        s               n   Inz(*Off)
     dcurDate          s                   Like(TCdt)
     ddecClass         s              5  0
     disDisplay        s              1n   inz(*on)
     dstrClass         s              5

      /Free

       isDltTC   = *Off;
       isAddTC   = *Off;
       scTCSave  = 'Save';
       curDate   = DateTo7(SystemDate());
       btnTCCanc = *Off;
       btnTCSvRp = *Off;
       btnTCSave = *Off;

       Select;
       When inMode = 'D'; // Delete Mode
          Delete inRRN ptc0p;
          Return;
       When inMode = 'A'; // Add mode
          isDltTC    = *Off;
          isAddTC    = *On;
          scTCHdg    = 'Add Template Class Code';
          scTCSavPos = 225;
       EndSl;

       ExSr LoadTCData;
       //DoU BtnTCCanc = *On or (BtnTCSave = *On And clsErrors = *Off);
       Dou not isDisplay;
          ExFmt RcdTmpClas;
          Exsr TCErrorClr;
          Select;
          When BtnTCCanc = *On;
             Leave;
          When BtnTCSave = *On Or BtnTCSvRp = *On;
             ExSr TCErrorChk;
             Select;
             When clsErrors;
                Reset isDisplay;
             When not clsErrors;
                ExSr SaveTCData;
                If btnTCSvRp = *on;
                   ExSr LoadTCData;
                Endif;
             Other;
             Endsl;
             btnTCSave = *Off;
             btnTCSvRp = *Off;
          EndSl;
       EndDo;

       //===================================================================
       // Load Template Class Code data
       //===================================================================
       BegSr LoadTCData;
          CreateStateLists();
          Reset isDisplay;
          Clear scClassCd;
          If inMode = 'A';
             Clear ptc0p;
             Clear scClassCod;
             TCstat = 'A';
          Else;
             Chain(n) inRRN ptc0p;
          EndIf;

          TCname = scTmpName;
          scStat   = TCstat;
          scRateSt = TCrateSt;
          If isTemplateAdd;
             TCname = %char(q1jobN);
          Endif;
          If scRateSt = *blank;
             scRateSt = '**';
          Endif;

       Endsr;

       //===================================================================
       // Save Template Class Code data
       //===================================================================
       BegSr SaveTCData;
        date7 = DateTo7(SystemDate());
        time6 = TimeTo6(SystemTime());

        Clear ptc0p;
        Reset dsWITCP;
        Reset i1;
        // Read through selected codes
        Dou strClass = *blank;
           strClass = ParseString(scClassCod:',':1:*on);
           Select;
           When strClass = *blank;
              Iter;
           Endsl;
           decClass = %dec(strClass:5:0);
           Exec sql
            select count(*) into :decCount
             from WITCP
             where TCco# = :intCo# and TCname = :scClasName
              and TCrateSt = :scRateSt and TCclas = :decClass;
           Select;
           When decCount <> *zero;
              Iter;
           Endsl;
           Exec sql
            insert into WITCP values(:intCo#, :scClasName, :decClass, '',
                :scRateSt,
                :q1User, :q1Pgm, :date7, :time6,
                :q1User, :q1Pgm, :date7, :time6, :scStat);
           i1 += 1;
           dsWITCP(i1).TCrateSt = scRateSt;
           dsWITCP(i1).TCclas = decClass;
        Enddo;

       Endsr;

       //===================================================================
       // Validate Template Class Code Data
       //===================================================================
       BegSr TCErrorChk;

          If inMode = 'A';
             i1 = 0;
             Dou strClass = *blank;
                i1 += 1;
                strClass = ParseString(scClassCod:',':i1:*off);
                Select;
                When strClass = *blank;
                   Iter;
                Endsl;
                decClass = %dec(strClass:5:0);
                // Duplicates are not allowed
                Chain(n) (intCo#:TMname:scRateSt:decClass) ptc00;
                If %Found(witcl);
                   mtClassCd = %trim(mtClassCd) + %trim(strClass) + ',';
                   erClassCd = *On;
                   clsErrors = *On;
                EndIf;
             Enddo;
             If mtClassCd <> *blanks;
                mtClassCd = %scanrpl(',':'':mtClassCd:%len(%trim(mtClassCd))-1);
                mtClassCd = 'Class Code(s) already exist for this template - '
                 + mtClassCd;
             Endif;
          Endif;
       Endsr;

       //===================================================================
       // Clear Template Class Code error conditions
       //===================================================================
       Begsr TCErrorClr;
        isDisplay = *off;
        clsErrors  = *off;
        mtClassCd  = ' ';
        erClassCd  = *off;
       Endsr;

      /End-free

     pAEDTmpClassCD    e

       //===================================================================
       // Determine if any exposure changes were made
       //===================================================================
     PChkExposureChgs...
     P                 b
     DChkExposureChgs...
     D                 pi             1n
      /Free
       For i1 = 1 to #ofExpCodes;
          i2 = %lookup(dsWIQUPEXP(i1).QUexpCode:dsWIQUPEXPin(*).QUexpCode);
          Select;
          When dsWIQUPEXP(i1).QUexpCode = *zero;
             Iter;
          When i2 <= 0;
             Return *on;
          When dsWIQUPEXP(i1).QUst <> dsWIQUPEXPin(i2).QUst;
             Return *on;
          Endsl;
       Endfor;
       For i1 = 1 to #ofExpCodes;
          i2 = %lookup(dsWIQUPEXPin(i1).QUexpCode:dsWIQUPEXP(*).QUexpCode);
          Select;
          When dsWIQUPEXPin(i1).QUexpCode = *zero;
             Iter;
          When i2 <= 0;
             Return *on;
          When dsWIQUPEXPin(i1).QUst <> dsWIQUPEXP(i2).QUst;
             Return *on;
          Endsl;
       Endfor;
       Return *off;
      /End-free
     PChkExposureChgs...
     P                 e

       //===================================================================
       // Verify the history files match the current files
       //===================================================================
     PChkQuestHistFiles...
     P                 b
     DChkQuestHistFiles...
     D                 pi             1n
      /Free
       Select;
       When not ChkFileLayouts('WIQHP':'WIQUP')
        or  not ChkFileLayouts('WIQHPACD':'WIQUPACD')
        or  not ChkFileLayouts('WIQHPSCD':'WIQUPSCD')
        or  not ChkFileLayouts('WIQHPEXP':'WIQUPEXP');
          PopOneBtn('The layouts for the history files are not in sync. ' +
           'Please contact ' + %trim(strBrand) + ' for assistance.'
           :'Question Hitory File Issue');
          Return *off;
       Endsl;
       Return *on;
      /End-free
     PChkQuestHistFiles...
     P                 e
      //===================================================
      // Default Email Address Edits
      //===================================================
     p DefaultEmailErrors...
     p                 b
     d                 pi              n
      /Free
       Return *Off;
      /End-Free
     p DefaultEmailErrors...
     p                 e
      //===================================================
      // Create state list for selection
      //===================================================
     pCreateStateLists...
     p                 b
     d                 pi
      /Free
       chRateSt = '*ALL - All States';
       cvRateSt = '**';
       Exec SQL close cursorStates;
       Exec SQL declare cursorStates cursor for
        select STKEY, STNAME
         from STSTP
         where STstat <> 'I' and STnSt <> '  '
         order by STKEY;
       Exec SQL open cursorStates;
       Dou not IsValidFetch(sqlstt);
          Exec SQL fetch next from cursorStates into :dsStates;
          Select;
          When not IsValidFetch(sqlstt);
             Iter;
          Endsl;
          chRateSt = %trim(chRateSt) + ',' + %trim(dsStates.abbr) + ' - ' +
           TitleCase(dsStates.name);
          cvRateSt = %trim(cvRateSt) + ',' + dsStates.abbr;
       Enddo;
       Return;
      /End-free
     pCreateStateLists...
     p                 e

      //===================================================
      // Maintenance of Default Email Addresses
      //===================================================
     p DefaultEmailAddEdit...
     p                 b
     d                 pi              n

     d isEmailProcess  s               n
     d isEmailDisplay  s               n
      //*** Beg Chg *** 01/20/23 *** WC2875 *****************************
     d***strGroup        s             10a
     d strGroup        s            100a   inz
      //*** End Chg *** 01/20/23 *** WC2875 *****************************

      /Free
       isWindow = *on;
       dspEmlSave = *Off;
       dspEmlNext = *Off;
       Select;
       When btnAddDfEm = *On;
          isEmailProcess = *On;
          isEmailDisplay = *On;
          btnAddDfEm = *Off;
          dspEmlSave = *On;
          dspEmlNext = *On;
          posEmlCanc = 810;
          rrnWIDFEML = 0;
          Exsr LoadScreen;
       Other;
          DoU %Eof;
             Readc sflDfEmail;
             If %Eof;
                Leave;
             EndIf;
             Select;
             When isEdtEmail = *On;
                isEdtEmail = *Off;
                isEmailProcess = *On;
                isEmailDisplay = *On;
                dspEmlSave = *On;
                posEmlCanc = 920;
                Exsr LoadScreen;
             When isDltEmail = *On;
                isEmailProcess = *On;
                isEmailDisplay = *Off;
                isDltEmail = *Off;
                If PopTwoBtn('Please confirm deletion.':
                'Default Email Addresses':'Delete':'Cancel':500:300);
                   Exsr DeleteData;
                Endif;
             EndSl;
          Enddo;
       EndSl;
       Dou isEmailDisplay = *Off;
          If isEmailDisplay = *Off;
             Leave;
          EndIf;
          isEmailDisplay = *Off;
          Exfmt RcdDfEmEnt;
          Select;
          When btnCxEmail = *On;
             btnCxEmail = *Off;
             isEmailDisplay = *Off;
             Iter;
          When btnSvEmail = *On or btnSvNxEml = *On;
             If DefaultEmailErrors() = *On;
                isEmailDisplay = *On;
                Iter;
             Endif;
             Exsr SaveData;
             Select;
             When btnSvNxEml = *On;
                rrnWIDFEML = 0;
                Exsr LoadScreen;
                isEmailDisplay = *On;
             Endsl;
             btnSvEmail = *Off;
             btnSvNxEml = *Off;
          Endsl;
       Enddo;
       Return isEmailProcess;
       //--------------------------------
       Begsr DeleteData;
        Chain rrnWIDFEML WIDFTEMLP;
        Delete WIDFTEMLP;
       Endsr;
       //--------------------------------
       Begsr LoadScreen;
        Chain(n) rrnWIDFEML pem0p;
        If Not(%Found);
           Clear pem0p;
        Endif;
        scHidCo# = intCo#;
        scHidGroup = scPolOrClm;
        scGrpDsr = EMfnd;
        scEmailCgy = EMctgy;
        GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');
        scEmailAdd = EMDftEmail;
        scLogo = EMlogo;
        Write rcdDfEmEnt;
       Endsr;
       //--------------------------------
       Begsr SaveData;
        Chain rrnWIDFEML WIDFTEMLP;
        EMco#  = intCo#;
        EMctgy = scEmailCgy;
        EMfnd  = scGrpDsr;
        EMDftEmail = scEmailAdd;
        EMlogo = scLogo;
        EMuser = Q1user;
        EMpgm  = Q1pgm;
        EMdt   = DateTo7(SystemDate());
        EMtime = TimeTo6(SystemTime());
        If %Found(WIDFTEMLP);
           Update pem0p;
        Else;
           EMiusr = EMuser;
           EMipgm = EMpgm;
           EMidt  = EMdt;
           EMitim = EMtime;
           Write pem0p;
        Endif;
       Endsr;
      /End-Free
     p DefaultEmailAddEdit...
     p                 e
      //===================================================
      // Returns Darts Remark Code Description from STCTP
      //===================================================
     p GetDartsDesc    b
     d GetDartsDesc    pi            40a
     d  inCode                        5a
     d outDesc         s             40a
      /Free
       Exec Sql
        Select CTdesc
        Into :outDesc
        From STCTP
        Where CTrmcd = :inCode;
       Return outDesc;
      /End-Free
     p GetDartsDesc    e
      //===================================================
      // Returns Notification Category Description from WTGIP
      //===================================================
     p GetNoCatDesc    b
     d GetNoCatDesc    pi            40a
     d  inCo#                         3  0
     d  inCode                        4a
     d outDesc         s             40a
      /Free
       Exec Sql
        Select GIdesc
        Into :outDesc
        From WTGIP
        Where GIco# = :inCo#
         And GItbid = :inCode;
       Return outDesc;
      /End-Free
     p                 e
      //===================================================
      // Returns Notification Type Description from WTGTP
      //===================================================
     p GetNoTypDesc    b
     d GetNoTypDesc    pi            40a
     d  inCo#                         3  0
     d  inCategory                    4a
     d  inType                       10a
      //*** Beg Chg **** 01/20/23 **** WC2875 ***************************
     d***outGroup                     10a
     d  outGroup                    100a
      //*** End Chg **** 01/20/23 **** WC2875 **************************
     d outDesc         s             40a
      /Free
       Exec Sql
        Select GTdesc, GTcf01
        Into :outDesc, :outGroup
        From WTGTP
        Where GTco# = :inCo#
         And GTtbid = :inCategory
         And GTkey = :inType;
       Return outDesc;
      /End-Free
     p                 e
       //*** Beg Add *** 05/09/22 *********************************************
       //===================================================================
       // Load Tab Information
       //===================================================================
     P LoadTabInfo     B
     D LoadTabInfo     PI
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

       // Templates tab

       Chain (intCo#:'WICONFIG':scPnlHdg:'Templates'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          TabNames = 'Templates';
          index = 1;
          namesArr(index) = 'Templates';
       Endif;

       // Questions
       Chain (intCo#:'WICONFIG':scPnlHdg:'Questions'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          If index = 0;
             TabNames = 'Questions';
          Else;
             TabNames = %Trim(TabNames) + ', Questions';
          Endif;
          index = index + 1;
          namesArr(index) = 'Questions';
       Endif;

       // Notifications
       Chain (intCo#:'WICONFIG':scPnlHdg:'Notifications'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          If index = 0;
             TabNames = 'Notifications';
          Else;
             TabNames = %Trim(TabNames) + ', Notifications';
          Endif;
          index = index + 1;
          namesArr(index) = 'Notifications';
       Endif;

       // Consumer
       Chain (intCo#:'WICONFIG':scPnlHdg:'Consumer'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          If index = 0;
             TabNames = 'Consumer';
          Else;
             TabNames = %Trim(TabNames) + ', Consumer';
          Endif;
          index = index + 1;
          namesArr(index) = 'Consumer';
       Endif;

       // Roles
       Chain (intCo#:'WICONFIG':scPnlHdg:'Roles'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          If index = 0;
             TabNames = 'Roles';
          Else;
             TabNames = %Trim(TabNames) + ', Roles';
          Endif;
          index = index + 1;
          namesArr(index) = 'Roles';
       Endif;

     P LoadTabInfo     E
       //*** End Add *** 05/09/22 **********************************************
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n

      /free

       // Program Mode.
       If %parms >= 2;
          mode = @@mode;
       Endif;
       If q1prms >= 2;
          mode = @@mode;
          //*** Beg Add *** 05/09/22 ***************************************
          Select;
          When @@mode = 'D';
             dspMode = *on;
          Endsl;
          //*** End Add *** 05/09/22 ***************************************
       Endif;


       RtvCltId(cltId);
       isWC2820 = '0';
       If cltId = '133';
          isWC2820 = *on;
       Endif;
       isRoleDone = *on;
       isDspMode = *Off;

       Select;
       When mode = 'C'; // Change Mode
       When mode = 'D'; // Display Mode
          isDspMode = *On;
       EndSl;

       strJobN = %char(q1jobN);

       // Tab Index Mode.
       TabIndex = 0;

       // Get Company no.
       intCo# = UserCo#(q1user);

       // Get Company name
       scCompName = getCompNam(intCo#);

       // Panel Heading
       scPnlHdg = GetModuleProperty('Portal Module':'Name')+' Configuration';

       //Check for applicable options
       //*** Beg Chg *** 06/22/22 ********************************************
       //***scMenuChoi = 'Application Configuration,Claims Configuration';
       //***scMenuValu = 'WTAPPCFG,WTCLMCFG';

       //***If isModEnabled('Managed Care');
       //***   scMenuChoi = %trim(scMenuChoi) + ',Managed Care Configuration';
       //***   scMenuValu = %trim(scMenuValu) + ',MTINTCFG';
       //***Endif;

       //***If isModEnabled('PEO Module');
       //***   scMenuChoi = %trim(scMenuChoi) + ',Policy Configuration';
       //***   scMenuValu = %trim(scMenuValu) + ',WTPOLCFG';
       //***Endif;

       //***If isModEnabled('Portal Module');
       //***   scMenuChoi = %trim(scMenuChoi) + ',' +
       //***    GetModuleProperty('Portal Module':'Name') + ' Configuration';
       //***   scMenuValu = %trim(scMenuValu) + ',WICONFIG';

       //***Endif;
       GetActPnlItems(strMenuChoi:strMenuvalu);
       scMenuChoi = strMenuChoi;
       scMenuValu = strMenuValu;
       //*** End Chg *** 06/22/22 ********************************************
       GetObjLib('WITCP':'*FILE':strLibWITCP);
       GetObjLib('WITMPQST':'*FILE':strLibWITMPQST);

       BtnAddTmp = *Off;
       BtnAddQst = *Off;
       processMe = *on;

       // Convert depricated rate set code to rating state
       Exec sql
        update WITCP
         set TCRATEST = case
              when TCRATECD <> '' and TCRATEST = '' then
               (select RSJRST
                 from WMRSP
                 where RSCO#=TCCO# and TCRATECD=RSRTCD)
              else TCRATEST
              end;

       Return;

      /end-free
     p ProgramStart    e

       //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     D Main            PI

      *** Beg Chg *** 01/20/23 *** WC2875 ****************************
     d***strGroup        s             10a
     d strGroup        s            100a   inz
      *** End Chg *** 01/20/23 *** WC2875 ****************************
     DisEofSIRLP       s              1n   inz('0')
     DisEofWIQUP       s              1n   inz('0')
     DstrSqlWIQUP      s            500
     dsaveTabIndex     s                   like(tabIndex)

     Dwiqup          e ds                  extname(WIQUP)

     DdsWIQUP          ds                  qualified inz
     D rrn                            9s 0
     D co#                                 like(QUco#)
     D question                            like(QUquestion)
     D id#                                 like(QUid#)
     D type                                like(QUtype)
     D answer                              like(QUanswer)
     D parent                              like(QUparent)
     D seq#                                like(QUseq#)
     D code                                like(QUcode)
     D status                              like(QUstat)
      /free


       ExitNow = *Off;
       error = *Off;

       // Process until exit button is pressed.
       DoU ExitNow = *On;

          // Load tabs
          Exsr LoadTabs;

          // Process policy Configuration screen.
          Exsr ExfmtScreens;

          error = *off;

          // Perform actions on screen.
          Select;

          // Process question add/edit
          When QuestionsAddEdit() = *on;

          // Process template add/edit
          When TemplatesAddEdit() = *on;

          When NotificationsAddEdit() = *on;

          When DefaultEmailAddEdit() = *on;

          When RolesAddEdit() = *on;
             isRefresh = *on;

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;

          // do nothing - catch tab click event
          When tabClick = *on;
             If tabIndex <> saveTabIndex;
             Endif;
             tabClick = *off;

          When btnRefresh = *On;
             btnRefresh = *Off;
             error = *Off;

          When btnSave = *on;
             error   = *off;

             If mode <> 'D';
                editCheck();
                If error = *off;
                   SaveDetail();
                Endif;
             Endif;
          //*** Beg Add *** 05/09/22 ******************************************
          // Add User Access Window Logic
          When btnUsrAcc = *on;
             inDspFileName = 'WICONFIG';
             inPgmName     = scPnlHdg;
             inTabName     =  namesArr(TabIndex + 1);
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;
          //*** End Add *** 05/09/22 ******************************************
          Endsl;

          // Clear Subfiles
          ExSr ClrSubFiles;

       Enddo;
       //===================================================================
       // Clear Subfiles
       //===================================================================
       BegSr ClrSubFiles;

          rrnTemplat = 0;
          clrTemplat = *On;
          dspTemplat = *Off;
          write rcdPortTmp;

          rrnQuest   = 0;
          clrQuest   = *On;
          dspQuest   = *Off;
          Write RcdPortQA;

          rrnSfNotic = 0;
          clrNotific  = *On;
          dspNotific  = *Off;
          rrnSfEmail = 0;
          clrDfEmail = *On;
          dspDfEmail = *Off;
          Write RcdPortNot;

          rrnRoles   = 0;
          clrRoles   = *On;
          dspRoles   = *Off;
          Write RcdPortRol;

       Endsr;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       aMenHeight = 120;

       Select;
       //*** Beg Chg *** 05/09/22 **********************************************
       //***When TabIndex = 0;
       When namesArr(TabIndex + 1) = 'Templates';
       //*** End Chg *** 05/09/22 **********************************************
          TabHeight = 895;
          APnlHeight = TabHeight + 60;
          apFootHght = aMenHeight + 125;
          apShowMenu = *on;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt RcdPortTmp;
          Read rcdSysTbl;
          Read rcdPanel;
       //*** Beg Chg *** 05/09/22 **********************************************
       //***When TabIndex = 1;
       When namesArr(TabIndex + 1) = 'Questions';
       //*** End Chg *** 05/09/22 **********************************************
          TabHeight = 800;
          APnlHeight = TabHeight + 60;
          apFootHght = aMenHeight + 125;
          apShowMenu = *on;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt RcdPortQA;
          Read rcdSysTbl;
          Read rcdPanel;
       //*** Beg Chg *** 05/09/22 **********************************************
       //***When TabIndex = 2;
       When namesArr(TabIndex + 1) = 'Notifications';
       //*** End Chg *** 05/09/22 **********************************************
          TabHeight = 940;
          APnlHeight = TabHeight + 60;
          apFootHght = aMenHeight + 125;
          apShowMenu = *on;
          Exsr LoadPortSrCfg;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt RcdPortNot;
          Read rcdSysTbl;
          Read rcdPanel;
       //*** Beg Chg *** 05/09/22 **********************************************
       //***When TabIndex = 3;
       When namesArr(TabIndex + 1) = 'Consumer';
       //*** End Chg *** 05/09/22 **********************************************
          TabHeight = 770;
          APnlHeight = TabHeight + 60;
          apFootHght = aMenHeight + 125;
          apShowMenu = *on;
          Exsr LoadPortSrCfg;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt RcdConsume;
          Read rcdSysTbl;
          Read rcdPanel;
       //*** Beg Chg *** 05/09/22 **********************************************
       //***When TabIndex = 4;
       When namesArr(TabIndex + 1) = 'Roles';
       //*** End Chg *** 05/09/22 **********************************************
          TabHeight = 870;
          APnlHeight = TabHeight + 60;
          apFootHght = aMenHeight + 125;
          apShowMenu = *on;
          Write rcdSysTbl;
          Write rcdPanel;
          Exfmt RcdPortRol;
          Read rcdSysTbl;
          Read rcdPanel;
       EndSl;

       Endsr;

       //===================================================================
       // Save Portal S/R Configuration Changes;
       //===================================================================
       //===================================================================
       // Read action panel;
       //===================================================================
       BegSr ReadAction;
        Read rcdSysTbl;
       Endsr;
       //===================================================
       // Load Consumer
       //===================================================
       Begsr LoadConsumer;
       EndSr;
       //===================================================
       // Load Default Email Addresses
       //===================================================
       Begsr LoadDefaultEmails;
        selDfEmail = *On;
        dspDfEmail = *On;
        clrDfEmail = *Off;
        Setll intCo# pem00;
        Dou %Eof(WIDFTEMLP);
           Reade intCo# pem00;
           If %Eof;
              Leave;
           Endif;
           scCo# = EMco#;
           scDfCatCde = EMctgy;
           scDfCatDsc = GetNoCatDesc(EMco#:EMctgy);
           scDfEmlGrp = EMfnd;
           If EMfnd = 0;
              scDfGrpDsc = '*All Groups';
           Else;
              scDfGrpDsc = %EditC(EMfnd:'X') + ' - ' + GetGrpName(EMfnd);
           Endif;
           scDfEmAddr = EMDftEmail;
           scDtlLogo  = EMlogo;
           isEdtEmail = *Off;
           isDltEmail = *Off;
           rrnSfEMail += 1;
           rrnWIDFEML = WIDFTEMLPrrn;
           Write sflDfEmail;
           selDfEmail = *Off;
        Enddo;
       Endsr;
       //===================================================
       // Load Notifications
       //===================================================
       Begsr LoadNotifications;
        selNotific = *On;
        dspNotific = *On;
        clrNotific = *Off;
        Setll intCo# pno00;
        Dou %Eof(WINOTIL);
           Reade intCo# pno00;
           If %Eof;
              Leave;
           Endif;
           scCo# = NOco#;
           scNoTypCde = NOtype;
           scNoTypDsc = GetNoTypDesc(NOCo#:NOctgy:NOtype:strGroup);
           Select;
           When scPolOrClm = 'P' and strGroup <> 'POLICY'
            And strGroup <> 'QTESTATUS';
             Iter;
           When scPolOrClm = 'C' and strGroup <> 'CLAIMS';
             Iter;
           Endsl;
           scNoCatCde = NOctgy;
           scNoCatDsc = GetNoCatDesc(NOCo#:NOctgy);
           scNoDart = NOdart;
           scNoDrtDsc = GetDartsDesc(NOdart);
           scBody = NObody;
           Select;
           When NoStat = 'A';
              scNoStat = 'Active';
           When NoStat = 'I';
              scNoStat = 'Inactive';
           EndSl;
           isEdtNotif = *Off;
           isDltNotif = *Off;
           rrnSfNotic += 1;
           rrnWINOTIP = WINOTIPrrn;
           Write sflNoGrid;
           selNotific = *Off;
        Enddo;
       EndSr;
       //===================================================================
       // Load Policy Templates
       //===================================================================
       BegSr LoadPolTemp;

       selTemplat = *On;
       dspTemplat = *On;
       clrTemplat = *Off;
       Chain intCo# ptm00;
       If %Found(witml);
          DoU %Eof(witml);
             scCo#     = TMco#;
             scTmpName = TMname;
             scMinPrmG = TMminPrm;
             scMaxPrmG = TMmaxPrm;
             scMinYrs  = TMminYrs;
             scMaxLRa  = TMmaxLRa;
             scMaxCLR  = TMmaxCLR;
             scMaxScore = TMmaxScore;
             scMinScore = TMminScore;

             If TMreqCon = 'Y';
                scReqConG = 'Yes';
             Else;
                scReqConG = 'No';
             EndIf;

             If TMreqLHS = 'Y';
                scReqHisG = 'Yes';
             Else;
                scReqHisG = 'No';
             EndIf;

             If TMieClCd = 'E';
                scIeClCd = 'Exclude';
             Else;
                scIeClCd = 'Include';
             EndIf;

             isEdtTmp = *Off;
             isDltTmp = *Off;

             scMaxModG  = TMmaxMod;
             scTmpRrn   = WITMPrrn;
             rrnTemplat = rrnTemplat + 1;
             write sflTmplate;
             selTemplat = *Off;
             ReadE intCo# ptm00;
          EndDo;
       EndIf;

       EndSr;

       //===================================================================
       // Load Policy Questions
       //===================================================================
       BegSr LoadPolQuestions;
        dspQuest   = *On;
        clrQuest   = *Off;
        selQuest   = *on;
        scDate7 = DateTo7(SystemDate());
        strSqlWIQUP = 'select ' +
         'rrn(WIQUP), QUco#, QUquestion, QUid#, QUtype, QUanswer, ' +
         'QUparent, QUseq#, QUcode, QUstat ' +
         ' from WIQUP ' +
         ' where QUco# = ' + %char(intCo#) +
         ' order by QUco#, QUseq#, QUid#';
        Exec SQL close cursorQuestion;
        Exec SQL prepare questions from :strSqlWIQUP;
        Exec SQL declare cursorQuestion cursor for questions;
        Exec SQL open cursorQuestion;
        Reset isEofWIQUP;
        Dou isEofWIQUP;
           Exec SQL fetch next from cursorQuestion into :dsWIQUP;
           Select;
           When sqlStt <> '00000';
              isEofWIQUP = *on;
              Iter;
           Endsl;
           scRRN      = dsWIQUP.rrn;
           scCo#      = dsWIQUP.co#;
           scQuestion = dsWIQUP.question;
           scId#      = dsWIQUP.id#;
           scType     = dsWIQUP.type;
           scAnswer   = dsWIQUP.answer;
           isQuestEdt = *Off;
           isQuestDlt = *Off;
           scParentYN = 'No';
           If dsWIQUP.parent <> 0;
              scParentYN = 'Yes';
           Endif;
           scSeq#     = dsWIQUP.seq#;
           scCode     = dsWIQUP.code;
           Select;
           When dsWIQUP.status = 'I';
              scStatus = 'Inactive';
           Other;
              scStatus = 'Active';
           Endsl;
           rrnQuest  += 1;
           Write sflQuest;
           Select;
           When selQuest = *on;
              selQuest   = *off;
           Endsl;
        Enddo;
       EndSr;

       //===================================================================
       // Load Portal S/R Configuration
       //===================================================================
       BegSr LoadPortSrCfg;
        If not isRefresh;
           Leavesr;
        Endif;
        isRefresh = *off;
        hgPortalIP = GetModuleProperty('Portal Module':'Name') +
         ' Site IP Address';
        //*** Beg Del *** 01/20/23 *** WC2875 ******************************
        //***tpTokenIP = GetModuleProperty('Portal Module':'Name') +
        //***' Site IP Address';
        //***tpTokenLgo = 'Token is only availble in Email Body. ' +
        //***'If not Logo is defined, token will be cleared.';
        //***If not isLogoReady;
          //***tpTokenLgo = 'Logo NOT currently available. ' +
          //***'If added to Email Body, token will be cleared.';
        //***Endif;
        //*** End Del *** 01/20/23 *** WC2875 ******************************
        Clear N1dt;
        Clear N1time;
        Chain(n) intCo# pn10p;
        If %Found;
           scPortalIP = N1PortIp;
           scRetryLim = N1Retries;
           scLockSrRs = N1LckRSbmt;
           scAlwIfPnd = N1AllwPend;
           scPndInBal = N1InclPend;
           scAlwCx    = N1alwCx;
           scAlwCxPnd = N1alwCxPnd;
           scDftUW    = N1dftUW;
           //*** Beg Del *** 01/06/23 *****************************
           //*** scPmtCxPnd = N1pmtCxPnd;
           //*** End Del *** 01/06/23 *****************************
           scScore = N1scrvnd;
           scEffDat = dtMDYY(N1scrvndef);
        EndIf;
        saveDate = N1dt;
        saveTime = N1time;
       Endsr;

       //===================================================
       // Load Roles
       //===================================================
       Begsr LoadRoles;
       //***** Beg Del ***** 04/11/22 *****************************
         //***If not isRefresh;
          //***  Leavesr;
         //***Endif;
       //***** End Del ***** 04/11/22 *****************************
        dspRoles   = *on;
        clrRoles   = *off;
        selRoles   = *on;
        isRefresh = *off;
        strSql =
         'select rrn(SIRLP), RLCO#, RLKEY, RLDESC, RLSTAT ' +
         ' from SIRLP ' +
         ' where RLco# = ' + %char(intCo#) +
         ' order by RLKEY';
        Exec SQL close cursorRoles;
        Exec SQL prepare roles from :strSql;
        Exec SQL declare cursorRoles cursor for roles;
        Exec SQL open cursorRoles;
        Reset isEofSIRLP;
        Dou isEofSIRLP;
           Exec sql fetch next from cursorRoles into :dsSIRLP;
           Select;
           When not IsValidFetch(sqlstt);
              isEofSIRLP = *on;
              Iter;
           Endsl;
           rrnRoles  += 1;
           scRoleRRN = dsSIRLP.rrn;
           scCo# = dsSIRLP.co#;
           scKey = dsSIRLP.key;
           scRoleDsc = dsSIRLP.desc;
           Exec sql
            select count(*) into :decCount
             from SIRLPPRD
             where RLco# = :scCo# and RLkey = :scKey
              and RLproduct = 'iDARTS';
           visEdtRole = *on;
           visDltRole = *on;
           visRmkCode = *on;
           Select;
           When isDspMode = *on;
              visEdtRole = *off;
              visDltRole = *off;
              visRmkCode = *off;
           When dsSIRLP.key = *blank;
              visEdtRole = *off;
              visDltRole = *off;
              scKey = '**ALL**';
           When decCount = 0;
              visRmkCode = *off;
           Endsl;
           Write sflRoles;
           selRoles   = *off;
        Enddo;
       EndSr;
       //===================================================================
       // Load Screen(s)
       //===================================================================
       BegSr LoadTabs;

        saveTabIndex = tabIndex;

        Select;
        //Load Policy Templates
        When TabIndex = 0;
           Exsr LoadPolTemp;
        //Load Policy Questions
        When TabIndex = 1;
           Exsr LoadPolQuestions;
        When TabIndex = 2;
           Exsr LoadNotifications;
           Exsr LoadDefaultEmails;
        When TabIndex = 3;
           Exsr LoadConsumer;
        When TabIndex = 4;
           Exsr LoadRoles;
        EndSl;

       Endsr;

      /end-free
     P Main            E

      //===================================================
      // Process notifications
      //===================================================
     pNotificationsAddEdit...
     p                 b
     dNotificationsAddEdit...
     d                 pi              n

     d isNotiProcess   s               n
     d isNotiDisplay   s               n
      *** Beg Chg *** 01/20/23 *** WC2875 *****************************
     d***strGroup        s             10a
     d strGroup        s            100    inz
      *** End Chg *** 01/20/23 *** WC2875 *****************************
      *** Beg Add *** 01/20/23 *** WC2875 *****************************
     d str100          s            100    inz
      *** End Add *** 01/20/23 *** WC2875 *****************************

      /Free
       isWindow = *on;
       dspNotSave = *Off;
       dspNotNext = *Off;
       Clear erNotiMsg1;
       Clear erNotiType;
       modeEdit = *Off;
       Select;
       When btnAddNoti = *On;
          isNotiProcess = *On;
          isNotiDisplay = *On;
          btnAddNoti = *Off;
          dspNotSave = *On;
          dspNotNext = *On;
          posNotCanc = 860;
          rrnWINOTIP = 0;
          Exsr LoadScreen;
       Other;
          DoU %Eof;
             Readc sflNoGrid;
             If %Eof;
                Leave;
             EndIf;
             Select;
             When isEdtNotif = *On;
                isEdtNotif = *Off;
                isNotiProcess = *On;
                isNotiDisplay = *On;
                dspNotSave = *On;
                modeEdit = *On;
                posNotCanc = 970;
                Exsr LoadScreen;
                Leave;
             When isDltNotif = *On;
                isNotiProcess = *On;
                isNotiDisplay = *Off;
                isDltNotif = *Off;
                If PopTwoBtn('Please confirm deletion.':'Notifications':
                 'Delete':'Cancel':500:300);
                   Exsr DeleteData;
                Endif;
                Leave;
             EndSl;
          Enddo;
       EndSl;
       Dou isNotiDisplay = *Off;
          If isNotiDisplay = *Off;
             Leave;
          EndIf;
          isNotiDisplay = *Off;
          visNumb1  = *Off;
          visFlag1 = *Off;
          VisFlag2 = *Off;
          Exfmt RcdNotEntr;
          Select;
          When btnCxNotif = *on;
             btnCxNotif = *off;
             isNotiDisplay = *off;
             Iter;
          When btnSvNotif = *on or btnSvNxNot = *on;
             If NotificationErrors(isRefresh) = *On;
                //*** Beg Del *** 04/18/23 *** 109069 *******************
                // If isRefresh;
                //   Exsr LoadScreen;
                // Endif;
                //*** End Del *** 04/18/23 *** 109069 *******************
                isNotiDisplay = *on;
                Iter;
             Endif;
             Exsr SaveData;
             Select;
             When btnSaveNxt = *on;
                rrnWINOTIP = 0;
                Exsr LoadScreen;
                isNotiDisplay = *on;
             Endsl;
             btnSvNotif = *off;
             btnSvNxNot = *off;
          //*** Beg Add *** 04/18/23 *** 109069 **************************
          // If user selects a new combination of Notification Category and
          // Type then load the available email tokens via the subroutine.
          Other;
             If scNotiCtgy <> ' ' and scNotiType <> ' ';
                isNotiDisplay = *on;
                If NotificationErrors(isRefresh) = *On;
                   iter;
                Else;
                   Exsr DisplayTokens;
                Endif;
             Endif;
          //*** End Add *** 04/18/23 *** 109069 **************************
          Endsl;
       Enddo;
       Return isNotiProcess;

       //--------------------------------
       Begsr DeleteData;
        Chain rrnWINOTIP WINOTIP;
        Delete WINOTIP;
       Endsr;
       //--------------------------------
       Begsr LoadScreen;
        Chain(n) rrnWINOTIP pno0p;
        If Not(%Found);
           Clear pno0p;
           scNotiStat = 'A';
        Else;
           scNotiStat = NOstat;
        Endif;
        scHidCo# = intCo#;
        scHidGroup = scPolOrClm;
        scNotiCtgy = NOctgy;
        scNotiType = NOtype;
        scNotiRmCd = NOdart;
        scEmalSubj = NOsubj;
        scEmalBody = NObody;
        scNumb1 = NOnumb1;
        scFlag1 = NOflag1;
        scFlag2 = NOflag2;

        //*** Beg Add *** 01/20/23 *** WC2875 ******************************
        Clear dsEmail;
        ptrEmail = %addr(dsEmail);
        GetNoTypDesc(NOCo#:NOctgy:NOtype:strGroup);

        // Determine Run Type
        clear str100;
        Select;
           When strGroup = 'QTESTATUS';
              str100 = 'Connect Quote';
           When strGroup = 'POLICY';
              str100 = 'Connect PayGo';
        Endsl;

        scTokenCrt = 'job = ''' + %char(q1JobN) + '''';
        GetTokensEmail(ptrEmail:str100);
        //*** End Add *** 01/20/23 *** WC2875 ******************************

        saveDate = NOdt;
        saveTime = NOtime;

        Write rcdNotEntr;
       Endsr;
        //*** Beg Add *** 04/18/23 *****************************************
       //-------------------------------------------------------------------
       // SUBROUTINE: Load Notification Screen And Available Email Tokens
       //             When Adding New Notification Portal Category and Type.
       //-------------------------------------------------------------------
       Begsr DisplayTokens;
          Clear dsEmail;
          ptrEmail = %addr(dsEmail);
          GetNoTypDesc(intCo#:scNotiCtgy:scNotiType:strGroup);

          // Determine Run Type
          Clear str100;
          Select;
             When strGroup = 'QTESTATUS';
                str100 = 'Connect Quote';
             When strGroup = 'POLICY';
                str100 = 'Connect PayGo';
          Endsl;

        scTokenCrt = 'job = ''' + %char(q1JobN) + '''';
        GetTokensEmail(ptrEmail:str100);

        saveDate = NOdt;
        saveTime = NOtime;

        Write rcdNotEntr;
       Endsr;
        //*** Beg End *** 04/18/23 *****************************************
       //--------------------------------
       Begsr SaveData;
        Chain rrnWINOTIP WINOTIP;
        NOco#  = intCo#;
        NOctgy = scNotiCtgy;
        NOtype = scNotiType;
        NOdart = scNotiRmCd;
        NOstat = scNotiStat;
        NOsubj = scEmalSubj;
        NObody = scEmalBody;
        NOnumb1 = scNumb1;
        NOflag1 = scFlag1;
        NOflag2 = scFlag2;
        NOuser = Q1user;
        NOpgm  = Q1pgm;
        NOdt   = DateTo7(SystemDate());
        NOtime = TimeTo6(SystemTime());
        If %Found(WINOTIP);
           Update pno0p;
        Else;
           NOiusr = NOuser;
           NOipgm = NOpgm;
           NOidt  = NOdt;
           NOitim = NOtime;
           Write pno0p;
        Endif;
       Endsr;
       //--------------------------------
      /End-Free
     pNotificationsAddEdit...
     p                 e
      //====================================================
      // Error Check Notification Maintenance Screen
      //====================================================
     pNotificationErrors...
     p                 b
     dNotificationErrors...
     d                 pi              n
     d outRefresh                      n
      /Free
       outRefresh = *off;
       //*** Beg Add *** 04/18/23 **********************************   *****
       Select;
       When rrnWINOTIP <> 0;
       //*** End Add *** 04/18/23 **********************************   *****
       fileUpdated = RcdUpdatedErr('WINOTIP':rrnWINOTIP:'NOdt':'NOtime':'NOuser'
        :saveDate:saveTime:'1');
       If fileUpdated <> '0';
          outRefresh = fileUpdated = '2';
          Return *on;
       Else;
          Return *off;
       Endif;
       //*** Beg Del *** 04/18/23 ******************************************
       // Select;
       //*** End Del *** 04/18/23 ******************************************
       When rrnWINOTIP = 0;
          //*** Beg Add *** 04/18/23 ***************************************
          Clear erNotiType;
          Clear erNotiMsg1;
          //*** End Add *** 04/18/23 ***************************************
          Chain (intCo#:scNotiCtgy:scNotiType) pno00;
          If %Found;
             erNotiType = *On;
             erNotiMsg1 = 'This category and type has already been entered.';
             Return *On;
          Endif;
          Return *Off;
       Other;
          Return *Off;
       EndSl;
      /End-Free
     pNotificationErrors...
     p                 e
       //===================================================
       // Process questions (add/edit)
       //===================================================
     pQuestionsAddEdit...
     p                 b
     dQuestionsAddEdit...
     d                 pi              n

     DdecClass         s              5  0
     DdecId#           s              7  0
     DisAssocaited     s               n   inz(*on)
     DisDisplayScreen  s               n   inz(*on)
     DisHistoryOk      s               n   inz(*on)
     DisProcess        s               n   inz(*off)
     DisUnderConst     s               n   inz(*off)
     DstrClass         s              5
     DstrSQL           s            500    varying
     DstrTempMbr       s             10    varying inz('S')
     DstrTempLib       s             10    varying inz('QTEMP')

     Di1               s             10i 0
     Di2               s             10i 0

     Dincrementer      c                   1

     DsizeExp          s             10i 0 inz(1000)
      /Free
       Exsr TempDataCreate;
       isWindow = *on;
       scStubHdg = 'Questions';
       modeAdd  = *off;
       modeEdit = *off;
       Clear scId#;
       Select;
       When BtnAddQst = *on and ChkQuestHistFiles;
          isProcess = *on;
          modeAdd = *on;
          scStubHdg = %trim(scStubHdg) + ' - Add';
          BtnAddQst = *off;
       Other;
          Dou %eof;
             Readc sflQuest;
             Select;
             When %eof;
                Iter;
             When isQuestEdt = *on and ChkQuestHistFiles;
                scStubHdg = %trim(scStubHdg) + ' - Edit';
                isProcess = *on;
                isQuestEdt = *off;
                modeEdit = *on;
                disQuest = *off;
                Clear tipQuest;
                Select;
                When QuestionAssociated(scCo#:scId#);
                Endsl;
             When isQuestDlt = *on and ChkQuestHistFiles;
                scStubHdg = %trim(scStubHdg) + ' - Delete';
                isProcess = *on;
                isQuestDlt = *off;
                isDisplayScreen = *off;
                Select;
                When QuestionAssociated(scCo#:scId#);
                   PopOneBtn('Question can not be deleted. It is currently ' +
                    'associated with another question, template, policy or ' +
                    'application.':
                    scStubHdg:'Ok':500:300);
                When PopTwoBtn('Please confirm deletion.':scStubHdg:
                 'Delete':'Cancel':500:300);
                   Exsr DeleteData;
                Endsl;
             Endsl;
             If isProcess;
                Leave;
             Endif;
          Enddo;
       Endsl;
       If isProcess and isDisplayScreen;
          Exsr LoadScreen;
          Dow isDisplayScreen;
             isDisplayScreen = *off;
             Exfmt RcdQuest;
             Select;
             When btnCancel = *on;
                btnCancel = *off;
                isDisplayScreen = *off;
                Iter;
             Endsl;
             If btnSave = *on or btnSaveNxt = *on;
                If QuestionErrors(isRefresh);
                   If isRefresh;
                      Exsr LoadScreen;
                   Endif;
                   isDisplayScreen = *on;
                   Iter;
                Endif;
                Exsr SaveData;
                Select;
                When btnSaveNxt = *on;
                   Clear scId#;
                   Exsr LoadScreen;
                   isDisplayScreen = *on;
                Endsl;
                btnSave = *off;
                btnSaveNxt = *off;
             Endif;
          Enddo;
       Endif;
       Exsr TempDataDelete;
       Return isProcess;

       //================================================================
       // Delete the data
       //================================================================
       Begsr DeleteData;
        Exec sql
         DELETE FROM WIQUP
          WHERE QUCO# = :scCo# and QUID# = :scId#;
        Exec sql
         DELETE FROM WIQUPACD
          WHERE QUCO# = :scCo# and QUID# = :scId#;
        Exec sql
         DELETE FROM WIQUPEXP
          WHERE QUCO# = :scCo# and QUID# = :scId#;
        Exec sql
         DELETE FROM WIQUPSCD
          WHERE QUCO# = :scCo# and QUID# = :scId#;
       Endsr;

       //================================================================
       // Load the add/edit screen fields
       //================================================================
       Begsr LoadScreen;

        Reset dsWIQUP;
        Reset dsWIQUPACD;
        Reset dsWIQUPEXP;
        Reset dsWIQUPSCD;
        Clear scExpList;
        Clear scExpCode;

        Exec SQL
         select *
          into :dsWIQUP
          from WIQUP
          where QUCO# = :intCo# and QUid# = :scId#;
        Exec SQL
         select *
          into :dsWIQUPACD
          from WIQUPACD
          where QUCO# = :intCo# and QUid# = :scId#;
        Exec SQL
         select *
          into :dsWIQUPSCD
          from WIQUPSCD
          where QUCO# = :intCo# and QUid# = :scId#;
        Exec SQL close exp;
        Exec SQL
         declare exp cursor for
          select *
           from WIQUPEXP
           where QUCO# = :intCo# and QUid# = :scId#
           order by QUco#, QUid#, QUrateSet, QUexpCode;
        Exec SQL open exp;
        Exec SQL
         fetch next from exp
          for :sizeExp rows
          into :dsWIQUPEXP;
        sizeExp = sqlEr3;
        // Save incoming information
        dsWIQUPin = dsWIQUP;
        dsWIQUPACDin = dsWIQUPACD;
        dsWIQUPEXPin = dsWIQUPEXP;
        dsWIQUPSCDin = dsWIQUPSCD;

        scCo#      = intCo#;
        scId#      = dsWIQUP.QUid#;
        scType     = dsWIQUP.QUtype;
        scQuestion = dsWIQUP.QUquestion;
        scAnswer   = dsWIQUP.QUanswer;
        scCode     = dsWIQUP.QUcode;
        scSeq#     = dsWIQUP.QUseq#;
        scParent   = dsWIQUP.QUparent;
        scPrntOpr  = dsWIQUP.QUprntOpr;
        scPrntOpd  = dsWIQUP.QUprntOpd;
        scAcord#   = dsWIQUPACD.QUacord#;
        scAcord#FL = dsWIQUPACD.QUacord#FL;
        scRateSt   = dsWIQUPSCD.QUrateSt;
        scFactor   = dsWIQUPSCD.QUfactor;
        For i1 = 1 to sizeExp;
           Select;
           When i1 = 1;
              scRateSet = dsWIQUPEXP(i1).QUrateSet;
              Select;
              When dsWIQUPEXP(i1).QUst = *blank;
                 scSt = RatingSt(scCo#:scRateSet);
              Other;
                 scSt = dsWIQUPEXP(i1).QUst;
              Endsl;
           Other;
              scExpList = %trim(scExpList) + ',';
           Endsl;
           scExpList = %trim(scExpList) + %editc(dsWIQUPEXP(i1).QUexpCode:'X');
        Endfor;
        scExpCode = scExpList;

        If scRateSt = *blank;
           scRateSt = '**';
        Endif;
        If scSt = *blank;
           scSt = '**';
        Endif;

        Select;
        When dsWIQUP.QUstat = *blank;
           dsWIQUP.QUstat = 'A';
        Endsl;
        scStat = dsWIQUP.QUstat;
        // Check if question assigned to a template
        If scStat = 'A';
           disStat = *off;
           Clear tipStat;
           Reset decCount;
           Exec sql
            select count(*) into :decCount
             from WITMPQST
             where TMco# = :scCo# and TMQUESTIONID# = :scId#;
           If decCount >= 1;
              disStat = *on;
              tipStat = 'Question assiocaited with template(s).  ' +
               'Question cannot be inactivated.';
           Endif;
        Endif;

        saveDate = dsWIQUP.QUdt;
        saveTime = dsWIQUP.QUtime;

        chType = 'GTCO# = ' + %char(intCo#) + ' and GTTBID = ''QST''' +
         ' and GTSTAT <> ''I''';
        chParent = 'QUCO# = ' + %char(intCo#) +
         ' and QUID# <> ' + %char(dsWIQUP.QUid#) + ' and QUID# > 0' +
         ' and QUstat <> ''I''';
       Endsr;

       //================================================================
       // Save the data elements
       //================================================================
       Begsr SaveData;
        date7 = DateTo7(SystemDate());
        time6 = TimeTo6(SystemTime());

        Select;
        When scParent <= 0;
           Clear scPrntOpr;
           Clear scPrntOpd;
        Other;
           Select;
           When scPrntOpr = 'Exists';
              Clear scPrntOpd;
           Endsl;
        Endsl;

        Select;
        When modeAdd = *on;
          Exec SQL
           select max(QUID#)
            into :decId#
            from WIQUP
            where QUCO# = :intCo#;
          scId# = decId# - %rem(decId#:incrementer) + incrementer;
        When modeEdit = *on;
        Endsl;

        If scRateSt = '**';
           scRateSt = *blank;
        Endif;
        If scSt = '**';
           scSt = *blank;
           scRateSet = *blank;
        Endif;

        If scPrntOpd2 <> scPrntOpd;
           scPrntOpd = scPrntOpd2;
        Endif;

        Select;
        When modeAdd = *on;
           Exec sql
            INSERT INTO WIQUP VALUES(:scCo#, :scId#, :scSeq#, :scType, :scCode,
             :scQuestion, :scAnswer, :scParent, :scPrntOpr, :scPrntOpd, :scStat,
             :q1Pgm, :q1User, :date7, :time6, :q1Pgm, :q1User, :date7, :time6);
        When modeEdit = *on;
           Exec sql
            UPDATE WIQUP SET QUCO# = :scCo#, QUID# = :scId#, QUSEQ# = :scSeq#,
             QUTYPE = :scType, QUCODE = :scCode, QUQUESTION = :scQuestion,
             QUANSWER = :scAnswer, QUPARENT = :scParent, QUPRNTOPR = :scPrntOpr,
             QUPRNTOPD = :scPrntOpd,
             QUSTAT = :scStat, QUPGM = :q1Pgm,
             QUUSER = :q1User, QUDT = :date7, QUTIME = :time6
              WHERE QUCO# = :scCo# and QUID# = :scId#;
        Endsl;
        Select;
        When Upper(scType) = 'ACORD' and modeAdd = *on;
           Exec sql
            INSERT INTO WIQUPACD VALUES(:scCo#, :scId#, :scAcord#, :scAcord#FL,
             :scStat, :q1Pgm, :q1User, :date7, :time6, :q1Pgm, :q1User, :date7,
             :time6);
        When Upper(scType) = 'ACORD' and modeEdit = *on;
           Exec sql
            UPDATE WIQUPACD SET QUCO# = :scCo#, QUID# = :scId#,
            QUACORD# = :scAcord#, QUACORD#FL = :scAcord#FL,
             QUSTAT = :scStat, QUPGM = :q1Pgm,
             QUUSER = :q1User, QUDT = :date7, QUTIME = :time6
              WHERE QUCO# = :scCo# and QUID# = :scId#;
        When Upper(scType) = 'SCHEDULE RATING' and modeAdd = *on;
           Exec sql
            INSERT INTO WIQUPSCD VALUES(:scCo#, :scId#, :scRateSt, :scFactor,
             :scStat, :q1Pgm, :q1User, :date7, :time6, :q1Pgm, :q1User, :date7,
             :time6);
        When Upper(scType) = 'SCHEDULE RATING' and modeEdit = *on;
           Exec sql
            UPDATE WIQUPSCD SET QUCO# = :scCo#, QUID# = :scId#,
            QURATEST = :scRateSt, QUFACTOR = :scFactor,
             QUSTAT = :scStat, QUPGM = :q1Pgm,
             QUUSER = :q1User, QUDT = :date7, QUTIME = :time6
              WHERE QUCO# = :scCo# and QUID# = :scId#;
        When Upper(scType) = 'EXPOSURE CODE';
           Reset dsWIQUPEXP;
           Reset i1;
           // Delete exposure codes first, then rewrite
           Exec sql
            DELETE FROM WIQUPEXP
             WHERE QUCO# = :scCo# and QUID# = :scId#;
           // Read through selected codes
           Dou strClass = *blank;
              strClass = ParseString(scExpCode:',':1:*on);
              Select;
              When strClass = *blank;
                 Iter;
              Endsl;
              decClass = %dec(strClass:5:0);
              Exec sql
               INSERT INTO WIQUPEXP VALUES(:scCo#, :scId#, :scSt,
                :scRateSet, :decClass,
                :scStat, :q1Pgm, :q1User, :date7, :time6, :q1Pgm, :q1User,
                :date7, :time6);
              i1 += 1;
              dsWIQUPEXP(i1).QUst = scSt;
              dsWIQUPEXP(i1).QUexpCode = decClass;
           Enddo;
        Endsl;

        dsWIQUP.QUid# = scCo#;
        dsWIQUP.QUid# = scId#;
        dsWIQUP.QUtype = scType;
        dsWIQUP.QUquestion = scQuestion;
        dsWIQUP.QUanswer = scAnswer;
        dsWIQUP.QUcode = scCode;
        dsWIQUP.QUseq# = scSeq#;
        dsWIQUP.QUparent = scParent;
        dsWIQUP.QUprntOpr = scPrntOpr;
        dsWIQUP.QUprntOpd = scPrntOpd;
        dsWIQUP.QUstat = scStat;
        dsWIQUPACD.QUacord# = scAcord#;
        dsWIQUPACD.QUacord#FL = scAcord#FL;
        dsWIQUPSCD.QUrateSt = scRateSt;
        dsWIQUPSCD.QUfactor = scFactor;

        If dsWIQUP <> dsWIQUPin or dsWIQUPACD <> dsWIQUPACDin
         or dsWIQUPSCD <> dsWIQUPSCDin
         or ChkExposureChgs;
           Exsr SaveHistory;
        Endif;
       Endsr;

       //================================================================
       // Save the history information
       //================================================================
       Begsr SaveHistory;
        Exec sql
         INSERT INTO WIQHP VALUES(:scCo#, :scId#, :scSeq#, :scType, :scCode,
          :scQuestion, :scAnswer, :scParent, :scPrntOpr, :scPrntOpd, :scStat,
          :q1Pgm, :q1User, :date7, :time6, :q1Pgm, :q1User, :date7, :time6);
        Select;
        When Upper(scType) = 'ACORD';
           Exec sql
            INSERT INTO WIQHPACD VALUES(:scCo#, :scId#, :scAcord#, :scAcord#FL,
             :scStat, :q1Pgm, :q1User, :date7, :time6, :q1Pgm, :q1User, :date7,
             :time6);
        When Upper(scType) = 'SCHEDULE RATING';
           Exec sql
            INSERT INTO WIQHPSCD VALUES(:scCo#, :scId#, :scRateSt, :scFactor,
             :scStat, :q1Pgm, :q1User, :date7, :time6, :q1Pgm, :q1User, :date7,
             :time6);
        When Upper(scType) = 'EXPOSURE CODE';
           // Read through selected codes
           For i2 = 1 to i1;
              decClass = dsWIQUPEXP(i1).QUexpCode;
              Exec sql
               INSERT INTO WIQHPEXP VALUES(:scCo#, :scId#, :scSt,
                :scRateSet, :decClass,
                :scStat, :q1Pgm, :q1User, :date7, :time6, :q1Pgm, :q1User,
                :date7, :time6);
           Endfor;
        Endsl;
       Endsr;

       //================================================================
       // Create temporary data files
       //================================================================
       Begsr TempDataCreate;
        CreateStateLists();
       Endsr;

       //================================================================
       // Remove temporary data files
       //================================================================
       Begsr TempDataDelete;
        Monitor;
        On-error;
        Endmon;
       Endsr;
      /End-free
     pQuestionsAddEdit...
     p                 e

       //===================================================================
       // Process questions (add/edit)
       //===================================================================
     pQuestionAssociated...
     p                 b
     dQuestionAssociated...
     d                 pi              n
     d inCo#                          3  0
     d inId#                          7  0

     ddecCount1        s              7  0
     ddecCount2        s              7  0
     ddecCount3        s              7  0
      /Free
       If inID# = *zero;
          Return *off;
       Endif;
       Exec SQL
        select count(*)
            into :decCount1
            from WIQUP
            where QUCO# = :inCo# and QUPARENT = :inId#;
       Exec SQL
        select count(*)
            into :decCount2
            from WITMPQST
            where TMCO# = :inCo# and TMQUESTIONID# = :inId#;
       Exec SQL
        select count(*)
            into :decCount3
            from WIAPPANS
            where APCO# = :inCo# and APID# = :inId#;
       Return (decCount1 > 0 or decCount2 > 0 or decCount3 > 0);
      /End-free
     pQuestionAssociated...
     p                 e

       //===================================================================
       // Process questions (add/edit)
       //===================================================================
     pQuestionErrors...
     p                 b
     dQuestionErrors...
     d                 pi              n
     d outRefresh                     1n

     dstrMsg           c                   const('This question is currently -
     d                                     associated with another -
     d                                     question, template, -
     d                                     policy or application.')
     dstrErr           c                   const('Association exists with -
     d                                     another question, template, -
     d                                     policy or application.')
      /Free
       Exsr ClearErrors;
       Exec SQL
        select rrn(WIQUP), QUdt, QUtime, QUuser
         into :fileRRN, :fileDate, :fileTime, :fileUser
         from WIQUP
         where QUCO# = :intCo# and QUid# = :scId#;
       fileUpdated = RcdUpdatedErr('WIQUP':fileRRN:'QUdt':'QUtime':'QUuser'
        :saveDate:saveTime:'1');
       If fileUpdated <> '0';
          outRefresh = fileUpdated = '2';
          Return *on;
       Endif;
       If scStat = 'I' and dsWIQUPin.QUstat <> scStat
        and QuestionAssociated(scCo#:scId#)
        and not PopTwoBtn(strMsg +
         ' Please confirm inactivation.':'Inactivate Question?':
         'Inactivate':'Cancel');
          msStat = strErr;
          erStat =  *on;
          Return *on;
       Endif;
       If dsWIQUPin.QUquestion <> scQuestion
        and QuestionAssociated(scCo#:scId#)
        and not PopTwoBtn(strMsg +
         ' This will change the question associated with previously ' +
         'submitted. Do you want to continue?':'Change Question?');
          msQuestion = strErr;
          erQuestion =  *on;
          Return *on;
       Endif;
       If dsWIQUPexpin(1).QUrateSet <> scRateSet
        and QuestionAssociated(scCo#:scId#)
        and not PopTwoBtn(strMsg +
         ' This change will no longer associate the Rate Set Code ' +
         'corretly. Do you want to continue?':'Change Rate Set Code?');
          msRateSet = strErr;
          erRateSet =  *on;
          Return *on;
       Endif;
       Return *off;
       //==================================================================
       Begsr ClearErrors;
        outRefresh = *off;
        erStat = *off;
        erQuestion = *off;
       Endsr;
      /End-free
     pQuestionErrors...
     p                 e

       //===================================================================
       // Save Data.
       //===================================================================
     p SaveDetail      b
     d SaveDetail      pi

      /free

       Exsr SavePortSrCfg;

       //===================================================================
       // Save Portal S/R Configuration Changes;
       //===================================================================
       BegSr SavePortSrCfg;
        If btnSave = *off;
           Leavesr;
        Endif;
        btnSave = *off;
        If scPortalIP <> N1PortIp
         Or scRetryLim <> N1Retries
         Or scLockSrRs <> N1LckRSbmt
         Or scAlwIfPnd <> N1AllwPend
         Or scPndInBal <> N1InclPend
         Or scAlwCx    <> N1alwCx
         Or scAlwCxPnd <> N1alwCxPnd
         Or scDftUW    <> N1dftUW
         //*** Beg Del *** 01/06/23 *****************************
         //*** Or scPmtCxPnd <> N1pmtCxPnd
         //*** End Del *** 01/06/23 *****************************
         Or scScore <> N1scrvnd
         Or scEffDat <> dtMDYY(N1scrvndef);
           fileUpdated = RcdUpdatedErr('WINOTIP1':WINOTIP1rrn:'N1dt':'N1time'
            :'N1user':saveDate:saveTime:'1');
           If fileUpdated <> '0';
              If fileUpdated = '2';
                 isRefresh = *on;
              Endif;
              Leavesr;
           Endif;
           Chain intCo# pn10p;
           N1co# = intCo#;
           N1PortIp = scPortalIP;
           N1Retries = scRetryLim;
           N1LckRSbmt = scLockSrRs;
           N1AllwPend = scAlwIfPnd;
           N1InclPend = scPndInBal;
           N1alwCx    = scAlwCx;
           N1alwCxPnd = scAlwCxPnd;
           N1dftUW = scDftUW;
           //*** Beg Del *** 01/06/23 *****************************
           //*** N1pmtCxPnd = scPmtCxPnd;
           //*** End Del *** 01/06/23 *****************************
           N1scrvnd = scScore;
           N1scrvndef = dtCymd(scEffDat);
           N1User = Q1user;
           N1Pgm = Q1pgm;
           N1dt = DateTo7(SystemDate());
           N1time = TimeTo6(SystemTime());
           If %Found(WINOTIP1);
              Update pn10p;
           Else;
              N1iusr = N1user;
              N1ipgm = N1pgm;
              N1idt  = N1dt;
              N1itim = N1time;
              Write pn10p;
           Endif;
        Endif;
        isRefresh = *on;
       Endsr;
       //===================================================================
       // Write/Update Record
       //===================================================================
       BegSr SrWrtUpd;


       Endsr;
       //===================================================================

      /end-free

     p SaveDetail      e

       //===================================================================
       // Process templates (add/edit)
       //===================================================================
     pTemplatesAddEdit...
     p                 b
     dTemplatesAddEdit...
     d                 pi              n

     DisTmpProc        s               n   Inz(*Off)
     DtempRRN          s              9  0 Inz(0)

     Di1               s             10i 0

     Dincrementer      c                   1

     DsizeExp          s             10i 0 inz(1000)
     DdsWITMP        e ds                  extname(WITMP)    inz qualified
     D                                     dim(1000)
      /Free

       isWindow   = *on;

       Select;
       When BtnAddTmp = *On;
          AEDTemplates(tempRRN:'A');
          BtnAddTmp = *Off;
          isTmpProc = *On;
       Other;
          Dou %eof;
             Readc sflTmplate;
             Select;
             When %Eof;
                Iter;
             When isEdtTmp = *On;
                AEDTemplates(scTmpRrn:'E');
                BtnAddTmp = *Off;
                isTmpProc = *On;
             When isDltTmp = *On;
                AEDTemplates(scTmpRrn:'D');
                BtnAddTmp = *Off;
                isTmpProc = *On;
             Endsl;
             If isTmpProc;
                Leave;
             Endif;
          Enddo;
       Endsl;

       Return isTmpProc;

      /End-free
     pTemplatesAddEdit...
     p                 e
       //===================================================================
       // Process template class codes (add/edit)
       //===================================================================
     pTmpClassAddEdit...
     p                 b
     dTmpClassAddEdit...
     d                 pi              n

     DisTmpClassProc   s               n   Inz(*Off)
     DtmpClassRRN      s              9  0 Inz(0)
      /Free

       Select;
       When BtnAddCC = *On;
          AEDTmpClassCD(tmpClassRRN:'A');
          BtnAddCC       = *Off;
          isTmpClassProc = *On;
       Other;
          Dou %eof;
             Readc sflClsTmp;
             Select;
             When %Eof;
                Iter;
             When isDltTmpC = *On;
                If PopTwoBtn('Please confirm deletion.' +
                 '<br>Rating state: ' + scRateSt +
                 '<br>Class code: ' + %editc(scClassCd:'X')
                 :'Template Class Code':'Delete':'Cancel':175:85);
                   AEDTmpClassCd(scTmpClRrn:'D');
                Endif;
                isDltTmpC = *Off;
                isTmpClassProc = *On;
             Endsl;
             If isTmpClassProc;
                Leave;
             Endif;
          Enddo;
       Endsl;

       Return isTmpClassProc;

      /End-free
     pTmpClassAddEdit...
     p                 e
       //===================================================================
       // Process template questions (add/edit)
       //===================================================================
     pTmpQuestAddEdit...
     p                 b
     dTmpQuestAddEdit...
     d                 pi              n

     DdecId#           s              7  0
     DisDisplayScreen  s               n   inz(*on)
     DisError          s               n   inz(*off)
     DisProcess        s               n   inz(*off)
     DisUnderConst     s               n   inz(*off)

     Dincrementer      c                   1

     DdsWITMPQST     e ds                  extname(WITMPQST) inz
      /Free

       isWindow = *on;
       scStubHdg = 'Template Questions';
       modeAdd  = *off;
       modeEdit = *off;
       Clear scQstId#;
       Select;
       When BtnAddQst = *on;
          isProcess = *on;
          modeAdd = *on;
          scStubHdg = %trim(scStubHdg) + ' - Add';
          Clear scRRNQst;
          BtnAddQst = *off;
       Other;
          Dou %eof;
             Readc sflQstTmp;
             Select;
             When %eof;
                Iter;
             When isQuestEdt = *on;
                scStubHdg = %trim(scStubHdg) + ' - Edit';
                isProcess = *on;
                isQuestEdt = *off;
                modeEdit = *on;
             When isQuestDlt = *on;
                scStubHdg = %trim(scStubHdg) + ' - Delete';
                isProcess = *on;
                isQuestDlt = *off;
                Exsr DeleteData;
                isDisplayScreen = not PopOneBtn(
                 'Record has been deleted.'
                 :scStubHdg);
             Endsl;
             If isProcess;
                Leave;
             Endif;
          Enddo;
       Endsl;
       If isProcess and isDisplayScreen;
          Exsr LoadScreen;
          Dow isDisplayScreen;
             isDisplayScreen = *off;
             Exfmt RcdTmpQst;
             Select;
             When btnCancel = *on;
                btnCancel = *off;
                isDisplayScreen = *off;
                Iter;
             Endsl;
             If btnSave = *on or btnSaveNxt = *on;
                Exsr EditData;
                Select;
                When isError;
                   If isRefresh;
                      Exsr LoadScreen;
                   Endif;
                   isDisplayScreen = *on;
                Other;
                   Exsr SaveData;
                Endsl;
                Select;
                When btnSaveNxt = *on;
                   Exsr LoadScreen;
                   isDisplayScreen = *on;
                Endsl;
                btnSave = *off;
                btnSaveNxt = *off;
             Endif;
          Enddo;
       Endif;
       Return isProcess;

       //================================================================
       // Delete the data
       //================================================================
       Begsr DeleteData;
        Exec sql
         DELETE FROM WITMPQST
          WHERE rrn(WITMPQST) = :scRRNQst;
       Endsr;

       //================================================================
       // Edit the data
       //================================================================
       Begsr EditData;
        isError = *off;
        fileUpdated = RcdUpdatedErr('WITMPQST':scRRNQst:'TMdt':'TMtime':'TMuser'
         :saveDate:saveTime:'1');
        If fileUpdated <> '0';
           isRefresh = fileUpdated = '2';
           isError =  *on;
           Leavesr;
        Endif;
       Endsr;
       //================================================================
       // Load the add/edit screen fields
       //================================================================
       Begsr LoadScreen;

        scQstName = scTmpName;
        If isTemplateAdd;
           scQstName = %char(q1jobN);
        Endif;

        Reset dsWITMPQST;

        Exec SQL
         select *
          into :dsWITMPQST
          from WITMPQST
          where rrn(WITMPQST) = :scRRNQst;

        scCo# = intCo#;
        scId# = TMque00001;
        scOpr = TMoperator;
        scOpd = TMoperand;
        If scOpr = 'Exists';
           Clear scOpd;
        Endif;
        scStat = TMstat;
        If scStat = *blanks;
           scStat = 'A';
        Endif;

        saveDate = TMdt;
        saveTime = TMtime;

        chQuest = 'QUCO# = ' + %char(intCo#) +
         ' and QUstat <> ''I''' +
         ' and (QUid# not in ' +
         '  (select TMquestionId# from WITMPQST ' +
         '    where TMCO# = ' + %char(intCo#) +
         '    and TMNAME = ''' + %trim(scQstName) + ''') ' +
         '  or QUID# = ' + %char(TMque00001) + ')';

       Endsr;

       //================================================================
       // Save the data elements
       //================================================================
       Begsr SaveData;
        date7 = DateTo7(SystemDate());
        time6 = TimeTo6(SystemTime());

        Select;
        When modeAdd = *on;
        When modeEdit = *on;
        Endsl;

        If scOpd2 <> scOpd;
           scOpd = scOpd2;
        Endif;

        If scOpr = 'Exists';
           Clear scOpd;
        Endif;

        Select;
        When modeAdd = *on;
           Exec sql
            INSERT INTO WITMPQST VALUES(:scCo#, :scQstName, :scId#, :scOpr,
             :scOpd, :scStat,
             :q1Pgm, :q1User, :date7, :time6, :q1Pgm, :q1User, :date7, :time6);
        When modeEdit = *on;
           Exec sql
            UPDATE WITMPQST SET TMCO# = :scCo#, TMNAME = :scQstName,
             TMQUESTIONID# = :scId#, TMOPERATOR = :scOpr, TMOPERAND = :scOpd,
             TMSTAT = :scStat, TMPGM = :q1Pgm,
             TMUSER = :q1User, TMDT = :date7, TMTIME = :time6
              WHERE rrn(WITMPQST) = :scRRNQst;
        Endsl;
       Endsr;

      /End-free
     pTmpQuestAddEdit...
     p                 e
       //===================================================================
       // Load Data.
       //===================================================================
     p LoadData        b
     d LoadData        pi
     d intStrCnt                      3  0

      /free


      /end-free

     p LoadData        e

      //===================================================
      // Maintenance of Roles
      //===================================================
     pRolesAddEdit...
     p                 b
     d                 pi              n

     disDisplay        s              1n   inz('1')
     DisEofSIRLPPRD    s              1n   inz('0')
     disRefresh        s              1n   inz('1')
      /Free
       Exsr DeleteTempAssociations;
       Select;
       When btnAddRole = *On;
          scRoleAED = 'Add';
          scRoleHdg = 'Add Role';
          scRoleRRN = *zero;
          Exsr LoadScreen;
       Other;
          Dou %eof;
             Readc sflRoles;
             If %eof;
                Return *off;
             Endif;
             Select;
             When isEdtRole = *on;
                scRoleAED = 'Edit';
                scRoleHdg = 'Edit Role';
                isEdtRole = *off;
                Exsr LoadScreen;
                Leave;
             When isDltRole = *on;
                scRoleAED = 'Delete';
                scRoleHdg = 'Delete Role';
                isDltRole = *off;
                Exsr LoadScreen;
                Leave;
             When isRmkCode = *on;
                isRmkCode = *off;
                isDisplay = *off;
                Exsr ProcessRemarkCodes;
                Leave;
             Endsl;
          Enddo;
       EndSl;
       Dow isDisplay = *on;
          isDisplay = *off;
          Exsr LoadScreen;
          Exfmt RcdEdtRole;
          Exsr ClearErrors;
          Select;
          When RoleAssociationAddEdit();
             Exsr LoadRoleAssociations;
             isDisplay = *on;
          When btnRoleCx = *on;
             btnRoleCx = *off;
             If scRoleAED <> 'Delete';
                Exec sql
                 select count(*) into :decCount
                  from SIRLPPRD
                  where RLkey = :strJobN;
                If decCount > 0;
                   Select;
                   When PopTwoBtn('Association records have been added for +
                    this role. If you continue with cancel, these will be +
                    lost.':
                    'Role Cancellation':'Continue':'Return':500:300);
                   Other;
                      isDisplay = *on;
                      Iter;
                   Endsl;
                Endif;
             Endif;
             isDisplay = *off;
             Iter;
          When btnRoleSv = *on or btnRoleNxt = *on;
             If RolesError() = *on;
                isDisplay = *on;
                Iter;
             Endif;
             Select;
             When scRoleAED = 'Delete';
                Exsr DeleteData;
             Other;
                Exsr SaveData;
             Endsl;
             Select;
             When btnRoleNxt = *on;
                scRoleRRN = *zero;
                Exsr LoadScreen;
                isDisplay = *on;
             Endsl;
             btnRoleSv = *off;
             btnRoleNxt  = *off;
          Endsl;
       Enddo;
       Exsr DeleteTempAssociations;
       Return *on;
       //--------------------------------
       Begsr ClearErrors;
        Clear erRoleKey;
       Endsr;
       //--------------------------------
       Begsr DeleteData;
        Exec sql
         delete from SIRLP
          where RLco# = :intCo# and RLkey = :scRoleKey;
        Exec sql
         delete from SIRLPPRD
          where RLco# = :intCo# and RLkey = :scRoleKey;
       Endsr;
       //--------------------------------
       Begsr DeleteTempAssociations;
        // Delete association records with key = job#
        Exec sql
         delete from SIRLPPRD
          where RLkey = :strJobN;
       Endsr;
       //--------------------------------
       Begsr LoadRoleAssociations;
        scCo# = intCo#;
        Select;
        When scRoleAED = 'Delete';
           visDltAsc = *off;
        Other;
           visDltAsc = *on;
        Endsl;
        // Clear association grid
        rrnRoleAsc = 0;
        clrRoleAsc = *on;
        dspRoleAsc = *off;
        Write RcdEdtRole;
        // Load association grid with current key including key = job#
        dspRoleAsc = *on;
        clrRoleAsc = *off;
        selRoleAsc = *on;
        isRefresh = *off;
        strSql =
         'select rrn(SIRLPPRD), RLco#, RLkey, RLproduct, RLprodSub, RLauth, ' +
         ' RLstat ' +
         ' from SIRLPPRD ' +
         ' where RLco# = ' + %char(intCo#) + ' and ' +
         ' RLkey in (''' + %trim(scRoleKey) + ''',''' + %trim(strJobN) + ''')' +
         ' order by RLkey, RLproduct, RLprodSub';
        Exec SQL close cursorAsc;
        Exec SQL prepare asc from :strSql;
        Exec SQL declare cursorAsc cursor for asc;
        Exec SQL open cursorAsc;
        Reset isEofSIRLPPRD;
        Dou isEofSIRLPPRD;
           Exec sql fetch next from cursorAsc into :dsSIRLPPRD;
           Select;
           When not IsValidFetch(sqlstt);
              isEofSIRLPPRD = *on;
              Iter;
           Endsl;
           rrnRoleAsc += 1;
           scAscRRN = dsSIRLPPRD.rrn;
           scProd = dsSIRLPPRD.product;
           scProdSub = dsSIRLPPRD.prodSub;
           scAuth = dsSIRLPPRD.auth;
           Write SflRoleAsc;
           selRoleAsc = *off;
        Enddo;
       Endsr;
       //--------------------------------
       Begsr LoadScreen;
        If not isRefresh;
           Leavesr;
        Endif;
        Reset dsSIRLP;
        Exec sql
         select rrn(SIRLP), RLco#, RLkey, RLdesc, RLstat
          into :dsSIRLP
          from SIRLP
          where rrn(SIRLP) = :scRoleRRN;
        scRoleKey = dsSIRLP.key;
        scRoleDesc = dsSIRLP.desc;
        scStat = dsSIRLP.status;
        Exsr LoadRoleAssociations;
        isRefresh = *off;
       Endsr;
       //--------------------------------
       Begsr ProcessRemarkCodes;
        Reset dsSIRLP;
        Exec sql
         select rrn(SIRLP), RLco#, RLkey, RLdesc, RLstat
          into :dsSIRLP
          from SIRLP
          where rrn(SIRLP) = :scRoleRRN;
        scKey2 = dsSIRLP.key;
        scRmkCdSql = 'select RLrmCd, RLbeginW, rrn(SIRLPRMK) ' +
         'from SIRLPRMK where RLco# = ' + %char(intCo#) +
         ' and RLkey = ''' + %trim(dsSIRLP.key) + '''';
        Exfmt RcdRmkCode;
       Endsr;
       //--------------------------------
       Begsr SaveData;
        date7 = DateTo7(SystemDate());
        time6 = TimeTo6(SystemTime());

        Select;
        When scRoleAED = 'Add';
           Exec sql
            insert into SIRLP values(:intCo#, :scRoleKey, :scRoleDesc, :scStat,
             :q1Pgm, :q1User, :date7, :time6, :q1Pgm, :q1User, :date7, :time6);
        When scRoleAED = 'Edit';
           Exec sql
            update SIRLP set RLCO# = :scCo#, RLkey = :scRoleKey,
             RLdesc = :scRoleDesc,
             RLSTAT = :scStat, RLPGM = :q1Pgm,
             RLUSER = :q1User, RLDT = :date7, RLTIME = :time6
              WHERE rrn(SIRLP) = :scRoleRRN;
        Endsl;

        // Replace keys with job# with current key
        Exec sql
         update SIRLPPRD
          set RLkey = :scRoleKey
          where RLco# = :intCo# and RLkey = :strJobN;
       Endsr;
      /End-Free
     pRolesAddEdit...
     p                 e

      //===================================================
      // Maintenance of Role Associations
      //===================================================
     pRoleAssociationAddEdit...
     p                 b
     d                 pi              n

     disDisplay        s              1n
      /Free
       Select;
       When btnAddAsc = *On;
          scAscAED = 'Add';
          scAscRRN = *zero;
          Exsr LoadScreen;
       Other;
          Dou %eof;
             Readc sflRoleAsc;
             If %eof;
                Return *off;
             Endif;
             Select;
             When isEdtAsc = *on;
                scAscAED = 'Edit';
                isEdtAsc = *off;
                Exsr LoadScreen;
                Leave;
             When isDltAsc = *on;
                scAscAED = 'Delete';
                isDltAsc = *off;
                If PopTwoBtn('Please confirm deletion.':
                 'Role Assocation':'Delete':'Cancel':500:300);
                   Exsr DeleteData;
                Endif;
                Return *on;
             Endsl;
          Enddo;
       EndSl;
       Dou isDisplay = *off;
          isDisplay = *off;
          Exfmt RcdRoleAsc;
          Exsr ClearErrors;
          Select;
          When btnAscCx = *on;
             btnAscCx = *off;
             isDisplay = *off;
             Iter;
          When btnAscSave = *on or btnAscNext = *on;
             If RoleAssociationError() = *on;
                isDisplay = *on;
                Iter;
             Endif;
             Exsr SaveData;
             Select;
             When btnAscNext = *on;
                scAscRRN = *zero;
                Exsr LoadScreen;
                isDisplay = *on;
             Endsl;
             btnAscSave = *off;
             btnAscNext  = *off;
          Endsl;
       Enddo;
       Return *on;
       //--------------------------------
       Begsr ClearErrors;
        Clear erProduct;
        Clear erSubProd;
       Endsr;
       //--------------------------------
       Begsr DeleteData;
        Exec sql
          delete from SIRLPPRD
          where rrn(SIRLPPRD) = :scAscRRN;
       Endsr;
       //--------------------------------
       Begsr LoadScreen;
        Reset dsSIRLPPRD;
        dsSIRLPPRD.key = strJobN;
        dsSIRLPPRD.auth = '*All';
        Exec sql
         select rrn(SIRLPPRD), RLco#, RLkey, RLproduct, RLprodSub, RLauth,
          RLstat
          into :dsSIRLPPRD
          from SIRLPPRD
          where rrn(SIRLPPRD) = :scAscRRN;
        scAscKey = dsSIRLPPRD.key;
        scProduct = dsSIRLPPRD.product;
        scSubProd = dsSIRLPPRD.prodSub;
        scAuth = dsSIRLPPRD.auth;
        scStat = dsSIRLPPRD.status;
       Endsr;
       //--------------------------------
       Begsr SaveData;
        date7 = DateTo7(SystemDate());
        time6 = TimeTo6(SystemTime());

        Select;
        When scAscAED = 'Add';
           Exec sql
            insert into SIRLPPRD values(:intCo#, :scAscKey, :scProduct,
             :scSubProd, :scAuth, :scStat,
             :q1Pgm, :q1User, :date7, :time6, :q1Pgm, :q1User, :date7, :time6);
        When scRoleAED = 'Edit';
           Exec sql
            update SIRLPPRD set RLCO# = :scCo#, RLkey = :scAscKey,
             RLproduct = :scProduct, RLprodSub = :scSubProd, RLauth = :scAuth,
             RLSTAT = :scStat, RLPGM = :q1Pgm,
             RLUSER = :q1User, RLDT = :date7, RLTIME = :time6
              WHERE rrn(SIRLPPRD) = :scAscRRN;
        Endsl;
       Endsr;
      /End-Free
     pRoleAssociationAddEdit...
     p                 e

      //===================================================
      // Check for errors in maintenance of Roles
      //===================================================
     pRolesError...
     p                 b
     d                 pi              n

     disError          s              1n   inz

      /Free
       If scRoleAED = 'Delete';
          Return *off;
       Endif;
       // Edit key
       Select;
       When scRoleAED <> 'Add';
       When scRoleKey = *blanks;
          erRoleKey = *on;
          mgRoleKey = 'SYM0503';
          isError = *on;
       Other;
          // Check for duplicate key
          Exec sql
           select count(*) into :decCount
            from SIRLP
            where RLco# = :intCo# and RLkey = :scRoleKey;
          If decCount > 0;
             erRoleKey = *on;
             mgRoleKey = 'SYM0520';
             isError = *on;
          Endif;
       Endsl;
       Return isError;
      /End-free
     pRolesError...
     p                 e

      //===================================================
      // Check for errors in maintenance of Roles Associations
      //===================================================
     pRoleAssociationError...
     p                 b
     d                 pi              n

     disError          s              1n   inz

      /Free
       // Check for duplicates
       Exec sql
        select count(*) into :decCount
         from SIRLPPRD
         where RLco# = :intCo# and RLkey = :scRoleKey
          and RLproduct = :scProduct and RLprodSub = :scSubProd;
       If decCount > 0;
          erProduct = *on;
          mgProduct = 'SYM0520';
          erSubProd = *on;
          mgSubProd = 'SYM0520';
          isError = *on;
       Endif;
       Return isError;
      /End-free
     pRoleAssociationError...
     p                 e

       //===================================================================
       // Save Data.
       //===================================================================
     p SaveData        b
     d SaveData        pi
     d intStrCnt                      3  0

      /free


      /end-free

     p SaveData        e
       //===================================================================
       // Edit Detail Data.
       //===================================================================
     p EditCheck       b
     d EditCheck       pi

      /free
       erEfDt = *off;
       erScore = *off;
       msEfDt = *blanks;
       msScore = *blanks;

       //*** Beg Add *** 01/26/22 **********************************************
       If isWC2820 = *on;
       //*** End Add *** 01/26/22 **********************************************
          erEfDt = ValidDate(scEffDat:message);
          If erEfDt = *on;
             error = *On;
             msEfDt = message;
          Endif;

          If scScore <> *Blanks and scEffDat = 0;
             error = *On;
             erEfDt = *On;
             erScore = *On;
             msEfDt = 'WCR0544';
             msScore = 'WCR0544';
          Endif;

          If scScore = *Blanks and scEffDat <> 0;
             error = *On;
             erEfDt = *On;
             erScore = *On;
             msEfDt = 'WCR0544';
             msScore = 'WCR0544';
          Endif;
       //*** Beg Add *** 01/26/22 **********************************************
       Endif;
       //*** End Add *** 01/26/22 **********************************************

      /end-free

     p EditCheck       e
