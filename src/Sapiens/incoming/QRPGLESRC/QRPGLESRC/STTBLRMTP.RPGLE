     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('STBNDDIR')
     H dftactgrp(*no) actgrp(*CALLER)
      //
     H/TITLE STTBLRMTP - Remark Type Code Maintenance
      ************************************************************************
      *  Revisions:                                                          *
      *                                                                      *
      *  05/23/24 C:333 Support: 115637 (SUNZ)                               *
      *                - Remark Type Maintenance Screen - edit description   *
      *                  - Removed 'TxbRemarkTypeDescEdt' disable property.  *
      *                  - Adding 'TxbRemarkTypeDescEdt' text transform.     *
      *                  - 'scDescEdt' text transform to UpperCase.          *
      *                                                                      *
      *  07/07/22 C:079 Project: 152WBV                                      *
      *                - Add Immediate Lock of Remark field                  *
      *                                                                      *
      *  03/21/22 C:333 Project: SY434                                       *
      *                - Update Grids for Sorting Filtering Upgrade          *
      *
      *  07/02/19 C:079 Support: 152/80680                                   *
      *                 Clear subfile select field if not on row 1           *
      *
      *  01/22/18 C:291 Support: 63854                                       *
      *                 Fix Add button to execute Add screen.                *
      ************************************************************************
      *                                                                     *
     fSTTBLRMTP cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflRmkType:scSflRRN)
     fStActpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
      //  Physical file definition (for maintaining record)
     Fstctp     uf a e             Disk    Rename(pct01:pct0p) InfDS(rtFdsP)
     Fstctl1    if   e           k Disk    InfDS(rtFds)
     Fstsml     if   e           k Disk    Rename(psm01:psm00)
     f*** Beg Add *** 07/07/22 *************************************************
     Fssylf     if   e           k Disk
     f*** End Add *** 07/07/22 *************************************************

     dpsds           esds                  extname(szq1p)

      // Prototypes
     d/copy sPrCompany
     d/copy sPrGenPop
     d/copy sPrUsrPrms
     d/copy sPrDate
     d/copy sPrString

     D* Clear Error Fields
     DClearErrFields   pr

     D* Error Check Edit/Add Screen
     DErrorCheck       pr              n

     d* Main routine
     dMain             pr

     d* Program Start
     dProgramStart     pr

     d* Remark Type Filter Process
     dRmkTypeFilter    pr
     d inMode                        10    const options(*nopass)
     d

     d* Add/Update Remark Type
     dRmkTypeUpdate    pr
     d inRRN                          9  0 const
     d inMode                        10    const

     d ValidRstAdd     pr             1n
     d  inType                        4    const options(*nopass)

     D MultRecipient   pr                  extpgm('SC995R')
     D  inCall                        1    Const
     D  inExit                        1    Const
     D  inCancel                      1    Const
     D  inRmkType                     5

     d RtvCltId        pr                  ExtPgm('RTVCLTID')
     d  cltId                         3

     DdsFileInfo       ds
     D rrn                   397    400b 0

     DrtFdsP           ds                  likeds(dsFileInfo)
     DrtFds            ds                  likeds(dsFileInfo)

     d cltID           s              3
     d inCall          s              1
     d isAddMode       s               n
     d isEdtMode       s               n
     d dspMode         s              1    inz('0')
     d edtMode         s              1    inz('0')

     c     *entry        Plist
     c                   Parm                    inCall

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *inLR = *On;
       Return;

      /end-free

       //===================================================================
       // Clear errors for configuration screen
       //===================================================================
     pClearErrFields   b
     DClearErrFields   pi
      /Free

       msRmCdEdt  = ' ';
       erRmCdEdt  = *Off;
       msDescEdt  = ' ';
       mgDescEdt  = ' ';
       erDescEdt  = *Off;
       erDescEdt2 = *Off;
       msSmCdEdt  = ' ';
       erSmCdEdt  = *Off;
       msDePgEdt  = ' ';
       erDePgEdt  = *Off;
       msTypeEdt  = ' ';
       erTypeEdt  = *Off;
       ms#dChEdt  = ' ';
       er#dChEdt  = *Off;
       msDPrgEdt  = ' ';
       erDPrgEdt  = *Off;
       msDsLvEdt  = ' ';
       erDsLvEdt  = *Off;
       msDlSlEdt  = ' ';
       erDlSlEdt  = *Off;
       msCmSlEdt  = ' ';
       erCmSlEdt  = *Off;
       msCpSlEdt  = ' ';
       erCpSlEdt  = *Off;
       msDMsgEdt  = ' ';
       erDMsgEdt  = *Off;
       mgRdPlEdt  = ' ';
       erRdPlEdt  = *Off;
       mgRdPtEdt  = ' ';
       erRdPtEdt  = *Off;
       mgRd#Edt   = ' ';
       erRd#Edt   = *Off;
       mgCrAdEdt  = ' ';
       erCrAdEdt  = *Off;

      /End-free

     pClearErrFields   e
       //===================================================================
       // ErrorCheck - Error check the Edit/Add Screen
       //===================================================================
     pErrorCheck       b
     DErrorCheck       pi             1n

     d error           s               n   Inz(*Off)

      /Free

       // Remark Code must be entered
       If isAddMode = *On;
          If scRmCdEdt = ' ';
             msRmCdEdt = 'SCM0503';
             erRmCdEdt = *On;
             error     = *On;
          Else;
             // No duplicates allowed
             Chain scRmCdEdt pct01;
             If %Found(stctl1);
                msRmCdEdt = 'SC40002';
                erRmCdEdt = *On;
                error     = *On;
             EndIf;
          EndIf;
       EndIf;

       // Description must be entered
       If scDescEdt = ' ';
          msDescEdt = 'SCM0503';
          erDescEdt = *On;
          error     = *On;
       Else;
          // No Single Quotes Allowed
          If %Scan(x'7D':scDescEdt) <> 0;
             mgDescEdt  = 'Single Quotes are not permitted.';
             erDescEdt2 = *On;
             error      = *On;
          EndIf;
       EndIf;

       // Purge days are not allowed with non-diaries
       If scFl01Edt <> 'Y' And scDprgEdt <> 0;
          msDPrgEdt = 'SC40003';
          erDPrgEdt = *On;
          error     = *On;
       EndIf;

       // Recurring Diary Edits <===============================
       // Recurring Diary Period Type must be entered if Period Length entered
       If (scRdPlEdt <> 0 Or scRdPtEdt <> ' ') And scFl01edt <> 'Y';
          mgRdPlEdt = 'Recurring Diaries can only be set for diary types.';
          erRdPlEdt = *On;
          mgRdPtEdt = 'Recurring Diaries can only be set for diary types.';
          erRdPtEdt = *On;
          error     = *On;
       Endif;

       If scRdPlEdt = 0 And scRdPtEdt <> ' ';
          mgRdPlEdt = 'Period length must be entered if Period type entered.';
          erRdPlEdt = *On;
          error     = *On;
       EndIf;

       // Recurring Diary Period Length must be entered if Period Type entered
       If scRdPlEdt <> 0 And scRdPtEdt = ' ';
          mgRdPtEdt = 'Period type must be entered if Period length entered.';
          erRdPtEdt = *On;
          error     = *On;
       EndIf;

       // # of Recurrences can only be entered if period info entered.
       If scRdPlEdt <> 0 And scRdPtEdt <> ' ' And scRd#Edt = 0;
          mgRd#Edt = '# of Recurrences must be entered if period ' +
                     'information entered.';
          erRd#Edt = *On;
          error     = *On;
       EndIf;

       // # of Recurrences can only be entered if period info entered.
       If scRdPlEdt = 0 And scRdPtEdt = ' ' And scRd#Edt <> 0;
          mgRd#Edt = 'No Period Information entered.';
          erRd#Edt = *On;
          error     = *On;
       EndIf;

       // Create All Diaries can't be entered if no period information entered
       If scRdPlEdt = 0 And scRdPtEdt = ' ' And scCrAdEdt = 'Y';
          mgCrAdEdt = 'No Period Information entered.';
          erCrAdEdt = *On;
          error     = *On;
       EndIf;

       Return error;

      /End-Free

     p ErrorCheck      e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free
       RmkTypeFilter('Set');

       Dou scBtnCncl = *on;

          // Load user subfile grid
          Exsr LoadRmTpGrid;

          // Display grid info screen
          Exsr ExfmtRmkType;
          Select;
             When scRmTpAdd = *On;
                scRmTpAdd = *Off;
                RmkTypeUpdate(0:'Add');
             When scRmTypFlt = *on;
                scRmTypFlt = *off;
                RmkTypeFilter();
             When scRmTypClr = *On;
                scRmTypClr = *Off;
                RmkTypeFilter('Clear');
             When scBtnTab = *on;
                scBtnTab = *off;
             When scBtnRfrsh = *on;
                scBtnRfrsh = *off;
             When scBtnSave = *on;
                scBtnSave = *off;
          Endsl;

          // Check for a edit/delete click
          ReadC sflRmkType;
          DoW Not %Eof or scRmtpAdd = *On;
             Select;
             When scRmTypEdt = *On;
                scRmTypEdt = *Off;
                RmkTypeUpdate(scRRN:'Edit');
             When scRmTypMlt = *On;
                scRmTypMlt = *Off;
                MultRecipient(' ':'0':'0':scRmkType);
             Endsl;
             ReadC sflRmkType;
          EndDo;
       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       Begsr ExfmtRmkType;

         Select;
         When TabIndex = 0;
            RmkTypeFilter('Check');
            TabHeight = 830;
            APnlHeight = TabHeight + 50;
            // Tab height and action panel height are calculated in javascrip (on-load)
            Write RcdSysTbl;
            Write RcdPanel;
            Exfmt RcdRmkType;
            Read RcdSysTbl;
            Read RcdPanel;
         EndSl;

       Endsr;
       //===================================================================
       // Load user grid
       //===================================================================
       Begsr LoadRmTpGrid;

         // Clear the subfile
         scSflDsp = *off;
         scSflClr = *on;
         Write rcdRmkType;
         scSflClr = *off;
         scSflDsp = *on;
         scSflRRN = 0;

         // Populate the subfile
         Setll *loval stctl1;
         Dou %eof;
            Read stctl1;

            Select;
            When %eof;
               Iter;
            When ssRmCd <> *Blanks And Not StringFound(CTrmCd:ssRmCd:'1');
               Iter;
            When ssDesc <> *Blanks And Not StringFound(CTdesc:ssDesc:'1');
               Iter;
            When ssFl01 <> *Blanks And ssFl01 <> CTfl01;
               Iter;
            When ssSmCd <> *Blanks and ssSmCd <> CTsmCd;
               Iter;
            When ssStatus <> *blank and ssStatus <> CTstat;
               Iter;
            Endsl;

            scSflRRN += 1;
            scSflSel = *off;

            If scSflRRN = 1;
               scSflSel = *on;
            Else;
               scSflSel = *off;
            Endif;

            scRmkType = CTrmCd;
            scRmTpDsc = CTdesc;

            Chain CTsmCd psm00;
            If %Found(stsml);
               scSumCode = %Trim(CTsmCd) + '-' + SMdesc;
            Else;
               scSumCode = CTsmCd;
            EndIf;


            scSubArea = CTtype;
            scDatEnt  = CTdePg;
            scDryType = CTfl01;
            scPrgDay  = CTdPrg;

            If CTrdPl <> 0;
               scRecDry = 'Y';
            Else;
               scRecDry = 'N';
            EndIf;

            Select;
            When CTstat = 'A';
               scStat = 'Active';
            Other;
               scStat = 'Inactive';
            Endsl;

            scRRN  = rtFds.rrn;
            Write sflRmkType;
         Enddo;

         scSflRRN = 1;

       Endsr;

      /End-Free

     p Main            e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free

       Select;
       When inCall = ' ';
          edtMode = '1';
       When inCall = 'D';
          dspMode = '1';
       Endsl;

       RtvCltId(cltID);

       // Load Action panel with the Company Name.
       scCompName = GetCompNam(GetUserInfo(q1user));

       scBtnTab = *Off;
       //*** Beg Add *** 07/07/22 **********************************************
       // Determine if immediate lock of remark codes is set and populate
       // the Tool Tip and turn on indicator to control the disabled prop.
       Clear sdr01;
       Chain(n) 'SC' sdr01;
       If DRfl16 = 'Y';
         isImdLock = *on;
         ttImdLock = 'System settings are overriding the availability at this +
          level.  Please contact software support to update system setting.';
       Else;
         isImdLock = *off;
         ttImdLock = *blanks;
       Endif;
       //*** End Add *** 07/07/22 **********************************************

       Return;

      /End-Free

     p ProgramStart    e
       //===================================================================
       // Remark Type Filters
       //===================================================================
     pRmkTypeFilter    b
     dRmkTypeFilter    pi
     d inMode                        10    const options(*nopass)

     disError          s              1n
     dsvRmCd           s                   like(ssRmCd)
     dsvDesc           s                   like(ssDesc)
     dsvFl01           s                   like(ssFl01)
     dsvSmCd           s                   like(ssSmCd)
     dsvStatus         s                   like(ssStatus)

      /Free

       Select;
       When %parms >= 1 and inMode = 'Clear';
          Exsr ClearFilters;
       When %parms >= 1 and inMode = 'Check';
       When %parms >= 1 and inMode = 'Set';
          Exsr SetFilters;
       Other;
          Exsr DefineFilters;
       Endsl;

       Exsr CheckFilters;

       Return;

       //==================================================================
       Begsr CheckFilters;

        isRmTpFltr = *off;
        If ssDesc <> ' ' Or ssFl01 <> ' ' Or ssSmCd <> ' ' Or ssStatus <> ' ' Or
           ssRmCd <> ' ';
           isRmTpFltr = *on;
        Endif;

       Endsr;
       //==================================================================
       Begsr ClearFilters;

        Clear ssRmCd;
        Clear ssDesc;
        Clear ssFl01;
        Clear ssSmCd;
        Clear ssStatus;

       Endsr;
       //==================================================================
       Begsr DefineFilters;

        svRmCd   = ssRmCd;
        svDesc   = ssDesc;
        svFl01   = ssFl01;
        svSmCd   = ssSmCd;
        svStatus = ssStatus;

        Dou not isError;
           isError = *off;
           Exfmt rcdFilter;
           Select;
           When filtCancel = '1';
              Iter;
           Other;
              Exsr EditFilters;
           Endsl;
        Enddo;
        // Reset filters to entry values
        If filtCancel = '1';
           filtCancel = '0';
           ssRmCd   = svRmCd;
           ssDesc   = svDesc;
           ssFl01   = svFl01;
           ssSmCd   = svSmCd;
           ssStatus = svStatus;
        Endif;

       Endsr;
       //==================================================================
       Begsr EditFilters;

        // Place Edits here when needed

       Endsr;
       //==================================================================
       Begsr SetFilters;

        RmkTypeFilter('Clear');
        ssStatus = 'A';

       Endsr;

      /End-free

     pRmkTypeFilter    e

       //===================================================================
       // RmkTypeUpdate - Update Remark Type record
       //===================================================================
     p RmkTypeUpdate   b
     d RmkTypeUpdate   pi
     d  inRRN                         9  0 const
     d  inMode                       10    const

     dconMxRi          c                   const(8)

     disError          s              1n
     dstrPhn#          s             10
     dstrUsr           s             10
     dstrUsrRst        s             10

      /Free
       Clear pct0p;

       Chain inRRN STCTP;

       isAddMode = *off;
       isEdtMode = *off;

       Select;
       When inMode = 'Add';
          isAddMode = *On;
          scAddEdit = 'Add';
          scRmTpHdg = 'Add Remark Type Code';
       When inMode = 'Edit';
          isEdtMode = *On;
          scAddEdit = 'Save';
          scRmTpHdg = 'Edit Remark Type Code';
       Endsl;

       Exsr MoveDBSC;

       ClearErrFields();
       Dou not isError Or scCancel = *On;

          isError = *off;
          Exfmt rcdRmTpCfg;

          // Clear Error Fields
          ClearErrFields();

          If scSubmit = *On;
             isError = ErrorCheck();
          EndIf;
       Enddo;

       If isError = *Off And scSubmit = *On;
          Select;
          When isAddMode;
             Exsr WriteRcds;
          When isEdtMode;
             Exsr UpdateRcds;
          Endsl;
       EndIf;

       Return;
       //======================================================================
       Begsr MoveSCDB;

         CTstat = scStatEdt;
         CTtype = scTypeEdt;
         CTdesc = scDescEdt;
         CTfl01 = scFl01Edt;
         CTfl03 = scFl03Edt;
         CTfl04 = scFl04Edt;
         CTsmCd = scSmCdEdt;
         CTdPrg = scDPrgEdt;
         CTdePg = scDePgEdt;
         CTrmCd = scRmCdEdt;
         CTdsLv = scDSLvEdt;
         CT#dCh = sc#dChEdt;
         CTdlSl = scDlSlEdt;
         CTcmSl = scCmSlEdt;
         CTcpSl = scCpSlEdt;
         CTdMsg = scDMsgEdt;
         CTcrAd = scCrAdEdt;
         CTrdPl = scRdPlEdt;
         CTrdPt = scRdPtEdt;
         CTrd#  = scRd#Edt;
         //*** Beg Add *** 07/07/22 *******************************************
         If isImdLock = *off;
            CTimdLock = scImdLock;
         Endif;
         //*** End Add *** 07/07/22 *******************************************

         CTuser = q1User;
         CTpgm  = q1Pgm;
         CTdt   = DateTo7(SystemDate);
         CTtime = TimeTo6(SystemTime);

       Endsr;

       //======================================================================
       Begsr MoveDBSC;

         scStatEdt = CTstat;
         scTypeEdt = CTtype;
         scDescEdt = CTdesc;

         scFl01Edt = CTfl01;
         scFl03Edt = CTfl03;
         scFl04Edt = CTfl04;

         If scFl01Edt = ' ';
            scFl01Edt = 'N';
         EndIf;

         If scFl03Edt = ' ';
            scFl03Edt = 'N';
         EndIf;

         If scFl04Edt = ' ';
            scFl04Edt = 'N';
         EndIf;

         If scCrAdEdt = ' ';
            scCrAdEdt = 'N';
         EndIf;

         scSmCdEdt = CTsmCd;
         scDPrgEdt = CTdPrg;
         scDePgEdt = CTdePg;
         scRmCdEdt = CTrmCd;
         scDSLvEdt = CTdsLv;
         sc#dChEdt = CT#dCh;
         scDlSlEdt = CTdlSl;
         scCmSlEdt = CTcmSl;
         scCpSlEdt = CTcpSl;
         scDMsgEdt = CTdMsg;
         scCrAdEdt = CTcrAd;
         scRdPlEdt = CTrdPl;
         scRdPtEdt = CTrdPt;
         scRd#Edt  = CTrd#;
         //*** Beg Add *** 07/07/22 *******************************************
         scImdLock = CTimdLock;
         //*** End Add *** 07/07/22 *******************************************

       Endsr;
       //======================================================================
       Begsr UpdateRcds;

         Chain inRRN STCTP;
         Exsr MoveSCDB;
         Update pct0p;

       Endsr;
       //======================================================================
       Begsr WriteRcds;

         Clear pct0p;
         Exsr MoveSCDB;
         CTiUsr = CTuser;
         CTiPgm = CTiPgm;
         CTiDt  = CTdt;
         CTiTim = CTtime;
         Write pct0p;
         Feod stctp;

       Endsr;
      /End-Free

     p RmkTypeUpdate   e
