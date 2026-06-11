     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h/TITLE WTCLMPRV - Additional Payee Information
     h**********************************************************************
     h*
     h*  Description:  This Program Will Display Additional Payee Info.
     h*
     h*  Called from: Profound UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 3/9/2012 by C:189 for Project WC2115
     h**********************************************************************
     h* Revisions:
     h*
     H* 03/25/22 C:333 Project: SY434
     H*                - Update Grids for Sorting Filtering Upgrade
     h*
     H*  06/23/21 C:318 Project WC2302 - Auto tab closure for
     H*                deletion.
     h*
     h* 06/09/20  C:33  Project: SY420
     h*                 Mask SSN
      *                                                                    *
      *  07/16/18 C:62  Project: 133WQO                                    *
      *                 Add auto-diary.                                    *
      *                                                                    *
      *  05/30/18 C:79  Project: WC2515                                    *
      *                 Add EDI Release 3.1 changes                        *
      *                                                                    *
      *  05/17/18 C:33  Support: 70651                                     *
      *                 Clear save fields.                                 *
      *                                                                    *
      *  01/23/18 C:062 Project 129WUV                                     *
      *                 Add inquiry authority override processing          *
      *                                                                    *
      *  11/23/16 C:180 Support 42455                                      *
      *                 Add EFT tab to Payee info page                     *
      *                                                                    *
      *  03/23/16 C:308 Project: ELEVATE                                   *
      *                 Recompile only                                     *
      *                                                                    *
      * 01/07/16 C:65  Project: WC2401                                     *
      *                Change the coding done under WC2254 to call an      *
    *                API instead of directly calling the various         *
      *                programs for windows here.                          *
      *                                                                    *
      *  09/17/15 C:302 Project: wc2254                                    *
      *                 Recompile only                                     *
     h*
     h*  07/02/15 C:306 Support: SEQ86962 / 31805                    *
     h*                 Added the email validation check on Addit-   *
     h*                 ional payee screen                           *
     h*
     h*  04/16/15 C:65  Project: SY251
     h*                 Add API call to launch new Department letters.
     h*
     h* 03/19/15  C:279 Support: 32825 (Phaseware)
     h*                 Restructured program logic to not exit on enter.
     h*
     h* 07/28/14  C:295 Project: 114WMD
     h*                 Compilation only as the changes are in Action Panel
     h*
     h* 09/03/13  C:189 Redmine: 3015
     h*                 Use include instead of ignore for action panel record
     h*                 format
     h*
     h**********************************************************************
     fwtClmPrv  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflAddPaye:rrnAddPaye)
     f                                     Sfile(SflEftInfo:rrnEftInfo)
     f                                     InfDs(fids01)
     fwtActPnl  cf   e             WorkStn
     f                                     Handler('PROFOUNDUI(HANDLER)')
     f                                     Include(rcdClmPnl)
     fwmcmp     uf   e             Disk
     f                                     ReName(pcm01:dbrcdf)
     f                                     InfDs(dbfds)
     fwmeml     if   e           k Disk    ReName(pem01:pem00)
     f                                     InfDs(dbfds2)
     fwdacl     if   e           k Disk    ReName(pac01:pac00)
     f                                     infds(dbcalfds)
     fwdacp     uf a e             Disk    ReName(pac01:pac0p)
     Fwmcxl     if   e           k disk    ReName(pcx01:pcx00)
     Fwdahp     o    e             disk    ReName(pah01:pah0p)
     Fwarplc    uf   e           k disk    ReName(prp01:prp0c)
     Fwmefl3    if   e           k disk
     F                                     Rename(pef01:pef03)
     F                                     infds(effds0)
     Fwtgtl     if   e           k Disk    ReName(pgt01:pgt00)
     d* Proto-types

     d* New payee procedure
     d AddNewPayee     pr

     d* Add a record in the history
     d AddRec          pr

     d* Audit Dart Information
     d AuditDart       pr

     d* Edit Payee Information
     d EditAddPayee    pr

     d* Initialize indicators
     d InzInd          pr

     d* Load Payee Information
     d LoadAddPayee    pr

     d* Load Existing Payee Information
     d LodchgPayee     pr

     d* Load New Payee Information
     d LodNewPayee     pr

     d* Retrieve Changed record
     d RtvChgRec       pr

     D UpdEft          pr                  ExtPgm('WA085R')
     D ##type                         1
     D ##rrn                          9  0
     D ##rtn                          1
      *

     d* Build tab header
     d BldTabHdr       pr
      *
     d* Process EFT Information
     d LoadEftInfo     pr

     d* Display main screen
     d DisplayMain     pr

     d* Clear Subfiles
     d ClearPayInfo    pr
     d ClearEftSub     pr

     d* General Prototypes
     d/copy sPrAtrium
     d/copy sCopyDsPhx
      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     d/copy sPrString
     d/copy wPrGroup
     d/copy wPrUsrPrms
     d/copy sPrDate
     d/copy wPrClaims
     d/copy wPrAgent
     d/copy wPrContact
     d/copy wPrLoc
     d/copy wPrEmplr
     d/copy sPrGenErr
     d/copy sPrLetter
     d/copy sPrCompany

     d/copy sPrtable
     d/copy wPrEft

     d* Load Action Panel procedure Prototype and variables
     d/Copy sCopyClmPr


     D* System Data Structures
     D dbfds           ds
     D  flstcm           *status
     D  dbrrn                397    400b 0

     D dbfds2          ds
     D  dbrrn2               397    400b 0
     D*
     D*WMEFL RRN
     D
     D effds0          ds
     D  efrrn                397    400b 0

     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)

     D*Variables
     D @@RRN           s              9
     D @@Mode          s              1
     D @@Next          s             10
     D outClientID     s              3    inz('000')
     D addMode         s               n   inz(*off)
     D chgMode         s               n   inz(*off)
     D dspMode         s               n   inz(*off)
     D isError         s               n   inz(*off)
     D edit            s               n   inz(*off)
     d isExit          s               n   inz(*off)
     D inRRNClm        s              9  0
     D inMode          s              1
     D svPnam          s                   like(ACpnam)
     D svPfNam         s                   like(ACfNam)
     D svAdr1          s                   like(ACadr1)
     D svAdr2          s                   like(ACadr2)
     D svCty           s                   like(ACcty)
     D svSt            s                   like(ACst)
     D svZip           s                   like(ACzip)
     D svCtry          s                   like(ACctry)
     D intSeq#         s                   like(ACseq#)
     D svTime          s                   like(ACtime)
     D svDate          s                   like(ACdt)
     D allowAddrChg    s               n
     D overrideInq     s               n
     D ##afl           s              1
     D ##ahbaf         s              1
     D ##rtn           s              1
     D ##rtn#          s              1  0
     D ##TbId          s                   like(GTtbid)

     D ##type          s              1
     D ##rrn           s              9  0
     D ##Key           s             10
     D
     D firstTime       s               n   inz(*off)
      *
     D intTab          S              5  0
     D svShwEftLnk     S              1    INZ
     D ##eftMod        S              1
     D ##eftTyp        S              1
     D ##eftRrn        S              9  0
     D ##cmrrn         S              9  0
     D I_EftRefresh    S               N   INZ(*off)
     D*WDACL rrn
     D
     D dbcalfds        ds
     D WDACLrrn              397    400b 0

     C     *entry        Plist
     C                   Parm                    @@RRN
     C                   Parm                    @@Mode
     C                   Parm                    @@Next
      *

     C     keyWMEML      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv


     C     keyWDACL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     keyWMCXL      Klist
     C                   Kfld                    CMco#
     C                   kfld                    CMfnd

     C     keyWARPLC     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   kfld                    CMcase

     C     keyWDACL#     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    intSeq#

     C     keyWTGTL      klist
     C                   Kfld                    CMco#
     C                   Kfld                    ##TbId
     C                   Kfld                    ##KEY

      /Free

       ExSr  ProgramStart ;

       // Load the action panel for both tabs

       //Load data in all the tabs
       exsr TabAddPayeeinfo;
       exsr TabEFTinfo;
       LoadActPan();

       Dow isExit = *off;
          DisplayMain();


          Select ;
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'CS':txtWinData);
             btnShowWin = *off;

          When TabClick = *On;
            TabClick = *Off;

          // Refresh
          When refrshData = *On;
               BldTabHdr();
               exsr TabAddPayeeinfo;
               exsr TabEFTinfo;
             refrshData = *Off;

          // Menu Choices
          When scMenuRsp <> *blanks;
             @@next = scMenuRsp;
             Leave;

          When AddPayee = *on;
             AddNewPayee();
          other;
             RtvChgRec();
          Endsl;
       Enddo;

       ExSr  ProgramEnd;

       //===================================================================
       // ProgramStart - Start Processing
       //===================================================================
       Begsr ProgramStart;


       //Load default values or passed in values
       If %parms()>0;
          inRRNClm = %int(@@RRN);
          inMode = @@Mode;
       Else;
          inRRNClm = 41;
          inMode = 'C';
       Endif;

       // Access the claim record
       Chain(n) inRRNClm dbrcdf;

       // Get the client id
       GetClientID(outClientID);

       // Populate hidden fields
       scActCltId=outClientID;
       scActRrn=inRrnClm;

       // Set the Mode
       shwPayeAdd=*On;
       Select;
       When inMode = 'D';
          shwPayeAdd=*Off;
          dspMode = *on;
       When inMode = 'A';
          addMode = *on;
       When inMode = 'C';
          chgMode = *on;
       Endsl;
          scActMode = inMode;

       // Set the Action Panel Height
       aPnlHeight = 1080;

       // Check to see if add/change payee is allowed
       allowAddrChg = (getChgAddrFlag(q1User)='Y');
       overrideInq  = (GetInqAuthOvr(CMco#)='Y');
       Select;
       When dspMode And allowAddrChg And overrideInq;
          shwPayeAdd = *On;
          disNamAddr = *Off;
          dspMode    = *Off;
          chgMode    = *On;
          inMode     = 'C';
       When chgMode And allowAddrChg;
          shwPayeAdd = *On;
          disNamAddr = *Off;
       Other;
          shwPayeAdd = *Off;
          disNamAddr = *On;
       EndSl;

       // Set the initial tab
       TabIndex  = 0;
       BldTabHdr();

       endsr;
       //===================================================================
       // TabAddPayeeinfo - Processing for Addtional Payee Tab
       //===================================================================
       Begsr TabAddPayeeinfo;

       // Load the subfile grid for Addtional payee Tab
        LoadAddPayee();

       endsr;
       //===================================================================
       // TabEFTinfo - Processing for EFT information Tab
       //===================================================================
       Begsr TabEFTinfo;

       // Load the subfile grid for EFT information tab
        LoadEftInfo();

       endsr;
       //===================================================================
       // ProgramEnd - Start Processing
       //===================================================================
       Begsr ProgramEnd;
          *inlr = *on;
       Return;

       endsr;
      /end-Free
        //===================================================================
        // AddNewPayee - Add a new Payee processing
        //===================================================================
     P AddNewPayee     B
     D AddNewPayee     PI
      *
      /Free

       LodNewPayee();
       LoadAddPayee();
       Write rcdClmPnl;
       Write rcdPanel;
       Exfmt rcdAddPaye;
       AddPayee = *off;

      /End-Free
     P AddNewPayee     E
        //===================================================================
        // ADDREC - Store the Image in WDAHP  (Existing)
        //===================================================================
     P AddRec          B
     D AddRec          PI
      *
     C                   CLEAR                   PAH0P
     C                   MOVE      ACCO#         AHCO#
     C                   MOVE      ACFND         AHFND
     C                   MOVE      ACFYR         AHFYR
     C                   MOVE      ACCASE        AHCASE
     C                   MOVE      ##AHBAF       AHBAFL
     C                   MOVEL     ACPNAM        AHLSN
     C                   MOVE      ACPNAM        @AHFSN           10
     C                   MOVEL     @AHFSN        AHFSN
     C                   MOVE      ACADR1        AHADR1
     C                   MOVE      ACADR2        AHADR2
     C                   MOVE      ACCTY         AHCTY
     C                   MOVE      ACST          AHST
     C                   MOVE      ACZIP         AHZIP
     C                   MOVE      ACCTRY        AHCTRY
     C                   MOVE      ACADR3        AHADR3
     C                   MOVE      ACCKAL        AHCKAL
     C                   MOVE      Q1PGM         AHPGM
     C                   MOVE      Q1USER        AHUSER
     C                   EVAL      AHTIME =  Timeto6(SystemTime())
     C                   EVAL      AHDT   =   Dateto7(SystemDate())
     C                   WRITE     PAH0P
     C*
     P AddRec          E
     C*//===================================================================
     C*// AuditDart - WRITE DART IF CLAIMANT INFO CHANGES
     C*//===================================================================
     P AuditDart       B
     D AuditDart       PI
      *
     C                   MOVE      ' '           ##AFL
      *
     C     scDtlNam      IFNE      SVPNAM
     C     SVPNAM        ANDNE     *BLANK
     C     scDtlFnam     Orne      SVPfNam
     C     SVpFnam       Andne     *blank
     C     scDtlAdr1     ORNE      SVADR1
     C     SVADR1        ANDNE     *BLANK
     C     scDtlAdr2     ORNE      SVADR2
     C     SVADR2        ANDNE     *BLANK
     C     scDtlCty      ORNE      SVCTY
     C     SVCTY         ANDNE     *BLANK
     C     scDtlState    ORNE      SVST
     C     SVST          ANDNE     *BLANK
     C     scDtlZip      ORNE      SVZIP
     C     SVZIP         ANDNE     *ZERO
     C     scDtlNam      ORNE      SVPNAM
     C     SVPNAM        ANDEQ     *BLANK
     C     CXaadr        ANDEQ     'Y'
     C     scDtlFnam     Orne      SVPfNam
     C     SVpFnam       Andne     *blank
     C     CXaadr        ANDEQ     'Y'
     C     scDtlAdr1     ORNE      SVADR1
     C     SVADR1        ANDEQ     *BLANK
     C     CXaadr        ANDEQ     'Y'
     C     scDtlAdr2     ORNE      SVADR2
     C     SVADR2        ANDEQ     *BLANK
     C     CXaadr        ANDEQ     'Y'
     C     scDtlCty      ORNE      SVCTY
     C     SVCTY         ANDEQ     *BLANK
     C     CXaadr        ANDEQ     'Y'
     C     scDtlState    ORNE      SVST
     C     SVST          ANDEQ     *BLANK
     C     CXaadr        ANDEQ     'Y'
     C     scDtlZip      ORNE      SVZIP
     C     SVZIP         ANDEQ     *ZERO
     C     CXaadr        ANDEQ     'Y'
     C*
     C                   MOVE      'Y'           ##AFL
     C     scDtlAdr1     IFEQ      SVADR1
     C     scDtlAdr2     ANDEQ     SVADR2
     C     scDtlCty      ANDEQ     SVCTY
     C     scDtlState    ANDEQ     SVST
     C     scDtlZip      ANDEQ     SVZIP
     C*
     C                   MOVE      *BLANKS       XXCHAD          116
     C                   ELSE
     C                   MOVE      *BLANKS       XXCHAD          116
     C                   MOVE      SVZIP         XXZIP             5
     C                   CAT(P)    SVADR1:1      XXCHAD
     C                   CAT(P)    SVADR2:1      XXCHAD
     C                   CAT(P)    SVCTY:1       XXCHAD
     C                   CAT(P)    SVST:1        XXCHAD
     C                   CAT(P)    XXZIP:1       XXCHAD
     C                   ENDIF
     C                   Eval      ##chg = ' '
     C     scDtlNam      IfNE      SVPNAM
     C     SVPNAM        ANDEQ     *BLANK
     C     scDtlFnam     Orne      SVPfNam
     C     SVpFnam       Andne     *blank
     C     scDtlAdr1     ORNE      SVADR1
     C     SVADR1        ANDEQ     *BLANK
     C     scDtlAdr2     ORNE      SVADR2
     C     SVADR2        ANDEQ     *BLANK
     C     scDtlCty      ORNE      SVCTY
     C     SVCTY         ANDEQ     *BLANK
     C     scDtlState    ORNE      SVST
     C     SVST          ANDEQ     *BLANK
     C     scDtlZip      ORNE      SVZIP
     C     SVZIP         ANDEQ     *ZERO
     C                   Eval      ##chg = 'A'
     C                   EndIf
     C*
     C*This will be uncommented when its screen will get ready
     C
     C                   CALL      'WA074R'
     C                   PARM      CMCO#         ##CO#             3 0
     C                   PARM      CMFND         ##FND             3 0
     C                   PARM      CMFYR         ##FYR             3 0
     C                   PARM      CMCASE        ##CASE            7 0
     C                   PARM      0             ##FEIN            9 0
     C                   PARM      0             ##MOD             3 0
     C                   PARM      CXAADP        ##RCOD            5
     C                   PARM      *BLANKS       ##RTN
     C                   PARM      XXCHAD        ##XHAD          116
     C                   PARM                    ##CHG             1
     C                   ENDIF
     C

     P AuditDart       E
       //===================================================================
       // EditAddPayee - Validate the information entered by user
       //===================================================================
     P EditAddPayee    B
     D EditAddPayee    PI
     D ##type1         s              1
      /Free
       InzInd();

       // Name can not be blank

       If scDtlNam =  *blanks;
          msDtlNam = 'WCR0503';
          isError = '1';
          erDtlNam  = '1' ;
       Endif;

       // Address can not be blank

       If scDtlAdr1 =  *blanks;
          msDtlAdr1 = 'WCR0503';
          isError = '1';
          erDtlAdr1 = '1' ;
       Endif;

       //either  C,S,Z should be entered of out of country address

       If  scDtlcty =  *blanks and scDtlState = *blanks and scDtlZip  = 0
       and scDtlCnt =  *blanks  and scDtlProv = *blanks ;
          msDtlProv = 'WC40034';
          isError = '1';
          erDtlProv =  '1' ;
       Endif;

       If  scDtlCnt <>  *blanks and scDtlCnt <>  'USA';
          If  scDtlProv  = *blanks;
             msDtlprov = 'WC40034';
             isError = '1';
             erDtlProv = '1' ;
          Endif;
       Endif;

       If  scDtlDDob  <> 0;
          If  DtCheck(scDtlDDob) = 0 ;
             msDtlDDob = 'WCR0500';
             isError = '1';
             erDtlDDob = '1' ;
          Endif;
       Endif;

       If  scDtlCnt <>  'USA' and scDtlCnt <>  *blanks;
          If scDtlCty <> *blanks  or scDtlState <> *blanks or scDtlZip  <> 0;
             msdtlprov = 'WC40034';
             isError = '1';
             erDtlProv = '1' ;
          Endif;
       Else;
          If  scDtlProv <> *blanks;
             msdtlprov = 'WC40034';
             erDtlProv =  '1' ;
             isError = '1';
          Endif;
       Endif;

       If  scDtlCnt = *blanks or  scDtlCnt = 'USA';
          If  scDtlCty  = *blanks;
             msDtlCty = 'WCR0503';
             erDtlCty =  '1' ;
             isError = '1';
          Endif;

          If  scDtlState = *blanks;
             msDtlState = 'WCR0503';
             erDtlState =  '1' ;
             isError = '1';
          Endif;

          If  scDtlZip   = 0;
             msDtlZip = 'WCR0503';
             erDtlZip  =  '1' ;
             isError = '1';
          Endif;
       Endif;

       If  scDtlProv  = *blanks;
          If  scDtlCnt <>  *blanks and scDtlCnt <>  'USA' ;
             msDtlProv = 'WCR0503';
             erDtlProv =  '1' ;
             isError = '1';
          Endif;
       Endif;

       If  scDtlSts <>'A' and  scDtlSts <>'I' ;
          msDtlsts = 'WCR0503';
          erDtlsts =  '1' ;
          isError =  '1' ;
       Endif;

       If svDate  <> 0;
       Chain(n)  scAddpRRN pac0p;
          If (svDate <> ACdt Or  svTime <> ACtime) and ACuser <> Q1user;
             errmsgSv  = 'WC30000' ;
             errSav  =  '1' ;
             isError =  '1' ;
          Endif;
       Endif;

       If  isError = '0';
          Chain keyWMCXL pcx00;
             If CXaadp <> *blanks;
                AuditDart();
                If ##AFL = 'Y' and ##RTN <> 'Y' ;
                   isError = '1' ;
                Endif;
             Endif;
       Endif;

       ##type1 = 'P';
       erDtlPhn# = Check_Phone#(scDtlPhn#:##type1:msDtlPhn#);
       If erDtlPhn# = *on;
          isError = '1';
       Endif;
        // Validate the Payee Email id
       erPayEmad = *Off;
       If %Len(%trim(scPayEmAd)) > 0;
          erPayEmad = Check_Email(scPayEmAd:mspayemad);
          If erPayEmad = *on;
            isError = '1';
          Endif;
       Endif;

      /End-Free
     P EditAddPayee    E
       //===================================================================
       // InzInd  - Intialize the indicators
       //===================================================================
     P InzInd          B
     D InzInd          PI
      /Free

       isError = '0';
       erDtlProv = '0';
       erDtlCty = '0';
       erDtlstate = '0';
       erDtlzip = '0';
       erDtlDDob  = '0';
       erDtlNam = '0';
       erDtladr1 = '0';
       erDtlSts  = '0';
       errSav  = '0';

      /End-Free
     P InzInd          E
       //===================================================================
       // LoadAddPayee - Load addtional Payee information on screen
       //===================================================================
     P LoadAddPayee    B
     D LoadAddPayee    PI
      /Free

       rrnAddPaye = 0;
       ClrAddPaye = *on;
       dspAddPaye = *off;
       Write rcdAddPaye;
       dspAddPaye = *on;
       ClrAddPaye = *off;

       //
       //Retrive the information to be shown on grid

       Setll KeyWDACL  pac00;
       Reade KeyWDACL  pac00;
       Dow Not %Eof(Wdacl);

          If ACfNam <> ' ';
             scAddPNam =  %Trim(ACfNam) + ' ' + %Trim(ACpNam);
          Else;
             scAddPNam =  %Trim(ACpNam);
          Endif;
          scAddPadd =  ACadr1;
          scAddPCty =  ACcty;

          If  ACckal = 'Y';
             scAddPChk = 'Yes';
          Else;
             scAddPChk = 'No';
          Endif;

          scPayEmAdG=  ACemdC;
          scPayEmAdT=  ACemdC;
          If scPayEmAdT=*Blanks;
             scPayEmAdT=noEmail;
          EndIf;
          If ACstat = 'A';
             scAddpSts =  'Active';
          Else;
             scAddpSts =  'Inactive';
          Endif;
          scAddpRRN =  WDACLrrn;

          shwPayeLnk=*On;
          If dspMode=*On;
             shwPayeLnk=*Off;
          EndIf;

          rrnAddPaye  = rrnAddPaye + 1 ;
          selAddPaye = *Off;
          If rrnAddPaye = 1;
             selAddPaye = *on;
          Endif;
          Write sflAddPaye;
          Reade KeyWDACL  pac00;

       Enddo;


      /End-Free
     P LoadAddPayee    E
       //===================================================================
       // LodChgPayee  - Load the window to update/Add
       //===================================================================
     P LodChgPayee     B
     D LodChgPayee     PI
      /Free

      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = inRRNClm;
      //**** End Add  ***  06/23/21  ****************************************
       Chain(n) scAddpRRN pac0p;
       If %found(wdacp);
          MoveDbsc();
          Exfmt rcdupdpaye;

          If savePayee = *on;
             EditAddPayee();
             Dow isError = *on ;
                Exfmt rcdupdpaye;
                If canPayee = *on;
                   InzInd();
                   leave;
                Endif;
                EditAddPayee();
             Enddo;

             If isError = *off and savePayee = *on;
                Chain  scAddpRRN pac0p;
      /end-Free

     C*      Make change to Request Payments also
     C     keyWARPLC     CHAIN     PRP0C                              99
     C     *IN99         DOWEQ     *OFF
     C     ACPNAM        IFEQ      RPPNAM
     C     ACADR1        ANDEQ     RPADR1
     C     ACADR2        ANDEQ     RPADR2
     C     ACCTY         ANDEQ     RPCTY
     C     ACST          ANDEQ     RPST
     C     ACZIP         ANDEQ     RPZIP
     C     ACPNAM        OREQ      RPPNAM
     C     ACADR1        ANDEQ     RPADR1
     C     ACADR2        ANDEQ     RPADR2
     C     ACCTRY        ANDEQ     RPCTRY
     C     ACADR3        ANDEQ     RPADR3
     C*
     C                   MOVEL     scDtlNam      RPPNAM
     C                   MOVEL     scDtlAdr1     RPADR1
     C                   MOVEL     scDtlAdr2     RPADR2
     C                   MOVEL     scDtlCty      RPCTY
     C                   MOVEL     scDtlState    RPST
     C                   MOVEL     scDtlZip      RPZIP
     C                   MOVEL(P)  scDtlCnt      RPCTRY
     C                   MOVEL(P)  scDtlProv     RPADR3
     C*
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   EVAL      RPtime =  Timeto6(SystemTime())
     C                   EVAL      RPdt   =   Dateto7(SystemDate())
     C*
     C                   UPDATE    PRP0C
     C                   ENDIF                                                  . ACNAM=RP
     C     keyWARPLC     READE     PRP0C                                  99
     C                   ENDDO                                                  . 99 DOW
     C*
     C                   MOVE      'B'           ##ahbaf
      /Free
       // indentation continued

                AddRec();
                MoveScdb();
                ACuser =  q1user    ;
                ACpgm  =  q1pgm     ;
                ACtime = Timeto6(SystemTime());
                ACdt   = Dateto7(SystemDate());
                update pac0p ;

                ##ahbaf  = 'A';
                AddRec();
                ##type = 'D';
                ##rrn  =  dbrrn;
                ##rtn  =  ' ' ;
                If  ACpnam <> svpnam or ACfNam <> svPfNam;
                   UpdEft(##type:##rrn:##rtn);
                Endif;
             Endif;
          else;
          Endif;
       Endif;

      /End-Free
     P LodChgPayee     E
       //===================================================================
       // LodNewPayee  - Load the window to update/Add
       //===================================================================
     P LodNewPayee     B
     D LodNewPayee     PI
      /Free
       Clear rcdupdpaye;
      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = inRRNClm;
      //**** End Add  ***  06/23/21  ****************************************
       Clear svTime;
       Clear svDate;
       Clear svDate;
       Clear svTime;
       Clear svPnam;
       Clear svPfNam;
       Clear svAdr1;
       Clear svAdr2;
       Clear svCty ;
       Clear svSt  ;
       Clear svZip ;
       Clear svCtry;
       scDtlSts = 'A';
       Exfmt rcdupdpaye;

       If savePayee = *on;
          EditAddPayee();

          Dow isError = *on ;
             Exfmt rcdupdpaye;
             If canPayee = *on;
                InzInd();
                leave;
             Endif;
             EditAddPayee();
          Enddo;

          If isError = *off and savePayee = *on;
             //ASSIGN NEXT SEQUENCE NUMBER

             intSeq# = *hival ;

             Setgt  keyWDACL# pac00 ;
             Readpe keyWDACL  pac00 ;
             ACseq# = ACseq# + 1 ;

             MoveScDb();

             ACuser =  q1user    ;
             ACiusr =  q1user    ;
             ACpgm  =  q1pgm     ;
             ACipgm =  q1pgm     ;

             ACtime = Timeto6(SystemTime());
             ACdt   = Dateto7(SystemDate());
             ACidt  = Dateto7(SystemDate());


             Write pac0p ;

          // Add a record to history file

             ##ahbaf = 'B';
             AddRec();
             Feod   WDACP;
          Endif;
       Else;
       Endif;
      /End-Free
     P LodNewPayee     E
       //===================================================================
       // MoveDbSc - Move Database fields to Screen fields for Add Payee
       //===================================================================
     P MoveDbSc        B
     D MoveDbSc        PI
      /Free

       scDtlNam  =  ACpnam;
       scDtlFnam = ACfNam;
       scDtlSex = ACdSex;
       scDtlAdr1 =  ACadr1;
       scDtlAdr2 =  ACadr2;
       scDtlCty  =  ACcty;
       scDtlState = Acst;
       scDtlZip  = ACzip;
       scDtlCnt  = ACctry;
       scDtlProv = ACadr3;
       scPayEmAd = ACemdC;

       If  ACckal = 'Y';
          scDtlAlChk  =  'Y';
       Else;
          scDtlAlChk  =  'N';
       Endif;

       scDtlSts  = ACstat ;
       scdtlDepRl= ACrel;
       scDtlDBO  = ACord;
       scDtlPhn# = ACcrac * 10000000 + ACcrph;
       //***  Begin Chg  ***  06/09/20  **********************
       //Monitor;
       //   scDtlDSS# = %Dec(ACdss#:9:0);
       //On-Error 105;
       //   scDtlDSS# = 0;
       //EndMon;
       scDtlDSS# = MaskData('SSN':'ACdss#':ACdss#);
       //***  End   Chg  ***  06/09/20  **********************
       scDtlDDob = dtMDYY(ACddob);
       scDtlDep  = ACdpcy;
       svDate    = ACdt     ;
       svTime    = ACtime   ;
       svPnam    = scDtlNam ;
       svPfNam   = scDtlFnam;
       svAdr1    = scDtlAdr1;
       svAdr2    = scDtlAdr2;
       svCty     = scDtlCty ;
       svSt      = scDtlState ;
       svZip     = scDtlZip  ;
       svCtry    = scDtlCnt  ;
      /End-Free
     P MoveDbSc        E
       //===================================================================
       // MoveScDb - Move Screen fields to Database fields
       //===================================================================
     P MoveScDb        B
     D MoveScDb        PI
      /Free

       //Move fields from screen to data base (mvscdb)

       ACco#  = CMco# ;
       ACfnd  = CMfnd ;
       ACfyr  = CMfyr ;
       ACcase = CMcase;
       ACpnam =  scDtlNam ;
       ACfNam = scDtlFnam;
       ACdSex = scDtlSex;
       ACadr1 =  scDtlAdr1;
       ACadr2 =  scDtlAdr2;
       ACcty  =  scDtlCty ;
       Acst   =  scDtlState;
       ACzip  =  scDtlZip  ;
       ACctry =  scDtlCnt  ;
       ACadr3 =  scDtlProv ;
       ACemdC =  scPayEmad ;
       ACckal =  scDtlAlChk;

       ACstat =  scDtlSts;
       If  scDtlSts  = *blanks;
          ACstat =  'A' ;
       Endif;
       ACrel  =  scdtlDepRl;
       ACord  =  scDtlDBO  ;
       //***  Begin Chg  ***  06/09/20  **********************
       //If scDtlDSS#=0;
       //  ACdss#=*Blanks;
       //Else;
       //   ACdss# =  %EditC(scDtlDSS#:'X');
       //EndIf;
       ACdss# = UnMaskData('SSN':'ACdss#':scDtlDSS#);
       //***  End   Chg  ***  06/09/20  **********************
       ACddob =  dtCYMD(scDtlDDob);
       ACdpcy =  scDtlDep  ;

       ACcrac = %Int(scDtlPhn#/10000000);
       ACcrph = scDtlPhn# - (ACcrac * 10000000);

      /End-Free
     P MoveScDb        E
       //===================================================================
       // RtvChgRec- Processing for changing a record in the grid
       //===================================================================
     P RtvChgRec       B
     D RtvChgRec       PI
      /Free
       Edit = *Off;
       Readc sflAddPaye;
       DoW Not %Eof;
          If editPayee  = *on;
             LodchgPayee();
             Edit = *On;
             editPayee = *off;
          Endif;
          Readc sflAddPaye;
       EndDo;

       // read subfile change record
       If Edit;
          LoadAddPayee();
       Endif;

       I_EftRefresh = *Off;

       Readc SflEftInfo;
       Dow not %eof;

          Select;

          When scEdtEftLc = *On;
             scEdtEftLc =*Off;
             ##eftMod = 'E';
             ##eftTyp = scEftType;
             ##cmrrn  = 0;
             ##eftRrn = scLocRrn;
             eftAdd(##eftMod:##eftTyp:CMfnd:##eftRrn:##cmrrn);
             I_EftRefresh = *On;

          When scEftBnkLc = *On;
             scEftBnkLc =*Off;
             ##eftRrn = scLocRrn;
             EFTBANKINFO(##eftRrn);
             I_EftRefresh = *On;

          When scRqPNtLc = *On;
             scRqPNtLc =*Off;
             ##eftRrn = scLocRrn;
             If DateTo8(scLocPNtDt) = 0;
                ReqPreNote(##eftRrn);
             Else;
                ClrPreNote(##eftRrn);
             Endif;
             I_EftRefresh = *On;

          EndSl;
          Readc SflEftInfo;
       Enddo;

       If I_EftRefresh = *On;
          LoadEftInfo();
       Endif;
      /End-Free

     P RtvChgRec       E
       //
       //===================================================================
       // BldTabHdr - Build tab header
       //===================================================================
     P BldTabHdr       B
     D BldTabHdr       PI
      /Free

       clear intTab;
       clear tabHeader;
       tabHeader = 'Additional Payee Details';
       intTab = 0;

       If EFTinstalled;
          tabHeader = %trim(tabHeader) + ',' + 'EFT Information';
          intTab = intTab + 1;
          svshwEftLnk = *on;
       Else;
          svshwEftLnk = *off;
       Endif;

      /End-Free
     P BldTabHdr       E

       //===================================================================
       // DisplayMain - Display main screen
       //===================================================================
     P DisplayMain     B
     D DisplayMain     PI
      /Free
       TabHeight = 860;
       aPnlHeight = TabHeight + 70;
       ShowAsWdw = *off;
       Select;
       When TabIndex = 0;
         // Additional Payee details
         Write rcdClmPnl;
         Write rcdPanel;
         Exfmt rcdAddPaye;

       When TabIndex = 1;
         // Eft Information
         Write rcdClmPnl;
         Write rcdPanel;
         Exfmt rcdEftInfo;
       EndSl;
       Read rcdClmPnl;
       Read rcdPanel;
      /End-Free
     P DisplayMain     E
       //===================================================================
       // LoadEftInfo - Load Subfile for EFT information
       //===================================================================
     P LoadEftInfo     B
     D LoadEftInfo     PI
      *
      /free
       ClearEftSub();

       Setll (CMco#:CMfnd:CMfyr:CMcase) pef03;
       Reade (CMco#:CMfnd:CMfyr:CMcase) pef03;
       Dow not %eof(WMEFL3);
          Clear SflEftInfo;
          scLocEfDat = CvtToDate(EFefdt);
          scLocExpDt = CvtToDate(EFexdt);
          scLocPNtDt = CvtToDate(EFpdt);
          scLocAppDt = CvtToDate(EFadt);
          scLocFTrDt = CvtToDate(EFftdt);
          scLocLTrDt = CvtToDate(EFltdt);

          If EFpdt = 0;
             scNoteLnk = 'Request Pre-note';
          Else;
             scNoteLnk = 'Clear Pre-note';
          Endif;

          // Get Payee name and address
          scLocPName = %trim(EFpnam);

          If EFtype = 'D';
             Chain keyWDACL# pac00;
             scEftAddr = ACadr1;
             scEftType  = EFtype;
             rrnEFTInfo = rrnEFTInfo + 1;
             scSelRow = *Off;
             If rrnEFTInfo = 1;
             scSelRow = *On;
             Endif;
             scLocRrn = efrrn;
             shwEftLnk = SvshwEftLnk;
             Write SflEftInfo;
          Endif;
             Reade (CMco#:CMfnd:CMfyr:CMcase) pef03;
       Enddo;

       If rrnEFTInfo >=1;
          dspEFTInfo = *On;
          rrnEFTInfo = 1;
          isShowEFT = *On;
       Else;
          dspEFTInfo = *Off;
          tabHeader = 'Additional Payee Details';
          svshwEftLnk = *off;
          isShowEFT = *Off;
          intTab = 0;
          TabIndex = 0;
       Endif;

      /end-free
     P LoadEftInfo     E
       //===================================================================
       // ClearPayInfo - Clears Additional Payees subfile
       //===================================================================
     P ClearPayInfo    B
     D ClearPayInfo    PI
      /Free
       rrnAddPaye = 0;
       ClrAddPaye = *on;
       dspAddPaye = *off;
       Write rcdAddPaye;
      /End-Free
     P ClearPayInfo    E

       //===================================================================
       // ClearEftSub - Clear Subfile for EFT information
       //===================================================================
     P ClearEftSub     B
     D ClearEftSub     PI
      /Free
        rrnEFTInfo = 0;
        clrEFTInfo = *On;
        dspEFTInfo = *Off;
        Write rcdEftInfo;
        clrEFTInfo = *Off;
        dspEFTInfo = *On ;
      /End-Free
     P ClearEftSub     E
       //===================================================================
       // * utility procedure definitions
       //===================================================================
      /Copy sCopyClmPi
     *========================================================================*
