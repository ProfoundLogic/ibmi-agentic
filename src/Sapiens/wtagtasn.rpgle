     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')

      //*********************************************************************
     h/title WTAGTASN - Agency Group Assignments
     h dftname(WTAGTASN)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('WTGRPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 02/15/22                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2395- Redesign of Agency Configuration program *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //  04/21/24 C:340 Project: WC2964                               *
      //                 Add Carriers Column to display all Carriers   *
      //                 assigned to Agency.                           *
      //                                                               *
      // 08/28/23 C:300 Project: WC2395, Support: 113191               *
      //                Action Hyperlinks like Req Pre-note/Clr Pre-note
      //                should not be included as last column of excel *
      //                spreadsheet export                             *
      //                DSPF: Removed hyperlink scNoteLnk and created  *
      //                2 new hyperlinks for Req Pre-note/Clr Pre-note *
      //                                                               *
      // 08/21/23 C:300 Project: WC2395, Support: 113191               *
      //                DSPF: Correct Export to Excel File Name        *
      //                                                               *
      // 08/16/23 C:300 Project: WC2395, Support:113145                *
      //                In Quote Tracking Grid - Screen Positioning    *
      //                lost on clicking Edit/Status links on a Row    *
      //                DSPF: Changed condition for setting Defalt Filter
      //                in onload property of rcdQteTrck record format *
      //                                                               *
      // 07/26/23 C:300 Project: WC2395, Support:113136                *
      //                Enable 'Default Rate Expiration Date' and Added*
      //                1 validation (Default Rate # and Default Rate  *
      //                Expiration Date are Mutually Inclusive)        *
      //                                                               *
      // 07/24/23 C:300 Project: WC2395, Support:113091                *
      //               -Provide a way for users to Copy selected Agent *
      //                Group record to All Other Groups               *
      //               -Disable "Default Group for Connect" if user    *
      //                selects "Copy to All Groups" checkbox          *
      //               -Copy all records with BLANK "Default Group for *
      //                Connect" if user selects "Copy to All Groups"  *
      //                                                               *
      //                                                               *
      // 07/14/23 C:300 Project: WC2395, Support:113090                *
      //                Require Save-Add Next button on Group Assignments
      //                                                               *
      // 06/02/23 C:342 Support:108934                                 *
      //                Increased height of panel on Connect Tab       *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fwtagtasn  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflAgtAsn:rrnAgtAsn)
     f                                     Sfile(sflQteTrck:rrnQteTrck)
     f                                     Sfile(sflEftInfo:rrnEftInfo)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdAgtPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)
      * Agent Master file
     fwmagp     if   e             disk    rename(pag01:pag0p)
      * Agent Assignment File
     fwmaap     uf a e             disk    rename(paa01:paa0p)
     f                                     infds(dbfds4)
     fwmaal     if   e           k disk    rename(paa01:paa00)
     f                                     infds(dbfds)
     fwmaal1    if   e           k disk    rename(paa01:paa02)
     f                                     prefix(a2_)
      * Agent History Master File
     fwmahl3    if   e           k disk    rename(pah01:pah03)
      * Fund Master file
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
      * Employer Detail File
     fwdell     uf   e           k disk    rename(pel01:pel00)
     f                                     usrOpn
      * EFT Account File
     fwmefp     uf a e             disk    rename(pef01:pef0p)
     f                                     infds(dbfds5)
     fwmefl2    if   e           k disk    rename(pef01:pef02)
     f                                     infds(dbfds2)
     fwmefl7    if   e           k disk    rename(pef01:pef07)
     f                                     prefix(l7)
      * Quote Tracking File
     fwaqtp     uf a e             disk    rename(pqt01:pqt0p)
     f                                     infds(dbfds6)
     fwaqtl8    if   e           k disk    rename(pqt01:pqt08)
     f                                     infds(dbfds3)
     fwaqtl9    if   e           k disk    rename(pqt01:pqt09)
     f                                     prefix(q9_)
     fwaqtl12   if   e           k disk    rename(pqt01:pqt12)
      * Quote Status File
     fwtqsl1    if   e           k disk
      * New Business/Renewal Tracking File
     fwmnbp     uf a e             disk    rename(pnb01:pnb0p)
     fwmnbl     if   e           k disk    rename(pnb01:pnb00)
     f                                     infds(nbFDS)
      * NCCI Submission History File
     fwmshlq    uf   e           k disk    rename(psh01:psh00q)
     f                                     usrOpn
      * Application System Control FIle
     fssylf     if   e           k disk    include(SWC01)
      *** Beg Add *** 04/21/24 *** WC2964 *******************************
     fWdF2l     IF   E           K Disk
      *** End Add *** 04/21/24 *** WC2964 *******************************

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
     d nbFDS           ds
     d  nbRRN                397    400b 0

      // work variables
     d saveDT          s                   like(AAdt)
     d saveTM          s                   like(AAtime)
     d saveDT2         s                   like(AAdt)
     d saveTM2         s                   like(AAtime)
     d saveDT3         s                   like(AAdt)
     d saveTM3         s                   like(AAtime)
     d flgRcdUpdated   s              1    inz
     d ##fyr           s                   like(ELfyr)
     d intCo#          s              3  0
     d xxRrn           s              9  0 inz
     d indexTab        s              5  0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d charMsg         s            100
     d process         s               n   inz(*off)
     d infein          s              9  0 inz
     d strfein         s              9    inz
     d xskey           s              2    inz
     d inco#           s              3  0 inz
     d inmod           s              5  0 inz
     d str9999Msg      s            256
     d svrrnAgtAsn     s              5  0
     d svrrnEftInfo    s              5  0
     d svrrnQteTrck    s              5  0
     D intWkDt         s                   like(QTwkdt)
     d tempTyp         s                   like(QTtype)
     d ZeroDate        s               D   inz datfmt(*ISO)
     DstrPortalName    s             40    varying inz
     DdecFnd           s                   like(AAfnd) inz
     Dsavagt#          s                   like(AAagt#) inz
      *** Beg Add *** 07/24/23 ******************************************
     DsavFnd           s                   like(AAfnd) inz
      *** End Add *** 07/24/23 ******************************************
     DsavEfDtAG        s                   like(scEfDtAG) inz
     DsavCnDtAG        s                   like(scCnDtAG) inz
     d*
     DarrTabNames      s            100    varying inz dim(20)
     dn1               s             10i 0 inz(1)
     dstrTabNames      s           2000    varying inz
      //

      // Proto-types

      * Initialize the Screen fields for Group Assignments
     d InzAgtAsnScVal  pr
     d strmode                        1

      * Change Quote Status
     d ChgQteSts       pr

      // Load Group Assignments Grid
     d LoadAgtAsn      pr

      // Load Payment Information Grid
     d LoadEftInfo     pr

      // Load Quote Tracking Information Grid
     d LoadQteTrck     pr

      * Main Processing
     d Main            pr

      * Process Add for Group Assignments Info
     d ProcAddAsn      pr

      * Process Add for EFT Info
     d ProcAddEft      pr

      * Process other Options for Group Assignments
     d ProcOthrAsn     pr

      * Process other Options for EFT Info
     d ProcOthrEft     pr

      * Process other Options for Quote Tracking
     d ProcOthrQte     pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

     d* Set the value of scChoice
     dSetScChoice      pr                  like(scNewStsCH)
     d inCo#                               like(QTco#)
     d inNewRenew                     1    const options(*nopass)
     d inQuote                        1    const options(*nopass)
     d inExcludeXFer                  1n   const options(*nopass)

      * Validate Group Assignments info
     d ValidateAsn     pr              n
     d strmode                        1

      * Validate EFT Info
     d ValidateEft     pr              n
     d strmode                        1

      * Validate Quote Tracking info
     d ValidateQte     pr              n
     d strmode                        1

      * Validate Quote Tracking Status Selection
     d ValidateSts     pr              n
     d strmode                        1


     d/copy sprusrprms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sprCompany
     d/copy wPrPolErr
     d/copy sPrString
     d/copy sPrGenErr
     d/copy wPrAgent
     d/copy sPrModule
     d/copy witmpltd
     d/copy sPrMask
     d/copy wPrEft
     d/copy sPrRunCmd
     d/copy wPrEmpLr
     d/copy sCopyGrpPr
     

     c     keyWMAAL      klist
     c                   kfld                    AGco#
     c                   kfld                    AGfein
     c                   kfld                    AGmod

     c     keyWMEFL7     klist
     c                   kfld                    kktype            1
     c                   kfld                    inCo#
     c                   kfld                    scFndAE
     c                   kfld                    kkkey            30

     c     keyWAQTL      klist
     c                   kfld                    QTco#
     c                   kfld                    QTfnd
     c                   kfld                    QTemp#
     c                   kfld                    QTdiv

     c     keyWAQTL8     klist
     c                   kfld                    AAco#
     c                   kfld                    AAfnd
     c                   kfld                    AAagt#

     c     keyWTQSL      klist
     c                   kfld                    QTco#
     c                   kfld                    QTtype
     
      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9            rrn
     c                   parm                    @@mode            1            mod
     c                   parm                    @@next           10            next pgm
     c                   parm                    @@TabInd          5            tab index

      /free
       Exec Sql Set Option Commit = *None;

       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Endif;

       Chain xxRrn pag0p;
       inCo# = AGco#;
       inFein = AGfein;
       inMod = AGmod;

       If (@@next = *Blanks or @@next = 'WTAGTASN');

          ProgramStart(process);

          str9999msg = 'Your request returned too many records. ' +
           'Click Continue to see the first 9999 records. ' +
           'Click Filter on the display screen to narrow your results.';

          If process;

             Main();

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
       // InzAgtAsnScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzAgtAsnScVal  b
     d InzAgtAsnScVal  pi
     d strmode                        1
      /Free
       scCo# = intCo#;

       // Set Selection Criteria of Group Number
       scFndSel = 'FNCO# = ' + %char(inCO#);

       If strmode = 'C';
          // Set Selection Criteria of Defautl Rate #
          // Need to show just latest record of each Rate Number
          arChoices = 'A.ARco# = ' + %char(inCO#) + ' AND A.ARfnd = '
           + %char(scFndAG) + ' AND A.ARstat = ''A''' + ' AND A.ARefdt = +
           (SELECT MAX(ARefdt) from WMARP B where A.ARco#=B.ARco# and +
           a.ARfnd=b.ARfnd and a.ARrt#=b.ARrt# group by ARco#, ARfnd, ARrt#)';

          // Set Selection Criteria of Chapter
          scChpSel = 'CPCO# = ' + %char(inCO#) + ' AND CPFND = '
           + %char(scFndAG) + ' AND CPSTAT = ''A''';
       Endif;

       // Set visibilty and Heading for Portal Group Checkbox
       isPortal = *off;
       strPortalName = *Blanks;
       isPortal = IsModEnabled('Portal Module');
       strPortalName = GetModuleProperty('Portal Module':'Name');
       scPortDftH = 'Default Group for ' + strPortalName;

      /End-Free
     p InzAgtAsnScVal  e

       //===================================================================
       // ChgQteSts - Change Quote status
       //===================================================================
     P ChgQteSts       B
     D ChgQteSts       PI

     D strCmd          s             80
     D intCmdSize      s              3  0
     D SAVEQTXFER      s                   like(QTxfer) inz

      /Free

       Select;
       When Upper(scNewSts) = 'T';
          Exsr Transfer;
       When Upper(scNewSts) = 'C' or Upper(scNewSts) = 'R';
          Exsr WriteWAQTP;
       When Upper(scNewSts) = 'D' or Upper(scNewSts) = 'S';
          Exsr CrtNewBusTrk;
          Exsr ChgQuoteType;
       When Upper(scNewSts) = 'L' or Upper(scNewSts) = 'D';
          Exsr CrtNewBusTrk;
          Exsr ChgQuoteType;
          Exsr DltSubmHist;
       Other;
          Exsr ChgQuoteType;
       Endsl;

       //=============================================================
       // Change the quote status type
       //=============================================================
       Begsr ChgQuoteType;

        Chain scQteTrRRN WAQTP;

        If %Found;
           Exsr LoadWAQTP;
           Update pqt0p;
        Endif;

       Endsr;

       //==============================================================
       // Create New Business Tracking Record
       //==============================================================
       Begsr CrtNewBusTrk;

        Clear WCfl26;
        Chain 'WC' swc01;

        Select;
        When WCfl26 = 'Y';
           Clear pqt0p;
           Chain(n) scQteTrRRN pqt0p;
           Chain (QTco#:QTfnd:QTemp#) pnb00;
           If %found(WMNBL);
              Chain nbRRN WMNBP;
           Else;
              Clear pnb0p;
           Endif;
           Select;
           When Upper(scNewSts) = 'L'; // Lost
              NBlsDt = NBdt;
           When Upper(scNewSts) = 'D'; // Declined
              NBdcDt = NBdt;
           Endsl;
           NBuser = q1user;
           NBpgm  = q1pgm;
           NBdt   = DateTo7(SystemDate);
           NBtime = TimeTo6(SystemTime);
           If %found(WMNBP);
              Update pnb0p;
           Else;
              NBco#  = QTco#;
              NBfnd  = QTfnd;
              NBemp# = QTemp#;
              NBiUsr = NBuser;
              NBiPgm = NBpgm;
              NBiDt  = NBdt;
              NBiTm  = NBtime;
              Write pnb0p;
           Endif;
        Endsl;

       Endsr;

       //===============================================================
       // Delete WMSHP records
       //===============================================================
       Begsr DltSubmHist;

        strCmd = 'OVRDBF FILE(WMSHLQ) TOFILE(' +
          %trim(RtvQuoteLib('*QTE')) + '/WMSHLQ)';
        intCmdSize = %Checkr(' ':strCmd);

        Callp RunCmd(strCmd:intCmdSize);

        Open WMSHLQ;

        //Dou %eof(WMSHLQ);
        Dou not %found(WMSHLQ);
           Delete (QTco#:QTfnd:QTemp#) psh00q;
        Enddo;

        Close WMSHLQ;

        strCmd = 'DLTOVR FILE(WMSHLQ)';
        intCmdSize = %Checkr(' ':strCmd);

        Callp(e) RunCmd(strCmd:intCmdSize);

       Endsr;

       //==============================================================
       // Load WAQTP fields (QTTYPE and maintenance)
       //==============================================================
       Begsr LoadWAQTP;

        QTtype = scNewSts;
        QTuser = q1User;
        QTpgm  = q1pgm;
        QTdt   = DateTo7(SystemDate);
        QTtime = TimeTo6(SystemTime);
        QTwkDt = QTdt;

       Endsr;

       //==============================================================
       // Transfer an employer from one agent to another
       //==============================================================
       Begsr Transfer;

       ///If Defined (TRANSFER)
        Select;
        When QTstat = 'Q';
           Exsr ChgQuoteType;
        Other;
           Select;
           When Upper(QTtype) = 'C' or Upper(QTtype) = 'R';
              saveQTxFer = QTtype;
           Endsl;
           scNewSts = 'X';
           Exsr WriteWAQTP;
           scNewSts = 'T';
           Exsr WriteWAQTP;
           Clear QTxFer;
        Endsl;
       ///Endif

       Endsr;

       //==============================================================
       // Write WAQTP record
       //==============================================================
       Begsr WriteWAQTP;

       ///If Defined (TRANSFER)
        Select;
        When Upper(scNewSts) = 'T';
           QTagt# = scNewAgt;
           QTxFer = saveQTxFer;
        When Upper(scNewSts) = 'X';
           QTagt# = QTagt#;
           QTxFer = saveQTxFer;
        Endsl;
       ///Endif

        Exsr LoadWAQTP;

        QTiUsr = QTuser;
        QTiPgm = QTpgm;
        QTiDt  = QTdt;
        QTiTm  = QTtime;

        Write pqt0p;

        Feod WAQTP;

       Endsr;


      /End-free

     P ChgQteSts       e

       //===================================================================
       // Load Group Assignments Grid
       //===================================================================
     P LoadAgtAsn      B
     D LoadAgtAsn      PI
      /free

       // Clear the subfile.
       clrAgtAsn = *On;
       dspAgtAsn = *off;
       Write rcdAgtAsn;
       clrAgtAsn = *Off;
       dspAgtAsn = *on;
       rrnAgtAsn = 0;
       scRowSel = 1;

       If dspMode;
          inquiry = *on;
       Endif;

       Setll keyWMAAL paa00;
       Reade keyWMAAL paa00;

       DoW not %eof(WMAAL);
          scFnd# = AAfnd;
          scAgt# = AAagt#;

          scName = *Blanks;
          Chain (AAco#:AAfnd) pfn00;
          If %found(WMFNL);
             scName = %trim(FNname);
          Endif;

          scEfDt = CvtToDate(AAefdt);
          scCnDt = CvtToDate(AAcndt);
          scStat = *Blanks;
          If AAstat = 'A';
             scStat = 'Active';
          Endif;
          If AAstat = 'I';
             scStat = 'Inactive';
          Endif;

      *** Beg Add *** 04/21/24 *** WC2964 *******************************
          Chain (AAco#:AAfnd) WDF2L;
          If %found(WDF2L);
             scGrpCarr = F2car#;
          Endif;
      *** End Add *** 04/21/24 *** WC2964 *******************************

          scAsnrrn = dbrrn;
          rrnAgtAsn = rrnAgtAsn + 1;
          scAsnSel = *off;

          // set the selection on 1st record for first load
          If svrrnAgtAsn = *zeros;
             If rrnAgtAsn = 1;
                scAsnSel = *on;
             Endif;
          Else;
             If svrrnAgtAsn = rrnAgtAsn;
                scAsnSel = *on;
             Endif;
          Endif;

          Write sflAgtAsn;

          Reade keyWMAAL paa00;
       Enddo;

       rrnAgtAsn = 1;
       If svrrnAgtAsn <> 0;
          rrnAgtAsn = svrrnAgtAsn;
          scRowSel = svrrnAgtAsn;
          svrrnAgtAsn = 0;
       Endif;

      /end-free
     P LoadAgtAsn      E

       //===================================================================
       // Load Payment Information Grid
       //===================================================================
     P LoadEftInfo     B
     D LoadEftInfo     PI
      /free

       // Clear the subfile.
       clrEftInfo = *On;
       dspEftInfo = *off;
       Write rcdEftInfo;
       clrEftInfo = *Off;
       dspEftInfo = *on;
       rrnEftInfo = 0;
       scRowSel = 1;

       If dspMode;
          inquiry = *on;
       Endif;

       Setll keyWMAAL pef02;
       Reade keyWMAAL pef02;

       DoW not %eof(WMEFL2);
          scPnam = EFpnam;
          strfein = %char(EFfein);
          scFein = MaskData('SSN':'EFfein':strfein);
          scFnd  = EFfnd;
          scEfDt = CvtToDate(EFefdt);
          scExDt = CvtToDate(EFexdt);
          scPdt  = CvtToDate(EFpdt);
          scAdt  = CvtToDate(EFadt);
          scFtDt = CvtToDate(EFftdt);
          scLtDt = CvtToDate(EFltdt);

          //*** Begin Chg ***** 08/28/23 *****************************
          // If EFpdt = 0;
          //    scNoteLnk = 'Req Pre-note';
          // Else;
          //    scNoteLnk = 'Clr Pre-note';
          // Endif;

          visReqNote = *off;               // New Hyperlink for Req Pre-note
          visClrNote = *off;               // New Hyperlink for Clr Pre-note
          If EFpdt = 0;
             visReqNote = *on;
          Else;
             visClrNote = *on;
          Endif;

          If dspMode;
             visReqNote = *off;
             visClrNote = *off;
          Endif;
          //*** End Chg ***** 08/28/23 *****************************

          scEftrrn = dbrrn2;
          rrnEftInfo = rrnEftInfo + 1;
          scEftSel = *off;

          // set the selection on 1st record for first load
          If svrrnEftInfo = *zeros;
             If rrnEftInfo = 1;
                scEftSel = *on;
             Endif;
          Else;
             If svrrnEftInfo = rrnEftInfo;
                scEftSel = *on;
             Endif;
          Endif;

          Write sflEftInfo;

          Reade keyWMAAL pef02;
       Enddo;

       rrnEftInfo = 1;
       If svrrnEftInfo <> 0;
          rrnEftInfo = svrrnEftInfo;
          scRowSel = svrrnEftInfo;
          svrrnEftInfo = 0;
       Endif;

      /end-free
     P LoadEftInfo     E

       //===================================================================
       // Load Quote Tracking Information Grid
       //===================================================================
     P LoadQteTrck     B
     d LoadQteTrck     PI
      /free

       // clear the subfile.
       clrQteTrk  = *On;
       dspQteTrSb = *off;
       Write rcdQteTrck;
       clrQteTrk  = *Off;
       dspQteTrSb = *on;
       rrnQteTrck = 0;
       scRowSel = 1;

       If dspMode;
          inquiry = *on;
       Endif;


       //Setll keyWAQTL8 pqt08;
       //Reade keyWAQTL8 pqt08;
       Setll AAco# pqt08;
       Reade AAco# pqt08;
       dow not %eof(WAQTL8);

          If QTagt# = AAagt#;
             // Write Quote tracking info
             scQteNam   = %trim(QTname);
             scQFnd#    = QTfnd;
             scQteEmp   = QTemp#;
             scQteDiv   = QTdiv;
             If QTstat = 'Q';
                scQteLiv = 'Quote';
             Else;
                scQteLiv = 'Live';
             Endif;

             scQtEffDt  = CvtToDate(QTefdt);
             scQtTypDt  = CvtToDate(QTwkdt);
             scQtSubDt  = CvtToDate(QTordt);
             scQtBusDt  = CvtToDate(QTnbdt);
             scQtSentDt = CvtToDate(QTfxdt);
             scQtPrem   = QTprem;

             clear scQteSts;
             Select;
             When QTtype = 'F';
                QTtype = 'p';
             When QTtype = 'G';
                QTtype = 'q';
             Endsl;
             Chain keyWTQSL pqs01;
             If %found(WTQSL1);
                scQteSts = %trim(QSdesc);
             Endif;

             scQteTrRRN = dbrrn3;
             rrnQteTrck = rrnQteTrck + 1;
             scQteSel = *off;

          // set the selection on 1st record for first load
          If svrrnQteTrck = *zeros;
             If rrnQteTrck = 1;
                scQteSel = *on;
             Endif;
          Else;
             If svrrnQteTrck = rrnQteTrck;
                scQteSel = *on;
             Endif;
          Endif;

             Write sflQteTrck;
          Endif;
          //Reade keyWAQTL8 pqt08;
          Reade AAco# pqt08;
       Enddo;

       rrnQteTrck = 1;
       If svrrnQteTrck <> 0;
          rrnQteTrck = svrrnQteTrck;
          scRowSel = svrrnQteTrck;
          svrrnQteTrck = 0;
       Endif;

      /end-free
     P LoadQteTrck     E
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
     d*
     d mode            s              1
      /free

       BtnCancel = *Off;

       // Process until exit button is pressed.
       DoW BtnCancel = *Off;

          // display screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;
          // do nothing - catch tab click event
          When TabClick = *on;
             TabClick = *off;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;

          // Add Group Assignments Info
          When BtnAsnAdd = *On;
             BtnAsnAdd = *Off;
             ProcAddAsn();

          // Add EFT Info
          When BtnEftAdd = *On;
             BtnEftAdd = *Off;
             ProcAddEft();

          // For "Change FEIN" option in More Action of Agency Toolbar
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:' ':txtWinData);
             btnShowWin = *off;


          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;


          Other;
             ProcOthrAsn();         //other option for Group Assignments Info
             ProcOthrEft();         //other option for EFT Info
             ProcOthrQte();         //other option for Quote Tracking

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;

       When arrTabNames(TabIndex+1) = 'Group Assignments';
          pnlHeight = 500;
          APnlHeight = pnlHeight + 60;

          // Loading initial values for Detail Box
          mode = 'D';
          InzAgtAsnScVal(mode);

          // Load Group Assignments Grid
          LoadAgtAsn();

          If dspMode;
             inquiry = *on;
          Endif;

          Write rcdAgtPnl;
          Write rcdPanel;
          Exfmt rcdAgtAsn;
          Read rcdAgtPnl;
          Read rcdPanel;

       When arrTabNames(TabIndex+1) = 'Quote Tracking';
          pnlHeight = 500;
          APnlHeight = pnlHeight + 60;

          // Load Quote Tracking Grid
          LoadQteTrck();

          // Set Group filter value to Default User Group
          Chain q1User smupl;
          scFndFlt = %Char(UPfnd);
          if UPfnd = 0;
             scFndFlt = *Blanks;
          Endif;

          If dspMode;
             inquiry = *on;
          Endif;

          Write rcdAgtPnl;
          Write rcdPanel;
          Exfmt rcdQteTrck;
          Read rcdAgtPnl;
          Read rcdPanel;

       When arrTabNames(TabIndex+1) = 'Payment Information';
          pnlHeight = 500;
          APnlHeight = pnlHeight + 60;

          // Load Payment Information Grid
          LoadEftInfo();

          If dspMode;
             inquiry = *on;
          Endif;

          Write rcdAgtPnl;
          Write rcdPanel;
          Exfmt rcdEftInfo;
          Read rcdAgtPnl;
          Read rcdPanel;

       When arrTabNames(TabIndex+1) = 'Connect';
          //*** Beg Chg *** 06/02/23 ******************************************
          //*** pnlHeight = 750;
          pnlHeight = 880;
          //*** End Chg *** 06/02/23 ******************************************
          APnlHeight = pnlHeight + 60;

          Reset dsWITMPLTparms;
          dsWITMPLTparms.co# = inCo#;
          dsWITMPLTparms.fein = inFein;
          dsWITMPLTparms.mod = inMod;

          Write rcdAgtPnl;
          Write rcdPanel;
          TemplateAssociations(dsWITMPLTparms:'Agency':@@Mode);
          Read rcdAgtPnl;
          Read rcdPanel;

       EndSl;

       Endsr;

      /end-free
     p Main            e

       //===================================================================
       // ProcAddAsn  - process addition of Group Assignments
       //===================================================================
     P ProcAddAsn      B
     D ProcAddAsn      PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdAddAsn;
       mode = 'A';
       isexit = *off;
       BtnAsnCan = *off;
       scAsnStat = 'A';
       scCmflAG  = 'Y';
       scMlflAG  = 'Y';
       scPrflAG  = 'N';
       scCo# = intCo#;                     // Company

       Dow BtnAsnCan = *off and isexit = *off;

          // Loading initial values
          InzAgtAsnScVal(mode);

          scAsnHdg = 'Add Group Assignments Information';
          visAsnAdS = *On;                           // Show Save/Add next Button

          Exfmt rcdAddAsn;

          Select;

          // Save-Add Next
          When BtnAsnAds = *on;
             BtnAsnAds = *off;
             If ValidateAsn(mode);
                Exsr WrtAsn;
                //Initialize fields for next add screen if save/addnext is pressed
                Clear rcdAddAsn;
                BtnAsnCan = *off;
                scAsnStat = 'A';
                scCmflAG  = 'Y';
                scMlflAG  = 'Y';
                scPrflAG  = 'N';
                scCo# = intCo#;
             EndIf;

          // Save Button
          When BtnAsnSav = *on;
             BtnAsnSav = *off;
             If ValidateAsn(mode);
                Exsr WrtAsn;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnAsnCan = *on;
             BtnAsnCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Writing Group Assignments Details Subroutine
       BegSr WrtAsn;
        Clear paa0p;
        Exsr MoveScDb;
        Write paa0p;
       EndSr;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        AAco#   = inCo#;                 // Company Number
        AAfein  = inFein;                // Agency Fein
        AAmod   = inMod;                 // Mod
        AAfnd   = scFndAG;               // Group Number
        AAagt#  = scAgt#AG;              // Agent Number Assigned by Group
        AAname  = AGname;                // Agency Name
        AAfdnm  = AGfdnm;                // Agency Federal Name
        AAefdt  = dtCYMD(scEfDtAG);      // Effective Date
        AAcndt  = dtCYMD(scCnDtAG);      // Cancellation Date
        AAstat  = scAsnStat;             // Status
        AAportdft = scPortDft;           // Portal Default Flag
        AAcmfl  = scCmflAG;              // Commission Mail Flag
        AAprfl  = scPrflAG;              // Prospect Mail Flag
        AAmlfl  = scMlflAG;              // Regular Mail Flag
        AAuwtr  = scUwtrAG;              // Underwriter
        AAchap  = scChapAG;              // Chapter
        AAcmmt  = scCmmtAG;              // Commitment Amount
        AAcmdt  = dtCYMD(scCmDtAG);      // Commitment Date
        AAdrt#  = scDrt#AG;              // Default Rate #
        AAexdt  = dtCYMD(scExDtAG);      // Default Rate Expiration Date

        AAuser = q1user;
        AApgm  = q1pgm;
        AAtime = Timeto6(SystemTime());
        AAdt   = Dateto7(SystemDate());
        AAitm  = Timeto6(SystemTime());
        AAidt  = Dateto7(SystemDate());
        AAiusr = q1user;
        AAipgm = q1pgm;
       EndSr;

      /end-free
     P ProcAddAsn      E

       //===================================================================
       // ProcAddEft  - process addition of EFT Info
       //===================================================================
     P ProcAddEft      B
     D ProcAddEft      PI
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdAddEft;
       mode = 'A';
       isexit = *off;
       BtnEftCan = *off;
       scCo# = intCo#;                     // Company

       Dow BtnEftCan = *off and isexit = *off;

          // Set Selection Criteria of Group Number
          scFndSel = 'FNCO# = ' + %char(inCO#);

          scEftHdg = 'Add EFT Information';
          visbDates = *Off;            //Hide date fields in Edit Mode

          Exfmt rcdAddEft;

          Select;

          // Save Button
          When BtnEftSav = *on;
             BtnEftSav = *off;
             If ValidateEft(mode);
                Exsr WrtEft;
                isexit = *on;
             EndIf;

          // Cancel Button
          When BtnEftCan = *on;
             BtnEftCan = *off;
             isexit = *on;

          EndSl;
       Enddo;

       // Writing Group Assignments Details Subroutine
       BegSr WrtEft;
        Clear pef0p;
        Exsr MoveScDb;
        Write pef0p;
       EndSr;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        EFco#   = inCo#;                 // Company Number
        EFfein  = inFein;                // Agency Fein
        EFmod   = inMod;                 // Mod
        EFfnd   = scFndAE;               // Group Number
        EFagt#  = AAagt#;                // Agent Number Assigned by Group
        EFpnam  = AAname;                // Payee Name
        EFtype  = 'A';                   // Type of Payee
        EFefdt  = dtCYMD(scEfDtAE);      // Effective Date
        EFexdt  = dtCYMD(scExDtAE);      // Expiration Date
        EFpdt   = dtCYMD(scPDtAE);       // Pre-notification Date
        EFadt   = dtCYMD(scADtAE);       // Approved Date
        EFftdt  = dtCYMD(scFtDtAE);      // First Transfer Date
        EFltdt  = dtCYMD(scLtDtAE);      // Last Transfer Date

        EFuser = q1user;
        EFpgm  = q1pgm;
        EFtime = Timeto6(SystemTime());
        EFdt   = Dateto7(SystemDate());
        EFitm  = Timeto6(SystemTime());
        EFidt  = Dateto7(SystemDate());
        EFiusr = q1user;
        EFipgm = q1pgm;
       EndSr;

      /end-free
     P ProcAddEft      E

       //===================================================================
       // ProcOthrAsn  - Process other Options for Group Assignments
       //===================================================================
     P ProcOthrAsn     B
     D ProcOthrAsn     PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdAddAsn;
       isexit = *off;
       ReadC sflAgtAsn;

       Dow not %eof;

          Select;
          When scAsnEdt = *On; // Processing for Edit Group Assignments Info

             scAsnEdt = *Off;
             mode = 'C';
             svrrnAgtAsn = rrnAgtAsn;   //Save Rrn value for edited record
             AGEdt = *On;                 //disable non-editable field

             Chain(n) scAsnRRN WMAAP;

             Exsr MovDbSc;

             // Loading initial values
             InzAgtAsnScVal(mode);

             scAsnHdg = 'Edit Group Assignments Information';

             Dow isexit = *off;
                Exfmt rcdAddAsn;

                Select;
                // Save Button
                When BtnAsnSav = *on;
                   BtnAsnSav = *off;
                   // Check if WMAAP updated since screen displayed (Group Assignments Info)
                   flgRcdUpdated=RcdUpdatedErr('WMAAP':dbrrn4:'AAdt':'AAtime':
                    'AAuser':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scAsnRRN WMAAP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateAsn(mode);
                         Chain scAsnRRN WMAAP;
                         Exsr MoveScDb;
                         Update paa0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnAsnCan = *on;
                   BtnAsnCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

             Unlock WMAAP;


          When scAsnCpy = *On; // Processing for Copy Group Assignments Info

             scAsnCpy = *Off;
             mode = 'A';
             AGEdt = *Off;                //enaable non-editable field
             visCopyX = *On;              // Show 'Copy to All Groups' checkbox

             Chain(n) scAsnRRN WMAAP;

             // Loading initial values
             InzAgtAsnScVal(mode);

             //*** Beg Add *** 07/24/23 ******************************************
             savFnd  = 0;               // Save Group Number
             //*** End Add *** 07/24/23 ******************************************
             savagt# = 0;               // Agent Number Assigned by Group
             savEfDtAG = 0;              // Effective Date
             savCnDtAG = 0;              // Cancellation Date

             Exsr MovDbSc;

             scFndAG = 0;               // Group Number
             scChapAG = 0;              // Chapter
             scAsnHdg = 'Copy Group Assignments Information';

             Dow isexit = *off;
                Exfmt rcdAddAsn;

                Select;
                // Save Button
                When BtnAsnSav = *on;
                   BtnAsnSav = *off;
                   If ValidateAsn(mode);
                      Clear paa0p;
                      Exsr MoveScDb;

                      AAco#   = inCo#;                 // Company Number
                      AAfein  = inFein;                // Agency Fein
                      AAmod   = inMod;                 // Mod
                      AAitm  = Timeto6(SystemTime());
                      AAidt  = Dateto7(SystemDate());
                      AAiusr = q1user;
                      AAipgm = q1pgm;

                      //*** Beg Chg *** 07/24/23 ******************************************
                      If scCopyX <> 'Y';
                         AAfnd   = scFndAG;               // Group Number
                         Write paa0p;
                      Endif;
                      If scCopyX = 'Y';                   // Copy to all groups
                         AAportdft = ' ';                 // Portal Default Flag
                         Setll *loval pfn00;
                         Read pfn00;
                         Dow not %eof(wmfnl);
                            If FNfnd <> savFnd;
                               Chain (inCo#:inFein:inMod:FNfnd) paa00;
                               If not %found(wmaal);
                                  AAfnd = FNfnd;               // Group Number
                                  Write paa0p;
                               Endif;
                            Endif;
                            Read pfn00;
                         Enddo;
                      Endif;
                      //*** End Chg *** 07/24/23 ******************************************

                      isexit = *on;
                   EndIf;

                // Cancel Button
                When BtnAsnCan = *on;
                   BtnAsnCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          Endsl;

          ReadC sflAgtAsn;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scCo# = intCo#;                  // Company
        scFeinAG = AAfein;               // Federal Tax ID
        scModAG  = AAmod;                // Modifier
        scFndAG  = AAfnd;                // Group Number
        //*** Beg Add *** 07/24/23 ******************************************
        savFnd   = AAfnd;                // Save Group Number
        //*** End Add *** 07/24/23 ******************************************
        scAgt#AG = AAagt#;               // Agent Number Assigned by Group
        savagt#  = AAagt#;               // Agent Number Assigned by Group
        scEfDtAG = dtMDYY(AAefdt);       // Effective Date
        savEfDtAG = dtMDYY(AAefdt);      // Effective Date
        scCnDtAG = dtMDYY(AAcndt);       // Cancellation Date
        savCnDtAG = dtMDYY(AAcndt);      // Cancellation Date
        scAsnStat = AAstat;              // Status
        scPortDft = AAportdft;           // Portal Default Flag
        scCmflAG = AAcmfl;               // Commission Mail Flag
        scPrflAG = AAprfl;               // Prospect Mail Flag
        scMlflAG = AAmlfl;               // Regular Mail Flag
        scUwtrAG = AAuwtr;               // Underwriter
        scChapAG = AAchap;               // Chapter
        scCmmtAG = AAcmmt;               // Commitment Amount
        scCmDtAG = dtMDYY(AAcmdt);       // Commitment Date
        scDrt#AG = AAdrt#;               // Default Rate #
        scExDtAG = dtMDYY(AAexdt);       // Default Rate Expiration Date

        saveDT = AAdt;
        saveTM = AAtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        AAagt#  = scAgt#AG;              // Agent Number Assigned by Group
        AAname  = AGname;                // Agency Name
        AAfdnm  = AGfdnm;                // Agency Federal Name
        AAefdt  = dtCYMD(scEfDtAG);      // Effective Date
        AAcndt  = dtCYMD(scCnDtAG);      // Cancellation Date
        AAstat  = scAsnStat;             // Status
        AAportdft = scPortDft;           // Portal Default Flag
        AAcmfl  = scCmflAG;              // Commission Mail Flag
        AAprfl  = scPrflAG;              // Prospect Mail Flag
        AAmlfl  = scMlflAG;              // Regular Mail Flag
        AAuwtr  = scUwtrAG;              // Underwriter
        AAchap  = scChapAG;              // Chapter
        AAcmmt  = scCmmtAG;              // Commitment Amount
        AAcmdt  = dtCYMD(scCmDtAG);      // Commitment Date
        AAdrt#  = scDrt#AG;              // Default Rate #
        AAexdt  = dtCYMD(scExDtAG);      // Default Rate Expiration Date

        AAuser = q1user;
        AApgm  = q1pgm;
        AAtime = Timeto6(SystemTime());
        AAdt   = Dateto7(SystemDate());
        saveDT = AAdt;
        saveTM = AAtime;
       EndSr;

      /end-free
     P ProcOthrAsn     E

       //===================================================================
       // ProcOthrEft  - Process other Options for EFT Info
       //===================================================================
     P ProcOthrEft     B
     D ProcOthrEft     PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdAddEft;
       isexit = *off;
       ReadC sflEftInfo;

       Dow not %eof;

          Select;
          When scEftEdt = *On; // Processing for Edit EFT Info

             scEftEdt = *Off;
             mode = 'C';
             svrrnEftInfo = rrnEftInfo;   //Save Rrn value for edited record
             AEEdt = *On;                 //disable non-editable field
             visbDates = *On;             //Show date fields in Edit Mode

             Chain(n) scEftRRN WMEFP;

             Exsr MovDbSc;

             scCo# = intCo#;
             // Set Selection Criteria of Group Number
             scFndSel = 'FNCO# = ' + %char(inCO#);

             scEftHdg = 'Edit EFT Information';

             Dow isexit = *off;
                Exfmt rcdAddEft;

                Select;
                // Save Button
                When BtnEftSav = *on;
                   BtnEftSav = *off;
                   // Check if WMEFP updated since screen displayed (EFT Info)
                   flgRcdUpdated=RcdUpdatedErr('WMEFP':dbrrn5:'EFdt':'EFtime':
                    'EFuser':saveDT2:saveTM2:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scEftRRN WMEFP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateEft(mode);
                         Chain scEftRRN WMEFP;
                         Exsr MoveScDb;
                         Update pef0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnEftCan = *on;
                   BtnEftCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

             Unlock WMEFP;


          When scEftbnkI = *On; // Processing for EFT Bank Info
             scEftbnkI = *Off;
             EFTBANKINFO(scEftRRN);


          //*** Begin Chg ***** 08/28/23 *****************************
          // When scRqPNtLc = *On; // Processing for Request/Clear Pre-Note Date
          //    scRqPNtLc = *Off;
          //    If scPDt = ZeroDate;
          //       ReqPreNote(scEftRRN);
          //    Else;
          //       ClrPreNote(scEftRRN);
          //    Endif;

          When scRqPNtLc = *On; // Processing for Request Pre-Note Date
             scRqPNtLc = *Off;
             ReqPreNote(scEftRRN);

          When scClPNtLc = *On; // Processing for Clear Pre-Note Date
             scClPNtLc = *Off;
             ClrPreNote(scEftRRN);
          //*** Begin Chg ***** 08/28/23 *****************************


          Endsl;

          ReadC sflEftInfo;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        scCo# = intCo#;                  // Company
        scFndAE  = EFfnd;                // Group Number
        scEfDtAE = dtMDYY(EFefdt);       // Effective Date
        scExDtAE = dtMDYY(EFexdt);       // Expiration Date
        scPDtAE  = dtMDYY(EFpdt);        // Pre-notification Date
        scADtAE  = dtMDYY(EFadt);        // Approved Date
        scFtDtAE = dtMDYY(EFftdt);       // First Transfer Date
        scLtDtAE = dtMDYY(EFltdt);       // Last Transfer Date
        saveDT2 = EFdt;
        saveTM2 = EFtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        EFefdt  = dtCYMD(scEfDtAE);      // Effective Date
        EFexdt  = dtCYMD(scExDtAE);      // Expiration Date
        EFpdt   = dtCYMD(scPDtAE);       // Pre-notification Date
        EFadt   = dtCYMD(scADtAE);       // Approved Date
        EFftdt  = dtCYMD(scFtDtAE);      // First Transfer Date
        EFltdt  = dtCYMD(scLtDtAE);      // Last Transfer Date

        EFuser = q1user;
        EFpgm  = q1pgm;
        EFtime = Timeto6(SystemTime());
        EFdt   = Dateto7(SystemDate());
        saveDT2 = EFdt;
        saveTM2 = EFtime;
       EndSr;

      /end-free
     P ProcOthrEft     E

       //===================================================================
       // ProcOthrQte  - Process other Options for Quote Tracking
       //===================================================================
     P ProcOthrQte     B
     D ProcOthrQte     PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
     D strCmd          s             80
     D intCmdSize      s              3  0
      /free

       Clear rcdEdtQte;
       Clear rcdSelSts;
       isexit = *off;
       ReadC sflQteTrck;

       Dow not %eof;

          Select;
          When scEdtQte = *On; // Processing for Edit Quote Tracking

             scEdtQte = *Off;
             mode = 'C';
             svrrnQteTrck = rrnQteTrck;   //Save Rrn value for edited record

             Chain(n) scQteTrRrn WAQTP;

             Exsr MovDbSc;

             Dow isexit = *off;
                Exfmt rcdEdtQte;

                Select;
                // Save Button
                When BtnQteSav = *on;
                   BtnQteSav = *off;
                   // Check if WAQTP updated since screen displayed (Quote Tracking Info)
                   flgRcdUpdated=RcdUpdatedErr('WAQTP':dbrrn6:'QTdt':'QTtime':
                    'QTuser':saveDT3:saveTM3:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scQteTrRrn WAQTP;
                      Exsr MovDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateQte(mode);
                         Chain scQteTrRrn WAQTP;
                         Exsr MoveScDb;
                         Update pqt0p;
                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cancel Button
                When BtnQteCan = *on;
                   BtnQteCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

             Unlock WAQTP;


          When scStsQte = *On; // Processing for Quote Tracking Status Selection

             scStsQte = *Off;
             mode = 'C';
             svrrnQteTrck = rrnQteTrck;   //Save Rrn value for edited record

             Chain(e) scQteTrRrn WAQTP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again lat+
                er.':'Record Lock');
                Return;
             Endif;

             If scQteLiv = 'Quote';
                strCmd = 'OVRDBF FILE(WDELL) TOFILE(' +
                 %trim(RtvQuoteLib('*QTE')) + '/WDELL)';
                intCmdSize = %Checkr(' ':strCmd);
                Callp RunCmd(strCmd:intCmdSize);
             Endif;
             Open WDELL;

             ##fyr = *hival;
             Chain(n) scQteTrRrn WAQTP;
             Setgt     (QTco#:QTfnd:QTemp#:QTdiv:##fyr) pel00;
             Readpe(n) (QTco#:QTfnd:QTemp#:QTdiv) pel00;

             // Set Selection Criteria for New Status Type
             scNewStsCH =
              SetScChoice(ELco#:ELnwRw:scQteLiv:*on);

             If scQteLiv = 'Quote';
                strCmd = 'DLTOVR FILE(WDELL)';
                intCmdSize = %Checkr(' ':strCmd);
                Callp RunCmd(strCmd:intCmdSize);
             Endif;
             Close WDELL;

             // Set Selection Criteria for New Agency Number
             scNewAgtCH = 'AACO# = ' + %char(QTco#) + ' AND AAFND = '
              + %char(QTfnd) + ' AND AASTAT = ''A''';

             Chain keyWTQSL pqs01;
             If %found(WTQSL1);
                scCurrSts = %trim(QSdesc);
             Endif;
             scNewSts = QTtype;

             Dow isexit = *off;
                Exfmt rcdSelSts;

                Select;
                // Confirm Button
                When BtnStsCon = *on;
                   BtnStsCon = *off;
                   If ValidateSts(mode);
                      ChgQteSts();
                      UpdFQLockSts(QTco#:QTfnd:QTemp#:QTdiv:scNewSts);
                      isexit = *on;
                   Endif;

                // Cancel Button
                When BtnStsCan = *on;
                   BtnStsCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          Endsl;

          ReadC sflQteTrck;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MovDbSc;
        Chain keyWTQSL pqs01;
        If %found(WTQSL1);
           scEdtTyp = %trim(QSdesc);
        Endif;

        scEdtEffDt = DtMDYY(QTefdt);
        scEdtChgDt = DtMDYY(QTwkdt);
        scEdtSubDt = DtMDYY(QTordt);
        scEdtBusDt = DtMDYY(QTnbdt);
        If scEdtBusDt = 0;
           protNewBus = *off;
        Else;
           protNewBus = *on;
        Endif;
        scEdtSntDt = DtMDYY(QTfxdt);
        scEdtCmnt1 = QTcomm;
        scEdtCmnt2 = QTcom2;
        scEdtCrInf = QTcarr;
        scEdtChptr = QTchap;
        If QTprem > 0;
           dspEdtPrem = *on;
        Else;
           dspEdtPrem = *off;
        Endif;
        scEdtPrem  = QTprem;

        saveDT3 = QTdt;
        saveTM3 = QTtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        QTefdt = DtCYMD(scEdtEffDt);
        QTwkdt = DtCYMD(scEdtChgDt);
        QTordt = DtCYMD(scEdtSubDt);
        QTnbdt = DtCYMD(scEdtBusDt);
        QTfxdt = DtCYMD(scEdtSntDt);
        QTcomm = scEdtCmnt1;
        QTcom2 = scEdtCmnt2;
        QTcarr = scEdtCrInf;
        QTprem = scEdtPrem;

        QTuser = q1user;
        QTpgm  = q1pgm;
        QTtime = Timeto6(SystemTime());
        QTdt   = Dateto7(SystemDate());
        saveDT3 = QTdt;
        saveTM3 = QTtime;
       EndSr;

      /end-free
     P ProcOthrQte     E

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free

       // Program Mode.
       If q1prms >= 2;
          Select;
          When @@mode = 'D';
             dspMode = *on;
          When @@mode = 'C';
             chgMod  = *on;
          Endsl;
       Endif;

       apShowMenu = *on;

       aMenHeight = 145;

       apFootHght = aMenHeight + 125;

       // Set Menu options
       scMenuChoi = 'Primary Agency Information';
       scMenuValu = 'WTAGTPRI';

       scMenuChoi = %trim(scMenuChoi) + ',Group Assignments';
       scMenuValu = %trim(scMenuValu) + ',WTAGTASN';

       scMenuChoi = %trim(scMenuChoi) + ',Producers';
       scMenuValu = %trim(scMenuValu) + ',WTAGTPROD';

       scMenuChoi = %trim(scMenuChoi) + ',Contacts';
       scMenuValu = %trim(scMenuValu) + ',WTAGTCON';

       scMenuChoi = %trim(scMenuChoi) + ',Service Request';
       scMenuValu = %trim(scMenuValu) + ',WTAGTSRVRT';

       scMenuChoi = %trim(scMenuChoi) + ',Return Funds';
       scMenuValu = %trim(scMenuValu) + ',WTAGTRTFND';

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
       // Update Action Panel Info (Rrn, Agency Name)
        scActRrn = xxRrn;
        scCompName = %trim(AGname);
        If scCompName = ' ';
           Chain intCo# pco00;
           If COname <> ' ';
              scCompName = COname;
           Endif;
        Endif;

       scCo# = intCo#;

       processMe = *on;

       strTabNames += AddToList('Group Assignments':arrTabNames:n1);
       strTabNames += AddToList('Quote Tracking':arrTabNames:n1);

       xsKey = 'WC';
       Chain xsKey SWC01;
       If WCfl24 = 'Y' Or WCfl38 = 'Y';
          strTabNames += AddToList('Payment Information':arrTabNames:n1);
       EndIf;

       If IsModEnabled('Portal Module');
          strTabNames += AddToList('Connect':arrTabNames:n1);
       EndIf;
       TbNmRcdPnl = strTabNames;

       Return;

      /end-free
     p ProgramStart    e

       //===================================================================
       // Set the value of scChoice
       //===================================================================
     pSetScChoice      b
     dSetScChoice      pi                  like(scNewStsCH)
     d inCo#                               like(QTco#)
     d inNewRenew                     1    const options(*nopass)
     d inQuote                        1    const options(*nopass)
     d inExcludeXFer                  1n   const options(*nopass)
     d*
     dflgNewRenew      s              1    inz
     dflgQuote         s              1    inz('W')
     disExcludeXFer    s              1n   inz
     dchoiceSelect     s                   like(scNewStsCH)

      /free
       If %parms >= 2;
          flgNewRenew = inNewRenew;
       Endif;
       If %parms >= 3;
          flgQuote = inQuote;
       Endif;
       If %parms >= 4;
          isExcludeXFer = inExcludeXFer;
       Endif;

       choiceSelect = 'QSco# = ' + %char(ELco#);

       Select;
       When flgQuote = '*';
        // No selection criteria
       When flgQuote = 'Q';
          choiceSelect = %trim(choiceSelect) + ' and QSqte = ''Q''';
       Other;
          choiceSelect = %trim(choiceSelect) + ' and QSqte = ''W''';
          flgNewRenew = '*';
       Endsl;

       Select;
       When flgNewRenew = '*';
        // No selection criteria
       When flgNewRenew = 'R';
          choiceSelect = %trim(choiceSelect) + ' and QSrenew = ''Y''';
       When flgNewRenew = 'N';
          choiceSelect = %trim(choiceSelect) + ' and QSrenew = '' ''';
       Endsl;

       Select;
       When isExcludeXFer;
          choiceSelect = %trim(choiceSelect) +
       //    ' and Upper(QSkey) <> ''T''' +
           ' and Upper(QSkey) <> ''X''';
       Endsl;

       Return choiceSelect;

      /end-free
     PSetScChoice      e

       //===================================================================
       // Validate Group Assignments Info
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
       //===================================================================
     p ValidateAsn     b
     d ValidateAsn     pi              n
     d strmode                        1
     d isErrAsnEdt     s               n   inz
      /Free

       isErrAsnEdt = *off;
       // Intialize all the messageId and error indicators.
       erFndAG = *off;
       msFndAG = *blanks;
       erAgt#AG = *off;
       msAgt#AG = *blanks;
       erPortDft = *off;
       msPortDft = *blanks;
       erEfDtAG = *off;
       msEfDtAG = *blanks;
       erCnDtAG = *off;
       msCnDtAG = *blanks;
       erCmmtAG = *off;
       msCmmtAG = *blanks;
       erCmDtAG = *off;
       msCmDtAG = *blanks;
       //*** Beg Add *** 07/26/23 ******************************************
       erDrt#AG = *off;
       msDrt#AG = *blanks;
       //*** End Add *** 07/26/23 ******************************************
       erExDtAG = *off;
       msExDtAG = *blanks;

       If strmode = 'A';                //Add Mode
          // Group cannot be Zero
          //*** Beg Chg *** 07/24/23 ******************************************
          //*** If scFndAG = 0;
          If scFndAG = 0 and scCopyX <> 'Y';
          //*** End Chg *** 07/24/23 ******************************************
             erFndAG = *on;
             msFndAG = 'WCR0502';
             isErrAsnEdt = *on;
          Endif;

          // Check if FEIN/MOD has not previously assigned to this Group
          If isErrAsnEdt = *off;
             Chain (inCo#:inFein:inMod:scFndAg) paa00;
             If %found(wmaal);
                erFndAG = *on;
                msFndAG = 'WC30005';
                isErrAsnEdt = *on;
             Endif;
          Endif;
       Endif;

       // Agent Number Assigned by Group cannot be zero
       If scAgt#AG = 0;
          erAgt#AG = *on;
          msAgt#AG = 'WCR0502';
          isErrAsnEdt = *on;
       Endif;

       // Check If Agent Number already assigned to another FEIN
       If isErrAsnEdt = *off and scAgt#AG <> 0;
          Chain (inCo#:scFndAG:scAgt#AG) paa02;
          If %found(wmaal1);
             If a2_AAfein <> scFeinAG or
              (a2_AAfein = scFeinAG and a2_AAmod <> scModAG);
                If a2_AAstat = 'A';
                   erAgt#AG = *on;
                   msAgt#AG = 'WCR0533';
                   isErrAsnEdt = *on;
                Endif;
             Endif;
          Endif;
       Endif;

       // If Changed the Agent Number Assigned by Group
       // Check If any active accounts exists for this agent
       If isErrAsnEdt = *off and scAgt#AG <> savagt# and scAgt#AG <> 0;
          Setll (inCo#:scFndAG:savagt#) pah03;
          Reade (inCo#:scFndAG:savagt#) pah03;
          Dow not %eof(wmahl3);
             If AHstat = 'A';
                erAgt#AG = *on;
                msAgt#AG = 'WCR1016';
                isErrAsnEdt = *on;
                Leave;
             Endif;
             Reade (inCo#:scFndAG:savagt#) pah03;
          Enddo;
       Endif;

       // Check for portal default flag set for another group
       erPortDft = *off;
       //*** Beg Chg *** 07/24/23 ******************************************
       //*** If isErrAsnEdt = *off and scPortDft = 'Y';
       If isErrAsnEdt = *off and scPortDft = 'Y'
        and scCopyX <> 'Y';
       //*** End Chg *** 07/24/23 ******************************************
          Reset decFnd;
          Exec sql
           select AAfnd into :decFnd
           from WMAAP
            where AAco# = :scCo#
             and AAfein = :scFeinAG
             and AAmod = :scModAG
             and AAfnd <> :scFndAG
             and AAportDft = 'Y';
          If decFnd <> 0;
             isErrAsnEdt = *on;
             erPortDft = *on;
             msPortDft = 'Group ' + %editc(decFnd:'Z') +
              ' is already defined as the default group.';
          Endif;
       Endif;

       // Validation Check for Effective Date
       If scEfDtAG <> *Zeros;
          erEfDtAG = ValidDate(scEfDtAG:charMsg);
          If erEfDtAG = *on;
             msEfDtAG = charMsg;
             isErrAsnEdt = *on;
          Endif;
       Endif;

       // Validation Check for Cancellation Date
       If scCnDtAG <> *Zeros;
          erCnDtAG = ValidDate(scCnDtAG:charMsg);
          If erCnDtAG = *on;
             msCnDtAG = charMsg;
             isErrAsnEdt = *on;
          Endif;
       Endif;

       // Dates must be in Ascending order
       If scEfDtAG <> 0 and scCnDtAG <> 0;
          If erEfDtAG = *off and erCnDtAG = *off;
             erCnDtAG = AscendDates(scEfDtAG:scCnDtAG:charMsg);
             If erCnDtAG = *on;
                erEfDtAG = *on;
                msCnDtAG = charMsg;
                msEfDtAG = charMsg;
                isErrAsnEdt = *on;
             Endif;
          Endif;
       Endif;

       // Do not allow user to change the Dates
       // If any account exists outside of the new Date Range
       If isErrAsnEdt = *off and (scEfDtAG<>savEfDtAG or scCnDtAG<>savCnDtAG);
          Setll (inCo#:scFndAG:scAgt#AG) pah03;
          Reade (inCo#:scFndAG:scAgt#AG) pah03;
          Dow not %eof(wmahl3);
             If AHstat = 'A';
                If dtCYMD(scEfDtAG) > AHefdt or
                 (dtCYMD(scCnDtAG) < AHefdt and scCnDtAG <> 0);
                   erCnDtAG = *on;
                   erEfDtAG = *on;
                   msCnDtAG = 'WC30139';
                   msEfDtAG = 'WC30139';
                   isErrAsnEdt = *on;
                   Leave;
                Endif;
             Endif;
             Reade (inCo#:scFndAG:scAgt#AG) pah03;
          Enddo;
       Endif;

       // Commitment Amount and Date are Mutually Inclusive
       If (scCmmtAG = *Zeros and scCmDtAG <> *Zeros) or
        (scCmmtAG <> *Zeros and scCmDtAG = *Zeros);
          erCmmtAG = *on;
          msCmmtAG = 'WCR0544';
          erCmDtAG = *on;
          msCmDtAG = 'WCR0544';
          isErrAsnEdt = *on;
       Endif;

       // Validation Check for Commitment Date
       If scCmDtAG <> *Zeros and erCmDtAG = *off;
          erCmDtAG = ValidDate(scCmDtAG:charMsg);
          If erCmDtAG = *on;
             msCmDtAG = charMsg;
             isErrAsnEdt = *on;
          Endif;
       Endif;

       // Validation Check for Default Rate Expiration Date
       If scExDtAG <> *Zeros;
          erExDtAG = ValidDate(scExDtAG:charMsg);
          If erExDtAG = *on;
             msExDtAG = charMsg;
             isErrAsnEdt = *on;
          Endif;

          //*** Beg Add *** 07/26/23 ******************************************
          // Default Rate # and Default Rate Expiration Date are Mutually Inclusive
          If (scDrt#AG = *Zeros and erExDtAG = *off);
             erDrt#AG = *on;
             msDrt#AG = 'WCR0544';
             erExDtAG = *on;
             msExDtAG = 'WCR0544';
             isErrAsnEdt = *on;
          Endif;
          //*** End Add *** 07/26/23 ******************************************
       Endif;


       // Return *on indicator when No error exists.
       If isErrAsnEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateAsn     e

       //===================================================================
       // Validate EFT Info
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
       //===================================================================
     p ValidateEft     b
     d ValidateEft     pi              n
     d strmode                        1
     d isErrEftEdt     s               n   inz
      /Free

       isErrEftEdt = *off;
       // Intialize all the messageId and error indicators.
       erFndAE = *off;
       msFndAE = *blanks;
       erEfDtAE = *off;
       msEfDtAE = *blanks;
       erExDtAE = *off;
       msExDtAE = *blanks;
       erPDtAE  = *off;
       msPDtAE  = *blanks;
       erADtAE  = *off;
       msADtAE  = *blanks;
       erFtDtAE = *off;
       msFtDtAE = *blanks;
       erLtDtAE = *off;
       msLtDtAE = *blanks;

       If strmode = 'A';                //Add Mode
          // Group cannot be Zero
          If scFndAE = 0;
             erFndAE = *on;
             msFndAE = 'WCR0502';
             isErrEftEdt = *on;
          Endif;
       Endif;

       // Validation Check for Effective Date
       erEfDtAE = ValidDate(scEfDtAE:charMsg);
       If erEfDtAE = *on;
          msEfDtAE = charMsg;
          isErrEftEdt = *on;
       Endif;

       // Effective Date can not be less than system date
       If erEfDtAE = *off and dtCYMD(scEfDtAE) <> EFefdt;
          If dtCYMD(scEfDtAE) < Dateto7(SystemDate());
             erEfDtAE = *on;
             msEfDtAE = 'WCR3210';
             isErrEftEdt = *on;
          Endif;
       Endif;

       // Validation Check for Expiration Date
       If scExDtAE <> *Zeros;
          erExDtAE = ValidDate(scExDtAE:charMsg);
          If erExDtAE = *on;
             msExDtAE = charMsg;
             isErrEftEdt = *on;
          Endif;
       Endif;

       // Expiration Date can not be less than system date
       If erExDtAE = *off and dtCYMD(scExDtAE) <> EFexdt;
          If dtCYMD(scExDtAE) < Dateto7(SystemDate());
             erExDtAE = *on;
             msExDtAE = 'WCR3210';
             isErrEftEdt = *on;
          Endif;
       Endif;

       // Dates must be in Ascending order
       If scEfDtAE <> 0 and scExDtAE <> 0;
          If erEfDtAE = *off and erExDtAE = *off;
             erExDtAE = AscendDates(scEfDtAE:scExDtAE:charMsg);
             If erExDtAE = *on;
                erEfDtAE = *on;
                msExDtAE = charMsg;
                msEfDtAE = charMsg;
                isErrEftEdt = *on;
             Endif;
          Endif;
       Endif;

       // Check if - Date range overlaps previous record
       If scEfDtAE <> 0 and scExDtAE <> 0;
          kktype = 'A';
          kkkey  = %char(AAagt#);
          Setll keyWMEFL7 pef07;
          Reade keyWMEFL7 pef07;
          Dow not %eof(wmefl7);
             If strmode = 'A';                //Add Mode
                If (l7EFefdt >= dtCYMD(scEfDtAE) and l7EFefdt <> l7EFexdt)
                 OR ( (l7EFefdt < dtCYMD(scEfDtAE)) and
                 (l7EFexdt = 0 or l7EFexdt > dtCYMD(scEfDtAE)) );
                   erExDtAE = *on;
                   erEfDtAE = *on;
                   msExDtAE = 'WC30297';
                   msEfDtAE = 'WC30297';
                   isErrEftEdt = *on;
                   Leave;
                Endif;
             Else;                            //Edit Mode
                If (dtCYMD(scEfDtAE)>l7EFefdt and (dtCYMD(scEfDtAE)<l7EFexdt))
                 OR ( (dtCYMD(scEfDtAE) <= l7EFefdt) and
                 (dtCYMD(scExDtAE) > l7EFefdt or dtCYMD(scExDtAE) = 0) );
                   erExDtAE = *on;
                   erEfDtAE = *on;
                   msExDtAE = 'WC30297';
                   msEfDtAE = 'WC30297';
                   isErrEftEdt = *on;
                   Leave;
                Endif;
             Endif;
             Reade keyWMEFL7 pef07;
          Enddo;
       Endif;

       // Validation Check for Pre-notification Date
       If scPDtAE <> *Zeros;
          erPDtAE = ValidDate(scPDtAE:charMsg);
          If erPDtAE = *on;
             msPDtAE = charMsg;
             isErrEftEdt = *on;
          Endif;
       Endif;

       // Pre-notification Date can not be less than system date
       If erPDtAE = *off and dtCYMD(scPDtAE) <> EFPdt;
          If dtCYMD(scPDtAE) < Dateto7(SystemDate());
             erPDtAE = *on;
             msPDtAE = 'WCR3210';
             isErrEftEdt = *on;
          Endif;
       Endif;

       // Validation Check for Approved Date
       If scADtAE <> *Zeros;
          erADtAE = ValidDate(scADtAE:charMsg);
          If erADtAE = *on;
             msADtAE = charMsg;
             isErrEftEdt = *on;
          Endif;
       Endif;

       // Approved Date can not be less than system date
       If erADtAE = *off and dtCYMD(scADtAE) <> EFAdt;
          If dtCYMD(scADtAE) < Dateto7(SystemDate());
             erADtAE = *on;
             msADtAE = 'WCR3210';
             isErrEftEdt = *on;
          Endif;
       Endif;

       // Pre-note date should be non-zero for approval date to be entered
       If erADtAE = *off and scADtAE <> *Zeros;
          If scADtAE = *Zeros;
             If scADtAE < Dateto7(SystemDate());
                erADtAE = *on;
                msADtAE = 'WCR3317';
                isErrEftEdt = *on;
             Endif;
          Endif;
       Endif;

       // Validation Check for Date of First Transfer
       If scFtDtAE <> *Zeros;
          erFtDtAE = ValidDate(scFtDtAE:charMsg);
          If erFtDtAE = *on;
             msFtDtAE = charMsg;
             isErrEftEdt = *on;
          Endif;
       Endif;

       // Validation Check for Date of Last Transfer
       If scLtDtAE <> *Zeros;
          erLtDtAE = ValidDate(scLtDtAE:charMsg);
          If erLtDtAE = *on;
             msLtDtAE = charMsg;
             isErrEftEdt = *on;
          Endif;
       Endif;


       // Return *on indicator when No error exists.
       If isErrEftEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateEft     e

       //===================================================================
       // Validate Quote Tracking Info
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
       //===================================================================
     p ValidateQte     b
     d ValidateQte     pi              n
     d strmode                        1
     d isErrQteEdt     s               n   inz
      /Free

       isErrQteEdt = *off;
       // Intialize all the messageId and error indicators.
       erEdtChgDt = *off;
       msEdtChgDt = *blanks;
       erEdtEffDt = *off;
       msEdtEffDt = *blanks;
       erEdtSubDt = *off;
       msEdtSubDt = *blanks;
       erEdtSntDt = *off;
       msEdtSntDt = *blanks;
       erEdtBusDt = *off;
       msEdtBusDt = *blanks;

       // Validation Check for Status Change Date
       If scEdtChgDt <> *Zeros;
          erEdtChgDt = ValidDate(scEdtChgDt:charMsg);
          If erEdtChgDt = *on;
             msEdtChgDt = charMsg;
             isErrQteEdt = *on;
          Endif;
       Endif;

       // Validation Check for Policy Effective Date
       If scEdtEffDt <> *Zeros;
          erEdtEffDt = ValidDate(scEdtEffDt:charMsg);
          If erEdtEffDt = *on;
             msEdtEffDt = charMsg;
             isErrQteEdt = *on;
          Endif;
       Endif;

       // Validation Check for Original Submission Date
       If scEdtSubDt <> *Zeros;
          erEdtSubDt = ValidDate(scEdtSubDt:charMsg);
          If erEdtSubDt = *on;
             msEdtSubDt = charMsg;
             isErrQteEdt = *on;
          Endif;
       Endif;

       // Validation Check for Quote Sent Date
       If scEdtSntDt <> *Zeros;
          erEdtSntDt = ValidDate(scEdtSntDt:charMsg);
          If erEdtSntDt = *on;
             msEdtSntDt = charMsg;
             isErrQteEdt = *on;
          Endif;
       Endif;

       // Validation Check for New Business Date
       If scEdtBusDt <> *Zeros;
          erEdtBusDt = ValidDate(scEdtBusDt:charMsg);
          If erEdtBusDt = *on;
             msEdtBusDt = charMsg;
             isErrQteEdt = *on;
          Endif;
       Endif;


       // Return *on indicator when No error exists.
       If isErrQteEdt = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateQte     e

       //===================================================================
       // Validate Quote Tracking Status Selection
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
       //===================================================================
     p ValidateSts     b
     d ValidateSts     pi              n
     d strmode                        1
     d isValid         s               n   inz
      /Free

       isValid  = *off;
       erNewSts = *off;
       msNewSts = *Blanks;
       erNewAgt = *off;
       msNewAgt = *Blanks;

       Select;
       When Upper(scNewSts) ='T';
          If Upper(QTtype) <> 'W' and Upper(QTtype) <> 'P' and
           Upper(QTtype) <> 'Q' and Upper(QTtype) <> 'B' and
           Upper(QTtype) <> 'M' and Upper(QTtype) <> 'T';
             msNewSts = 'WC30141';
             erNewSts = *on;
             isValid  = *on;
          Endif;

       // Check for prior Cancellation/Reinstatement
       When Upper(scNewSts) ='C' or Upper(scNewSts) ='R';

          // Read all WAQTP records for this employer, if the last recorded entry
          // is 'C'/'R' (regardless of the agent number assigned to the WAQTP
          // record) then the current status is Cancelled/Reinstated.
          Clear intWkDt;
          Clear TempTyp;
          Unlock WAQTP;
          Setll KeyWAQTL pqt12;
          Reade KeyWAQTL pqt12;
          Dow not %eof(WAQTL12);
             If Upper(QTtype) = 'C' or Upper(QTtype) = 'R' or
              Upper(QTtype) = 'W' or Upper(QTtype) = 'T';
                If QTwkdt > intWkDt;
                   Clear intWkDt;
                   TempTyp = Upper(QTtype);
                   intWkDt = QTwkdt;
                Endif;
             Endif;
             Reade KeyWAQTL pqt12;
          Enddo;
          If (TempTyp = 'C' and Upper(scNewSts) = 'C') or
           (TempTyp = 'R' and Upper(scNewSts) = 'R');
             msNewSts = 'WC30086';
             erNewSts = *on;
             isValid  = *on;
          Endif;

          If isValid = *off and TempTyp <> 'C' and Upper(scNewSts) = 'R';
             msNewSts = 'WC30089';
             erNewSts = *on;
             isValid  = *on;
          Endif;
          Chain(n) scQteTrRrn pqt0p;

       Endsl;

       // Validate New Agency Number, it can't be same as current agency number
       If Upper(scNewSts) = 'T' and scNewAgt <> 0;
          If scNewAgt = QTagt#;
             msNewAgt = 'WC30005';
             erNewAgt = *on;
             isValid  = *on;
          Endif;
       Endif;


       // Return *on indicator when No error exists.
       If isValid = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateSts     e

      // utility procedure definitions
      /COPY SCOPYCOMPI

