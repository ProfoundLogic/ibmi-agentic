     hCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/title W7944RCS - CompSuite Group Select
     h/define   PROFOUNDUI
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program Will Allow user to select groups to process
     h*
     h*  Called from: W7944R
     h*  Calls to:    Many
     h*
     h**********************************************************************
     h*  Created on 03/01/2014 b5 C:062 for Project: 114WGU
     h**********************************************************************
     h*  Revisions:                                                        *
     h*                                                                    *
     h*  03/06/25 C:333  Project WC2998                                    *
     h*                  Changes to CMS Settlement Reporting.              *
     h*                  - Recompiled.                                     *
     h*                                                                    *
     h*  17/01/24 C:342 Project: WC2944                                    *
     h*                 Group Select - Dup Buttons at Top of Screen        *
     h*                 DSPF changes:                                      *
     h*                  1.Added Include All, Exclude All, Cancel, Submit  *
     h*                    button at top of the grid as well.              *
     h*                                                                    *
     h*  03/24/22 C:333 Project: SY434                                     *
     h*                 - Update Grids for Sorting Filtering Upgrade       *
     h*                                                                    *
     h*  04/28/20 C:079 Support: 146/88598                                 *
     h*                 Add edit for IDC NCCI group carrier code missing   *
     h*                                                                    *
     h*  08/19/19 C:079 Project: WC2604                                    *
     h*                 Add NCCI Indemnity Data Call                       *
     h*                                                                    *
     h*  01/22/19 C:079 Support: 133/75765                                 *
     h*                 Add a file name to %eof on ReadC                   *
     h*                 Add logical file over wdd1p to speed up process    *
     h*                                                                    *
     h*  09/20/16 C:307 Support: 43403                                     *
     h*                 To correct check/uncheck issue with group select   *
     h*                                                                    *
     h*  04/22/16 C:062 Support: 40871(65)
     h*                 Correct group restrictions.
     h*                                                                    *
     h*    01/07/16 C:65  Project: WC2401                                  *
     h*                   Recompile Only                                   *
     h*                                                                    *
     h*  11/27/15 C:308 Support: 38426                                     *
     h*                 Allow default group to group select screen if group*
     h*                 restriction is 'Y'in user parameters setup.        *
     h*                                                                    *
      *  09/17/15 C:302 Project: wc2254                                    *
      *                 Recompile only                                     *
     h*                                                                    *
     H* 09/11/15 C:295 PROJECT: WC2362
     H*                Include Transaction ID to Group Selection
     h*
     H* 07/20/15 C:295 36111
     H*                WTGRPSLCT needs to have the new/old edit on the
     H*               -same carrier # changed - GSLIKECAR
     h*
     H* 06/23/15 C:33  35519
     H*               -Screen changes.
     H*               -RRE filter selections.
     h*
     H* 06/10/15 C:280 PROJECT: WC2261
     H*                Change verbage on submit window
     h*                remove scsure button
     h*
     h*  10/14/14  C:279 Project: 114WNC
     h*             :295 Added processing for check schedule maintenance.
     h*                  as per this new group select program.
     h*
     h**********************************************************************
     fwtgrpslct cf   e             workstn extfile('WTGRPSLCT')
     f                                     Sfile(sflGrpGrid:rrnGrpSlct)
     f                                     handler('PROFOUNDUI(HANDLER)')
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Include(rcdComPnl)

     fwtgsp     if   e           k Disk    ReName(pgs01:pgs0p)
     fwxgsl     uf   e           k Disk    ReName(pgs01:pgs00)
     fwdf2l     if   e           k Disk    ReName(pf201:pf200)
     Fwmd1l     if   e           k disk    ReName(pd101:pd100)
     F*** Beg Chg *** 01/22/19 *******************************************
     F*** wdd1l  uf   e           k disk    ReName(pd101:pd200) Prefix(d2:2)
     Fwdd1l5    uf   e           k disk    ReName(pd101:pd200) Prefix(d2:2)
     F*** End Chg *** 01/22/19 *******************************************
     F*** Beg Add *** 01/22/19 *******************************************
     Fwdd1l6    uf   e           k disk    ReName(pd101:pd300) Prefix(d3:2)
     F*** End Add *** 01/22/19 *******************************************
     Fwmcxl     if   e           k disk    Rename(pcx01:pcx00)
      ***** Beg Add ***** 11/27/15 **************************************
     Fwmupl     if   e           k disk    Rename(pup01:pup00)
     Fsmazl     if   e           k disk    Rename(paz01:paz00)
     d strPass         S              1A   inz('N')
      ***** Beg End ***** 11/27/15 **************************************

     d psds          esds                  extname(szq1p)

      * constant
     d intFlgDim       c                   10
     d clientID        s              3    inz('000')
     d low             c                   'abcdefghijklmnopqrstuvwxyz'
     d up              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D***  Begin Add  ***  06/23/15  **************************************
     d errorMsgTitle   c                   '*******  Error Message *******'
     D***  End   Add  ***  06/23/15  **************************************

     D***  Beg Add  ***  07/20/15  **************************************
     d InforMsgTitle   c                   '******* Information Message *******'
     d strNccimsg      s            256    inz
     d strInfmsg       s            256    inz
     D***  End Add  ***  07/20/15  **************************************

     d* Program to be called Plist 0
     d CallProgram0    pr                  ExtPgm(GSpPgm)

     d* Program to be called Plist 1
     d CallProgram1    pr                  ExtPgm(GSpPgm)
     d   inKC                         1
     d   inKL                         1

     d* Program to be called Plist 2
     d CallProgram2    pr                  ExtPgm(GSpPgm)
     d   inPrDs                      40
     d   inCall                       1
     d   inKC                         1
     d   inKL                         1

     d* Program to be called Plist 3
     d CallProgram3    pr                  ExtPgm(GSpPgm)
     d   inPrDs                      40
     d   inCall                       1

     d* Program to be called Plist 4
     d CallProgram4    pr                  ExtPgm(GSpPgm)
     d   inPrDs                      40

     d* Program to be called Plist 5
     d CallProgram5    pr                  ExtPgm(GSpPgm)
     d   inKC                         1
     d   inKL                         1
     d   inCall                       1
     d   inRrn                        9  0

      ***** beg add ******* 10/14/14 *******************************
     d VldFund1        pr                  extPgm('WTCOMCHK01')
     d ##errFnd                            like(##errFnd)

     d ValidFnd        pr                  extPgm('WTCOMCHK02')
     d ##errFnd                            like(##errFnd)

     d ##errFnd        s              1
      ***** end add ******* 10/14/14 *******************************

      ***** Beg add ******* 09/11/15 *******************************
     d wa099c          pr                  extpgm('WA099C')
     d inCo#                          3    Const
     d inGroup#                       3    Const
     d outChgDt                       7
     d out#ofRcds                    10  0

     d outChgDt        s              7
     d out#ofRcds      s             10  0
      ***** End add ******* 09/11/15 *******************************

     d* Check to See If Status can be changed
     d ChangeStatus    pr              n

     d* Check to See If Exclude/Include Option Used
     d ChkExcIncOpt    pr

     d* Check for Additional Submission Criteria
     d ChkPreSubmit    pr

     d* Check Re-Submit
     d ChkReSubmit     pr              n

     d* Check EDI Security
     d ChkSecurity     pr              n
     d  groupIn                       3  0

     d* Clear Error Indicators
     d ClearErrors     pr

     d* Clear Filter
     d ClearFilter     pr

     d* Error Check
     d ErrorCheck      pr              n

     d* Filter Groups Listed
     d FilterGroup     pr

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

     d* Load Group Select Grid
     d LoadGrpGrid     pr

     d* Main Processing
     d Main            pr

     d* Process Submission
     d ProcessSubmit   pr

     d* Program End
     d ProgramEnd      pr

     d* Program Start
     d ProgramStart    pr

     d* Alerts User if Cancelling and Resubmit set
     d ReSubmitAlert   pr

     d* Subset
     d Subset          pr              n

     d* Update WXGSP File
     d UpdWxgspSts     pr
     d  status                        1    Const

     d* User Security
     d UserSecurity    pr                  ExtPgm('SRCCSC')
     d   inSRuTyp                     1
     d   inSRuLvl                     3
     d   inSRcTyp                     1
     d   inSRcLvl                     3
     d   inSRexec                     1
     d   inSRres                      1
     d   inSRcTyp2                    1
     d   inSRcTyp3                    1
     d   inSRcTyp4                    1
     d   inSRcTyp5                    1
     d   inSRcTyp6                    1

     d/copy wPrGroup
     d/copy sPrDate
     d***  Begin Add  ***  06/23/15  **************************************
     d/copy sPrGenPop
     d***  End   Add  ***  06/23/15  **************************************
     d/copy sPrString
     d/copy SPrCompany
     d/copy wPrUsrPrms

     D intCo#          s              3  0
     d ##CltId         s              3
     d @@call          s              1
     d @@pgm           s             10
     d reSubmit        s               n   Inz(*Off)
     d errorFound      s               n   Inz(*Off)
     d gSelectFnd      s               n   Inz(*Off)

     d***  Begin Add  ***  06/23/15  **************************************
     d dsMemberDta     ds                  dtaara('W7944DTA')
     d  charMember                   10
     d***  End   Add  ***  06/23/15  **************************************

     C     *Entry        Plist
     C                   Parm                    @@call
     C                   Parm                    @@pgm

      /free

       // Main Procesing
       Main();

       *inlr = *on;

      /end-free
       //===================================================================
       // ChangeStatus - Check to see if status can be changed
       //===================================================================
     p ChangeStatus    b
     d ChangeStatus    pi              n

      /free

       If @@Pgm = 'W7480R';
          SetGT (intCo#:GSfnd) pd100;
          ReadPE (intCo#:GSfnd) pd100;
          If Not %Eof(WMD1L) And D1rsDt = 0;

             // Check to see if user has authority to change status
             Return ChkSecurity(GSfnd);
          EndIf;
       EndIf;

       Return *On;

      /end-free

     p ChangeStatus    e
       //===================================================================
       // ChkExcIncOpt - Check to see if include/exclude option used
       //===================================================================
     p ChkExcIncOpt    b
     d ChkExcIncOpt    pi

      /free

       // Initialize
       scIncExcR = *Off;

       //***  Begin Add  ***  06/23/15  ***********************************
       //*** Beg Chg *** 01/22/19 *****************************************
       //***** Dou %eof;
       Dou %eof(WTGRPSLCT);
       //*** End Chg *** 01/22/19 *****************************************
       //***  End   Add  ***  06/23/15  ***********************************

       // Read Subfile
       ReadC sflGrpGrid;

       //***  Begin Add  ***  06/23/15  ***********************************
       //*** Beg Chg *** 01/22/19 *****************************************
       //**** If not %eof;
       If not %eof(WTGRPSLCT);
       //*** End Chg *** 01/22/19 *****************************************
       //***  End   Add  ***  06/23/15  ***********************************

          // See If Exclude/Include Option is being Used
       // See If Exclude/Include Option is being Used
       //***  Begin Chg  ***  06/23/15  ***********************************
       //***If scIncExcR = *On;
       If scIncExcR = *On or scIEflC = *on;
       //***  End   Chg  ***  06/23/15  ***********************************
          Chain (intCo#:scGroup#) pgs00;
          If %Found(WXGSL);

             // Check to see if Resubmit
       //***  Begin Chg  ***  09/20/16  ***********************************
       //    If ChkReSubmit() = *On;
       //       Select;
       //       When GSieFl = 'I';
       //          GSieFl = 'E';
       //       When GSieFl = 'E';
       //          GSieFl = 'I';
       //       EndSl;
       //    EndIf;

             If ChkReSubmit() = *On;
                Select;
                When scieFl = 'I';
                   GSieFl = 'I';
                When scieFl = 'E';
                   GSieFl = 'E';
                EndSl;
             EndIf;
       //***  End   Chg  ***  09/20/16  ***********************************
          Update pgs00;
          EndIf;
       EndIf;
       //***  Begin Add  ***  06/23/15  ***********************************
       EndIf;
       Enddo;
       //***  End   Add  ***  06/23/15  ***********************************

      /end-free

     p ChkExcIncOpt    e
       //===================================================================
       // ChkPreSubmit - Check to see if Additional Submision Criteria Met
       //===================================================================
     p ChkPreSubmit    b
     d ChkPreSubmit    pi

     d errorRRE        s               n   Inz(*Off)
     d holdRRE         s                   like(GSrre#) Inz(0)
     d*** Beg Add *** 08/19/19 ********************************************
     d holdNcarr       s                   like(GSnCarGpCd) Inz
     d errorNcarr      s               n   Inz(*Off)
     d errorNCCIrqd    s               n   Inz(*Off)
     d*** End Add *** 08/19/19 ********************************************
     d*** Beg Add *** 04/28/20 ********************************************
     d errorNCCIGrpRq  s               n   Inz(*Off)
     d*** End Add *** 04/28/20 ********************************************
     d***  Begin Add  ***  06/23/15  **************************************
     d errorNcci       s               n   Inz(*Off)
     d holdNcci        s                   like(GSncci) inz
     d***  Begin Add  ***  06/23/15  **************************************
     d***  Begin Add  ***  09/11/15  **************************************
     d errorTcc        s               n   Inz(*Off)
     d holdTcc         s                   like(GStcc) inz
     d***  Begin Add  ***  09/11/15  **************************************
     d gsFound         s               n   Inz(*Off)
     d first           s               n   Inz(*On)
     d****** beg add ******* 10/14/14 ***************************************
     d errorSel        s               n   Inz(*Off)
     d****** end add ******* 10/14/14 ***************************************

      /free

       // Determine If any items were included and all same RRE or Carrier #
       Chain(n) intCo# pgs00;
       If %Found(WXGSL);
          DoU %Eof(WXGSL);
             If GSieFl = 'I';
                If first = *On;
                   holdRRE = GSrre#;
                   //***  Beg Add  ***  09/11/15  ***********************
                   holdTcc  = GSTcc;
                   //***  End Add  ***  09/11/15  ***********************
                   //***  Begin Add  ***  06/23/15  ***********************
                   holdNcci = GSncci;
                   //***  End   Add  ***  06/23/15  ***********************
                   //***  Begin Add  ***  08/19/19  ***********************
                   holdNcarr = GSnCarGpCd;
                   //***  End   Add  ***  08/19/19  ***********************
                   first = *Off;
                EndIf;
                gsFound = *On;
                If GSrre# <> holdRRE;
                   errorRRE = *On;
              //***  Begin Del  ***  06/23/15  ********************
              //***Leave;
              //***  End   Del  ***  06/23/15  ********************
                EndIf;
                //***  Begin Add  ***  06/23/15  ***********************
                If GSncci <> holdNcci;
                   errorNcci = *On;
                EndIf;
                //***  End   Add  ***  06/23/15  ***********************
                //***  Beg Add  ***  09/11/15  ***********************
                If GStcc <> holdTcc;
                   errorTcc = *on;
                EndIf;
                //***  End Add  ***  09/11/15  ***********************
                //***  Begin Add  ***  08/19/19  ***********************
                If GSnCarGpCd <> holdNcarr;
                   errorNcarr = *On;
                EndIf;
                If GSnCCI = *blanks or GSncci = *zeros;
                   errorNcciRqd = *On;
                EndIf;
                //***  End   Add  ***  08/19/19  ***********************
                //***  Beg   Add  ***  04/28/20  ***********************
                If GSnCarGpCd = *blanks or GSnCarGpCd = *zeros;
                   errorNcciGrpRq = *On;
                EndIf;
                //***  End   Add  ***  04/28/20  ***********************
             EndIf;
             ReadE intCo# pgs00;
          EndDo;
       EndIf;

       // Something has to be included to run this thing
       Select;
       When gsFound = *Off;
       //***  Begin Chg  ***  06/23/15  ***********************************
       //***ExFmt wdwNoItems;
          PopOneBtn('Unable to submit. No groups selected for submission.':
           errorMsgTitle);
       //***  End   Chg  ***  06/23/15  ***********************************
          scSubmit = *Off;
       // Make sure the same rre's are selected
       //***  Begin Chg  ***  06/23/15  ***********************************
       //**When (GSid = 'W7480R' Or GSid = 'W7480RE' Or GSid = 'W7488R') And
       //****errorRRE = *On;
       //****ExFmt wdwSameRRE;
       When GScRRE = 'Y' and GSlikeRRE = 'Y' and errorRRE = *On;
          PopOneBtn('The RRE ID does not match across all of the groups -
           that have been selected for inclusion.':
           errorMsgTitle);
       //***  End  'Chg  ***  06/23/15  ***********************************
          scSubmit = *Off;
       //***  Begin Add  ***  06/23/15  ***********************************
       // Make sure the same carrier #'s are selected
       //***  Beg   Chg  ***  07/20/15  ***********************************
       //*** When GScar# = 'Y' and GSlikeCar# = 'Y' and errorNcci = *On;
       //*** PopOneBtn('The NCCI Carrier # does not match across all of the -
       //*** groups that have been selected for inclusion.':
       //*** errorMsgTitle);
       When GScar# = 'Y' and GSlikeCar# <> *Blanks and errorNcci = *On;

       strNccimsg = 'The NCCI Carrier # does not match across all of the -
        groups that have been selected for inclusion.This will cause a data -
        rejection error unless specific approval from the receiving -
        bureau has been granted.';

       strInfmsg = 'The NCCI Carrier # does not match across all of the -
        groups that have been selected for inclusion.This will cause data -
        rejection error unless specific approval from the receiving -
        bureau has been granted.Press CONTINUE to continue or CANCEL to -
        go back.';

          If GSlikeCar#  ='Y';
             PopOneBtn(strNccimsg:errorMsgTitle);
             scSubmit = *Off;
          ELse;
             scSubmit = PopTwoBtn(strInfmsg:InforMsgTitle:'Continue':
              'Cancel');
          Endif;
       //***  End  'Chg  ***  06/23/15  ***********************************
       //***  scSubmit = *Off;
       //***  Eng   Chg  ***  07/20/15  ***********************************
       //********** beg add ******* 10/14/14 ***************************************
       When GSid = 'WTCOMCHK';
          ##errFnd = ' ';
          ValidFnd(##errFnd);

          If ##errFnd = 'Y';
             errorSel = *On;
          Else;
             VldFund1(##errFnd);
             If ##errFnd = 'Y';
                errorSel = *On;
             Endif;
          Endif;

          If errorSel = *on;
             scSubmit = *Off;
          Endif;
       //********** end add ******* 10/14/14 ***************************************
       //***  Beg Add  ***  09/11/15  ***********************************
       When GSdspTCC = 'Y'and GSlikeTCC = 'Y' and errorTCC = *On;
          PopOneBtn('The Transaction ID does not match across all of the -
                     groups that have been selected for -
                     inclusion.':errorMsgTitle);
          scSubmit = *Off;
       //***  End Add  ***  09/11/15  ***********************************
       //***  Beg Add  ***  08/19/19  ***********************************
       When GSnCarGrp= 'Y' and errorNcarr = *On;
          PopOneBtn('The NCCI Carrier Group Code does not match accross -
                     all of the groups that have been selected for -
                     inclusion.':errorMsgTitle);
          scSubmit = *Off;
       When GSnCarGrp= 'Y' and errorNcciRqd = *On;
          PopOneBtn('The NCCI Carrier Number is blank for one or -
                     more of the groups that have been selected for -
                     inclusion.':errorMsgTitle);
          scSubmit = *Off;
       //***  End Chg  ***  08/19/19  ***********************************
       //***  Beg Add  ***  04/28/20  ***********************************
       When GSnCarGrp= 'Y' and errorNcciGrpRq = *On;
          PopOneBtn('The IDC NCCI Carrier Group Code is blank for one or -
                     more of the groups that have been selected for -
                     inclusion.':errorMsgTitle);
          scSubmit = *Off;
       //***  End Add  ***  04/28/20  ***********************************
       EndSl;

       // Give Confirmation Window
       If GSid = 'W7480R' And scSubmit = *On;
       //***** Beg Chg ** 06/10/15 ***********************************
       //*****   DoU *INkl = *On Or scSure = 'N' Or scSure = 'Y';
       //*****      ExFmt wdwEdiSub;
       //*****      If scSure <> 'Y' And scSure <> 'N';
       //*****         *IN72 = *On;
       //*****      Else;
       //*****         *IN72 = *Off;
       //*****      EndIf;
       //*****   EndDo;

       //*****   If *INkl = *On Or scSure = 'N';
       //*****  scSubmit = *Off;
       //*****  EndIf;

          ExFmt wdwEdiSub;

          If *INkl = *On;
            scSubmit = *Off;
          EndIf;

        EndIf;
       //***** End Chg ** 06/10/15 ***********************************

      /end-free

     p ChkPreSubmit    e
       //===================================================================
       // ChkReSubmit - Check to see if include/exclude option used
       //===================================================================
     p ChkReSubmit     b
     d ChkReSubmit     pi              n

     d   inSRuTyp      s              1
     d   inSRuLvl      s              3
     d   inSRcTyp      s              1
     d   inSRcLvl      s              3
     d   inSRexec      s              1
     d   inSRres       s              1
     d   inSRcTyp2     s              1
     d   inSRcTyp3     s              1
     d   inSRcTyp4     s              1
     d   inSRcTyp5     s              1
     d   inSRcTyp6     s              1
     d   type          s              1
     d   authority     s              1

      /free

       resubmit = *Off;
       errorFound = *Off;

       If @@Pgm = 'W7480R';
          SetGT (intCo#:scGroup#) pd100;
          ReadPE (intCo#:scGroup#) pd100;
          If Not %Eof(WMD1L) And D1rsDt = 0;

             // Check to see if user has authority to change status
             If ChkSecurity(scGroup#) = *Off;
                authority = 'N';
                mgIncExcV = 'You lack authority to resubmit.';
                erIncExcV = *On;
                Update sflGrpGrid;
                errorFound = *On;
             Else;
                authority = 'Y';
                // set a flag to make sure that once a group for re-submission is selected,
                // F7 is pressed. If not, then we might have to clear re-submit flag in WDD1P
                // update recent most records with re-submission flag as Y
                Select;
                When GSieFl = 'E';
                   SetGT (intCo#:scGroup#) pd200;
                   ReadPE (intCo#:scGroup#) pd200;
                //*** Beg Chg *** 01/23/19 ********************************
                   //*** DoW Not %Eof(WDD1L);
                   //***   If D2rSbm = ' ' And D2rsDt = 0 And D2type <> 'Q';
                   DoW Not %Eof(WDD1L5);
                      If D2rSbm = ' ';
                //*** End Chg *** 01/23/19 ********************************
                         D2rSbm = 'Y';
                         resubmit = *On;
                         Update pd200;
                      EndIf;
                      ReadPE (intCo#:scGroup#) pd200;
                   EndDo;
                When GSieFl = 'I';
                   SetGT (intCo#:scGroup#) pd200;
                   ReadPE (intCo#:scGroup#) pd200;
                //*** Beg Chg *** 01/23/19 ********************************
                //*** DoW Not %Eof(WDD1L);
                //*** If D2rSbm = 'Y' And D2rsDt = 0 And D2type <> 'Q';
                   DoW Not %Eof(WDD1L5);
                      If D2rSbm = 'Y';
                //*** End Chg *** 01/23/19 ********************************
                         D2rSbm = ' ';
                         resubmit = *Off;
                         Update pd200;
                      EndIf;
                      ReadPE (intCo#:scGroup#) pd200;
                   EndDo;
                EndSl;
             EndIf;
             If authority = 'Y';
                Return *On;
             Else;
                Return *Off;
             EndIf;
          Else;
             Return *On;
          EndIf;
       EndIf;

       Return *On;

      /end-free

     p ChkResubmit     e
       //===================================================================
       // ChkSecurity - Check EDI Security
       //===================================================================
     p ChkSecurity     b
     d ChkSecurity     pi              n
     d  group#                        3  0

     d   inSRuTyp      s              1
     d   inSRuLvl      s              3
     d   inSRcTyp      s              1
     d   inSRcLvl      s              3
     d   inSRexec      s              1
     d   inSRres       s              1
     d   inSRcTyp2     s              1
     d   inSRcTyp3     s              1
     d   inSRcTyp4     s              1
     d   inSRcTyp5     s              1
     d   inSRcTyp6     s              1
     d   type          s              1

      /free

       Chain (intCo#:group#) pcx00;
       If %Found(WMCXL);
          inSRuLvl = %EditC(GetUsrLvl(q1user:type):'X');
          inSRuTyp = type;
          inSRcTyp = CXrbs1;
          inSRcLvl = %EditC(CXrbs:'X');
          inSRexec = '1';
          inSRres  = '1';
          inSRcTyp2 = CXrbs1;
          inSRcTyp3 = CXrbs2;
          inSRcTyp4 = CXrbs3;
          inSRcTyp5 = CXrbs4;
          inSRcTyp6 = CXrbs5;

          UserSecurity(inSRuTyp:inSRuLvl:inSRcTyp:inSRcLvl:inSRexec:
           inSRres:inSRcTyp2:inSRcTyp3:inSRcTyp4:inSRcTyp5:inSRcTyp6);

          If inSRres = '0';
             Return *On;
          Else;
             Return *Off;
          Endif;
       EndIf;

       Return *On;

      /end-free

     p ChkSecurity     e
       //===================================================================
       // ClearErrors - Clear Error Indicators
       //===================================================================
     p ClearErrors     b
     d ClearErrors     pi

      /free

       mgIncExcV = ' ';
       erIncExcV = *Off;

      /end-free

     p ClearErrors     e
       //===================================================================
       // ClearFilter - Clears Filter
       //===================================================================
     p ClearFilter     b
     d ClearFilter     pi

      /free

       ssGroup# = 0;
       ssGrpName = ' ';
       ssStatus = ' ';
       ssNCCI# = ' ';
       ssRRE# = ' ';
       noFilter = *On;

       //******* Beg Add *********** 09/11/15 ********************************
       Clear ssTCC;
       //******* End Add *********** 09/11/15 ********************************
      /end-free

     p ClearFilter     e
       //===================================================================
       // ErrorCheck - Error Check
       //===================================================================
     p ErrorCheck      b
     d ErrorCheck      pi              n

     D i               s              3  0
     D error           s               n   Inz(*Off)

     d***  Beg Add  ***  09/11/15  **************************************
     d intdat          s              7  0 Inz
     d ChgdatD         s               d   datfmt(*Iso)
     d***  End Add  ***  09/11/15  **************************************

      /free

       //******* Beg Add ****** 09/11/15*************************************
        Select;
        When GSdspTCC = 'Y' and GSreqTCC = 'Y' and scTCC = *blank;
          PopOneBtn('Group does not have an IRS transmittal code.-
           Can not include. If the group is to be included, then use the Work-
           with Groups menu option to change the IRS -
           transmitall code.':errorMsgTitle);
           error = *on;

        When @@pgm = '1099IRS' ;
           wa099c(%char(intCo#):%char(scGroup#):outChgDt:out#ofRcds);

           Select;
           When out#ofRcds = *zero;
           PopOneBtn('Group does not have current workfile information.-
            Can not include. If group is to be included, then use the Create -
            1099 Workfile menu option to create workfile information.-
            ':errorMsgTitle);
           error = *on;

           When CvtToDate(%dec(outChgDt:7:0)) <
                CvtToDate(DtPast(DtMDYY(%Dec(%Date():*cymd)):180));
           PopOneBtn('Group does not have current workfile information.-
            Can not include. If group is to be included, then use the Create -
            1099 Workfile menu option to create workfile information.-
            ':errorMsgTitle);
           error = *on;
           EndSl;
        EndSl;

       //******* End Add ****** 09/11/15*************************************

       Return error;

      /end-free

     p ErrorCheck      e
       //===================================================================
       // FilterGroup - Filter Group Listing
       //===================================================================
     p FilterGroup     b
     d FilterGroup     pi

     d error           s               n
     d hldGroup#       s                   like(ssGroup#)
     d hldGrpName      s                   like(ssGrpName)
     d hldStatus       s                   like(ssStatus)
     d hldNCCI#        s                   like(ssNCCI#)
     d hldRRE#         s                   like(ssRRE#)
     d hldTcc          s                   like(ssTcc)

      /free

       DoU cnlFilter = *On Or appFilter = *On;

          // Hold Original Values
          hldGroup# = ssGroup#;
          hldGrpName = ssGrpName;
          hldStatus = ssStatus;
          hldNCCI# = ssNCCI#;
          hldRRE# = ssRRE#;
          //****** Beg Add ****** 09/11/15********************************
          hldTCC  = ssTCC;
          //****** End Add ****** 09/11/15********************************

          ExFmt RcdGrpFltr;

          If cnlFilter = *On;
             // Restore Original Values
             ssGroup# = hldGroup#;
             ssGrpName = hldGrpName;
             ssStatus = hldStatus;
             ssNCCI# = hldNCCI#;
             ssRRE# = hldRRE#;
             //****** Beg Add ****** 09/11/15********************************
             ssTCC   = hldTCC;
             //****** End Add ****** 09/11/15********************************
          Else;
             Select;
             When ssGroup# <> 0;
                noFilter = *Off;
             When ssGrpName <> ' ';
                noFilter = *Off;
             When ssStatus <> ' ';
                noFilter = *Off;
             When ssNCCI# <> ' ';
                noFilter = *Off;
             When ssRRE# <> ' ';
                noFilter = *Off;
             //****** Beg Add ****** 09/11/15********************************
             When ssTCC <> ' ';
                noFilter = *Off;
             //****** End Add ****** 09/11/15********************************
             Other;
                noFilter = *On;
             EndSl;
          EndIf;

       EndDo;

      /end-free

     p FilterGroup     e
       //===================================================================
       // LoadGrpGrid - Load the Group Select Grid
       //===================================================================
     p LoadGrpGrid     b
     d LoadGrpGrid     pi

      /free

       rrnGrpSlct = 0;
       clrGrpSlct = *on;
       dspGrpSlct = *off;
       Write rcdGrpSel0;
       dspGrpSlct = *on;
       clrGrpSlct = *off;
       rrnGrpSlct = 0;

       // Load the Visit Type subfile.
       //***** Beg Add ***** 11/27/15 **************************************
       strPass = 'N';
       Chain(n) q1user pup00;
       If %Found(WMUPL)  and UPXFND = 'Y';
          Chain(n) (UPco#:UPusr) paz00;
          If Not %Found(SMAZL);
             //*** Begin Add *** 04/22/16 ***************************************
             // Initialize WXGSP
             ExSr InitWXGSP;
             //*** End   Add *** 04/22/16 ***************************************
             //*** Begin Chg *** 04/22/16 ***************************************
             //**********Chain(n) (UPco#:UPfnd) pgs00;
             Chain (UPco#:UPfnd) pgs00;
             //*** End   Chg *** 04/22/16 ***************************************
             If %Found(WXGSL);
                If Subset() = *On;
                   scGroup# = GSfnd;
                   scGrpName = GetGrpName(GSfnd);
                   //*** Begin Add *** 04/22/16 ***************************************
                   GSieFL = 'I';
                   //*** End   Add *** 04/22/16 ***************************************
                   scIEfl = GSIEfl;

                   Select;
                   When GSIEfl = 'I';
                       scStatus = 'Include';
                       scIncExcT = 'Exclude';
                   When GSIEfl = 'E';
                       scStatus = 'Exclude';
                       scIncExcT = 'Include';
                   EndSl;

                   If GScRRE = 'Y';
                      scRRE# = %EditC(GSrre#:'X');
                   EndIf;
       //*** Beg Add *** 08/19/19 ******************************************
                   If GSnCarGrp = 'Y';
                      scNCarGpCd = GSnCarGpCd;
                   EndIf;
       //*** End Add *** 08/19/19 ******************************************
                   scNCCI = GSNCCI;
                   scTcc  = GSTcc;
                   mgIncExcV = ' ';
                   erIncExcV = *Off;
                   rrnGrpSlct = rrnGrpSlct + 1;
                   write sflGrpGrid;
                EndIf;
                //*** Begin Add *** 04/22/16 ***************************************
                Update PGS00;
                //*** End   Add *** 04/22/16 ***************************************
             EndIf;
             strPass = 'Y';
          EndIf;
        EndIf;

       If strPass = 'N';
       //***** End Add ***** 11/27/15 **************************************
       Chain(n) intCo# pgs00;
       If %Found(WXGSL);
          DoU %Eof(WXGSL);

             If Subset() = *On;
                scGroup# = GSfnd;
                scGrpName = GetGrpName(GSfnd);
                scIEfl = GSIEfl;

                Select;
                When GSIEfl = 'I';
                   scStatus = 'Include';
                   scIncExcT = 'Exclude';
                When GSIEfl = 'E';
                   scStatus = 'Exclude';
                   scIncExcT = 'Include';
                EndSl;

                If GScRRE = 'Y';
                   scRRE# = %EditC(GSrre#:'X');
                EndIf;

          //***  Begin Chg  ***  06/23/15  ********************************
          //*** Chain (GSco#:GSfnd) pf200;
          //*** If %Found(WDF2L);
          //***    scNCCI = %EditC(F2car#:'X');
          //*** Else;
          //***    scNCCI = '00000';
          //*** EndIf;
                scNCCI = GSNCCI;
          //***  End   Chg  ***  06/23/15  ********************************
          //***  Beg Add  ***  09/11/15  ********************************
                scTcc  = GSTcc;
          //***  End Add  ***  09/11/15  ********************************
          //*** Beg Add *** 08/19/19 **************************************
                If GSnCarGrp = 'Y';
                   scNCarGpCd = GSnCarGpCd;
                EndIf;
          //*** End Add *** 08/19/19 **************************************

                mgIncExcV = ' ';
                erIncExcV = *Off;
                rrnGrpSlct = rrnGrpSlct + 1;
                write sflGrpGrid;
             EndIf;

             ReadE(n) intCo# pgs00;
          EndDo;
       EndIf;
       //***** Beg Add ***** 11/27/15 *************************************
       EndIf;
       //***** End End ***** 11/27/15 *************************************
       //*** Begin Add *** 04/22/16 ***************************************
       //==================================================================
       //  InitWXGSP - Initialize WXGSP
       //==================================================================
       Begsr InitWXGSP;

       Setll *Loval pgs00;
       Read pgs00;
       DoW not %EOF(WXGSL);
          GSieFL = 'E';
          Update pgs00;
          Read pgs00;
       EndDo;

       endsr;
       //*** End   Add *** 04/22/16 ***************************************

      /end-free

     p LoadGrpGrid     e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

     D error           s               n   Inz(*Off)

      /free

       // Start Of Program
       ProgramStart();

       //***  Begin Add  ***  06/23/15  ***********************************
       In dsMemberDta;
       scMember = charMember;
       //***  End   Add  ***  06/23/15  ***********************************

       // Process until exit button is pressed.
       Dou  scCancel = *On Or (scSubmit = *On And error = *Off);

          // Load Group Select Grid
          If errorFound = *Off;
             LoadGrpGrid();
          Else;
             errorFound = *Off;
          Endif;

          // Process Payment Value 1 code Screens
          aPnlHeight = 870;
          Write rcdComPnl;
          Write rcdGrpSel;
          ExFmt rcdGrpSel0;
          Read rcdComPnl;
          Read rcdGrpSel;

          // Check to see if Exclude/Include Option Used
          ChkExcIncOpt();

          // Clear Error Indicators
          ClearErrors();

          Select;
          When scIncAll = *On;
             UpdWxgspSts('I');
          When scExclAll = *On;
             UpdWxgspSts('E');
          When scSubset = *On;
             FilterGroup();
          When scClrFltr = *On;
             ClearFilter();
          When scSubmit = *On;
             error = errorCheck();
             If error = *Off;
                ChkPreSubmit();
             EndIf;
          When scCancel = *On;
             If reSubmit = *On;
                ReSubmitAlert();
             EndIf;
          EndSl;

          // Submit the Job
          If scCancel = *Off And scSubmit = *On And Error = *Off;
             ProcessSubmit();
          EndIf;

       Enddo;

       ProgramEnd();

      /end-free

     p Main            e
       //===================================================================
       // ProcessSubmit - Process Submissions
       //===================================================================
     p ProcessSubmit   b
     d ProcessSubmit   pi

     d kc              s              1    Inz('0')
     d kl              s              1    Inz('0')
     d call            s              1    Inz(' ')
     d prds            s             40    Inz(' ')
     d rrn             s              9  0 Inz(0)

      /free

       kc   = GSkc;
       kl   = GSkl;
       prds = GSprds;
       call = GScall;

       Select;
       When GSpLst = 0;
          CallProgram0();
       When GSpLst = 1;
          CallProgram1(kc:kl);
       When GSpLst = 2;
          CallProgram2(prDs:call:kc:kl);
       When GSpLst = 3;
          CallProgram3(prDs:call);
       When GSpLst = 4;
          CallProgram4(prDs);
       When GSpLst = 5;
          CallProgram5(kc:kl:call:rrn);
       EndSl;

       //***** Beg Chg ****** 09/11/15 ********************************
       //****** If kl = *On And kc = *On;
       If kc = *On;
          scCancel = *On;
       Endif;
       If GSPPGM = 'W1099RP' and kl = *On;
          scSubmit = *off;
       EndIf;
       //***** End Chg ****** 09/11/15 ********************************

      /end-free

     p ProcessSubmit   e
       //===================================================================
       // ProgramEnd - Final Processing
       //===================================================================
     p ProgramEnd      b
     d ProgramEnd      pi

      /free

       //***  Begin Chg  ***  06/23/15  ***********************************
       //***** If @@pgm = 'W7417R1' And scCancel = *On;
       //*****   @@pgm = '*ON';
       If GScxPgmVal <> *blanks and scCancel = *on;
          @@pgm = GScxPgmVal;
       //***  End   Chg  ***  06/23/15  ***********************************
       EndIf;

      /end-free

     p ProgramEnd      e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /free

       // Load Action panel with the Company Name.
       clear scCompName;
       intCo# = userCo#(q1user);
       // Get company name
       scCompName = GetCompNam(intCo#);

       // Get the client id
       GetClientID(##CltId);

       // Get the group Select Record
       Chain @@pgm pgs0p;
       If %Found(WTGSP);
          gSelectFnd = *On;
          scCompName = GStitl;
       EndIf;

       // Activate Filter if Used
       vsSubset = *Off;
       noFilter = *On;
       If GSf17 = 'Y';
          vsSubset = *On;
       EndIf;

       // Control What Headings Display
       //***  Begin Chg  ***  06/23/15  ******************************
       //***If GScRRE = 'Y';
          //***scGrpHead = 'Group,Description,Status,RRE#,NCCI Carrier #';
          //***vsRRE = *On;
       //***Else;
          //***scGrpHead = 'Group,Description,Status,,NCCI Carrier #';
          //***vsRRE = *Off;
       //***EndIf;
       //***** Beg Add **** 09/11/15***********************************
       vsTcc = *off;
       //***** End Add **** 09/11/15***********************************
       vsNCCI = *off;
       vsRRE  = *off;
       scGrpHead = 'Include,Group,Description,';
       If GScar# = 'Y';
          scGrpHead = %trim(scGrpHead) + 'NCCI Carrier #';
          vsNCCI = *on;
       Endif;
       scGrpHead = %trim(scGrpHead) + ',';
       If GScRre = 'Y';
          scGrpHead = %trim(scGrpHead) + 'RRE ID';
          vsRRE = *on;
       Endif;
       //***  End   Chg  ***  06/23/15  ******************************

       //***  Beg Chg  ***  09/11/15  ******************************
       scGrpHead = %trim(scGrpHead) + ',';
       If GSdspTcc = 'Y';
          scGrpHead = %trim(scGrpHead) + 'Transaction ID';
          vsTCC = *on;
       Endif;
       //***  End Chg  ***  09/11/15  ******************************
       //***  Beg Add  ***  08/19/19  ******************************
       scGrpHead = %trim(scGrpHead) + ',';
       If GSnCarGrp = 'Y';
          scGrpHead = %trim(scGrpHead) + 'NCCI Carrier Group Code';
          vsNCarGpCd = *on;
       Endif;
       //***  End Add  ***  08/19/19  ******************************

      /end-free
     p ProgramStart    e
       //===================================================================
       // ResubmitAlert - Alert User that Tries to cancel, that resubmit
       //                 was set
       //===================================================================
     p ReSubmitAlert   b
     d ReSubmitAlert   pi

     d saveGroup       s              3  0

      /free

       // If user is exiting without taking F7 for re-submission groups
       // then display a window telling them that there are some re-submissions
       // to be done
       scEnd = 'N';
       ExFmt wdwRsubmit;

       // Give them options to end the program, clear the resubmit flags (this may be difficult,have
       // to be careful only to clear those that were set with the current session), or return to
       // the program
       Select;
       When scEnd = 'Y';
          //*** Beg Del *** 01/23/19 ********************************
          //***SetGT (intCo#) pd200;
          //***ReadPE (intCo#) pd200;
          //***DoW Not %Eof(WDD1L);
          //***   If D2rsdt <> 0 And D2Type <> 'Q';
          //***      saveGroup = D2fnd;
          //***      DoU %Eof(WDD1L) Or D2fnd <> saveGroup;
          //***         ReadPE (intCo#) pd200;
          //***      EndDo;
          //***   EndIf;

          //***   If D2rSbm = 'Y' And D2rsDt = 0 and D2type <> 'Q';
          //***      D2rSbm = ' ';
          //***      Update pd200;
          //***   EndIf;

          //***   ReadPE (intCo#) pd200;
          //***EndDo;
          //*** End Del *** 01/23/19 ********************************
          //*** Beg Add *** 01/23/19 ********************************
          SetGT (intCo#) pd300;
          ReadPE (intCo#) pd300;
          DoW Not %Eof(WDD1L6);
             If D3rsdt <> 0;
                saveGroup = D3fnd;
                DoU %Eof(WDD1L6) Or D3fnd <> saveGroup;
                   ReadPE (intCo#) pd300;
                EndDo;
             EndIf;

             If D3rSbm = 'Y' And D3rsDt = 0;
                D3rSbm = ' ';
                Update pd300;
             EndIf;

             ReadPE (intCo#) pd300;
          EndDo;
          //*** End Add *** 01/23/19 ********************************
       Other;
          scCancel = *Off;
       EndSl;

      /end-free

     p ReSubmitAlert   e
       //===================================================================
       // Subset - Subset Groupss Displayed
       //===================================================================
     p Subset          b
     d Subset          pi              n

      /free

       //***  Begin Del  ***  06/23/15  ***********************************
       //***Chain (GSco#:GSfnd) pf200;
       //***If Not %Found(WDF2L);
       //***   F2car# = 0;
       //***EndIf;
       //***  End   Del  ***  06/23/15  ***********************************

       Select;
       When ssGroup# <> 0 And ssGroup# <> GSfnd;
          Return *Off;
       When ssGrpName <> ' ' And %Scan(%Trim(%Xlate(low:up:ssGrpName)):
                                       %Trim(%Xlate(low:up:GSname))) = 0;
          Return *Off;
       When ssStatus <> ' ' And ssStatus <> GSieFl;
          Return *Off;
       //***  Begin Chg  ***  06/23/15  ***********************************
       //***When ssNCCI# <> ' ' And %Dec(ssNcci#:5:0) <> F2Car#;
       When ssNCCI# <> ' ' And ssNcci# <> GSncci;
       //***  End   Chg  ***  06/23/15  ***********************************
          Return *Off;
       When ssRRE# <> ' ' And %Dec(ssRRE#:9:0) <> GSrre#;
          Return *Off;
       //***** Beg Add ***** 09/11/15 *************************************
       When ssTCC <> *blank and ssTCC <> GStcc ;
          Return *off;
       //***** End Add ***** 09/11/15 *************************************
       EndSl;

       Return *On;

      /end-free

     p Subset          e
       //===================================================================
       // UpdWxgspSts - Update WXGSP Statuses
       //===================================================================
     p UpdWxgspSts     b
     d UpdWxgspSts     pi
     d  statusIn                      1    Const

      /free

       // Update All Statuses
       Chain intCo# pgs00;
       If %Found(WXGSL);
          DoU %Eof(WXGSL);
             If Subset() = *On;
                If ChangeStatus() = *On;
                   GSieFl = statusIn;
                   Update pgs00;
                EndIf;
             EndIf;
             ReadE intCo# pgs00;
          EndDo;
       EndIf;

      /end-free

     p UpdWxgspSts     e
