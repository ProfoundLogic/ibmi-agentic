     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE WTPOLDEL - Employer Deletion
     H DFTNAME(WTPOLDEL)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP('QILE')
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 10/31/12                                         *
      *  Programmer: C:279                                            *
      *  Project   : WC2115                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
     h*  01/12/15 C:283 RedMine: 4885                                 *
     h*                Add the handling for Deletion of All Divisions *
     h*                if requested.                                  *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fwpigenpop cf   e             workstn extfile('WPIGENPOP')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwmall     if   e           k disk    rename(pal01:pal00)
     Fwasrl7    if   e           k disk    rename(psr01:psr07)
      *** Beg Chg *** 01/12/15 *********************************************
     F***********sdcmla    if   e           k disk    rename(pcm01:scm0a) prefix(darts)
     Fsdcmla    if   e           k disk    rename(pcm01:scm0a)
     Fwmeml     if   e           k disk    rename(pem01:pem00)
     F                                     infds(dbfds)
      *** End Chg *** 01/12/15 *********************************************
     fssylf     if   e           k disk

     d ##co            s                   like(EMco#)
     d ##fnd           s                   like(EMfnd)
     d ##emp           s                   like(EMemp#)
     d ##div           s                   like(EMdiv)

     D ##kc            s               n   inz(*off)
     D ##kl            s               n   inz(*off)
     D quoteMode       s              1    inz(' ')
     D process         s               n   inz(*off)
     D mode            s              1    inz('E')
     d xxRrn           s              9  0
      *** Beg Add *** 01/12/15 *********************************************
     d empDiv          s           1500    inz(' ')
     d empDivs         s           1500    inz(' ')
     d empDivs1        s           1500    inz(' ')
     d empDivs2        s           1500    inz(' ')
     d allDiv          s              1
     d xxRrn1          s              9  0
      *** End Add *** 01/12/15 *********************************************

     d* main Processing
     d main            pr

     d* program Start
     d programStart    pr
     d processMe                       n

     d* deletion program
     d empDelete       pr                  extpgm('W0050R02')
     d ##kc                                like(##kc)
     d ##kl                                like(##kl)
     d ##mode                              like(mode)
     d ##rrn                               like(xxRrn)
     d ##qte                               like(quoteMode)

     d* Darts exist
     d DARTSExist      pr              n

     d* Payroll Exist
     d PayrollExist    pr              n

      *** Beg Add *** 01/12/15 *********************************************
     d* Location Exist
     d LocationExist   pr              n

     d* Display 2 button Pop up's
     d popDisplay      pr              n
     d rtnBtnOk                       1n

     d dbfds           ds
     d  dbrrn                397    400b 0
      *** End Add *** 01/12/15 *********************************************
     d/copy wPrUsrPrms
     d/copy sPrString
    
     c* Employer Master File Key List
     c     keyWMALL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    ##div

     c* Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@rrn             9 0          wmemp
     c                   Parm                    @@Qte             1            Quote
      *** Beg Add *** 01/12/15 *********************************************
     c                   Parm                    @@AllDiv          1            All Divisions
      *** End Add *** 01/12/15 *********************************************

      /free

       // main program processing
       If %parms >= 1;
          xxRrn = @@rrn;
       Endif;

       // Quote Environment
       If %parms >= 2;
          quoteMode = @@Qte;
       Endif;
       //*** Beg Add *** 01/12/15 *********************************************
       // Single/Multiple Divisions deletion
       allDiv = *Blanks;
       If %parms >= 3;
          allDiv = @@AllDiv;
       Endif;
       //*** End Add *** 01/12/15 *********************************************

       ProgramStart(process);

       If process;
          Main();
       Endif;

       *InLr = *On;
       Return;

      /end-free
       //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     d Main            PI

      /free

       clear scOkCnTtl;
       scOkCnTtl = 'Confirm your choice';
       // Check for Darts and payroll
       //*** Beg Chg *** 01/12/15 *********************************************
       //*********** If DARTSExist = *off and PayrollExist = *off;
          // Check for location/Entity
          //********btnOk = *off;
          //********btnCancel = *off;
          //********clear scOkCnMsg;
          //********clear scBtn1Txt;
          //********clear scBtn2Txt;
          //********scBtn1txt = 'Yes';
          //********scBtn2Txt = 'No';
          //********Chain keyWMALL pal00;
          //********If %found(WMALL);
             //*****scOkCnMsg = 'Entities/Locations exist for this employer. ' +
             //***** 'Please confirm if you want to continue.';
             //*****dou btnOk = *on or btnCancel = *on;
                //**exfmt rcdTwoBtn;
             //*****enddo;
          //********Endif;
          empDiv = *Blanks;
          empDivs = *Blanks;
          empDivs1 = *Blanks;
          If allDiv = 'Y';
             Setll (EMco#:EMfnd:EMemp#) pem00;
             Dou %eof(WMEML);
                Reade (EMco#:EMfnd:Ememp#) pem00;
                If not %eof(WMEML);
                   empDiv = %Trim(empDiv) + ', ' + %Editc(EMdiv:'X');
                   If DARTSExist = *On;
                      empDivs = %Trim(empDivs) + ', ' + %Editc(EMdiv:'X');
                   Endif;
                   If PayrollExist = *On;
                     empDivs1 = %Trim(empDivs1) + ', ' + %Editc(EMdiv:'X');
                  Endif;
                  If LocationExist = *On;
                     empDivs2 = %Trim(empDivs2) + ', ' + %Editc(EMdiv:'X');
                  Endif;
                Endif;
             Enddo;
             If empDivs <> *Blanks And btnCancel = *Off;
                scOkCnMsg = 'An active DARTS Remark exists for Employer: ' +
                              %Editc(EMemp#:'X') + ' Divisions: ' +
                             %Trim(%Subst(empDivs:2)) + ' .' +
                             'Do you want to Override error message?';
                btnCancel = popDisplay(BtnOk);
             Endif;
             If empDivs1 <> *Blanks And btnCancel = *Off;
                scOkCnMsg = 'Self-reporting payrolls have been '+
                 'entered for Employer: ' + %Editc(EMemp#:'X') +
                 ' Divisions: ' + %Trim(%Subst(empDivs1:2)) + ' .' +
                 'Continue with deletion?';
                 btnCancel = popDisplay(BtnOk);
             Endif;
             If empDivs2 <> *Blanks And btnCancel = *Off;
                scOkCnMsg = 'Entities/Locations exist for Employer: ' +
                  %Editc(ALemp#:'X') + ' Divisions: ' +
                  %Trim(%Subst(empDivs2:2)) +
                  ' Please confirm if you want to continue.';
                btnCancel = popDisplay(BtnOk);
             Endif;

          Else;
             If DARTSExist = *On;
                scOkCnMsg = 'An active DARTS Remark exists for Employer: ' +
                              %Editc(EMemp#:'X') + ' Division: ' +
                              %Editc(EMdiv:'X') + ' .' +
                              'Do you want to Override error message?';
                btnCancel = popDisplay(BtnOk);
             Endif;
             If PayrollExist = *On And btnCancel = *Off;
                scOkCnMsg = 'Self-reporting payrolls have been '+
                 'entered for Employer: ' + %Editc(EMemp#:'X') +
                 ' Division: ' + %Editc(EMdiv:'X') +
                 ' .' + 'Continue with deletion?';
                btnCancel = popDisplay(BtnOk);
             Endif;
             If LocationExist = *On And btnCancel = *Off;
                scOkCnMsg = 'Entities/Locations exist for Employer: ' +
                  %Editc(ALemp#:'X') + ' Division: ' + %Editc(EMdiv:'X') +
                  ' Please confirm if you want to continue.';
                btnCancel = popDisplay(BtnOk);
             Endif;
          Endif;
       //*** End Chg *** 01/12/15 *********************************************

          If btnCancel = *off;
             btnOk = *off;
             btnCancel = *off;
             clear scOkCnMsg;
             clear scBtn1Txt;
             clear scBtn2Txt;
             scBtn1Txt = 'Continue';
             scBtn2Txt = 'Cancel';
       //*** Beg Add *** 01/12/15 *********************************************
             If AllDiv = 'Y';
                scOkCnMsg = 'Please confirm if you want to process the ' +
                 'Deletion of Employer ' + %editc(Emfnd: 'X')+ '-' +
                 %editc(Ememp#: 'X')+ ' Divisions: ' + %Trim(%Subst(empDiv:2)) +
                 '.';
             Else;
       //*** End Add *** 01/12/15 *********************************************
             scOkCnMsg = 'Please confirm if you want to process the ' +
              'Deletion of Employer ' + %editc(Emfnd: 'X')+ '-' +
              %editc(Ememp#: 'X')+ ' Division: ' + %editc(Emdiv: 'X') + ' ' +
              Emname + %trimr(Emnam2) + '.';
       //*** Beg Add *** 01/12/15 *********************************************
              Endif;
       //*** End Add *** 01/12/15 *********************************************
             dou btnOk = *on or btnCancel = *on;
                exfmt rcdTwoBtn;
             enddo;
          Endif;

          //*** Beg Chg *** 01/12/15 *********************************************
          //************ If btnOk = *on;
          If btnOk = *On and allDiv <> 'Y';
          //*** End Chg *** 01/12/15 *********************************************
             btnOk = *off;
             clear scMessage;
             empDelete(##kc:##kl:mode:xxRrn:quoteMode);
             scMessage = 'Employer Deleted Successfully.';
             dou BtnOk = *on;
                exfmt rcdOneBtn;
             Enddo;
             BtnOk = *off;
          Endif;
          //*** Beg Add *** 01/12/15 *********************************************
          If btnOk = *On and allDiv = 'Y';
             btnOk = *off;
             clear scMessage;
             Setll (EMco#:EMfnd:EMemp#) pem00;
             Reade (EMco#:EMfnd:Ememp#) pem00;
             Dow Not %Eof(WMEML);
                xxRrn1 = dbRrn;
                empDelete(##kc:##kl:mode:xxRrn1:quoteMode);
                Reade (EMco#:EMfnd:Ememp#) pem00;
             Enddo;

             scMessage = 'Employer Deleted Successfully.';
             dou BtnOk = *on;
                 exfmt rcdOneBtn;
             Enddo;
             BtnOk = *off;
          Endif;
          //*** End Add *** 01/12/15 *********************************************
       //*** Beg Del *** 01/12/15 *********************************************
       //************* Endif;
       //*** End Del *** 01/12/15 *********************************************

      /end-free
     P Main            E
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n

      /free

       // Chain to Employer master for Program.
       Chain xxRrn PEM0P;

       // Process only if valid employer record found.
       If not %found;
          processme = *Off;
          Return;
       Endif;

       ##co    = EMco#;
       ##Fnd   = EMfnd;
       ##Emp   = EMemp#;
       ##Div   = EMdiv;
       scPnlTitle = 'Information';
       scBtnText = 'Ok';
       scPLeft = 360;
       scPTop  = 215;
       scMLeft = 370;
       scMTop  = 260;
       scBLeft = 535;
       scBTop  = 340;
       scOkLeft = 515;
       scOkTop  = 340;
       scCnLeft = 625;
       scCnTop  = 340;

       processMe = *on;
       Return;

      /end-free
     P ProgramStart    E
      //===================================================================
       // Determine if DARTS records exist
      //===================================================================
     P DARTSExist      b
     D DARTSExist      pi              n

     D xxSyKey         s              2

     D                 ds
     D dartsKey                      40
     D  dartsCo#                      3  0 overlay(dartsKey)
     D  dartsFnd                      3  0 overlay(dartsKey:4)
     D  dartsEmp#                     9  0 overlay(dartsKey:7)
     D  dartsDiv                      5  0 overlay(dartsKey:16)
     D  dartsQte                      1    overlay(dartsKey:40)
      /free

       xxSyKey = 'SY';
       clear scOkCnMsg;
       clear scBtn1Txt;
       clear scBtn2Txt;
       scBtn1Txt = 'Yes';
       scBtn2Txt = 'No';
       //*** Beg Del *** 01/12/15 *********************************************
       //*********** scOkCnMsg = 'An active DARTS Remark exists for this employer. '+
       //***********  'Do you want to Override error message?';
       //*** End Del *** 01/12/15 *********************************************
       Chain xxSyKey ssy01;
       If %found and SYscif = 'Y';

          // Establish Key
          clear dartsKey;
          dartsCo#  = EMco#;
          dartsFnd  = EMfnd;
          dartsEmp# = EMemp#;
          dartsDiv  = EMdiv;

          if quoteMode = 'Q';
             dartsQte = 'Q';
          endif;

          // Chain to DARTS Remarks detail file
          setll dartsKey scm0a;
          dou %eof;
             reade dartsKey scm0a;
             //*** Beg Chg *** 01/12/15 ***************************************
             //************* if not %eof and dartsCMdifl = 'Y' and dartsCMstat <> 'I' and
             //*************  dartsCMdcdt = *zero;
             if not %eof and CMdifl = 'Y' and CMstat <> 'I' and
              CMdcdt = *zero;
             //*** End Chg *** 01/12/15 ***************************************

                //*** Beg Del *** 01/12/15 ************************************
                //************** btnOk = *off;
                //************** btnCancel = *off;
                //************** dou btnOk = *on or btnCancel = *on;
                   //*********** exfmt rcdTwoBtn;
                //************** enddo;
                //************** select;
                //************** when btnOk = *on;
                //**************    return *off;
                //************** other;
                //*** End Del *** 01/12/15 ************************************
                   return *on;
                //*** Beg Del *** 01/12/15 ************************************
                //************** endsl;
                //*** End Del *** 01/12/15 ************************************
             endif;
          enddo;
       endif;
       return *off;
      /end-free
     P DARTSExist      e
      //===================================================================
       // Determine if Payroll records exist
      //===================================================================
     P PayrollExist    b
     D PayrollExist    pi              n

     C     keyWASRL7     klist
     C                   kfld                    EMco#
     C                   kfld                    EMfnd
     C                   kfld                    EMemp#
     C                   kfld                    EMdiv

      /free

       clear scOkCnMsg;
       clear scBtn1Txt;
       clear scBtn2Txt;
       scBtn1Txt = 'Yes';
       scBtn2Txt = 'No';
       //*** Beg Del *** 01/12/15 *************************************************
       //************ scOkCnMsg = 'Self-reporting payrolls have been '+
       //************  'entered for this policy. Continue with deletion?';
       //*** End Del *** 01/12/15 *************************************************

       setll keyWASRL7 psr07;
       dou %eof;
          reade keyWASRL7 psr07;
          if not %eof;

             //*** Beg Del *** 01/12/15 ********************************************
             //************ btnOk = *off;
             //************ btnCancel = *off;
             //************ dou btnOk = *on or btnCancel = *on;
             //************    exfmt rcdTwoBtn;
             //************ enddo;
             //************ select;
             //************ when btnOk = *on;
             //************    return *off;
             //************ other;
             //*** End Del *** 01/12/15 ********************************************
                return *on;
             //*** Beg Del *** 01/12/15 ********************************************
             //************ endsl;
             //*** End Del *** 01/12/15 ********************************************
          endif;
       enddo;
       return *off;
      /end-free
     P PayrollExist    e
      //*** Beg Add *** 01/12/15 ****************************************************
      //===================================================================
       // Determine if Entity/Location record exists
      //===================================================================
     P LocationExist   b
     D LocationExist   pi              n

     c* Employer Master File Key List with Div
     c     keyWMALL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    ##div

      /Free
          // Check for location/Entity
          btnOk = *off;
          btnCancel = *off;
          clear scOkCnMsg;
          clear scBtn1Txt;
          clear scBtn2Txt;
          scBtn1txt = 'Yes';
          scBtn2Txt = 'No';

          Chain keyWMALL pal00;
          If %Found(wmall);
             scOkCnMsg = 'Entities/Locations exist for Employer: ' +
               %Editc(ALemp#:'X') + ' Division: ' + %Editc(ALdiv:'X') + ' ' +
               ' Please confirm if you want to continue.';
             Return *On;
          Endif;

          Return *Off;
      /End-Free
     P LocationExist   e
      //===================================================================
       // Display the Pop-Up with 2 buttons
      //===================================================================
     P popDisplay      b
     D popDisplay      pi              n
     D rtnBtnOk                       1n

      /Free
       btnOk = *off;
       btnCancel = *off;
       dou btnOk = *on or btnCancel = *on;
          exfmt rcdTwoBtn;
       enddo;
       rtnBtnOk = btnOk;
       Return btnCancel;
      /End-Free
     P popDisplay      e
      //*** End Add *** 01/12/15 ****************************************************
