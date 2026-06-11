     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE A/R Detail Report - Prompt
     H*********************************************************************
     H*  DESCRIPTION:  This program prompts for selection criteria to run *
     H*                the A/R Detail Report.                             *
     H*                                                                   *
     H*  CALLED FROM:                                                     *
     H*    MENU                                                           *
     H*                                                                   *
     H*  CALLS TO:                                                        *
     H*                                                                   *
     H*                                                                   *
     H*  REWRITE:               C:306     PROJECT: WC2418                 *
     H*********************************************************************
     H*
     H* REVISIONS:
     H*
     H* 11/06/23 C:291 Project: WC2934
     H*                Add Multi-select box for Batch Types.
     H*
     H* 06/21/17 C:136 Support: 47921
     H*                Fix year range values passed out of program
     H*
     H****************************************************************
      * Files Declaration
     fWtrptardtlcf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
     fwmemp     if   e             disk    rename(pem01:pem0p)
     f*** Employer Master File
     fwtctl     if   e           k disk    rename(pct01:pct00)
     f*** A/R Category Type Table File
     fswcpf     if   e             disk
     F*** Workers Compensation Control File
     fwxgsl     if   e           k Disk    reName(pgs01:pgs00)
      *** Group select Intermediate file.
     fwdelp     if   e             disk    Rename(pel01:pel02)
     fwmeml     if   e           k disk    Rename(pem01:pem02)
     f* Transaction Type Code file for AR Payment codes
     fwtabl     if   e           k Disk    include(ptt01)
     f
      * Prototypes

     d/copy sPrDate
     d/copy sPrString
     d/copy sPrCompany
     d/copy wPrEmpLr
     d/copy wPrUsrprms
     d/copy WTSearchDs
     d
     dSA030R           PR                  extpgm('SA030R')
     d outsufx                        3

     dS0006R           PR                  extpgm('S0006R')
     d inEnvrnment                    1    const

     d CheckLib        pr                  extpgm('SRCHKLIB')
     d inLib                         10    const
     d inType                         4    const
     d inAppendSfx                    1    const
     d inFlag                         1
     d
      * Employer Search Proc.
     d EmployerSrch    pr
     d
      * Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     d inMode                         1
     d inCall                         1
     d outRrn#                        9
     d inSelect                            likeds(dsSrchCrit)
     d
      * Load A/R category Types
     d LoadARCatTyp    pr
     d
      * Load A/R Codes
     d LoadARCodes     pr
     d
     dW7380C           pr                  extpgm('W7380C')
     d inco#                          3
     d infnd_fr                       3
     d infnd_to                       3
     d inyr_fr                        3
     d inyr_to                        3
     d inardt_fr                      8
     d inardt_to                      8
     d inEmpNoAct                     1
     d insort                         1
     d inagt#                         5
     d inemp#                         9
     d indiv                          5
     d inplan                         1
     d inctyp                        10
     d inAR_code                      1
     d inarcd                        30
     d inreport                       1
     d inoutput                       1
     d inoutq                        10    const
     d inarpt                         1
     d inbilltyp                      1
     d inMnthEnd                      1
      //*** Beg Add ***** 11/06/23 ****************************************
     d inBatchTyp                    20
      //*** End Add ***** 11/06/23 ****************************************
     d
     d inco#           s              3
     d infnd_fr        s              3
     d infnd_to        s              3
     d inyr_fr         s              3
     d inyr_to         s              3
     d inardt_fr       s              8
     d inardt_to       s              8
     d inEmpNoAct      s              1
     d insort          s              1
     d inagt#          s              5
     d inemp#          s              9
     d indiv           s              5
     d inplan          s              1
     d inctyp          s             10
     d inAR_code       s              1
     d inarcd          s             30
     d inreport        s              1
     d inoutput        s              1
     d*inoutq                        10    CONST
     d inarpt          s              1
     d inbilltyp       s              1
     d inMnthEnd       s              1
     d intGrp          s              3  0 inz
     d
     d inMode          s              1
     d inCall          s              1
     d outRrn#         s              9
     d empRrn          s              9  0
     d inSelect        s           1024
     d intCo#          s                   Like(ELco#)
     d intFnd          s                   Like(ELfnd)
     d intEmp#         s                   Like(ELemp#)
     d intDiv          s                   Like(ELdiv)
     d intFyr          s                   Like(ELfyr)
     d
     d
     d comapi          PR                  extpgm('WA001R')
     d outCo#                         3  0
     d outfnd                         3  0
     d outconm                       40
     d outfnam                       40
     d
     d/copy scopypr01d
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d conoptnm        c                   const('A/R Detail Report')
     d condigits       C                   ' 0123456789'
     d @@call          s              1
     d @@prds          s             40
     d strxxcall       s                   like(@@call)
     d strco#          s              3  0
     d strfnam         s             40
     d strfnd          s              3  0
     d strEmp#         s                   like(scEmp#)
     d strDiv          s                   like(scDiv)
     d
     d intGrpCnt       s              5  0 inz(0)
     d issrchCtg       s               n   inz(*off)
     d issrchCode      s               n   inz(*off)
     d
     d x               s              3  0
     d outsufx         s              3
     d strrptl         s             23
     d
     d intgroup        s              3  0
     d intFNrrn        s              9  0
     d intEMrrn        s              9  0
     d intCTrrn        s              9  0
     d intpos          s              6  0 inz(0)
     d strARcode       s             10
     d strerror        s              1
     d strtmp          s             40
     d isflag          s              1
     d
     d outCo#          s              3  0 inz(000)
     d outfnd          s              3  0 inz(000)
     d outconm         s             40    inz(*Blank)
     d outfnam         s             40    inz(*Blank)
     d iserrflg        s              1
     d inctypTmp       s             50    inz(*Blank)
      //*** Beg Add ***** 11/06/23 ****************************************
     d inBatchTyp      s             20    inz(*Blank)
      //*** End Add ***** 11/06/23 ****************************************
     d inarcdTmp       s            125    inz(*Blank)
     d strARTyDsc      s            125    inz(*Blanks)
     d strARCdDsc      s            125    inz(*Blanks)

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3

      * Intialize the Screen fields
     d LoadInitVal     pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Process other options
     d ProcOther       pr

      * Set Criteria
     d SetCriteria     pr

     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
     C*
      /Free
       // Loading intialize values
       loadInitVal();

       // Start Of Program
       ProgramStart();



       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free


       //===================================================================
       // LoadInitVal - Intialize the Screen fields
       //===================================================================
     p LoadInitVal     b
     d LoadInitVal     pi
     d
      /Free
        clear RcdArDtlRp;
        scArCodes = ' ';
        scBillTyp = ' ';
        isVisbEmp = *off;
        BtnEmpSrch = *off;
        intGrpCnt = 0;
      /End-Free
     p LoadInitVal     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            PI
     d
     d
     d
      /free
       //*  Process until cancel button is pressed.

       dou BtnCancel = *On;


        scTabHght  = 710;
        aPnlHeight = 770;
        write RcdComPnl;
        write RcdTabArDt;
        Exfmt RcdArDtlRp;
        read RcdComPnl;
        read RcdTabArDt;

       Select;

       When BtnEmpSrch = *on;
            BtnEmpSrch = *off;
            EmployerSrch();
       When BtnSubmit = *on;
          BtnSubmit = *off;
          Exsr arCod_typ;
          //*** Beg Add ***** 11/06/23 *************************************
          Exsr Batch_typ;
          //*** End Add ***** 11/06/23 *************************************
          Exsr errck;
          If iserrflg = *Off;
             Exsr Prsctl;
             Leave;
          Endif;
       When BtnRefresh = *On;
          BtnRefresh =*Off;
          // Intialize the Screen fields
          LoadInitVal();
          // Reload the program start values gain.
          ProgramStart();

       Other;

       EndSl;
       EndDo;

       BegSr arCod_typ;
          inctypTmp = ' ';
          inctypTmp = %trim(%xlate(',':' ':scCTyp_1));
          intpos = 0;
          Dou intpos = 0;
             intpos = %scan(' ':%trim(inctypTmp));
             If intpos <> 0;
                inctypTmp  =  %replace('':inctypTmp:intpos:1);
             Endif;
          EndDo;

          inarcdTmp = ' ';
          inarcdTmp =  %trim(%xlate(',':' ':scARCD_1));
          intpos = 0;
          Dou intpos = 0;
             intpos = %scan(' ':%trim(inarcdTmp));
             If intpos <> 0;
                inarcdTmp = %replace('':inarcdTmp:intpos:1);
             EndiF;
          EndDo;
       EndSr;

       //*** Beg Add ***** 11/06/23 ****************************************
       BegSr Batch_typ;
          inBatchTyp = ' ';
          inBatchTyp = %trim(%xlate(',':' ':scBType));
          intpos = 0;
          Dou intpos = 0;
             intpos = %scan(' ':%trim(inBatchTyp));
             If intpos <> 0;
                inBatchTyp  =  %replace('':inBatchTyp:intpos:1);
             Endif;
          EndDo;
       EndSr;
       //*** End Add ***** 11/06/23 ****************************************


       Begsr errck;
       iserrflg = *off;     // any field error message edit.

       erPolyrFm = *off;
       erPolyrFm2 = *off;
       erArRDt_Fr = *off;
       erArRDt_To = *off;
       erArRDt_T2 = *off;
       ersort = *off;
       erEmpNoAct = *off;
       erCommPlan = *off;
       erEmp# = *off;
       erDiv2  = *off;
       erArtype = *Off;
       erArCodes1 = *off;
       erArCodes2 = *off;
       erArtype1 = *off;
       erArCod = *off;
       erARpt1 = *off;
       erARpt2 = *off;
       erReport = *off;
       erOutput = *off;

       //*** Policy year range   */
       If scYr_fr < *zero;
          erPolyrFm = *on;
          iserrflg = *on;
       EndIf;

       If scYr_to < scYr_fr;
          erPolyrFm2 = *on;
          iserrflg = *on;
       EndIf;

       scArRDt_Fr = dtCheck(scArRDt_Fr);

       If scArRDt_Fr = *zero;
          erArRDt_Fr = *on;
          iserrflg = *on;
       EndIf;

       scArRDt_To = dtCheck(scArRDt_To);

       If scArRDt_To = *zero;
          erArRDt_To = *on;
          iserrflg = *on;
       EndIf;

       If cvtToDate(scArRDt_To) < cvtToDate(scArRdt_fr);
          erArRDt_T2 = *on;
          iserrflg = *on;
       EndIf;

       If scEmpNoAct <> 'Y' and scEmpNoAct <> 'N';
          erEmpNoAct = *on;
          iserrflg = *on;
       EndIf;

       if scSort <> 'A' and scSort <> 'E' and
          scSort <> 'N';
          ersort = *on;
          iserrflg = *on;
       EndIf;

       If scEmp# = *zero and scDiv <> *blank;
          erEmp# = *on;
          iserrflg = *on;
       EndIf;

       If (scEmp# <> strEmp# or scDiv <> strDiv)
          and scDiv = *blank ;
          erDiv2  = *on;
          iserrflg = *on;

          strEmp# =  scEmp#;
          strDiv  =  scDiv;
       EndIf;

       If scCommPlan <> 'A' and scCommPlan <> 'E'
          and scCommPlan <> 'C';
          erCommPlan = *on;
          iserrflg = *on;
       Endif;

        //***Errchk 2 ***
       If %len(%trim(inctypTmp)) > 10;
          erArtype1 = *on;
          iserrflg = *on;
       EndIf;

       If %len(%trim(inarcdTmp)) > 60;
          erArCod = *on;
          iserrflg = *on;
       EndIf;

       If inctypTmp <> *blank and (inarcdTmp <> *blank Or
          scARCodes  <> *blank);
          erArtype = *On;
          iserrflg = *on;
       EndIf;

       If scARCodes <> *Blanks and scARCodes <> 'I' and scARCodes <> 'E'
          and scARCodes <> 'C';
          erArCodes1 = *on;
          iserrflg = *on;
       EndIf;

       If inarcdTmp <> *blank and
          (scARCodes <> 'I' and scARCodes <> 'E');
          erArCodes2 = *on;
          iserrflg = *on;
       Endif;

       //*** Beg Add ***** 11/06/23 ****************************************
       mgBType = *Blanks;
       erBType = *Off;
       If %len(%trim(inBatchTyp)) > 20;
          mgBType = 'Maximum 20 Batch type selections are allowed. ';
          erBType = *On;
       Else;
          If %Subst(scBType:1:1) = *Blank and %Subst(scBType:3:1) <> *Blank;
             mgBType = 'Other Batch Types cannot be selected with ' +
              'option ''All''. ';
             erBType = *On;
          Endif;
       EndIf;
       //*** End Add ***** 11/06/23 ****************************************

       //** Errchk 3 **/

       If scarpt <> 'Y' and scarpt <> 'N';
          erARpt1 = *on;
          iserrflg = *on;
       endIf;

       If scarpt = 'Y' and scCommPlan <> 'E';
          erARpt2 = *on;
          iserrflg = *on;
       EndIf;

       If scOutput <> 'P' and scOutput <> 'F'
          and scOutput <> 'B';
          erOutput = *on;
          iserrflg = *on;
       Endif;

       If (scOutput = 'P' or scOutput = 'B') and
          (scReport <> 'D' and scReport <> 'S' and
          scReport <> 'B');
          erReport = *on;
          iserrflg = *on;
       EndIf;

       Endsr;

  //*  //** PRSCTL - Process control after no errors found (Call Submitter)

       BegSr PrsCtl;
          comapi(outCo#:outfnd:outconm:outfnam);
          inco# = %editc(outCo#:'X');
         //*** beg chg ***** 06/21/17 ***************
         //*** inyr_fr   =  %char(scYr_Fr);
         //*** inyr_to   =  %char(scYr_To);
          inYr_Fr   =  %Editc(scYr_Fr:'X');
          inYr_To   =  %Editc(scYr_To:'X');
         //*** end chg ***** 06/21/17 ***************
          inardt_fr =  %editc(scArRDt_Fr:'X');
          inardt_to =  %editc(scArRDt_To:'X');
          inagt#    =  %editc(scAgt#:'X') ;
          inemp#    =  %editc(scEmp#:'X');
          inctyp   =   %trim(inctypTmp);
          inarcd  =    %trim(inarcdTmp);

          infnd_fr = '000';
          infnd_to = '000';


          //*** Beg Chg ***** 11/06/23 ****************************************
          //***W7380C(inco#:infnd_fr:infnd_to:inyr_fr:inyr_to:inardt_fr:inardt_to:
          //***scEmpNoAct:scSort:inagt#:inemp#:scDiv:scCommPlan:inctyp:scARcodes:
          //***inarcd:scReport:scOutput:GetUsrOutq(q1user):scarpt:scbilltyp:
          //***scMnthEnd);
          W7380C(inco#:infnd_fr:infnd_to:inyr_fr:inyr_to:inardt_fr:inardt_to:
          scEmpNoAct:scSort:inagt#:inemp#:scDiv:scCommPlan:inctyp:scARcodes:
          inarcd:scReport:scOutput:GetUsrOutq(q1user):scarpt:scbilltyp:
          scMnthEnd:inBatchTyp);
          //*** End Chg ***** 11/06/23 ****************************************

       EndSr;
      /End-Free

     p Main            e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free
       If %parms >= 2;
          strxxcall = @@call;
       Endif;

       strEmp# = *zeros;
       strDiv  = ' ';
       scarpt = 'N';
       scempnoact = 'N';

       read swcpf;
       if wcFl23 = 'Y' or wcFl24 = 'Y';
          isArBillty = *On;
       Else;
          isArBillty = *Off;
       EndIf;

       dspMnthEnd = *off;
       CheckLib('PDSMTH':'*USR':'1':isflag);

       If isflag = '1';
          dspMnthEnd = *on;
       Endif;

       //**************************************************************************/
       Sa030r(outSufx);
       If outSufx = *blanks;
         strrptl = 'PDSRPT' + ', if selected.';
       Else;
         strrptl = 'PDSRPT' + outSufx + ', if selected.';
       EndIf;
          scrptl = 'A/R Detail Report will produce output file WFARP in '+
                   strrptl;

          scrpt2 = 'Aging report will produce output file WFAGP2 in ' +
                   strrptl;

       // Load Action panel with the Company Name.
           clear scCompName;
           intCo# = userCo#(q1user);
           scCompName = GetCompNam(intCo#);

       // Checking multiple group selection.

           Setll (intCo#:*loval) pgs00;
           reade (intCo#) pgs00;
           Dow not %eof(wxgsl);
           If GSIeFl = 'I';
              intGrp = GSfnd;
              intGrpCnt = intGrpCnt + 1;
           Endif;

           If intGrpCnt > 1;
              isVisbEmp = *on;
              Leave;
           EndIf;
           reade (intCo#) pgs00;
           EndDo;

       // Loading the A/R category type values.
       LoadARCatTyp();

       // Loading the A/R Codes values.
       LoadARCodes();

       //*** Beg Add ***** 11/06/23 *************************************
       scBTypeCho = 'All,Blank - Standard Batch,EFT,' +
        'SR - S/R Payment(CiCenter),LB - Lockbox,DED - Deductible,' +
        'RDP - Retrospective, BIL - S/R Billing(CiCenter),' +
        '3PY - 3rd Party Payment, 3BL - PayGoBilling';
       scBTypeVal = ' ,#,A,S,L,D,R,B,T,U';
       scBType = ' ,';

       //*** End Add ***** 11/06/23 *************************************

      /End-Free
     p ProgramStart    e


       //===================================================================
       // EmployerSrch  - Employer/Div Search
       //===================================================================
     p EmployerSrch    b
     d EmployerSrch    pi

      /Free
       inMode  = 'E';
       inCall  = '2';
       clear dsSrchCrit;
       clear inSelect;
       dsEmpGrp  = intGrp;
       dsEmpName = ' ';
       inSelect  = dsSrchCrit;
       outRrn#   = '000000000';
       EmpSearch(inMode:inCall:outRrn#:inSelect);

       If outRrn# <> '000000000';
          empRrn=%Dec(outRrn#:9:0);
          Chain(n) empRrn pel02;
          If %Found(Wdelp);
             intCo#  = ELco#;
             intFnd  = ELfnd;
             intEmp# = ELEmp#;
             intDiv  = ELdiv;

             Chain(n)(intCo#:intFnd:intEmp#:intDiv) pem02;
             If %Found(Wmeml);
                scEmp#     = EMemp#;
                scDiv      = %editc(EMdiv:'X');
             EndIf;
          EndIf;

       EndIf;

      /End-Free

     p EmployerSrch    e

     p LoadARCatTyp    b
      /Free
       scARTypSel = *Blanks;
       scARTypSel = 'All';
       scARTypVal = *Blanks;
       strARTyDsc = *Blanks;

       // Read WTCTL file from starting
        Setll intCo# pct00;
        Reade intCo# pct00;
        Dow not %Eof(wtctl);
           If CTstat = 'A';
              strARTyDsc = TitleCase(CTdesc);
              scARTypSel = %trim(scARTypSel)+','+%trim(CTcTyp)+'-'+
                           %trim(strARTyDsc);
              scARTypVal = %trim(scARTypVal)+','+%trim(CTcTyp);
           EndIf;
        Reade intCo# pct00;
        EndDo;
      /End-Free
     p LoadARCatTyp    e

     p LoadARCodes     b
      /Free
       scARCodSel = *Blanks;
       scARCodVal = *Blanks;
       strARCdDsc = *Blanks;

       // Read WTTTP file from starting
       Chain(n) intCo# ptt01;
       If %found(wtabl);
          Dow not %Eof(wtabl);
             If TTstat = 'A';
                strARCdDsc = TitleCase(TTdesc);
                scARCodSel = %trim(scARCodSel)+','+%trim(TTkey)+'-'+
                             %trim(strARCdDsc);
                scARCodVal = %trim(scARCodVal)+','+%trim(TTkey);
             endif;
          ReadE intCo# ptt01;
          enddo;
       endif;

      /End-Free
     p LoadARCodes     e
