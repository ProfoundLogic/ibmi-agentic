     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h nomain
     h DFTNAME(WPIPOLCPY)
     h option(*nodebugio: *srcstmt)
     H bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:  This Program process the policy copy.
     h*
     h*  Called from:
     h*  Calls to:
     h*
     h*
     h*  Created on 9/17/2015 by c:302 for Project WC2254
     h****************************************************************
     h* Revisions:
     h*
     h* 05/23/24 C:300 Support: 115634 (SUNZ)
     h*                Update WMFNP Maintenance fields while updating
     h*                Next Available Employer Number
     h*
     h* 05/22/24 C:079 Support: 115510 - 146
     h*                Rollover to Quote then copy division to another
     h*                Quote...check Live for next avail division Num.
     h*
     h* 11/21/23 C:342 Support: 114536 (Cove Risk)
     h*               -Show new pop-up message "This quote has already been
     h*                submitted to xxxxxxx." when copying employer.
     h*                "xxxxxxxx" will be the name of the 3rd-party
     h*                payment provider
     h*
     h* 11/06/23 C:333 Project: WC2926
     h*               -Policy copy and search enhancements.
     h*                DSPF changes -
     h*                  WPIPOLCPY : Copy all divisions(new checkbox).
     h*
     h* 10/28/22 C:300 Support: 104084 (LUBA)
     h*               -GetRatingState() procedure in WPIEMPLR not returning
     h*                the ratestate, when trying to copy Renewal Pending
     h*                Quote to different Group
     h*               -Check for %found after chain to avoid program blow-up
     h*                on Update when copying to same Group
     h*               -Clear ELfQte flag for new quote
     h*               -Modify the condition to check Final Bill status
     h*               -In DSPF: In From Policy Year, Remove 'All Years' option
     h*                and make it mandatory for input
     h*
     h* 01/26/22 C:65  Project: Phw#100161 (SCS)
     h*                Remove client 146 exception on updating initial
     h*                employer effective date.  WC2254 allowed the client
     h*                to choose on prompt screen of maintaining initial
     h*                effective date or not.
     h*
     h* 05/04/21 C:062 Project: WC2663
     h*                Don't copy if the final bill has run
     h*
     h* 06/15/20 C:298 Support: 89438
     h*                Copy Large Deductible Info
     h*
     h* 05/16/18 C:33  Support: 70972
     h*                Release group record lock
     h*
     h* 05/11/18 C:291 Project: C150WCR
     h*                Clear Master/MCP override flag.
     h*
     h* 02/21/18 C:136 Project: WC2588
     h*                Clear quote transfer date
     h*
     h* 04/07/17 C:062 Project 136WHF
     h*                Add Territory Rating for CareWest.
     h*
     h* 01/09/17 C:136 Support: 44983
     h*                Correct new/renewal logic
     h*
     h* 02/29/16 C:136 Support: 40016
     h*                Change div dropdown box to a text box and SQL
     h*                to select only one division in divChoices so that
     h*                selected division will copy instead of just div 0
     h**********************************************************************
     Fwpipolcpy CF   E             Workstn HANDLER('PROFOUNDUI(HANDLER)')
     F                                     SFILE(GrdValChg:rrnValChg)
     F                                     SFILE(grdError:rrnErrorG)
     fwdelp     if a e             Disk    Rename(PEL01:PEL0P)
     fwmeml     if   e           k Disk
     fwmemp     uf a e             Disk    Rename(PEM01:PEM0P)
     fwdell     uf   e           k Disk    infds(dbfds) rename(pel01:pel00)

     Fwmfnl     uf   e           k Disk
     fwdf1l1    if   e           k disk
     Fwmahl     if a e           k Disk
     Fwmarl     if   e           k Disk
     Fsmcol     if   e           k Disk
     Fwmepl     if a e           k Disk
     Fwaqtl7    if a e           k Disk
     Fwmool     if a e           k Disk
     Fwzexp     o    e             Disk
     Fwma3l     if a e           k Disk
     Fwmall     if a e           k Disk
     Fwmanl     if a e           k Disk
     Fwmecl     if a e           k Disk
     Fwmnbp     if a e           k Disk
     Fwdrtp     if a e           k Disk
     Fwmldl1    if a e           k Disk
     Fwtlml1    if   e           k Disk
     Fwtddl2    if   e           k Disk
     Fwdrrl     if   e           k Disk    rename(prr01:prr00)
     Fwmrsl     if   e           k Disk    rename(prs01:prs00)

     D/copy wPrEmplr
     D/copy wPrAgent
     D/copy wPrGenPop
     D/copy wPrPolCpy
     d/copy wPrUsrPrms
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrModule
     d******  Beg Add  ***  05/22/24  *************************
     d/copy sPrFiles
     d******  End Add  ***  05/22/24  *************************

     d psds          esds                  extname(szq1p)
     d today           s               d
     d today7          s              7  0
     d xxTime          s              6  0
     D hldStRp         s              1
     D hldnwrw         s              1
     D hldEmp#         s              9  0
     D hldDiv          s              5  0
     D hldFyr          s              3  0
     D isError         s               n
     D isRenewal       s               n
     D  inRRN          s              9S 0
     D  inQuote        s              1A
     D xxClt#          s              3A
     D xxtemp          s              1N
      *** Beg Add ***** 05/22/24 *******************************************
     D outFileLib      s             10a
     D strLibFile      s             25a
     D strSql          s            256a
     D chrCo#          s              3a
     D chrFnd          s              3a
     D chrEmp#         s              9a
     D divNmbLive      s              5s 0
      *** End Add ***** 05/22/24 *******************************************

     D dbfds           ds
     D  elRRN                397    400b 0

       //===================================================================
       // Procedure Definition Section
       //===================================================================

     D* --------------------------------------------------
     D* Prototype for procedure: ExecCommand
     D* --------------------------------------------------
     D ExecCommand     PR                  Extproc('QCMDEXC')
     D  command                     255A   Const
     D  cmdLenght                    15S 5 Const

     D* --------------------------------------------------
     D* Prototype for procedure: CopyBasisError
     D* --------------------------------------------------
     D CopyBasisError  PR

     D* --------------------------------------------------
     D* Prototype for procedure: WriteError
     D* --------------------------------------------------
     D WriteError      PR
     D  strError                    255A   Const

     D* --------------------------------------------------
     D* Prototype for procedure: WriteNewPolicy
     D* --------------------------------------------------
     D WriteNewPolicy  PR

     D  isFromQuote    s              1N

     D* --------------------------------------------------
     D* Prototype for procedure: WriteRecords
     D* --------------------------------------------------
     D WriteRecords    PR
     d*** Beg Add 05/22/24 *******************************
     d GetObjLib       pr                  extpgm('RTVOBJLIB')
     d Object                        10    const
     d Type                           7    const
     d Library                       10
     d*** End Add 05/22/24 *******************************

     D* --------------------------------------------------
     D* Prototype for procedure: GetObjLib
     D* --------------------------------------------------

     D* --------------------------------------------------
     D* Prototype for procedure: ReviewPol
     D* --------------------------------------------------
     D ReviewPol       PR                  Extpgm('WTPOLQTE')
     D  outRrn                        9A   Const
     D  outMode                       1A   Const
     D  outNext                      10A   Const
     D  outTabIndex                   5A   Const
     D  outQuote                      1A

     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltID                       3

     P* --------------------------------------------------
     P* Procedure name: CopyPolicy
     P* Purpose:        Copy a policy
     P* Returns:
     P* Parameter:      inRRN => Relative record number of employer
     P* Parameter:      inQuote => Q for Quote
     P* Parameter:      inRRNType => Detail/Master
     P* --------------------------------------------------
     P CopyPolicy      B                   Export
     D CopyPolicy      PI
     D  inRRN                         9  0
     D  inQuote                       1A   Options(*nopass)
     D  inRRNType                    10A   Options(*nopass)

     D  strRRNType     s             10A
     D  strMsg         s            256A
     d  message        s            256a   Inz
     d  title          s             50a   Inz('Quote Copy')
     d  str3rdPPmt     s            256    inz varying
     d  isQteCopy      s               n   inz
     D  invAmt         s              9  2

      /Free
       Exec Sql Set Option Commit = *none;

       today7 = DateTo7(systemDate());
       xxTime = TimeTo6(systemTime());

       GetClientID(xxClt#);

       isFromQuote = *off;
       If %parms >= 2 and inQuote = 'Q';
          isFromQuote = *on;
       Endif;

       strRRNType = 'Detail';
       If %parms >= 3 and inRRNType <> *blank;
          strRRNType = inRRNType;
       Endif;

       Select;
          When strRRNType = 'Detail';
             Chain inRRN wdelp;
             chain (ELco#:ELfnd:ELemp#:ELdiv) wmeml;
             // If this is calld from quote and final bill has run stop
             // copy
             // Check final bill run status from WDEAP_HLD (Holding area for Invoice Cloud)
             invAmt = 0;
             Exec Sql Select EAamt Into :invAmt
                      From WDEAP_HLD Where EAco#=:ELco# And
                      EAfnd=:ELfnd And EAemp#=:ELEmp# And
                      EAdiv=:ELdiv And EAipgm='WPIPMTSVC';
             If isFromQuote and invAmt <> 0;
                str3rdPPmt = GetModuleProperty('Invoice Cloud':'Name');
                If str3rdPPmt = '1' and str3rdPPmt = *blanks;
                   str3rdPPmt = 'Invoice Cloud';
                Endif;
                message  = 'This quote has already been submitted to ' +
                           %trim(str3rdPPmt);
                isQteCopy = PopTwoBtn(message:title:'Proceed':'Cancel');
                Select;
                When isQteCopy = *On;
                When isQteCopy = *Off;
                   Return;
                Endsl;
             EndIf;
          When strRRNType = 'Master';
             Chain(n) inRRN wmemp;
             ELco# = EMco#;
             ELfnd = EMfnd;
             ELemp# = EMemp#;
             ELdiv = EMdiv;
       Endsl;

       hldEmp# = EMemp#;
       Chain(n) (EMco#:EMfnd) wmfnl;

       isDspGrid = *off;
       isError = *off;
       scFrmDiv = EMdiv;
       // Set visibility of Blank Option in 'From Policy Year' dropdown
       scBlkOpt = 'false';
       If xxClt# = '114' and inQuote <> 'Q';
          scBlkOpt = 'true';
       Endif;

       fyrChoices = 'ELco# = ' + %char(EMco#) + ' AND ELfnd = '
        + %char(EMfnd) + ' AND ELemp# = ' + %char(EMemp#)
        + ' AND ELdiv = ' + %char(EMdiv);

       divChoices = 'EMco# = ' + %char(EMco#) + ' AND EMfnd = '
        + %char(EMfnd) + ' AND EMemp# = ' + %char(EMemp#)
        + ' AND EMdiv = ' + %char(EMdiv);

       scFrmGrp =  FNfnd;
       scFrmDiv = EMdiv;
       scAutoAsg = 'Y';
       scNxtDvNm = 'Y';

       GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

       scToGrp =  FNfnd;

       Chain EMco# SMCOL;

       scMntOrgDt = COmaintIdt;
       scMntNwRnw = COmaintnew;

       clrErrorG = *on;
       dspErrorG = *off;
       Write recCpyEmp;
       clrErrorG = *off;
       dspErrorG = *on;
       rrnErrorG = 1 ;

       Dou (isError = *off and
        BtnSubmit = *on) or
        BtnCancel = *on;

          exfmt recCpyEmp;

          clrErrorG = *on;
          dspErrorG = *off;
          Write recCpyEmp;
          clrErrorG = *off;
          dspErrorG = *on;
          rrnErrorG = 1 ;

          CopyBasisError();

       Enddo;

       If BtnSubmit = *On;

          WriteNewPolicy();

          // Notify user of value changes to employer/division
          rrnValChg = *zeros;
          clrValChg = *on;
          dspValChg = *off;
          Write CopyChange;
          clrValChg = *off;
          dspValChg = *on;


          If hldStRp <> EMstrp;
             rrnValChg = rrnValChg + 1;
             scValueChg = 'State Reporting Division changed from ' + hldStRp
              + ' to ' + EMstrp ;
             Write grdValChg;
          Endif;

          If hldnwrw <> ELnwrw;
             rrnValChg = rrnValChg + 1;
             scValueChg = 'New/Renewal Flag changed from ' + hldnwrw
              + ' to ' + ELnwrw ;
             Write grdValChg;
          Endif;

          If rrnValChg > *zeros;
             exfmt CopyChange;
          Endif;

          //
          If scCpyAlDiv <> 'Y';
             strMsg = 'Employer information copied to Group: ' +
              %Char(sctoGrp) + '  Employer: ' + %Char(scEmpNmb) +
              '  Division: ' + %Char(scDivNmb);
          Else;
             strMsg = 'Employer information copied to Group: ' +
              %Char(sctoGrp) + '  Employer: ' + %Char(scEmpNmb);
          Endif;

          If Not PopTwoBtn(strMsg:'Policy Copy Confirmation':'Ok':'Review');
             Chain(n) (ELco#:scToGrp:scEmpNmb:scDivNmb) wdell;
             ReviewPol(%EditC(elRRN:'X'):'C':'          ':'':inQuote);
          Endif;

       EndIf;

      /End-Free

     P CopyPolicy      E


     P* --------------------------------------------------
     P* Procedure name: CopyBasisError
     P* Purpose:        Check for errors in the copy
     P* Returns:
     P* --------------------------------------------------
     P CopyBasisError  B
     D CopyBasisError  PI

     D empRRN          s              9  0
     D rateState       s              2
     D rateDate7       s              7  0
     D rateDateUSA     s               d   datfmt(*usa)
     D stRptFlg        s              1

      /Free

       isDspGrid = *off;
       isError = *off;
       errEmpInd1 = *off;
       errEmpInd2 = *off;
       errDivInd = *off;
       erFrmFyr = *off;
       msFrmFyr = *blanks;

       // From Policy Year can't be Blank
       If scBlkOpt = 'false' and scFrmFyr = 0;
          erFrmFyr = *on;
          msFrmFyr = 'WCR0502';
          isError = *on;
       Endif;

       If scAutoAsg <> 'Y';

          //  Error if not auto-assign and no employer entered.
          If scEmpNmb = *Zeros;
             isDspGrid = *on;
             isError = *on;
             errEmpInd1 = *on;
          Endif;

          //  Error if not auto-assign div=0 but group does not allow for div=0
          Chain(n) (EMco#:sctoGrp) wmfnl;
          If scDivNmb = *zeros and FNdiv0 = 'N';
             isDspGrid = *on;
             isError = *on;
             errDivInd = *on;
          Endif;

          //  Error if 'To employer/Div' already exist and not auto-assign Div
          If scNxtDvNm <> 'Y';
             Chain (EMco#:sctoGrp:scEmpNmb:scDivNmb) wmeml;
             If %Found(wmeml);
                isDspGrid = *on;
                isError = *on;
                errEmpInd2 = *on;
                Chain(n) inRRN wmemp;
             Endif;
          Endif;

          //  Ensure that state rpt divisions exists
          //   if copying a division to an existing
          //   policy
          Chain (EMco#:scToGrp:scEmpNmb) wmeml;
          If %found;
             GetStRptDv(EMco#:scToGrp:scEmpNmb:empRRN:stRptFlg);
             If stRptFlg <> 'Y';
                WriteError('Policy being copied to does not have -
                 a state reporting division.');
             Endif;
          Endif;
       Endif;

       If scFrmGrp <> sctoGrp;
          //
          // Ensure fund year exists for new group
          //
          Setll (ELco#:ELfnd:ELemp#:ELdiv) wdell;
          Reade(n) (ELco#:ELfnd:ELemp#:ELdiv) wdell;
          Dow Not %EOF(wdell);

             If scFrmFyr <> *Zeros and ELfyr <> scFrmFyr;
                reade(n) (ELco#:ELfnd:ELemp#:ELdiv) wdell;
                iter;
             Endif;

             Setgt (ELco#:scToGrp:ELpfdt) wdf1l1;
             Readpe (ELco#:scToGrp) wdf1l1;
             If %EOF(wdf1l1) or ELpfdt >= F1enpo;
                WriteError('Year ' + %Char(ELfyr)
                 + ' not available for group ' + %Char(scToGrp));
             EndIf;

             Leave;

          Reade(n) (ELco#:ELfnd:ELemp#:ELdiv) wdell;

          Enddo;

          //
          //  Ensure Rating and Statistics record exists for new group
          //

          Clear rateState;
          Chain (ELco#:ELrtcd) prs00;
          If %found(wmrsl);
             rateState = RSjrst;
          Endif;

          If ELandt <> 0;
             rateDate7 = ELandt;
          Else;
             rateDate7 = ELpfdt;
          Endif;

          rateDateUSA  = %Date(rateDate7:*CYMD);
          Setgt (ELco#:scToGrp:rateState:rateDate7) prr00;
          Readpe (ELco#:scToGrp:rateState) prr00;
          If %Eof;
             WriteError('Ratings and Statistics is not ' +
              'set up for ' + %char(rateDateUSA) + ' in  group ' +
              %char(scToGrp) + ' for ' + rateState + '.');
          Endif;

          //
          //  Get LAST active agent history record for FROM employer
          //
          Setgt (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) wmahl;
          Readpe (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) wmahl;

          Dow Not %EOF(wmahl) and AHstat <> 'A';
             Readpe (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) wmahl;
          Enddo;

          If AHagt# <> *Zeros;

             If Not ValidAgt#(AHagt#:scToGrp:ELco#);
                WriteError('Primary Agent ' + %Char(AHagt#) + ' in year '
                 + %Char(ELfyr) + ' not available for group ' +
                 %Char(scToGrp));
             Endif;

             Chain (AHco#:scToGrp:AHrt#) wmarl;
             If Not %found;
                WriteError('Primary Agent Rate ' + %Char(AHrt#) + ' in year '
                 + %Char(ELfyr) + ' not available for group ' +
                 %Char(scToGrp));
             Endif;

          Endif;

          If AHsagt <> *Zeros;

             If Not ValidAgt#(AHsagt:scToGrp:ELco#);
                WriteError('Secondary Agent ' + %Char(AHsagt) + ' in year '
                 + %Char(ELfyr) + ' not available for group ' +
                 %Char(scToGrp));
             Endif;

             Chain (AHco#:scToGrp:AHsrt#) wmarl;
             If Not %found;
                WriteError('Secondary Agent Rate ' + %Char(AHsrt#) + ' in year '
                 + %Char(ELfyr) + ' not available for group ' +
                 %Char(scToGrp));
             Endif;

          Endif;

          If AH3agt <> *Zeros;

             If Not ValidAgt#(AH3agt:scToGrp:ELco#);
                WriteError('Tertiary Agent ' + %Char(AH3agt) + ' in year '
                 + %Char(ELfyr) + ' not available for group ' +
                 %Char(scToGrp));
             Endif;

             Chain (AHco#:scToGrp:AH3rt#) wmarl;
             If Not %found;
                WriteError('Tertiary Agent Rate ' + %Char(AH3rt#) + ' in year '
                 + %Char(ELfyr) + ' not available for group ' +
                 %Char(scToGrp));
             Endif;

          Endif;

          If AHagrc <> *Zeros;

             If Not ValidAgt#(AHagrc:scToGrp:ELco#);
                WriteError('Primary Agent of record' + %Char(AHagrc)
                 + ' in year ' + %Char(ELfyr) + ' not available for group ' +
                 %Char(scToGrp));
             Endif;

             Chain (AHco#:scToGrp:AHrtrc) wmarl;
             If Not %found;
                WriteError('Primary Agent of record Rate ' + %Char(AHrtrc)
                 + ' in year ' + %Char(ELfyr) + ' not available for group ' +
                 %Char(scToGrp));
             Endif;

          Endif;

          If AH2arc <> *Zeros;

             If Not ValidAgt#(AH2arc:scToGrp:ELco#);
                WriteError('Secondary Agent of record ' + %Char(AH2arc)
                 + ' in year ' + %Char(ELfyr) + ' not available for group ' +
                 %Char(scToGrp));
             Endif;

             Chain (AHco#:scToGrp:AH2rrc) wmarl;
             If Not %found;
                WriteError('Secondary Agent of record Rate ' + %Char(AH2rrc)
                 + ' in year ' + %Char(ELfyr) + ' not available for group ' +
                 %Char(scToGrp));
             Endif;

          Endif;

          If AH3arc <> *Zeros;

             If Not ValidAgt#(AH3arc:scToGrp:ELco#);
                WriteError('Tertiary Agent of record ' + %Char(AH3arc)
                 + ' in year ' + %Char(ELfyr) + ' not available for group ' +
                 %Char(scToGrp));
             Endif;

             Chain (AHco#:scToGrp:AH3rrc) wmarl;
             If Not %found;
                WriteError('Tertiary Agent of record Rate ' + %Char(AH3rrc)
                 + ' in year ' + %Char(ELfyr) + ' not available for group ' +
                 %Char(scToGrp));
             Endif;

          Endif;

       Endif;

      /End-Free

     P CopyBasisError  E

     P* --------------------------------------------------
     P* Procedure name: WriteError
     P* Parameter:      strError
     P* --------------------------------------------------
     P WriteError      B
     D WriteError      PI
     D  strError                    255A   Const
      /Free

         scEmpCpyRs = strError;
         Write grdError;
         rrnErrorG = rrnErrorG + 1;
         isDspGrid = *on;
         isError = *on;

      /End-Free

     P WriteError      E

     P* --------------------------------------------------
     P* Procedure name: WriteNewPolicy
     P* Purpose:
     P* Returns:
     P* --------------------------------------------------
     P WriteNewPolicy  B
     D WriteNewPolicy  PI
      /Free

       Chain (EMco#:sctoGrp) wmfnl;

       If scAutoAsg = 'Y';
          If isFromQuote;
             scEmpNmb = FNnaqn;
             FNnaqn = FNnaqn + 1;
          Else;
             scEmpNmb = FNnaen;
             FNnaen = FNnaen + 1;
          Endif;

          //*** Beg Add ***** 05/23/24 ***********************
          FNpgm  = Q1pgm;
          FNuser = Q1user;
          FNtime = Timeto6(SystemTime());
          FNdt   = Dateto7(SystemDate());
          //*** End Add ***** 05/23/24 ***********************

          Update pfn01;

          If scNxtDvNm = 'Y';
             If FNdiv0 = 'N';
                scDivNmb = 1;
             Else;
                scDivNmb = *zeros;
             Endif;
          EndIf;

       Else;
          Unlock wmfnl;

          If scNxtDvNm = 'Y';
             //*** Beg Add *** 05/22/24 ********************************
             // Retrieve the Live file library using Group file name and
             // build lib/file variable
             Monitor;
                GetObjLib('WMFNL':'*FILE':outFileLib);
                strLibFile = %Trim(outFileLib) + '/' + 'WMEMP';
             On-Error;
             Endmon;

             // Define global variable to hold Max Division Number using Live
             // file library
             chrCo# = %Editc(EMco#:'Z');
             chrFnd = %Editc(scToGrp:'Z');
             chrEmp# = %Editc(scEmpNmb:'Z');

             Exec Sql Create Or Replace Variable Max_Div_Num Decimal(5,0);
             strSql = 'Set Max_Div_Num = (Select Max(EMdiv) From ' +
              %Trim(strLibFile) + ' Where EMco# = ' + %Trim(chrCo#) +
              ' and EMfnd = ' + %Trim(chrFnd) + ' and EMemp# = ' +
              %Trim(chrEmp#) + ')';

             Exec Sql Execute Immediate :strSql;

             // Load global variable into work field
             Exec sql Set :DivNmbLive = Max_Div_Num;
             Exec Sql Drop Variable Max_Div_Num;
             //*** End Add *** 05/22/24 ********************************

             Setgt (EMco#:scToGrp:scEmpNmb) wmeml;
             Readpe (EMco#:scToGrp:scEmpNmb) wmeml;
             //*** Beg Chg *** 05/22/24 ********************************
             //***scDivNmb = EMdiv + 1;
             If EMdiv >= DivNmbLive;
                scDivNmb = EMdiv + 1;
             Else;
                scDivNmb = DivNmbLive + 1;
             Endif;
             //*** End Chg *** 05/22/24 ********************************
          Endif;

       Endif;
       If scCpyAlDiv = 'Y';
          xxtemp = *Off;
          setll (EMco#:scFrmGrp:hldEmp#) wmeml;
          Reade(n) (EMco#:scFrmGrp:hldEmp#) wmeml;
          Dow not %eof;
             If scAutoAsg <> 'Y';
                If xxtemp <> *Off;
                   scDivNmb += 1;
                Else;
                   scDivNmb = 0;
                Endif;
             Else;
                scDivNmb = EMdiv;
             Endif;
             scFrmDiv = EMdiv;
             WriteRecords();
             xxtemp = *On;
             Reade(n) (EMco#:scFrmGrp:hldEmp#) wmeml;
          Enddo;
       Else;
          WriteRecords();
       Endif;

      /End-Free
     P WriteNewPolicy  E

     P* --------------------------------------------------
     P* Procedure name: WriteRecords
     P* Purpose:
     P* Returns:
     P* --------------------------------------------------
     P WriteRecords    B
     D WriteRecords    PI

     D isChgGrp        s               n
     D isStRptExists   s               n
     D ratingState     S              2A
     D strKey          S             10A
     D xi              s              5  0
     D xxEfDt          s              7  0
     D xxNewFyr        s              3  0
     D xxNwRnw         s              1
     D xxQuote         s              1
     D xxStRptDiv      s              5  0

      /Free

       If isFromQuote;
          xxQuote = 'Q';
       Endif;

       Chain (EMco#:scToGrp:scEmpNmb) wmeml;
       isStRptExists = %found;
       Chain (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wmeml;

       hldStRp = EMstrp;

       //  Maintain Original Inception Date
       If scMntOrgDt = 'Y';
          xxEfDt = EMefd1;
       Else;
          If scFrmFyr = *Zeros;
             Chain(n) (EMco#:scFrmGrp:EMemp#:scFrmDiv) wdell;
          Else;
             Chain(n) (EMco#:scFrmGrp:EMemp#:scFrmDiv:scFrmFyr) wdell;
          EndIf;
          xxEfDt = ELpfdt;
       Endif;

       If isStRptExists;
          EMstrp = 'N';
       Else;
          EMstrp = 'Y';
       Endif;

       EMfnd = scToGrp;
       EMemp# = scEmpNmb;
       EMdiv = scDivNmb;
       Clear EMrqfl;
       Clear EMrdfl;

       If xxEfDt <> 0;
          EMefd1 = xxEfDt;
          Clear EMefd2;
          Clear EMefd3;
          Clear EMefd4;
          Clear EMefd5;
          Clear EMcad1;
          Clear EMcad2;
          Clear EMcad3;
          Clear EMcad4;
          Clear EMcad5;
       Endif;
       EMuser = q1User;
       EMiusr = q1User;
       EMpgm = q1pgm;
       EMipgm = q1pgm;
       EMdt = today7;
       EMidt = today7;
       EMtime = xxTime;
       EMitim = xxTime;
       Clear EMqtfr;
       Clear EMovrmp;
       Write pem0p;

       //  Write Cross Reference Record if LIVE environment
       If SCFrmgrp <> scToGrp or hldEmp# <> scEmpNmb;
          EXco# = EMco#;
          EXfnd = sctoGrp;
          EXEmp# = scEmpNmb;
          EXDiv = scDivNmb;
          EXofnd = scFrmGrp;
          EXoEmp = hldEMp#;
          EXoDiv = scFrmDiv;
          EXuser = q1User;
          EXiusr = q1User;
          EXpgm = q1pgm;
          EXdt = today7;
          EXidt = today7;
          EXtime = xxTime;
          Write pex01;
          Feod wzexp;
       Endif;

       //  Policy detail ...

       If scFrmFyr = *Zeros;
          Setll (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wdell;
          Reade(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wdell;
       Else;
          Setll (EMco#:scFrmGrp:hldEmp#:scFrmDiv:scFrmFyr) wdell;
          Reade(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv:scFrmFyr) wdell;
       EndIf;

       DoW not %EOF(wdell);
          ratingState = GetRatingState(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
          hldNwRw = ELnwrw;
          hldDiv  = ELdiv;
          hldFyr  = ELfyr;

          If scFrmGrp <> sctoGrp;
             isChgGrp = *on;
             Setgt (ELco#:scToGrp:ELpfdt) wdf1l1;
             Readpe (ELco#:scToGrp) wdf1l1;
             ELfyr = F1fyr;
          EndIf;
          xxNewFyr = ELfyr;

          //  Set the new record for the detail file
          ELfnd  = sctoGrp;
          ElEmp# = scEmpNmb;
          ELdiv  = scDivNmb;

          If isChgGrp;

             Setgt (ELco#:ELfnd:ratingState:ELpfdt) prr00;
             Readpe (ELco#:ELfnd:ratingState) prr00;
             // Clear dev #1 if not set up in new group
             If RRdv1c = 0;
                Clear ELasp2;
                Clear ELdvef;
                Clear ELdvex;
                Clear ELd1s2;
                Clear ELd1d2;
                Clear ELd1x2;
             Endif;
             // Clear dev #2 if not set up in new group
             If RRdv2c = 0;
                Clear ELdev2;
                Clear ELd2ef;
                Clear ELd2ex;
                Clear ELd2s2;
                Clear ELd2d2;
                Clear ELd2x2;
                Clear ELdev2F;
                Clear ELdev2BP;
                Clear ELdev2AP;
             Endif;
             // Clear waiver if not set up in new group
             If RRwvac = 0;
                Clear ELwvtp;
                Clear ELwvpc;
                Clear ELfchg;
                Clear ELwmin;
                Clear ELwmax;
                Clear ELwvfl;
             Endif;
             // Clear subject prem 1a if not set up in new group
             If RR1acd = 0;
                Clear ELmcef;
                Clear ELmcex;
                Clear ELaspm;
                Clear ELmccd;
                Clear ELmcgp;
             Endif;
             // Clear subject prem 1b if not set up in new group
             If RR1bcd = 0;
                Clear ELscef;
                Clear ELscex;
                Clear ELasps;
             Endif;
             // Clear debit credit if not set up in new group
             If RRdbcd = 0 and RRcrcd = 0;
                Clear ELsdef;
                Clear ELsdex;
             Endif;
             // Clear Facultative Reinsurance if not set up in new group
             If RRarcc = 0;
                Clear ELarcp;
                Clear ELaref;
                Clear ELarex;
             Endif;
             // Clear contractor's credit if not set up in new group
             If RRcccd = 0;
                Clear ELasp1;
                Clear ELcref;
                Clear ELcrex;
             Endif;
             // Clear voluntary labor charge if not set up in new group
             If RRvlcc = 0;
                Clear ELvlca;
             Endif;
             //  Check the limits tables
             strKey = ELilcd;
             Setgt (ELco#:ELfnd:ratingState:strKey:ELpfdt) wtlml1;
             Readpe (ELco#:ELfnd:ratingState:strKey) wtlml1;
             Dow not %EOF(wtlml1) and LMstat = 'I';
                Readpe (ELco#:ELfnd:ratingState:strKey) wtlml1;
             Enddo;
             If %EOF(wtlml1);
                Clear ELilcd;
                Clear ELilef;
                Clear ELilex;
             Endif;

             strKey = ELinl2;
             Setgt (ELco#:ELfnd:ratingState:strKey:ELpfdt) wtlml1;
             Readpe (ELco#:ELfnd:ratingState:strKey) wtlml1;
             Dow not %EOF(wtlml1) and LMstat = 'I';
                Readpe (ELco#:ELfnd:ratingState:strKey) wtlml1;
             Enddo;
             If %EOF(wtlml1);
                Clear ELinl2;
                Clear ELi2ef;
                Clear ELi2ex;
             Endif;

             //  Check deductible discount tables
             strKey = ELdcde;
             Setgt (ELco#:ELfnd:ratingState:strKey:ELpfdt) wtddl2;
             Readpe (ELco#:ELfnd:ratingState:strKey) wtddl2;
             Dow not %EOF(wtddl2) and DDstat = 'I';
                Readpe (ELco#:ELfnd:ratingState:strKey) wtddl2;
             Enddo;
             If %EOF(wtddl2);
                Clear ELdcde;
                Clear ELdcef;
                Clear ELdcex;
             Endif;

             strKey = ELdcde;
             Setgt (ELco#:ELfnd:ratingState:strKey:ELpfdt) wtddl2;
             Readpe (ELco#:ELfnd:ratingState:strKey) wtddl2;
             Dow not %EOF(wtddl2) and DDstat = 'I';
                Readpe (ELco#:ELfnd:ratingState:strKey) wtddl2;
             Enddo;
             If %EOF(wtddl2);
                Clear ELdcd2;
                Clear EL2def;
                Clear EL2dex;
             Endif;

          Endif;  //isChgGrp

          Clear ELdisp;
          Clear ELmrfl;
          Clear ELrqfl;
          Clear ELfaf;
          Clear ELmapr;
          Clear ELampr;
          Clear ELsram;
          Clear ELesbj;
          Clear ELasbj;
          Clear ELstpr;
          Clear ELaspr;
          Clear ELempm;
          Clear ELampm;
          Clear ELenmp;
          Clear ELanmp;
          Clear ELanpr;
          Clear ELaapr;
          Clear ELinmp;
          Clear ELemnp;
          Clear ELamnp;
          Clear ELtxpr;
          Clear ELtxdu;
          Clear ELprtd;
          Clear ELprtf;
          Clear ELtclp;
          Clear ELt#cl;
          Clear ELtclc;
          Clear ELouts;
          Clear ELewav;
          Clear Elawav;
          Clear ELedc1;
          Clear ELadc1;
          Clear ELedc2;
          Clear ELadc2;
          Clear ELd1e2;
          Clear ELd1a2;
          Clear ELedv2;
          Clear ELadv2;
          Clear ELd2e2;
          Clear ELd2a2;
          Clear ELedc3;
          Clear ELadc3;
          Clear ELexp1;
          Clear ELexp2;
          Clear ELexp3;
          Clear ELaxp1;
          Clear ELaxp2;
          Clear ELaxp3;
          Clear ELrpe1;
          Clear ELrpe2;
          Clear ELrpe3;
          Clear ELrpa1;
          Clear ELrpa2;
          Clear ELrpa3;
          Clear ELein1;
          Clear ELain1;
          Clear ELein2;
          Clear ELain2;
          Clear ELedct;
          Clear ELadct;
          Clear ELedcm;
          Clear ELadcm;
          Clear ELedcs;
          Clear ELadcs;
          Clear ELedcr;
          Clear ELadcr;
          Clear ELedc;
          Clear ELadc;
          Clear ELemin;
          Clear ELamin;
          Clear ELemax;
          Clear ELamax;
          Clear ELstdt;
          Clear ELfset;
          Clear ELstam;
          Clear ELretn;
          Clear ELefrs;
          Clear ELafrs;
          Clear ELsbfe;
          Clear EL1apr;
          Clear EL2apr;
          Clear EL2apr;
          Clear EL3apr;
          Clear EL5apr;
          Clear EL6apr;
          Clear EL7apr;
          Clear ELanca;
          Clear ELancm;
          Clear ELancf;
          ELuser = q1User;
          ELiusr = q1User;
          ELpgm  = q1pgm;
          ELipgm = q1pgm;
          ELdt   = today7;
          ELidt  = today7;
          ELtime = xxTime;
          ELitm  = xxTime;
          Clear ELqtfr;
          Clear ELfQte;
          Write pel0p;
          FEOD  wdelp;

          Select;
          When isStRptExists;
           // Divisions copied to an existing employer get
           //  the state reporting division's status
             xxStRptDiv = GetStRptDv(ELco#:ELfnd:ELemp#);
             Chain(n) (ELco#:ELfnd:ELemp#:xxStRptDiv:ELfyr) pel00;
             xxNwRnw = ELnwrw;
             Chain (ELco#:ELfnd:ELemp#:scDivNmb:ELfyr) pel00;
             If %found(wdell);
             ELnwrw = xxNwRnw;
             Update pel00;
             EndIf;
          When scMntNwRnw <> 'Y';
             isRenewal = ChkRenewal(ELco#:ELfnd:ELemp#:ELfyr:xxQuote);
             If isRenewal;
                xxNwRnw = 'R';
             Else;
                xxNwRnw = 'N';
             Endif;
             Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pel00;
             If %found(wdell);
             ELNwRw = xxNwRnw;
             Update pel00;
             EndIf;
          Endsl;

          If ELnwrw = 'N';
             NBco# = ELco#;
             NBfnd = ELfnd;
             NBemp# = ELemp#;
             NBrcdt = today7;
             NBendt = today7;
             NBuser = q1User;
             NBiusr = q1User;
             NBpgm = q1pgm;
             NBipgm = q1pgm;
             NBdt = today7;
             NBidt = today7;
             NBtime = xxTime;
             NBitm = xxTime;
             Write pnb01;
          Else;
             RTco# = ELco#;
             RTfnd = ELfnd;
             RTemp# = ELemp#;
             RTfyr = ELfyr;
             RTycdt = today7;
             RTuser = q1User;
             RTiusr = q1User;
             RTpgm = q1pgm;
             RTipgm = q1pgm;
             RTdt = today7;
             RTidt = today7;
             RTtime = xxTime;
             RTitm = xxTime;
             Write prt01;
          Endif;

          // Copy last active records from the Agent History File, each year
          Setgt (ELco#:scFrmGrp:hldEmp#:hldDiv:hldFyr) wmahl;
          Readpe (ELco#:scFrmGrp:hldEmp#:hldDiv:hldFyr) wmahl;
          Dow not %EOF(wmahl) and AHstat = 'I';
             Readpe (ELco#:scFrmGrp:hldEmp#:hldDiv:hldFyr) wmahl;
          Enddo;

          If not %EOF(wmahl);
             AHfnd = scToGrp;
             AHEmp# = scEmpNmb;
             AHDiv = scDivNmb;
             AHfyr = xxNewFyr;
             If xxEfDt <> 0;
                AHefdt = xxEfDt;
                If today7 < AHefdt;
                   AHefdt = today7;
                Endif;
             Endif;
             AHuser = q1User;
             AHiusr = q1User;
             AHpgm = q1pgm;
             AHipgm = q1pgm;
             AHdt = today7;
             AHidt = today7;
             AHtime = xxTime;
             AHitm = xxTime;
             Write pah01;
          Endif;

       // Copy Employer Payrolls
          Setll (EMco#:scFrmGrp:hldEmp#:hldDiv:hldFyr) wmepl;
          Reade(n) (EMco#:scFrmGrp:hldEmp#:hldDiv:hldFyr) wmepl;
          Dow Not %EOF(wmepl);
             If EPstat = 'A';
                EPfnd = sctoGrp;
                EPEmp# = scEmpNmb;
                EPDiv = scDivNmb;
                EPfyr = xxNewFyr;
                EPuser = q1User;
                EPiusr = q1User;
                EPpgm = q1pgm;
                EPipgm = q1pgm;
                EPdt = today7;
                EPidt = today7;
                EPtime = xxTime;
                EPitm = xxTime;
                Write pep01;
             Endif;
          Reade(n) (EMco#:scFrmGrp:hldEmp#:hldDiv:hldFyr) wmepl;
          Enddo;

          If scFrmFyr = *Zeros;
             Reade(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wdell;
          Else;
             Reade(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv:scFrmFyr) wdell;
          EndIf;

       Enddo;

       //  Copy records from the Quote Tracking File
       Setll (EMco#:scFrmGrp:hldEmp#:scFrmDiv) waqtl7;
       Reade(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv) waqtl7;
       Dow not %EOF(waqtl7);

          QTfnd = sctoGrp;
          QTEmp# = scEmpNmb;
          QTDiv = scDivNmb;

          If ELnwrw = 'N';
             QTtype = Upper(QTType);
          Endif;

          QTuser = q1User;
          QTiusr = q1User;
          QTpgm = q1pgm;
          QTipgm = q1pgm;
          QTdt = today7;
          QTidt = today7;
          QTtime = xxTime;
          QTitm = xxTime;
          Write pqt01;

          Reade(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv) waqtl7;
       Enddo;

       //  Copy records from the Owner/Officer File
       Setll (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wmool;
       Reade(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wmool;
       Dow not %EOF(wmool);
          If OOstat = 'A';
             OOfnd = sctoGrp;
             OOEmp# = scEmpNmb;
             OODiv = scDivNmb;
             OOuser = q1User;
             OOiusr = q1User;
             OOpgm = q1pgm;
             OOipgm = q1pgm;
             OOdt = today7;
             OOidt = today7;
             OOtime = xxTime;
             OOitim = xxTime;
             Write poo01;
          Endif;
          Reade(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wmool;
       Enddo;

       //  Copy records from the Employer Contact file WMECP
       Setll (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wmecl;
       Reade(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wmecl;
       Dow not %EOF(wmecl);
          ECfnd = sctoGrp;
          ECEmp# = scEmpNmb;
          ECDiv = scDivNmb;
          ECuser = q1User;
          ECiusr = q1User;
          ECpgm = q1pgm;
          ECipgm = q1pgm;
          ECdt = today7;
          ECidt = today7;
          ECtime = xxTime;
          ECitim = xxTime;
          Write pec01;

          Reade(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wmecl;
       Enddo;

       //  Copy record from the Employer Third Address File
       Chain(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wma3l;
       If %Found(wma3l) and A3stat <> 'I';
          A3fnd = sctoGrp;
          A3Emp# = scEmpNmb;
          A3Div = scDivNmb;
          A3user = q1User;
          A3iusr = q1User;
          A3pgm = q1pgm;
          A3ipgm = q1pgm;
          A3dt = today7;
          A3idt = today7;
          A3time = xxTime;
          A3itm = xxTime;
          Write pa301;
       EndIf;

       //  Copy records from the Entity/Location File
       Setll (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wmall;
       Reade(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wmall;
       Dow not %EOF(wmall);
          ALfnd = sctoGrp;
          ALEmp# = scEmpNmb;
          ALDiv = scDivNmb;
          ALuser = q1User;
          ALiusr = q1User;
          ALpgm = q1pgm;
          ALipgm = q1pgm;
          ALdt = today7;
          ALidt = today7;
          ALtime = xxTime;
          ALitm = xxTime;
          Write pal01;
          Reade(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wmall;
       Enddo;

       //  Copy records from the Additional Named Insured
       Setll (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wmanl;
       Reade(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wmanl;
       Dow not %EOF(wmanl);
          If ANstat <> 'I';
             ANfnd = sctoGrp;
             ANEmp# = scEmpNmb;
             ANDiv = scDivNmb;
             ANuser = q1User;
             ANiusr = q1User;
             ANpgm = q1pgm;
             ANipgm = q1pgm;
             ANdt = today7;
             ANidt = today7;
             ANtime = xxTime;
             Write pan01;
          Endif;
          Reade(n) (EMco#:scFrmGrp:hldEmp#:scFrmDiv) wmanl;
       Enddo;

       Setll (EMco#:scFrmGrp:hldEmp#:hldDiv:hldFyr) wmldl1;
       Reade(n) (EMco#:scFrmGrp:hldEmp#:hldDiv:hldFyr) wmldl1;
       Dow Not %Eof(wmldl1);
          LDfnd = sctoGrp;
          LDEmp# = scEmpNmb;
          LDDiv = scDivNmb;
          LDfyr = xxNewFyr;
          LDuser = q1User;
          LDiusr = q1User;
          LDpgm = q1pgm;
          LDipgm = q1pgm;
          LDdt = today7;
          LDidt = today7;
          LDtime = xxTime;
          LDitim = xxTime;
          Write pld01;
          Reade(n) (EMco#:scFrmGrp:hldEmp#:hldDiv:hldFyr) wmldl1;
       Enddo;

      /End-Free
     P WriteRecords    E
