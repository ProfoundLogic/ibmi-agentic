     hcopyright('(c) 2023 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for return fund checks
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTRFCHK
     h*  Calls to:
     h*
     h*  Created on 10/19/2023 by C:342 for Project WC2676
     h**********************************************************************
     h* Revisions:
     h*                                                                *
     h*
     h**********************************************************************
     fwar4p     if   e             disk    rename(pr401:pr40p)
     fwmfnl     if   e           k disk
     fwmeml     if   e           k disk
     fwmpfl     if   e           k disk    rename(ppf01:ppf00)
     fwdell     if   e           k disk    rename(pel01:pel00)

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule
     d/copy wPrAgent

      * *ENTRY
     dwtRfChk01        pr                  extpgm('WTRFCHK01')
     d                                9  0
     d                            32767
     d                                1

     dwtRfChk01        pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strGroup        s             70
     d strBtype        s             40    inz(*blanks)
     d strVS           s              4    inz(*blanks)
     d strempname      s             40    inz(*blanks)
     d strPFName       s             50    inz(*blanks)
     d strPaidTo       s             40    inz(*blanks)
     d strAgtName      s             50    inz(*blanks)
     d strAgt#Name     s             50    inz(*blanks)
     d strMMYYYY       s              8    inz(*blanks)

     c     pfky          klist
     c                   kfld                    R4co#
     c                   kfld                    R4fnd
     c                   kfld                    R4pfin
      *
     c     pfky2         klist
     c                   kfld                    R4co#
     c                   kfld                    R4fnd
     c                   kfld                    R4pfin
     c                   kfld                    ELpfdt

     c     elky          klist
     c                   kfld                    R4co#
     c                   kfld                    R4fnd
     c                   kfld                    R4emp#
     c                   kfld                    R4div
     c                   kfld                    R4fyr

      /Free

       Chain inRRN pr40p;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'DtlOutCustID' :%trim(R4cust));
       BuildJSON(outJSON :'DtlOutGroup' :%trim(strGroup));
       BuildJSON(outJSON :'DtlOutEmployer' :%trim(%char(R4emp#)));
       BuildJSON(outJSON :'DtlOutDivision' :%trim(%char(R4div)));
       BuildJSON(outJSON :'DtlOutYear' :%trim(%char(R4fyr)));
       BuildJSON(outJSON :'DtlOutPayeeName' :%trim(R4ptnm));
       BuildJSON(outJSON :'DtlOutPayeeType' :%trim(strPaidTo));
       BuildJSON(outJSON :'DtlOutBankAcct' :%trim(R4acct));
       BuildJSON(outJSON :'DtlOutCheckAmt' :%trim(%char(R4amt)));
       BuildJSON(outJSON :'DtlOutCheck#' :%trim(%char(R4chk#)));
       BuildJSON(outJSON :'DtlOutComment1' :%trim(R4com1));
       BuildJSON(outJSON :'DtlOutComment2' :%trim(R4com2));
       BuildJSON(outJSON :'DtlOutComment3' :%trim(R4com3));
       BuildJSON(outJSON :'DtlOutVoidStp' :%trim(strVS));
       BuildJSON(outJSON :'DtlOutBatchTyp' :%trim(strBtype));
       BuildJSON(outJSON :'DtlOutEmpName' :%trim(strempname));
       BuildJSON(outJSON :'DtlOutPremFin' :%trim(strPFName));
       BuildJSON(outJSON :'DtlOutAgentFEIN' :%trim(%char(R4fein)));
       BuildJSON(outJSON :'DtlOutAgentMod' :%trim(%char(R4mod)));
       BuildJSON(outJSON :'DtlOutAgentName' :%trim(strAgt#Name));
       BuildJSON(outJSON :'DtlOutStatMY' :%trim(strMMYYYY));


       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Group Number
       strGroup = *Blanks;
       Chain (R4co#:R4fnd) pfn01;
       IF %found(wmfnl);
          strGroup = %char(FNfnd) + ' - '  + %trim(FNname);
       EndIf;

       If R4emp# <> *zeros;
          Chain(n) (R4co#:R4fnd:R4emp#:R4div) wmeml;
          If %found(wmeml);
             strempname = %trim(EMname);
          Endif;
       Endif;

       // get employer details info
       Chain elky pel00;
       If %found(wdell);
          // Get Premium finance compnay name.
          If R4pfin <> *blanks;
             strPFName = R4pfin;
             setgt pfky2 wmpfl;
             readpe pfky  wmpfl;
             if not %eof(wmpfl);
                strPFName = %trim(R4pfin) + ' - '  + %trim(PFname);
             Endif;
          Endif;
       Endif;

       // Agent Name
       strAgtName = GetAgtName(R4agt#);
       strAgt#Name = %trim(%Char(R4agt#));
       strAgt#Name = %trim(strAgt#Name) + ' - ' +%trim(strAgtName);

       //Statement MM/YYYY
       If R4stdt <> *zeros;
          strMMYYYY = %editc(dt6MDYY(R4stdt):'X');
          strMMYYYY = %subst(strMMYYYY:1:2) + '/' + %subst(strMMYYYY:3:4);
       Endif;

       select;
       when R4btyp = 'A';
          strBtype = 'Account Current';
       when R4btyp = 'D';
          strBtype = 'Deposit';
       when R4btyp = 'I';
          strBtype = 'Independent Cash Distribution';
       when R4btyp = 'P';
          strBtype = 'Premium';
       endsl;

       select;
       when R4ptfl  = 'A';
          strPaidTo = 'Agent';
       when R4ptfl  = 'E';
          strPaidTo = 'Employer';
       when R4ptfl  = 'G';
          strPaidTo = 'Group';
       when R4ptfl  = 'P';
          strPaidTo = 'Premium Finance Company';
       other;
          strPaidTo = *blanks;
       endsl;


       If R4vdsp = 'V';
          strVS = 'Void';
       Endif;

       If R4vdsp = 'S';
          strVS = 'Stop';
       Endif;

       endsr;
      /End-Free
