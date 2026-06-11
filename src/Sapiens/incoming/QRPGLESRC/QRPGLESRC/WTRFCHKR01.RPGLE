     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')

     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for return fund check requests
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTRFCHKREQ
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
     fwmagl     if   e           k disk    rename(pag01:pag00)
     fwmpfl     if   e           k disk    rename(ppf01:ppf00)
     fwdell     if   e           k disk    rename(pel01:pel00)
     fwtabl     if   e           k disk    include(ptt01)

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule

      * *ENTRY
     dwtRfChkR01       pr                  extpgm('WTRFCHKR01')
     d                                9  0
     d                            32767
     d                                1

     dwtRfChkR01       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strGroup        s             70
     d strBtype        s             40    inz(*blanks)
     d intZip          s              9  0 inz(*zeros)
     d strempname      s             40    inz(*blanks)
     d strAgtName      s             40    inz(*blanks)
     d strPFName       s             50    inz(*blanks)
     d strPaidTo       s             40    inz(*blanks)
     d strPaidAdd1     s             40    inz(*blanks)
     d strPaidAdd2     s             40    inz(*blanks)
     d strPaidAdd3     s             40    inz(*blanks)
     d strMMYYYY       s              8    inz(*blanks)
     d strARcd         s             40    inz(*blanks)
     d savARcd         s             10    inz(*blanks)

     c     keyWmagl      klist
     c                   kfld                    R4co#
     c                   kfld                    R4fein
     c                   kfld                    R4mod

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
       BuildJSON(outJSON :'DtlOutBatchTyp' :%trim(strBtype));
       BuildJSON(outJSON :'DtlOutCustID' :%trim(R4cust));
       BuildJSON(outJSON :'DtlOutAgentFEIN' :%trim(%char(R4fein)));
       BuildJSON(outJSON :'DtlOutAgentMod' :%trim(%char(R4mod)));
       BuildJSON(outJSON :'DtlOutAgentName' :%trim(strAgtName));
       BuildJSON(outJSON :'DtlOutGroupName' :%trim(strGroup));
       BuildJSON(outJSON :'DtlOutEmployer' :%trim(%char(R4emp#)));
       BuildJSON(outJSON :'DtlOutDivision' :%trim(%char(R4div)));
       BuildJSON(outJSON :'DtlOutYear' :%trim(%char(R4fyr)));
       BuildJSON(outJSON :'DtlOutCheckAmt' :%trim(%char(R4amt)));
       BuildJSON(outJSON :'DtlOutCheck#' :%trim(%char(R4chk#)));
       BuildJSON(outJSON :'DtlOutStatMY' :%trim(strMMYYYY));
       BuildJSON(outJSON :'DtlOutPremFin' :%trim(strPFName));

       BuildJSON(outJSON :'DtlOutARCode' :%trim(strARcd));
       BuildJSON(outJSON :'DtlOutPaidTo' :%trim(strPaidTo));
       BuildJSON(outJSON :'DtlOutPaidToNm' :%trim(R4ptnm));
       BuildJSON(outJSON :'DtlOutPaidToAddr1' :%trim(strPaidAdd1));
       BuildJSON(outJSON :'DtlOutPaidToAddr2' :%trim(strPaidAdd2));
       BuildJSON(outJSON :'DtlOutPaidToAddr3' :%trim(strPaidAdd3));

       //BuildJSON(outJSON :'DtlOutSurDvd' :%trim(R4sdfl));
       BuildJSON(outJSON :'DtlOutComment1' :%trim(R4com1));
       BuildJSON(outJSON :'DtlOutComment2' :%trim(R4com2));
       BuildJSON(outJSON :'DtlOutComment3' :%trim(R4com3));

       BuildJSON(outJSON :'DtlOutEmpName' :%trim(strempname));


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

       // Agent Name
       Chain keyWmagl pag00;
       If %found(wmagl);
          strAgtName = AGname;
       Endif;

       //Statement MM/YYYY
       If R4stdt <> *zeros;
          strMMYYYY = %editc(dt6MDYY(R4stdt):'X');
          strMMYYYY = %subst(strMMYYYY:1:2) + '/' + %subst(strMMYYYY:3:4);
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


       // Paid To Address
       strPaidAdd1 = %Trim(R4pad1);
       strPaidAdd2 = %Trim(R4pad2);
       intZip      = %int(R4pzip);
       strPaidAdd3 = %Trim(R4pcty) + ', ' + R4pst + ' ' +
        %Subst(%EditC(intZip:'X'):1:5);
       If %Subst(%EditC(intZip:'X'):6:4) <> *Blanks;
          strPaidAdd3 = %Trim(strPaidAdd3) + '-' +
           %Subst(%EditC(intZip:'X'):6:4);
       EndIf;
       If strPaidAdd2 = *Blanks;
          strPaidAdd2 = strPaidAdd3;
          strPaidAdd3 = *Blanks;
       EndIf;
       If R4pzip = *zeros;
          strPaidAdd1 = *Blanks;
          strPaidAdd2 = *Blanks;
          strPaidAdd3 = *Blanks;
       EndIf;

       If R4emp# <> *zeros;
          Chain(n) (R4co#:R4fnd:R4emp#:R4div) wmeml;
          If %found(wmeml);
             strempname = %trim(EMname);
          Endif;
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

       // AR Code
       savARcd = R4arcd;
       Chain (R4Co#:savARcd) ptt01;
       If %found(wtabl);
          strARcd  = %trim(TTkey) + ' - '  + %trim(TTdesc);
       Endif;

       endsr;
      /End-Free
