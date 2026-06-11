     hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Bank Infomration
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTGRPPRI
     h*  Calls to:
     h*
     h*  Created on 03/24/2020 by C:300 for Project WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fwmbip     if   e             disk    rename(pbi01:pbi0s)
     fwtgtl     if   e           k disk
     Fwsckl     if   e           k disk    rename(pck01:pck0p)
      * E-Pay Vendor Bank Account File
     fwdb2l1    if   e           k disk    rename(pb201:pb200)

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrModule

      * *ENTRY
     dwtGrpPri04       pr                  extpgm('WTGRPPRI04')
     d                                9  0
     d                            32767
     d                                1

     dwtGrpPri04       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strApplType     s             60
     d strEftFlag      s             10
     d strVpFacTbl     s             55

     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    BIco#
     C                   KFLD                    kktbid            4
     C                   KFLD                    kkkey            10


      /Free

       Chain inRRN pbi0s;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlBankAcct' :%trim(BIacct)+' - '+%trim(BIai01));
       BuildJSON(outJSON :'OutDtlAcctAdd1' :%trim(BIai02));
       BuildJSON(outJSON :'OutDtlAcctAdd2' :%trim(BIai03));
       BuildJSON(outJSON :'OutDtlApplType' :%trim(strApplType));
       BuildJSON(outJSON :'OutDtlBankAcctCode' :%trim(BIcktp));
       BuildJSON(outJSON :'OutDtlEFTFlag' :%trim(strEftFlag));
       BuildJSON(outJSON :'OutDtlBankEffDate' :
                         alphaUSA(cvtToDate(dtMDYY(BIefdt))));
       BuildJSON(outJSON :'OutDtlEftCrCmp' :%trim(BIccid));
       BuildJSON(outJSON :'OutDtlEftDbCmp' :%trim(BIdcid));
       BuildJSON(outJSON :'OutDtlSignatures1' :%trim(BInam1));
       BuildJSON(outJSON :'OutDtlSignatures2' :%trim(BInam2));
       BuildJSON(outJSON :'CbxDtlPrintHardCopy' :BIhdcy);
       BuildJSON(outJSON :'CbxDtlTrust' :BItrst);
       BuildJSON(outJSON :'CbxDtlLogo' :BIlogo);
       // Read WSCKP file for Voucher Only Checkbox
       Chain(n) (BIco#:BIacct) pck0p;
       If %found(wsckl);
          BuildJSON(outJSON :'CbxDtlVouchersOnly' :CKvchr);
       EndIf;
       BuildJSON(outJSON :'CbxDtlBlankSignLine' :BI1a2);
       BuildJSON(outJSON :'OutDtlFormType' :%trim(BIftyp));
       BuildJSON(outJSON :'OutDtlLimit2ndSign' :%editc(BI$lmt:'N'));
       BuildJSON(outJSON :'OutDtlVoidAfterDays' :%editc(BI50n3:'N'));
       BuildJSON(outJSON :'OutDtlMacroID' :%trim(%Char(BImacr)));
       BuildJSON(outJSON :'OutDtlCheckOUTQ' :%trim(BIoutq));
       BuildJSON(outJSON :'OutDtlFileCopyOUTQ' :%trim(BIcpyq));
       BuildJSON(outJSON :'OutDtlDesc1' :%trim(BI40a1));
       BuildJSON(outJSON :'OutDtlDesc2' :%trim(BI40a2));
       BuildJSON(outJSON :'OutDtlDesc3' :%trim(BI40a3));
       BuildJSON(outJSON :'OutDtlDesc4' :%trim(BI40a4));
       BuildJSON(outJSON :'OutDtlTextonCheck' :%trim(BItxtonck) +
                           %trim(BItxtonck2));
       BuildJSON(outJSON :'OutDtlClmChkPrtVal' :%trim(BIckpv));
       BuildJSON(outJSON :'OutDtlRoutEntUpr' :%trim(BIrtup));
       BuildJSON(outJSON :'OutDtlRoutEntLow' :%trim(BIrtlw));
       BuildJSON(outJSON :'OutDtlRoutforMICR' :%trim(BItrns));
       BuildJSON(outJSON :'OutDtlTransNum' :%trim(BIbnk#));
       BuildJSON(outJSON :'OutDtlTransChkDgt' :%trim(BIckdg));
       BuildJSON(outJSON :'OutDtlAuxOnUS' :%trim(BIaxou));
       BuildJSON(outJSON :'OutDtlOnUS' :%trim(BIonus));

       // Load E-Pay Info
       BuildJSON(outJSON :'OutEPayVendor' :%trim(B2ePvend));
       BuildJSON(outJSON :'OutVpayTableSelection' :%trim(strVpFacTbl));
       BuildJSON(outJSON :'OutVPayFreeFormEntry' :%trim(B2vPfacff));
       BuildJSON(outJSON :'CbxClaimsIndemnity' :B2ePclmInd);
       BuildJSON(outJSON :'CbxClaimsMedical' :B2ePclmMed);
       BuildJSON(outJSON :'CbxClaimsExpense' :B2ePclmExp);
       BuildJSON(outJSON :'CbxClaimsLegal' :B2ePclmLeg);


       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Application Type
       strApplType = %trim(BIappl);
       IF BIappl <> *Blanks;
          kktbid = 'BTYP';
          kkkey =  BIappl;
          Chain GTKY pgt01;
          IF %found(WTGTL) and GTstat ='A';
             strApplType = %trim(strApplType) + ' - '  + GTdesc;
          EndIf;
       EndIf;

       // EFT Flag
       Select;
       When BIeftc = *Blanks;
          strEftFlag = 'MICR';
       When BIeftc = 'E';
          strEftFlag = 'EFT';
       When BIeftc = 'B';
          strEftFlag = 'Both';
       Endsl;

       // Load E-Pay Info
       Chain(N) (BIco#:BIappl:BIfnd:BIacct:BIckTp:BIefDt) pb200;
       strVpFacTbl = *Blanks;
       If %Found(Wdb2l1);
          // Vendor Pay Funding Account Code Table Selection
          strVpFacTbl = %trim(B2vPfacTbl);
          IF strVpFacTbl <> *Blanks;
             kktbid = 'VPFA';
             kkkey =  %trim(B2vPfacTbl);
             Chain GTKY pgt01;
             IF %found(WTGTL) and GTstat = 'A';
                strVpFacTbl = %trim(strVpFacTbl) + ' - '  + GTdesc;
             EndIf;
          EndIf;
       Endif;


       endsr;
      /End-Free
