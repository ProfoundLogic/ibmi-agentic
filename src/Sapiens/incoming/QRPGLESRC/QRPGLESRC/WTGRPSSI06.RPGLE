     hcopyright('(c) 2021 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Cancellation/Reinstatement State Definition Information
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTGRPSSI
     h*  Calls to:
     h*
     h*  Created on 03/03/2021 by C:318 for Project WC2196
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fwtxcp     if   e             disk    rename(pxc01:pxc0s)
     fwtgtl     if   e           k disk
     fstctl1    if   e           k disk

     d/copy sPrDate
     d/copy sPrAtrium

      * *ENTRY
     dwtGrpSSI06       pr                  extpgm('WTGRPSSI06')
     d                                9  0
     d                            32767
     d                                1

     dwtGrpSSI06       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strRsnCode      s            200
     d strRmkCode1     s            200
     d strRmkCode2     s            200
     d strRmkCode3     s            200
     d strRmkCode4     s            200

     d*

     c     KeyWtgtl      klist
     c                   kfld                    XCco#
     c                   kfld                    kktbid            4
     c                   kfld                    kkkeyGT          10
     c     KeyStctl1     klist
     c                   kfld                    keyRmcd           5
      /Free

       Chain inRRN pxc0s;
       ExSr  BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlStateCRSD':%trim(XCst));
       BuildJSON(outJSON :'OutDtlEffDateCRSD':
                         alphaUSA(cvtToDate(dtMDYY(XCefdt))));
       BuildJSON(outJSON :'OutDtlCxNoticeDays':%trim(%char(XCnotc)));
       BuildJSON(outJSON :'OutDtlCxLimit':%trim(%char(XCcxlm)));
       BuildJSON(outJSON :'OutDtlNumberOfMonths':%trim(%char(XCcxmn)));
       BuildJSON(outJSON :'OutDtlStateDepName':%trim(XCstnm));
       BuildJSON(outJSON :'OutDtlACORDRmk1Default':%trim(XCdrem));
       BuildJSON(outJSON :'OutDtlStateCxFormFolder':%trim(XCcxfl));
       BuildJSON(outJSON :'OutDtlStateCxFormDoc':%trim(XCcxdc));
       BuildJSON(outJSON :'OutDtlStateReFormFolder':%trim(XCrefl));
       BuildJSON(outJSON :'OutDtlStateReFormDoc':%trim(XCredc));
       BuildJSON(outJSON :'CbxDtlRequireReCode':%trim(XCrrcd));
       BuildJSON(outJSON :'CbxDtlStateCxTraOnStateRem':%trim(XCstcx));
       BuildJSON(outJSON :'OutDtl1stNoticeOfCxFolder':%trim(XC#1LF));
       BuildJSON(outJSON :'OutDtl1stNoticeOfCxDoc':%trim(XC#1LD));
       BuildJSON(outJSON :'OutDtl1stNoticeOfCxRmk':%trim(strRmkCode1));
       BuildJSON(outJSON :'OutDtl2ndNoticeOfCxFolder':%trim(XC#2LF));
       BuildJSON(outJSON :'OutDtl2ndNoticeOfCxDoc':%trim(XC#2LD));
       BuildJSON(outJSON :'OutDtl2ndNoticeOfCxRmk':%trim(strRmkCode2));
       BuildJSON(outJSON :'OutDtl3rdNoticeOfCxFolder':%trim(XC#3LF));
       BuildJSON(outJSON :'OutDtl3rdNoticeOfCxDoc':%trim(XC#3LD));
       BuildJSON(outJSON :'OutDtl3rdNoticeOfCxRmk':%trim(strRmkCode3));
       BuildJSON(outJSON :'OutDtl4thNoticeOfCxFolder':%trim(XC#4LF));
       BuildJSON(outJSON :'OutDtl4thNoticeOfCxDoc':%trim(XC#4LD));
       BuildJSON(outJSON :'OutDtl4thNoticeOfCxRmk':%trim(strRmkCode4));
       BuildJSON(outJSON :'OutDtlCxCoverLetterFolder':%trim(XCcclf));
       BuildJSON(outJSON :'OutDtlCxCoverLetterDoc':%trim(XCccld));
       BuildJSON(outJSON :'OutDtlCxCoverLetterReasonCd':%trim(strRsnCode));
       BuildJSON(outJSON :'OutDtlClientSpeCxConCertLtrFld':%trim(XCcflf));
       BuildJSON(outJSON :'OutDtlClientSpeCxConCertLtrDoc':%trim(XCcfld));
       BuildJSON(outJSON :'OutDtlClientSpeCxCertLtrFld':%trim(XCcscf));
       BuildJSON(outJSON :'OutDtlClientSpeCxCertLtrDoc':%trim(XCcscd));
       BuildJSON(outJSON :'OutDtlClientSpeReCertLtrFld':%trim(XCcsrf));
       BuildJSON(outJSON :'OutDtlClientSpeReCertLtrDoc':%trim(XCcsrd));
       BuildJSON(outJSON :'OutDtlReCovLetterFolder':%trim(XCrclf));
       BuildJSON(outJSON :'OutDtlReCovLetterDoc':%trim(XCrcld));
       rtnCode = ' ';
       Return;

       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

       // Cancellation Cover Letter reason code
       strRsnCode = %trim(XCccrs);
       If XCccrs <> *Blanks;
          kktbid = 'CXCD';
          kkkeyGT = XCccrs;
          Chain KeyWtgtl pgt01;
          IF %found(WTGTL) ;
             strRsnCode = %trim(strRsnCode) + ' - '  + GTdesc;
          EndIf;
       EndIf;

       // 1st Notice of Cancellation Document Remark Code
       strRmkCode1 = %trim(XC#1DR);
       If XC#1DR <> *Blanks;
          keyRmcd = XC#1DR;
          Chain KeyStctl1 pct01;
          IF %found(STCTL1) ;
             strRmkCode1 = %trim(strRmkCode1) + ' - '  + CTdesc;
          EndIf;
       EndIf;

       // 2nd Notice of Cancellation Document Remark Code
       strRmkCode2 = %trim(XC#2DR);
       If XC#2DR <> *Blanks;
          keyRmcd = XC#2DR;
          Chain KeyStctl1 pct01;
          IF %found(STCTL1) ;
             strRmkCode2 = %trim(strRmkCode2) + ' - '  + CTdesc;
          EndIf;
       EndIf;

       // 3rd Notice of Cancellation Document Remark Code
       strRmkCode3 = %trim(XC#3DR);
       If XC#3DR <> *Blanks;
          keyRmcd = XC#3DR;
          Chain KeyStctl1 pct01;
          IF %found(STCTL1) ;
             strRmkCode3 = %trim(strRmkCode3) + ' - '  + CTdesc;
          EndIf;
       EndIf;

       // 4th Notice of Cancellation Document Remark Code
       strRmkCode4 = %trim(XC#4DR);
       If XC#4DR <> *Blanks;
          keyRmcd = XC#4DR;
          Chain KeyStctl1 pct01;
          IF %found(STCTL1) ;
             strRmkCode4 = %trim(strRmkCode4) + ' - '  + CTdesc;
          EndIf;
       EndIf;

       endsr;
      /End-Free
