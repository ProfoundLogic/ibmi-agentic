     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve owner/officer information
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLQTE
     h*  Calls to:
     h*
     h*  Created on 06/27/2012 by C:065 for Project Transform - WC2116
     h**********************************************************************
     h* Revisions:
     h*
     h* 06/09/20  C:298 Project: SY420
     h*                 Mask SSN
     h*
     h**********************************************************************
     fwmoop     if   e             disk    rename(poo01:poo0p)
     FWMEML     IF   E           K DISK    rename(PEM01:PEM00)
     FWMALL     IF   E           K DISK    rename(PAL01:PAL00)

      /copy sPrString
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrRateSet
      /copy wPrTable
      /copy wPrEmpLr
      /copy wPrAgent
      /copy wPrProduce

      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************

      * *ENTRY
     DQuoteGrid09      pr                  ExtPgm('WTPOLQTE09')
     D                                9  0
     D                            32767
     D                                1
      *
     DQuoteGrid09      pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d numEdtCde       c                   const('N')
     d ss#             s             11
     D AdL1            s             40    inz
     D AdL2            s             40    inz
     D AdL3            s             40    inz
     C*
     C* Employer Master File Key List
     C*
     C     keyWMEML      Klist
     C                   Kfld                    OOCo#
     C                   Kfld                    OOFnd
     C                   Kfld                    OOEmp#
     C                   Kfld                    OODiv
     C*
     C* Additional Location File Key List
     C*
     C     keyWMALL      Klist
     C                   Kfld                    OOCo#
     C                   Kfld                    OOFnd
     C                   Kfld                    OOEmp#
     C                   Kfld                    OODiv
     C                   Kfld                    OOElid
      *

      /Free

       Chain inRRN poo0p;

       outJSON = ' ';

       BuildJSON(outJSON :'OutOwnDetDOB' :
        %EditW(dtMDYY(OODOB):'  /  /    '));

       //***  Begin Chg  ***  06/09/20  ***********************
       //Clear ss#;
       //If OOOSS# <> ' ';
          //ss# = %subst(OOOSS#:1:3) +
          // '-' + %subst(OOOSS#:4:2) + '-' + %subst(OOOSS#:6:4);
       //Endif;
       //BuildJSON(outJSON :'OutOwnDetSS#' : ss#);
       BuildJSON(outJSON :'OutOwnDetSS#'
        :DisplaySS#(MaskData('SSN':'OOOSS#': OOOSS#)));
       //***  End   Chg  ***  06/09/20  ***********************

       Chain keyWMEML PEM00;

       BuildJSON(outJSON :'OutOwn9Division2' : %editc(OODIV: 'X'));

       FormatAddress(EMMad1:EMMad2:EMMcty:EMMst:EMMzip:AdL1:AdL2:AdL3);
       BuildJSON(outJSON :'OutOwn9DivMail1' :%Trim(AdL1));
       BuildJSON(outJSON :'OutOwn9DivMail2' :%Trim(AdL2));
       BuildJSON(outJSON :'OutOwn9DivMail3' :%Trim(AdL3));

       FormatAddress(EMPad1:EMPad2:EMPcty:EMPst:EMPzip:AdL1:AdL2:AdL3);
       BuildJSON(outJSON :'OutOwn9DivPhys1' :%Trim(AdL1));
       BuildJSON(outJSON :'OutOwn9DivPhys2' :%Trim(AdL2));
       BuildJSON(outJSON :'OutOwn9DivPhys3' :%Trim(AdL3));

       If OOELID <> ' ';
          Chain keyWMALL PAL00;
       Else;
          Clear ALMAd1;
          Clear ALMAd2;
          Clear ALMCty;
          Clear ALMSt;
          Clear ALMZip;
          Clear ALPAd1;
          Clear ALPAd2;
          Clear ALPCty;
          Clear ALPSt;
          Clear ALPZip;
       Endif;

       BuildJSON(outJSON :'OutOwn9Entity2' : OOELID);

       FormatAddress(ALMad1:ALMad2:ALMcty:ALMst:ALMzip:AdL1:AdL2:AdL3);
       BuildJSON(outJSON :'OutOwn9EntMail1' :%Trim(AdL1));
       BuildJSON(outJSON :'OutOwn9EntMail2' :%Trim(AdL2));
       BuildJSON(outJSON :'OutOwn9EntMail3' :%Trim(AdL3));

       FormatAddress(ALPad1:ALPad2:ALPcty:ALPst:ALPzip:AdL1:AdL2:AdL3);
       BuildJSON(outJSON :'OutOwn9EntPhys1' :%Trim(AdL1));
       BuildJSON(outJSON :'OutOwn9EntPhys2' :%Trim(AdL2));
       BuildJSON(outJSON :'OutOwn9EntPhys3' :%Trim(AdL3));

       rtnCode = ' ';
       Return;
      /End-Free

