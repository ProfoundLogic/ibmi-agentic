     H******************************************************************
     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H******************************************************************
     H*/TITLE FIXWDRPP - Update Provider Info and Check Number         *
     H******************************************************************
     H*  DESCRIPTION: This program will update the provider name and   *
     H*               address, and check number in WDRPP from the      *
     H*               data in WDPAP                                    *
     H*                                                                *
     H*  CREATED: 09/24/14  CONSULTANT: C:079  Support: MC025 86719    *
     H******************************************************************
     H*                                                                *
     H* REVISIONS:                                                     *
     H*                                                                *
     H******************************************************************
     F*
     F* Claims checks view file
     FWDRPL3    UF   E           K DISK
     F*
     F* Claims Check payment file
     FWDPAP     IF   E             DISK
     F* Provider Demographic File
     FWMPDL     IF   E           K DISK
     F*
      /copy sPrDate
     D psds          esds                  ExtName(szq1p)
     D flgChgFile      S              1A
     D strPnam         S                   like(PDName)
       //==================================================================
       // MAIN
       //==================================================================
      /Free
       //
       // Cycle through Payment File
       //
         Read wdpap;
           dow not %EOF;
         //
         // Get Payment view file Record and update provider and/or check#
         //
            If PAIpgm = 'W0120R' and PAEOB# <> 0;
             Chain (PACo#: PAFnd: PAFYr: PACase: PASeq#) WDRPL3;
               If %found(WDRPL3);
                 flgChgFile = 'N';
                 Chain (PAFein: PAMod) WMPDL;
           //
           // Update Provider Info
           //
                 If %found(WMPDL);
                   ExSr srGetPnam;
                   If RPPNam <> strPnam;
                     ExSr srLoadProv;
                   Endif;
                 Endif;
           //
           // Update Check Info
           //
                 If PAChk# <> RPChk#;
                   RPChk# = PAChk#;
                   flgChgFile = 'Y';
                 Endif;
           //
           // Update the File
           //
                 If flgChgFile = 'Y';
                   ExSr srUpdWDRPP;
                 Else;
                   Update PRP01;
                 EndIf;
               Endif;
         //
         // Read the next record
         //
              EndIf;
             Read wdpap;
           EndDo;
           *inlr = *on;
           Return;
       //==================================================================
       // End of MAIN
       //==================================================================
       //
       //===================================================================
       // srGetPnam - Get Provider Name
       //===================================================================
       BegSr srGetPnam;
         If PDDba1 = 'Y';
           strPnam = PDAdr1;
         Else;
           strPnam = PDName;
           If PDFsn <> *blanks;
             strPnam = %trim(strPnam) + ', ' + %trim(PDFsn);
           EndIf;
         EndIf;
       EndSr;
       //===================================================================
       // srLoadProv - Load Provider Info
       //===================================================================
       BegSr srLoadProv;
         If PDDba1 = 'Y';
           RPAdr1 = PDAdr2;
           RPAdr2 = *blanks;
         Else;
           RPAdr1 = PDAdr1;
           RPAdr2 = PDAdr2;
         EndIf;

         RPPnam = strPnam;
         RPCty  = PDCty;
         RPSt  = PDSt;
         RPZip = PDZip;
         flgChgFile = 'Y';
       EndSr;
       //===================================================================
       // srUpdWDRPP - Update WDRPP
       //===================================================================
       BegSr srUpdWDRPP;
         RPUser   = q1user;
         RPPgm    = q1pgm;
         RPDt     = Dateto7(SystemDate());
         RPTime   = Timeto6(SystemTime());
         Update PRP01;
       EndSr;
      /End-Free
