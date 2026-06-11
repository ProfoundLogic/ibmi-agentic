     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Cancellation History, State Reporting
     h*               detail box - TAB 1.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLSUM
     h*  Calls to:
     h*
     h*  Created on 04/20/2012 by C:190 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwmalp     if   e             disk    rename(pal01:pal0p)
     fwtabl     if   e           k disk    include(pcn01)
     fwtgtl     if   e           k disk    rename(pgt01:pgt01)
     fwmeml     if   e           k disk    rename(pem01:pem01)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy sPrString

      * *ENTRY
     DPolicySumm06     pr                  ExtPgm('WTPOLSUM06')
     D                                9  0
     D                            32767
     D                                1
      *
     DPolicySumm06     pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d numEdtCde       c                   const('N')
     d tempZip         s              9
     d tempPhn#        s              7
     d tempFax#        s              7
     d ncciFlag        s              3
     d xxKey           s             10
     d countyDesc      s             60
     d tableID1        s              4
     d tableID2        s             10
     d bussDesc        s             60
     d naicDesc        s             60
     d sicDesc         s             60
     D AdL1            s             40    inz
     D AdL2            s             40    inz
     D AdL3            s             40    inz
      *
     c     keyTable      klist
     c                   kfld                    Alco#
     c                   kfld                    xxKey

     c     keyWTGTL      klist
     c                   kfld                    Alco#
     c                   kfld                    tableID1
     c                   kfld                    tableID2

     c     keyWMEML      klist
     c                   kfld                    Alco#
     c                   kfld                    ALfnd
     c                   kfld                    ALemp#
     c                   kfld                    ALdiv

      /Free

       Chain(n) inRRN pal0p;

       outJSON = ' ';

       // Populate Location information.
       BuildJSON(outJSON :'OutLocEnt#' :%TrimR(ALelid));
       BuildJSON(outJSON :'OutLocFedId':%Editc(ALfein:'X'));
       BuildJSON(outJSON :'OutLocDBANm' :%TrimR(ALname));
       BuildJSON(outJSON :'OutLocFEDNm' :%TrimR(ALfdnm));

       FormatAddress(ALmad1:ALmad2:ALmcty:ALmst:ALmzip:AdL1:AdL2:AdL3);
       BuildJSON(outJSON :'OutLocMalAdr1' :%TrimR(AdL1));
       BuildJSON(outJSON :'OutLocMalAdr2' :%TrimR(AdL2));
       BuildJSON(outJSON :'OutLocMalCtyZip' :%Trim(AdL3));

       FormatAddress(ALpad1:ALpad2:ALpcty:ALpst:ALpzip:AdL1:AdL2:AdL3);
       BuildJSON(outJSON :'OutLocPhyAdr1' :%TrimR(AdL1));
       BuildJSON(outJSON :'OutLocPhyAdr2' :%TrimR(AdL2));
       BuildJSON(outJSON :'OutLocPhyCtyZip' :%TrimR(AdL3));

       Clear xxKey;
       If ALpst <> ' ';
          xxkey = ALpst + ALcnty;
       Else;
          xxkey = ALmst + ALcnty;
       Endif;
       Chain(n) keyTable pcn01;
       Clear countyDesc;
       countyDesc = CNdesc;
       BuildJSON(outJSON :'OutLocCounty':%TrimR(countyDesc));

       If ALPhn# <> 0;
          Clear tempPhn#;
          tempPhn# = %Editc(ALphn#:'X');
          BuildJSON(outJSON :'OutLocPhn#' : '(' + %Editc(ALarea:'X') + ') '
           + %subst(tempPhn#:1:3) + '-' + %subst(tempPhn#:4:4));
       Else;
          BuildJSON(outJSON :'OutLocPhn#' : ' ');
       Endif;

       If ALFax# <> 0;
          Clear tempFax#;
          tempFax# = %Editc(ALFax#:'X');
          BuildJSON(outJSON :'OutLocFax#' : '(' + %Editc(ALfara:'X') + ') '
           + %subst(tempFax#:1:3) + '-' + %subst(tempFax#:4:4));
       Else;
          BuildJSON(outJSON :'OutLocFax#' : ' ');
       Endif;

       If ALncci = 'Y';
          ncciFlag = 'Yes';
       Else;
          ncciFlag = '   ';
       Endif;
       BuildJSON(outJSON :'OutLocNCCI':%TrimR(ncciFlag));

       tableID1 = 'BUSI';
       tableID2 = ALtpob;
       Chain(n) keyWTGTL pgt01;
       If %found;
          Clear bussDesc;
          bussDesc  = GTdesc;
          Else;
          bussDesc  = %TrimR(ALtpob);
       Endif;
       BuildJSON(outJSON :'OutLocTypBuss': %TrimR(bussDesc));

       tableID1 = 'SIC';
       tableID2 = ALsic;
       Chain(n) keyWTGTL pgt01;
       If %found;
          Clear sicDesc;
          sicDesc = %TrimR(ALsic) + ' - ' + GTdesc;
       Else;
          sicDesc = %TrimR(ALsic);
       Endif;
       BuildJSON(outJSON :'OutLocSIC': %TrimR(sicDesc));

       tableID1 = 'NAIC';
       tableID2 = ALnaic;
       Chain(n) keyWTGTL pgt01;
       If %found;
          Clear naicDesc;
          naicDesc = %TrimR(ALnaic) + ' - ' + GTdesc;
       Else;
          naicDesc = %TrimR(ALnaic);
       Endif;
       BuildJSON(outJSON :'OutLocNAIC': %TrimR(naicDesc));

       BuildJSON(outJSON :'OutLocRiskID': %TrimR(ALsemp));
       BuildJSON(outJSON :'OutLocUnEmp#': %TrimR(ALue#));


       // Populate Division information.
       Chain(n) keyWMEML pem01;
       BuildJSON(outJSON :'OutDivision#':%Editc(EMdiv:'X'));
       BuildJSON(outJSON :'OutDivFedId':%Editc(EMfein:'X'));
       BuildJSON(outJSON :'OutDivDBANm' :%TrimR(EMname));
       BuildJSON(outJSON :'OutDivFEDNm' :%TrimR(EMfdnm));

       FormatAddress(EMmad1:EMmad2:EMmcty:EMmst:EMmzip:AdL1:AdL2:AdL3);
       BuildJSON(outJSON :'OutDivMalAdr1' :%TrimR(AdL1));
       BuildJSON(outJSON :'OutDivMalAdr2' :%TrimR(AdL2));
       BuildJSON(outJSON :'OutDivMalCtyZip' :%Trim(Adl3));

       FormatAddress(EMpad1:EMpad2:EMpcty:EMpst:EMpzip:AdL1:AdL2:AdL3);
       BuildJSON(outJSON :'OutDivPhyAdr1' :%TrimR(AdL1));
       BuildJSON(outJSON :'OutDivPhyAdr2' :%TrimR(AdL2));
       BuildJSON(outJSON :'OutDivPhyCtyZip' :%TrimR(Adl3));

       Clear xxKey;
       If EMpst <> ' ';
          xxkey = EMpst + EMcnty;
       Else;
          xxkey = EMmst + EMcnty;
       Endif;
       Chain(n) keyTable pcn01;
       Clear countyDesc;
       countyDesc = CNdesc;
       BuildJSON(outJSON :'OutDivCounty':%TrimR(countyDesc));

       If EMPhn# <> 0;
          Clear tempPhn#;
          tempPhn# = %Editc(EMphn#:'X');
          BuildJSON(outJSON :'OutDivPhn#' : '(' + %Editc(EMarea:'X') + ') '
           + %subst(tempPhn#:1:3) + '-' + %subst(tempPhn#:4:4));
       Else;
          BuildJSON(outJSON :'OutDivPhn#' : ' ');
       Endif;

       If EMFax# <> 0;
          Clear tempFax#;
          tempFax# = %Editc(EMfax#:'X');
          BuildJSON(outJSON :'OutDivFax#' : '(' + %Editc(EMfaxa:'X') + ') '
           + %subst(tempFax#:1:3) + '-' + %subst(tempFax#:4:4));
       Else;
          BuildJSON(outJSON :'OutDivFax#' : ' ');
       Endif;

       tableID1 = 'BUSI';
       tableID2 = EMtpob;
       Chain(n) keyWTGTL pgt01;
       If %found;
          Clear bussDesc;
          bussDesc  = GTdesc;
          Else;
          bussDesc  = %TrimR(EMtpob);
       Endif;
       BuildJSON(outJSON :'OutDivTypBuss': %TrimR(bussDesc));

       tableID1 = 'SIC';
       tableID2 = EMsic;
       Chain(n) keyWTGTL pgt01;
       If %found;
          Clear sicDesc;
          sicDesc = %TrimR(EMsic) + ' - ' + GTdesc;
       Else;
          sicDesc = %TrimR(EMsic);
       Endif;
       BuildJSON(outJSON :'OutDivSIC': %TrimR(sicDesc));

       tableID1 = 'NAIC';
       tableID2 = EMnaic;
       Chain(n) keyWTGTL pgt01;
       If %found;
          Clear naicDesc;
          naicDesc = %TrimR(EMnaic) + ' - ' + GTdesc;
       Else;
          naicDesc = %TrimR(EMnaic);
       Endif;
       BuildJSON(outJSON :'OutDivNAIC': %TrimR(naicDesc));

       BuildJSON(outJSON :'OutDivRiskID': %TrimR(EMsemp));
       BuildJSON(outJSON :'OutDivUnEmp#': %TrimR(EMue#));

       rtnCode = ' ';
       Return;
      /End-Free

