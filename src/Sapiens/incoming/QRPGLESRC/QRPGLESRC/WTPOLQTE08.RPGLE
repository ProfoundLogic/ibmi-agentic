     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
      **********************************************************************
      *
      *  Description: Response program to serve additional location information
      *               - Activation group should be *CALLER
      *               - Do not seton LR, just do a return
      *               - This process should not lock any file
      *               - This process should only read files, no updates/writes should be made
      *
      *  Called from: WTPOLQTE
      *  Calls to:
      *
      *  Created on 06/27/2012 by C:065 for Project Transform - WC2116
      **********************************************************************
      * Revisions:
      *
      * 03/03/15 C:302 PROJECT: 114WRG                               *
      *                Policy Concentration Details                  *
      *                                                              *
      *
      **********************************************************************

     fwmalp     if   e             disk    rename(pal01:pal0p)
     fwtabl     if   e           k disk    include(pcn01)
     fwtgtl     if   e           k disk    rename(pgt01:pgt01)

      /copy sPrString
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrRateSet
      /copy wPrTable
      /copy wPrEmpLr
      /copy wPrAgent
      /copy wPrProduce

      * *ENTRY
     DQuoteGrid08      pr                  ExtPgm('WTPOLQTE08')
     D                                9  0
     D                            32767
     D                                1
      *
     DQuoteGrid08      pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *
     d numEdtCde       c                   const('N')
     D AdL1            s             40    inz
     D AdL2            s             40    inz
     D AdL3            s             40    inz
     d tempPhn#        s              7
     d tempFax#        s              7
     d xxKey           s             10
     d countyDesc      s             60
     d tableID1        s              4
     d tableID2        s             10
     d bussDesc        s             60
     d naicDesc        s             60
     d sicDesc         s             60
      *
     c     keyTable      klist
     c                   kfld                    Alco#
     c                   kfld                    xxKey

     c     keyWTGTL      klist
     c                   kfld                    Alco#
     c                   kfld                    tableID1
     c                   kfld                    tableID2


      /Free

       Chain inRRN pal0p;

       outJSON = ' ';

       BuildJSON(outJSON :'OutLocDetFedName' : ALFdnm);

       BuildJSON(outJSON :'OutLocDetRiskID' : ALSEMP);

       BuildJSON(outJSON :'OutLocDetUE#' : ALUE#);

       //*** Beg Add 03/03/15
       BuildJSON(outJSON :'OutLatitudeLongitude' : ALMAPCOOR);
       //*** Beg Add 03/03/15

       If ALNCCI = 'Y';
          BuildJSON(outJSON :'OutLocDetNCCI' : 'Yes');
       Else;
          BuildJSON(outJSON :'OutLocDetNCCI' : 'No');
       Endif;

       FormatAddress(ALpad1:ALpad2:ALpcty:ALpst:ALpzip:AdL1:AdL2:AdL3);
       BuildJSON(outJSON :'OutLocDetPadr1' :%Trim(AdL1));
       BuildJSON(outJSON :'OutLocDetPadr2' :%Trim(AdL2));
       BuildJSON(outJSON :'OutLocDetPadr3' :%Trim(AdL3));

       FormatAddress(ALmad1:ALmad2:ALmcty:ALmst:ALmzip:AdL1:AdL2:AdL3);
       BuildJSON(outJSON :'OutLocDetMadr1' :%Trim(AdL1));
       BuildJSON(outJSON :'OutLocDetMadr2' :%Trim(AdL2));
       BuildJSON(outJSON :'OutLocDetMadr3' :%Trim(AdL3));

       Clear xxKey;
       If ALpst <> ' ';
          xxkey = ALpst + ALcnty;
       Else;
          xxkey = ALmst + ALcnty;
       Endif;
       Chain(n) keyTable pcn01;
       Clear countyDesc;
       countyDesc = CNdesc;
       BuildJSON(outJSON :'OutLocDetCounty':%TrimR(countyDesc));

       Clear tempPhn#;
       tempPhn# = %Editc(ALphn#:'X');
       BuildJSON(outJSON :'OutLocDetPhone' :'(' + %Editc(ALarea:'X') + ') '
        + %subst(tempPhn#:1:3) + '-' + %subst(tempPhn#:4:4));
       Clear tempFax#;
       tempFax# = %Editc(ALFax#:'X');
       BuildJSON(outJSON :'OutLocDetFax' :'(' + %Editc(ALfara:'X') + ') '
        + %subst(tempFax#:1:3) + '-' + %subst(tempFax#:4:4));


       tableID1 = 'BUSI';
       tableID2 = ALtpob;
       Chain(n) keyWTGTL pgt01;
       If %found;
          Clear bussDesc;
          bussDesc  = GTdesc;
          Else;
          bussDesc  = %TrimR(ALtpob);
       Endif;
       BuildJSON(outJSON :'OutLocDetTypeofBus': %TrimR(bussDesc));

       tableID1 = 'SIC';
       tableID2 = ALsic;
       Chain(n) keyWTGTL pgt01;
       If %found;
          Clear sicDesc;
          sicDesc = %TrimR(ALsic) + ' - ' + GTdesc;
       Else;
          sicDesc = %TrimR(ALsic);
       Endif;
       BuildJSON(outJSON :'OutLocDetSIC': %TrimR(sicDesc));

       tableID1 = 'NAIC';
       tableID2 = ALnaic;
       Chain(n) keyWTGTL pgt01;
       If %found;
          Clear naicDesc;
          naicDesc = %TrimR(ALnaic) + ' - ' + GTdesc;
       Else;
          naicDesc = %TrimR(ALnaic);
       Endif;
       BuildJSON(outJSON :'OutLocDetNAICS': %TrimR(naicDesc));

       rtnCode = ' ';
       Return;
      /End-Free

