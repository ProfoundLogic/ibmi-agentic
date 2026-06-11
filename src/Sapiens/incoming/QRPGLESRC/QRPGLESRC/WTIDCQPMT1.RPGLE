     h**********************************************************************
     hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
      **********************************************************************
      * Created: 8/5/2019 C:079 Project: WC2604
      **********************************************************************
      * Revisions:
      *
      **********************************************************************
     FWMIDCSC   IF   E             disk
      * Entry
     dGetIDCDetail     pr                  ExtPgm('WTIDCQPMT1')
     d                                9  0
     d                            32767
     d                                1
     dGetIDCDetail     pi
     d inRrn                          9  0
     d outJSON                    32767
     d RtnCode                        1

     d RptQtrCd        s              3
     d xxStatus        s             10

      /copy sPrAtrium

      /free
       outJSON  = ' ';

       // Load detail for IDC Prior Quarterly Files
       Exsr ProcessIDC;

       RtnCode = ' ';
       Return;
       //==================================================================
       Begsr ProcessIDC;
        Chain(n) inRRN pcc01;
        If %found(wmidcsc);
           BuildJSON(outJSON:'OutSubFileIDDtl':CCSubFilID);
           BuildJSON(outJSON:'OutBchIDDtl':CCBchID);
           BuildJSON(outJSON:'OutSubDateDtl':CCSubDate);
           BuildJSON(outJSON:'OutSubTypeCodeDtl':CCSubTyCd);
           Select;
           When CCrptQtrCd = '1';
              RptQtrCd = '1st';
           When CCrptQtrCd = '2';
              RptQtrCd = '2nd';
           When CCrptQtrCd = '3';
              RptQtrCd = '3rd';
           When CCrptQtrCd = '4';
              RptQtrCd = '4th';
           Endsl;
           BuildJSON(outJSON:'OutRptQtrCodeDtl':RptQtrCd);
           Select;
           When CCrunt = 'O' and CCsts = 'S';
              xxStatus = 'Sent';
           When CCrunt = 'S' and CCsts = 'S';
              xxStatus = 'Resent';
           When CCrunt = 'R' and CCsts = 'R';
              xxStatus = 'Replaced';
           When CCrunt = 'D' and CCsts = 'D';
              xxStatus = 'Deleted';
           Other;
              xxStatus = 'Not sent';
           Endsl;
           BuildJSON(outJSON:'OutStatusDtl':xxStatus);
           BuildJSON(outJSON:'OutRunTypeDtl':CCRunt);
           BuildJSON(outJSON:'OutCarrGroupCdDtl':CCcarGpCd);
           BuildJSON(outJSON:'OutReportYearDtl':CCrptYear);
           BuildJSON(outJSON:'OutRecordTotalDtl':CCrecTot);
        Endif;
       Endsr;

       //==================================================================
      /end-free

