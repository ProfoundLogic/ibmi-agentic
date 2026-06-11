     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio: *srcstmt)
     H debug(*yes) BNDDIR('WTBNDDIR')
     H dftactgrp(*no) actgrp(*caller)

     D/copy sPrDate
     D/copy wPrAgent
     D/copy wPrEMplr
     D/copy wPrGroup
     D/copy sPrModule
     D/copy sPrSql
     D/copy wPrAR
     D/copy wPrPmtSvc

       Dcl-S strEmpName  Char(60);
       Dcl-S strQuoteLib Char(10);
       Dcl-S strLiveLib  Char(10);
       Dcl-S decInvoice# Zoned(10:0);
       Dcl-S chrCo#      Char(3);
       Dcl-S chrFnd      Char(3);
       Dcl-S chrEmp#     Char(9);
       Dcl-S chrDiv      Char(5);
       Dcl-S strSql      Char(1000) Inz;
       Dcl-S count       Zoned(5:0);

       Dcl-DS dsWMEMP extname('WMEMP') qualified inz;
       End-Ds dsWMEMP;

       // Get live library name
       strQuoteLib = RtvQuoteLib('*QTE');
       strLiveLib  = RtvQuoteLib('*LIV');

       // Find unpaid invoices.
       chrCo#  = '777';
       chrFnd  = '900';
       chrEmp# = '000002059';
       chrDiv  = '00000';

       strSql  = 'Values (Select * ' +
                 'From ' + %Trim(strQuotelib) + '/WMEMP ' +
                 'Where EMco#=' + chrCo# + ' And EMfnd=' + chrFnd +
                 ' And EMemp#=' + chrEmp# + ' And EMdiv=' + chrDiv +
                 ') Into ?';
       Clear dsWMEMP;
       exec sql PREPARE S0 FROM :strSQL;
       exec sql EXECUTE S0 USING :dsWMEMP;

       If Not IsValidFetch(sqlStt);
          strSql  = 'Values (Select * ' +
                    'From ' + %Trim(strLivelib) + '/WMEMP ' +
                    'Where EMco#=' + chrCo# + ' And EMfnd=' + chrFnd +
                    ' And EMemp#=' + chrEmp# + ' And EMdiv=' + chrDiv +
                    ') Into ?';

          Clear dsWMEMP;
          exec sql PREPARE S1 FROM :strSQL;
          exec sql EXECUTE S1 USING :dsWMEMP;
       EndIf;

       strEmpName = dsWMEMP.EMname;

       *inLr = *On;
