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

       Dcl-S strQuoteLib Char(10);
       Dcl-S decInvoice# Zoned(10:0);
       Dcl-S chrCo#      Char(3);
       Dcl-S chrFnd      Char(3);
       Dcl-S chrEmp#     Char(9);
       Dcl-S chrDiv      Char(5);
       Dcl-S chrInv#     Char(10);
       Dcl-S strSql      Char(1000) Inz;
       Dcl-S strSql2     Like(strSql) Inz;
       Dcl-S strSql3     Like(strSql) Inz;
       Dcl-S count       Zoned(5:0);


       // Get live library name
       strQuoteLib = RtvQuoteLib('*QTE');

       // Find unpaid invoices.
       chrCo#  = '777';
       chrFnd  = '900';
       chrEmp# = '000502143';
       chrDiv  = '00000';
       strSql  = 'Select EAinv#  ' +
                 'From ' + %Trim(strQuotelib) + '/WDEAP_HLD ' +
                 'Where EAco#=' + chrCo# + ' And EAfnd=' + chrFnd +
                 ' And EAemp#=' + chrEmp# + ' And EAdiv=' + chrDiv;

       // Define global variable to hold A/R Count
       Exec Sql Drop Variable AR_Count;
       Exec Sql Create Or Replace Variable AR_Count Decimal(5,0);

       Exec SQL close batchDelCursor;
       Exec SQL Prepare batchDel From :strSQL;
       Exec SQL Declare batchDelCursor Cursor For batchDel;
       Exec SQL Open batchDelCursor;

       Dou Not IsValidFetch(sqlStt);
          Exec SQL Fetch Next From batchDelCursor Into :decInvoice#;
          If isValidFetch(sqlStt);

             // Check to see if there is any A/R which means it is paid
             chrInv# = %EditC(decInvoice#:'X');

             // Set AR_Count
             strSql3 = 'Set AR_count = ' +
                       '(Select count(*) ' +
                       'From ' + %Trim(strQuotelib) + '/WDEAP_HLD ' +
                       'Where EAco#=' + chrCo# + ' And EAfnd=' + chrFnd +
                       ' And EAemp#=' + chrEmp# + ' And EAdiv=' + chrDiv +
                       ' And EAinv#=' + chrInv# + ' And EAeaTp = ''IP'')';
             Exec Sql Execute Immediate :strSql3;

             // Load global variable into work field
             exec sql Set :count = AR_Count;

             // Drop global variable

             // Skip if there the invoice was paid
             If count <> 0;
                iter;
             EndIf;

             Return;

          EndIf;
       EndDo;

       Exec Sql Drop Variable AR_Count;
       Exec SQL close batchDelCursor;

       *inLr = *On;
