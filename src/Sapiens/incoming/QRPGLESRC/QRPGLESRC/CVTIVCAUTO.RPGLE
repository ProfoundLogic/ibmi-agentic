     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
     h/title CVTIVCAUTO - Conversion IVC Autopay
     h dftname(CVTIVCAUTO)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/define profoundUI
     h bnddir('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      // Created   : 11/09/23                                         *
      // Programmer: C:079                                            *
      // Project   : WC2924 - Autopay Export for IVC                  *
      *===============================================================*
      // Revisions:                                                   *
      //                                                              *
      //------------------------------------------------------------- *
      // NOTE: Please add comments to the top of Revisions.           *
      //------------------------------------------------------------- *
      *===============================================================*
       // File declarations
       // IVC EFT Workfile
     fwxivceftp uf a e             Disk
       // Employer Master file
     fwmeml     if   e             Disk
       // Employer Master file
     fwdell2    if   e           k Disk
       // System Wide Table File
     fwtabl     if   e           k Disk
       // EFT file
     fwmefp     if   e             Disk

      *===============================================================*
       // Procedure declaration
     d Main            pr

      * Out suffix.
     d rtvSuffix       pr                  extpgm('SA030R')
     d  outSuffix                     3

     d/copy sPrDate
     d/copy wPrPmtSvc
     d/copy wPrBank
     d/copy sPrRunCmd

      *===============================================================*
       // Data structures

     d psds          esds                  extname(szq1p)

      *===============================================================*
       // Array declaration

      *===============================================================*
       // Standalone variables

     d inDate          s              7a
     d intDate         s              7s 0
     d strCmd          s            200a
     d hasEft          s              1n
     d strPre          s              1a
     d strRrn          s              9a
     d strEOBTyp       s              3a
     d strCkTp         s             10a
     d strCo#          s              3a
     d strFnd          s              3a
     d strEmp#         s              9a
     d strFyr          s              3a
     d strDiv          s              5a
     d strEnv          s              1a
     d strEftKey       s             30a
     d strEfDt         s              7a
     d strEftType      s              1a
     d strBsCd         s              3a
     d intRrn          s              9s 0
     d hasData         s              1n   Inz(*off)
     d outSuffix       s              3a
     d strPath         s            200a
     d intEfId#        s              8s 0
     d n               s              1s 0

      *===============================================================*
      *entry
     c     *entry        plist
     c                   parm                    inDate

      *===============================================================*
       // Main program processing

       // If the incoming date is valid then perform processing
       intDate = %Dec(inDate:7:0);
       If dtMdYY(intDate) > 0;
          main();
       Endif;

       *inlr = *on;
       Return;


       //===================================================================
       // Main - Mainline Procedure
       //===================================================================
     p Main            b
     d Main            pi

       // Read through employer file selecting only state reporting div
       // and employer detail record where date passed in is between from/to dates
       Read pem01;
       Dow not %EOF(WMEML);
          If EMstRp = 'Y';
             Chain (EMco#:EMfnd:EMEmp#:EMdiv) pel01;
             Dow not %Eof(WDELL2);
                If intDate >= ELpfDt and intDate < ELptDt;
                   strBsCD = %Editc(ELbsCd:'X');
                   Chain (ELco#:strBsCd) pbs01;

                   // Billing schedule code must be 'E' and policy not Opted
                   // into Invoice Cloud
                   If %Found(WTABL) and BSseFl = 'E';
                      If ICPolicyOpt(ELco#:ELfnd:ELemp#) <> 'Y';
                         strCo#  = %EditC(ELCo#:'X');
                         strFnd  = %EditC(ELfnd:'X');
                         strEmp# = %EditC(ELemp#:'X');
                         strDiv  = %EditC(ELdiv:'X');
                         strFyr  = %EditC(ELFyr:'X');
                         strEftKey = strEmp# + strDiv;
                         strEfDt = %Editc(intDate:'X');
                         strEftType = 'E';

                         // EFT must be setup.  If so then write to the workfile
                         hasEFT =  GetEftStatus(strCo#:strEftType:strfnd:
                          strEftKey:strEfDt:strPre:strRrn:strEobTyp:
                          strCkTp:strFyr:strEnv);
                         If hasEft = *on;

                            // Write the record to the workfile
                            WriteWorkfile();
                            hasData = *on;
                         Endif;
                      Endif;
                   Endif;
                   Leave;
                Endif;
                Reade (EMco#:EMfnd:EMEmp#:EMdiv) pel01;
             Enddo;
          Endif;
          Read pem01;
       Enddo;

       // Write to the CSV file
       If hasData = *on;
          WriteToCSV();
       Endif;

     p Main            e

       //===================================================================
       // WriteToCSV - Write to CSV file
       //===================================================================
     p WriteToCSV      b
     d WriteToCSV      pi

          // Get suffix for hosted clients
          Callp rtvSuffix(outSuffix);
          If outSuffix = *blanks;
             strPath = '/FileDownloads';
          Else;
             strPath = '/PDS/' + %Trim(outSuffix) +
              '/FileDownloads';
          Endif;

          // Create the folder if it does not exist
          strCmd = 'MKDIR DIR(' + '''' +%Trim(strPath) + '''' + ')';
          Monitor;
          RunCmd(strCmd:%Len(%Trim(strCmd)));
          On-Error;
          Endmon;

          // Add file name to path
          strPath = %Trim(strPath) + '/EftToIvc.csv';

          // Create the CSV file
          strCmd = 'CPYTOIMPF FROMFILE(WXIVCEFTP) TOSTMF(' +
          '''' + %Trim(strPath) + '''' + ')' + ' MBROPT(*REPLACE)' +
           ' FROMCCSID(37) STMFCCSID(1252) RCDDLM(*CRLF)';

          Monitor;
          RunCmd(strCmd:%Len(%Trim(strCmd)));
          On-Error;
          Endmon;

     p WriteToCSV      e

       //===================================================================
       // WriteToWorkfile - Write to IVC EFT Workfile
       //===================================================================
     p WriteWorkfile   b
     d WriteWorkfile   pi

       IntRrn = %Dec(strRrn:9:0);
       Chain intRrn pef01;
       If %Found(WMEFP);
          IVCPol# = %EditC(ELco#:'X') + %EditC(ELfnd:'X') +
           %Editc(ELemp#:'X');
          IVCCuNm = %Trim(EMname);
          IVCacct = EFacct;
          Select;
          When EFacTp = 'C';
             IVCTypAcct = 'Personal Checking';
          When EFacTp = 'S';
             IVCTypAcct = 'Personal Savings';
          Endsl;
          // If ID# < 8 characters pad with zeros
          n = %Scan(' ':EFid#);
          If n <> 0;
             intEFid# = %Dec(%Subst(EFid#:1:n-1):8:0);
             EFid# = %Editc(intEFid#:'X');
          Endif;
          IVCrt# = EFid# + EFdgit;
          IVCbkNm = *blanks;
          IVCbkAdr = *blanks;
          IVCbkCty = *blanks;
          IVCbkSt = *blanks;
          IVCbkZip = *zeros;
          IVCdeflt = 'Y';
          IVCautoPay = 'Y';
          Write pivc01;
       Endif;

     p WriteWorkfile   e
