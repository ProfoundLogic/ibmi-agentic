     H* Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H*Option(*NoDebugio:*SrcStmt)
     H DftActGrp(*No) ActGrp(*Caller)
     H/Title W1099RTP - Third Party 1099 Extrac process
     H Bnddir('WTBNDDIR')
      *===============================================================
      * Created:
      *
      * 03/04/24 C: 291 Project: 140WCE - Third Party 1099 Extract process
      *===============================================================
      *  Revisions:                                                   *
      *                                                               *
      * 02/05/25 C:291 Support: 118398 (UBIC)                         *
      *                Grant proper authorities to user PDSxxx        *
      *                for TPY1099 folder.                            *
      *                                                               *
      *===============================================================*

       //*Entry
       Dcl-Pr W1099RTP ExtPgm('W1099RTP');
        inYear char(4);
       End-Pr;
       Dcl-Pi W1099RTP;
        inYear char(4);
       End-Pi;

       Dcl-Pr GetSuffix  ExtPgm('SA030R');
        outSuffix Char(3);
       End-Pr;

       /copy sPrRunCmd

       //Local variable
       Dcl-S outSuffix  Char(3) Inz;
       Dcl-S strFileNEC Char(40) Inz;
       Dcl-S strFileMIS Char(40) Inz;
       Dcl-S strFolder  Char(100) Inz;
       Dcl-S strMkDir   Char(100) Inz;
       Dcl-S strCmd     Char(300) Inz;
       Dcl-S strUser    Char(10) Inz;

       //********************************************************************
       // Main program processing
       //********************************************************************

       Exec Sql Set Option Commit = *None;

       // Retrieve client suffix
       GetSuffix(outSuffix);

       //Clear WDNECP and WDMSCP;
       Exec sql
        Delete from WDNECP;
       Exec sql
        Delete from WDMSCP;

       //Populate WDNECP
       Exec sql
        Insert into WDNECP
        (Select T6rnam, T6rnm2, 'EIN', digits(T6fein), 'No',
         substr(T6radr,1,35), substr(T6rad2,1,35),
         T6rcty, T6rst, T6rzip, ' ', ' ', ' ', ' ', ' ', 'No',
         'banderson@United-Business.us', '0000000000', '6787668242',
         ' ', 'No', T6amt7, 'No', 0, 0, ' ', ' ', 0, 0, ' ', ' ', 0
         From WXT6P
         Where T6amt7 <> 0);

       //Populate WDMSCP
       Exec sql
        Insert Into WDMSCP
        (Select T6rnam, T6rnm2, 'EIN', digits(T6fein), 'No',
         substr(T6radr,1,35), substr(T6rad2,1,35),
         T6rcty, T6rst, T6rzip, ' ', ' ', ' ', ' ', ' ', 'No',
         'banderson@United-Business.us', '0000000000', '6787668242',
         ' ', 'No', 0, 0, T6amt3, 0, 0, T6amt6, 'No', 0, 0, T6amtc, 0, 0,
         'No', 0, 0, 0, ' ', ' ', 0, 0, ' ', ' ', 0
         From WXT6P
         Where T6amt3 <> 0 or T6amt6 <> 0 or T6amtc <> 0);

       //Load manual 1099s into WDNECP
       Exec sql
        Insert Into WDNECP
        (Select TNpnm1, TNpnm2, 'EIN', TNfein, 'No',
         substr(TNprad,1,35), ' ' ,
         TNpcty, TNpst, TNpzip, ' ', ' ', ' ', ' ', ' ', 'No',
         'banderson@United-Business.us', '0000000000', '6787668242',
         ' ', 'No', TNpmt7, 'No', 0, 0, ' ', ' ', 0, 0, ' ', ' ', 0
         From WATNPB
         Where TNptyr = :inYear and TNpmt7 <> 0);

       //Load manual 1099s into WDMSCP
       Exec sql
        Insert Into WDMSCP
        (Select TNpnm1, TNpnm2, 'EIN', TNfein, 'No',
        substr(TNprad,1,35), ' ',
        TNpcty, TNpst, TNpzip, ' ', ' ', ' ', ' ', ' ', 'No',
        'banderson@United-Business.us', '0000000000', '6787668242',
        ' ', 'No', 0, 0, TNpmt3, 0, 0, 0, 'No', 0, 0, 0, 0, 0,
        'No', 0, 0, 0, ' ', ' ', 0, 0, ' ', ' ', 0
        From WATNPB
        Where TNptyr = :inYear and TNpmt3<>0 and TNcombine3 <> 'Y');


        // Create Directory
        strFolder = '/docroot/pds';
        If outSuffix <> *Blank;
           strFolder = %Trim(strFolder) + '/' + %Trim(outSuffix);
        Endif;
        strFolder = %Trim(strFolder) + '/TPY1099';
        strMkDir = 'MKDIR DIR(''' + %trim(strFolder) + ''')';
        Monitor;
         RunCmd(strMkDir:%Len(%Trim(strMkDir)));
        On-error;
        Endmon;

        //*** Beg Add ***** 02/05/25 ********************************
        //* Attempt to update permissions
        Monitor;
           clear strCmd;
           strUser = 'PDS' + %Trim(outSuffix);
           strCmd = 'CHGAUT OBJ(''' + %trim(strFolder) + ''') USER(' +
            %trim(strUser) + ') ' +
            'DTAAUT(*RWX) OBJAUT(*ALL)';
           RunCmd(strCmd:%Len(%Trim(strCmd)));
        On-error;
        Endmon;
        //*** End Add ***** 02/05/25 ********************************

        // Create File NEC1099.csv
        strFileNEC = 'NEC1099'+ %char(%date():*iso0) + '_' +
         %char(%time():*iso0);
        strCmd = 'CPYTOIMPF FromFile(*libl/wdnecp) ToStmF(''' +
         %Trim(strFolder) + '/' + %Trim(strFileNEC) + '.csv'') ' +
         'MbrOpt(*replace) StmfCCSID(*PCASCII) RcdDlm(*CRLF) ';

        Monitor;
         RunCmd(strCmd:%Len(%Trim(strCmd)));
        On-error;
        Endmon;

        // Create File MIS1099.csv
        strFileMIS = 'MIS1099'+ %char(%date():*iso0) + '_' +
         %char(%time():*iso0);
        strCmd = 'CPYTOIMPF FromFile(*libl/wdmscp) ToStmF(''' +
         %Trim(strFolder) + '/' + %Trim(strFileMIS) + '.csv'') ' +
         'MbrOpt(*replace) StmfCCSID(*PCASCII) RcdDlm(*CRLF) ';
        Monitor;
         RunCmd(strCmd:%Len(%Trim(strCmd)));
        On-error;
        Endmon;

       *InLr = *On;

      /End-Free
