     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
    H*********************************************************************
    H/TITLE Automate Rate Set Data Upload Process
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DEBUG(*YES)
     H bnddir('WTBNDDIR')
    H*********************************************************************
    H*  CALLED FROM: W8621C                                              *
    H*                                                                   *
    H*  CALLS TO:  W8622R                                                *
    H*                                                                   *
    H*  CREATED: 06/29/23      C:291     PROJECT: WC2915                 *
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
     H* 11/14/24 C:291 Support: PHW118010                                 *
     H*                Fix edits on numeric fields: E8EXDT, E8LCEX, P1EXDT*
    H*                                                                   *
     H* 11/13/24 C:344 Project: WC2952                                    *
     H*                Increase Rate field (E8RT) to 8.3.                 *
    H*                                                                   *
     H* 01/15/24 C:291 Support: PHW114879                                 *
     H*                Fix edits on numeric fields: E8CLAS, E8RT, E8MNE8  *
     H*                and P1CLAS.                                        *
    H*                                                                   *
     H* 06/29/23 C:291 Project: WC2915                                    *
     H*                Convert to CompSuite and Rewrite.                  *
     H*                Change files WXPRP and WXPXP1 to have all alpha    *
     H*                fields.                                            *
     H*                                                                   *
    H*********************************************************************

       Dcl-F wxprp  Usage(*Input) Infds(DBFDS);
       Dcl-F wxpxp1 Usage(*Input) Infds(DBFDS1);
       Dcl-F WMRSL  Keyed Usage(*Input);
       Dcl-F SMUPL  Keyed Usage(*Input);
       Dcl-F wmrsl1 Keyed Usage(*Input) Rename(prs01:prs02);
       Dcl-F wmprl1 Keyed Usage(*Input) Rename(ppr01:ppr01);
       Dcl-F ststl1 Keyed Usage(*Input) Rename(pst01:pst01);
       Dcl-F wtgtl  Keyed Usage(*Input) Rename(pgt01:pgt00);
       Dcl-F W8621P Printer OFlind(InOf);

       // *Entry
       Dcl-Pr W8621R ExtPgm('W8621R');
          outUpdFlg char(1);
          outUpdFlg1 char(1);
       End-Pr;
       Dcl-Pi W8621R;
          outUpdFlg char(1);
          outUpdFlg1 char(1);
       End-Pi;

        //Program Status Data Structure
       Dcl-Ds SZQ1P ExtName('SZQ1P') psds;
       End-Ds;

       //File Information Data Structure
       Dcl-Ds DBFDS ;
         dbrrn int(10) pos(397);
       End-Ds;
       //File Information Data Structure
       Dcl-Ds DBFDS1 ;
         dbrrn1 int(10) pos(397);
       End-Ds;

      //Stand alone fields
       Dcl-s xxCO# packed(3:0);
       Dcl-s errInd Ind Inz(*Off);
       Dcl-s hdrPrt Ind Inz(*Off);
       Dcl-s qqUpdFlg char(1);
       Dcl-s qqUpdFlg1 char(1);
       Dcl-s hasFileError Ind Inz(*Off);
       Dcl-s strErrMsg char(150);
       Dcl-s isRequired Ind Inz(*On);
       Dcl-s isNotRequired Ind Inz(*Off);
       Dcl-s xxKey  char(10);
       Dcl-s xxTBid char(4);
       Dcl-s xxJRST char(2);

      //Constants
       Dcl-c validChars 'ABCDEFGHIJKLMNOPQRSTUVWXYZ+
                        abcdefghijklmnopqrstuvwxyz+
                        0123456789 ()<>!;%$#*?@+&^=-_":/''.,~';

      /copy sprdate
      /copy wprusrprms

      //===============================================================
      // Main Processing
      //===============================================================
       ProgramStart();
       //Process Payroll Class Code Rates file
       qqUpdFlg = ' ';             //No records in file WXPRP
       hasFileError = *Off;
       //Check if there is data in file WXPRP
       Setll 1 wxprp;
       Read wxprp;
       If not %Eof(wxprp);
          qqUpdFlg = 'N';           //Data in file
       Endif;
       Dow not %eof(wxprp);
          BadDataChk('WXPRP');
          Read wxprp;
       Enddo;
       If hasFileError = *Off;
          ErrorChkData('WXPRP');
          If hasFileError = *Off and errInd = *Off;
             QQUPDFLG = 'Y';          //Records in file WXPRP and NO error
          Endif;
       Endif;

       //Process Payroll Class Code Description File
       qqUpdFlg1 = ' ';             //No records in file WXPXP1
       hasFileError = *Off;
       Setll 1 wxpxp1;
       Read wxpxp1;
       If not %Eof(wxpxp1);
          qqUpdFlg1 = 'N';           //Data in file
       Endif;
       Dow not %eof(wxpxp1);
          BadDataChk('WXPXP1');
          Read wxpxp1;
       Enddo;
       If hasFileError = *Off;
          ErrorChkData('WXPXP1');
          If qqUpdFlg1 = 'N' and hasFileError = *Off;      //Records in wxpxp1 and NO bad data
             qqUpdFlg1 = 'Y';        //Records in file wxpxp1 and NO error
          Endif;
       Endif;

       outUpdFlg  = qqUpdFlg;
       outUpdFlg1 = qqUpdFlg1;
       *Inlr = *On;

      //----------------------------------------------------------------
      // BadDataChk - Invalid data check in all the incoming fields
      //----------------------------------------------------------------
       Dcl-Proc BadDataChk;
       Dcl-Pi *N;
        inFileNm char(25) const;
       End-Pi;
       Select;
       When inFileNm = 'WXPRP';
          //Check Alpha fields
          InvalidChar('Rate Set Code':E8RTCD:'WXPRP':DBRRN);
          InvalidChar('Description':E8DESC:'WXPRP':DBRRN);
          InvalidChar('Hazard Group':E8HZGR:'WXPRP':DBRRN);
          InvalidChar('Payroll Per Capita Flag':E8PCF:'WXPRP':DBRRN);
          InvalidChar('L&H Designation':E8LHCD:'WXPRP':DBRRN);
          InvalidChar('Contractors Class':E8CCFL:'WXPRP':DBRRN);
          InvalidChar('SIC Code':E8SIC:'WXPRP':DBRRN);
          InvalidChar('Exposure Act':E8EACT:'WXPRP':DBRRN);
          InvalidChar('Rating Tier':E8TIER:'WXPRP':DBRRN);
          InvalidChar('Governing Class Exculsion':E8GVEX:'WXPRP':DBRRN);
          InvalidChar('A Rate Flag':E8ARFL:'WXPRP':DBRRN);
          InvalidChar('Seasonal Class Flag':E8SCCD:'WXPRP':DBRRN);
          InvalidChar('Occupational Disease Class':E8OCCD:'WXPRP':DBRRN);
          InvalidChar('Logging Flag':E8LOGF:'WXPRP':DBRRN);
          InvalidChar('NCCI Industry Group':E8NCIG:'WXPRP':DBRRN);
          InvalidChar('Non-Ratable Class Code':E8NRAT:'WXPRP':DBRRN);
          //Check numeric fields in WXPRP
          InvalidNumber('Class Code':E8CLAS:'WXPRP':DBRRN);
          InvalidNumber('Payroll Class Rate':E8RT:'WXPRP':DBRRN);
          InvalidNumber('Minumum Premium':E8MNE8:'WXPRP':DBRRN);
          InvalidNumber('Loss Constant':E8LOSS:'WXPRP':DBRRN);
          InvalidNumber('Transfer Class':E8XFER:'WXPRP':DBRRN);
          InvalidNumber('Loss Cost Multiplier':E8LCM:'WXPRP':DBRRN);
          //Check incoming Date fields
          InvalidDate('Effective Date':E8EFDT:'WXPRP':DBRRN:isRequired);
          InvalidDate('Expiration Date':E8EXDT:'WXPRP':DBRRN:isNotRequired);
          InvalidDate('Loss Cost Mult Exp Dt':E8LCEX:'WXPRP':DBRRN
           :isNotRequired);
       When inFileNm = 'WXPXP1';
          //Check Alpha fields
          InvalidChar('Class Desc Extension':P1EXT:'WXPXP1':DBRRN1);
          InvalidChar('Extension Desc':P1DESC:'WXPXP1':DBRRN1);
          InvalidChar('SIC Code':P1SIC:'WXPXP1':DBRRN1);
          InvalidChar('Status':P1STAT:'WXPXP1':DBRRN1);
          InvalidChar('State':P1ST:'WXPXP1':DBRRN1);
          //Check numeric fields in WXPXP1
          InvalidNumber('Class Code':P1CLAS:'WXPXP1':DBRRN1);
          InvalidNumber('Statistical Code':P1CODE:'WXPXP1':DBRRN1);
          //Check incoming Date fields
          InvalidDate('Effective Date':P1EFDT:'WXPXP1':DBRRN1:isRequired);
          InvalidDate('Expiration Date':P1EXDT:'WXPXP1':DBRRN1:isNotRequired);
       Endsl;
       End-Proc;

       //----------------------------------------------------------------
       // ErrorChkData - Check data errors in all the work files
       //----------------------------------------------------------------
       Dcl-Proc ErrorChkData;
       Dcl-Pi *N;
        inFileNm char(25) const;
       End-Pi;
       Select;
       //Process WXPRP for Data check
       When inFileNm = 'WXPRP';
          Setll 1 wxprp;
          Read wxprp;
          Dow Not %Eof(wxprp);
             //Rate Set Code
             Chain (xxCo#:E8Rtcd) Prs01;
             If %Found(wmrsl);
                If RSstat = 'I';
                   strErrMsg = 'Rate set code not active.';
                   WriteErrMsg(inFileNm:'Rate Set Code':E8RTCD:DBRRN:strErrMsg);
                Endif;
                xxJRST = RSJRST;
             Else;
                strErrMsg = 'Rate set code not found.';
                WriteErrMsg(inFileNm:'Rate Set Code':E8RTCD:DBRRN:strErrMsg);
                xxJRST = *Blank;
             Endif;

             //Class Code
             //*** Beg Chg ***** 01/15/24 *******************************
             //***IF %Int(E8CLAS) <= 0;
             //***   strErrMsg = 'Class code is invalid: +
             //***       Must be greater than zero.';
             //***   WriteErrMsg(inFileNm:'Class Code':E8CLAS:DBRRN:strErrMsg);
             //***ENDIF;
             If E8CLAS = *Blank;
                strErrMsg = 'Class code cannot be blank.';
                WriteErrMsg(inFileNm:'Class Code':E8CLAS:DBRRN:strErrMsg);
             Else;
                If %Int(E8CLAS) <= 0;
                   strErrMsg = 'Class code is invalid: +
                       Must be greater than zero.';
                   WriteErrMsg(inFileNm:'Class Code':E8CLAS:DBRRN:strErrMsg);
                Endif;
             Endif;
             //*** End Chg ***** 01/15/24 *******************************

             //Effective Date
             If dtCheck(DtMDYY(%Int(E8EFDT))) = 0;
                strErrMsg = 'Invalid effective date.';
                WriteErrMsg(inFileNm:'Effective Date':E8EFDT:DBRRN:strErrMsg);
             Endif;

             //Description
             IF E8DESC = *BLANKS;
                strErrMsg = 'Description cannot be blank.';
                WriteErrMsg(inFileNm:'Description':E8DESC:DBRRN:strErrMsg);
             ENDIF;

             //Class Rate
             //*** Beg Chg ***** 01/15/24 *******************************
             //***IF %Dec(E8RT:7:2) < 0;
             //***   strErrMsg = 'Invalid class rate: Must be greater than zero.';
             //***   WriteErrMsg(inFileNm:'Class Rate':E8RT:DBRRN:strErrMsg);
             //***ENDIF;
             If E8RT <> *Blank;
                //*** Beg Chg ***** 11/13/24 ****************************
                //***If %Dec(E8RT:7:2) < 0;
                If %Dec(E8RT:8:3) < 0;
                //*** End Chg ***** 11/13/24 ****************************
                   strErrMsg = 'Invalid class rate: Must be greater than zero.';
                   WriteErrMsg(inFileNm:'Class Rate':E8RT:DBRRN:strErrMsg);
                Endif;
             Endif;
             //*** End Chg ***** 01/15/24 *******************************

             //Minimum Premium
             //*** Beg Chg ***** 01/15/24 *******************************
             //***IF %Dec(E8MNE8:7:2) < 0;
             //***   strErrMsg = 'Invalid Min Premium: +
             //***    Cannot be less than zero.';
             //***   WriteErrMsg(inFileNm:'Minumum Premium':E8MNE8:DBRRN:strErrMsg);
             //***ENDIF;
             If E8MNE8 <> *Blank;
                If %Dec(E8MNE8:7:2) < 0;
                   strErrMsg = 'Invalid Min Premium: +
                    Cannot be less than zero.';
                   WriteErrMsg(inFileNm:'Minumum Premium':E8MNE8:DBRRN:
                    strErrMsg);
                Endif;
             Endif;
             //*** End Chg ***** 01/15/24 *******************************

             //Payroll Per Capita Flag
             IF E8PCF <>'P' AND E8PCF <> 'C' AND E8PCF <>'O';
                strErrMsg = 'Invalid Payroll Per Capita Flag.';
                WriteErrMsg(inFileNm:'Payroll Per Capita Flag':E8PCF:
                 DBRRN:strErrMsg);
             ENDIF;

             //L & H Designation flag
             IF E8LHCD <>'Y' AND E8LHCD <> 'N';
                strErrMsg = 'Invalid L & H Designation flag.';
                WriteErrMsg(inFileNm:'L&H Designation':E8LHCD:DBRRN:strErrMsg);
             ENDIF;

             //Contractor Class flag
             IF E8CCFL <>'Y' AND E8CCFL <> 'N';
                strErrMsg = 'Invalid Contractor Class flag.';
                WriteErrMsg(inFileNm:'Contractors Class':E8CCFL:
                 DBRRN:strErrMsg);
             ENDIF;

             //Governing Class
             IF E8GVEX <>'Y' AND E8GVEX <> *BLANKS;
                strErrMsg = 'Invalid Governing Class Exclusion flag.';
                WriteErrMsg(inFileNm:'Governing Class Exculsion':E8GVEX:
                 DBRRN:strErrMsg);
             ENDIF;

             //Expiration date
             //***Beg Chg ****** 11/14/24 ***************************
             //***IF %Int(E8EXDT) <> 0;
             //***   If dtCheck(DtMDYY(%Int(E8EXDT))) = 0;
             //***      strErrMsg =  'Invalid expiration date.';
             //***      WriteErrMsg(inFileNm:'Expiration Date':E8EXDT:
             //***       DBRRN:strErrMsg);
             //***   Endif;
             //***ENDIF;
             If E8EXDT <> *Blank;
                IF %Int(E8EXDT) <> 0;
                   If dtCheck(DtMDYY(%Int(E8EXDT))) = 0;
                      strErrMsg =  'Invalid expiration date.';
                      WriteErrMsg(inFileNm:'Expiration Date':E8EXDT:
                       DBRRN:strErrMsg);
                   Endif;
                Endif;
             Endif;
             //***End Chg ****** 11/14/24 ***************************

             //A Rate flag
             IF E8ARFL <> 'Y' AND E8ARFL <> *BLANKS;
                strErrMsg = 'Invalid A Rate flag.';
                WriteErrMsg(inFileNm:'A RateFlag':E8ARFL:DBRRN:strErrMsg);
             ENDIF;

             //Seasonal Class flag
             IF E8SCCD <>'Y' AND E8SCCD <> *BLANKS;
                strErrMsg = 'Invalid Seasonal Class flag.';
                WriteErrMsg(inFileNm:'Seasonal Class Flag':E8SCCD:
                 DBRRN:strErrMsg);
             ENDIF;

             //Occupational Disease Class flag
             IF E8OCCD <>'Y' AND E8OCCD <> *BLANKS;
                strErrMsg = 'Invalid Occupational Disease Class flag.';
                WriteErrMsg(inFileNm:'Occupational Disease Class':E8OCCD:
                 DBRRN:strErrMsg);
             ENDIF;

             //Loss Cost Factor expiration date
             //***Beg Chg ****** 11/14/24 ***************************
             //***If %Int(E8LCEX) <> 0;
             //***   If dtCheck(DtMDYY(%Int(E8LCEX))) = 0;
             //***      strErrMsg = 'Invalid Loss Cost Factor expiration date.';
             //***      WriteErrMsg(inFileNm:'Loss Cost Mult Exp Dt':E8LCEX:
             //***       DBRRN:strErrMsg);
             //***   Endif;
             //***Endif;
             If E8LCEX <> *Blank;
                If %Int(E8LCEX) <> 0;
                   If dtCheck(DtMDYY(%Int(E8LCEX))) = 0;
                      strErrMsg = 'Invalid Loss Cost Factor expiration date.';
                      WriteErrMsg(inFileNm:'Loss Cost Mult Exp Dt':E8LCEX:
                       DBRRN:strErrMsg);
                   Endif;
                Endif;
             Endif;
             //***End Chg ****** 11/14/24 ***************************

             //Exposure Act
             If E8EACT <> *Blank;
                xxKey = E8EACT;
                %Subst(xxTBID:1:2) = xxJRST;
                %Subst(xxTBID:3:2) = 'XA';
                Chain (xxCo#:xxTBID:xxKey) pgt00;
                If not %found(wtgtl) and xxJRST <> *Blank;
                   xxTBID = '  XA';
                   Chain (xxCo#:xxTBID:xxKey) pgt00;
                Endif;
                If not %found(wtgtl);
                   strErrMsg = 'Invalid Exposure Act.';
                   WriteErrMsg(inFileNm:'Exposure Act':E8EACT:
                    DBRRN:strErrMsg);
                Else;                      //Found
                   If GTstat = 'I';
                      strErrMsg = 'Exposure Act is not Active';
                      WriteErrMsg(inFileNm:'Exposure Act':E8EACT:
                       DBRRN:strErrMsg);
                   Endif;
                Endif;
             Endif;

             //Rating Tier
             If E8TIER <> *Blank;
                xxKey = E8TIER;
                %Subst(xxTBID:1:2) = xxJRST;
                %Subst(xxTBID:3:2) = 'RT';
                Chain (xxCo#:xxTBID:xxKey) pgt00;
                If not %found(wtgtl) and xxJRST <> *Blank;
                   xxTBID = '  RT';
                   Chain (xxCo#:xxTBID:xxKey) pgt00;
                Endif;
                If not %found(wtgtl);
                   strErrMsg = 'Invalid Rating Tier.';
                   WriteErrMsg(inFileNm:'Rating Tier':E8TIER:
                    DBRRN:strErrMsg);
                Else;                      //Found
                   If GTstat = 'I';
                      strErrMsg = 'Rating Tier is not Active';
                      WriteErrMsg(inFileNm:'Rating Tier':E8TIER:
                       DBRRN:strErrMsg);
                   Endif;
                Endif;
             Endif;
             Read wxprp;
          Enddo;
       When inFileNm = 'WXPXP1';
          //Process WXPXP1 for Data check
          Setll 1 wxpxp1;
          Read wxpxp1;
          Dow Not %Eof(wxpxp1);
             //Class code
             //*** Beg Add ***** 01/15/24 *******************************
             If P1Clas = *Blanks;
                strErrMsg = 'Class code cannot be blank.';
                 WriteErrMsg(inFileNm:'Class Code':P1clas:DBRrn1:
                   strErrMsg);
             Else;
             //*** End Add ***** 01/15/24 *******************************
                If %Int(P1Clas) <= 0;
                   strErrMsg = 'Class code is invalid: Must be greater ' +
                    'than zero.';
                    WriteErrMsg(inFileNm:'Class Code':P1clas:DBRrn1:
                      strErrMsg);
                Else;
                   //Check if there is a rate set code setup for this state
                   Chain (xxCo#:P1st) prs02;
                   If Not %Found(wmrsl1);
                      strErrMsg = 'No Rate Set Code exist for state: '+
                       %trim(P1st) + '.';
                       WriteErrMsg(inFileNm:'State':P1st:DBRrn1:
                        strErrMsg);
                   Endif;
                Endif;               //P1Clas <= 0;
             //*** Beg Add ***** 01/15/24 *******************************
             Endif;
             //*** End Add ***** 01/15/24 *******************************

             //Extension
             If P1Ext = *Blank;
                strErrMsg = 'Extension cannot be blank.';
                   WriteErrMsg(inFileNm:
                    'Class Desc Extension':P1Ext:DBRrn1: strErrMsg);
             Endif;

             //Description
             If P1Desc = *Blank;
                strErrMsg = 'Extension Description cannot be blank.';
                WriteErrMsg(inFileNm:'Extension Description':P1Desc:
                 DBRrn1: strErrMsg);
             Endif;

             //Effective Date
             If dtCheck(DtMDYY(%Int(P1Efdt))) = 0;
                strErrMsg = 'Invalid effective Date';
                WriteErrMsg(inFileNm:'Effective Date':P1Efdt:DBRrn1: strErrMsg);
             Endif;

             //Expiration Date
             //***Beg Chg ****** 11/14/24 ***************************
             //***If %Int(P1Exdt) <> 0;
             //***   If dtCheck(DtMDYY(%Int(P1Exdt))) = 0;
             //***      strErrMsg = 'Invalid expiration Date';
             //***      WriteErrMsg(inFileNm:'Expiration Date':P1Exdt:
             //***       DBRrn1:strErrMsg);
             //***   Endif;
             //***Endif;
             If P1Exdt <> *Blank;
                If %Int(P1Exdt) <> 0;
                   If dtCheck(DtMDYY(%Int(P1Exdt))) = 0;
                      strErrMsg = 'Invalid expiration Date';
                      WriteErrMsg(inFileNm:'Expiration Date':P1Exdt:
                       DBRrn1:strErrMsg);
                   Endif;
                Endif;
             Endif;
             //***End Chg ****** 11/14/24 ***************************

             //State
             If P1St <> *Blank;
                Chain (P1St) pst01;
                If not %found(ststl1);
                   strErrMsg = 'Invalid State';
                   WriteErrMsg(inFileNm:'State':P1St:DBRrn1: strErrMsg);
                Endif;
             Endif;

             //SIC Code
             If P1sic <> *Blank;
                Chain (xxCo#:'SIC':P1sic) pgt00;
                If not %found(wtgtl);
                   strErrMsg = 'Invalid SIC code';
                   WriteErrMsg(inFileNm:'SIC Code':P1sic:DBRrn1: strErrMsg);
                Endif;
             Endif;
             Read wxpxp1;
          Enddo;
       Endsl;

       End-Proc;
      //----------------------------------------------------------------
      // InvalidChars - look for invalid characters in Alpha fields
      //----------------------------------------------------------------
       Dcl-Proc InvalidChar;
       Dcl-Pi *N;
        inFldName char(25)  const;
        inValueToChk char(100) const;
        inFileName char(25) const;
        inRrn Int(10:0) const;
       End-Pi;

       If inValueToChk <> *Blank;
          strErrMsg = *Blank;
          If %Check(validChars:inValueToChk) <> 0;
             strErrMsg = 'Invalid characters in data';
          Endif;
          If strErrMsg <> *Blank;
             WriteErrMsg(inFileName:inFldName:inValueToChk:inRrn:strErrMsg);
          Endif;
       Endif;

       End-Proc;
      //----------------------------------------------------------------
      // InvalidDate - Check if Date is Invalid
      //----------------------------------------------------------------
       Dcl-Proc InvalidDate;
       Dcl-Pi *N;
        inFldName char(25) const;
        inValueToChk char(10) const;
        inFileName char(25) const;
        inRrn Int(10:0) const;
        isRequired Ind const;
       End-Pi;
       Dcl-s rptDate Date(*usa);

       strErrMsg = *Blank;
       Monitor;
         rptDate = %date(%Int(inValueToChk):*usa);
       On-Error;
          Select;
          When Not(isRequired) and inValueToChk = *blanks or
           inValueToChk = '0';
          When inValueToChk = *blanks and isRequired;
             strErrMsg = 'Value is blank. This field is required.';
          Other;
             strErrMsg = 'Invalid Date';
          Endsl;
       EndMon;
       If strErrMsg <> *Blank;
          WriteErrMsg(inFileName:inFldName:inValueToChk:inRrn:strErrMsg);
       Endif;

       End-Proc;
      //----------------------------------------------------------------
      // InvalidNumber - look for invalid data in numeric field
      //----------------------------------------------------------------
       Dcl-Proc InvalidNumber;
       Dcl-Pi *N;
        inFldName char(25) const;
        inValueToChk char(15) const;
        inFileName char(25) const;
        inRrn Int(10:0) const;
       End-Pi;
       Dcl-s intNumber zoned(11:0);

       If inValueToChk <> *Blank;
          strErrMsg = *Blank;
          Monitor;
             intNumber = %Int(inValueToChk);
          On-Error;
             strErrMsg = 'Numeric field has invalid data';
          EndMon;
          If strErrMsg <> *Blank;
             WriteErrMsg(inFileName:inFldName:inValueToChk:inRrn:strErrMsg);
          Endif;
       Endif;

       End-Proc;
      //---------------------------------------------------------------
      // programStart - Initialization
      //---------------------------------------------------------------
       Dcl-Proc ProgramStart;
        xxCo# = userCo#(q1user);
        qqUpdFlg  = ' ';
        qqUpdFlg1 = ' ';
       End-Proc;

      //----------------------------------------------------------------
      // WriteErrMsg - Write error message
      //----------------------------------------------------------------
       Dcl-Proc WriteErrMsg;
        Dcl-Pi *N;
         inFileNm Char(25) const;
         inFldNm Char(25) const;
         inValue char(15) const;
         inRrn Int(10:0) const;
         inMessage Char(150) const;
        End-Pi;
        PRRRN = inRrn;
        PRFLNM = inFileNm;
        PRFLDN = inFldNm;
        PRFLDV = inValue;
        PRERRM = inMessage;
        If HdrPrt = *Off;
           Write PrtHdr;
           HdrPrt = *On;
        Endif;
        If InOf = *On;
           Write PrtHdr;
           InOf = *Off;
        Endif;
        Write PrtBk1;
        ErrInd = *On;
        hasFileError = *On;
       End-Proc;

