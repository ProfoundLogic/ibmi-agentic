     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
      *  Description: Response program to serve Policy Contact detail box
      *               - Activation group should be *CALLER
      *               - Do not seton LR, just do a return
      *               - This process should not lock any file
      *               - This process should only read files, no updates/writes should be made
      *
      *  Called from: WTPOLCONT
      *  Calls to:
      *
      *  Created on 07/30/2012 by C:279 for Project Transform
      **********************************************************************
      * Revisions:
      *
      * 08/29/24 C:333 Project: WC2826 (117276/116980)
      *                Claims Contact.
      *
      * 04/11/24 C:340 Project: 129WXP
      *                Add Distribution Types before Status on Grid
      *
      * 02/25/19 C:136 Project: SY407 (US586/TA1005)
      *                Add Self Reporting Notification and Email Address
      *                to detail box
      *
      * 04/17/15 C:242 Project: 132WQS                               *
      *                Retrofit of Custom Code to Base for CompSuite:*
      *                Added Preferred Billing Method.               *
      *
      **********************************************************************
      * file
     fwmecp     if   e             Disk    reName(pec01:pec0p)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwmexl     if   e           k disk    rename(pex01:pex00)
      *** Beg Add ***** 04/11/24 *** 129WXP ******************
     fwmeclcrt  if   e           k disk    rename(pec01:pec0p_cl)
     f                                     prefix(CL_)
     fSTDsl     if   e           k disk
      *** End Add ***** 04/11/24 *** 129WXP ******************

      /copy sPrDate
      /copy sPrString
      /copy sPrAtrium
      /copy wPrUsrPrms
      /copy wPrTable

     d* Get Entity/Location Information
     d GetEnLocInfo    pr

     d* Get Employee Contact Information
     d GetEmpCnInfo    pr

     d* Get Division Information
     d GetDivInfo      pr

     d* Clear Detail box
     d ClearDtlBox     pr

      * *Entry
     dPolicyContact01  pr                  ExtPgm('WTPOLCON01')
     d                                9  0
     d                            32767
     d                                1

     dPolicyContact01  pi
     d inRRN                          9  0
     d outJSON                    32767
     d RtnCode                        1

     d ##co#           s                   like(EMco#)
     d ##fnd           s                   like(EMfnd)
     d ##emp#          s                   like(EMemp#)
     d ##div           s                   like(EMdiv)
     d strname         s             50    inz(*blanks)
      *** Beg Del ***** 08/29/24 ********************************
     d*strpst          s             40    inz(*blanks)
     d*strDtlphn#      s             30    inz(*blanks)
      *** End Del ***** 08/29/24 ********************************
     d strDtlEphn#     s             30    inz(*blanks)
      *** Beg Del ***** 08/29/24 ********************************
     d*strDtlcell#     s             30    inz(*blanks)
     d*strDtlfax#      s             30    inz(*blanks)
      *** End Del ***** 08/29/24 ********************************
     d strDtlEfax#     s             30    inz(*blanks)
     d strDtlCnfr      s             40    inz(*blanks)
     d strDtlPAd1      s             40    inz(*blanks)
     d strDtlPAd2      s             40    inz(*blanks)
     d strDtlMAd1      s             40    inz(*blanks)
     d strDtlMAd2      s             40    inz(*blanks)
      *** Beg Del ***** 08/29/24 ********************************
     d*strDtlCAd1      s             40    inz(*blanks)
     d*strDtlCAd2      s             40    inz(*blanks)
     d*strDtlcsz1      s             40    inz(*blanks)
      *** End Del ***** 08/29/24 ********************************
     d strDtlcsz2      s             40    inz(*blanks)
     d strDtlcsz3      s             40    inz(*blanks)
     d*** Beg add ******** 04/17/15 *****************************
     d strDtlPEF       s             10    inz(*blanks)
     d*** Beg add ******** 04/17/15 *****************************
     d*** beg add ***** 02/25/19 ********************************
     d strSrNoFlg      s              3a
      *** Beg Del ***** 08/29/24 ********************************
     d*strDtlEmailAdd  s             60a
      *** End Del ***** 08/29/24 ********************************
     d*** end add ***** 02/25/19 ********************************
      *** Beg Add ***** 04/11/24 *** 129WXP ******************
     D ##seq#          S                    like(EcSeq#)
      *** Beg Del ***** 08/29/24 ********************************
     d*strConCrt       s            285a
      *** End Del ***** 08/29/24 ********************************
      *** End Add ***** 04/11/24 *** 129WXP ******************

     c     keyWMEML      Klist
     c                   Kfld                    ##co#
     c                   Kfld                    ##fnd
     c                   Kfld                    ##emp#
     c                   Kfld                    ##div

      /free

       // initialize detail box
       ClearDtlBox();

       // Main process
       Chain inRRN pec0p;
       GetEmpCnInfo();

       // set key values
       Exsr BuildData;
       outJSON = ' ';
       BuildJSON(outJSON :'OutDtlConNm'  : %trim(strname));
       //*** Beg Del ***** 08/29/24 **************************************
       //*BuildJSON(outJSON :'OutDtlCnAdr1' : %trim(strDtlCAd1));
       //*BuildJSON(outJSON :'OutDtlCnAdr2' : %trim(strDtlCAd2));
       //*BuildJSON(outJSON :'OutDtlCnCs'   : %trim(strDtlcsz1));
       //*BuildJSON(outJSON :'OutDtlCnPst'  : %trim(strpst));
       //*** End Del ***** 08/29/24 **************************************
       BuildJSON(outJSON :'OutDtlCnFor'  : %trim(strDtlCnfr));
       //*** Beg Del ***** 08/29/24 **************************************
       //*BuildJSON(outJSON :'OutDtlCnCPhn#': %trim(strDtlcell#));
       //*BuildJSON(outJSON :'OutDtlCnPhn#' : %trim(strDtlphn#));
       //*BuildJSON(outJSON :'OutDtlCnFax#' : %trim(strDtlfax#));
       //*** End Del ***** 08/29/24 **************************************
       BuildJSON(outJSON :'OutDtlSelfReportNotification' :
        %trim(strSrNoFlg));
       //*** Beg Del ***** 08/29/24 **************************************
       //*BuildJSON(outJSON :'OutDtlEmailAddress' : %trim(strDtlEmailAdd));
       //*** End Del ***** 08/29/24 **************************************
       BuildJSON(outJSON :'OutDtlDiv'    : %editc(##div:'X'));
       BuildJSON(outJSON :'OutDtlPAdr1'  : %trim(strDtlPAd1));
       BuildJSON(outJSON :'OutDtlPAdr2'  : %trim(strDtlPAd2));
       BuildJSON(outJSON :'OutDtlPCs'    : %trim(strDtlcsz2));
       BuildJSON(outJSON :'OutDtlMAdr1'  : %trim(strDtlMAd1));
       BuildJSON(outJSON :'OutDtlMAdr2'  : %trim(strDtlMAd2));
       BuildJSON(outJSON :'OutDtlMCs'    : %trim(strDtlcsz3));
       BuildJSON(outJSON :'OutDtlEnPhn#' : %trim(strDtlEphn#));
       BuildJSON(outJSON :'OutDtlEnFax#' : %trim(strDtlEfax#));
       //***** beg add ******* 04/17/15 *******************************
       BuildJSON(outJSON :'OutDtlPEF' : %trim(strDtlPEF));
       //***** end add ******* 04/17/15 *******************************
       //***** Beg Add ***** 04/11/24 *** 129WXP **********************
       //***** Beg Del ***** 08/29/24 *********************************
       //*BuildJSON(outJSON :'OutDtlDstTyp' : %trim(strConCrt));
       //***** End Del ***** 08/29/24 *********************************
       //***** End Add ***** 04/11/24 *** 129WXP **********************

       RtnCode = ' ';
       Return;
       //***********************************************************
        // BuildData for detail box
       //***********************************************************
       BegSr BuildData;
    
       //get the information as per the logic
       GetDivInfo();

      /end-free
     C                   EndSr
      //===============================================================
       // GetEmpCnInfo - Retrieve Employee contact information
      //===============================================================
     P GetEmpCnInfo    B
     d GetEmpCnInfo    PI
     c     keyWMEXL      Klist
     c                   Kfld                    ##co#
     c                   Kfld                    ##fnd

      /free

       ##co#  = ECco#;
       ##fnd  = ECfnd;
       ##emp# = ECemp#;
       ##div  = ECdiv;
      *** Beg Add ***** 04/11/24 *** 129WXP ******************
       ##seq# = ECSeq#;
      *** End Add ***** 04/11/24 *** 129WXP ******************

       Chain keyWMEXL pex00;
       clear strname;
       //*** Beg Chg ***** 08/29/24 **************************************
       //***If EClsn <> *blanks;
       //***   strname = %trim(EClsn) + ',' + %trim(ECfsn);
       //***Else;
       //***   strname = %trim(ECfsn);
       //***Endif;
       //***If ECmid <> *blanks;
       //***   strname = %trim (strname) + ',' + %trim(ECmid);
       //***Endif;
       strname = FormatName(EClsn:ECfsn:ECmid:'*FML');
       //*** End Chg ***** 08/29/24 **************************************

       // contact address
       //***** Beg Del ******* 08/29/24 **********************************
       //***FormatAddress(ECadr1:ECadr2:ECcty:ECst:ECzip:strDtlCAd1:strDtlCAd2:
       //*** strDtlcsz1);
       //***** End Del ******* 08/29/24 **********************************

       // contact for
       clear strDtlCnfr;
       If ECpcnt = '1';
          strDtlCnfr = EXcnt1;
       ElseIf ECpcnt = '2';
          strDtlCnfr = EXcnt2;
       ElseIf ECpcnt = '3';
          strDtlCnfr = EXcnt3;
       Endif;

           //*** Beg Add ***** 04/11/24 *** 129WXP ******************
           //*** Beg Del ***** 08/29/24 *****************************
           // Populate the Distribution Types
           //***Setll (##Co#:##Fnd:##Emp#:##Div:##Seq#) pec0p_cl;
           //***Reade (##Co#:##Fnd:##Emp#:##Div:##Seq#) pec0p_cl;
           //***strConCrt = ' ';
           //***Dow not %eof(wmeclcrt);
           //***   If CL_ECcritid = 'Distribution';
           //***      Chain (##Co#:CL_ECcritVal) STDsl;
           //***         If %Found;
           //***            If strConCrt <> ' ';
           //***               strConCrt = %Trim(strConCrt) + ', ' + DsDesc;
           //***            Else;
           //***               strConCrt = DsDesc;
           //***            Endif;
           //***         Endif;
           //***   Endif;
           //***   Reade (##Co#:##Fnd:##Emp#:##Div:##Seq#) pec0p_cl;
           //***Enddo;
           //*** End Del ***** 08/29/24 *****************************

           //*** End Add ***** 04/11/24 *** 129WXP ******************
       // position
       //***** Beg Del ******* 08/29/24 **********************************
       //*clear strpst;
       //*strpst = %trim(ECposn);
       //***** End Del ******* 08/29/24 **********************************

       //***** Beg Del ******* 08/29/24 **********************************
       //*If ECarea <> *zeros or ECphn# <> *zeros or ECext# <> *zeros;
       //*   strDtlphn# = '(' + %editc(ECarea:'X') + ')' +
       //*    %subst(%editc(ECphn#:'X'):1:3) + '-' +
       //*    %subst(%editc(ECphn#:'X'):4:4) + 'x' +
       //*    %trim(%editc(ECext#:'X'));
       //*Endif;

       // Cell phone#
       //*If ECcara <> *zeros or ECcphn <> *zeros;
       //*   strDtlcell# = '(' + %editc(ECcara:'X') + ')' +
       //*    %subst(%editc(ECcphn:'X'):1:3) + '-' +
       //*    %subst(%editc(ECcphn:'X'):4:4);
       //*Endif;

       //*If ECfara <> *zeros or ECfphn <> *zeros;
       //*   strDtlfax# = '(' + %editc(ECfara:'X') + ')' +
       //*    %subst(%editc(ECfphn:'X'):1:3) + '-' +
       //*    %subst(%editc(ECfphn:'X'):4:4);
       //*Endif;
       //***** End Del ******* 08/29/24 **********************************

       //***** beg add ******* 04/17/15 *******************************
       Select;
          When ECpef = 'P';
             strDtlPEF = 'Postal';
          When ECpef = 'E';
             strDtlPEF = 'Email ';
          When ECpef = 'F';
             strDtlPEF = 'Fax   ';
       Endsl;
       //***** end add ******* 04/17/15 *******************************

       //*** beg add ***** 02/25/19 *******************************
       //***** Beg Del ******* 08/29/24 **********************************
       //*strDtlEmailAdd = ECemal;
       //***** End Del ******* 08/29/24 **********************************
       If ECsrem = 'Y';
          strSrNoFlg = 'Yes';
       Else;
          strSrNoFlg = 'No';
       Endif;
       //*** end add ***** 02/25/19 *******************************

      /end-free
     P GetEmpCnInfo    E
       //==============================================================
       // GetDivInfo - Retrieve Division information
       //==============================================================
     P GetDivInfo      B
     D GetDivInfo      PI
     c     keyWMEXL      Klist
     c                   Kfld                    ##co#
     c                   Kfld                    ##fnd

      /free
       Chain keyWMEML pem00;
       Chain keyWMEXL pex00;

       // physical address
       FormatAddress(EMpad1:EMpad2:EMpcty:EMpst:EMpzip:strDtlPAd1:strDtlPAd2:
        strDtlcsz2);

       // mailing address
       FormatAddress(EMmad1:EMmad2:EMmcty:EMmst:EMmzip:strDtlMAd1:strDtlMAd2:
        strDtlcsz3);

       If EMarea <> *zeros or EMphn# <> *zeros;
          strDtlEphn# = '(' + %editc(EMarea:'X') + ')' +
           %subst(%editc(EMphn#:'X'):1:3) + '-' +
           %subst(%editc(EMphn#:'X'):4:4);
       Endif;
       If EMfaxa <> *zeros or EMfax# <> *zeros;
          strDtlEfax# = '(' + %editc(EMfaxa:'X') + ')' +
           %subst(%editc(EMfax#:'X'):1:3) + '-' +
           %subst(%editc(EMfax#:'X'):4:4);
       Endif;

      /end-Free

     P GetDivInfo      E
       //==============================================================
       // clearDtlBox - initialize detail box fields
       //==============================================================
     P clearDtlBox     B
     D clearDtlBox     PI

      /free

       clear strname;
       //***** Beg Del ******* 08/29/24 **********************************
       //*clear strpst;
       //*clear strDtlphn#;
       //***** End Del ******* 08/29/24 **********************************
       clear strDtlEphn#;
       //***** Beg Del ******* 08/29/24 **********************************
       //*clear strDtlcell#;
       //*clear strDtlfax#;
       //***** End Del ******* 08/29/24 **********************************
       clear strDtlEfax#;
       clear strDtlCnfr;
       clear strDtlPAd1;
       clear strDtlPAd2;
       clear strDtlMAd1;
       clear strDtlMAd2;
       //***** Beg Del ******* 08/29/24 **********************************
       //*clear strDtlCAd1;
       //*clear strDtlCAd2;
       //*clear strDtlcsz1;
       //***** End Del ******* 08/29/24 **********************************
       clear strDtlcsz2;
       clear strDtlcsz3;
       //***** beg add ******* 04/17/15 *******************************
       clear strDtlPEF;
       //***** end add ******* 04/17/15 *******************************
       //*** beg add ******* 02/25/19 *******************************
       clear strSrNoFlg;
       //***** Beg Del ******* 08/29/24 **********************************
       //*clear strDtlEmailAdd;
       //***** End Del ******* 08/29/24 **********************************
       //*** beg add ******* 02/25/19 *******************************

      /end-Free
     P clearDtlBox     E
