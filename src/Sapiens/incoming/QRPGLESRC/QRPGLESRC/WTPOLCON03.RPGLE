     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Agency Contact detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLCONT
     h*  Calls to:
     h*
     h*  Created on 08/23/2021 by C:298 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h* 08/29/24 C:333 Project: WC2826 (117276/116980)
     h*                Claims Contact.
     h*
     h*
     h**********************************************************************
      * file
     fwmapp     if   e             Disk    reName(pap01:pap0p)
     fwmagl     if   e           k disk    rename(pag01:pag00)

      /copy sPrDate
      /copy sPrString
      /copy sPrAtrium
      /copy wPrUsrPrms
      /copy wPrTable

     d* Get Entity/Location Information
     d GetEnLocInfo    pr

     d* Get Agency Contact Information
     d GetProCnInfo    pr

     d* Get Division Information
     d GetDivInfo      pr

     d* Clear Detail box
     d ClearDtlBox     pr

      * *Entry
     dPolicyContact03  pr                  ExtPgm('WTPOLCON03')
     d                                9  0
     d                            32767
     d                                1

     dPolicyContact03  pi
     d inRRN                          9  0
     d outJSON                    32767
     d RtnCode                        1

      *** Beg Del ***** 08/29/24 *****************************************
     d*strname         s             50    inz(*blanks)
      *** End Del ***** 08/29/24 *****************************************
     d strpst          s             40    inz(*blanks)
      *** Beg Del ***** 08/29/24 *****************************************
     d*strDtlphn#      s             30    inz(*blanks)
     d*strDtlcell#     s             30    inz(*blanks)
     d*strDtlfax#      s             30    inz(*blanks)
     d*strDtlCAd1      s             40    inz(*blanks)
     d*strDtlCAd2      s             40    inz(*blanks)
     d*strDtlcsz1      s             40    inz(*blanks)
     d*strConTyp       s             40    inz(*blanks)
      *** End Del ***** 08/29/24 *****************************************
     d strConDsg       s              5    inz(*blanks)

      /free

       // initialize detail box
       ClearDtlBox();

       // Main process
       Chain inRRN pap0p;
       GetProCnInfo();

       // set key values
       Exsr BuildData;
       outJSON = ' ';
       //*** Beg Del ***** 08/29/24 ***************************************
       //*BuildJSON(outJSON :'OutDtlConNam' : %trim(strname));
       //*BuildJSON(outJSON :'OutDtlCnAd1'  : %trim(strDtlCAd1));
       //*BuildJSON(outJSON :'OutDtlCnAd2'  : %trim(strDtlCAd2));
       //*BuildJSON(outJSON :'OutDtlCnCsz'  : %trim(strDtlcsz1));
       //*** End Del ***** 08/29/24 ***************************************
       BuildJSON(outJSON :'OutDtlCnPost' : %trim(strpst));
       BuildJSON(outJSON :'OutDtlConTl'  : %trim(' '));
       //*** Beg Del ***** 08/29/24 ***************************************
       //*BuildJSON(outJSON :'OutDtlCnCPhn' : %trim(strDtlcell#));
       //*BuildJSON(outJSON :'OutDtlCnPhn'  : %trim(strDtlphn#));
       //*BuildJSON(outJSON :'OutDtlCnFax'  : %trim(strDtlfax#));
       //*BuildJSON(outJSON :'OutDtlCnTyp'  : %trim(strConTyp));
       //*** End Del ***** 08/29/24 ***************************************
       BuildJSON(outJSON :'OutDtlCnDsg'  : %trim(strConDsg));

       RtnCode = ' ';
       Return;
       //***********************************************************
        // BuildData for detail box
       //***********************************************************
       BegSr BuildData;
    

      /end-free
     C                   EndSr
      //===============================================================
       // GetProCnInfo - Retrieve Producer contact information
      //===============================================================
     P GetProCnInfo    B
     d GetProCnInfo    PI

      /free

       //*** Beg Del ***** 08/29/24 ***************************************
       //*clear strname;
       //***If APlsn <> *blanks;
       //***   strname = %trim(APlsn)+',' + %trim(APfsn);
       //***Else;
       //***   strname = %trim(APfsn);
       //***Endif;

       // contact address
       //*FormatAddress(APmad1:strDtlCAd2:APmcty:APmst:APmzip:strDtlCAd1
       //* :strDtlCAd2:strDtlcsz1);
       //*** End Del ***** 08/29/24 **************************************

       // position/responsibility
       clear strpst;
       strpst = 'Producer';

       //*** Beg Del ***** 08/29/24 ***************************************
       //*Chain (APco#:APfein:APmod) pag00;
       //*If %Found(wmagl);
       //*   If AGphn1<>*Zeros;
       //*      strDtlphn# = '(' + %EditC(AGare1:'X') + ') ' +
       //*       %Subst(%EditC(AGphn1:'X'):1:3) + '-' +
       //*       %Subst(%EditC(AGphn1:'X'):4:4);
       //*   EndIf;
       //*EndIf;

       // Cell phone#
       //*strDtlcell# = *blanks;

       //*strDtlfax# = *blanks;

       // Type
       //*strConTyp = 'Producer';
       //*** End Del ***** 08/29/24 ***************************************

       // Primary phone
          strConDsg = 'Phone';

      /end-free

     P GetProCnInfo    E
       //==============================================================
       // clearDtlBox - initialize detail box fields
       //==============================================================
     P clearDtlBox     B
     D clearDtlBox     PI

      /free

       //*** Beg Del ***** 08/29/24 ***************************************
       //*clear strname;
       //*** End Del ***** 08/29/24 ***************************************
       clear strpst;
       //*** Beg Del ***** 08/29/24 ***************************************
       //*clear strDtlphn#;
       //*clear strDtlcell#;
       //*clear strDtlfax#;
       //*clear strDtlCAd1;
       //*clear strDtlCAd2;
       //*clear strDtlcsz1;
       //*clear strConTyp;
       //*** End Del ***** 08/29/24 ***************************************
       clear strConDsg;

      /end-Free
     P clearDtlBox     E
