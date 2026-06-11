     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hCopyright('(c) 2024 Sapiens. All Rights Reserved.')
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
     h*  Created on 09/25/2012 by C:279 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h* 08/29/24 C:333 Project: WC2826 (117276/116980)
     h*                Claims Contact.
     h*
     h* 04/11/24 C:340 Project: 129WXP
     h*                Add Distribution Types before Status on Grid
     h*
     h*
     h**********************************************************************
      * file
     fwma5p     if   e             Disk    reName(pac01:pac0p)
     fwtayl1    if   e           k disk    rename(pay01:pay01)
      *** Beg Add ***** 04/11/24 *** 129WXP ******************
     fwma5pcrt  if a e           k disk    prefix(PR_)
     f
     fSTDsl     if   e           k disk
      *** End Add ***** 04/11/24 *** 129WXP ******************

      /copy sPrDate
      /copy sPrString
      /copy sPrAtrium
      /copy wPrUsrPrms
      /copy wPrTable

     d* Get Entity/Location Information
     d GetEnLocInfo    pr

     d* Get Agency Contact Information
     d GetAgnCnInfo    pr

     d* Get Division Information
     d GetDivInfo      pr

     d* Clear Detail box
     d ClearDtlBox     pr

      * *Entry
     dPolicyContact02  pr                  ExtPgm('WTPOLCON02')
     d                                9  0
     d                            32767
     d                                1

     dPolicyContact02  pi
     d inRRN                          9  0
     d outJSON                    32767
     d RtnCode                        1

     d ##co#           s                   like(A5co#)
      *** Beg Del ***** 08/29/24 **************************************
     d*strname         s             50    inz(*blanks)
      *** End Del ***** 08/29/24 **************************************
     d strpst          s             40    inz(*blanks)
      *** Beg Del ***** 08/29/24 **************************************
     d*strDtlphn#      s             30    inz(*blanks)
     d*strDtlcell#     s             30    inz(*blanks)
     d*strDtlfax#      s             30    inz(*blanks)
     d*strDtlCAd1      s             40    inz(*blanks)
     d*strDtlCAd2      s             40    inz(*blanks)
     d*strDtlcsz1      s             40    inz(*blanks)
     d*strConTyp       s             40    inz(*blanks)
      *** End Del ***** 08/29/24 **************************************
     d strConDsg       s              5    inz(*blanks)
      *** Beg Add ***** 04/11/24 *** 129WXP ******************
     d strConCrt       s            285    inz(*blanks)
      *** End Add ***** 04/11/24 *** 129WXP ******************

      /free

       // initialize detail box
       ClearDtlBox();

       // Main process
       Chain inRRN pac0p;
       ##co#  = A5co#;
       GetAgnCnInfo();

       // set key values
       Exsr BuildData;
       outJSON = ' ';
       //*** Beg Del ***** 08/29/24 **************************************
       //*BuildJSON(outJSON :'OutDtlConNam' : %trim(strname));
       //*BuildJSON(outJSON :'OutDtlCnAd1'  : %trim(strDtlCAd1));
       //*BuildJSON(outJSON :'OutDtlCnAd2'  : %trim(strDtlCAd2));
       //*BuildJSON(outJSON :'OutDtlCnCsz'  : %trim(strDtlcsz1));
       //*** End Del ***** 08/29/24 **************************************
       BuildJSON(outJSON :'OutDtlCnPost' : %trim(strpst));
       BuildJSON(outJSON :'OutDtlConTl'  : %trim(A5titl));
       //*** Beg Del ***** 08/29/24 **************************************
       //*BuildJSON(outJSON :'OutDtlCnCPhn' : %trim(strDtlcell#));
       //*BuildJSON(outJSON :'OutDtlCnPhn'  : %trim(strDtlphn#));
       //*BuildJSON(outJSON :'OutDtlCnFax'  : %trim(strDtlfax#));
       //*BuildJSON(outJSON :'OutDtlCnTyp'  : %trim(strConTyp));
       //*** End Del ***** 08/29/24 **************************************
       BuildJSON(outJSON :'OutDtlCnDsg'  : %trim(strConDsg));
       //*** Beg Add ***** 04/11/24 *** 129WXP ******************
       BuildJSON(outJSON :'OutDtlADstTyp': %trim(strConCrt));
       //*** End Add ***** 04/11/24 *** 129WXP ******************

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
       // GetAgnCnInfo - Retrieve Agency contact information
      //===============================================================
     P GetAgnCnInfo    B
     d GetAgnCnInfo    PI
     c     keyWTAYL1     klist
     c                   kfld                    ##co#
     c                   kfld                    A5acty

      /free

       //*** Beg Del ***** 08/29/24 **************************************
       //*clear strname;
       //***If A5lsn <> *blanks;
       //***   strname = %trim(A5lsn)+',' + %trim(A5fsn);
       //***Else;
       //***   strname = %trim(A5fsn);
       //***Endif;
       //***If A5mid <> *blanks;
       //***   strname = %trim (strname) + ',' + %trim(A5mid);
       //***Endif;

       // contact address
       //*FormatAddress(A5adr1:A5adr2:A5cty:A5st:A5zip:strDtlCAd1:strDtlCAd2:
       //* strDtlcsz1);
       //*** End Del ***** 08/29/24 **************************************

       // position/responsibility
       clear strpst;
       strpst = %trim(A5posn);

       //*** Beg Add ***** 04/11/24 *** 129WXP ******************
       // Populate the Distribution Types
       Setll (A5co#:A5Fein:A5Mod:A5seq#) wma5pcrt;
       Reade(n) (A5co#:A5Fein:A5Mod:A5seq#) wma5pcrt;
       strConCrt = ' ';
       Dow not %eof(wma5pcrt);
          If PR_A5critID = 'Distribution';
             Chain (A5co#:PR_A5critVal) STDsl;
                If %Found;
                   If strConCrt <> ' ';
                      strConCrt = %Trim(strConCrt) + ', ' + DsDesc;
                   Else;
                      strConCrt = DsDesc;
                   Endif;
                Endif;
          Endif;
              Reade(n) (A5co#:A5Fein:A5Mod:A5seq#) wma5pcrt;
       Enddo;

       //*** End Add ***** 04/11/24 *** 129WXP ******************

       //*** Beg Del ***** 08/29/24 **************************************
       //*If A5area <> *zeros or A5phn# <> *zeros or A5ext# <> *zeros;
       //*   strDtlphn# = '(' + %editc(A5area:'X') + ')' +
       //*    %subst(%editc(A5phn#:'X'):1:3) + '-' +
       //*    %subst(%editc(A5phn#:'X'):4:4) + 'x' +
       //*    %trim(%editc(A5ext#:'X'));
       //*Endif;

       // Cell phone#
       //*If A5cara <> *zeros or A5cphn <> *zeros;
       //*   strDtlcell# = '(' + %editc(A5cara:'X') + ')' +
       //*    %subst(%editc(A5cphn:'X'):1:3) + '-' +
       //*    %subst(%editc(A5cphn:'X'):4:4);
       //*Endif;

       //*If A5fara <> *zeros or A5fphn <> *zeros;
       //*   strDtlfax# = '(' + %editc(A5fara:'X') + ')' +
       //*    %subst(%editc(A5fphn:'X'):1:3) + '-' +
       //*    %subst(%editc(A5fphn:'X'):4:4);
       //*Endif;

       // Type
       //*Chain keyWTAYL1 pay01;
       //*strConTyp = %trim(AYdesc);
       //*** End Del ***** 08/29/24 **************************************

       // Primary phone
       If A5pphf = 'P';
          strConDsg = 'Phone';
       ElseIf A5pphf = 'C';
          strConDsg = 'Cell';
       Endif;

      /end-free

     P GetAgnCnInfo    E
       //==============================================================
       // clearDtlBox - initialize detail box fields
       //==============================================================
     P clearDtlBox     B
     D clearDtlBox     PI

      /free

       //*** Beg Del ***** 08/29/24 **************************************
       //*clear strname;
       //*** End Del ***** 08/29/24 **************************************
       clear strpst;
       //*** Beg Del ***** 08/29/24 **************************************
       //*clear strDtlphn#;
       //*clear strDtlcell#;
       //*clear strDtlfax#;
       //*clear strDtlCAd1;
       //*clear strDtlCAd2;
       //*clear strDtlcsz1;
       //*clear strConTyp;
       //*** End Del ***** 08/29/24 **************************************
       clear strConDsg;

      /end-Free
     P clearDtlBox     E
