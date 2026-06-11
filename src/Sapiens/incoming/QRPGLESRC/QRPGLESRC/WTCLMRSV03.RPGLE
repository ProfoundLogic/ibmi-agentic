     h**********************************************************************
     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve Request for Payments detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTCLMRSV
     h*  Calls to:
     h*
     h*  Created on 05/22/2012 by C:279 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*  06/14/23 C:344 Project: WC2430
     h*                 Add Duplicate Comment field to Check Request Tab.
     h*
     h*  07/20/20 C:079 Project: WC2749
     h*                 E-Pay changes for headings, values, etc.
     h*
     h*  11/08/19 C:079 Support: 099/82052
     h*                 Change Managed Care Service programs to be API's
     h*
     h*  04/17/15 C:180 Support: 34266
     h*                 Fix incorrect EOB#
     h*
     h*  10/25/12 C:284 Project: 114WGD
     h*                 CSI Retrofit of Custom Code to Base. Invoice# inclusion
     h*
     h*
     h**********************************************************************
      * Customer Master File
     Fwarpp     if   e             Disk    rename(prp01:prp0p)
     Fwtgtl     if   e           k Disk    ReName(pgt01:pgt00)
     fwtabl     if   e           k Disk
     f                                     Ignore(prs01)
     f                                     Ignore(psy01)
     f                                     Ignore(pid01)
     f                                     ReName(prt01:prt00)
     f                                     ReName(pb201:pb200)
     f                                     ReName(pb701:pb700)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable

     d* Get Payee Information
     d GetPayee        pr

      * *ENTRY
     DGetAdjState      pr                  EXTPGM('FA787R')
     D inCo#                          3  0
     D inFnd                          3  0
     D inFyr                          3  0
     D inCase                         7  0
     D OutAdjState                    2
     DGetFaeopRrn      pr                  EXTPGM('FA788R')
     D inCo#                          3  0
     D inFnd                          3  0
     D inFyr                          3  0
     D inCase                         7  0
     D inEob#                         9  0
     D OutFaeopRrn                    9
     DClaimReserve03   pr                  ExtPgm('WTCLMRSV03')
     D                                9  0
     D                            32767
     D                                1
      *
     DClaimReserve03   pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1
      *
     D nf              c                   const('NOT FOUND')
     D numEdtCde       c                   const('N')
     D ##Key           s             10
     D ##TbId          s                   like(GTtbid)
     D intDtlDes       s                   Like(GTDesc)
     D intDtlDes2      s                   Like(GTDesc)
     D intDtlDes7      s                   Like(GTDesc)
     D intDtlRcD       s                   Like(GTDesc)
     D decDtlAmount    s              9  2
     D strKy           s             10
     D strCtrd         s             15
     D strDtlRmk       s             30
     D strDtlRmk1      s             30
     D strDtlRmk2      s             30
     D strDtlRmk3      s             30
     D strDtlBr2       s             30
     D strDtlAdr2      s             40
     D strDtlAdr3      s             40
     D strDtlBr7       s             30
     D strGroup        s              3
     D*** Beg Chg *** 07/20/20 *********************************************
     D*** strEftc         s              3
     D strReqType      s              5
     D*** End Chg *** 07/20/20 *********************************************

     D strEobLink      s            720
     D strEobTitle     s             50
     D strEobRrn       s              9
     D strAdjstate     s              2
     D strQueue        s             10
     D strMode         s              1
     D strPayType      s             40

     D                 ds
     D  intzip                 1      9  0
     D  intzip1                1      5  0
     D  intzip2                6      9  0
      *
      /Free

       Chain inRRN prp0p;
        // set key values

       outJSON = ' ';
       BuildJSON(outJSON :'OutDtlPayNam' : %Trim(RPpnam));
       BuildJSON(outJSON :'OutDtlPayAdr1': %Trim(RPadr1));
       Exsr BuildData;
       BuildJSON(outJSON :'OutDtlPayAdr2': strDtlAdr2);
       BuildJSON(outJSON :'OutDtlPayAdr3' : strDtlAdr3);
       BuildJSON(outJSON :'OutDtlRmk'    : strDtlRmk);
       BuildJSON(outJSON :'OutDtlRmk1'   : strDtlRmk1);
       BuildJSON(outJSON :'OutDtlRmk2'   : strDtlRmk2);
       BuildJSON(outJSON :'OutDtlRmk3'   : strDtlRmk3);
       BuildJSON(outJSON :'OutDtlMedDesc': intDtlDes);
       BuildJSON(outJSON :'OutDtlPV1Desc': intDtlDes2);
       BuildJSON(outJSON :'OutDtlPV2Desc': intDtlDes7);
       BuildJSON(outJSON :'OutDtlAmnt'   : %EditC(RPamt:numEdtcde));
       BuildJSON(outJSON :'OutDtlGroup'    : strGroup);
       BuildJSON(outJSON :'OutDtlRCd'    : intDtlRcD);
       BuildJSON(outJSON :'OutDtlPdt'    :%EditW(dtMDYY(RPtrdt):'  /  /    '));
       BuildJSON(outJSON :'OutDtlSfdt'   :%EditW(dtMDYY(RPfrdt):'  /  /    ') +
        ' - ' + %EditW(dtMDYY(RPtodt):'  /  /    '));
       BuildJSON(outJSON :'OutDtlRdt'    :%EditW(dtMDYY(RPrcvd):'  /  /    '));
       //*** Beg Chg *** 07/20/20 *************************************************
       //***BuildJSON(outJSON :'OutDtlEft'    : strEftc);
       BuildJSON(outJSON :'OutDtlRequestType' : strReqType);
       //*** End Chg *** 07/20/20 *************************************************
       BuildJSON(outJSON :'OutDtlStatus'    : RPsts);
       BuildJSON(outJSON :'OutInvoice#' :%Trim(RPinv#));
       //*** Begin Add ***  06/14/23  ****************************************
       BuildJSON(outJSON :'OutDtlDupComR' : %Trim(RPdupCom));
       //*** End   Add ***  06/14/23  ****************************************

       If RPeob#=0;
          strEobLink=' ';
          BuildJSON(outJSON :'LnkDtlEob#': %Editc(RPeob#:'Z'));
       Else;
          outJSON = %Trim(outJSON) + ', "LnkDtlEob#":';
          StartJSONelemnt(outJSON);                        // Start the element build
          BuildJSON(outJSON :'text' :%Editc(RPeob#:'Z'));  // Assign the value
          // Retrieve information to build the url
          strEobTitle = 'Print Eob#: ' + %Trim(%EditC(RPeob#:'4'));
          Callp GetFaeopRrn(RPco#:RPfnd:RPfYr:RPcase:RPeob#:strEobRrn);
          Callp GetAdjstate(RPco#:RPfnd:RPfYr:RPcase:strAdjState);
          strMode='D';
          strQueue='QPRINT';
          strEobLink = CreateURL('F0340C':strEobTitle:strEobRrn:strAdjState:
           strMode:strQueue);
          BuildJSON(outJSON :'url' :%Trim(strEobLink));    // Assign the URL
          EndJSONelemnt(outJSON);                          // End the element build
          BuildJSON(outJSON :'OutDtlRcn#'    : %EditW(RPeobs:'     0 '));
       EndIf;

       RtnCode = ' ';
       Return;
      /End-Free

      *************************************************************
      * BuildData for detail box
      *************************************************************
     C     BuildData     BegSr
     C*
     C     keyWTABL      klist
     C                   kfld                    RPco#
     C                   kfld                    strKy
    
      /FREE

       //get the Payee Address information as per the logic
       GetPayee();

       intDtlDes = RPdesc;

       //Load Reserve Code and Description
       strKy =  RPreCd ;
       intDtlRcd = *blanks;
       Chain  keyWTABL PRT00 ;
       If %found();
          intDtlRcd = %trim(RPreCd) + '  -  '+ %trim(RTdesc);
       Endif;

       //Load Pmt Val 1 and description
       strKy =  RPbr2 ;
       intDtlDes2 = *blanks;
       Chain  keyWTABL PB200 ;
       If %found();
          intDtlDes2 = %trim(RPbr2) + '  -  '+ %trim(B2desc);
       Endif;

       // Load Pmt Val 2 and description
       strKy =  RPbr7 ;
       intDtlDes7 = *blanks;
       Chain keyWTABL PB700;
       If %found();
          intDtlDes7 = %trim(RPbr7) + '  -  '+ %trim(B7desc);
       Endif;

       //*** Beg Chg *** 07/20/20 ******************************************
       //***If RPeftC='E';
       //***   strEftC='Yes';
       //***Else;
       //***   strEftC='No';
       //***EndIf;
       Select;
       When RPeftC = 'E';
          strReqType = 'EFT';
       When RPeftC = 'V';
          strReqType = 'E-Pay';
       Other;
          strReqType = 'Check';
       Endsl;
       //*** End Chg *** 07/20/20 ******************************************

       If RPpgTp='G';
          strGroup='Yes';
       Else;
          strGroup='No';
       EndIf;

      /End-Free
     C                   EndSr
      *================================================================
      *  GetPayee - Assign Payee Address
      *================================================================
     P GetPayee        B
     D GetPayee        PI
      *
     C     keyWTGTL      klist
     C                   Kfld                    RPco#
     C                   Kfld                    ##TbId
     C                   Kfld                    ##KEY

      /Free
       strDtlAdr2  = RPAdr2 ;
       intzip     = RPZip  ;

       //LOAD CITY, STATE AND ZIP

       If strDtlAdr2=*Blanks;
          strDtlAdr2 = %trimR(RPcty) + ', ' + RPst + ' ' +
           %editc(intZip1:'X') + '-'+ %editc(intZip2:'X');
          strDtlAdr3=*Blanks;
       Else;
          strDtlAdr3 = %trimR(RPcty) + ', ' + RPst + ' ' +
           %editc(intZip1:'X') + '-'+ %editc(intZip2:'X');
       EndIf;

       If RPctry <> *blanks and RPctry <> 'USA' ;
          ##TbId =  'CTRY';
          ##KEY  =  RPctry ;
          Chain   keyWTGTL  PGT00;

          If %found();
             strCtrd  = GTDESC ;
          Else;
             strCtrd  = NF;
          Endif;

          If    RPAdr2 =  *BLANKS ;
             strDtlAdr2 =   Rpadr3;
             strDtlAdr3 =  strCtrd;
          Else;
             strDtlAdr3 =  RPAdr3;
          Endif;
       Endif;

       strDtlRmk =  RPrmds;
       If RPrmk1 <> *blanks;
          strky  = ' '  ;
          strky  = RPrmk1;
          Chain keyWTABL PRC01;
          strDtlRmk1 =  RCDESC;
       Else;
          strDtlRmk1 =  *blanks;
       Endif;

       If RPrmk2 <> *blanks;
          strky  = ' '  ;
          strky  = RPrmk2;
          Chain keyWTABL PRC01;
          strDtlrmk2 =  RCdesc;
       Else;
          strDtlrmk2 =  *blanks;
       Endif;

       If RPrmk3 <> *blanks;
          strky  = ' '  ;
          strky  = RPrmk3;
          Chain keyWTABL PRC01;
          strDtlrmk3 =  RCdesc;
       Else;
          strDtlrmk3 =  *blanks;
       Endif;

      /end-Free


     P GetPayee        E
