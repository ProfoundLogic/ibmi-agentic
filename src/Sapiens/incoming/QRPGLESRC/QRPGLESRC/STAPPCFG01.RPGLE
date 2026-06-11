     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Visit Tracking Configuration detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTAPPCFG
     h*  Calls to:
     h*
     h*  Created on 12/04/2014 by C:242 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h*  06/08/23 C:342 Project: WC2747                                    *
     h*                 Add C = Completion in Textbox on service request   *
     h*                 configuration.                                     *
     h*                                                                    *
      *  09/24/19 C:291 Project: 129WVN                                    *
      *                 Add "Allow duplicate entries" checkbox.            *
      *                                                                    *
     h**********************************************************************
      * Visit Type Master File
     Fwtvip     if   e             disk    rename(pvi01:pvi0p)
      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      * *ENTRY

     d* CheckBox Conversion Procedure
     d****** Beg Chg ***** 06/08/23 ********************************
     d****** ChkBoxCnv       pr             3A
     d ChkBoxCnv       pr            10A
     d****** End Chg ***** 06/08/23 ********************************
     d                                1    const

     DVisitTrackDet    pr                  ExtPgm('WTAPPCFG01')
     D                                9  0
     D                            32767
     D                                1
      *
     DVisitTrackDet    pi
     D inRRN                          9  0
     D outJSON                    32767
     D RtnCode                        1
      *
      /Free

       Chain(n) inRRN pvi0p;
       outJSON = ' ';
       BuildJSON(outJSON :'DtlVstType':%trim(VIType));
       BuildJSON(outJSON :'DtlVstCat' :%trim(VICat));
       //*** Beg Add ***** 09/24/19 ********************************
       BuildJSON(outJSON :'DtlVstAllowDupEnt' :ChkBoxCnv(VIDupEntry));
       //*** End Add ***** 09/24/19 ********************************
       Select;
          when VIStat = 'A';
             BuildJSON(outJSON :'DtlVstSts' :'Active');
          when VIStat = 'I';
             BuildJSON(outJSON :'DtlVstSts' :'Inactive');
          other;
             BuildJSON(outJSON :'DtlVstSts' :' ');
       endsl;

       Exsr BuildData;

       RtnCode = ' ';
       Return;

       //*************************************************************
       //* BuildData for detail box
       //*************************************************************
       begsr BuildData;

       BuildJSON(outJSON :'DtlDate01' :%trim(VIdtHd1));
       BuildJSON(outJSON :'DtlDteReq01' :ChkBoxCnv(VIdtRq1));
       BuildJSON(outJSON :'DtlDate02' :%trim(VIdtHd2));
       BuildJSON(outJSON :'DtlDteReq02' :ChkBoxCnv(VIdtRq2));
       BuildJSON(outJSON :'DtlDate03' :%trim(VIdtHd3));
       BuildJSON(outJSON :'DtlDteReq03' :ChkBoxCnv(VIdtRq3));
       BuildJSON(outJSON :'DtlDate04' :%trim(VIdtHd4));
       BuildJSON(outJSON :'DtlDteReq04' :ChkBoxCnv(VIdtRq4));
       BuildJSON(outJSON :'DtlDate05' :%trim(VIdtHd5));
       BuildJSON(outJSON :'DtlDteReq05' :ChkBoxCnv(VIdtRq5));
       BuildJSON(outJSON :'DtlDate06' :%trim(VIdtHd6));
       BuildJSON(outJSON :'DtlDteReq06' :ChkBoxCnv(VIdtRq6));
       BuildJSON(outJSON :'DtlDate07' :%trim(VIdtHd7));
       BuildJSON(outJSON :'DtlDteReq07' :ChkBoxCnv(VIdtRq7));
       BuildJSON(outJSON :'DtlDate08' :%trim(VIdtHd8));
       BuildJSON(outJSON :'DtlDteReq08' :ChkBoxCnv(VIdtRq8));
       BuildJSON(outJSON :'DtlDate09' :%trim(VIdtHd9));
       BuildJSON(outJSON :'DtlDteReq09' :ChkBoxCnv(VIdtRq9));
       BuildJSON(outJSON :'DtlDate10' :%trim(VIdtHd10));
       BuildJSON(outJSON :'DtlDteReq10' :ChkBoxCnv(VIdtRq10));

       BuildJSON(outJSON :'DtlChkBox01' :%trim(VIcbHd1));
       BuildJSON(outJSON :'DtlChkBox02' :%trim(VIcbHd2));
       BuildJSON(outJSON :'DtlChkBox03' :%trim(VIcbHd3));
       BuildJSON(outJSON :'DtlChkBox04' :%trim(VIcbHd4));
       BuildJSON(outJSON :'DtlChkBox05' :%trim(VIcbHd5));
       BuildJSON(outJSON :'DtlChkBox06' :%trim(VIcbHd6));
       BuildJSON(outJSON :'DtlChkBox07' :%trim(VIcbHd7));
       BuildJSON(outJSON :'DtlChkBox08' :%trim(VIcbHd8));
       BuildJSON(outJSON :'DtlChkBox09' :%trim(VIcbHd9));
       BuildJSON(outJSON :'DtlChkBox10' :%trim(VIcbHd10));

       BuildJSON(outJSON :'DtlItg01' :%trim(VIinHd1));
       BuildJSON(outJSON :'DtlItgReq01' :ChkBoxCnv(VIinRq1));
       BuildJSON(outJSON :'DtlItg02' :%trim(VIinHd2));
       BuildJSON(outJSON :'DtlItgReq02' :ChkBoxCnv(VIinRq2));
       BuildJSON(outJSON :'DtlItg03' :%trim(VIinHd3));
       BuildJSON(outJSON :'DtlItgReq03' :ChkBoxCnv(VIinRq3));
       BuildJSON(outJSON :'DtlItg04' :%trim(VIinHd4));
       BuildJSON(outJSON :'DtlItgReq04' :ChkBoxCnv(VIinRq4));
       BuildJSON(outJSON :'DtlItg05' :%trim(VIinHd5));
       BuildJSON(outJSON :'DtlItgReq05' :ChkBoxCnv(VIinRq5));
       BuildJSON(outJSON :'DtlItg06' :%trim(VIinHd6));
       BuildJSON(outJSON :'DtlItgReq06' :ChkBoxCnv(VIinRq6));
       BuildJSON(outJSON :'DtlItg07' :%trim(VIinHd7));
       BuildJSON(outJSON :'DtlItgReq07' :ChkBoxCnv(VIinRq7));
       BuildJSON(outJSON :'DtlItg08' :%trim(VIinHd8));
       BuildJSON(outJSON :'DtlItgReq08' :ChkBoxCnv(VIinRq8));
       BuildJSON(outJSON :'DtlItg09' :%trim(VIinHd9));
       BuildJSON(outJSON :'DtlItgReq09' :ChkBoxCnv(VIinRq9));
       BuildJSON(outJSON :'DtlItg10' :%trim(VIinHd10));
       BuildJSON(outJSON :'DtlItgReq10' :ChkBoxCnv(VIinRq10));

       BuildJSON(outJSON :'DtlDec01' :%trim(VIdcHd1));
       BuildJSON(outJSON :'DtlDecReq01' :ChkBoxCnv(VIdcRq1));
       BuildJSON(outJSON :'DtlDec02' :%trim(VIdcHd2));
       BuildJSON(outJSON :'DtlDecReq02' :ChkBoxCnv(VIdcRq2));
       BuildJSON(outJSON :'DtlDec03' :%trim(VIdcHd3));
       BuildJSON(outJSON :'DtlDecReq03' :ChkBoxCnv(VIdcRq3));
       BuildJSON(outJSON :'DtlDec04' :%trim(VIdcHd4));
       BuildJSON(outJSON :'DtlDecReq04' :ChkBoxCnv(VIdcRq4));
       BuildJSON(outJSON :'DtlDec05' :%trim(VIdcHd5));
       BuildJSON(outJSON :'DtlDecReq05' :ChkBoxCnv(VIdcRq5));
       BuildJSON(outJSON :'DtlDec06' :%trim(VIdcHd6));
       BuildJSON(outJSON :'DtlDecReq06' :ChkBoxCnv(VIdcRq6));
       BuildJSON(outJSON :'DtlDec07' :%trim(VIdcHd7));
       BuildJSON(outJSON :'DtlDecReq07' :ChkBoxCnv(VIdcRq7));
       BuildJSON(outJSON :'DtlDec08' :%trim(VIdcHd8));
       BuildJSON(outJSON :'DtlDecReq08' :ChkBoxCnv(VIdcRq8));
       BuildJSON(outJSON :'DtlDec09' :%trim(VIdcHd9));
       BuildJSON(outJSON :'DtlDecReq09' :ChkBoxCnv(VIdcRq9));
       BuildJSON(outJSON :'DtlDec10' :%trim(VIdcHd10));
       BuildJSON(outJSON :'DtlDecReq10' :ChkBoxCnv(VIdcRq10));

       BuildJSON(outJSON :'DtlText01' :%trim(VItfHd1));
       BuildJSON(outJSON :'DtlTextReq01' :ChkBoxCnv(VItfRq1));
       BuildJSON(outJSON :'DtlText02' :%trim(VItfHd2));
       BuildJSON(outJSON :'DtlTextReq02' :ChkBoxCnv(VItfRq2));
       BuildJSON(outJSON :'DtlText03' :%trim(VItfHd3));
       BuildJSON(outJSON :'DtlTextReq03' :ChkBoxCnv(VItfRq3));
       BuildJSON(outJSON :'DtlText04' :%trim(VItfHd4));
       BuildJSON(outJSON :'DtlTextReq04' :ChkBoxCnv(VItfRq4));
       BuildJSON(outJSON :'DtlText05' :%trim(VItfHd5));
       BuildJSON(outJSON :'DtlTextReq05' :ChkBoxCnv(VItfRq5));
       BuildJSON(outJSON :'DtlText06' :%trim(VItfHd6));
       BuildJSON(outJSON :'DtlTextReq06' :ChkBoxCnv(VItfRq6));
       BuildJSON(outJSON :'DtlTable06' :%trim(VItfTb6));
       BuildJSON(outJSON :'DtlText07' :%trim(VItfHd7));
       BuildJSON(outJSON :'DtlTextReq07' :ChkBoxCnv(VItfRq7));
       BuildJSON(outJSON :'DtlTable07' :%trim(VItfTb7));
       BuildJSON(outJSON :'DtlText08' :%trim(VItfHd8));
       BuildJSON(outJSON :'DtlTextReq08' :ChkBoxCnv(VItfRq8));
       BuildJSON(outJSON :'DtlTable08' :%trim(VItfTb8));
       BuildJSON(outJSON :'DtlText09' :%trim(VItfHd9));
       BuildJSON(outJSON :'DtlTextReq09' :ChkBoxCnv(VItfRq9));
       BuildJSON(outJSON :'DtlTable09' :%trim(VItfTb9));
       BuildJSON(outJSON :'DtlText10' :%trim(VItfHd10));
       BuildJSON(outJSON :'DtlTextReq10' :ChkBoxCnv(VItfRq10));
       BuildJSON(outJSON :'DtlTable10' :%trim(VItfTb10));

       endSr;
      /End-Free
       //===================================================================
       // ProcReqDel  - Process the pending check request deletion
       //===================================================================
     P ChkBoxCnv       B                   export
       //*** Beg Chg ***** 06/08/23 ********************************
     D****** ChkBoxCnv       PI             3A
     D ChkBoxCnv       PI            10A
       //*** Beg Chg ***** 06/08/23 ********************************
     D  @Value                        1A   const
      /Free
       Select;
          When @Value = 'Y';
             Return ('Yes');
          When @Value = 'N' or @Value = ' ';
             Return ('No ');
       //*** Beg Add ***** 06/08/23 ********************************
          When @Value = 'C';
             Return ('Completion');
       //*** Beg Add ***** 06/08/23 ********************************
          Other;
             Return ('N/A');
       endsl;
      /End-Free
     P ChkBoxCnv       E
