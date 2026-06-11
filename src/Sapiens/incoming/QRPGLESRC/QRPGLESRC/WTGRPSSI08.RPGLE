     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: API - AJAX program to populate the detail box
     h*               for Endorsements Controls Information
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTGRPSSI
     h*  Calls to:
     h*
     h*  Created on 03/22/2021 by C:318 for Project WC2196
     h**********************************************************************
     h* Revisions:
     h*                                                               *
     h* 02/19/24 C:333  Project: WC2942                               *
     h*                 HI Endorsement WC520602 Schedule Data         *
     h*                                                               *
     h* 02/14/24 C:333  Project: WC2957                               *
     h*                 Suppress WC99* Forms for WCPOLS by State      *
     h*                                                               *
     h* 11/13/23 C:340  Project: WC2929                               *
     h*                 Assigned Stat Code for ME(WC180607*)          *
     h*                                                               *
     h* 11/17/22 C:340  Project: WC2886                               *
     h*                 Add AZ Endorsements ? WC020603A, WC020604     *
     h*                                                               *
     h*
     h**********************************************************************
     fwmnxp     if   e             disk    rename(pnx01:pnx0s)

     d/copy sPrDate
     d/copy sPrAtrium

      * *ENTRY
     dwtGrpSSI08       pr                  extpgm('WTGRPSSI08')
     d                                9  0
     d                            32767
     d                                1

     dwtGrpSSI08       pi
     d inRRN                          9  0
     d outJSON                    32767
     d rtnCode                        1

     d strPhnNum       s             14


      /Free

       Chain inRRN pnx0s;

       Exsr BuildData;

       outJSON = ' ';

       // Populate Detail Box information.
       BuildJSON(outJSON :'OutDtlStateEDC':%trim(NXst));
       BuildJSON(outJSON :'OutDtlEffDateEDC' :
                         alphaUSA(cvtToDate(dtMDYY(NXefdt))));
       BuildJSON(outJSON :'CbxDtlPendRateChg':%trim(NXprcf));
       BuildJSON(outJSON :'OutDtlPendRateChgDate':
                         alphaUSA(cvtToDate(dtMDYY(NXprcd))));
       BuildJSON(outJSON :'CbxDtlRateChange':%trim(NXrcef));
       BuildJSON(outJSON :'OutDtlRateChgDate':
                         alphaUSA(cvtToDate(dtMDYY(NXrced))));
       BuildJSON(outJSON :'OutDtlStateCovPctChg':%editc(NXcpct:'N'));
       BuildJSON(outJSON :'OutDtlLong&HarbWorkCovPct':%editc(NXlpct:'N'));
       BuildJSON(outJSON :'CbxDtlSupressPolNotes':%trim(NXspln));
       BuildJSON(outJSON :'CbxDtlReqOwnTitle':%trim(NXofft));
       BuildJSON(outJSON :'OutDtlSalLimitRangeLower':%editc(NXllmt:'N'));
       BuildJSON(outJSON :'OutDtlSalLimitRangeHigher':%editc(NXhlmt:'N'));
       BuildJSON(outJSON :'OutDtlStateFormContDep':%trim(NXdept));
       BuildJSON(outJSON :'CbxDtlConLossAdjExpAsAdv':%trim(NXdedf));
       BuildJSON(outJSON :'OutDtlStateFormContPhn#':%trim(strPhnNum));
       //***** Beg Add *** 11/17/22 **********************************
       BuildJSON(outJSON :'OutDtlInsurerCxNoticeDays':%editc(NXinsrCxDy:'N'));
       BuildJSON(outJSON :'OutDtlInsuredCxNoticeDays':%editc(NXinsdCxDy:'N'));
       //***** End Add *** 11/17/22 **********************************
       //***** Beg Add *** 11/13/23 *** WC2929 ***********************
       BuildJSON(outJSON :'OutDtlStatCodeForME':%editc(NXStatCdMe:'N'));
       //***** End Add *** 11/13/23 *** WC2929 ***********************
       //***** Beg Add *** 02/14/24 **********************************
       BuildJSON(outJSON :'CbxDtlExcWC99*':%trim(NXexcl99));
       //***** End Add *** 02/14/24 **********************************
       //***** Beg Add *** 02/19/24 **********************************
       BuildJSON(outJSON :'OutDtlMedCareEtcPercentA':%editc(NXMCareA:'N'));
       BuildJSON(outJSON :'OutDtlMedCareEtcPercentB':%editc(NXMCareB:'N'));
       BuildJSON(outJSON :'OutDtlWageLossPercentA':%editc(NXWgLossA:'N'));
       BuildJSON(outJSON :'OutDtlWageLossPercentB':%editc(NXWgLossB:'N'));
       BuildJSON(outJSON :'OutDtlIndemBenPercentA':%editc(NXIBenDisA:'N'));
       BuildJSON(outJSON :'OutDtlIndemBenPercentB':%editc(NXIBenDisB:'N'));
       BuildJSON(outJSON :'OutDtlDeathBenPercentA':%editc(NXDBenA:'N'));
       BuildJSON(outJSON :'OutDtlDeathBenPercentB':%editc(NXDBenB:'N'));
       BuildJSON(outJSON :'OutDtlAdminLegalPercentA':%editc(NXAdmLglA:'N'));
       BuildJSON(outJSON :'OutDtlAdminLegalPercentB':%editc(NXAdmLglB:'N'));
       BuildJSON(outJSON :'OutDtlMiscellaneousPercentA':%editc(NXMiscA:'N'));
       BuildJSON(outJSON :'OutDtlMiscellaneousPercentB':%editc(NXMiscB:'N'));
       //***** End Add *** 02/19/24 **********************************

       rtnCode = ' ';
       Return;

       //===================================================================
       // Buld Data That will be passed on to AJAX
       //===================================================================
       Begsr BuildData;

        // Phone
        If NXdpar <> *zeros;
           strPhnNum = '(' + %Subst(%EditC(NXdpar:'X'):1:3) + ')';
        EndIf;
        If NXdppn <> *zeros;
           strPhnNum = %trim(strPhnNum) + ' ' + %Subst(%EditC(NXdppn:'X'):1:3) +
           '-' + %Subst(%EditC(NXdppn:'X'):4:4);
        EndIf;

       endsr;
      /End-Free
