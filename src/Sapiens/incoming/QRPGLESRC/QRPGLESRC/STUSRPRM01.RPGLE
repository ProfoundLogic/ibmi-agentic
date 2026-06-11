     h**********************************************************************
     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
      **********************************************************************
      * Revisions:
      *
      * 01/25/22  C:079 Project 133WSP
      *                 Add check box for Staff Assignments security
      *
      * 10/24/18  C:180 Project 129WVH
      *                 Add check box for Contacts security.
      *
      **********************************************************************
     FSSYLF     IF   E           K disk
     FSMUPP     if   e             disk    rename(PUP01:sup0p)
     FWMUPL     UF A E           K disk    usropn
     F                                     rename(pup01:wup00)
     FSMSLL2    UF A E           K disk    usropn
     F                                     rename(psl01:ssl02)

      * Entry
     dGetUserDetail    pr                  ExtPgm('STUSRPRM01')
     d                                9  0
     d                            32767
     d                                1
     dGetUserDetail    pi
     d inRrn                          9  0
     d outJSON                    32767
     d RtnCode                        1

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrString

     DChkRestriction   pr             1n
     d inType                         1    const

     dFilesClose       pr
     dFilesOpen        pr

     d string          s            500
     d isDARTS         s              1n
     d isOV            s              1n
     d isWCMC          s              1n
     d isWCMS          s              1n

      /free
       outJSON  = ' ';

       // Check modules
       Chain(n) inRRN sup0p;
       If not %found;
          BuildJSON(outJSON:'RtnCode':'Record no longer exists.');
          RtnCode = ' ';
          Return;
       Endif;
       Exsr Initialize;
       Exsr ProcessSYSM;
       If isWCMC;
          Exsr ProcessWCMC;
       Endif;
       If isDARTS;
          Exsr ProcessDARTS;
       Endif;
       If isWCMS;
          Exsr ProcessWCMS;
       Endif;

       FilesClose();
       RtnCode = ' ';
       Return;
       //==================================================================
       Begsr Initialize;

        Chain 'SY' ssy01;
        isWCMS = *off;
        If SYwcIf = 'Y';
           isWCMS = *on;
        Endif;
        isWCMC = *off;
        If SYfbIf = 'Y';
           isWCMC = *on;
        Endif;
        isDARTS = *off;
        If SYscIf = 'Y';
           isDARTS = *on;
        Endif;

        FilesOpen();

       Endsr;

       //==================================================================
       Begsr ProcessDARTS;
        // Write never executed, only to allow for clear of record format
        If 1=0;
           Write ssl02;
        Endif;
        Chain(n) UPusr ssl02;
        If not %found;
           Clear ssl02;
        Endif;
        BuildJSON(outJSON:'CbxRestrictDARTSDtl':SLrAcc);
        BuildJSON(outJSON:'CbxAllowEmailDtl':SLalwR);
        Select;
        When SLvDaD = 'D';
           string = 'YY';
        When SLvDaD = 'Y';
           string = 'YN';
        Other;
           Clear string;
        Endsl;
        BuildJSON(outJSON:'CbxAccessVisualDARTSDtl' :%subst(string:1:1));
        BuildJSON(outJSON:'CbxDefaultVisualDARTSDtl':%subst(string:2:1));
        BuildJSON(outJSON:'CbxAuthOV400Dtl'              :SLf01);
        BuildJSON(outJSON:'CbxDARTFlag02Dtl'             :SLf02);
        BuildJSON(outJSON:'CbxAuthExitDtl'               :SLf03);
        BuildJSON(outJSON:'CbxDARTFlag04Dtl'             :SLf04);
        BuildJSON(outJSON:'CbxAuthDisplayDocDtl'         :SLf05);
        BuildJSON(outJSON:'CbxAuthDocSearchDtl'          :SLf06);
        BuildJSON(outJSON:'CbxAuthChangeDefaultFolderDtl':SLf07);
        BuildJSON(outJSON:'CbxAuthImageScanDtl'          :SLf08);
        BuildJSON(outJSON:'CbxAuthUserSearchDtl'         :SLf09);
        BuildJSON(outJSON:'CbxAuthTableFileSearchDtl'    :SLf10);
        BuildJSON(outJSON:'CbxAuthAddRemarksDtl'         :SLf11);
        BuildJSON(outJSON:'CbxAuthCancelPreviousDtl'     :SLf12);
        BuildJSON(outJSON:'CbxAuthCustomFieldsDtl'       :SLf13);
        BuildJSON(outJSON:'CbxAuthSummCodeSeqDtl'        :SLf14);
        BuildJSON(outJSON:'CbxAuthDateSequenceDtl'       :SLf15);
        BuildJSON(outJSON:'CbxAuthTypeSequenceDtl'       :SLf16);
        BuildJSON(outJSON:'CbxAuthDocIDSeqDtl'           :SLf17);
        BuildJSON(outJSON:'CbxAuthSequenceEnteredDtl'    :SLf18);
        BuildJSON(outJSON:'CbxAuthPrintRemarksDtl'       :SLf19);
        BuildJSON(outJSON:'CbxAuthAllowCopyTransferDtl'  :SLf20);
        BuildJSON(outJSON:'CbxDARTFlag21Dtl'             :SLf21);
        BuildJSON(outJSON:'CbxDARTFlag22Dtl'             :SLf22);
        BuildJSON(outJSON:'CbxAuthSecureRemarksLevelDtl' :SLf23);
        BuildJSON(outJSON:'CbxAuthMoreKeysDtl'           :SLf24);
        BuildJSON(outJSON:'OutUserRemarkPrefix':SLcode);
        BuildJSON(outJSON:'OutUserRemarkType':SLrmCd);
        BuildJSON(outJSON:'OutUserCustomID':SLcsId);
        BuildJSON(outJSON:'OutUserFolderDefault':SLfold);
        BuildJSON(outJSON:'CbxRemarkAllowChangeDtl':SLcCtF);
        BuildJSON(outJSON:'CbxRemarkDeleteDtl':SLdelC);
        BuildJSON(outJSON:'CbxRemarkTypeChangeDtl':SLcCiF);
        BuildJSON(outJSON:'CbxOfficeDocAllowDisplayDtl':SLdDc);
        BuildJSON(outJSON:'CbxOfficeDocAllowEditDtl':SLeDc);
        BuildJSON(outJSON:'CbxOfficeDocAllowAddDtl':SLaDc);
        BuildJSON(outJSON:'TxtUserEmailDARTS':SLeMal);
        BuildJSON(outJSON:'TxtUserDescriptionDARTS':SLuDsc);
        BuildJSON(outJSON:'TxtSecurityLevelDARTS':%editc(SLsecL:'Z'));
       Endsr;

       //==================================================================
       Begsr ProcessSYSM;
        BuildJSON(outJSON:'OutUserDtl':%trim(UPusr));
        BuildJSON(outJSON:'OutUserNameDtl':%trim(UPusNm));
        BuildJSON(outJSON:'OutUserStaffCodeDtl':%trim(UPstaf));
        BuildJSON(outJSON:'OutUserInitialMenuDtl':%trim(UPiPgm));
        BuildJSON(outJSON:'OutUserTitleDtl':%trim(UPtitl));
        BuildJSON(outJSON:'OutUserDepartmentDtl':%trim(UPdept));
        BuildJSON(outJSON:'OutUserOutQDtl':%trim(UPoutQ));
        BuildJSON(outJSON:'CbxCompanyRestriction':%trim(UPxCo#));
        BuildJSON(outJSON:'OutUserCo#Dtl':%editc(UPco#:'Z'));
        BuildJSON(outJSON:'OutUserMaxRecentItemsDtl':%editc(UPmxRi:'Z'));
        BuildJSON(outJSON:'OutUserSecurityLevelDtl':%editc(UPlvl:'Z'));
        BuildJSON(outJSON:'OutUserSecurityTypeDtl':%trim(UPtype));
        string = '(' + %editc(UPphnA:'X') + ')' + %editw(UPphn#:'0   -    ');
        BuildJSON(outJSON:'OutUserPhone#Dtl':%trim(string));
        BuildJSON(outJSON:'CbxPhoneTextEnabledDtl':UPphnTxt);
        Select;
        When UPalt# > 9999999;
           string = %editw(UPalt#:'0(   )&   -    ');
        When UPalt# > 9999;
           string = %editw(UPalt#:'   0   -    ');
        Other;
           string = %editc(UPalt#:'Z');
        Endsl;
        string = RightJstfy(%trim(string):15);
        BuildJSON(outJSON:'OutUserAlt#Dtl':%trimr(string));
        BuildJSON(outJSON:'CbxAlternateTextEnabledDtl':UPaltTxt);
        BuildJSON(outJSON:'OutUserPhoneExtDtl':%editc(UPext#:'Z'));
        BuildJSON(outJSON:'OutUserNetworkPrinterDtl':%trim(UPpKey));
        string = AlphaUSA(CvtToDate(UPprDt));
        BuildJSON(outJSON:'OutUserProcessDateDtl':%trim(string));
        BuildJSON(outJSON:'OutUserEmailDtl':%trim(UPmail));
        BuildJSON(outJSON:'OutUserWordProcessorDtl':%trim(UPal4));
        BuildJSON(outJSON:'CbxUpdateContacts':UPcontUpd);
        //*** Beg Add *** 01/25/22 *********************************************
        BuildJSON(outJSON:'CbxStaffAssignmentDtl':UPstafUpd);
        //*** End Add *** 01/25/22 *********************************************
       Endsr;

       //==================================================================
       Begsr ProcessWCMC;
        BuildJSON(outJSON:'OutUserYearWCMC':%editc(UPfyr:'Z'));
        BuildJSON(outJSON:'OutUserSubscriberWCMC':%editc(UPsub#:'Z'));
        BuildJSON(outJSON:'OutUserGroupWCMC':%editc(UPfnd:'Z'));
        BuildJSON(outJSON:'OutUserEmp#WCMC':%editc(UPemp#:'Z'));
        string = %editc(UPdiv:'Z');
        If UPfl5 = 'Y' and UPdiv = *zero;
           string = RightJstfy('0':%size(UPdiv));
        Endif;
        BuildJSON(outJSON:'OutUserDivWCMC':%trimr(string));
        BuildJSON(outJSON:'CbxSubscriberRestrictionWCMCDtl':UPmrSu);
        BuildJSON(outJSON:'CbxGroupRestrictionWCMCDtl':UPmrGr);
        BuildJSON(outJSON:'CbxEmployerRestrictionWCMCDtl':UPmrEm);
        BuildJSON(outJSON:'CbxAdditionalRestrictionsWCMC':ChkRestriction('M'));
       Endsr;

       //==================================================================
       Begsr ProcessWCMS;
        // Write never executed, only to allow for clear of record format
        If 1=0;
           Write wup00;
        Endif;
        Chain(n) UPusr wup00;
        If not %found;
           Clear wup00;
        Endif;
        Clear string;
        If UPfein <> *zero;
           string = %editc(UPfein:'X');
        Endif;
        BuildJSON(outJSON:'OutUserFEIN':string);
        BuildJSON(outJSON:'OutUserYearWCMS':%editc(UPfyr:'Z'));
        BuildJSON(outJSON:'OutUserGroupWCMS':%editc(UPfnd:'Z'));
        BuildJSON(outJSON:'OutUserEmp#WCMS':%editc(UPemp#:'Z'));
        string = %editc(UPdiv:'Z');
        If UPfl5 = 'Y' and UPdiv = *zero;
           string = RightJstfy('0':%size(UPdiv));
        Endif;
        BuildJSON(outJSON:'OutUserDivWCMS':%trimr(string));
        BuildJSON(outJSON:'CbxGroupRestrictionWCMSDtl':UPxFnd);
        BuildJSON(outJSON:'CbxEmployerRestrictionWCMSDtl':UPxEmp);
        BuildJSON(outJSON:'OutUserOfficeLocation':UPoff#);
        BuildJSON(outJSON:'CbxAdditionalRestrictionsWCMS':ChkRestriction('W'));
        BuildJSON(outJSON:'CbxAllowAdditionalClaimaint':UPfl2);
        BuildJSON(outJSON:'OutUserPaymentLimit':%editc(UPpAl:'J'));
        BuildJSON(outJSON:'OutUserReserveLimitMed':%editc(UPmRAl:'J'));
        BuildJSON(outJSON:'OutUserReserveLimitComp':%editc(UPcRAl:'J'));
        BuildJSON(outJSON:'OutUserReserveLimitExp':%editc(UPeRAl:'J'));
        BuildJSON(outJSON:'OutUserReserveLimitLegal':%editc(UPlRAl:'J'));
        BuildJSON(outJSON:'OutUserReserveLimitCum':%editc(UPrAl:'J'));
        string = UPrsv1 + '  ' + UPrsv2 + '  ' + UPrsv3 + '  ' + UPrsv4;
        BuildJSON(outJSON:'OutUserReserveLimitCumTypes':string);
        BuildJSON(outJSON:'CbxReserveBasedOnIncurred':UPtInc);
       Endsr;
      /end-free

       //===================================================================
       // Check for active restriction recordsp
       //===================================================================
     pChkRestriction   b
     DChkRestriction   pi             1n
     d inType                         1    const
     C                   CALL      'SA008R'                             99
     C                   PARM                    UPco#
     C                   PARM                    UPusr
     C                   PARM      inType        ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
      /Free
       Return ##rcds = *on;
      /End-free
     pChkRestriction   e

       //===================================================================
       // Close files after use
       //===================================================================
     p FilesClose      b
     d FilesClose      pi
      /Free
       If %open(WMUPL);
          Close WMUPL;
       Endif;
       If %open(SMSLL2);
          Close SMSLL2;
       Endif;
      /End-free
     p FilesClose      e

       //===================================================================
       // Open files for use
       //===================================================================
     p FilesOpen       b
     d FilesOpen       pi
      /Free
       If not %open(WMUPL) and isWCMS = *on;
          Open WMUPL;
       Endif;
       If not %open(SMSLL2) and isDARTS = *on;
          Open SMSLL2;
       Endif;
      /End-free
     p FilesOpen       e
