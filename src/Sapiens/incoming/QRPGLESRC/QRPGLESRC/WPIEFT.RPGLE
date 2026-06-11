     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H nomain
     *========================================================================
     *
      *
     * WPIEFT - Procedures for Bank information
     *
     * Created   : 06/05/12
     * Programmer: C:279
     * Project   : WC2115
     *
     *========================================================================
     * REVISIONS:
      *
      * 06/23/23 C:340 Support: 110581
      *                Allow Expiration Date in the past If there is another valid
      *                record on the policy for that date and the one being
      *                expired has no transfer dates.
      *
      * 07/15/22 C:136 Project: SY436
      *                Fix edits on Claim EFT Add for date format and starting
      *                policy year.
      *
      * 12/07/21 C:079 Support: 93254/99
      *                Fix unmasking issue and move Emp logic under type = E
      *
      * 06/23/21 C:318 Project WC2302 - Auto tab closure for
      *                deletion.
      *
      * 05/19/21 C:180 Project: WC1986
      *                         Default starting policy year for Live Env.
      *
      *                         Add edits for Default Info check box.
      *
      *                         Remove "Employer" Account Type from display.
      *                         Add Policy year incoming parm to EFTADD.
      *
      *                         Add starting policy year, Env Flag, and
      *                         State Reporting Default fields to screen.
      *
      *                         Add environment parm to EFTADD
      *
      * 06/09/20 C:298 Project: SY420
      *                Mask SSN
      *
      * 12/02/16 C:180 Support: INHOUSE 44510
      *                Fix edit on Addtional Payee Drop Down.
      *
      * 10/20/15 C:062 Support: WC025 37589
      *                Add edit on Additional Payee Drop Down
      *
      * 11/04/13 C:136 Support: WA146 83976
      *                Disable Employer EFT edit per GW
      *
      * 04/02/13 C:136 Support: RED2560
      *                Correct Receiving DFI and Check Digit to alpha
     *
     *========================================================================
     f*
     fwpieft    cf   e             workstn extfile('WPIEFT')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f*  physical file definition (for maintaining record)
     fwmefp     uf a e             disk    rename(pef01:pef0p)
     f                                     infds(ef0fds)
      *** Begin Chg *** 06/23/21 *****************************************
     f*****wmcml1    if   e           k disk    rename(pcm01:pcm01)
     fwmcml1    if   e           k disk    rename(pcm01:pcm01)
     f                                     infds(cm0fds)
      *** End Chg *** 06/23/21 *****************************************
     f*  Claims physical file definition (for search)
     fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     fwdacp     if   e             disk    rename(pac01:pac0p)
     fwdacl     if   e           k disk    rename(pac01:pac00)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     fwmefl7    if   e           k disk    rename(pef01:pef07)
     f                                     infds(ef7fds) prefix(l7)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fwmaap     if   e             disk    rename(paa01:paa0p)
     Fwmaal1    if   e           k disk    rename(paa01:paa01)
     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwmpdp     if   e             disk    rename(ppd01:ppd0p)
      *** Begin Add *** 05/19/21 *****************************************
     fwdell     if   e           k disk
      *** End   Add *** 05/19/21 *****************************************
      *** Begin Add *** 06/23/21 *****************************************
     fwdell5    if   e           k disk    rename(pel01:pel02)
     f                                     infds(el0fds)
      *** End   Add *** 06/23/21 *****************************************
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d ef7fds          ds
     d   ef7rrn              397    400B 0
      *** Begin Add *** 05/19/21 **********************************************
     d ef0fds          ds
     d   ef0rrn              397    400B 0
      *** End  Add *** 05/19/21 **********************************************
      *** Begin Add *** 06/23/21 **********************************************
     d el0fds          ds
     d   el0rrn              397    400B 0
     d cm0fds          ds
     d   cm0rrn              397    400B 0
      *** End  Add *** 06/23/21 **********************************************
     D psds          esds                  ExtName(szq1p)
     d
     d ##co            s              3
     d ##fndc          s              3
     d ##co#           s              3  0
     d ##fnd           s              3  0
     d ##grp#          s                   like(scAddGrp)
     d ##typ           s              1
     d ##key           s                   like(EFkey)
     d ##pre           s               n
     d ##rtn           s              1
     d ##date          s              7
     d ##rrn           s              9  0
     d ##mod           s              1
     d ##rrn1          s              9  0
     d ##fnd#          s              3  0
     d ##emp#          s              9  0
     d ##div#          s              5  0
     d ##fyr#          s              3  0
     d ##agt#          s              5  0
     d ##anam#         s             60
     d ##prod#         s              5  0
     d ##pnam#         s             60
      *** Beg Add *** 05/19/21 ***********************************
     d ##Env           s              1
     d sysDate7        s              7  0 inz
     d strType         s              1    inz
     d intStRptDiv     s                   like(EFdiv) inz
     d intEMrrn        s              9  0 inz
     d strRpFlg        s              1    inz
     d ##Sfyr          s                   like(EFfyr) inz
      *** End Add *** 05/19/21 ***********************************
     d kkco#           s                   like(EFco#)
     d kkfnd           s                   like(EFfnd)
     d kkfein          s                   like(EFfein)
     d kkfyr           s                   like(EFfyr)
     d kkmod           s                   like(EFmod)
     d kkcase          s                   like(EFcase)
     d kkseq#          s                   like(EFseq#)
     d kkefdt          s                   like(EFefdt)
     d kkname          s                   like(EFpnam)
     d kkemp#          s                   like(EFemp#)
     d kkdiv           s                   like(EFdiv)
     d int1940         s              8  0 Inz(19400101)
     d intSeq          s              5  0
     d intdate         s              8  0
     d intExpDt        s              8  0
     d intPreDt        s              8  0
     d intAprDt        s              8  0
     d intdat          s              8  0
     d intdat7         s              7  0
     d isValid         s               n
     d svAddAcTyp      s                   like(scAddAcTyp)
       //*** Begin Add *** 05/19/21 **************************************************
       Dcl-c strMsg1 Const('Default EFT information to the non-state reporting -
       divisions');
       Dcl-c strMsg2 Const('Default EFT information from the state reporting -
       division');
       Dcl-s strKey  char(30);
       Dcl-s isEftSrd ind inz(*Off);
       Dcl-s strEfDt char(7) inz;
       Dcl-s strpre char(1) inz;
       Dcl-s strRrn char(9) inz('000000000');
       Dcl-s strEobType char(3) inz;
       Dcl-s strCkTp char(10);
       Dcl-s strSfyr char(3);
       Dcl-s chrEnv char(1);
       Dcl-s strDftSrD char(1) inz;
       Dcl-Ds KeyWdell likerec(pel01:*key);       //Key Data structure
       Dcl-S intInceptDt packed(7:0) inz;         //global
       //*** End   Add *** 05/19/21 **************************************************
       //*** Beg Add *** 06/23/23 *** 110581 *******************************
       Dcl-S MarkExpired ind inz(*Off);
       //*** End Add *** 06/23/23 *** 110581 *******************************
    D*---------------------------------------------------------------------
    d* Stand Alone Fields - BOTTOM
    d*---------------------------------------------------------------------
     * Prototypes
    d*---------------------------------------------------------------------

     d* Initialize field and indicators
     d Initialize      pr

     d* Get payee type
     d GetPayTyp       pr
     d ##typ                               like(##typ)

     d* Lod Add screen
     d LodAddEft       pr
     d ##typ                          1
     d ##rrn1                         9  0

     d* Lod edit screen
     d LodEdtEft       pr

     d* Lod EFT bank info screen
     d LodEFTBank      pr

     d* Validate popup screen for Add/Edit
     d Validate        pr

     d* Validate EFT bank info screen
     d ValidEftBank    pr

     d* process pop up screen for add/edit
     d procAdd         pr
     d ##rrn1                         9  0

     d* process EFT bank info screen
     d procEFTBank     pr
     d ##rrn1                         9  0

     d* Save changes for add/edit screen
     d savChanges      pr
     d ##rrn1                         9  0

     d* Save changes for EFT bank info screen
     d savEftBank      pr
     d ##rrn1                         9  0

     d*  GET Agent/Producer Information
     d GetAgnPrInfo    pr                  ExtPgm('WA002R')
     d ##co#                               like(##co#)
     d ##fnd#                              like(##fnd#)
     d ##emp#                              like(##emp#)
     d ##div#                              like(##div#)
     d ##fyr#                              like(##fyr#)
     d ##agt#                              like(##agt#)
     d ##anam#                             like(##anam#)
     d ##prod#                             like(##prod#)
     d ##pnam#                             like(##pnam#)

     d WrtHldFile      pr                  ExtPgm('WA073R')
     d ##typ                               like(##typ)
     d ##rrn1                              like(##rrn1)
     d ##rrn                               like(##rrn)
     d ##rtn                               like(##rtn)

     d/copy wPrEft
     d/copy wPrBank
     d/copy sprString
     d/copy sprDate
     d/copy sPrMask
      ***  Begin Add  ***  05/19/21  ****************************
     d/copy wPrEmplr
      ***  End   Add  ***  05/19/21  ****************************
     d charFein        s              9
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C     keyWDACL      klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C*
     C* key list for claims file
     C     keyWMCML1     klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase

     C     keyWMAAL1     klist
     C                   kfld                    ##co#
     C                   kfld                    ##grp#
     C                   kfld                    EFagt#

     C     keyWMFNL      klist
     C                   kfld                    ##co#
     C                   kfld                    ##grp#

     C     keyWMEML      klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv

     C     keyWMEFL7     klist
     C                   kfld                    kktype            1
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkkey            30

      //----------------------------------------------------------------------
      // EFTAdd  - EFT Add/Edit popup
      //----------------------------------------------------------------------
     P EFTAdd          B                   export
     D EFTAdd          PI
     D inmode                         1
     D intype                         1
     D infnd                          3  0
     D inrrn                          9  0
     D inrrn1                         9  0
     d inEnv                          1    const  options(*nopass)              RRN - ADD MODE
     d inPolYr                        3  0 const  options(*nopass)              Policy year.

      /free

       // initialize fields and indicators
       Initialize();
       ##typ = *blank;
       ##mod = inmode;
       ##fnd = infnd;
       ##rrn = inrrn;
       //*** Begin Add *** 05/19/21 **********************************
       If %parms() > 5;
          ##Env = inEnv;
       Else;
          ##Env = *blanks;
       Endif;

       If inType = 'E';
          disFyr = *Off;
       Else;
          disFyr = *On;
       Endif;

       scEftHdg = 'EFT Account Information';

       If ##Env = 'Q' and inType = 'E';
          scEftHdg = %Trim(scEftHdg) + ' - Quote';
          roSfyr = *On;                    //Protect Starting Yr in Qte
       Else;
          roSfyr = *Off;
       Endif;

       strType = inType;
       sysDate7 = DateTo7(SystemDate);
       dspAdPrNDt = RequiresPreNote(##Co#:##Grp#:strType:sysDate7);
       disDftEinf = *Off;

       If %parms() > 6;
          ##Sfyr = inPolYr;
          strSfyr = %Editc(inPolYr:'X');
       Else;
          ##Sfyr = *Zeros;
       Endif;
       //*** End   Add *** 05/19/21  **********************************

       // get payee type
       If inmode = 'A' and intype = *blank;
          ##typ = *blank;
          GetPayTyp(##typ);
          intype = ##typ;
       Endif;

       // process for Add mode
       If inmode = 'A' and intype <> *blank;
          ##typ = intype;
          ##rrn1 = inrrn1;
          LodAddEft(##typ:##rrn1);
       Endif;

       // process for edit mode
       If inmode = 'E';
          Chain(n) inrrn pef0p;
          ##typ = EFtype;
          LodEdtEft();
       Endif;

       // process for add/edit
       procAdd(inrrn);

       return;
      /end-free
     P EFTAdd          E

      //----------------------------------------------------------------------
      // ReqPreNote  - processing for Request Pre-Note
      //----------------------------------------------------------------------
     P ReqPreNote      B                   export
     D ReqPreNote      PI
     D inrrn                          9  0

      /free

       isValid = *off;
       intdate = 0;
       clear scErrMsg;
       Chain(n) inrrn pef0p;
      //**** Begin Add  ***  06/23/21  ****************************************
       Select;
       When EFtype = 'C' or EFtype = 'D';
          Chain (efco#:effnd:effyr:efcase) pcm01;
          If %found(WMCML1);
             OutRrn = cm0rrn;
          Endif;
       When EFtype = 'E';
          Chain (efco#:effnd:efemp#:efdiv) pel02;
          If %found(WDELL5);
             OutRrn = el0rrn;
          Endif;
       Endsl;
      //**** End Add  ***  06/23/21  ****************************************
       intdate = dtMDYY(EFpdt);

       If intdate <> 0;
          scErrMsg = 'Pre-note already sent, option is not allowed.';
          isValid = *on;
       Endif;

       If EFid# = *blanks and isValid = *off;
          scErrMsg = 'Pre-note cannot be processed. EFT Bank Info is +
                     not setup.';
          isValid = *on;
       Endif;

       // Check validity for prenote processing
       ##co  = %editc(EFco#:'X');
       ##typ = EFtype;
       ##fndc = %editc(EFfnd:'X');
       ##key = EFkey;
       ##date = %editc(Dateto7(SystemDate()):'X');

       If isValid = *off;
          If not GetEFTStatus(##co:##typ:##fndc:##key:##date:##pre);
             scErrMsg = 'EFT setup not complete, option is not allowed.';
             isValid = *on;
          Endif;

          If ##pre <> *on and isValid = *off;
         scErrMsg = 'Pre-note transaction processing is not allowed +
                        in bank setup.';
             isValid = *on;
          Endif;
       Endif;

       If isValid = *off;
          ##rrn1 = 0;
          ##rrn = inrrn;
          ##rtn = *blanks;
          scCnfCancl = *off;
          scCnfOk  = *off;

          scCnfMsg = 'Request Pre-note.';
          Dou scCnfOk = *on or scCnfCancl = *on;
             Exfmt rcdCnfWdw;
             Select;
             When scCnfCancl = *on;
                scCnfCancl = *off;
                leave;
             When scCnfOk = *on;

                //  Call API to write pre-note to Central Holding file
                WrtHldFile(##typ:##rrn1:##rrn:##rtn);

                If ##rtn <> ' ';
                   scErrMsg = 'Pre-note could not be processed, check bank +
                              setup.';
                   isValid = *on;
                Endif;

                ##typ = 'X';
                ##rrn = 0;
                ##rrn1 = 0;
                ##rtn = *blanks;

                //  Call API again to cleanup
                WrtHldFile(##typ:##rrn1:##rrn:##rtn);
             Endsl;
          Enddo;
       Endif;

       If scErrMsg <> ' ';
          Dou scErrCancl = *on;
              Exfmt rcdErrWdw;
          Enddo;
       Endif;

       return;
      /end-free
     P ReqPreNote      E

      //----------------------------------------------------------------------
      // ClrPreNote  - processing for Clear Pre-Note
      //----------------------------------------------------------------------
     P ClrPreNote      B                   export
     D ClrPreNote      PI
     D inrrn                          9  0

      /free
       isValid = *off;
       intdat  = 0;
       intdate = 0;
       clear scErrMsg;
       Chain(n) inrrn pef0p;
       intdate = dtMDYY(EFadt);
       intdat  = dtMDYY(EFftdt);

       If intdate <> 0 or intdat <> 0;
          scErrMsg = 'Clear pre-note date option is not allowed if an approval +
                     date or the date of first transfer exists.';
          isValid = *on;
          Dou scErrCancl = *on;
              Exfmt rcdErrWdw;
          Enddo;
       Endif;

       // Processing for clear pre-note date, it will have @@mode = 'S'
       // Clear pre note in the DB if either no approval date or date of
       // first transfer exist and then update the file.
       If isValid = *off;
          scCnfCancl = *off;
          scCnfOk  = *off;

          scCnfMsg = 'Clear Pre-note.';
          Dou scCnfOk = *on or scCnfCancl = *on;
             Exfmt rcdCnfWdw;
             Select;
             When scCnfCancl = *on;
                scCnfCancl = *off;
                leave;
             When scCnfOk = *on;
                Chain inrrn pef0p;
                EFpdt = 0;
                EFpgm = q1pgm;
                EFuser = q1user;
                EFdt =  Dateto7(SystemDate());
                EFtime =  Timeto6(SystemTime());
                update pef0p;
             Endsl;
          Enddo;
       Endif;

       unlock WMEFP;
       return;
      /end-free
     P ClrPreNote      E

      //----------------------------------------------------------------------
      // EFTBankInfo - EFT Bank Information
      //----------------------------------------------------------------------
     P EFTBankInfo     B                   export
     D EFTBankInfo     PI
     D inrrn                          9  0

      /free

       Chain(n) inrrn pef0p;
       LodEFTBank();

       // process EFT bank info
       procEFTBank(inrrn);

       unlock WMEFP;
       return;
      /end-free
     P EFTBankInfo     E
      //----------------------------------------------------------------------
      // Initialize - Initialize the fields and indicators
      //----------------------------------------------------------------------
     P Initialize      B
     D Initialize      PI
      /free
       scAddECan  = *off;
       scAddESav  = *off;
       scSelOk    = *off;
       scSelCan   = *off;
       //dspFEIN    = *off;
       dspGrp     = *off;
       dspAgnt    = *off;
       dspClm     = *off;
       dspDpn     = *off;
       dspEmp     = *off;
       dspAcTyp   = *off;
       dspAddAdnP = *off;
       //*** Begin Delete 05/19/21 ***********************************************
       //dspAdPrNDt = *off;
       //*** End   Delete 05/19/21 ***********************************************
       erSlpayTyp = *off;
       clear rcdAddEft;
       isValid = *off;

      /end-free
     P Initialize      E
      //----------------------------------------------------------------------
      // GetPayTyp  - Get Payee Type
      //----------------------------------------------------------------------
     P GetPayTyp       B
     D GetPayTyp       PI
     D inTyp                          1
      /free
       scSlpayTyp = *blanks;
       // show up a window asking for payee type
       dou scSelCan =  *on or (scSelOk = *on and erSlPayTyp = *off);
          exfmt rcdtype;
          select;
          when scSelCan = *on;
             leave;
          when scSelOk = *on;
             scSlpayTyp = Upper(scSlpayTyp);
             If scSlpayTyp <> 'C' and scSlpayTyp <> 'P' and scSlpayTyp <> 'A'
              and scSlpayTyp <> 'D' and scSlpayTyp <> 'E';
                msSlPayTyp = 'WC40044';
                erSlPayTyp = *on;
             Else;
                erSlPayTyp = *off;
                inTyp = scSlpayTyp;
             Endif;
          Endsl;
       Enddo;
      /end-free
     P GetPayTyp       E
      //----------------------------------------------------------------------
      // LodAddEft  - Load EFT add popup screen
      //----------------------------------------------------------------------
     P LodAddEft       B
     D LodAddEft       PI
     D inTyp                          1
     D inRrn                          9  0
      /free
       //*** beg add ***** 07/15/22 *************
       scSFyr = 0;
       //*** end add ***** 07/15/22 *************
       Select;
       When intyp = 'A';
          If inrrn <> *zeros;
             Chain inrrn paa0p;
             //dspFEIN     = *on;
             dspGrp      = *on;
             dspAgnt     = *on;
             ##co#       = AAco#;
             scAddGrp    = AAfnd;
             scAddAgnt   = AAagt#;
             scAddAgtNm  = AAname;
             scAddFeid   = AAfein;
             scAddMod    = AAmod;
             ##grp#      = scAddGrp;
             Chain keyWMFNL pfn00;
             scAddGrpNm  = FNname;
          Endif;

       When intyp = 'C';
          If inrrn <> *zeros;
      //**** Begin Add  ***  06/23/21  ****************************************
             OutRrn = inrrn;
      //**** End Add  ***  06/23/21  ****************************************
             Chain inrrn pcm0p;
             dspclm = *on;
             dspAcTyp = *on;
             scAddFnd = CMfnd;
             scAddFyr = CMfyr;
             scAddCase = CMcase;
             scAddClm# = %trim(%char(CMfnd)) + '-' + %trim(%char(CMfyr)) +'-'+
                         %trim(%char(CMcase));
             ##co# = CMco#;
             kkco# = CMco#;
             kkfnd = CMfnd;
             kkfyr = CMfyr;
             kkcase = CMcase;
             scAddClmNm = %trim(CMlsn) + ', ' + %trim(CMfsn)
                          + ' ' + %trim(CMmid);
             scAddSS#  = MaskData('SSN':'CMss#':CMss#);
             scAddAccN1 = 'Claimant';
             scAddAccN2 = 'Additional Payee';
             scAddAcVl1 = 'C';
             scAddAcVl2 = 'D';
             scAddAcTyp = 'C';

             Clear scAddBldSt;
             scAddBldSt = 'ACco# = ' + %char(CMco#) + ' AND ACfnd = '
              + %char(CMfnd) + ' AND ACfyr = ' + %char(CMfyr)
              + ' AND ACcase = ' + %char(CMcase);
          Endif;

       When intyp = 'D';
          If inrrn <> *zeros;
             Chain inrrn pac0p;
             dspClm  = *on;
             dspDpn  = *on;

             scAddFnd = ACfnd;
             scAddFyr = ACfyr;
             scAddCase = ACcase;
             scAddClm# = %trim(%char(ACfnd)) + '-' + %trim(%char(ACfyr)) +'-'+
                         %trim(%char(ACcase));
             intseq = ACseq#;
             scAddDep = ACpnam;
             scAddClmNm = *blanks;

             ##co#  = ACco#;
             kkco#  = ACco#;
             kkfnd  = ACfnd;
             kkfyr  = ACfyr;
             kkcase = ACcase;
             kkseq# = ACseq#;

             Chain keyWMCML1 pcm01;
             If %found(WMCML1);
                scAddClmNm  = %trim(CMlsn) + ', ' + %trim(CMfsn)
                              + ' ' + %trim(CMmid);
                scAddSS# = MaskData('SSN':'CMss#':CMss#);
      //**** Begin Add  ***  06/23/21  ****************************************
                OutRrn = cm0rrn;
      //**** End Add  ***  06/23/21  ****************************************
             Endif;
          Endif;

       When intyp = 'E';
          If inrrn <> *zeros;
             Chain inrrn pem0p;
      //**** Begin Add  ***  06/23/21  ****************************************
             chain (emco#:emfnd:ememp#:emdiv) pel02;
             If %Found(WDELL5);
                OutRrn = el0rrn;
             Endif;
      //**** End Add  ***  06/23/21  ****************************************
             dspGrp = *on;
             dspEmp = *on;
             //*** Begin Change 05/19/21 *************************************
             //dspAcTyp = *on;
             dspAcTyp = *off;
             //*** End   Change 05/19/21 *************************************
             //dspFEIN  = *on;
             scAddGrp = EMfnd;
             ##grp#   = EMfnd;
             scAddEmp = EMemp#;
             scAddDiv = EMdiv;

             scAddEmpNm = EMname;
             scAddFeid = EMfein;
             scAddMod = *Zeros;

             ##co#  = EMco#;
             kkco#  = EMco#;
             kkfnd  = EMfnd;
             kkemp# = EMemp#;
             kkdiv  = EMdiv;
             Chain keyWMFNL pfn00;
             scAddGrpNm = FNname;
             //*** Begin Add    *** 05/19/21 ***********************************
             strRpFlg = *blank;
             intStRptDiv = GetStRptDv(kkco#:kkfnd:kkemp#:intEMrrn:strRpFlg);
             scDftSrd = *blank;
             //*** End    Add   *** 05/19/21 ***********************************
             //*** Begin Delete 05/19/21 *************************************
             //scAddAccN1 = 'Employer';
             //scAddAccN2 = 'Agency';
             //*** End   Delete 05/19/21 *************************************
             scAddAcVl1 = 'E';
             scAddAcVl2 = 'A';
             scAddAcTyp = 'E';
             //*** Begin Add    05/19/21 *************************************
             scSfyr = ##Sfyr;                    //Default
             If kkdiv = intStRptDiv and strRpFlg = 'Y';
                ttDftSrd = strMsg1;
             Else;
                ttDftSrd = strMsg2;
             Endif;
             intInceptDt = GetEmIncDt(kkco#:kkfnd:kkemp#:kkdiv);
             scYrIncDt =  %subdt(%date(intInceptDt:*CYMD):*years);
             scEnv = ##Env;
             //*** End   Add *** 05/19/21 **************************************
          Endif;

       When intyp = 'P';
          If inrrn <> *zeros;
             Chain inrrn ppd0p;
             //dspFEIN = *on;
             dspGrp  = *on;
             kkfein  = PDfein;
             kkmod   = PDmod;
             scAddPName  = PDname;
             scAddFeid  = PDfein;
             scAddMod = PDmod;
             scAddGrp = ##fnd;
             ##grp#   = ##fnd;
             Chain keyWMFNL pfn00;
             scAddGrpNm  = FNname;
          Endif;

       Endsl;
       //*** Begin Change *** 05/19/21 *****************************************
       //dspAdPrNDt = *off;
       dspAdPrNDt = RequiresPreNote(##Co#:##Grp#:strType:sysDate7);
       //*** End Change 05/19/21 ***********************************************
       edtMode = *off;

      /end-free
     P LodAddEft       E
      //----------------------------------------------------------------------
      // LodEdtEft  - Load EFT update popup screen
      //----------------------------------------------------------------------
     P LodEdtEft       B
     D LodEdtEft       PI
      /free
       ##co# = EFco#;
       //*** beg add ***** 07/15/22 *************
       scSFyr = 0;
       //*** end add ***** 07/15/22 *************
       Select;
       When EFtype = 'P';
          scAddFeid = EFfein;
          scAddMod  = EFmod;
          kkfein = EFfein;
          kkmod = EFmod;
          scAddPName = EFpnam;
          scAddGrp = EFfnd;
          ##grp#   = EFfnd;
          //dspFEIN = *on;
          dspGrp  = *on;
          Chain keyWMFNL pfn00;
          scAddGrpNm = FNname;

       When EFtype = 'C' or EFtype = 'D';
      //**** Begin Add  ***  06/23/21  ****************************************
          Chain (efco#:effnd:effyr:efcase) pcm01;
          If %found(WMCML1);
             OutRrn = cm0rrn;
          Endif;
      //**** End Add  ***  06/23/21  ****************************************
          dspClm  = *on;
          dspAcTyp = *on;
          scAddAccN1 = 'Claimant';
          scAddAccN2 = 'Additional Payee';
          scAddAcVl1 = 'C';
          scAddAcVl2 = 'D';
          If EFtype = 'C';
             dspAddAdnP = *off;
             Clear scAddBldSt;
             scAddBldSt = 'ACco# = ' + %char(EFco#) + ' AND ACfnd = '
              + %char(EFfnd) + ' AND ACfyr = ' + %char(EFfyr)
              + ' AND ACcase = ' + %char(EFcase);
          Endif;
          scAddFnd = EFfnd;
          scAddFyr = EFfyr;
          scAddCase = EFcase;
          scAddClm# = %trim(%char(EFfnd)) + '-' + %trim(%char(EFfyr)) +'-'+
                      %trim(%char(EFcase));
          charFein = %char(EFfein);
          scAddSS#  = MaskData('SSN':'EFfein':charFein);
          kkfyr = EFfyr;
          kkcase = EFcase;
          kkco# = EFco#;
          scAddClmNm = EFpnam;
          scAddAcTyp = 'C';
          kkfnd = EFfnd;
          If EFtype = 'D';
             scAddAcTyp = 'D';
             dspDpn = *on;
             scAddDep = EFpnam;
             scAddAdnP = EFseq#;
             Chain keyWMCML1 pcm01;
             If %found(WMCML1);
                scAddClmNm  = %trim(CMlsn) + ', ' + %trim(CMfsn)
                              + ' ' + %trim(CMmid);
             Endif;
          Endif;

       When EFtype = 'A';
          dspGrp  = *on;
          dspAgnt = *on;
          //dspFEIN = *on;
          scAddGrp = EFfnd;
          ##grp#   = EFfnd;
          scAddAgnt = EFagt#;
          scAddFeid = EFfein;
          scAddMod  = EFmod;
          scAddAgtNm = EFpnam;
          Chain keyWMFNL pfn00;
          scAddGrpNm = FNname;

       When EFtype = 'E';
      //**** Begin Add  ***  06/23/21  ****************************************
          Chain (efco#:effnd:efemp#:efdiv) pel02;
          If %found(WDELL5);
             OutRrn = el0rrn;
          Endif;
      //**** End Add  ***  06/23/21  ****************************************
          dspGrp   = *on;
          dspEmp   = *on;
          //*** Begin Delete *** 05/19/21 ***********************************************
          // dspAcTyp = *on;
          //*** End   Delete *** 05/19/21 ***********************************************
          //dspFEIN  = *on;
          //*** Begin Add *** 05/19/21 **************************************************
          kkCo# = EFco#;
          kkFnd = EFfnd;
          kkEmp# = EFemp#;
          kkdiv = EFdiv;
          //*** End   Add *** 05/19/21 **************************************************
          scAddGrp = EFfnd;
          ##grp#   = EFfnd;
          scAddEmp = EFemp#;
          scAddDiv = EFdiv;
          scAddFeid = EFfein;
          scAddMod  = EFmod;
          scAddEmpNm = EFpnam;
          Chain keyWMFNL pfn00;
          scAddGrpNm = FNname;
          //*** Begin Delete *** 05/19/21 ***********************************************
          //scAddAccN1 = 'Employer';
          //scAddAccN2 = 'Agency';
          //*** End   Delete *** 05/19/21 ***********************************************
          scAddAcVl1 = 'E';
          scAddAcVl2 = 'A';
          //*** Beg Add *** 12/07/21**************************************
          strRpFlg = *blank;
          intStRptDiv = GetStRptDv(EFco#:EFfnd:EFemp#:intEMrrn:strRpFlg);
          scSfyr = EFsfyr;
          scDftSrd = EFdftsrd;
          intInceptDt = GetEmIncDt(kkco#:kkfnd:kkemp#:kkdiv); //needed for
          scYrIncDt = %subdt(%date(intInceptDt:*CYMD):*years);//java scriptto
          scEnv = ##Env;                                      //calc starting yr
          //*** End Add *** 12/07/21**************************************
          //*** Begin Add *** 05/19/21 **************************************************
             If kkdiv = intStRptDiv and strRpFlg = 'Y';
                ttDftSrd = strMsg1;
             Else;
                ttDftSrd = strMsg2;
             Endif;
          //*** End   Add *** 05/19/21 **************************************************
       Endsl;

       scAddEffDt = dtMDYY(EFefdt);
       scAddExpDt = dtMDYY(EFexdt);
       scAddPrNDt = dtMDYY(EFpdt);
       scAddPApDt = dtMDYY(EFadt);
       scAddFTrDt = dtMDYY(EFftdt);
       scAddLTrDt = dtMDYY(EFltdt);
       // account type
       If EFtype = 'A' or EFtype = 'E';
          If EFeatp = 'AG';
             scAddAcTyp = 'A';
          Else;
             scAddAcTyp = 'E';
          Endif;
       Endif;

       //*** Begin Change *** 05/19/21 ***********************************
       //dspAdPrNDt = *on;
       dspAdPrNDt = RequiresPreNote(EFco#:EFfnd:strType:sysDate7);
       //*** Beg Del *** 12/07/21*****************************************
       //***strRpFlg = *blank;
       //***intStRptDiv = GetStRptDv(EFco#:EFfnd:EFemp#:intEMrrn:strRpFlg);
       //***scSfyr = EFsfyr;
       //***scDftSrd = EFdftsrd;
       //***intInceptDt = GetEmIncDt(kkco#:kkfnd:kkemp#:kkdiv);   //needed for
       //***scYrIncDt =  %subdt(%date(intInceptDt:*CYMD):*years); //java script to
       //***scEnv = ##Env;                                        //calc starting yr
       //*** End Del *** 12/07/21*****************************************
      //*** End    Change *** 05/19/21 ***********************************
       edtMode = *on;

      /end-free
     P LodEdtEft       E
      //----------------------------------------------------------------------
      // LodEFTBank - Load EFT bank information
      //----------------------------------------------------------------------
     P LodEFTBank      B
     D LodEFTBank      PI
      /free

       scEFTSav   = *off;
       scEFTCan   = *off;
       dspEFTAcTy = *off;
       dspEFTGrp  = *off;
       dspEFTAgnt = *off;
       //dspEFTFMod = *off;
       dspEFTClm  = *off;
       dspEFTDep  = *off;
       dspEFTEmpD = *off;
       clear rcdEFTbank;

       Select;
       When EFtype = 'P';
          //dspEFTFMod = *on;
          scEFTFeMod = %trim(%char(EFfein)) + ' - ' + %trim(%char(EFmod))
                       + '    ' + %trim(EFpnam);

       When EFtype = 'C' or EFtype = 'D';
      //**** Begin Add  ***  06/23/21  ****************************************
          Chain (efco#:effnd:effyr:efcase) pcm01;
          If %found(WMCML1);
             OutRrn = cm0rrn;
          Endif;
      //**** End Add  ***  06/23/21  ****************************************
          dspEFTClm = *on;
          dspEFTAcTy = *on;
          scEFTClm = %trim(EFpnam);
          scEFTSS# = %char(EFfein);
          scEFTAcTyp = 'C - Claimant';
          If EFtype = 'D';
             clear scEFTAcTyp;
             scEFTAcTyp = 'D - Additional Payee';
             kkco# = EFco#;
             kkfnd = EFfnd;
             kkfyr = EFfyr;
             kkcase = EFcase;
             dspEFTDep = *on;
             scEFTDep  = EFpnam;
             Chain keyWMCML1 pcm01;
             If %found(WMCML1);
                clear scEFTClm;
                scEFTClm=%trim(CMlsn)+', '+%trim(CMfsn) + ' ' + %trim(CMmid);
             Endif;
          Endif;

       When EFtype = 'A';
          dspEFTGrp  = *on;
          dspEFTAgnt = *on;
          //dspEFTFMod = *on;
          ##co# = EFco#;
          ##grp# = EFfnd;
          Chain keyWMFNL pfn00;
          scEFTGrp = %trim(FNname);
          scEFTFeMod = %trim(%char(EFfein)) + ' - ' + %trim(%char(EFmod));

          Chain keyWMAAL1 paa01;
          scEFTAgnt# = %trim(AAname);

       When EFtype = 'E';
      //**** Begin Add  ***  06/23/21  ****************************************
          Chain (efco#:effnd:efemp#:efdiv) pel02;
          If %found(WDELL5);
             OutRrn = el0rrn;
          Endif;
      //**** End Add  ***  06/23/21  ****************************************
          dspEFTGrp  = *on;
          dspEFTEmpD = *on;
          dspEFTAcTy = *on;
          ##co# = EFco#;
          ##grp# = EFfnd;
          Chain keyWMFNL pfn00;
          scEFTGrp = %trim(FNname);
          scEFTEmpDv = %trim(EFpnam);
          If EFeatp = ' ';
             scEFTAcTyp = 'Employer';
          Elseif EFeatp = 'AG';
             scEFTAcTyp = 'Agency';
          Endif;
       Endsl;

       scEFTChkSv = EFactp;
       scEFTDFIId = EFid#;
       scEFTDFIDg = EFdgit;
       scEFTDFIAN = EFacct;

       scEFTEfDt  = dtMDYY(EFefdt);
       scEFTExpDt = dtMDYY(EFexdt);

      /end-free
     P LodEFTBank      E
      //----------------------------------------------------------------------
      // procAdd - process Add/edit popup screen
      //----------------------------------------------------------------------
     P procAdd         B
     D procAdd         PI
     D inrrn#                         9  0
      /free
       isValid = *off;
       svAddAcTyp = scAddAcTyp;

       Dou scAddECan = *on or (scAddESav = *on and isValid = *off);
          Exfmt rcdAddEft;
          isValid = *off;

          Select;
          When scAddECan = *on;
             leave;

          When ##typ = 'C' and scAddAcTyp <> svAddAcTyp;
             svAddAcTyp = scAddAcTyp;
             If scAddAcTyp ='D';
                dspAddAdnP = *on;
             Else;
                dspAddAdnP = *off;
             Endif;

          When scAddESav = *on;
           Validate();
             If isValid = *off;
                savChanges(inrrn#);
             Endif;
          Endsl;
       Enddo;
       unlock WMEFP;
      /end-free
     P procAdd         E
      //----------------------------------------------------------------------
      // procEFTBank - process EFT bank info screen
      //----------------------------------------------------------------------
     P procEFTBank     B
     D procEFTBank     PI
     D inRrn3                         9  0
      /free

       isValid = *off;

       Dou scEFTCan = *on or (scEFTSav = *on and isValid = *off);
          Exfmt rcdEftBank;
          isValid = *off;

          Select;
          When scEFTCan = *on;
             leave;

          When scEFTSav = *on;
             ValidEftBank();
             If isValid = *off;
                savEFTBank(inrrn3);
             Endif;
          Endsl;
       Enddo;

      /end-free
     P procEFTBank     E
      //----------------------------------------------------------------------
      // Validate - Validate Add/edit popup screen
      //----------------------------------------------------------------------
     P Validate        B
     D Validate        PI
      /free
       erAddEffDt = *off;
       erAddExpDt = *off;
       erAddPApDt = *off;
       erAddPrNDt = *off;
       erAddEfDt1 = *off;
       erAddAdnP = *off;
       isValid = *off;

       //Validate effective date
       If scAddEffDt = 0;
          erAddEffDt = *on;
          msAddEffDt = 'WCR0556';
          isValid = *on;
       Endif;

       //*** beg chg ***** 07/15/22 *****************
       If scAddEffDt <> 0 and scAddEffDt <> dtMDYY(EFefdt) and isValid = *off;
          scAddEffDt = DtCheck(scAddEffDt:'*mdy');
          If scAddEffDt = *zero;
             erAddEffDt = *on;
             msAddEffDt = 'WCR0500';
             isValid = *on;
          Endif;

          If isValid = *off;
             intdate = dtYYMD(scAddEffDt);
             If intDate < int1940;
                erAddEffDt = *on;
                msAddEffDt = 'WCR0500';
                isValid = *on;
             Else;
                Select;
                When ##typ = 'E';

                Other;
                   intdat7 = Dateto7(SystemDate());
                   intdat = dtYYMD(dtMDYY(intdat7));
                   If intdate < intdat;
                      erAddEffDt = *on;
                      msAddEffDt = 'WCR3210';
                      isValid = *on;
                   Endif;
                Endsl;
             Endif;
          Endif;
       EndiF;

       //Validate expiration date
       If isValid = *off and scAddExpDt <> 0 and scAddExpDt  <> dtMDYY(EFexdt);
          scAddExpDt = DtCheck(scAddExpDt:'*mdy');
          If scAddExpDt = *zero;
             erAddExpDt = *on;
             msAddExpDt = 'WCR0500';            //Invalid date entered
             isValid = *on;
          Endif;

          If isValid = *off;
             intExpDt = dtYYMD(scAddExpDt);
             If intExpDt < int1940;
                erAddExpDt = *on;
                msAddExpDt = 'WCR0500';       //Invalid date entered
                isValid = *on;
             Else;
                intdat7 = Dateto7(SystemDate());
                intdat = dtYYMD(dtMDYY(intdat7));

                //Exp date cannot be less than system date
                If (intExpDt < intdat);
         //*** Beg Add *** 06/23/23 *** 110581 *******************************
         // Check if there is another valid record on the policy for that
         // and the one being expired has no transfer dates

                  If scAddFTrDt = 0 and scAddLTrDt = 0 ;
                     kktype = 'E';
                     kkco# = ##co#;
                     kkfnd = scAddGrp;
                     kkkey = %editc(scAddEmp:'X') + %editc(scAddDiv:'X');
                     kkefdt = dtCYMD(scAddEffDt);

                     MarkExpired = *Off;
                     SetLL (kktype: kkco#: kkfnd: kkkey: kkefdt) pef07;
                     ReadE (kktype: kkco#: kkfnd: kkkey: kkefdt) pef07;
                     Dow not %eof(WMEFL7) and isValid = *off;
                         If ef7rrn <> ##rrn and L7EFexdt = *zeros;
                            MarkExpired = *On;
                            Leave;
                         EndIf;
                     ReadE (kktype: kkco#: kkfnd: kkkey: kkefdt) pef07;
                     EndDo;
                     If MarkExpired = *Off;
                        erAddExpDt = *on;
                        msAddExpDt = 'WCR3210';
                        isValid = *on;
                     EndIf;
                  Else;
         //*** End Add *** 06/23/23 *** 110581 *******************************
                   erAddExpDt = *on;
                   msAddExpDt = 'WCR3210';
                   isValid = *on;
         //*** Beg Add *** 06/23/23 *** 110581 *******************************
                  Endif;
         //*** End Add *** 06/23/23 *** 110581 *******************************
                Endif;

                //Exp date cannot be less than effective date
         //*** Beg Chg *** 06/23/23 *** 110581 *******************************
            //  If intExpDt < intdate and isValid = *Off;
                If intExpDt < dtYYMD(scAddEffDt) and isValid = *Off;
         //*** End Chg *** 06/23/23 *** 110581 *******************************
                   erAddExpDt = *on;
                   msAddExpDt = 'WCR3211';
                   isValid = *on;
                Endif;
             Endif;

          Endif;
       Endif;


       //*** end chg ***** 07/15/22 *****************

       //
       // If Additional Payee Type then an Additional Payee must be selected.
       //
       If scAddAcTyp = 'D' And scAddAdnP = 0;
          erAddAdnP = *On;
          isValid = *On;
       EndIf;

       // Pre note date
       If isValid = *off and scAddPrNDt <> 0 and scAddPrNDt <> dtMDYY(EFpdt);
          scAddPrNDt = DtCheck(scAddPrNDt:'*mdy');
          If scAddPrNDt = *zero;
             erAddPrNDt = *on;
             msAddPrNDt = 'WCR0500';
             isValid = *on;
          Endif;

          If isValid = *off;
             intPreDt = dtYYMD(scAddPrNDt);
             If intPreDt < int1940;
                erAddPrNDt = *on;
                msAddPrNDt = 'WCR0500';
                isValid = *on;
             Else;
                intdat7 = Dateto7(SystemDate());
                intdat = dtYYMD(dtMDYY(intdat7));
                If  intPreDt < intdat;
                   erAddPrNDt = *on;
                   msAddPrNDt = 'WCR3210';
                   isValid = *on;
                Endif;
             Endif;
          Endif;
       Endif;

       // approval date
       If isValid = *off and scAddPApDt <> 0 and scAddPApDt <> dtMDYY(EFadt);
          scAddPApDt = DtCheck(scAddPApDt:'*mdy');
          If scAddPApDt = *zero;
             erAddPApDt = *on;
             msAddPApDt = 'WCR0500';
             isValid = *on;
          Endif;

          If isValid = *off;
             intAprDt = dtYYMD(scAddPApDt);
             If intAprDt < int1940;
                erAddPApDt = *on;
                msAddPApDt = 'WCR0500';
                isValid = *on;
             Else;
                intdat7 = Dateto7(SystemDate());
                intdat = dtYYMD(dtMDYY(intdat7));
                If  intAprDt < intdat;
                   erAddPApDt = *on;
                   msAddPApDt = 'WCR3210';
                   isValid = *on;
                Endif;
             Endif;
          Endif;

          // Pre-note date should be non-zero for approval date to be entered
          If isValid = *off;
             If scAddPrNDt = *zero;
                erAddPApDt = *on;
                msAddPApDt = 'WCR3317';
                isValid = *on;
             Endif;
          Endif;
       Endif;

       //Validate starting policy Year;
       //*** beg chg ***** 09/24/21
       //*** If ##typ = 'E';
       If ##typ = 'E' And scSfyr <> 0;
       //*** end chg ***** 09/24/21
          KeyWdell.ELco#  = kkCo#;
          KeyWdell.ELfnd  = kkFnd;
          KeyWdell.ELemp# = kkEmp#;
          KeyWdell.ELdiv  = kkdiv;
          KeyWdell.ELfyr  = scSfyr;
          setll %kds(KeyWdell) PEL01;
          If %equal(wdell);
             erSfyr = *Off;
          Else;
             mgSfyr = 'WC31477';
             erSfyr = *On;
             isValid = *on;
          Endif;
       Endif;

       // Check for overlapping date ranges
       kkco# = ##co#;
       kkefdt = dtCYMD(scAddEffDt);
       intdat7 = dtCYMD(scAddExpDt);

       //build key
       clear kkkey;
       Select;
       When ##typ = 'C' or ##typ = 'D';
          kkkey = %editc(scAddFyr:'X') + %editc(scAddCase:'X');
          kktype = 'C';
          kkfnd = scAddFnd;
          If ##typ = 'D';
             kkkey = %trim(kkkey) + %editc(intseq:'X');
             kktype = 'D';
          Endif;
          If scAddAcTyp = 'D';
             kkkey = %trim(kkkey) + %editc(scAddAdnP:'X');
             kktype = 'D';
          Endif;

       When ##typ = 'A';
          kkkey = %editc(scAddAgnt:'X');
          kktype = 'A';
          kkfnd = scAddGrp;

       When ##typ = 'E';
          kkkey = %editc(scAddEmp:'X') + %editc(scAddDiv:'X');
          If scAddAcTyp = 'A';
             kkkey = %trim(kkkey) + 'AG';
             scAddAcTyp = 'A';
          Endif;
          kktype = 'E';
          kkfnd = scAddGrp;
          //*** Begin Add *** 05/19/21 *************************
          strKey = %editc(scAddEmp:'X') + %editc(intStRptDiv:'X');
          If scAddAcTyp = 'A';
             strkey = %trim(strKey) + 'AG';
          Endif;

          //*** End   Add *** 05/19/21 *************************
       When ##typ = 'P';
          kkkey = %editc(scAddFeid:'X') + %editc(scAddMod:'X');
          kktype = 'P';
          kkfnd = scAddGrp;
       Endsl;

       Setll keyWMEFL7 pef07;
       Reade keyWMEFL7 pef07;
       Dow not %eof(WMEFL7) and isValid = *off;
      //*** Begin Add *** 05/19/21 ******************************************
          If ((##env = *blank and l7EFenvflg = *blank) Or (##env = 'Q'));
      //*** End   Add    *** 05/19/21 ******************************************
             If ##mod = 'A';
                Select;
                When l7EFefdt = kkefdt and l7EFexdt <> l7EFefdt;
                   //*** Begin Change *** 05/19/21 ******************************************
                   //erAddExpDt = *on;
                   //msAddExpDt = 'WC30297';
                   //isValid = *on;
                   If scSfyr <> EFsfyr;
                      //OK to overlap when starting years are different
                   Else;
                      erAddExpDt = *on;
                      msAddExpDt = 'WC30297';
                      isValid = *on;
                   Endif;
                   //*** End   Change *** 05/19/21 ******************************************
                When l7EFefdt < kkefdt and (l7EFexdt = 0 or l7EFexdt > kkefdt);
                   //*** Begin Change *** 05/19/21 ******************************************
                   //erAddExpDt = *on;
                   //msAddExpDt = 'WC30297';
                   //isValid = *on;
                   If scSfyr <> EFsfyr;
                      //OK to overlap when starting years are different
                   Else;
                      erAddExpDt = *on;
                      msAddExpDt = 'WC30297';
                      isValid = *on;
                   Endif;
                   // Never allow a new eft record with an eff date prior to
                   // an existing record, unless that existing record is expired. Different
                   // starting policy year does not override this rule.
                   //*** End   Change *** 05/19/21 ******************************************
                When l7EFefdt > kkefdt and  l7EFefdt <> l7EFexdt;
                   erAddExpDt = *on;
                   msAddExpDt = 'WC30297';
                   isValid = *on;
                Endsl;
             Else;                                 //On Update
                Select;
                When ef7rrn = ##rrn;          //same record??? DISCUSS  PENDING
                                              //no overlapping because its being replaced
                When kkefdt = intdat7;        //being expired
                When l7EFefdt = l7EFexdt;     //Expired
                //*** Begin Change *** 05/19/21 *********************************************
                //When kkefdt <= l7EFefdt and (intdat7 > l7EFefdt or intdat7 = 0);  //DISCUSS
                // New record effective date cannot be prior to any existing unexpired record
                   //erAddExpDt = *on;
                   //msAddExpDt = 'WC30297';
                   //isValid = *on;
                When kkefdt = l7EFefdt and (intdat7 > l7EFefdt or intdat7 = 0);  //DISCUSS
                   If scSfyr <> EFsfyr;
                      //OK to overlap when starting years are different
                   Else;
         //*** Beg Add *** 06/23/23 *** 110581 *******************************
                      If MarkExpired = *Off;
         //*** End Add *** 06/23/23 *** 110581 *******************************
                      erAddExpDt = *on;
                      msAddExpDt = 'WC30297';
                      isValid = *on;
         //*** Beg Add *** 06/23/23 *** 110581 *******************************
                      EndIf;
         //*** End Add *** 06/23/23 *** 110581 *******************************
                   Endif;
                When kkefdt > l7EFefdt and kkefdt < l7EFexdt;
                   //erAddExpDt = *on;
                   //msAddExpDt = 'WC30297';
                   //isValid = *on;
                   If scSfyr <> EFsfyr;
                      //OK to overlap when starting years are different
                   Else;
         //*** Beg Add *** 06/23/23 *** 110581 *******************************
                      If MarkExpired = *Off;
         //*** End Add *** 06/23/23 *** 110581 *******************************
                      erAddExpDt = *on;
                      msAddExpDt = 'WC30297';
                      isValid = *on;
         //*** Beg Add *** 06/23/23 *** 110581 *******************************
                      EndIf;
         //*** End Add *** 06/23/23 *** 110581 *******************************
                   Endif;
                Endsl;
             Endif;
                //*** End   Change *** 05/19/21 *********************************************
          //*** Begin Add  *** 05/19/21 ******************************************
          Endif;
          //*** End   Add  *** 05/19/21 ******************************************
          Reade keyWMEFL7 pef07;
       Enddo;

      /end-free
     P Validate        E
      //----------------------------------------------------------------------
      // ValidEftBank - validate EFT bank info screen
      //----------------------------------------------------------------------
     P ValidEftBank    B
     D ValidEftBank    PI
      /free

       erEFTChkSv = *off;
       erEFTDFIId = *off;
       erEFTDFIDg = *off;
       erEFTDFIAN = *off;
       isValid = *off;

       // Validate check/saving
       scEFTChkSv = upper(scEFTChkSv);
       If scEFTChkSv = *blanks or (scEFTChkSv <> 'C' and scEFTChkSv <> 'S');
          erEFTChkSv = *on;
          msEFTChkSv = 'WCR1087';
          isValid = *on;
       Endif;
       If isValid = *off and scEFTDFIId = *blanks;
          erEFTDFIId = *on;
          msEFTDFIId = 'WCR0503';
          isValid = *on;
       EndiF;

       If isValid = *off and scEFTDFIDg = *blanks;
          erEFTDFIDg = *on;
          msEFTDFIDg = 'WCR0503';
          isValid = *on;
       EndiF;

       If isValid = *off and scEFTDFIAN = *blanks;
          erEFTDFIAN = *on;
          msEFTDFIAN = 'WCR0503';
          isValid = *on;
       EndiF;

      /end-free
     P ValidEftBank    E
      //----------------------------------------------------------------------
      // savChanges - Save Changes for Add/edit popup screen
      //----------------------------------------------------------------------
     P savChanges      B
     D savChanges      PI
     D inRrn2                         9  0
      /free
      //*** Begin Add *** 05/19/21 ********************************************
       Dcl-S outRrn  packed(9:0);
       Dcl-S outFyr  packed(3:0);
       Dcl-S outExdt packed(7:0);
      //*** End   Add *** 05/19/21 ********************************************
       Chain inRrn2 pef0p;
       EFco# =  ##co#;
       EFefdt = dtCYMD(scAddEffDt);
       EFexdt = dtCYMD(scAddExpDt);
       EFpdt  = dtCYMD(scAddPrNDt);
       EFadt  = dtCYMD(scAddPApDt);
       EFeatp = ' ';

       If ##mod = 'A';
          Select;
          When ##typ = 'C' or ##typ = 'D';
             EFfnd = scAddFnd;
             EFfyr = scAddFyr;
             EFcase = scAddCase;
             If scAddSS# <> *blanks;
                clear charFein;
                //*** Beg Chg *** 12/07/21 *************************************
                //***charFein = UnMaskData('SSN':'EFfein':scAddSS#);
                charFein = UnMaskData('SSN':'CMss#':scAddSS#);
                //*** End Chg *** 12/07/21 *************************************
                EFfein = %dec(charFein:9:0);
             Endif;
             EFpnam = scAddClmNm;
             EFmod = 0;
             EFtype = 'C';
             EFkey = %editc(scAddFyr:'X') + %editc(scAddCase:'X');
             If ##typ = 'C' and scAddAcTyp = 'D';
                kkco#  = EFco#;
                kkfnd  = EFfnd;
                kkfyr  = EFfyr;
                kkcase = EFcase;
                kkseq# = scAddAdnP;
                Chain keyWDACL pac00;
                If %found(WDACL);
                   EFpnam = ACpnam;
                   EFtype = 'D';
                   EFseq# = ACseq#;
                   EFkey = %trim(EFkey) + %editc(ACseq#:'X');
                Endif;
             Endif;

             If ##typ = 'D';
                EFpnam = scAddDep;
                EFtype = 'D';
                EFseq# = intseq;
                EFkey = %trim(EFkey) + %editc(intseq:'X');
             Endif;

          When ##typ = 'P';
             EFkey = %editc(scAddFeid:'X') + %editc(scAddMod:'X');
             EFfnd  = scAddGrp;
             EFfein = scAddFeid;
             EFmod = scAddMod;
             EFpnam = scAddPName;
             EFtype = 'P';
             EFseq# = 0;

          When ##typ = 'A';
             EFfnd  = scAddGrp;
             EFfein = scAddFeid;
             EFmod = scAddMod;
             EFtype = 'A';
             EFagt# = scAddAgnt;
             EFpnam = scAddAgtNm;
             EFkey = %editc(scAddAgnt:'X');

          When ##typ = 'E';
             EFfnd  = scAddGrp;
             EFfein = scAddFeid;
             EFmod = scAddMod;
             EFtype = 'E';
             EFemp# = scAddEmp;
             EFdiv  = scAddDiv;
             If scAddAcTyp = 'A';
                EFeatp = 'AG';
                clear ##anam#;
                ##fnd# =  EFfnd;
                ##emp# =  EFemp#;
                ##div# =  EFdiv;
                ##fyr# =  0;

                // GET Agent/Producer Information
                GetAgnPrInfo(##co#:##fnd:##emp#:##div#:##fyr#:##agt#:##anam#:
                 ##prod#:##pnam#);

                EFpnam = ##anam#;
                EFagt# = ##agt#;
             Else;
                EFeatp = ' ';
                EFpnam = scAddEmpNm;
             Endif;

             EFkey = %editc(scAddEmp:'X') + %editc(scAddDiv:'X');
             EFkey = %trim(EFkey) + EFeatp;

          Endsl;
          EFtrcd = *blanks;
          EFpre  = *blanks;
          EFid#  = *blanks;
          EFdgit = *blanks;
          EFacct = *blanks;
          EFstat = *blanks;
          EFipgm = q1pgm;
          EFiusr = q1user;
          EFidt =  Dateto7(SystemDate());
          EFitm =  Timeto6(SystemTime());
       Endif;

       //*** Begin Add 05/19/21 ********* 05/19/21 ********************
       EFenvflg = ##Env;                                           //05/19/21
       EFsfyr = scSfyr;
       EFdftsrd = scDftSrd;
       //*** End   Add 05/19/21 ********* 05/19/21 ********************
       EFpgm = q1pgm;
       EFuser = q1user;
       EFdt =  Dateto7(SystemDate());
       EFtime =  Timeto6(SystemTime());

       If ##mod = 'A';
          write pef0p;
          //*** Begin Add *** 05/19/21 *************************************
          outRrn  = ef0rrn;
          outFyr  = EFfyr;
          outExDt = EFefdt;
          expireEft(outRrn:outFyr:outExDt);
          //*** End   Add *** 05/19/21 *************************************
       Elseif ##mod = 'E';
          update pef0p;
       Endif;
       unlock WMEFP;

      /end-free
     P savChanges      E
      //----------------------------------------------------------------------
      // savEftBank - Save Changes for EFT bank info screen
      //----------------------------------------------------------------------
     P savEftBank      B
     D savEftBank      PI
     D inrrn4                         9  0
      /free

       Chain inRrn4 pef0p;
       EFpgm = q1pgm;
       EFuser = q1user;
       EFdt =  Dateto7(SystemDate());
       EFtime =  Timeto6(SystemTime());

       EFactp =  scEFTChkSv;
       EFid#  =  scEFTDFIId;
       EFdgit =  scEFTDFIDg;
       EFacct =  scEFTDFIAN;
       update pef0p;
       Unlock WMEFP;
      /end-free
     P savEftBank      E
      //----------------------------------------------------------------------
      // BuildEFTkey - Build EFT key
      //----------------------------------------------------------------------
     P BuildEFTkey     B                   Export
     D BuildEFTkey     PI            30
     D  inType                        1    const                                1
     D  inEmp#                        9  0 options(*nopass) const               2
     D  inDiv                         5  0 options(*nopass) const               3
     D  inEATP                        2    options(*nopass) const               4
     D  inFYR                         3  0 options(*nopass) const               5
     D  inCase                        7  0 options(*nopass) const               6
     D  inDSeq                        5  0 options(*nopass) const               7
     D  inFein                        9  0 options(*nopass) const               8
     D  inMod                         3  0 options(*nopass) const               9
     D  inAgt#                        5  0 options(*nopass) const               10

     D xxKey           s             30    inz
      /free
       Select;
       When inType = 'A';
         If %Parms > 9;
           xxKey = %Editc(inAgt#:'X');
         Endif;
       When inType = 'C';
         If %Parms > 4;
           xxKey = %Editc(inFyr:'X') + %Editc(inCase:'X');
         Endif;
       When inType = 'D';
         If %Parms > 4;
           xxKey = %Editc(inFyr:'X')+%Editc(inCase:'X')+%Editc(inDSeq:'X');
         Endif;
       When inType = 'E';
         If %Parms > 1;
           xxKey = %Editc(inEmp#:'X') + %Editc(inDiv:'X') + inEATP;
         Endif;
       When inType = 'P';
         If %Parms > 7;
           xxKey = %Editc(inFein:'X') + %Editc(inMod:'X');
         Endif;
       Endsl;
       Return xxKey;
      /end-free
     P BuildEFTkey     E
      //----------------------------------------------------------------------
      // expireEft  - Expire EFT record with same starting year.
      //----------------------------------------------------------------------
       Dcl-Proc expireEft;
        Dcl-Pi *N;
         in0Rrn packed(9:0);
         inSfyr packed(3:0);
         inExdt packed(7:0);
        End-Pi;
         Setll keyWMEFL7 pef07;
         Reade keyWMEFL7 pef07;

         Dow not %eof(WMEFL7);
            If inSfyr = L7EFsfyr and in0Rrn <> ef7rrn;
               If EFefdt >=L7efdt and (L7EFexdt = *zeros or L7EFexdt > EFefdt);
                  chain ef7rrn pef0p;
                  If %found(Wmefp);
                     EFexdt = inExDt;
                     Update pef0p;
                     Leave;
                  Endif;
               Endif;
            Endif;
            Reade keyWMEFL7 pef07;
         Enddo;

       End-Proc;
