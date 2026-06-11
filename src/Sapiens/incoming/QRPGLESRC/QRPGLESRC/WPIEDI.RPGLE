     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H nomain
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H* 02/17/22 C:79  Project: WC2797                                                     *
     H*                Add IAIABC 3.4.1 Screen Control Procedure
     H*                                                                                    *
     H* 02/12/14 C:53  REDMINE: 3881                                                       *
     H*                                                                                    *
     H**************************************************************************************
     Fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     Fssylf     if   e           k disk    rename(swc01:swc00)
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     Fwmlgl     if   e           k disk    rename(plg01:plg00)
     Fwmixl     if   e           k disk    rename(pix01:pix00)
     Fwtabl     if   e           k DISK    rename(prt01:prt00)
     F                                     ignore(pat01)
     F                                     ignore(psp01)
     F                                     ignore(pnc01)
     F                                     ignore(prc01)
     F                                     ignore(ppc01)
     F                                     ignore(pb201)
     F                                     ignore(pb701)
     F                                     ignore(pbs01)
     F                                     ignore(ptt01)
     F                                     ignore(pcc01)
     F                                     ignore(pbc01)
     F                                     ignore(pcs01)
     F                                     ignore(pcn01)
     F                                     ignore(prs01)
     F                                     ignore(plr01)
     F                                     ignore(pvs01)
     F                                     ignore(ptr01)
     F                                     ignore(phy01)
     F                                     ignore(psy01)
     F                                     ignore(pba01)
     F                                     ignore(pid01)
     F                                     ignore(pis01)

     d* Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

     d* EDI Control
     d GetControlInfo  pr                  ExtPgm('WA076R')
     d   company#                     3  0
     d   jurState                     2
     d   tranID                       3
     d   group#                       3  0
     d   returnCode                   1
     d   ediFlag                      3
     d   IXeLt                        1
     d   IXeLtD                       3  0
     d   IXfroi                       2
     d   IXsDue                       3  0
     d   IXrls                        1
     d   IXcsDt                       7  0
     d   IXeaDc                       5
     d   IXhcPr                       5
     d   IXfnRc                       5
     d   IXhcEd                       1
     d   IXcrEr                       5
     d   IXceIr                       5
     d   IXceCr                       5
     d   IXctAr                       5
     d   IXctRr                       5
     d   IXmo                         1
     d   IXmLtD                       3  0

     d* Checks to see if claim has comp payments
     DHasCompPmts      pr              n
     D claimRrnIn                     9  0 const

     D/copy wprusrprms
     D/copy wprgroup
     D/copy wpredi

     D psds          esds                  extname(szq1p)

     D  rsvCodeDs      ds
     D  CMrt01
     D  CMrt02
     D  CMrt03
     D  CMrt04
     D  CMrt05
     D  CMrt06
     D  CMrt07
     D  CMrt08
     D  CMrt09
     D  CMrt10
     D  rsvCode                       2    Overlay(rsvCodeDs) Dim(10)

     D  rsvPaidDs      ds
     D  CMar01
     D  CMar02
     D  CMar03
     D  CMar04
     D  CMar05
     D  CMar06
     D  CMar07
     D  CMar08
     D  CMar09
     D  CMar10
     D  rsvPaid                       9  2 Overlay(rsvPaidDs) Dim(10)

     d tranID          s              3
     d returnCode      s              1    Inz(' ')
     d ediFlag         s              3    Inz(' ')
     d IXeLt           s              1    Inz(' ')
     d IXeLtD          s              3  0 Inz(0)
     d IXfroi          s              2    Inz(' ')
     d IXsDue          s              3  0 Inz(0)
     d IXrls           s              1    Inz(' ')
     d IXcsDt          s              7  0 Inz(0)
     d IXeaDc          s              5    Inz(' ')
     d IXhcPr          s              5    Inz(' ')
     d IXfnRc          s              5    Inz(' ')
     d IXhcEd          s              1    Inz(' ')
     d IXcrEr          s              5    Inz(' ')
     d IXceIr          s              5    Inz(' ')
     d IXceCr          s              5    Inz(' ')
     d IXctAr          s              5    Inz(' ')
     d IXctRr          s              5    Inz(' ')
     d IXmo            s              1    Inz(' ')
     d IXmLtD          s              3  0 Inz(0)

     d tranSetId       s                   Like(IXtrId)
     d outCltID        s              3
     d key             s             10
     C*
     C     keyWMFNL      klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C*
     C     keyWMLGL      klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C                   kfld                    CMfYr
     C                   kfld                    CMcase
     C*
     C     keyWMIXL      KLIST
     C                   KFLD                    CMco#
     C                   KFLD                    CMjrSt
     C                   KFLD                    tranSetId
     C                   KFLD                    FNediF
     C*
     C     keyWTABL      KLIST
     C                   KFLD                    CMco#
     C                   KFLD                    key
    P*====================================================================
    P* IsEdi - Determine if EDI claim
    P*
    P*  IsEdi - returns is EDI claim
    P*   claimRrn  - accepts the Claim RRN                       (required)
    P*====================================================================
     PIsEdi            b                   export
    P*
     DIsEdi            PI              n
     D claimRrnIn                     9  0 const

      /free

       // Get Claim Information
       Chain claimRrnIn pcm0p;

       // Is EDI installed?
       Chain 'WC' swc00;

       // Get EDI Control Information
       If WCfl09='Y';
          tranId='148';
          GetControlInfo(CMco#:CMjrSt:tranId:CMfnd:returnCode:ediFlag:
           IXeLt:IXeLtD:IXfroi:IXsDue:IXrls:IXcsDt:IXeaDc:IXhcPr:IXfnRc:IXhcEd:
           IXcrEr:IXceIr:IXceCr:IXctAr:IXctRr:IXmo:IXmLtD);
           If returnCode='Y';
              Return *On;
           EndIf;
       EndIf;

       Return *Off;

      /end-free

     PIsEdi            e
    P*====================================================================
    P* ExitEdi148 - Determine if EDI 148 Processing Should End
    P*
    P*  ExitEdi148  - returns Exit
    P*   claimRrn   - accepts the Claim RRN                       (required)
     P*   exitReason - exit Reason
    P*====================================================================
     PExitEdi148       b                   export
    P*
     DExitEdi148       PI              n
     D claimRrnIn                     9  0 const
     D exitReason                    80

      /free
       GetClientID(outCltID);

       ExitReason=' ';
       Chain claimRrnIn pcm0p;

       // Is Group Enrolled into EDI?
       tranSetId=' ';
       Chain keyWMFNL pfn00;
       If %Found(WMFNL) And FNedi='Y';
          exitReason='Group not enrolled in EDI.';
          Return *On;
       EndIf;

       // Claim must have valid Lost time flag
       If CMltFl<>'I' And CMltFl<>'M' And CMltFl<>'I';
          exitReason='Invalid claim lost time flag.';
          Return *On;
       EndIf;

       // RAT Control Record must not Exist
       tranSetId='RAT';
       Chain keyWMIXL pix00;
       If %Found(WMIXL);
          exitReason='RAT(Rose Auto Trigger) Control Record Exists.';
          Return *On;
       EndIf;

       // Control Record must Exist
       tranSetId=' ';
       Chain keyWMIXL pix00;
       If not %Found(WMIXL);
          exitReason='Claim Control Record Doesn''t Exist.';
          Return *On;
       Else;
          // Make sure the claim is effective for EDI
          If (outCltID<>'114' And CMiDt<IXcsDt) Or
             (outCltID='114' And CMacDt<IXcsDt);
             exitReason='Claim was entered prior to date of EDI processing.';
             Return *On;
          EndIf;
       EndIf;

       // Make sure that a log record hasn't already been sent or extracted
       Chain keyWMLGL plg00;
       If %Found(WMLGL);
          DoU %Eof(WMLGL);
       // *** Beg Chg *** 02/12/14 ***********************************
       // ***If CMjrSt=LGjSt And (LGsts='SN' or LGsts='EX');**********
             If CMjrSt=LGjSt And LGtset='148' And LGmtc='00'
                And (LGsts='SN' or LGsts='EX');
       // *** End Chg *** 02/12/14 ***********************************
                 exitReason='EDI has already been sent or extracted.';
                 Return *On;
             EndIf;
             ReadE keyWMLGL plg00;
          EndDo;
       EndIf;

       Return *Off;

      /end-free

     PExitEdi148       e
    P*====================================================================
    P* ExitEdiA49 - Determine if EDI A49 Processing Should End
    P*
    P*  ExitEdiA49  - returns Exit
    P*   claimRrn   - accepts the Claim RRN                       (required)
     P*   exitReason - exit Reason
    P*====================================================================
     PExitEdiA49       b                   export
    P*
     DExitEdiA49       PI              n
     D claimRrnIn                     9  0 const
     D exitReason                    80

      /free

       ExitReason=' ';
       Chain claimRrnIn pcm0p;

       // Is Group Enrolled into EDI?
       tranSetId=' ';
       Chain keyWMFNL pfn00;
       If %Found(WMFNL) And FNedi='Y';
          exitReason='Group not enrolled in EDI.';
          Return *On;
       EndIf;

       Return *Off;

      /end-free

     PExitEdiA49       e
    P*====================================================================
    P* ShowEdi148 - Determine if EDI 148 Screen Should Display
    P*
    P*  ShowEdi148  - returns Display
    P*   claimRrn   - accepts the Claim RRN                       (required)
    P*====================================================================
     PShowEdi148       b                   export
    P*
     DShowEdi148       PI              n
     D claimRrnIn                     9  0 const

     D showScreen      s               n   Inz(*On)

      /free

       Chain claimRrnIn pcm0p;

       // Show If State Claim # or if TX there must be a comp pmt
       If (CMjrSt<>'TX' And CMacl#<>' ') Or (CMjrSt='TX' And CMacl#<>' ' And
           HasCompPmts(claimRrnIn)=*On);
           Return *On;
       EndIf;

       // Retrieve log Record
       Chain keyWMLGL plg00;
       If %Found(WMLGL);
          DoU %Eof(WMLGL);
             If LGtSet='148' And LGsts<>'EC' And LGsts<>'EI' And LGsts<>'TR';
                 Return *On;
             EndIf;
             If LGtSet='148' And LGmtc='04' And LGsts<>'TR';
                 Return *On;
             EndIf;
             If LGtSet='A49' And LGmtc<>'04' And LGsts<>'TR';
                 Return *On;
             EndIf;
             ReadE keyWMLGL plg00;
          EndDo;
       EndIf;

       Return *Off;

      /end-free

     PShowEdi148       e
    P*====================================================================
    P* HasCompPmts - Calculate Claim Payments
    P*
    P*  HasCompPmts - returns if there are comp payments
    P*   claimRrn   - accepts the Claim RRN                       (required)
    P*====================================================================
     PHasCompPmts      b
    P*
     DHasCompPmts      PI              n
     D claimRrnIn                     9  0 const

     D i               s              2  0

      /free
       Chain claimRrnIn pcm0p;

       For i=1 to 10;
          key=rsvCode(i);
          Chain keyWTABL prt00;
          If rttype='C' and rsvPaid(i)>0;
             Return *On;
          EndIf;
       EndFor;

       Return *Off;

      /end-free

     PHasCompPmts      e
       //*** Beg Add *** 02/17/22 ***************************************
    p*====================================================================
    p* EdiScrCtrl - Return field screen control information for EDI fields
    p*
    p*====================================================================
     pEdiScrCtrl       b                   export
     dEdiScrCtrl       PI
    p*
     d inCompany                      3  0
     d inJurState                     2
     d inTranId                       3
     d inFormatId                     3
     d outD433                        1
     d outD440                        1
     d outD441                        1
     d outD441D                       7  0
     d outD442                        1
     d outRtnCode                     1
       Chain (inCompany:inJurState:inTranId:inFormatId) pix00;
       If %Found(WMIXL);
          outD433  = IXd433;
          outD440  = IXd440;
          outD441  = IXd441;
          outD441D = IXd441D;
          outD442  = IXd442;
          outRtnCode = '0';
       Else;
          outRtnCode = '1';
       EndIf;
     pEdiScrCtrl       e
       //*** End Add *** 02/17/22 ***************************************
