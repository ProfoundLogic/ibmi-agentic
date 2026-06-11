     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bndDir('WTBNDDIR')
     *========================================================================
     *
     * FAVPAYOUT - Managed Care E-Pay Processing
     *
      *=====================================================================
     * Created on 09/05/2020 by c:62 for Project WC2749
     *========================================================================
     * Revisions:
     *
     * 04/20/21 C:062 Support: 95603(133)
     *                -Don't report negative eobs
     *                -If address info is missing get it from other locations
     *
     * 04/19/21 C:079 Support: 90/95537
     *                Add Bank Code to API
     *
     *========================================================================
     FFAEAL1    IF   E           K DISK
     FFAEOL4    IF   E           K DISK    Rename(peo01:peo04) infds(dbfds)
     FWMCML1    IF   E           K DISK
     FWMEML     IF   E           K DISK    Rename(pem01:pem00)
     FWTGTL     IF   E           K DISK    Rename(pgt01:pgt00)
     FFMPDL     IF   E           K DISK    Rename(pPD01:pPD00)

     d inCompany#      s                   Like(EAco#)
     d inSubscriber    s                   Like(EAsub#)
     d inGroup         s                   Like(EAfnd)
     d inYear          s                   Like(EAfYr)
     d inCase          s                   Like(EAcase)
     d inEob#          s                   Like(EAeob#)
     d inRecon         s                   Like(EArcn)

     d account#        s             20
     d vendor          s              5
     d payerID         s             50
     d epay#           s              7  0

     d @@call          s              1
     d @@co            s              3
     d @@fnd           s              3
     d @@appl          s              2
     d @@srdt          s              8
     d @@acct          s             20
     d @@hdcy          s              1
     d @@chkq          s             10
     d @@ftyp          s             10
     d @@rtcd          s              1
     d @@efdt          s              7
     d @@cpyq          s             10
     d @@cktp          s             10
     d @@coid          s             20
     d @@eftc          s              1

     d returncode      s              1
     d chrCompany#     s              3
     d chrWcmsGroup    s              3
     d chrSubscriber   s              5
     d chrWcmcGroup    s              3

     d wcmsGroup       s                   Like(EAfnd)
     d wcmcSub#        s                   Like(EAsub#)
     d wcmcGroup       s                   Like(EAfnd)

     D* Claims
     D inClaim#        s             20
     D inMemberName    s             50
     D inPolicyNumber  s            100
     D inAccidentDate  s             10
     D inAdjusterName  s             50
     D inPaymentPrd    s             25
     D inWeeksDays     s              6
     D inPV1Desc       s             40
     D inRsvDesc       s             40
     D inMemo1         s             40
     D inMemo2         s             40
     D inMemo3         s             40
     D inMemo4         s             40

     D* Payee Fields
     D inPayeeType     s              1
     D inPayeeName     s            100
     D inPayeeAdr1     s             40
     D inPayeeAdr2     s             40
     D inPayeeAdr3     s             40
     D inPayeeCity     s             50
     D inPayeeState    s              3
     D inPayeeCntry    s              3
     D inPayeeZip      s             15
     D inPayeeFein     s              9

     D* Payment Fields
     D inPaymentID     s            100
     D inClientRefID   s             16
     D inPayerID       s             50
     D inPayerTime     s             26
     D inPaymentAmt    s             18
     D inPaymentCurr   s              3
     D inRemitMethod   s             10
     D inJurState      s              3

     d faeopRRN        s              9
     d adjState        s              2
     d docPath         s            255
     d documentID      s             50

     d format          s                   Like(GTcf01)

     D dbfds           ds
     D dbrrn                 397    400b 0

     d/copy sprCompany
     d/copy sprDate
     d/copy wprUsrPrms
     d/copy wprEPay
     d/copy wprBank
     d/copy wprTable

     d GenerateEOB     PR                  ExtPgm('F0346C')
     d  faeopRRN                      9
     d  adjState                      2
     d  docPath                     255

     d SubGrpXlate     PR                  ExtPgm('FA700R')
     d  returncode                    1
     d  chrCompany#                   3
     d  chrWcmsGroup                  3
     d  chrSubscriber                 5
     d  chrWcmcGroup                  3

     C     *ENTRY        PLIST
     C                   PARM                    inCompany#
     C                   PARM                    inSubscriber
     C                   PARM                    inGroup
     C                   PARM                    inYear
     C                   PARM                    inCase
     C                   PARM                    inEob#
     C                   PARM                    inRecon

      /free

       // Translate Subscriber and Group
       returncode = ' ';
       chrCompany#   = %EditC(inCompany#:'X');
       If inSubscriber = 0;  // This is coming from WCMS
          chrWcmsGroup  = %EditC(inGroup:'X');
          chrSubscriber = ' ';
          chrWcmcGroup  = ' ';
       Else;
          chrWcmsGroup  = ' ';
          chrSubscriber = %EditC(inSubscriber:'X');
          chrWcmcGroup  = %EditC(inGroup:'X');
       EndIf;

       SubGrpXlate(returnCode:chrCompany#:chrWcmsGroup:
        chrSubscriber:chrWcmcGroup);

       wcmsGroup = %Dec(chrWcmsGroup:3:0);
       wcmcSub#  = %Dec(chrSubscriber:5:0);
       wcmcGroup = %Dec(chrWcmcGroup:3:0);

       // Access Eob Header file
       Chain (inCompany#:wcmcSub#:wcmcGroup:inYear:inCase:inEob#) peo04;

       // Access Eob Header Amount Transaction file
       Chain (inCompany#:wcmcSub#:wcmcGroup:inYear:inCase:inEob#:inRecon)
        pea01;

       // Access WCMS Claim Master
       Chain (inCompany#:wcmsGroup:inYear:inCase) pcm01;

       // Access WCMS Employer Master
       Chain (inCompany#:wcmsGroup:CMemp#:CMdiv) pem00;

       // Get the account#
       @@co   = %EditC(EAco#:'X');
       @@fnd  = chrWcmsGroup;
       @@srDt = %EditC(DateTo8(systemDate()):'X');
       @@appl = 'WC';
       @@acct = *Blanks;
       @@call = *Blanks;
       @@hdcy = *Blanks;
       @@chkq = *Blanks;
       @@ftyp = *Blanks;
       @@rtcd = *Blanks;
       @@efdt = *Zeros ;
       @@cpyq = *Zeros ;
       @@cktp = *Blanks;
       @@coid = *Blanks;
       @@eftc = 'V'    ;

       account# = GetBankAcc(@@co : @@fnd: @@appl: @@srdt: @@call:
                  @@hdcy: @@chkq: @@ftyp: @@efdt: @@cpyq:
                  @@cktp: @@eftc: @@coid: @@rtcd);

       // Get the Vendor
       //*** Beg Chg *** 04/19/21 ******************************************
       //***vendor = GetEobEpay(EAco#:'WC':EAfnd:account#:
       //*** DateTo7(systemDate()));
       vendor = GetEobEpay(EAco#:'WC':EAfnd:account#:
        DateTo7(systemDate()):@@CkTp);
       //*** End Chg *** 04/19/21 ******************************************

       // Get the Payer ID
       //*** Beg Chg *** 04/19/21 ******************************************
       //***payerID = GetPayerID(EAco#:'WC':EAfnd:account#:
       //*** DateTo7(systemDate()));
       payerID = GetPayerID(EAco#:'WC':EAfnd:account#:
        DateTo7(systemDate()):@@CkTp);
       //*** End Chg *** 04/19/21 ******************************************

       Select;
       When vendor = 'VPAY' And EApaid = 0;
          ExSr WritePaymentRcd;
          ExSr WritePayeeRcd;
          ExSr WriteClaimRcd;
       EndSl;

       *inLR = *On;
       //*==================================================================
       //* Print Eob and convert to PDF
       //*==================================================================
       BegSr PrintEob;

          // EOB RRN
          faeopRRN = %EditC(dbrrn:'X');

          // Create the path statement
          docPath = %Trim(GetEPayOutFolderForDocuments(vendor:
                DateTo7(systemDate()))) + '/' + documentID;

          // Create the EOB PDF
          GenerateEOB(faeopRRN:EOaSt:docPath);

       EndSr;
       //*==================================================================
       //* WriteClaimRcd
       //*==================================================================
       BegSr WriteClaimRcd;

          inClaim# = %EditC(CMfnd:'X') + %EditC(CMfYr:'X') +
                     %EditC(CMcase:'X');
          inMemberName   = %Trim(CMfsn) + ' ' + CMlsn;
          inPolicyNumber = %EditC(CMfnd:'X') + %EditC(CMemp#:'X') +
                           %EditC(CMfYr:'X') + ': ' +  EMname;
          inAccidentDate = alphaUSA(cvtToDate(CMacDt));
          inAdjusterName = GetUsrName(GetUserId(CMclex));
          inPaymentPrd   = ' ';
          inWeeksDays    = ' ';
          inPV1Desc      = ' ';
          inRsvDesc      = ' ';
          inMemo1        = ' ';
          inMemo2        = ' ';
          inMemo3        = ' ';
          inMemo4        = ' ';

          CrtVPayClaimRecord(inPaymentID:inClaim#:inMemberName:
             inPolicyNumber:inAccidentDate:inAdjusterName:inPaymentPrd:
             inWeeksDays:inPV1Desc:inRsvDesc:inMemo1:inMemo2:inMemo3:
             inMemo4);

       EndSr;
       //*==================================================================
       //* WritePayeeRcd
       //*==================================================================
       BegSr WritePayeeRcd;

          inPayeeType = 'B';

          If EApNam = ' ';
             If EOpnam = ' ';
                Chain (EAfein:EAmod) ppd00;
                If PDdba1 = 'Y';
                   inPayeeName  = PDadr1;
                   inPayeeAdr1  = PDadr2;
                   inPayeeAdr2  = ' ';
                Else;
                   If PDfsn = ' ';
                      inPayeeName  = PDname;
                   Else;
                      inPayeeName  = %Trim(PDfsn) + ' ' + PDname;
                   EndIf;
                   inPayeeAdr1  = PDadr1;
                   inPayeeAdr2  = PDadr2;
                Endif;

                inPayeeAdr3  = PDadr3;
                inPayeeCity  = PDcty;
                inPayeeState = PDst;
                inPayeeZip   = %EditC(PDzip:'X');

             Else;
                inPayeeName  = EOpNam;
                inPayeeAdr1  = EOadr1;
                inPayeeAdr2  = EOadr2;
                inPayeeAdr3  = EOadr3;
                inPayeeCity  = EOcty;
                inPayeeState = EOst;
                inPayeeZip   = %EditC(EOzip:'X');
             Endif;
          Else;
             inPayeeName  = EApNam;
             inPayeeAdr1  = EAadr1;
             inPayeeAdr2  = EAadr2;
             inPayeeAdr3  = EAadr3;
             inPayeeCity  = EActy;
             inPayeeState = EAst;
             inPayeeZip   = %EditC(EAzip:'X');
          Endif;

          inPayeeCntry = CMctry;
          inPayeeFein  = %EditC(EAfein:'X');

          CrtVPayPayeeRecord(inPaymentID:inPayeeType:inPayeeName:
           inPayeeAdr1:inPayeeAdr2:inPayeeAdr3:inPayeeCity:inPayeeState:
           inPayeeCntry:inPayeeZip:inPayeeFein);

       EndSr;
       //*==================================================================
       //* WritePaymentRcd
       //*==================================================================
       BegSr WritePaymentRcd;

          format = GetEpayFormat(vendor:EAtrDt);
          Chain (EAco#:'EPFM':format) pgt00;
          If %Found(wtgtl);
             inRemitMethod = GTcf01;
          Else;
             inRemitMethod = 'NONE';
          EndIf;

          ePay# = GetNextEPay#(CMco#:account#);

          Select;
          When inRemitMethod = 'PDF';
             inPaymentID = %EditC(EAeob#:'X') + %EditC(EArcn:'X') +
               %EditC(ePay#:'X');
             documentID  = %Trim(inPaymentID) + '.PDF';
             ExSr PrintEob;
          Other;
             inPaymentID = %EditC(CMfnd:'X') +
              %EditC(CMfYr:'X') + %EditC(CMcase:'X') +
              %EditC(ePay#:'X');
          EndSl;

          inClientRefID = 'EPAY# ' + %EditC(ePay#:'X');
          inPayerID     = payerID;
          inPayerTime   = %Subst(%EditC(dtMDYY(EAtrDt):'X'):5:4) + '-' +
                          %Subst(%EditC(dtMDYY(EAtrDt):'X'):1:2) + '-' +
                          %Subst(%EditC(dtMDYY(EAtrDt):'X'):3:2) + '-' +
                          %Subst(%EditC(EAtime:'X'):1:2) + '.' +
                          %Subst(%EditC(EAtime:'X'):3:2) + '.' +
                          %Subst(%EditC(EAtime:'X'):5:2);
          inPaymentAmt  = %EditC(EApaid:'P');
          inPaymentCurr = 'USD';
          inJurState    = CMjrSt;

          CrtVPayPaymentRecord(inPaymentID:inClientRefID:inPayerID:
           inPayerTime:inPaymentAmt:inPaymentCurr:inRemitMethod:
           inJurState);

       EndSr;
      /end-free
