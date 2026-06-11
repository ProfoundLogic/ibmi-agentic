     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H*****************************************************************
     H/TITLE W7014R - E-Pay Group Check Processing
     H DftActGrp(*NO) ActGrp(*caller)
     H Option(*nodebugio:*srcstmt)
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*                                                               *
     H* Program Description: E-Pay Group Check Processing             *
     H*                                                               *
     H*****************************************************************
     H* Created on 10/12/2020 by c:62 for Project WC2749              *
     H*****************************************************************
     H* Revisions:                                                    *
    H*                                                               *
    H* 04/19/21 C:79 Support: 90/95537                              *
    H*               Add Bank Code to API                           *
     H*                                                               *
     H* 03/16/21 c:62 Project WC2749(95007)                           *
     H*               Use batch date not current date                 *
     H*                                                               *
     H*****************************************************************
     Fwar3l4    if   e           k disk    Rename(prp01:prp04)
     Fwdpal10   if   e           k disk    Rename(ppa01:ppa10)
     Fwdrpl3    if   e           k disk    Rename(prp01:prp03)
     Fwmcdl     if   e           k disk    Rename(pcd01:pcd00)
     Fwmcml1    if   e           k disk
     Fwmeml     if   e           k disk    Rename(pem01:pem00)
     Fwmpdl     if   e           k disk    Rename(ppd01:ppd00)
     Fwtgtl     if   e           k disk    Rename(pgt01:pgt00)
    F*
    D*---------------------------------------------------------------------
    D* Prototypes
    D*---------------------------------------------------------------------
      /copy sprDate
      /copy wprBank
      /copy wprEPay
      /copy wprTable
      /copy wprUsrPrms
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------
     D currentDate     s                   like(PAtrDt)                         Batch Date
     d vendor          s              5
     d payerID         s             50
     D inPaymentID     s            100
     d format          s                   Like(GTcf01)
     d inDate          s              8
     d decDate7        s                   Like(PAtrDt)
     d decDate8        s              8  0

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
     D inClientRefID   s             16
     D inPayerID       s             50
     D inPayerTime     s             26
     D inPaymentAmt    s             18
     D inPaymentCurr   s              3
     D inRemitMethod   s             10
     D inJurState      s              3
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
     D psds          esds                  extname(szq1p)
    D*
     c     *entry        plist
     c                   parm                    inDate

      /free
       // Get Batch date and convert to CYMD
       decDate8 = %Dec(inDate:8:0);
       decDate7 = dtCYMD(decDate8);

       // Get the system date
       currentDate = DateTo7(systemDate());

       // Read Thru Group Check File (WAR3P)
       Read prp04;
       DoW Not %Eof(war3l4);

          // Only Process V-Pay
          If RPeftC = 'V';
             // Write Payment Record (VR001)
             ExSr WritePaymentRcd;
             // Write Payee Record   (VR002)
             ExSr WritePayeeRcd;
             // Write Claim Records  (VR003)
             ExSr WriteClaimRcd;
          Endif;

          Read prp04;
       EndDo;

       *inLR = *on;

       //*==================================================================
       //* WriteClaimRcd
       //*==================================================================
       BegSr WriteClaimRcd;

          Chain (RPco#:RPckTp:RPfnd:RPfein:RPmod:
                 RPeftC:decDate7) ppa10;
          If %Found(wdpal10);
             DoU %Eof(wdpal10);
                If PApgTp = 'G';

                   // Access Payment Demographic Record
                   Chain (PAco#:PAfnd:PAfyr:PAcase:PAseq#) prp03;
                   If Not %Found(wdrpl3);
                      Clear prp03;
                   EndIf;

                   // Access Claim Record
                   Chain (PAco#:PAfnd:PAfyr:PAcase) pcm01;

                   inClaim# = %EditC(CMfnd:'X') + %EditC(CMfYr:'X') +
                              %EditC(CMcase:'X');

                   Chain (PAco#:PAfnd:CMemp#:CMdiv) pem00;
                   inMemberName   = %Trim(CMfsn) + ' ' + CMlsn;
                   inPolicyNumber = %EditC(CMfnd:'X') + %EditC(CMemp#:'X') +
                                    %EditC(CMfYr:'X') + ': ' + EMname;
                   inAccidentDate = alphaUSA(cvtToDate(CMacDt));
                   inAdjusterName = GetUsrName(GetUserId(CMclex));
                   inPaymentPrd   = alphaUSA(cvtToDate(PAfrDt)) + '-' +
                                    alphaUSA(cvtToDate(PAtoDt));
                   inWeeksDays    = %Trim(%EditW(PAwks:' 0 ')) + '/' +
                                    %EditC(PAdays:'X');
                   inPV1Desc      = BR2CodeDs(PAco#:PAbr2);
                   inRsvDesc      = ResTypCdDs(PAco#:PAreCd);
                   inMemo1        = RemarkCdDs(PAco#:RPrmk1);
                   inMemo2        = RemarkCdDs(PAco#:RPrmk2);
                   inMemo3        = RemarkCdDs(PAco#:RPrmk3);
                   inMemo4        = RPrmDs;

                   CrtVPayClaimRecord(inPaymentID:inClaim#:inMemberName:
                      inPolicyNumber:inAccidentDate:inAdjusterName:
                      inPaymentPrd:inWeeksDays:inPV1Desc:inRsvDesc:inMemo1:
                      inMemo2:inMemo3:inMemo4);
                EndIf;

                ReadE (RPco#:RPckTp:RPfnd:RPfein:RPmod:
                       RPeftC:decDate7) ppa10;
             EndDo;
          EndIf;

       EndSr;

       //*==================================================================
       //* WritePayeeRcd
       //*==================================================================
       BegSr WritePayeeRcd;

          If RPfein = 0;
             inPayeeType = 'C';
             Chain CMss# pcd00;
             inPayeeAdr3  = CDadr3;
             inPayeeFein  = CMss#;
          Else;
             inPayeeType  = 'B';
             Chain (RPfein:RPmod) ppd00;
             inPayeeAdr3  = PDadr3;
             inPayeeFein  = %EditC(RPfein:'X');
          Endif;

          inPayeeName  = RPpNam;
          inPayeeAdr1  = RPadr1;
          inPayeeAdr2  = RPadr2;
          inPayeeCity  = RPcty;
          inPayeeState = RPst;
          inPayeeCntry = CMctry;
          inPayeeZip   = %EditC(RPzip:'X');

          CrtVPayPayeeRecord(inPaymentID:inPayeeType:inPayeeName:
            inPayeeAdr1:inPayeeAdr2:inPayeeAdr3:inPayeeCity:inPayeeState:
            inPayeeCntry:inPayeeZip:inPayeeFein);

       EndSr;

       //*==================================================================
       //* WritePaymentRcd
       //*==================================================================
       BegSr WritePaymentRcd;

          // Get the Vendor ID
          vendor = GetEpayVendor(RPco#:RPacct);

          // Get the Payer ID
        //*** Beg Chg *** 04/19/21 ***************************************
        //***  payerID = GetPayerID(RPco#:'WC':RPfnd:RPacct:
        //***   DateTo7(systemDate()));
          payerID = GetPayerID(RPco#:'WC':RPfnd:RPacct:
           DateTo7(systemDate()):RPckTp);
        //*** End Chg *** 04/19/21 ***************************************

          inRemitMethod = 'NONE';

          inPaymentID = %EditC(RPfnd:'X') + %EditC(RPfYr:'X') +
                        %EditC(RPcase:'X') + %EditC(RPchk#:'X');

          inClientRefID = 'EPAY# ' + %EditC(RPchk#:'X');
          inPayerID     = payerID;
          inPayerTime   = %Subst(%EditC(dtMDYY(currentDate):'X'):5:4) + '-' +
                          %Subst(%EditC(dtMDYY(currentDate):'X'):1:2) + '-' +
                          %Subst(%EditC(dtMDYY(currentDate):'X'):3:2) + '-' +
                          %Subst(%EditC(RPtime:'X'):1:2) + '.' +
                          %Subst(%EditC(PAtime:'X'):3:2) + '.' +
                          %Subst(%EditC(PAtime:'X'):5:2);
          inPaymentAmt  = %EditC(RPamt:'P');
          inPaymentCurr = 'USD';

          Chain (RPco#:RPfnd:RPfYr:RPcase) pcm01;
          If %Found(wmcml1);
             inJurState = CMjrSt;
          Else;
             inJurState = RPst;
          EndIf;

          CrtVPayPaymentRecord(inPaymentID:inClientRefID:inPayerID:
            inPayerTime:inPaymentAmt:inPaymentCurr:inRemitMethod:
            inJurState);

       EndSr;
      /end-free
