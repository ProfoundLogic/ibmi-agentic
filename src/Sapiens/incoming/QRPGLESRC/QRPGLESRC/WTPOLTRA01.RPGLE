     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h**********************************************************************
     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h**********************************************************************
     h*
     h*  Description: Response program to serve Audit Tracking detail box
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTPOLTRACK
     h*  Calls to:
     h*
     h*  Created on 09/03/2012 by C:279 for Project Transform
     h**********************************************************************
     h* Revisions:
     h*
     h* 08/28/15 C:298 Support: 36615                                 *
     h*                Populate cancellation date in detail box even  *
   h*                if in the future.                              *
     h*
     h**********************************************************************
      * file
     fwmaup     if   e             Disk    reName(pau01:pau0p)
     fwttal     if   e           k disk    rename(pta01:pta00)
     fwtacl     if   e           k disk    rename(pac01:pac00)
     fwmeml     if   e           k disk    rename(pem01:pem00)
     fsmupl1    if   e           k disk    rename(pup01:pup01)
     fwdell     if   e           k disk    rename(pel01:pel00)
     fwdell1    if   e           k disk

      /copy sPrDate
      /copy sPrString
      /copy sPrAtrium
      /copy wPrUsrPrms
      /copy wPrTable

     d* Get Information
     d GetInfo         pr

     d* Clear Detail box
     d ClearDtlBox     pr

     d*  GET Agent/Producer Information
     d GetAgnPrInfo    pr                  ExtPgm('WA002R')
     d ##co#                               like(##co#)
     d ##fnd                               like(##fnd)
     d ##emp#                              like(##emp#)
     d ##div                               like(##div)
     d ##fyr                               like(##fyr)
     d ##agt#                              like(##agt#)
     d ##anam#                             like(##anam#)
     d ##prod#                             like(##prod#)
     d ##pnam#                             like(##pnam#)

      * *Entry
     dAuditTracking01  pr                  ExtPgm('WTPOLTRA01')
     d                                9  0
     d                            32767
     d                                1

     dAuditTracking01  pi
     d inRRN                          9  0
     d outJSON                    32767
     d RtnCode                        1

     d ##co#           s                   like(AUco#)
     d ##fnd           s                   like(AUfnd)
     d ##emp#          s                   like(AUemp#)
     d ##div           s                   like(AUdiv)
     d ##typ           s                   like(TAtypa)
     d ##fyr           s                   like(AUfyr)
     d ##agt#          s              5  0
     d ##anam#         s             60
     d ##prod#         s              5  0
     d ##pnam#         s             60
     d intCnt          s              3  0
     d indate          s              7  0
     d strtype         s             30    inz(*blanks)
     d strAct          s             10    inz(*blanks)
     d strRsgn         s              5    inz(*blanks)
     d strRefl         s              5    inz(*blanks)
     d strRvfl         s              5    inz(*blanks)
     d strCnFl         s              5    inz(*blanks)
     d strCoName       s             40    inz(*blanks)
     d strUwtr         s             40    inz(*blanks)
     d strRep          s             40    inz(*blanks)
     d strArvw         s             40    inz(*blanks)
     d strPsrv         s             40    inz(*blanks)
     d strLrep         s             40    inz(*blanks)
     d strpacc         s             40    inz(*blanks)
     d strAmgr         s             40    inz(*blanks)
     d strCnTyp        s              3    inz(*blanks)
     d x1              s              1  0

     d                 ds
     d CX                      1     35  0
     d                                     dim(5)
     d EMcad1                  1      7  0
     d EMcad2                  8     14  0
     d EMcad3                 15     21  0
     d EMcad4                 22     28  0
     d EMcad5                 29     35  0

     d                 ds
     d EF                      1     35  0
     d                                     dim(5)
     d EMefd1                  1      7  0
     d EMefd2                  8     14  0
     d EMefd3                 15     21  0
     d EMefd4                 22     28  0
     d EMefd5                 29     35  0

     c     keyWMEML      Klist
     c                   Kfld                    ##co#
     c                   Kfld                    ##fnd
     c                   Kfld                    ##emp#
     c                   Kfld                    ##div

     c     keyWDELL      klist
     c                   kfld                    ##co#
     c                   kfld                    ##fnd
     c                   kfld                    ##emp#
     c                   kfld                    ##div
     c                   kfld                    ##fyr

     c     keyWDELL1     klist
     c                   kfld                    ##co#
     c                   kfld                    ##fnd
     c                   kfld                    ##fyr
     c                   kfld                    ##emp#

      /free

       // initialize detail box
       ClearDtlBox();

       // Main process
       Chain inRRN pau0p;
       ##co#  = AUco#;
       ##fnd  = AUfnd;
       ##emp# = AUemp#;
       ##div  = AUdiv;
       ##typ  = AUtypa;
       ##fyr  = AUfyr;

       clear indate;
       Chain keyWMEML pem00;
       Chain keyWDELL pel00;
       If %found(WDELL);
          //Find policy effective date
          x1 = 1;
          x1 = %lookup(*zero:EF);

          If EF(x1) <> *zero;
             x1 = 5;
          Else;
             x1 = x1 - 1;
          Endif;

          Dow x1 > 0;
              If EF(x1) > ELpfdt;
                 x1 = x1 - 1;
              Else;
                 leave;
              Endif;
          Enddo;

          // Determine if cancellation date exists, set canx flag
          If x1 = 0;
             strCnTyp = 'No';
          Else;
             //***** Beg Chg ***** 08/28/15 ****************************************
             //*****If CX(x1) = *zeros or CX(x1) > dtCYMD(UDATE);
             If CX(x1) = *zeros;
             //***** End Chg ***** 08/28/15 ****************************************
                strCnTyp = 'No';
             Else;
                strCnTyp = 'Yes';
                indate = CX(x1);
             Endif;
          Endif;
       Endif;

       GetInfo();

       // set key values
       outJSON = ' ';
       BuildJSON(outJSON :'OutDtlAudTyp'  :%trim(strtype));
       BuildJSON(outJSON :'OutDtlAudReqDt':%EditW(dtMDYY(AUrqdt):'  /  /    '));
       BuildJSON(outJSON :'OutDtlAudRecDt':%EditW(dtMDYY(AUrcvd):'  /  /    '));
       BuildJSON(outJSON :'OutDtlAudAtmDt':%EditW(dtMDYY(AUatdt):'  /  /    '));
       BuildJSON(outJSON :'OutDtlAudCalDt':%EditW(dtMDYY(AUacdt):'  /  /    '));
       BuildJSON(outJSON :'OutDtlAudPrtDt':%EditW(dtMDYY(AUapdt):'  /  /    '));
       BuildJSON(outJSON :'OutDtlFnlPrtDt':%EditW(dtMDYY(AUprdt):'  /  /    '));
       BuildJSON(outJSON :'OutDtlAudMlDt' :%EditW(dtMDYY(AUmail):'  /  /    '));
       BuildJSON(outJSON :'OutDtlAudCntDt':%EditW(dtMDYY(AUcntd):'  /  /    '));
       BuildJSON(outJSON :'OutDtlAudCnDt' :%EditW(dtMDYY(indate):'  /  /    '));
       BuildJSON(outJSON :'OutDtlAudCnTyp'   : %trim(strCnTyp));
       BuildJSON(outJSON :'OutDtlAudActSts'  : %trim(strAct));
       BuildJSON(outJSON :'OutDtlAudReAsnSts': %trim(strRsgn));
       BuildJSON(outJSON :'OutDtlAudRevSts'  : %trim(strRefl));
       BuildJSON(outJSON :'OutDtlAudReAudSts': %trim(strRvfl));
       BuildJSON(outJSON :'OutDtlAudCntSts'  : %trim(strCnFl));
       BuildJSON(outJSON :'OutDtlAudCoAsn'   : %trim(strCoName));
       BuildJSON(outJSON :'OutDtlAudUnwAsn'  : %trim(strUwtr));
       BuildJSON(outJSON :'OutDtlAudRepAsn'  : %trim(strRep));
       BuildJSON(outJSON :'OutDtlAudRevAsn'  : %trim(strArvw));
       BuildJSON(outJSON :'OutDtlAudPolAsn'  : %trim(strPsrv));
       BuildJSON(outJSON :'OutDtlAudLsCntAsn': %trim(strLrep));
       BuildJSON(outJSON :'OutDtlAudPrmAccAsn': %trim(strPacc));
       BuildJSON(outJSON :'OutDtlAudMngAsn'   : %trim(strAmgr));
       BuildJSON(outJSON :'OutDtlAudEmpNmInfo': %trim(EMname));
       BuildJSON(outJSON :'OutDtlAudPolFr':%EditW(dtMDYY(ELpfdt):'  /  /    '));
       BuildJSON(outJSON :'OutDtlAudPolTo':%EditW(dtMDYY(ELptdt):'  /  /    '));

       // get div count
       Setll keyWDELL1 pel01;
       Reade keyWDELL1 pel01;
       Dow not %eof(WDELL1);
          intCnt  = intCnt + 1;
          Reade keyWDELL1 pel01;
       Enddo;

       // GET Agent/Producer Information
       GetAgnPrInfo(##co#:##fnd:##emp#:##div:##fyr:##agt#:##anam#:
        ##prod#:##pnam#);

       BuildJSON(outJSON :'OutDtlAudDivInfo'  : %EditC(intCnt:'X'));
       BuildJSON(outJSON :'OutDtlAudAgnInfo'  : %EditC(##agt#:'X'));
       BuildJSON(outJSON :'OutDtlAudAgnNmInfo': %trim(##anam#));
       BuildJSON(outJSON :'OutDtlAudPrdInfo'  : %EditC(##prod#:'X'));
       BuildJSON(outJSON :'OutDtlAudPrdNmInfo': %trim(##pnam#));

       RtnCode = ' ';
       Return;
      /end-free
       //===============================================================
       // GetInfo - Retrieve information
       //===============================================================
     P GetInfo         B
     d GetInfo         PI
     c     keyWTTAL      Klist
     c                   Kfld                    ##co#
     c                   Kfld                    ##typ

     c     keyWTACL      klist
     c                   kfld                    ##co#
     c                   kfld                    AUaco

     c     keySMUPL      klist
     c                   kfld                    ##staf            3

      /free

       ##co#  = AUco#;
       ##fnd  = AUfnd;
       ##emp# = AUemp#;
       ##div  = AUdiv;
       ##typ  = AUtypa;
       ##fyr  = AUfyr;

       Chain keyWTTAL pta00;
       If %found(WTTAL);
          strtype = %trim(TAtypd);
       Endif;

       If AUstat = 'A';
          strAct = 'Active';
       Else;
          strAct = 'Inactive';
       Endif;

       If AUrsgn = 'Y';
          strRsgn = 'Yes';
       Else;
          strRsgn = 'No';
       Endif;

       If AUrefl = 'Y';
          strRefl = 'Yes';
       Else;
          strRefl = 'No';
       Endif;

       If AUrvfl = 'Y';
          strRvfl = 'Yes';
       Else;
          strRvfl = 'No';
       Endif;

       If AUcnfl = 'Y';
          strcnfl = 'Yes';
       Else;
          strcnfl = 'No';
       Endif;

       If AUaco <> ' ';
          Chain keyWTACL pac00;
          If %found(WTACL);
             strCoName = AUaco + ' - ' + ACname;
          Endif;
       Endif;

       clear ##staf;
       If EMuwtr <> ' ';
          ##staf = EMuwtr;
          Chain keySMUPL pup01;
          If %found(SMUPL1);
             strUwtr = UPusnm;
          Endif;
       Endif;

       clear ##staf;
       If AUrep <> ' ';
          ##staf = AUrep;
          Chain keySMUPL pup01;
          If %found(SMUPL1);
             strRep = AUrep + ' - ' + UPusnm;
          Endif;
       Endif;

       clear ##staf;
       If EMarvw <> ' ';
          ##staf = EMarvw;
          Chain keySMUPL pup01;
          If %found(SMUPL1);
             strarvw = UPusnm;
          Endif;
       Endif;

       clear ##staf;
       If EMpsrv <> ' ';
          ##staf = EMpsrv;
          Chain keySMUPL pup01;
          If %found(SMUPL1);
             strpsrv = UPusnm;
          Endif;
       Endif;

       clear ##staf;
       If EMlrep <> ' ';
          ##staf = EMlrep;
          Chain keySMUPL pup01;
          If %found(SMUPL1);
             strlrep = UPusnm;
          Endif;
       Endif;

       clear ##staf;
       If EMpacc <> ' ';
          ##staf = EMpacc;
          Chain keySMUPL pup01;
          If %found(SMUPL1);
             strpacc = UPusnm;
          Endif;
       Endif;

       clear ##staf;
       If EMamgr <> ' ';
          ##staf = EMamgr;
          Chain keySMUPL pup01;
          If %found(SMUPL1);
             stramgr = UPusnm;
          Endif;
       Endif;

      /end-free

     P GetInfo         E
       //==============================================================
       // clearDtlBox - initialize detail box fields
       //==============================================================
     P clearDtlBox     B
     D clearDtlBox     PI

      /free

       clear intCnt;
       clear strtype;
       clear strAct;
       clear strRsgn;
       clear strRefl;
       clear strRvfl;
       clear strcnfl;
       clear strCoName;
       clear strUwtr;
       clear strRep;
       clear strarvw;
       clear strpsrv;
       clear strlrep;
       clear strpacc;
       clear stramgr;
       clear strCnTyp;

      /end-Free
     P clearDtlBox     E
