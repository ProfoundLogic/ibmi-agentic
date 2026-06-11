     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H nomain
     *========================================================================
     *
     * WPIBANK - Procedures for Bank information
     *
     * Created   : 04/17/12
     * Programmer: C:282
     * Project   : WC2004
     *
     *========================================================================
     * REVISIONS:
      *
      * 08/04/25 C:079 Support: 119946 (Sapiens)
      *                Change WMP3L to User Open and override the member
      *
      * 04/24/25 C:79  Support: 118909 (Sapiens)
      *                Delete override of member Archive
      *
      * 04/05/23 C:65  Support: 107397 (Prescient)
      *                Clear return indicator on 3rd party pay procedure.
      *
      * 08/24/21 C:079 Project: WC1986
      *                Testing WC2663 had a record lock on PCK00.  Changed
      *                to read without locking.
      *
      * 04/06/21 C:136 Project: WC1986
      *                Add Has3rdPartyAcct procedure.
      *                Add parms to GetEftStatus.
      *                Add RequiresPreNote procedure.
      *                Add procedure to archive changes to WMP3P
      *
      * 08/20/20 C:062 Project: WC2749
      *                -Get Next EPay #
      *                -Get EPay Vendor
      *                -Check to see EPay Requests Exist
      *
      * 07/30/15 C:136 Project: WC2337
      *                Add subprocedure for checking if EFT check
      *                requests or transactions exist
      *
      * 07/23/15 C:136 Support: 36153
      *                Added bank code to GetEftStatus procedure
      *                Also corrected the parm counts in it
      *
      * 05/27/15 C:062 Project: 114MRH
      *                Add function to return and update the eft#.
      *
      * 01/20/15 C:136 PHW32817 (TASB)
      *                Return new EFT flag to calling program
      *
      *      09/28/12  C:191 PROJECT: TRANSFORM
      *                      FIX GETEFTSTATUS TO RETURN RRN AND CHECK CORRECT PA
      *
      *      10/06/12  C:279 PROJECT: TRANSFORM
      *                      FIX GETEFTSTATUS TO RETURN RRN AFTER CHAEKING CORRE
      *                      PARM
     *========================================================================
     * Prototypes
     *========================================================================
     Fwsckl     uf   e           k Disk    Rename(pck01:pck00)
     Fwar2l2    if   e           k disk    rename(pr201:pr202) usropn
     Fwmacl23   if   e           k disk    rename(pac01:pac23) usropn
     Fwarpl12   if   e           k disk    rename(prp01:prp12) usropn
     Fwdpalv    if   e           k disk    rename(ppa01:ppalv) usropn
       //*** beg add ***** 04/06/21 ************************
       //*** Beg Chg *** 08/04/25 ******************************************
       //***Dcl-F wmp3l Keyed Rename(pp301:pp300) Infds(fdsWMP3P);
       Dcl-F wmp3l Keyed Rename(pp301:pp300) Infds(fdsWMP3P) UsrOpn;
       Dcl-F wmbil5 Keyed Rename(pbi01:pbi05);
       //*** End Chg *** 08/04/25 ******************************************
       Dcl-F wmp3p disk usage(*output) usropn;

       Dcl-Ds fdsWMP3P;
          rrnWMP3P Int(10:0) Pos(397);
       End-Ds;

       Dcl-Pr RunCommand ExtPgm('QCMDEXC');
        inCmd    char(2750) const;
        inCmdLen packed(15:5) const;
       End-Pr;

      /Copy wPrEmplr
       //*** end add ***** 04/06/21 ************************
      /copy SprDate
     D psds          esds                  ExtName(szq1p)
     *------------------------------------------------------------------------
     * GetBankAcc - Return Bank Account no.
     *------------------------------------------------------------------------
     P GetBankAcc      B                   EXPORT

     *** Procedure Interface ***
     d GetBankAcc      PI            20
     d inCo#                          3
     d inFnd                          3
     d inAppl                         2
     d inSrdt                         8
     d inCall                         1
     d inHdcy                         1
     d inChkq                        10
     d inFTYP                        10
     d inEfdt                         7
     d inCpyq                        10
     d inCktp                        10    options(*nopass)
     d inEftc                         1    options(*nopass)
     d outCoid                       20    options(*nopass)
     d outRtcd                        1    options(*nopass)
      *
      * Local Variable
     d strCo#          s              3
     d strFnd          s              3
     d strAppl         s              2
     d strSrdt         s              8
     d strAcct         s             20
     d strCall         s              1
     d strHdcy         s              1
     d strChkq         s             10
     d strFTYP         s             10
     d strRtcd         s              1
     d strEfdt         s              7
     d strCpyq         s             10
     d strCktp         s             10
     d strCoid         s             20
     d strEftc         s              1
      *
     d BankAccount     pr                  extPgm('WA023R')
     d strCo#                         3
     d strFnd                         3
     d strAppl                        2
     d strSrdt                        8
     d strAcct                       20
     d strCall                        1
     d strHdcy                        1
     d strChkq                       10
     d strFTYP                       10
     d strRtcd                        1
     d strEfdt                        7
     d strCpyq                       10
     d strCktp                       10    options(*nopass)
     d strCoid                       20    options(*nopass)
     d strEftc                        1    options(*nopass)
      *
      /free
       strCo#  = inCo#;                                                      //
       strFnd  = inFnd;                                                      //
       strAppl = inAppl;                                                     //
       strSrdt = inSrdt;                                                     //
       strCall = inCall;                                                     //
       strHdcy = inHdcy;                                                     //
       strChkq = inChkq;                                                     //
       strFTYP = inFTYP;                                                     //
       strEfdt = inEfdt;                                                     //
       strCpyq = inCpyq;                                                     //

       If %parms > 10;
        strCktp  = inCktp;                                                    //
       Endif;

       If %parms > 11;
        strEftc   = inEftc;                                                   //
       Endif;

       BankAccount(strCo#:strFnd:strAppl:strSrdt:strAcct:
                    strCall:strHdcy:strChkq:strFTYP:strRtcd:
                    strEfdt:strCpyq:strCKtp:strCoid:strEftc);

       outRtcd = strRtcd;
       outcoid = strCoid;
       inftyp  = strftyp;
       inHdcy  = strHdcy;
       inchkq  = strchkq;
       incpyq  = strcpyq;
       inEftc  = strEftc;

       Return strAcct;

      /end-free

     P GetBankAcc      E
     *------------------------------------------------------------------------
     * ValidBankCode - Return '0' for Valid bank Code and '1' for invalid
      *                 Bank Code.
     *------------------------------------------------------------------------
     P ValidBankCode   B                   EXPORT

     *** Procedure Interface ***
     d ValidBankCode   PI             1n
     d inCo#                          3
     d inFnd                          3
     d inFyr                          3
     d inCase                         7
     d inAppl                         2
     d inData                        75
     d inCktp                        10
     d inEftc                         1    options(*nopass)
      *
      * Local Variable
     d strMode         s              1
     d strCo#          s              3
     d strFnd          s              3
     d strFyr          s              3
     d strCase         s              7
     d strAppl         s              2
     d strData         s             75
     d strCktp         s             10
     d strRtcd         s              1
     d strEftc         s              1
      *
     d BankCode        pr                  extPgm('WA023R1')
     d strMode                        1
     d strCo#                         3
     d strFnd                         3
     d strFyr                         3
     d strCase                        7
     d strAppl                        2
     d strData                       75
     d strCktp                       10
     d strRtcd                        1
     d strEftc                        1    options(*nopass)
      *
      /free
       strMode   = '1';                                                      //
       strCo#    = inCo#;                                                    //
       strFnd    = inFnd;                                                    //
       strFyr    = inFyr;                                                    //
       strCase   = inCase;                                                   //
       strAppl   = inAppl;                                                   //
       strData   = inData;                                                   //
       strCktp   = inCktp;                                                   //
       strRtcd   = ' ';                                                      //

       If %parms > 7 ;
        strEftc   = inEftc;                                                  //
       Endif;

       BankCode(strMode:strCo#:strFnd:strFyr:strCase:
                 strAppl:strData:strCktp:strRtcd:strEftc);

       If strRtcd = ' ';
          Return '1';
       Else;
          Return '0';
       EndIf;

      /end-free

     P ValidBankCode   E
     *------------------------------------------------------------------------
     * GetBankCode - Bank Code will be return.
     *------------------------------------------------------------------------
     P GetBankCode     B                   EXPORT

     *** Procedure Interface ***
     d GetBankCode     PI            10
     d inCo#                          3
     d inFnd                          3
     d inFyr                          3
     d inCase                         7
     d inAppl                         2
     d inData                        75
     d inRtcd                         1
     d outEftc                        1    options(*nopass)
      *
      * Local Variable
     d strMode         s              1
     d strCo#          s              3
     d strFnd          s              3
     d strFyr          s              3
     d strCase         s              7
     d strAppl         s              2
     d strData         s             75
     d strCktp         s             10
     d strRtcd         s              1
     d strEftc         s              1
      *
     d RtvBankCode     pr                  extPgm('WA023R1')
     d strMode                        1
     d strCo#                         3
     d strFnd                         3
     d strFyr                         3
     d strCase                        7
     d strAppl                        2
     d strData                       75
     d strCktp                       10
     d strRtcd                        1
     d strEftc                        1    options(*nopass)
      *
      /free
       strMode   = '2';                                                      //
       strCo#    = inCo#;                                                    //
       strFnd    = inFnd;                                                    //
       strFyr    = inFyr;                                                    //
       strCase   = inCase;                                                   //
       strAppl   = inAppl;                                                   //
       strData   = inData;                                                   //
       strRtcd   = inRtcd;                                                   //

       If %parms > 7 ;
        strEftc   = outEftc;                                                  //
       Else;
        strEftc   = *blanks;                                                 //
       Endif;

       RtvBankCode(strMode:strCo#:strFnd:strFyr:strCase:
                    strAppl:strData:strCktp:strRtcd:strEftc);

        outEftc = strEftc;

       Return strCktp;

      /end-free

     P GetBankCode     E
     *------------------------------------------------------------------------
     * GetEFTStatus- EFT Status
     *------------------------------------------------------------------------
     P GetEFTStatus    B                   EXPORT

     *** Procedure Interface ***
     d GetEFTStatus    PI             1n
     d inCo#                          3
     d inType                         1
     d inFnd                          3
     d inKey                         30
     d inDate                         7
     d inPre                          1
     d inRrn                          9    options(*nopass)
     d inEobType                      3    options(*nopass)
     d inCkTp                        10    options(*nopass)
     d*** beg add ***** 04/06/21 **********************************************
     d inFyr                          3    options(*nopass)
     d inEnv                          1    options(*nopass)
     d outDftSrd                      1    options(*nopass)
     d*** end add ***** 04/06/21 **********************************************
      *
      * Local Variable
     d strCo#          s              3
     d strType         s              1
     d strFnd          s              3
     d strKey          s             30
     d strDate         s              7
     d strEft          s              1
     d strPre          s              1
     d strRrn          s              9
     d strEobType      s              3    Inz
     d strCkTp         s             10
     d*** beg add ***** 04/06/21 *************************
     d strFyr          s              3    Inz('000')
     d strEnv          s              1
     d strDftSrd       s              1    Inz
     d*** end add ***** 04/06/21 *************************
      *
     d EFTStatus       pr                  extPgm('WA023R3')
     d strCo#                         3
     d strType                        1
     d strFnd                         3
     d strKey                        30
     d strDate                        7
     d strEft                         1
     d strPre                         1
     d strRrn                         9    options(*nopass)
     d strEobType                     3    options(*nopass)
     d strCkTp                       10    options(*nopass)
     d strFyr                         3    options(*nopass)
     d strEnv                         1    options(*nopass)
     d strDftSrd                      1    options(*nopass)
      *
      /free
       strCo#    = inCo#;
       strType   = inType;
       strFnd    = inFnd;
       strKey    = inKey;
       strDate   = inDate;
       strPre    = inPre;

       If %parms >= 7 ;
        strRrn    = inRrn;
       Else;
        strRrn = *blanks;
       Endif;

       If %parms >= 9;
          strCkTp = inCktp;
       Else;
          strCkTp = *blanks;
       Endif;

       //*** beg chg ***** 04/06/21 ***********************
       //***EFTStatus(strCo#:strType:strFnd:strKey:strDate:
       //*** strEft:strPre:strRrn:strEobType:strCkTp);

       If %parms >= 10;
          strFyr    = inFyr;
       Endif;

       If %parms >= 11;
          strEnv    = inEnv;
       Endif;

       EFTStatus(strCo#:strType:strFnd:strKey:strDate:
        strEft:strPre:strRrn:strEobType:strCkTp:strFyr:strEnv:strDftSrd);
       //*** end chg ***** 04/06/21 ***********************

       inPre = strPre;
       If %parms >= 7 ;
          inRrn = strRrn;
       Endif;

       If %parms >= 8 ;
          inEobType = strEobType;
       Endif;

       //*** Begin Add *** 04/06/21 *************************
       If %parms >=12;
          outDftSrd = strDftSrd;
       Endif;
       //*** End   Add *** 04/06/21 *************************

       Return strEft;
      /end-free

     P GetEFTStatus    E
     *------------------------------------------------------------------------
     * GetNextEft# - Get the next EFT # and update
     *------------------------------------------------------------------------
     P GetNextEft#     B                   EXPORT

     *** Procedure Interface ***
     d GetNextEft#     PI             7  0
     d inCo#                          3  0
     d inAcct                        20
      *
      * Local Variable
     d company#        s                   Like(CKco#)
     d account#        s                   Like(CKacct)
     d eft#            s                   Like(CKeft#)
      *
      /free
       company# = inCo#;                                                     //
       account# = inAcct;                                                    //

       Chain (company#:account#) pck00;
       If %Found(WSCKL);

          If CKeft# = 0;
             eft# = CKeft# + 1;
          Else;
             eft# = CKeft#;
          EndIf;

          CKeft# = eft# + 1;
          CKuser = q1User;
          CKpgm = q1Pgm;
          CKtime=TimeTo6(SystemTime());
          CKdt=DateTo7(SystemDate());
          Update pck00;
       EndIf;

       Return eft#;

      /end-free

     P GetNextEft#     E
      *** End   Add *** 05/27/15 **********************************************
      *** beg add ***** 07/30/15 **********************************************
     *------------------------------------------------------------------------
     * GetNextChk# - Get the next chk # and update
     *------------------------------------------------------------------------
     P GetNextChk#     B                   EXPORT

     *** Procedure Interface ***
     d GetNextChk#     PI             7  0
     d inCo#                          3  0
     d inAcct                        20
      *
      * Local Variable
     d company#        s                   Like(CKco#)
     d account#        s                   Like(CKacct)
     d chk#            s                   Like(CKlchk)
      *
      /free
       company# = inCo#;                                                     //
       account# = inAcct;                                                    //

       Chain (company#:account#) pck00;
       If %Found(WSCKL);

          If CKlchk = 0;
             chk# = CKlchk + 1;
          Else;
             chk# = CKlchk;
          EndIf;

          CKlchk = chk# + 1;
          CKuser = q1User;
          CKpgm = q1Pgm;
          CKtime=TimeTo6(SystemTime());
          CKdt=DateTo7(SystemDate());
          Update pck00;
       EndIf;

       Return chk#;

      /end-free

     P GetNextchk#     E
     *------------------------------------------------------------------------
      * GetEFTExist - returns *on if request or transaction exists for
      *               fnd/acct/appl
      *             - accepts fnd/acct/appl and looks in agent or claims
      *               files to see if an EFT request or payment exists
     *------------------------------------------------------------------------
     p GetEFTExist     b                   export
     d GetEFTExist     pi              n
     d  co#                           3  0
     d  fnd                           3  0
     d  efdt                          7  0
     d  appl                          2
     d  acct                         20
     d  cktp                         10
      /free
       Select;
       When appl = 'WC';
          Open WARPL12;
          Setgt (co#:fnd:cktp:'E') prp12;
          Readpe (co#:fnd:cktp:'E') prp12;
          Close WARPL12;
          If not(%eof) and RPtrdt >= efdt;
             Return *on;
          Endif;
          Open WDPALV;
          Chain (co#:fnd:acct:cktp:'E') ppalv;
          Close WDPALV;
          If %found;
             Return *on;
          Endif;
       When appl = 'AG';
          Open WAR2L2;
          Setgt (co#:fnd:'E':cktp) pr202;
          Readpe (co#:fnd:'E':cktp) pr202;
          Close WAR2L2;
          If not(%eof) and R2trdt >= efdt;
             Return *on;
          Endif;
          Open WMACL23;
          Chain (co#:fnd:acct:cktp:'E') pac23;
          Close WMACL23;
          If %found;
             Return *on;
          Endif;
       Endsl;
       Return *off;
      /end-free
     p GetEFTExist     e
     *------------------------------------------------------------------------
      * GetEPayExist - returns *on if request or transaction exists for
      *               fnd/acct/appl
      *             - accepts fnd/acct/appl and looks in agent or claims
      *               files to see if an EFT request or payment exists
     *------------------------------------------------------------------------
     p GetEPayExist    b                   export
     d GetEPayExist    pi              n
     d  co#                           3  0
     d  fnd                           3  0
     d  efdt                          7  0
     d  appl                          2
     d  acct                         20
     d  cktp                         10
      /free
       Select;
       When appl = 'WC';
          Open WARPL12;
          Setgt (co#:fnd:cktp:'V') prp12;
          Readpe (co#:fnd:cktp:'V') prp12;
          Close WARPL12;
          If not(%eof) and RPtrdt >= efdt;
             Return *on;
          Endif;
          Open WDPALV;
          Chain (co#:fnd:acct:cktp:'V') ppalv;
          Close WDPALV;
          If %found;
             Return *on;
          Endif;
       When appl = 'AG';
          Open WAR2L2;
          Setgt (co#:fnd:'V':cktp) pr202;
          Readpe (co#:fnd:'V':cktp) pr202;
          Close WAR2L2;
          If not(%eof) and R2trdt >= efdt;
             Return *on;
          Endif;
          Open WMACL23;
          Chain (co#:fnd:acct:cktp:'V') pac23;
          Close WMACL23;
          If %found;
             Return *on;
          Endif;
       Endsl;
       Return *off;
      /end-free
     p GetEPayExist    e
     *------------------------------------------------------------------------
     * GetEPayVendor - Get the EPay Vendor
     *------------------------------------------------------------------------
     P GetEPayVendor   B                   EXPORT
     d GetEPayVendor   PI             5
     d inCo#                          3  0
     d inAcct                        20
      *
      * Local Variable
     d company#        s                   Like(CKco#)
     d account#        s                   Like(CKacct)
     d epayVendor      s                   Like(CKepayVn) Inz
      *
      /free
       company# = inCo#;                                                     //
       account# = inAcct;                                                    //

       Chain(n) (company#:account#) pck00;
       If %Found(WSCKL);
          ePayVendor = CKepayVn;
       EndIf;

       Return ePayVendor;

      /end-free

     P GetEPayVendor   E
     *------------------------------------------------------------------------
     * GetNextEPay# - Get the next EPay # and update
     *------------------------------------------------------------------------
     P GetNextEPay#    B                   EXPORT

     *** Procedure Interface ***
     d GetNextEPay#    PI             7  0
     d inCo#                          3  0
     d inAcct                        20
      *
      * Local Variable
     d company#        s                   Like(CKco#)
     d account#        s                   Like(CKacct)
     d epay#           s                   Like(CKepay#)
      *
      /free
       company# = inCo#;                                                     //
       account# = inAcct;                                                    //

       Chain (company#:account#) pck00;
       If %Found(WSCKL);

          If CKepay# = 0;
             epay# = CKepay# + 1;
          Else;
             epay# = CKepay#;
          EndIf;

          CKepay# = epay# + 1;
          CKuser = q1User;
          CKpgm = q1Pgm;
          CKtime=TimeTo6(SystemTime());
          CKdt=DateTo7(SystemDate());
          Update pck00;
       EndIf;

       Return epay#;

      /end-free

     P GetNextEPay#    E
       //*** Beg Add ***** 04/06/21 ************************
       //===================================================
       Dcl-Proc Has3rdPartyAcct Export;
       //===================================================
       Dcl-Pi *N Ind;
        inCo# Packed(3:0) Const;
        inFnd Packed(3:0) Const;
        inEmp# Packed(9:0) Const;
        inDiv Packed(5:0) Const;
        inType Char(100) Const;
        inEfDt Packed(7:0) Const;
        inFyr Packed(3:0) Const;
        inEnv Char(1) Const;
        outSRDFlg Char(1);
       End-Pi;

       Dcl-S flg1stExpRecDft Char(1) Inz('X');
       Dcl-S has3rdParty Ind;
       Dcl-S intCtr Packed(1:0);
       Dcl-S intStart Packed(1:0);
       Dcl-S intStRptDiv Packed(5:0);
       Dcl-S isStRptDiv Ind;
       Dcl-S prCo# Packed(3:0);
       Dcl-S prDiv Packed(5:0);
       Dcl-S prEmp# Packed(9:0);
       Dcl-S prEnv Char(1);
       Dcl-S prFnd Packed(3:0);
       Dcl-S prFyr Packed(3:0);
       Dcl-S prType Char(100);
       Dcl-S prEfDt Packed(7:0);
       //*** Beg Add *** 08/04/25 ******************************************
       Dcl-S CmdOvrDbF char(36) Inz('OVRDBF FILE(WMP3L) MBR(WMP3L)');
       Dcl-S CmdDltOvr char(36) Inz('DLTOVR FILE(WMP3L)');
       Dcl-S intCmdLen packed(15:5) inz(36);
       //*** End Add *** 08/04/25 ******************************************

       //*** begin add ***  04/05/23  ***********************************
       Clear has3rdParty;
       Clear isStRptDiv;
       //*** end add *****  04/05/23  ***********************************
       prCo# = inCo#;
       prFnd = inFnd;
       prEmp# = inEmp#;
       prDiv = inDiv;
       prFyr = inFyr;
       prEnv = inEnv;
       prType = inType;
       prEfdt = inEfdt;
       // Look in current division for 3rd Party Setup.
       // If none, look in St Rpt Div if Default Info flag is set
       If inType = 'S/R Online';
          intStart = 1;
          intStRptDiv = GetStRptDv(inCo#:inFnd:inEmp#);
          If intStRptDiv = prDiv;
             isStRptDiv = *On;
          Endif;
       Else;
          intStart = 2;
       Endif;
       For intCtr = intStart To 2;
          Select;
          When flg1stExpRecDft <> 'X'
           And flg1stExpRecDft <> 'Y';  // NSRD doesn't check SRD EFT Status
             Leave;
          When intCtr = 2;
             If prDiv = intStRptDiv;
                Leave;
             EndIf;
             flg1stExpRecDft = *Blank;
             prDiv = intStRptDiv;
             isStRptDiv = *On;
          Endsl;
          //*** Beg Add *** 08/04/25 ***************************************
          If Not %Open(WMP3L);
          //Override File to WMP3L member and open file
             RunCommand(CmdOvrDbF:intCmdLen);
             Open WMP3L;
          Endif;
          //*** End Add *** 08/04/25 ***************************************
          Setgt (prCo#:prFnd:prEmp#:prDiv:prType:prEfDt) pp300;
          Dou %Eof(WMP3L);
             Readpe (prCo#:prFnd:prEmp#:prDiv:prType) pp300;
             If %Eof;
                Leave;
             Endif;
             Select;
             When P3enbld <> 'Y';
                Iter;
             When prEnv = *Blank And P3envFlg = 'Q';
                Iter;
             When prEfDt < P3efdt;
                Iter;
             When P3exdt <> 0 And prEfdt >= P3exdt;
                If isStRptDiv = *Off And flg1stExpRecDft = 'X';
                   flg1stExpRecDft = P3DftSrd;  // Only chk St RPt Div if 'Y'
                Endif;
                Iter;
             When prFyr <> 0 And prFyr < P3sfyr;
                Iter;
             When intCtr = 2 And P3dftSrd <> 'Y';
                Iter;
             Other;
                has3rdParty = *On;
                Leave;
             Endsl;
          Enddo;
          If has3rdParty = *On;
             If intCtr = 2;  // St Rpt Div provides EFT
                outSrdFlg = P3DftSrD;
             Endif;
             Leave;
          Endif;
       EndFor;
       //*** Beg Add *** 08/04/25 ******************************************
       If %Open(WMP3L);
          //Close and delete override
          Close WMP3L;
          RunCommand(CmdDltOvr:intCmdLen);
       Endif;
       //*** End Add *** 08/04/25 ******************************************
       Return has3rdParty;
       End-Proc;
       //===================================================
       Dcl-Proc RequiresPreNote Export;
       // Will need work if implemented into Claims EFT
       //  for Check Type or Epay/Vpay
       //===================================================
       Dcl-Pi *N Ind;
        inCo# Packed(3:0);
        inFnd Packed(3:0);
        inType Char(1);
        inEfDt Packed(7:0);
       End-Pi;
       Dcl-S chrAppl Char(2);
       Dcl-S chrCkTp Char(10);
       Select;
       When inType = 'A';
          chrAppl = 'AG';   // Bank Application Type
       When inType  = 'C' OR inType  = 'D' OR inType  = 'P';
          chrAppl = 'WC';
       When inType  = 'E';
          chrAppl = 'WP';
       Endsl;
       Setll (inCo#:chrAppl:chrCkTp:inFnd:inEfDt) pbi05;
       Dou %Eof(WMBIL5);
          Reade (inCo#:chrAppl:chrCkTp:inFnd) pbi05;
          If %Eof;
             Leave;
          Endif;
          If BIeftc = 'E' or BIeftc = 'B'; // Flagged for EFT
             Leave;
          Endif;
       Enddo;
       If Not(%Eof(WMBIL5));
          //*** Beg Chg *** 08/24/21 **************************************
          //***Chain (inCo#:BIacct) pck00;
          Chain(n) (inCo#:BIacct) pck00;
          //*** End Chg *** 08/24/21 **************************************
          If %Found And CKbypn <> 'Y';  // Bypass PreNote Flag
            Return *On;
          Endif;
       Endif;
       Return *Off;
       End-Proc;
       //*-----------------------------------------------------------------
       //* Archive WMP3P Data in ARCHIVE member                           *
       //*-----------------------------------------------------------------
       Dcl-Proc ArchiveWMP3P Export;
        Dcl-Pi *N;
         inData likerec(PP301) Const;
         inAction char(10) Const;
        End-Pi;
        Dcl-S intCmdLen packed(15:5) inz(36);
        Dcl-S CmdAddPfM char(36) Inz('ADDPFM FILE(WMP3P) MBR(ARCHIVE)');
        Dcl-S CmdOvrDbF char(36) Inz('OVRDBF FILE(WMP3P) MBR(ARCHIVE)');
        //*** Beg Add *** 04/24/25 *****************************************
        Dcl-S CmdDltOvr char(36) Inz('DLTOVR FILE(WMP3P)');
        //*** End Add *** 04/24/25 *****************************************
        Dcl-ds WMP3Parc likerec(pp301:*output);

        //Create ARCHIVE member
        Monitor;
           RunCommand(CmdAddPfM:intCmdLen);
        On-Error;
        EndMon;

        //Override File to ARCHIVE member
        RunCommand(CmdOvrDbF:intCmdLen);

        //Open File
        If not %open(WMP3P);
           Open WMP3P;
        Endif;

        //Archive Data
        WMP3Parc        = inData;
        WMP3Parc.P3user = q1User;
        WMP3Parc.P3pgm  = inAction;
        WMP3Parc.P3time = TimeTo6(SystemTime());
        WMP3Parc.P3dt   = DateTo7(SystemDate());
        write PP301 WMP3Parc;

        //Close File
        If %open(WMP3P);
           Close WMP3P;
           //*** Beg Add *** 04/24/25 **************************************
           //Delete Override to ARCHIVE member
           RunCommand(CmdDltOvr:intCmdLen);
           //*** End Add *** 04/24/25 **************************************
        Endif;
       End-Proc;
       //*** End Add ***** 04/06/21 ************************
