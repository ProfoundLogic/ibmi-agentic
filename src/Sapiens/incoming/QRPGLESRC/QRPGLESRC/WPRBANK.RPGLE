     D*=====================================================================
     D* Bank processing procedure prototypes
     D*=====================================================================
      *** Beg   Add ***** 04/06/21 ********************************************
     P*====================================================================
     P* Has3rdPartyAcct - Returns *On if 3rd party account
     P*====================================================================
     dHas3rdPartyAcct  PR             1N
     d inCo#                          3  0 Const
     d inFnd                          3  0 Const
     d inEmp#                         9  0 Const
     d inDiv                          5  0 Const
     d inType                         1    Const
     d inEfDt                         7  0 Const
     d inFyr                          3  0 Const
     d inEnv                          1    Const
     d outSrdFlg                      1
      *** End    Add ***** 04/06/21 *******************************************
     P*====================================================================
     P* GetBankAcc - Return Bank Account no.
     P*====================================================================
     d GetBankAcc      PR            20
     d eCo#                           3                                         Company
     d eFnd                           3                                         Group #
     d eAppl                          2                                         Applic Code
     d eSrdt                          8                                         MMDDCCYY
     d eCall                          1                                         (V)oucher/(C)heck
     d eHdcy                          1                                         Hardcopy Y/N
     d eChkq                         10                                         Check/Voucher OUTQ
     d eFTYP                         10                                         Form Type
     d eEfdt                          7                                         WMBIP Eff. DT CYYMMD
     d eCpyq                         10                                         Filecopy OUTQ
     d eCktp                         10    options(*nopass)                     Account Type
     d eEftc                          1    options(*nopass)                     EFT Flag
     d eCoid                         20    options(*nopass)                     Debit/Credit Comp.Id
     d eRtcd                          1    options(*nopass)                     Return Code
     P*====================================================================
     P* ValidBankCode - Return '1' for Valid bank Code and '0' for invalid
     P*                 Bank Code.
     P*====================================================================
     d ValidBankCode   PR             1n
     d eCo#                           3                                         Company
     d eFnd                           3                                         Group #
     d eFyr                           3                                         Year
     d eCase                          7                                         Claim #
     d eAppl                          2                                         Applic Code
     d eData                         75                                         Selection criteria
     d eCktp                         10                                         Bank Acct Code
     d eEftc                          1    options(*nopass)                     EFT Flag
     P*====================================================================
     P* GetBankCode - Return Bank Code
     P*====================================================================
     d GetBankCode     PR            10
     d eCo#                           3                                         Company
     d eFnd                           3                                         Group #
     d eFyr                           3                                         Year
     d eCase                          7                                         Claim #
     d eAppl                          2                                         Applic Code
     d eData                         75                                         Selection criteria
     d eRtcd                          1                                         Return Code
     d eEftc                          1    options(*nopass)                     EFT Flag
     P*====================================================================
     P* GetEFTStatus- EFT status Valid
     P*====================================================================
     d GetEFTStatus    PR             1n
     d eCo#                           3                                         Company
     d eType                          1                                         Type
     d eFnd                           3                                         Group #
     d eKey                          30                                         Key
     d eDate                          7                                         Date
     d ePre                           1                                         PRE
     d eRrn                           9    options(*nopass)                     RRN
     d eEobType                       3    options(*nopass)                     RRN
     d eCkTp                         10    options(*nopass)                     RRN
     d*** beg add ***** 04/06/21 *******************************************
     d inFyr                          3    options(*nopass)
     d inEnv                          1    options(*nopass)
     d outDftSrD                      1    options(*nopass)
     d*** end add ***** 04/06/21 *******************************************
     *------------------------------------------------------------------------
     * GetNextEft# - Get the next EFT # and update
     *------------------------------------------------------------------------
     d GetNextEft#     PR             7  0
     d inCo#                          3  0                                      01
     d inAcct                        20                                         02
     *------------------------------------------------------------------------
     * GetNextChk# - Get the next Chk # and update
     *------------------------------------------------------------------------
     d GetNextChk#     PR             7  0
     d inCo#                          3  0                                      01
     d inAcct                        20                                         02
     P*====================================================================
     P* GetEFTExist - Check if EFT transaction exists
     P*====================================================================
     d GetEFTExist     pr              n
     d  co#                           3  0
     d  fnd                           3  0
     d  efdt                          7  0
     d  appl                          2
     d  acct                         20
     d  cktp                         10
     *------------------------------------------------------------------------
      * GetEPayExist - returns *on if request or transaction exists for
      *               fnd/acct/appl
      *             - accepts fnd/acct/appl and looks in agent or claims
      *               files to see if an EFT request or payment exists
     *------------------------------------------------------------------------
     d GetEPayExist    pr              n
     d  co#                           3  0
     d  fnd                           3  0
     d  efdt                          7  0
     d  appl                          2
     d  acct                         20
     d  cktp                         10
     *------------------------------------------------------------------------
     * GetEPayVendor - Get the EPay Vendor
     *------------------------------------------------------------------------
     d GetEPayVendor   PR             5
     d inCo#                          3  0                                      01
     d inAcct                        20                                         02
     *------------------------------------------------------------------------
     * GetNextEPay# - Get the next EPay # and update
     *------------------------------------------------------------------------
     d GetNextEPay#    PR             7  0
     d inCo#                          3  0                                      01
     d inAcct                        20                                         02
      *** End   Add ***** 08/20/20 ********************************************
      *** Beg   Add ***** 04/06/21 ********************************************
     P*====================================================================
     P* RequiresPreNote - Returns *On if Pre-Note is required.
     P*====================================================================
     dRequiresPreNote  PR             1N
     d inCo#                          3  0
     d inFnd                          3  0
     d inType                         1
     d inEfDt                         7  0
      *------------------------------------------------------------------------
      * ArchiveWMP3P - Archives a record from WMP3P
      *------------------------------------------------------------------------
       Dcl-Ds WMP3Pds extname('WMP3P':*All) End-Ds;
       Dcl-Pr ArchiveWMP3P;
         inData likeDs(WMP3Pds) Const;
         inAction char(10) Const;
       End-Pr;
      *** End   Add ***** 04/06/21 ********************************************
