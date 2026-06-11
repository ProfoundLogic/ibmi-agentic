     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H DftName(FPIEPAY)
     H option(*nodebugio:*srcstmt)
     H nomain
     *========================================================================
     *
     * FPIEPAY - Procedures used for various e-pay processes
     *
      *=====================================================================
     * Created on 07/20/2020 by C:79 for Project WC2749
     *========================================================================
     * Revisions:
     * 04/19/21 C:079 Support: 90/95537
     *                Add B2CKTP to GetEobEPayMC
     *
     *========================================================================
       //-------------------------------------------------------------------
       // File Definitions
       //-------------------------------------------------------------------
     fwdb2l1    if   e           k disk
       //-------------------------------------------------------------------
       // Copybooks
       //-------------------------------------------------------------------
      /Copy sprModule
       //-------------------------------------------------------------------
       // Standalone fields
      //----------------------------------------------------------------------
      // GetEobPayMC - Return E-Pay Vendor for EOB
      //----------------------------------------------------------------------
     P GetEobEPayMC    B                   Export
     D GetEobEPayMC    PI             5a
     D inCo#                          3  0 Const
     D inAppl                         2    Const
     D inFnd                          3  0 Const
     D inAcct                        20    Const
     D inEfDt                         7  0 Const
     D*** Beg Add *** 04/19/21 *********************************************
     D inCkTp                        10    Const
     D*** End Add *** 04/19/21 *********************************************

     D epayVend        s              5a   Inz(*blanks)

      /free

       //*** Beg Chg *** 04/19/21 ******************************************
       //***Setll (inCo#:inAppl:inFnd:inAcct:inEfDt) pb201;
       //***Reade (inCo#:inAppl:inFnd:InAcct) pb201;
       Setll (inCo#:inAppl:inFnd:inAcct:inCkTp:inEfDt) pb201;
       Reade (inCo#:inAppl:inFnd:InAcct:inCkTp) pb201;
       //*** End Chg *** 04/19/21 ******************************************
       Dow not %Eof(WDB2L1);
       Select;
          When B2ePClmMed = 'Y';
             EpayVend = B2ePvend;
             Leave;
       Endsl;
       //*** Beg Chg *** 04/19/21 ******************************************
       //*** Reade (inCo#:inAppl:inFnd:inAcct) pb201;
          Reade (inCo#:inAppl:inFnd:InAcct:inCkTp) pb201;
       //*** End Chg *** 04/19/21 ******************************************
       Enddo;
       Return EpayVend;

      /end-free
     P GetEobEPayMC    E
